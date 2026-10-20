(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
module M = Map.Make (String)
module Runtime = Catala_runtime

type asserted_trace_variable = string

let asserted_trace_variable_to_string = Fun.id

type value = Runtime.Value.t

let value_to_string = Format.asprintf "%a" Runtime.Value.format

type trace_assertions = value option M.t

type failed_trace_assertions = {
  name : asserted_trace_variable;
  expected : value;
  current_value : value option;
  pos : Runtime.code_location option;
}

let empty = M.empty
let add_asserted_trace_variable name value map = M.add name value map

(* Alternative lookup, kept for reference: instead of following an explicit JSON
   path, it searches the trace by element name. Not wired into
   [check] yet. *)

let is_scope_call ?name (trace_element : Runtime.trace_element) =
  match trace_element.kind with
  | Runtime.ScopeCall ident ->
    Option.fold ~none:true
      ~some:(fun scope_called -> ident.name = scope_called)
      name
  | _ -> false

let is_var ?name (trace_element : Runtime.trace_element) =
  match trace_element.kind with
  | Runtime.ScopeVarDef def ->
    Option.fold ~none:true
      ~some:(fun scope_var -> def.var.name = scope_var)
      name
  | Runtime.LocalVarDef ident ->
    Option.fold ~none:true ~some:(fun scope_var -> ident = scope_var) name
  | _ -> false

(** [find_scope_obj] descends into the sub-trace of the elements that do not
    match. This is needed because a scope call or a scope variable is rarely a
    direct child of the element above it in the path: the trace interleaves
    [if_branching], [branch_condition], [exception]... elements between them.

    The search is breadth-first: a whole level is scanned before going one level
    deeper, so the match returned is the shallowest one. That is the right bias
    here, since a path segment designates the closest matching descendant; a
    depth-first search would dive into the first branch and could return a
    homonym buried in an unrelated one before even looking at its siblings. *)
let find_scope_obj
    ~(pred : ?name:string -> Runtime.trace_element -> bool)
    ~str
    (trace : Runtime.trace) : Runtime.trace_element option =
  let rec by_level = function
    | [] -> None
    | level -> (
      match List.find_opt (pred ~name:str) level with
      | Some _ as found -> found
      | None ->
        let sub_traces =
          List.concat_map
            (fun elt ->
              (* Don't dive in traces of a scope_call or a scope_var *)
              if is_scope_call elt || is_var elt then [] else elt.sub_trace)
            level
        in
        by_level sub_traces)
  in
  by_level trace

(* Names of the [Optional] enum, as embedded by [Shared_ast.Print] in the
   language of the file. *)
let optional_enum = ["Optional"; "Optionnel"; "Opcjonalny"]

(** Unwraps every [Optional] layer around a traced value:
    [Present (value, position)] yields the value with its position. Landing on
    [Absent] means the variable has no value: an [Absent] value is returned
    along with the position accumulated so far. *)
let unwrap_optional (value : Runtime.Value.t) :
    Runtime.Value.t * Runtime.code_location option =
  let open Runtime.Value in
  let rec unwrap (pos : Runtime.code_location option) (value : t) =
    match value with
    | V (Enum { name; constr }, v) when List.mem name optional_enum -> (
      match constr v with
      | _, _, None ->
        V (Runtime.Optional.rtype Unit, Runtime.Optional.Absent), pos
      | _, _, Some (V (Tuple fields, t) as payload) -> (
        match fields t with
        | [payload; V (Position, pos)] -> unwrap (Some pos) payload
        | _ -> unwrap pos payload)
      | _, _, Some payload -> unwrap pos payload)
    | _ -> value, pos
  in
  unwrap None value

(* [expected] comes from a #[trace.assert] attribute, parsed without type
   information (see [Surface.Expr_value]): enums may have an empty name and
   constructor indices are meaningless, so they are matched by constructor
   name. Leaves are delegated to [Runtime.Value.equal]. *)
let rec matches pos ~expected actual =
  let open Runtime.Value in
  match expected, actual with
  | V (Array te, ve), V (Array ta, va) ->
    Array.length ve = Array.length va
    && Array.for_all2
         (fun e a -> matches pos ~expected:e a)
         (Array.map te ve) (Array.map ta va)
  | V (Tuple te, ve), V (Tuple ta, va) ->
    let le = te ve and la = ta va in
    List.compare_lengths le la = 0
    && List.for_all2 (fun e a -> matches pos ~expected:e a) le la
  | V (Struct se, ve), V (Struct sa, va) ->
    let fe = se.fields ve and fa = sa.fields va in
    List.compare_lengths fe fa = 0
    && List.for_all
         (fun (f, e) ->
           match List.assoc_opt f fa with
           | Some a -> matches pos ~expected:e a
           | None -> false)
         fe
  | V (Enum ee, ve), V (Enum ea, va) ->
    (ee.name = "" || ee.name = ea.name)
    &&
    let _, ce, xe = ee.constr ve and _, ca, xa = ea.constr va in
    ce = ca && Option.equal (fun e a -> matches pos ~expected:e a) xe xa
  | _ -> Runtime.Value.equal pos expected actual

let check ~asserted_trace_variables ~tested_scope (trace : Runtime.trace) =
  if M.is_empty asserted_trace_variables || List.length trace = 0 then (
    Message.debug "No asserted trace variable to check for %s" tested_scope;
    [])
  else
    (* The variables lacking a path are gathered rather than reported one by one:
       on a large file that would drown the test output. *)
    M.fold
      (fun path (v : value option) failures ->
        let items = tested_scope :: String.split_on_char '.' path in
        match v with
        | None ->
          (* No asserted value *)
          failures
        | Some expected -> (
          let rec inspect_trace trace path =
            match path with
            | [] -> None
            | [id] -> find_scope_obj ~pred:is_var ~str:id trace
            | path :: paths -> (
              let pred =
                if String.begins_with_uppercase path then is_scope_call
                else is_var
              in
              match find_scope_obj ~pred ~str:path trace with
              | None -> None
              | Some trace -> inspect_trace trace.sub_trace paths)
          in
          let trace_element = inspect_trace trace items in
          match trace_element with
          | None ->
            { name = path; expected; current_value = None; pos = None }
            :: failures
          | Some trace_element -> (
            let current_value, pos =
              match trace_element.value with
              | Some value ->
                let value, pos = unwrap_optional value in
                Some value, pos
              | None -> None, None
            in
            let pos = Some (Option.value pos ~default:trace_element.pos) in
            let fail = { name = path; expected; current_value; pos } in
            match current_value with
            | Some value ->
              if matches trace_element.pos ~expected value then failures
              else fail :: failures
            | None -> fail :: failures)))
      asserted_trace_variables []

let trace_assertion_to_json (assertion : failed_trace_assertions) : Yojson.t =
  `Assoc
    ([
       "variable", `String assertion.name;
       "expected", `String (value_to_string assertion.expected);
     ]
    @ Option.fold ~none:[]
        ~some:(fun a -> ["value", `String (value_to_string a)])
        assertion.current_value)

let report_display ppf (assertion : failed_trace_assertions) =
  let f =
    Format.fprintf ppf
      "@[<v 4>@{<red>■@} Variable %s:@,Expected: %s@,Actual: %s@]"
      assertion.name
      (value_to_string assertion.expected)
  in
  match assertion.current_value with
  | None -> f "Missing"
  | Some l -> f (value_to_string l)

let display ppf (assertion : failed_trace_assertions) =
  (* Just trying to understand what is wrong with enumeration verification *)
  Format.fprintf ppf "Trace Assertion Failed: %s ≠ %s (%s) @]" assertion.name
    (value_to_string assertion.expected)
    (Option.fold ~none:"Missing" ~some:value_to_string assertion.current_value)
