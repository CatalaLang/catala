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
open Shared_ast
module Runtime = Catala_runtime

module Var = struct
  type t = string list

  let compare = List.compare String.compare
  let to_string = String.concat "."

  let format =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ".")
      (fun ppf s ->
        if String.begins_with_uppercase s then
          Format.fprintf ppf "@{<hi_magenta>%s@}" s
        else
          match String.split_on_char '#' s with
          | [var; state] -> Format.fprintf ppf "%s/@{<hi_cyan>%s@}" var state
          | _ -> Format.pp_print_string ppf s)
end

module M = Map.Make (Var)

type value = Runtime.Value.t

let value_to_string v = Message.unformat (fun ppf -> Runtime.Value.format ppf v)

type failed_trace_assertions = {
  name : Var.t;
  expected : value;
  current_value : (value * Runtime.code_location) option;
  error : string option;
}

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
    Option.fold ~none:true ~some:(fun scope_var -> ident.name = scope_var) name
  | _ -> false

(** [find_scope_obj] descends into the sub-trace of the elements that do not
    match. This is needed because a scope call or a scope variable is rarely a
    direct child of the element above it in the path: the trace interleaves
    [if_branching], [branch_condition], [exception]... elements between them.

    The search is breadth-first. *)
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

let get_failures asserted_trace_variables scope (trace : Runtime.trace) =
  (* The variables lacking a path are gathered rather than reported one by one:
     on a large file that would drown the test output. *)
  let rec find_in_trace trace path =
    match path with
    | [] -> None
    | [id] -> find_scope_obj ~pred:is_var ~str:id trace
    | path :: paths -> (
      let pred =
        if String.begins_with_uppercase path then is_scope_call else is_var
      in
      match find_scope_obj ~pred ~str:path trace with
      | None -> None
      | Some trace -> find_in_trace trace.sub_trace paths)
  in
  M.fold
    (fun path (expected : value) failures ->
      match find_in_trace trace (ScopeName.original_base scope :: path) with
      | None ->
        { name = path; expected; current_value = None; error = None }
        :: failures
      | Some { pos; value; _ } -> (
        match value with
        | Some value -> (
          let current_value = Some (value, pos) in
          (* Warning: this is a runtime equality check that may not be
              well-typed: we rely on the fact that the runtime verifies
              the type equality and returns `false` when wrong *)
          match Runtime.Value.equal pos expected value with
          | true -> failures
          | false ->
            { name = path; expected; current_value; error = None } :: failures
          | exception Runtime.Error (err, _, _) ->
            {
              name = path;
              expected;
              current_value;
              error = Some (Runtime.error_message err);
            }
            :: failures)
        | None ->
          { name = path; expected; current_value = None; error = None }
          :: failures))
    asserted_trace_variables []

(* TODO: since we have Runtime.to_json, it would be nice to include the structured values in here instead of raw strings *)
let trace_assertion_to_json (assertion : failed_trace_assertions) : Yojson.t =
  `Assoc
    ([
       "variable", `String (Var.to_string assertion.name);
       "expected", `String (value_to_string assertion.expected);
     ]
    @ Option.fold ~none:[]
        ~some:(fun (a, _) -> ["value", `String (value_to_string a)])
        assertion.current_value
    @ Option.fold ~none:[] ~some:(fun e -> ["error", `String e]) assertion.error
    )

let display ppf (assertion : failed_trace_assertions) =
  Format.fprintf ppf
    "@[<v>@[<hov>Trace assertion failed:@ expected %a =@ @{<green>%a@},@]@ \
     @[<hov>got:@ @{<red>%a@}@]"
    Var.format assertion.name Runtime.Value.format assertion.expected
    (Format.pp_print_option
       ~none:(fun ppf () -> Format.fprintf ppf "@{<grey>Missing@}")
       (fun ppf v -> Runtime.Value.format ppf (Mark.remove v)))
    assertion.current_value;
  Option.iter (Format.fprintf ppf "@ [%s]") assertion.error;
  Format.pp_close_box ppf ()

let report_trace_assertion_failures s failures =
  List.iter
    (fun failed_trace ->
      let pos =
        match failed_trace.current_value with
        | Some v -> Expr.runtime_to_pos (Mark.get v)
        | None -> snd (ScopeName.get_info s)
      in
      if Global.options.no_fail_on_assert then
        Message.warning ~pos "%a" display failed_trace
      else
        Message.delayed_error ~kind:TraceAssertFails () ~pos "%a" display
          failed_trace)
    failures

open Shared_ast

let check program scope trace =
  if not Global.options.check_trace_assertion then ()
  else
    let assertions, _ =
      let s = Program.find_scope scope program.code_items in
      let _, body = Bindlib.unbind s.scope_body_expr in
      BoundList.fold_left ~init:M.empty
        ~f:(fun acc sclet _ ->
          match sclet.scope_let_kind with
          | Assertion ->
            let a =
              Pos.get_attrs
                (Expr.mark_pos (Mark.get sclet.scope_let_expr))
                (function
                  | Desugared.Ast.TraceAssert (path, e0) -> (
                    let e = Expr.unbox (Typing.expr program.decl_ctx e0) in
                    try Some (path, Expr.embed_value program.decl_ctx e)
                    with Invalid_argument _ ->
                      Message.error ~pos:(Expr.pos e0)
                        "Invalid trace-assertion value: only constants are \
                         allowed")
                  | _ -> None)
            in
            M.add_seq (List.to_seq a) acc
          | _ -> acc)
        body
    in
    let failures = get_failures assertions scope trace in
    match Global.options.trace_format with
    | Human -> report_trace_assertion_failures scope failures
    | JSON ->
      Yojson.pretty_print (Message.std_ppf ())
        (`List (List.map trace_assertion_to_json failures))
