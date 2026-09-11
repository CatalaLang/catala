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
module M = Scan.M
module Runtime = Catala_runtime

type expected = {
  name : string;
  expected : string;
  current_value : string option;
}

(* Trace elements, as emitted by [Catala_runtime.Json.trace], are of the form
   [{"element": {"kind": ...; "name": ...}, "pos": ..., "value": ...,
     "trace": [<sub-elements>]}], the "trace" field holding the sub-trace of the
   element (absent when empty). A trace file contains one such list per
   evaluated scope. *)

let json_field name : Yojson.Safe.t -> Yojson.Safe.t option = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

(* Expected values are written by hand in the Catala source, with the surface
   syntax of the file's language ("100,25 €", "$100.25", "2024-1-3"...), whereas
   the trace holds them as rendered by [Catala_runtime.Json]: money as "100.25"
   (two decimals, dot separator, no currency symbol), dates zero-padded, exact
   decimals as a "num/den" rational, etc.

   [normalize_value] parses the surface form and re-renders it through the very
   same encoder the trace uses, so the two are comparable as plain strings by
   construction rather than by replicating the runtime's formatting rules. *)
let normalize_value (s : string) : string =
  let s = String.trim s in
  let money_of s =
    (* Accepts "$100.25" and "100,25 €"; thousands separators (including
       non-breaking spaces) are dropped. *)
    let strip_currency s =
      if String.contains s '$' then
        Some (String.concat "" (String.split_on_char '$' s))
      else
        let euro = "€" in
        let ls = String.length s and le = String.length euro in
        if ls >= le && String.sub s (ls - le) le = euro then
          Some (String.sub s 0 (ls - le))
        else None
    in
    match strip_currency s with
    | None -> None
    | Some n -> (
      let n =
        String.trim n
        |> Re.replace_string Re.(compile (alt [str " "; str "\xc2\xa0"])) ~by:""
        |> String.map (function ',' -> '.' | c -> c)
      in
      (* [decimal_of_string] is exact, unlike a float round-trip *)
      try Some (Runtime.money_of_decimal (Runtime.decimal_of_string n))
      with _ -> None)
  in
  let scan fmt f = try Some (Scanf.sscanf s fmt f) with _ -> None in
  let value : Runtime.Value.t option =
    match s with
    | "true" -> Some (Runtime.Value.V (Bool, true))
    | "false" -> Some (Runtime.Value.V (Bool, false))
    | s -> (
      match money_of s with
      | Some m -> Some (Runtime.Value.V (Money, m))
      | None -> (
        match int_of_string_opt s with
        | Some i -> Some (Runtime.Value.V (Integer, Runtime.integer_of_int i))
        | None -> (
          match scan "%d-%d-%d%!" (fun y m d -> y, m, d) with
          | Some (y, m, d) -> (
            try Some (Runtime.Value.V (Date, Runtime.date_of_numbers y m d))
            with _ -> None)
          | None -> (
            match scan "%dy %dm %dd%!" (fun y m d -> y, m, d) with
            | Some (y, m, d) ->
              Some
                (Runtime.Value.V (Duration, Runtime.duration_of_numbers y m d))
            | None -> (
              (* Not a literal: an enum constructor, which the trace stores
                 verbatim as a JSON string. *)
              match Runtime.decimal_of_string s with
              | d -> Some (Runtime.Value.V (Decimal, d))
              | exception _ -> None)))))
  in
  match value with
  | None -> s
  | Some v -> (
    (* Unwrap the JSON string the encoder produces for scalars, so that the
       result can be compared with what [inspect_value] extracts from the
       trace. *)
    match Yojson.Safe.from_string (Runtime.Json.runtime_value v) with
    | `String str -> str
    | json -> Yojson.Safe.to_string json
    | exception _ -> s)

(** Durations are the only scalar the trace stores as an object, of the shape
    [{"years":_,"months":_,"days":_}]. [duration_of_fields] recognises it and
    re-emits it in that canonical field order, the same one [normalize_value]
    produces, so that both sides compare equal whatever the order in the file.
    Returns [None] if the object is not a duration. *)
let duration_of_fields (assoc_l : (string * Yojson.Safe.t) list) : string option
    =
  let field f =
    match List.assoc_opt f assoc_l with Some (`Int i) -> Some i | _ -> None
  in
  match field "years", field "months", field "days" with
  | Some years, Some months, Some days ->
    Some
      (Yojson.Safe.to_string
         (`Assoc ["years", `Int years; "months", `Int months; "days", `Int days]))
  | _ -> None

(* Alternative lookup, kept for reference: instead of following an explicit JSON
   path, it searches the trace by element name. Not wired into [check_expected]
   yet. *)

let is_scope_call ?name elt =
  match json_field "element" elt with
  | None -> false
  | Some element ->
    json_field "kind" element = Some (`String "scope_call")
    && Option.fold ~none:true
         ~some:(fun scope -> json_field "name" element = Some (`String scope))
         name

let is_var ?name elt =
  match json_field "element" elt with
  | None -> false
  | Some element ->
    let kind = json_field "kind" element in
    (kind = Some (`String "scope_var") || kind = Some (`String "local_var"))
    && Option.fold ~none:true
         ~some:(fun var -> json_field "name" element = Some (`String var))
         name

let sub_trace elt =
  match json_field "trace" elt with Some (`List elts) -> elts | _ -> []

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
    ~(pred : ?name:string -> Yojson.Safe.t -> bool)
    ~str
    (trace : Yojson.Safe.t list) : Yojson.Safe.t option =
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
              if is_scope_call elt || is_var elt then [] else sub_trace elt)
            level
        in
        by_level sub_traces)
  in
  by_level trace

(* Constructors of the [Optional] enum standing for "present"; the name depends on
   the language the file is written in. The trace wraps an optional value in a
   single-field object, e.g. {"Présent": "12"}. *)
let optional_present = ["Present"; "Présent"; "Obecny"]

(** [value_of_json json] reads the scalar rendering of the ["value"] field of a
    trace element. Follows the shapes produced by [Catala_runtime.Json]: money,
    integers, decimals, dates and payload-less enum constructors are already
    strings, booleans are raw JSON booleans, durations are objects, and an
    optional value is wrapped in a single-field object.

    Returns [None] for the composite values (structs, tuples, arrays) that have
    no scalar rendering to compare against: the path must reach a leaf. *)
let rec value_of_json (json : Yojson.Safe.t) : string option =
  match json with
  | `String str -> Some str
  | `Bool b -> Some (string_of_bool b)
  | `Int i -> Some (string_of_int i)
  | `Assoc assoc_l -> (
    match duration_of_fields assoc_l with
    | Some _ as duration -> duration
    | None -> (
      match assoc_l with
      | [(ctor, payload)] when List.mem ctor optional_present ->
        (* Unwrap the "present" constructor. Its payload is sometimes a list whose
           first item is the value itself (see [traceValueFromJson] in the
           language server's traceUtils.ts). *)
        value_of_json
          (match payload with `List (v :: _) -> v | payload -> payload)
      | _ -> None))
  | _ -> None

let read_trace (file : File.t) : Yojson.Safe.t =
  try Yojson.Safe.from_file file
  with Yojson.Json_error msg ->
    Message.error "Invalid JSON in the trace file %a:@ %s" File.format file msg

let check_expected ~expected ~tested_scope (trace : Yojson.Safe.t) =
  if M.is_empty expected then (
    Message.debug "No expected value to check for %s" tested_scope;
    [])
  else
    let trace_elements = match trace with `List elts -> elts | elt -> [elt] in
    (* Remove _test from the tested_scope name *)
    let testing_scope =
      String.sub tested_scope 0 (String.length tested_scope - 5)
    in
    let testing_scope_var = Catala_utils.String.to_snake_case testing_scope in
    (* The variables lacking a path are gathered rather than reported one by one:
       on a large file that would drown the test output. *)
    let failures =
      M.fold
        (fun path (v : Scan.expected_variable) failures ->
          let items =
            tested_scope
            :: testing_scope_var
            :: testing_scope
            :: String.split_on_char '.' path
          in
          match String.trim v with
          | "" ->
            (* No expected value *)
            failures
          | expected ->
            let rec inspect_trace json elts =
              match elts with
              | [] -> None
              | [elt] -> find_scope_obj ~pred:is_var ~str:elt json
              | elt :: elts -> (
                let pred =
                  if String.begins_with_uppercase elt then is_scope_call
                  else is_var
                in
                match find_scope_obj ~pred ~str:elt json with
                | None -> None
                | Some json -> inspect_trace (sub_trace json) elts)
            in
            let current_value =
              Option.bind
                (Option.bind
                   (inspect_trace trace_elements items)
                   (json_field "value"))
                value_of_json
            in
            let expected = normalize_value expected in
            if current_value = Some expected then failures
            else { name = path; expected; current_value } :: failures)
        expected []
    in
    failures

let display_expected ppf (expected : expected) =
  let f =
    Format.fprintf ppf
      "@[<v 4>@{<red>■@} Variable %s:@,Expected: %s@,Actual: %s@]" expected.name
      expected.expected
  in
  match expected.current_value with None -> f "Missing" | Some l -> f l
