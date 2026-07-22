(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

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

include Ninja_utils.Var
(** Ninja variable names *)

(* See [cmd_only] in the mli. *)
let cmd_only_names = ref String.Set.empty
let is_cmd_only v = String.Set.mem (name v) !cmd_only_names

let cmd_only v =
  cmd_only_names := String.Set.add (name v) !cmd_only_names;
  v

(** Global vars: always defined, at toplevel *)

let ninja_required_version = make "ninja_required_version"
let builddir = make "builddir"
let clerk_exe = make "CLERK_EXE"
let clerk_flags = cmd_only (make "CLERK_FLAGS")
let catala_exe = make "CATALA_EXE"
let catala_flags = cmd_only (make "CATALA_FLAGS")

let make, all_vars_ref =
  let all_vars_ref = ref String.Map.empty in
  ( (fun s ->
      let v = make s in
      all_vars_ref := String.Map.add s v !all_vars_ref;
      v),
    all_vars_ref )

let runtime = make "CATALA_RUNTIME"
let all_vars = all_vars_ref.contents

(* Definition spreading different rules *)

let tdir = make "tdir"
let includes = make "includes"

(* Rule vars, Used in specific rules *)

let input = make "in"
let output = make "out"
let src = make "src"
let dst = make "dst"
let class_path = make "class_path"
let cat_files = make "cat_files" (* Useful on Windows only *)

(* let scope = make "scope" *)
let test_id = make "test-id"
let ( ! ) = Ninja_utils.Var.v
let quote_arg s = "\"" ^ s ^ "\""

let binding_words var words =
  if is_cmd_only var then List.map quote_arg words else words

let check_value var words =
  if is_cmd_only var then
    List.iter
      (fun w ->
        if String.contains w '"' then
          Message.error
            "Invalid word %S in the value of variable @{<blue;bold>$%s@}: \
             quote characters are not allowed, shell quoting is applied \
             automatically"
            w (name var))
      words;
  words

let re_var =
  let open Re in
  seq [str "${"; group (rep1 (diff any (char '}'))); char '}']

let check_path =
  let re = lazy Re.(compile re_var) in
  fun s ->
    List.iter
      (fun g ->
        let n = Re.Group.get g 1 in
        if String.Set.mem n (Stdlib.( ! ) cmd_only_names) then
          Message.error
            "Command-only variable @{<blue;bold>${%s}@} referenced in a ninja \
             path position"
            n)
      (Re.all (Lazy.force re) s);
    s

type bindings = (t * string list) list

let rec get_var =
  (* replaces ${var} with its value, recursively *)
  let re_single_var = Re.(compile (whole_string re_var)) in
  fun var_bindings v ->
    let s =
      try List.assoc v var_bindings
      with Not_found ->
        Message.error
          "Clerk configuration error: variable @{<blue;bold>$%s@} is undefined"
          (name v)
    in
    let get_var = get_var (List.remove_assoc v var_bindings) in
    List.concat_map
      (fun s ->
        match Re.exec_opt re_single_var s with
        | Some g -> get_var (make (Re.Group.get g 1))
        | None -> [expand_vars var_bindings s])
      s

and expand_vars =
  let re_var = Re.(compile re_var) in
  fun var_bindings s ->
    Re.replace ~all:true re_var
      ~f:(fun g ->
        String.concat " " (get_var var_bindings (make (Re.Group.get g 1))))
      s

module Nj = struct
  include Ninja_utils

  let build ?inputs ?implicit_in ~outputs ?implicit_out ?vars rule =
    let chk = List.map check_path in
    Ninja_utils.build ?inputs:(Option.map chk inputs)
      ?implicit_in:(Option.map chk implicit_in)
      ~outputs:(chk outputs)
      ?implicit_out:(Option.map chk implicit_out)
      ?vars rule
end
