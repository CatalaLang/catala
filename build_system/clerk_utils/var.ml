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

(** Global vars: always defined, at toplevel *)

let ninja_required_version = make_atom "ninja_required_version"
let builddir = make_atom "builddir"
let clerk_exe = make_atom "CLERK_EXE"
let clerk_flags = make_expr "CLERK_FLAGS"
let catala_exe = make_atom "CATALA_EXE"
let catala_flags = make_expr "CATALA_FLAGS"
let runtime = make_atom "CATALA_RUNTIME"

(* Definition spreading different rules *)

let tdir = make_atom "tdir"
let includes = make_expr "includes"

(* Rule vars, Used in specific rules *)

let input = make_atom "in"
let output = make_atom "out"
let src = make_atom "src"
let dst = make_atom "dst"
let class_path = make_atom "class_path"
let cat_files = make_atom "cat_files" (* Useful on Windows only *)

(* let scope = make "scope" *)
let test_id = make_atom "test-id"

let re_var =
  let open Re in
  seq [str "${"; group (rep1 (diff any (char '}'))); char '}']

type bindings = (string * string list) list

let rec get_var : bindings -> string -> string list =
  (* replaces ${var} with its value, recursively *)
  let re_single_var = Re.(compile (whole_string re_var)) in
  fun var_bindings (v : string) ->
    let s =
      try List.assoc v var_bindings
      with Not_found ->
        Message.error
          "Clerk configuration error: variable @{<blue;bold>$%s@} is undefined"
          v
    in
    let get_var = get_var (List.remove_assoc v var_bindings) in
    List.concat_map
      (fun s ->
        match Re.exec_opt re_single_var s with
        | Some g -> get_var (Re.Group.get g 1)
        | None -> [expand_vars var_bindings s])
      s

and expand_vars =
  let re_var = Re.(compile re_var) in
  fun var_bindings s ->
    Re.replace ~all:true re_var
      ~f:(fun g -> String.concat " " (get_var var_bindings (Re.Group.get g 1)))
      s

module Nj = struct
  include Ninja_utils
end

let ( ! ) = ref

let all_vars =
  String.Set.of_list
    (List.map name
       [
         builddir;
         clerk_exe;
         catala_exe;
         runtime;
         tdir;
         src;
         dst;
         test_id;
         class_path;
         cat_files;
       ]
    @ List.map name [clerk_flags; catala_flags; includes])
