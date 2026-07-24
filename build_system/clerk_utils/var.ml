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

let ninja_required_version = make_scalar "ninja_required_version"
let builddir = make_scalar "builddir"
let clerk_exe = make_scalar "CLERK_EXE"
let clerk_flags = make_vector "CLERK_FLAGS"
let catala_exe = make_scalar "CATALA_EXE"
let catala_flags = make_vector "CATALA_FLAGS"
let runtime = make_scalar "CATALA_RUNTIME"

(* Definition spreading different rules *)

let tdir = make_scalar "tdir"
let includes = make_vector "includes"

(* Rule vars, Used in specific rules *)

let input = make_scalar "in"
let output = make_scalar "out"
let src = make_scalar "src"
let dst = make_scalar "dst"
let class_path = make_scalar "class_path"
let cat_files = make_scalar "cat_files" (* Useful on Windows only *)

(* let scope = make "scope" *)
let test_id = make_scalar "test-id"

let re_var =
  let open Re in
  seq [str "${"; group (rep1 (diff any (char '}'))); char '}']

type bindings = (string * string list) list

let has_ref = Re.execp (Re.compile re_var)

let of_words (type a) (v : a t) (words : string list) : a =
  match kind v with
  | Scalar -> (
    match words with
    | [w] -> w
    | ws ->
      Message.error "Variable @{<blue;bold>$%s@} expects a single word, got %d"
        (name v) (List.length ws))
  | Vector -> List.map (fun w -> Ninja_utils.Expr.Word w) words

(* border guards: overrides only — authored defaults legitimately contain
   refs. Refs would expand in direct exec but quote-glue at emission; reject
   rather than diverge (composition is served by the append form). *)
let of_override_words (type a) (v : a t) (words : string list) : a =
  List.iter
    (fun w ->
      if has_ref w then
        Message.error
          "Invalid word %S in the value of variable @{<blue;bold>$%s@}: \
           variable references are not supported in overrides"
          w (name v);
      if String.contains w '"' then
        Message.error
          "Invalid word %S in the value of variable @{<blue;bold>$%s@}: quote \
           characters are not supported (values are quoted automatically; for \
           C string macros, prefer an included header)"
          w (name v))
    words;
  of_words v words

let to_words (type a) (v : a t) (x : a) : string list =
  let expr_words e =
    List.concat_map
      (function
        | Ninja_utils.Expr.Word w -> [w]
        | Splice v -> [Printf.sprintf "${%s}" (name v)]
        | Raw s -> [s])
      e
  in
  match kind v with Scalar -> [x] | Vector -> expr_words x

let env_of_bindings bs =
  List.map (fun (Ninja_utils.Binding.Any (v, x)) -> name v, to_words v x) bs

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
