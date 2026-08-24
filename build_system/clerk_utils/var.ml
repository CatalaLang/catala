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
module Nj = Ninja_utils
include Nj.Var

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

type bindings = Nj.Binding.any list

let has_ref = Re.execp (Re.compile re_var)

let binding_of_words (type a) (v : a t) (words : string list) : Nj.Binding.any =
  match v with
  (* the CLI splits values on spaces before kinds are known; a scalar is one
     value, so rejoin (lossless: the split was on a single space) *)
  | Scalar _ -> Nj.Binding.make v (String.concat " " words)
  | Vector _ ->
    Nj.Binding.make v (List.map (fun w -> Ninja_utils.Expr.Word w) words)

(* border guards: overrides only — authored defaults legitimately contain
   refs. Refs would expand in direct exec but quote-glue at emission; reject
   rather than diverge. Composition would need an append form (--vars X+=y),
   not implemented. *)
let binding_of_words_override (type a) (v : a t) (words : string list) :
    Nj.Binding.any =
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
  binding_of_words v words

let binding_to_words (bnd : Nj.Binding.any) : string list =
  let expr_words e =
    List.concat_map
      (function
        | Ninja_utils.Expr.Word w -> [w]
        | Splice v -> [Printf.sprintf "${%s}" (name v)]
        | Raw s -> [s])
      e
  in
  match bnd with Any (Scalar _, x) -> [x] | Any (Vector _, x) -> expr_words x

let env_of_bindings bs =
  List.map
    (fun (Ninja_utils.Binding.Any (v, _) as b) -> name v, binding_to_words b)
    bs

let rec take_binding : type a. bindings -> a t -> bindings * Nj.Expr.t option =
 fun bindings var ->
  match var, bindings with
  | _, [] -> [], None
  | Scalar n1, Nj.Binding.Any (Scalar n2, value) :: r when n1 = n2 ->
    r, Some [Word value]
  | Vector n1, Nj.Binding.Any (Vector n2, value) :: r when n1 = n2 ->
    r, Some value
  | _, bnd :: r ->
    let bindings, ret = take_binding r var in
    bnd :: bindings, ret

let rec get : type a. bindings -> a t -> string list =
 fun var_bindings v ->
  let var_bindings, exp_opt = take_binding var_bindings v in
  let exp =
    match exp_opt with
    | Some exp -> exp
    | None ->
      Message.error
        "Clerk configuration error: variable @{<blue;bold>$%s@} is undefined"
        (name v)
  in
  expr_to_list ~var_bindings exp

and expand =
  let re_var = Re.(compile re_var) in
  fun var_bindings s ->
    Re.replace ~all:true re_var
      ~f:(fun g ->
        let var = Scalar (Re.Group.get g 1) in
        String.concat " " (get var_bindings var))
      s

and expr_elt_to_list ?(var_bindings = []) e =
  match e with
  | Nj.Expr.Word s -> [expand var_bindings s]
  | Nj.Expr.Splice v -> get var_bindings v
  | Nj.Expr.Raw s -> [s]

and expr_to_list ?(var_bindings = []) exp =
  List.concat_map (expr_elt_to_list ~var_bindings) exp

let expr_elt_to_string ?var_bindings elt =
  String.concat " " (expr_elt_to_list ?var_bindings elt)

(* cmd has no [cat]: [copy /b "a"+"b" out]. Quoted per element — the whole list
   quoted is one file name, unquoted breaks on spaces. *)
let cmd_concat_operand files =
  String.concat "+" (List.map (fun f -> "\"" ^ f ^ "\"") ("nul" :: files))

module Op = struct
  let ( ! ) = ref

  (* Crutch: these expand to text that must not be quoted (ninja escapes
     in/out, [cat_files] carries its own quotes). Belongs in the type. *)
  let ( !! ) : type a. a t -> Ninja_utils.Expr.elt = function
    | Scalar _ as v ->
      if v = input || v = output || v = cat_files then Raw !v else Word !v
    | Vector _ as v -> Splice v
end

include Op
