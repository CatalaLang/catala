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
let catala_includes = make_vector "CATALA_INCLUDES"
let runtime = make_scalar "CATALA_RUNTIME"

(* Definition spreading different rules *)

let tdir = make_scalar "tdir"

(* Rule vars, Used in specific rules *)

let input = make_scalar "in"
let output = make_scalar "out"
let src = make_scalar "src"
let dst = make_scalar "dst"
let rspfile = make_scalar "rspfile"
let rspfile_content = make_scalar "rspfile_content"
let test_files = make_scalar "test_files" (* Useful on Windows only *)

(* let scope = make "scope" *)
let test_id = make_scalar "test-id"

let re_var =
  let open Re in
  seq [str "${"; group (rep1 (diff any (char '}'))); char '}']

type bindings = Nj.Binding.any list

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

let expr_of_list s =
  let re = Re.(compile (whole_string re_var)) in
  List.map
    (fun s ->
      match Re.exec_opt re s with
      | Some g -> Nj.Expr.Splice (Vector (Re.Group.get g 1))
      | None -> Word s)
    s

let raw_replace f s =
  Re.replace ~all:true (Re.compile re_var) ~f:(fun g -> f (Re.Group.get g 1)) s

let take_binding bindings var =
  let rec aux : type a.
      bindings -> bindings -> a t -> bindings * Nj.Expr.t option =
   fun acc bindings var ->
    match var, bindings with
    | _, [] -> acc, None
    | Scalar n1, Nj.Binding.Any (Scalar n2, value) :: r when n1 = n2 ->
      List.rev_append r acc, Some [Word value]
    | Vector n1, Nj.Binding.Any (Vector n2, value) :: r when n1 = n2 ->
      List.rev_append r acc, Some value
    | _, bnd :: r -> aux (bnd :: acc) r var
  in
  aux [] (List.rev bindings) var

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

and expand var_bindings s =
  raw_replace
    (fun vname -> String.concat " " (get var_bindings (Scalar vname)))
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

(* '|' can't occur in a Windows path: no quoting, unlike ninja's [$in_newline] *)
let file_list_sep = '|'

let file_list_to_string files =
  String.concat (String.make 1 file_list_sep) files

let file_list_of_string s =
  List.filter (( <> ) "") (String.split_on_char file_list_sep (String.trim s))

let def (type a) ~overrides (var : a t) (default_value : a Lazy.t) :
    Nj.Binding.any =
  let rec findq vname = function
    | (v1, x) :: r when vname = v1 -> Some (x, r)
    | _ :: r -> findq vname r
    | [] -> None
  in
  let var_ref vname = "${" ^ vname ^ "}" in
  match var with
  | Scalar vname ->
    let rec value overrides =
      match findq vname overrides with
      | None -> Lazy.force default_value
      | Some (words, overrides) ->
        let s =
          match words with
          | [s] -> s
          | _ ->
            Message.warning
              "Multiple items given in the definition of the build variable@ \
               @{<cyan>%s@}:@ they will be concatenated into a single string \
               because this variable is atomic."
              vname;
            String.concat " " words
        in
        raw_replace
          (fun v ->
            if v = vname then value overrides
            else var_ref v (* leave unchanged *))
          s
    in
    Nj.Binding.make var (value overrides)
  | Vector vname ->
    let rec value overrides =
      match findq vname overrides with
      | None -> Lazy.force default_value
      | Some (words, overrides) ->
        let vref = var_ref vname in
        let vref_re = Re.(compile (str vref)) in
        List.concat_map
          (fun w ->
            if w = "" then []
            else if Re.execp vref_re w then
              if String.length w = String.length vref then value overrides
              else (
                Message.warning
                  "In build variable@ @{<cyan>%s@},@ the@ self-reference in@ \
                   @{<cyan>%S@}@ will@ be@ ignored:@ this is a vector-kind \
                   variable and the reference is within a string."
                  vname w;
                [Nj.Expr.Word w])
            else [Nj.Expr.Word w])
          words
    in
    Nj.Binding.make var (value overrides)

module Op = struct
  let ( ! ) = ref

  (* Crutch: these expand to text that must not be quoted (ninja escapes
     in/out). Belongs in the type. *)
  let ( !! ) : type a. a t -> Ninja_utils.Expr.elt = function
    | Scalar _ as v -> if v = input || v = output then Raw !v else Word !v
    | Vector _ as v -> Splice v
end

include Op
