(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Shared_ast
open Catala_utils

type exception_tree =
  | Leaf of Dependency.ExceptionVertex.t
  | Node of exception_tree list * Dependency.ExceptionVertex.t

open Format

(* Original credits for this printing code: Jean-Christophe Filiâtre *)
let format_exception_tree (fmt : Format.formatter) (t : exception_tree) =
  let blue fmt n s =
    Format.fprintf fmt "@{<blue>%a@}" (fun fmt -> Format.pp_print_as fmt n) s
  in
  let rec print_node pref prefsz (t : exception_tree) =
    let label, sons =
      match t with
      | Leaf l -> l.Dependency.ExceptionVertex.label, []
      | Node (sons, l) -> l.Dependency.ExceptionVertex.label, sons
    in
    Format.fprintf fmt "\"%a\"" LabelName.format label;
    let w = String.width (fst (LabelName.get_info label)) + 2 in
    if sons != [] then
      let pref', prefsz' = pref ^ String.make (w + 1) ' ', prefsz + w + 2 in
      match sons with
      | [t'] ->
        blue fmt 3 "───";
        print_node (pref' ^ " ") (prefsz' + 1) t'
      | _ ->
        blue fmt 1 "─";
        print_sons pref' prefsz' "─┬──" sons
  and print_sons pref prefsz start = function
    | [] -> assert false
    | [s] ->
      blue fmt 4 " └──";
      print_node (pref ^ " ") (prefsz + 1) s
    | s :: sons ->
      blue fmt 4 start;
      print_node (pref ^ "| ") (prefsz + 2) s;
      pp_print_cut fmt ();
      blue fmt (prefsz + 2) (pref ^ " │");
      pp_print_cut fmt ();
      blue fmt prefsz pref;
      print_sons pref prefsz " ├──" sons
  in
  Format.pp_open_vbox fmt 0;
  print_node "" 0 t;
  Format.pp_close_box fmt ()

let build_exception_tree exc_graph =
  let base_cases =
    Dependency.ExceptionsDependencies.fold_vertex
      (fun v base_cases ->
        if Dependency.ExceptionsDependencies.out_degree exc_graph v = 0 then
          v :: base_cases
        else base_cases)
      exc_graph []
  in
  let rec build_tree (base_cases : Dependency.ExceptionVertex.t) =
    let exceptions =
      Dependency.ExceptionsDependencies.pred exc_graph base_cases
    in
    match exceptions with
    | [] -> Leaf base_cases
    | _ -> Node (List.map build_tree exceptions, base_cases)
  in
  List.map build_tree base_cases

let print_exceptions_graph
    (scope : ScopeName.t)
    (var : Ast.ScopeDef.t)
    (g : Dependency.ExceptionsDependencies.t) =
  Message.emit_result
    "Printing the tree of exceptions for the definitions of variable \"%a\" of \
     scope \"%a\"."
    Ast.ScopeDef.format var ScopeName.format scope;
  Dependency.ExceptionsDependencies.iter_vertex
    (fun ex ->
      Message.emit_result "@[<v>Definitions with label \"%a\":@,%a@]"
        LabelName.format ex.Dependency.ExceptionVertex.label
        (RuleName.Map.format_values Pos.format_loc_text)
        ex.Dependency.ExceptionVertex.rules)
    g;
  let tree = build_exception_tree g in
  Message.emit_result "@[<v>The exception tree structure is as follows:@,@,%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
       (fun fmt tree -> format_exception_tree fmt tree))
    tree
