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
  let blue fmt s =
    Format.fprintf fmt "@{<blue>%s@}" s
  in
  let rec print_node pref (t : exception_tree) =
    let label, sons = match t with
      | Leaf l -> l.Dependency.ExceptionVertex.label, []
      | Node (sons, l) -> l.Dependency.ExceptionVertex.label, sons
    in
    Format.fprintf fmt "@{<yellow>\"%a\"@}" LabelName.format_t label;
    let w = String.length (fst (LabelName.get_info label)) + 2 in
    if sons != [] then
      let pref' = pref ^ String.make (w + 1) ' ' in
      match sons with
      | [t'] ->
        blue fmt "───";
        print_node (pref' ^ " ") t'
      | _ ->
        blue fmt "──";
        print_sons pref' "─┬──" sons
  and print_sons pref start = function
    | [] -> assert false
    | [s] ->
      blue fmt " └──";
      print_node (pref ^ " ") s
    | s :: sons ->
      blue fmt start;
      print_node (pref ^ "| ") s;
      pp_print_cut fmt ();
      blue fmt (pref ^ " │");
      pp_print_cut fmt ();
      blue fmt pref;
      print_sons pref " ├──" sons
  in
  Format.pp_open_vbox fmt 0;
  print_node "" t;
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
  Messages.emit_result
    "Printing the tree of exceptions for the definitions of variable @{<yellow>\"%a\"@} of \
     scope @{<yellow>\"%a\"@}."
    Ast.ScopeDef.format_t var
    ScopeName.format_t scope;
  Dependency.ExceptionsDependencies.iter_vertex
    (fun ex ->
      Messages.emit_result "@[<v>Definitions with label @{<yellow>\"%a\"@}:@,%a@]"
        LabelName.format_t ex.Dependency.ExceptionVertex.label
        (Format.pp_print_list (fun fmt (_, pos) -> Pos.format_loc_text fmt pos))
        (RuleName.Map.bindings ex.Dependency.ExceptionVertex.rules))
    g;
  let tree = build_exception_tree g in
  Messages.emit_result "The exception tree structure is as follows:\n\n%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       (fun fmt tree -> format_exception_tree fmt tree))
    tree
