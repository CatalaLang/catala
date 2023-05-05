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
  let blue s =
    Format.asprintf "%a" (Cli.format_with_style [ANSITerminal.blue]) s
  in
  let rec print_node pref (t : exception_tree) =
    let (s, w), sons =
      let print_s s =
        ( Format.asprintf "%a"
            (Cli.format_with_style [ANSITerminal.yellow])
            (Format.asprintf "\"%a\"" LabelName.format_t
               s.Dependency.ExceptionVertex.label),
          String.length
            (Format.asprintf "\"%a\"" LabelName.format_t
               s.Dependency.ExceptionVertex.label) )
      in
      match t with Leaf s -> print_s s, [] | Node (sons, s) -> print_s s, sons
    in
    pp_print_string fmt s;
    if sons != [] then
      let pref' = pref ^ String.make (w + 1) ' ' in
      match sons with
      | [t'] ->
        pp_print_string fmt (blue "───");
        print_node (pref' ^ " ") t'
      | _ ->
        pp_print_string fmt (blue "──");
        print_sons pref' "─┬──" sons
  and print_sons pref start = function
    | [] -> assert false
    | [s] ->
      pp_print_string fmt (blue " └──");
      print_node (pref ^ " ") s
    | s :: sons ->
      pp_print_string fmt (blue start);
      print_node (pref ^ "| ") s;
      pp_force_newline fmt ();
      pp_print_string fmt (blue (pref ^ " │"));
      pp_force_newline fmt ();
      pp_print_string fmt (blue pref);
      print_sons pref " ├──" sons
  in
  print_node "" t

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
  Cli.result_format
    "Printing the tree of exceptions for the definitions of variable %a of \
     scope %a."
    (Cli.format_with_style [ANSITerminal.yellow])
    (Format.asprintf "\"%a\"" Ast.ScopeDef.format_t var)
    (Cli.format_with_style [ANSITerminal.yellow])
    (Format.asprintf "\"%a\"" ScopeName.format_t scope);
  Dependency.ExceptionsDependencies.iter_vertex
    (fun ex ->
      Cli.result_format "Definitions with label %a:\n%a"
        (Cli.format_with_style [ANSITerminal.yellow])
        (Format.asprintf "\"%a\"" LabelName.format_t
           ex.Dependency.ExceptionVertex.label)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
           (fun fmt (_, pos) ->
             Format.fprintf fmt "%s" (Pos.retrieve_loc_text pos)))
        (RuleName.Map.bindings ex.Dependency.ExceptionVertex.rules))
    g;
  let tree = build_exception_tree g in
  Cli.result_format "The exception tree structure is as follows:\n\n%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       (fun fmt tree -> format_exception_tree fmt tree))
    tree
