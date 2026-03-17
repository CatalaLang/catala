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

(* Returns the list of unique condition expressions for a vertex, deduplicated
   by their pretty-printed text. Empty if all rules are unconditional. *)
let conditions_of_vertex
    (lang : Global.backend_lang)
    (v : Dependency.ExceptionVertex.t) : Ast.expr list =
  RuleName.Map.values v.Dependency.ExceptionVertex.rules
  |> List.filter_map (fun (_, just_expr_opt) -> just_expr_opt)
  |> List.sort_uniq (fun e1 e2 ->
      String.compare
        (Format.asprintf "%a" (Print.UserFacing.expr lang) e1)
        (Format.asprintf "%a" (Print.UserFacing.expr lang) e2))

(* Renders a condition expression to a list of lines, wrapping at [width]
   columns. The stag functions from [fmt_outer] are forwarded so that color tags
   in the expression printer produce the same ANSI sequences as the outer
   formatter; this preserves syntax highlighting in the condition text. *)
let render_condition_lines lang width fmt_outer e =
  let buf = Buffer.create 80 in
  let inner = Format.formatter_of_buffer buf in
  Format.pp_set_formatter_stag_functions inner
    (Format.pp_get_formatter_stag_functions fmt_outer ());
  Format.pp_set_tags inner true;
  Format.pp_set_margin inner (max 20 width);
  Print.UserFacing.expr lang inner e;
  Format.pp_print_flush inner ();
  String.split_on_char '\n' (Buffer.contents buf)

(* Prints the exception tree vertically (one node per line) so that long
   condition labels don't break horizontal alignment. Each condition is printed
   on its own indented line below the node label; multi-line conditions have the
   tree's continuation prefix on every line so │ bars remain aligned. *)
let format_exception_tree (fmt : Format.formatter) (t : exception_tree) =
  let lang = Option.value Global.options.language ~default:Global.En in
  let margin = Format.pp_get_margin fmt () in
  let rec print_node prefix (t : exception_tree) =
    let label, vertex, children =
      match t with
      | Leaf l -> l.Dependency.ExceptionVertex.label, l, []
      | Node (children, l) -> l.Dependency.ExceptionVertex.label, l, children
    in
    Format.fprintf fmt "\"%a\"" LabelName.format label;
    (* For nodes with children, run a │ bar down through all condition lines to
       visually connect the label to the child connectors below. For leaves, use
       plain spaces. Continuation lines of multi-line conditions align under
       [. *)
    let has_children = children <> [] in
    let cond_width = max 20 (margin - String.length prefix - 3) in
    List.iter
      (fun e ->
        let lines = render_condition_lines lang cond_width fmt e in
        Format.pp_print_cut fmt ();
        if has_children then Format.fprintf fmt "@{<blue>%s│@} [" prefix
        else (
          Format.fprintf fmt "@{<blue>%s@}" prefix;
          Format.pp_print_string fmt "  [");
        (match lines with
        | [] -> ()
        | first :: rest ->
          Format.pp_print_string fmt first;
          List.iter
            (fun line ->
              Format.pp_print_cut fmt ();
              if has_children then (
                Format.fprintf fmt "@{<blue>%s│@}  " prefix;
                Format.pp_print_string fmt line)
              else (
                Format.fprintf fmt "@{<blue>%s@}" prefix;
                Format.pp_print_string fmt ("   " ^ line)))
            rest);
        Format.pp_print_string fmt "]")
      (conditions_of_vertex lang vertex);
    let last_idx = List.length children - 1 in
    List.iteri
      (fun i son ->
        Format.pp_print_cut fmt ();
        let connector = if i = last_idx then "└── " else "├── " in
        let continuation = if i = last_idx then "    " else "│   " in
        Format.fprintf fmt "@{<blue>%s%s@}" prefix connector;
        print_node (prefix ^ continuation) son)
      children
  in
  Format.pp_open_vbox fmt 0;
  print_node "" t;
  Format.pp_close_box fmt ()

let pos_to_json (pos : Pos.t) : Yojson.Safe.t =
  `Assoc
    [
      "file", `String (Pos.get_file pos);
      "start_line", `Int (Pos.get_start_line pos);
      "start_column", `Int (Pos.get_start_column pos);
      "end_line", `Int (Pos.get_end_line pos);
      "end_column", `Int (Pos.get_end_column pos);
    ]

let rec exception_tree_to_json (t : exception_tree) : Yojson.Safe.t =
  let vertex, children =
    match t with Leaf l -> l, [] | Node (children, l) -> l, children
  in
  let label = vertex.Dependency.ExceptionVertex.label in
  let rules =
    RuleName.Map.bindings vertex.Dependency.ExceptionVertex.rules
    |> List.map (fun (_rule_name, (rule_pos, just_expr_opt)) ->
        let lang = Option.value Global.options.language ~default:Global.En in
        `Assoc
          (("pos", pos_to_json rule_pos)
          ::
          (match just_expr_opt with
          | None -> []
          | Some e ->
            [
              "condition_pos", pos_to_json (Expr.pos e);
              ( "condition_text",
                `String (Format.asprintf "%a" (Print.UserFacing.expr lang) e) );
            ])))
  in
  `Assoc
    [
      "label", `String (fst (LabelName.get_info label));
      "rules", `List rules;
      "exceptions", `List (List.map exception_tree_to_json children);
    ]

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

let exceptions_graph
    (scope : ScopeName.t)
    (var : Ast.ScopeDef.t)
    (g : Dependency.ExceptionsDependencies.t) =
  Message.result
    "Printing the tree of exceptions for the definitions of variable \"%a\" of \
     scope \"%a\"."
    Ast.ScopeDef.format var ScopeName.format scope;
  Dependency.ExceptionsDependencies.iter_vertex
    (fun ex ->
      Message.result "Definitions with label@ \"%a\":" LabelName.format
        ex.Dependency.ExceptionVertex.label
        ~extra_pos:
          (List.concat_map
             (fun (rule_pos, just_expr_opt) ->
               ("", rule_pos)
               ::
               (match just_expr_opt with
               | None -> []
               | Some e -> ["under condition:", Expr.pos e]))
             (RuleName.Map.values ex.Dependency.ExceptionVertex.rules)))
    g;
  let tree = build_exception_tree g in
  Message.result "@[<v>The exception tree structure is as follows:@,@,%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
       (fun fmt tree -> format_exception_tree fmt tree))
    tree

let exceptions_graph_json
    (scope : ScopeName.t)
    (var : Ast.ScopeDef.t)
    (g : Dependency.ExceptionsDependencies.t) =
  let tree = build_exception_tree g in
  let json =
    `Assoc
      [
        "scope", `String (fst (ScopeName.get_info scope));
        "variable", `String (Format.asprintf "%a" Ast.ScopeDef.format var);
        "trees", `List (List.map exception_tree_to_json tree);
      ]
  in
  Yojson.Safe.to_channel stdout json;
  print_newline ()
