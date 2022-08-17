(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Scope dependencies computations using {{:http://ocamlgraph.lri.fr/}
    OCamlgraph} *)

open Utils
open Shared_ast

(** {1 Scope variables dependency graph} *)

(** {2 Graph declaration} *)

(** Vertices: scope variables or subscopes.

    The vertices of the scope dependency graph are either :

    - the variables of the scope ;
    - the subscopes of the scope.

    Indeed, during interpretation, subscopes are executed atomically. *)
module Vertex = struct
  type t =
    | Var of Ast.ScopeVar.t * Ast.StateName.t option
    | SubScope of SubScopeName.t

  let hash x =
    match x with
    | Var (x, None) -> Ast.ScopeVar.hash x
    | Var (x, Some sx) ->
      Int.logxor (Ast.ScopeVar.hash x) (Ast.StateName.hash sx)
    | SubScope x -> SubScopeName.hash x

  let compare = compare

  let equal x y =
    match x, y with
    | Var (x, None), Var (y, None) -> Ast.ScopeVar.compare x y = 0
    | Var (x, Some sx), Var (y, Some sy) ->
      Ast.ScopeVar.compare x y = 0 && Ast.StateName.compare sx sy = 0
    | SubScope x, SubScope y -> SubScopeName.compare x y = 0
    | _ -> false

  let format_t (fmt : Format.formatter) (x : t) : unit =
    match x with
    | Var (v, None) -> Ast.ScopeVar.format_t fmt v
    | Var (v, Some sv) ->
      Format.fprintf fmt "%a.%a" Ast.ScopeVar.format_t v Ast.StateName.format_t
        sv
    | SubScope v -> SubScopeName.format_t fmt v
end

(** On the edges, the label is the position of the expression responsible for
    the use of the variable. In the graph, [x -> y] if [x] is used in the
    definition of [y].*)
module Edge = struct
  type t = Pos.t

  let compare = compare
  let default = Pos.no_pos
end

module ScopeDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
(** Module of the graph, provided by OCamlGraph *)

module TopologicalTraversal = Graph.Topological.Make (ScopeDependencies)
(** Module of the topological traversal of the graph, provided by OCamlGraph *)

module SCC = Graph.Components.Make (ScopeDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** {2 Graph computations} *)

(** Returns an ordering of the scope variables and subscope compatible with the
    dependencies of the computation *)
let correct_computation_ordering (g : ScopeDependencies.t) : Vertex.t list =
  List.rev (TopologicalTraversal.fold (fun sd acc -> sd :: acc) g [])

(** Outputs an error in case of cycles. *)
let check_for_cycle (scope : Ast.scope) (g : ScopeDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < ScopeDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    let spans =
      List.flatten
        (List.map
           (fun v ->
             let var_str, var_info =
               match v with
               | Vertex.Var (v, None) ->
                 ( Format.asprintf "%a" Ast.ScopeVar.format_t v,
                   Ast.ScopeVar.get_info v )
               | Vertex.Var (v, Some sv) ->
                 ( Format.asprintf "%a.%a" Ast.ScopeVar.format_t v
                     Ast.StateName.format_t sv,
                   Ast.StateName.get_info sv )
               | Vertex.SubScope v ->
                 ( Format.asprintf "%a" SubScopeName.format_t v,
                   SubScopeName.get_info v )
             in
             let succs = ScopeDependencies.succ_e g v in
             let _, edge_pos, succ =
               List.find (fun (_, _, succ) -> List.mem succ scc) succs
             in
             let succ_str =
               match succ with
               | Vertex.Var (v, None) ->
                 Format.asprintf "%a" Ast.ScopeVar.format_t v
               | Vertex.Var (v, Some sv) ->
                 Format.asprintf "%a.%a" Ast.ScopeVar.format_t v
                   Ast.StateName.format_t sv
               | Vertex.SubScope v ->
                 Format.asprintf "%a" SubScopeName.format_t v
             in
             [
               ( Some ("Cycle variable " ^ var_str ^ ", declared:"),
                 Marked.get_mark var_info );
               ( Some
                   ("Used here in the definition of another cycle variable "
                   ^ succ_str
                   ^ ":"),
                 edge_pos );
             ])
           scc)
    in
    Errors.raise_multispanned_error spans
      "Cyclic dependency detected between variables of scope %a!"
      ScopeName.format_t scope.scope_uid

(** Builds the dependency graph of a particular scope *)
let build_scope_dependencies (scope : Ast.scope) : ScopeDependencies.t =
  let g = ScopeDependencies.empty in
  (* Add all the vertices to the graph *)
  let g =
    Ast.ScopeVarMap.fold
      (fun (v : Ast.ScopeVar.t) var_or_state g ->
        match var_or_state with
        | Ast.WholeVar -> ScopeDependencies.add_vertex g (Vertex.Var (v, None))
        | Ast.States states ->
          List.fold_left
            (fun g state ->
              ScopeDependencies.add_vertex g (Vertex.Var (v, Some state)))
            g states)
      scope.scope_vars g
  in
  let g =
    Scopelang.Ast.SubScopeMap.fold
      (fun (v : SubScopeName.t) _ g ->
        ScopeDependencies.add_vertex g (Vertex.SubScope v))
      scope.scope_sub_scopes g
  in
  let g =
    Ast.ScopeDefMap.fold
      (fun def_key scope_def g ->
        let def = scope_def.Ast.scope_def_rules in
        let fv = Ast.free_variables def in
        Ast.ScopeDefMap.fold
          (fun fv_def fv_def_pos g ->
            match def_key, fv_def with
            | ( Ast.ScopeDef.Var (v_defined, s_defined),
                Ast.ScopeDef.Var (v_used, s_used) ) ->
              (* simple case *)
              if v_used = v_defined && s_used = s_defined then
                (* variable definitions cannot be recursive *)
                Errors.raise_spanned_error fv_def_pos
                  "The variable %a is used in one of its definitions, but \
                   recursion is forbidden in Catala"
                  Ast.ScopeDef.format_t def_key
              else
                let edge =
                  ScopeDependencies.E.create
                    (Vertex.Var (v_used, s_used))
                    fv_def_pos
                    (Vertex.Var (v_defined, s_defined))
                in
                ScopeDependencies.add_edge_e g edge
            | ( Ast.ScopeDef.SubScopeVar (defined, _),
                Ast.ScopeDef.Var (v_used, s_used) ) ->
              (* here we are defining the input of a subscope using a var of the
                 scope *)
              let edge =
                ScopeDependencies.E.create
                  (Vertex.Var (v_used, s_used))
                  fv_def_pos (Vertex.SubScope defined)
              in
              ScopeDependencies.add_edge_e g edge
            | ( Ast.ScopeDef.SubScopeVar (defined, _),
                Ast.ScopeDef.SubScopeVar (used, _) ) ->
              (* here we are defining the input of a scope with the output of
                 another subscope *)
              if used = defined then
                (* subscopes are not recursive functions *)
                Errors.raise_spanned_error fv_def_pos
                  "The subscope %a is used when defining one of its inputs, \
                   but recursion is forbidden in Catala"
                  SubScopeName.format_t defined
              else
                let edge =
                  ScopeDependencies.E.create (Vertex.SubScope used) fv_def_pos
                    (Vertex.SubScope defined)
                in
                ScopeDependencies.add_edge_e g edge
            | ( Ast.ScopeDef.Var (v_defined, s_defined),
                Ast.ScopeDef.SubScopeVar (used, _) ) ->
              (* finally we define a scope var with the output of a subscope *)
              let edge =
                ScopeDependencies.E.create (Vertex.SubScope used) fv_def_pos
                  (Vertex.Var (v_defined, s_defined))
              in
              ScopeDependencies.add_edge_e g edge)
          fv g)
      scope.scope_defs g
  in
  g

(** {1 Exceptions dependency graph} *)

(** {2 Graph declaration} *)

module ExceptionVertex = struct
  include Ast.RuleSet

  let hash (x : t) : int =
    Ast.RuleSet.fold (fun r acc -> Int.logxor (Ast.RuleName.hash r) acc) x 0

  let equal x y = compare x y = 0
end

module EdgeExceptions = struct
  type t = Pos.t list

  let compare = compare
  let default = [Pos.no_pos]
end

module ExceptionsDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (ExceptionVertex)
    (EdgeExceptions)
(** Module of the graph, provided by OCamlGraph. [x -> y] if [y] is an exception
    to [x] *)

module ExceptionsSCC = Graph.Components.Make (ExceptionsDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** {2 Graph computations} *)

type exception_edge = {
  label_from : Ast.LabelName.t;
  label_to : Ast.LabelName.t;
  edge_positions : Pos.t list;
}

let build_exceptions_graph
    (def : Ast.rule Ast.RuleMap.t)
    (def_info : Ast.ScopeDef.t) : ExceptionsDependencies.t =
  (* First we partition the definitions into groups bearing the same label. To
     handle the rules that were not labeled by the user, we create implicit
     labels. *)

  (* All the rules of the form [definition x ...] are base case with no explicit
     label, so they should share this implicit label. *)
  let base_case_implicit_label =
    Ast.LabelName.fresh ("base_case", Pos.no_pos)
  in
  (* When declaring [exception definition x ...], it means there is a unique
     rule [R] to which this can be an exception to. So we give a unique label to
     all the rules that are implicitly exceptions to rule [R]. *)
  let exception_to_rule_implicit_labels : Ast.LabelName.t Ast.RuleMap.t =
    Ast.RuleMap.fold
      (fun _ rule_from exception_to_rule_implicit_labels ->
        match rule_from.Ast.rule_exception with
        | Ast.ExceptionToRule (rule_to, _) -> (
          match
            Ast.RuleMap.find_opt rule_to exception_to_rule_implicit_labels
          with
          | Some _ ->
            (* we already created the label *) exception_to_rule_implicit_labels
          | None ->
            Ast.RuleMap.add rule_to
              (Ast.LabelName.fresh
                 ( "exception_to_"
                   ^ Marked.unmark (Ast.RuleName.get_info rule_to),
                   Pos.no_pos ))
              exception_to_rule_implicit_labels)
        | _ -> exception_to_rule_implicit_labels)
      def Ast.RuleMap.empty
  in
  (* When declaring [exception foo_l definition x ...], the rule is exception to
     all the rules sharing label [foo_l]. So we give a unique label to all the
     rules that are implicitly exceptions to rule [foo_l]. *)
  let exception_to_label_implicit_labels : Ast.LabelName.t Ast.LabelMap.t =
    Ast.RuleMap.fold
      (fun _ rule_from
           (exception_to_label_implicit_labels : Ast.LabelName.t Ast.LabelMap.t) ->
        match rule_from.Ast.rule_exception with
        | Ast.ExceptionToLabel (label_to, _) -> (
          match
            Ast.LabelMap.find_opt label_to exception_to_label_implicit_labels
          with
          | Some _ ->
            (* we already created the label *)
            exception_to_label_implicit_labels
          | None ->
            Ast.LabelMap.add label_to
              (Ast.LabelName.fresh
                 ( "exception_to_"
                   ^ Marked.unmark (Ast.LabelName.get_info label_to),
                   Pos.no_pos ))
              exception_to_label_implicit_labels)
        | _ -> exception_to_label_implicit_labels)
      def Ast.LabelMap.empty
  in

  (* Now we have all the labels necessary to partition our rules into sets, each
     one corresponding to a label relating to the structure of the exception
     DAG. *)
  let label_to_rule_sets =
    Ast.RuleMap.fold
      (fun rule_name rule rule_sets ->
        let label_of_rule =
          match rule.Ast.rule_label with
          | Ast.ExplicitlyLabeled (l, _) -> l
          | Ast.Unlabeled -> (
            match rule.Ast.rule_exception with
            | BaseCase -> base_case_implicit_label
            | ExceptionToRule (r, _) ->
              Ast.RuleMap.find r exception_to_rule_implicit_labels
            | ExceptionToLabel (l', _) ->
              Ast.LabelMap.find l' exception_to_label_implicit_labels)
        in
        Ast.LabelMap.update label_of_rule
          (fun rule_set ->
            match rule_set with
            | None -> Some (Ast.RuleSet.singleton rule_name)
            | Some rule_set -> Some (Ast.RuleSet.add rule_name rule_set))
          rule_sets)
      def Ast.LabelMap.empty
  in
  let find_label_of_rule (r : Ast.RuleName.t) : Ast.LabelName.t =
    fst
      (Ast.LabelMap.choose
         (Ast.LabelMap.filter
            (fun _ rule_set -> Ast.RuleSet.mem r rule_set)
            label_to_rule_sets))
  in
  (* Next, we collect the exception edges between those groups of rules referred
     by their labels. This is also at this step that we check consistency of the
     edges as they are declared at each rule but should be the same for all the
     rules of the same group. *)
  let exception_edges : exception_edge list =
    Ast.RuleMap.fold
      (fun rule_name rule exception_edges ->
        let label_from = find_label_of_rule rule_name in
        let label_to_and_pos =
          match rule.Ast.rule_exception with
          | Ast.BaseCase -> None
          | Ast.ExceptionToRule (r', pos) -> Some (find_label_of_rule r', pos)
          | Ast.ExceptionToLabel (l', pos) -> Some (l', pos)
        in
        match label_to_and_pos with
        | None -> exception_edges
        | Some (label_to, edge_pos) -> (
          let other_edges_originating_from_same_label =
            List.filter
              (fun edge -> Ast.LabelName.compare edge.label_from label_from = 0)
              exception_edges
          in
          (* We check the consistency*)
          if Ast.LabelName.compare label_from label_to = 0 then
            Errors.raise_spanned_error edge_pos
              "Cannot define rule as an exception to itself";
          List.iter
            (fun edge ->
              if Ast.LabelName.compare edge.label_to label_to <> 0 then
                Errors.raise_multispanned_error
                  (( Some
                       "This declaration contradicts another exception \
                        declarations:",
                     edge_pos )
                  :: List.map
                       (fun pos ->
                         Some "Here is another exception declaration:", pos)
                       edge.edge_positions)
                  "The declaration of exceptions are inconsistent for variable \
                   %a."
                  Ast.ScopeDef.format_t def_info)
            other_edges_originating_from_same_label;
          (* Now we add the edge to the list*)
          let existing_edge =
            List.find_opt
              (fun edge ->
                Ast.LabelName.compare edge.label_from label_from = 0
                && Ast.LabelName.compare edge.label_to label_to = 0)
              exception_edges
          in
          match existing_edge with
          | None ->
            { label_from; label_to; edge_positions = [edge_pos] }
            :: exception_edges
          | Some existing_edge ->
            {
              label_from;
              label_to;
              edge_positions = edge_pos :: existing_edge.edge_positions;
            }
            :: List.filter (fun edge -> edge <> existing_edge) exception_edges))
      def []
  in
  (* We've got the vertices and the edges, let's build the graph! *)
  let g =
    Ast.LabelMap.fold
      (fun _label rule_set g -> ExceptionsDependencies.add_vertex g rule_set)
      label_to_rule_sets ExceptionsDependencies.empty
  in
  (* then we add the edges *)
  let g =
    List.fold_left
      (fun g edge ->
        let rule_group_from =
          Ast.LabelMap.find edge.label_from label_to_rule_sets
        in
        let rule_group_to =
          Ast.LabelMap.find edge.label_to label_to_rule_sets
        in
        let edge =
          ExceptionsDependencies.E.create rule_group_from edge.edge_positions
            rule_group_to
        in
        ExceptionsDependencies.add_edge_e g edge)
      g exception_edges
  in
  g

(** Outputs an error in case of cycles. *)
let check_for_exception_cycle (g : ExceptionsDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = ExceptionsSCC.scc_list g in
  if List.length sccs < ExceptionsDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    let spans =
      List.flatten
        (List.map
           (fun (vs : Ast.RuleSet.t) ->
             let v = Ast.RuleSet.choose vs in
             let var_str, var_info =
               ( Format.asprintf "%a" Ast.RuleName.format_t v,
                 Ast.RuleName.get_info v )
             in
             let succs = ExceptionsDependencies.succ_e g vs in
             let _, edge_pos, _ =
               List.find (fun (_, _, succ) -> List.mem succ scc) succs
             in
             [
               ( Some
                   ("Cyclic exception for definition of variable \""
                   ^ var_str
                   ^ "\", declared here:"),
                 Marked.get_mark var_info );
               ( Some
                   ("Used here in the definition of another cyclic exception \
                     for defining \""
                   ^ var_str
                   ^ "\":"),
                 List.hd edge_pos );
             ])
           scc)
    in
    Errors.raise_multispanned_error spans
      "Cyclic dependency detected between exceptions!"
