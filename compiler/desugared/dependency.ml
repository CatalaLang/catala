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

(** Scope dependencies computations using
    {{:http://ocamlgraph.lri.fr/} OCamlgraph} *)

open Catala_utils
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
    | Var of ScopeVar.t * StateName.t option
    | Assertion of Ast.AssertionName.t

  let hash x =
    match x with
    | Var (x, None) -> ScopeVar.id x
    | Var (x, Some sx) -> Hashtbl.hash (ScopeVar.id x, StateName.id sx)
    | Assertion a -> Hashtbl.hash (`Assert (Ast.AssertionName.id a))

  let compare x y =
    match x, y with
    | Var (x, xst), Var (y, yst) -> (
      match ScopeVar.compare x y with
      | 0 -> Option.compare StateName.compare xst yst
      | n -> n)
    | Assertion a, Assertion b -> Ast.AssertionName.compare a b
    | Var _, _ -> -1
    | _, Var _ -> 1

  let equal x y =
    match x, y with
    | Var (x, sx), Var (y, sy) ->
      ScopeVar.equal x y && Option.equal StateName.equal sx sy
    | Assertion a, Assertion b -> Ast.AssertionName.equal a b
    | (Var _ | Assertion _), _ -> false

  let format (fmt : Format.formatter) (x : t) : unit =
    match x with
    | Var (v, None) -> ScopeVar.format fmt v
    | Var (v, Some sv) ->
      Format.fprintf fmt "%a@%a" ScopeVar.format v StateName.format sv
    | Assertion a -> Ast.AssertionName.format fmt a

  let info = function
    | Var (v, None) -> ScopeVar.get_info v
    | Var (_, Some sv) -> StateName.get_info sv
    | Assertion a -> Ast.AssertionName.get_info a
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
  (* if there is a cycle, there will be a strongly connected component of
     cardinality > 1 *)
  let sccs = SCC.scc_list g in
  match List.find_opt (function [] | [_] -> false | _ -> true) sccs with
  | None -> ()
  | Some [] -> assert false
  | Some (v0 :: _ as scc) ->
    let module VSet = Set.Make (Vertex) in
    let scc = VSet.of_list scc in
    let rec get_cycle cycle cycle_set v =
      let cycle = v :: cycle in
      let cycle_set = VSet.add v cycle_set in
      let succ = ScopeDependencies.succ g v in
      if List.exists (fun v -> VSet.mem v cycle_set) succ then
        (* a cycle may be smaller than the scc, in that case we just return the
           first one found *)
        let rec cut_after acc = function
          | [] -> acc
          | v :: vs ->
            if List.mem v succ then v :: acc else cut_after (v :: acc) vs
        in
        cut_after [] cycle
      else
        get_cycle cycle cycle_set
          (List.find (fun succ -> VSet.mem succ scc) succ)
    in
    let cycle = get_cycle [] VSet.empty v0 in
    let extra_pos =
      List.map2
        (fun v1 v2 ->
          let msg =
            Format.asprintf "%a is used here in the definition of %a:"
              Vertex.format v1 Vertex.format v2
          in
          let _, edge_pos, _ = ScopeDependencies.find_edge g v1 v2 in
          msg, edge_pos)
        cycle
        (List.tl cycle @ [List.hd cycle])
    in
    Message.error ~extra_pos
      "Cyclic dependency detected between the following variables of scope \
       %a:@ @[<hv>%a@]"
      ScopeName.format scope.scope_uid
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " →@ ")
         Vertex.format)
      (cycle @ [List.hd cycle])

(** Builds the dependency graph of a particular scope *)
let build_scope_dependencies (scope : Ast.scope) : ScopeDependencies.t =
  let g = ScopeDependencies.empty in
  (* Add all the vertices to the graph *)
  let g =
    ScopeVar.Map.fold
      (fun (v : ScopeVar.t) var_or_state g ->
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
    ScopeVar.Map.fold
      (fun (v : ScopeVar.t) _ g ->
        ScopeDependencies.add_vertex g (Vertex.Var (v, None)))
      scope.scope_sub_scopes g
  in
  let g =
    Ast.AssertionName.Map.fold
      (fun a _ g -> ScopeDependencies.add_vertex g (Vertex.Assertion a))
      scope.scope_assertions g
  in
  (* then add the edges *)
  let g =
    let to_vertex (var, kind) =
      match kind with
      | Ast.ScopeDef.Var st -> Vertex.Var (Mark.remove var, st)
      | Ast.ScopeDef.SubScopeInput _ -> Vertex.Var (Mark.remove var, None)
    in
    Ast.ScopeDef.Map.fold
      (fun def_key scope_def g ->
        let def = scope_def.Ast.scope_def_rules in
        let v_defined = to_vertex def_key in
        let fv = Ast.free_variables def in
        Ast.ScopeDef.Map.fold
          (fun fv_def fv_def_pos g ->
            let v_used = to_vertex fv_def in
            let () =
              if Vertex.equal v_used v_defined then
                match def_key with
                | _, Ast.ScopeDef.Var _ ->
                  Message.error ~pos:fv_def_pos
                    "The variable@ %a@ is@ used@ in@ one@ of@ its@ \
                     definitions@ (Catala doesn't support recursion)."
                    Ast.ScopeDef.format def_key
                | v, Ast.ScopeDef.SubScopeInput _ ->
                  Message.error ~pos:fv_def_pos
                    "The subscope@ %a@ is@ used@ in@ the@ definition@ of@ its@ \
                     own@ input@ %a@ (Catala doesn't support recursion)."
                    ScopeVar.format (Mark.remove v) Ast.ScopeDef.format def_key
            in
            ScopeDependencies.add_edge_e g
              (ScopeDependencies.E.create v_used fv_def_pos v_defined))
          fv g)
      scope.scope_defs g
  in
  let g =
    Ast.AssertionName.Map.fold
      (fun a_name a g ->
        let used_vars = Ast.locations_used (Expr.unbox a) in
        Ast.LocationSet.fold
          (fun used_var g ->
            let edge_from =
              match Mark.remove used_var with
              | DesugaredScopeVar { name; state } ->
                Some (Vertex.Var (Mark.remove name, state))
              | ToplevelVar _ -> None
              (* we don't add this dependency because toplevel definitions are
                 outside the scope *)
            in
            match edge_from with
            | None -> g
            | Some edge_from ->
              let edge =
                ScopeDependencies.E.create edge_from (Expr.pos a)
                  (Vertex.Assertion a_name)
              in
              ScopeDependencies.add_edge_e g edge)
          used_vars g)
      scope.scope_assertions g
  in
  g

(** {1 Exceptions dependency graph} *)

(** {2 Graph declaration} *)

module ExceptionVertex = struct
  type t = { rules : Pos.t RuleName.Map.t; label : LabelName.t }

  let compare x y =
    RuleName.Map.compare
      (fun _ _ -> 0 (* we don't care about positions here*))
      x.rules y.rules

  let hash (x : t) : int =
    RuleName.Map.fold
      (fun r _ acc -> Hashtbl.hash (RuleName.id r, acc))
      x.rules 0

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
  label_from : LabelName.t;
  label_to : LabelName.t;
  edge_positions : Pos.t list;
}

let build_exceptions_graph
    (def : Ast.rule RuleName.Map.t)
    (def_info : Ast.ScopeDef.t) : ExceptionsDependencies.t =
  (* First we partition the definitions into groups bearing the same label. To
     handle the rules that were not labeled by the user, we create implicit
     labels. *)

  (* All the rules of the form [definition x ...] are base case with no explicit
     label, so they should share this implicit label. *)
  let base_case_implicit_label = LabelName.fresh ("base_case", Pos.no_pos) in
  (* When declaring [exception definition x ...], it means there is a unique
     rule [R] to which this can be an exception to. So we give a unique label to
     all the rules that are implicitly exceptions to rule [R]. *)
  let exception_to_rule_implicit_labels : LabelName.t RuleName.Map.t =
    RuleName.Map.fold
      (fun _ rule_from exception_to_rule_implicit_labels ->
        match rule_from.Ast.rule_exception with
        | Ast.ExceptionToRule (rule_to, _) -> (
          match
            RuleName.Map.find_opt rule_to exception_to_rule_implicit_labels
          with
          | Some _ ->
            (* we already created the label *) exception_to_rule_implicit_labels
          | None ->
            RuleName.Map.add rule_to
              (LabelName.fresh
                 ("exception_to_" ^ RuleName.to_string rule_to, Pos.no_pos))
              exception_to_rule_implicit_labels)
        | _ -> exception_to_rule_implicit_labels)
      def RuleName.Map.empty
  in
  (* When declaring [exception foo_l definition x ...], the rule is exception to
     all the rules sharing label [foo_l]. So we give a unique label to all the
     rules that are implicitly exceptions to rule [foo_l]. *)
  let exception_to_label_implicit_labels : LabelName.t LabelName.Map.t =
    RuleName.Map.fold
      (fun _ rule_from
           (exception_to_label_implicit_labels : LabelName.t LabelName.Map.t) ->
        match rule_from.Ast.rule_exception with
        | Ast.ExceptionToLabel (label_to, _) -> (
          match
            LabelName.Map.find_opt label_to exception_to_label_implicit_labels
          with
          | Some _ ->
            (* we already created the label *)
            exception_to_label_implicit_labels
          | None ->
            LabelName.Map.add label_to
              (LabelName.fresh
                 ("exception_to_" ^ LabelName.to_string label_to, Pos.no_pos))
              exception_to_label_implicit_labels)
        | _ -> exception_to_label_implicit_labels)
      def LabelName.Map.empty
  in

  (* Now we have all the labels necessary to partition our rules into sets, each
     one corresponding to a label relating to the structure of the exception
     DAG. *)
  let label_to_rule_sets =
    RuleName.Map.fold
      (fun rule_name rule rule_sets ->
        let label_of_rule =
          match rule.Ast.rule_label with
          | Ast.ExplicitlyLabeled (l, _) -> l
          | Ast.Unlabeled -> (
            match rule.Ast.rule_exception with
            | BaseCase -> base_case_implicit_label
            | ExceptionToRule (r, _) ->
              RuleName.Map.find r exception_to_rule_implicit_labels
            | ExceptionToLabel (l', _) ->
              LabelName.Map.find l' exception_to_label_implicit_labels)
        in
        LabelName.Map.update label_of_rule
          (fun rule_set ->
            let pos =
              (* We have to overwrite the law info on tis position because the
                 pass at the surface AST level that fills the law info on
                 positions only does it for positions inside expressions, the
                 visitor in [surface/fill_positions.ml] does not go into the
                 info of [RuleName.t], etc. Related issue:
                 https://github.com/CatalaLang/catala/issues/194 *)
              Pos.overwrite_law_info
                (snd (RuleName.get_info rule.rule_id))
                (Pos.get_law_info (Expr.pos rule.rule_just))
            in
            match rule_set with
            | None -> Some (RuleName.Map.singleton rule_name pos)
            | Some rule_set -> Some (RuleName.Map.add rule_name pos rule_set))
          rule_sets)
      def LabelName.Map.empty
  in
  let find_label_of_rule (r : RuleName.t) : LabelName.t =
    fst
      (LabelName.Map.choose
         (LabelName.Map.filter
            (fun _ rule_set -> RuleName.Map.mem r rule_set)
            label_to_rule_sets))
  in
  (* Next, we collect the exception edges between those groups of rules referred
     by their labels. This is also at this step that we check consistency of the
     edges as they are declared at each rule but should be the same for all the
     rules of the same group. *)
  let exception_edges : exception_edge list =
    RuleName.Map.fold
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
              (fun edge -> LabelName.compare edge.label_from label_from = 0)
              exception_edges
          in
          (* We check the consistency*)
          if LabelName.compare label_from label_to = 0 then
            Message.error ~pos:edge_pos
              "Cannot define rule as an exception to itself";
          List.iter
            (fun edge ->
              if LabelName.compare edge.label_to label_to <> 0 then
                Message.error ~pos:edge_pos
                  ~pos_msg:(fun ppf ->
                    Format.pp_print_text ppf
                      "This definition contradicts other exception definitions:")
                  ~extra_pos:
                    (List.map
                       (fun pos -> "Other exception definition:", pos)
                       edge.edge_positions)
                  "The definition of exceptions are inconsistent for variable \
                   %a."
                  Ast.ScopeDef.format def_info)
            other_edges_originating_from_same_label;
          (* Now we add the edge to the list*)
          let existing_edge =
            List.find_opt
              (fun edge ->
                LabelName.compare edge.label_from label_from = 0
                && LabelName.compare edge.label_to label_to = 0)
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
    LabelName.Map.fold
      (fun label rule_set g ->
        ExceptionsDependencies.add_vertex g { rules = rule_set; label })
      label_to_rule_sets ExceptionsDependencies.empty
  in
  (* then we add the edges *)
  let g =
    List.fold_left
      (fun g edge ->
        let rule_group_from =
          {
            ExceptionVertex.rules =
              LabelName.Map.find edge.label_from label_to_rule_sets;
            label = edge.label_from;
          }
        in
        let rule_group_to =
          {
            ExceptionVertex.rules =
              LabelName.Map.find edge.label_to label_to_rule_sets;
            label = edge.label_to;
          }
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
let check_for_exception_cycle
    (def : Ast.rule RuleName.Map.t)
    (g : ExceptionsDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = ExceptionsSCC.scc_list g in
  if List.length sccs < ExceptionsDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    let spans =
      List.rev_map
        (fun (vs : ExceptionVertex.t) ->
          let v, _ = RuleName.Map.choose vs.rules in
          let rule = RuleName.Map.find v def in
          let pos = Mark.get (RuleName.get_info rule.Ast.rule_id) in
          "", pos)
        scc
    in
    let v, _ = RuleName.Map.choose (List.hd scc).rules in
    Message.error ~extra_pos:spans
      "Exception cycle detected when defining@ %a:@ each of these %d \
       exceptions applies over the previous one,@ and@ the@ first@ applies@ \
       over@ the@ last."
      RuleName.format v (List.length scc)
