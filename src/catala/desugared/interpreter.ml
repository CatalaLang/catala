(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Pos = Utils.Pos
module Errors = Utils.Errors

(** The vertices of the scope dependency graph are either :

    - the variables of the scope ;
    - the subscopes of the scope.

    Indeed, during interpretation, subscopes are executed atomically.

    In the graph, x -> y if x is used in the definition of y. *)

module Vertex = struct
  type t = Var of Ast.Var.t | SubScope of Scopelang.Ast.SubScopeName.t

  let hash x =
    match x with Var x -> Ast.Var.hash x | SubScope x -> Scopelang.Ast.SubScopeName.hash x

  let compare = compare

  let equal x y =
    match (x, y) with
    | Var x, Var y -> Ast.Var.compare x y = 0
    | SubScope x, SubScope y -> Scopelang.Ast.SubScopeName.compare x y = 0
    | _ -> false

  let format_t (x : t) : string =
    match x with Var v -> Ast.Var.format_t v | SubScope v -> Scopelang.Ast.SubScopeName.format_t v
end

(** On the edges, the label is the expression responsible for the use of the variable *)
module Edge = struct
  type t = Pos.t

  let compare = compare

  let default = Pos.no_pos
end

module ScopeDependencies = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
module TopologicalTraversal = Graph.Topological.Make (ScopeDependencies)

module SCC = Graph.Components.Make (ScopeDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** Outputs an error in case of cycles. *)
let check_for_cycle (g : ScopeDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < ScopeDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    Errors.raise_multispanned_error "cyclic dependency dected between variables!"
      (List.flatten
         (List.map
            (fun v ->
              let var_str, var_info =
                match v with
                | Vertex.Var v -> (Ast.Var.format_t v, Ast.Var.get_info v)
                | Vertex.SubScope v ->
                    (Scopelang.Ast.SubScopeName.format_t v, Scopelang.Ast.SubScopeName.get_info v)
              in
              let succs = ScopeDependencies.succ_e g v in
              let _, edge_pos, succ = List.find (fun (_, _, succ) -> List.mem succ scc) succs in
              let succ_str =
                match succ with
                | Vertex.Var v -> Ast.Var.format_t v
                | Vertex.SubScope v -> Scopelang.Ast.SubScopeName.format_t v
              in
              [
                (Some ("cycle variable " ^ var_str ^ ", declared:"), Pos.get_position var_info);
                ( Some ("used here in the definition of another cycle variable " ^ succ_str ^ ":"),
                  edge_pos );
              ])
            scc))

let build_scope_dependencies (scope : Ast.scope) (ctxt : Name_resolution.context) :
    ScopeDependencies.t =
  let g = ScopeDependencies.empty in
  let scope_uid = scope.scope_uid in
  (* Add all the vertices to the graph *)
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let g =
    Ast.IdentMap.fold
      (fun _ (v : Ast.Var.t) g -> ScopeDependencies.add_vertex g (Vertex.Var v))
      scope_ctxt.var_idmap g
  in
  let g =
    Ast.IdentMap.fold
      (fun _ (v : Scopelang.Ast.SubScopeName.t) g ->
        ScopeDependencies.add_vertex g (Vertex.SubScope v))
      scope_ctxt.sub_scopes_idmap g
  in
  let g =
    Ast.ScopeDefMap.fold
      (fun def_key _def g ->
        let fv = assert false (* Dcalc.Ast.term_fv def *) in
        Ast.ScopeDefSet.fold
          (fun fv_def g ->
            match (def_key, fv_def) with
            | Ast.ScopeDef.Var defined, Ast.ScopeDef.Var used ->
                (* simple case *)
                ScopeDependencies.add_edge g (Vertex.Var used) (Vertex.Var defined)
            | Ast.ScopeDef.SubScopeVar (defined, _), Ast.ScopeDef.Var used ->
                (* here we are defining the input of a subscope using a var of the scope *)
                ScopeDependencies.add_edge g (Vertex.Var used) (Vertex.SubScope defined)
            | Ast.ScopeDef.SubScopeVar (defined, _), Ast.ScopeDef.SubScopeVar (used, _) ->
                (* here we are defining the input of a scope with the output of another subscope *)
                ScopeDependencies.add_edge g (Vertex.SubScope used) (Vertex.SubScope defined)
            | Ast.ScopeDef.Var defined, Ast.ScopeDef.SubScopeVar (used, _) ->
                (* finally we define a scope var with the output of a subscope *)
                ScopeDependencies.add_edge g (Vertex.SubScope used) (Vertex.Var defined))
          fv g)
      scope.scope_defs g
  in
  g
