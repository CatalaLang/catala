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

(** Scope dependencies computations using {{:http://ocamlgraph.lri.fr/} OCamlgraph} *)

module Pos = Utils.Pos
module Errors = Utils.Errors

(** {1 Graph declaration} *)

(** Vertices: scope variables or subscopes.

    The vertices of the scope dependency graph are either :

    - the variables of the scope ;
    - the subscopes of the scope.

    Indeed, during interpretation, subscopes are executed atomically. *)
module Vertex = struct
  type t = Var of Scopelang.Ast.ScopeVar.t | SubScope of Scopelang.Ast.SubScopeName.t

  let hash x =
    match x with
    | Var x -> Scopelang.Ast.ScopeVar.hash x
    | SubScope x -> Scopelang.Ast.SubScopeName.hash x

  let compare = compare

  let equal x y =
    match (x, y) with
    | Var x, Var y -> Scopelang.Ast.ScopeVar.compare x y = 0
    | SubScope x, SubScope y -> Scopelang.Ast.SubScopeName.compare x y = 0
    | _ -> false

  let format_t (fmt : Format.formatter) (x : t) : unit =
    match x with
    | Var v -> Scopelang.Ast.ScopeVar.format_t fmt v
    | SubScope v -> Scopelang.Ast.SubScopeName.format_t fmt v
end

(** On the edges, the label is the position of the expression responsible for the use of the
    variable. In the graph, [x -> y] if [x] is used in the definition of [y].*)
module Edge = struct
  type t = Pos.t

  let compare = compare

  let default = Pos.no_pos
end

module ScopeDependencies = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
(** Module of the graph, provided by OCamlGraph *)

module TopologicalTraversal = Graph.Topological.Make (ScopeDependencies)
(** Module of the topological traversal of the graph, provided by OCamlGraph *)

module SCC = Graph.Components.Make (ScopeDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** {1 Graph computations} *)

(** Returns an ordering of the scope variables and subscope compatible with the dependencies of the
    computation *)
let correct_computation_ordering (g : ScopeDependencies.t) : Vertex.t list =
  List.rev (TopologicalTraversal.fold (fun sd acc -> sd :: acc) g [])

(** Outputs an error in case of cycles. *)
let check_for_cycle (scope : Ast.scope) (g : ScopeDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < ScopeDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    Errors.raise_multispanned_error
      (Format.asprintf "Cyclic dependency detected between variables of scope %a!"
         Scopelang.Ast.ScopeName.format_t scope.scope_uid)
      (List.flatten
         (List.map
            (fun v ->
              let var_str, var_info =
                match v with
                | Vertex.Var v ->
                    ( Format.asprintf "%a" Scopelang.Ast.ScopeVar.format_t v,
                      Scopelang.Ast.ScopeVar.get_info v )
                | Vertex.SubScope v ->
                    ( Format.asprintf "%a" Scopelang.Ast.SubScopeName.format_t v,
                      Scopelang.Ast.SubScopeName.get_info v )
              in
              let succs = ScopeDependencies.succ_e g v in
              let _, edge_pos, succ = List.find (fun (_, _, succ) -> List.mem succ scc) succs in
              let succ_str =
                match succ with
                | Vertex.Var v -> Format.asprintf "%a" Scopelang.Ast.ScopeVar.format_t v
                | Vertex.SubScope v -> Format.asprintf "%a" Scopelang.Ast.SubScopeName.format_t v
              in
              [
                (Some ("Cycle variable " ^ var_str ^ ", declared:"), Pos.get_position var_info);
                ( Some ("Used here in the definition of another cycle variable " ^ succ_str ^ ":"),
                  edge_pos );
              ])
            scc))

(** Builds the dependency graph of a particular scope *)
let build_scope_dependencies (scope : Ast.scope) : ScopeDependencies.t =
  let g = ScopeDependencies.empty in
  (* Add all the vertices to the graph *)
  let g =
    Scopelang.Ast.ScopeVarSet.fold
      (fun (v : Scopelang.Ast.ScopeVar.t) g -> ScopeDependencies.add_vertex g (Vertex.Var v))
      scope.scope_vars g
  in
  let g =
    Scopelang.Ast.SubScopeMap.fold
      (fun (v : Scopelang.Ast.SubScopeName.t) _ g ->
        ScopeDependencies.add_vertex g (Vertex.SubScope v))
      scope.scope_sub_scopes g
  in
  let g =
    Ast.ScopeDefMap.fold
      (fun def_key (def, _, _) g ->
        let fv = Ast.free_variables def in
        Ast.ScopeDefMap.fold
          (fun fv_def fv_def_pos g ->
            match (def_key, fv_def) with
            | Ast.ScopeDef.Var defined, Ast.ScopeDef.Var used ->
                (* simple case *)
                if used = defined then
                  (* variable definitions cannot be recursive *)
                  Errors.raise_spanned_error
                    (Format.asprintf
                       "The variable %a is used in one of its definitions, but recursion is \
                        forbidden in Catala"
                       Scopelang.Ast.ScopeVar.format_t defined)
                    fv_def_pos
                else
                  let edge =
                    ScopeDependencies.E.create (Vertex.Var used) fv_def_pos (Vertex.Var defined)
                  in
                  ScopeDependencies.add_edge_e g edge
            | Ast.ScopeDef.SubScopeVar (defined, _), Ast.ScopeDef.Var used ->
                (* here we are defining the input of a subscope using a var of the scope *)
                let edge =
                  ScopeDependencies.E.create (Vertex.Var used) fv_def_pos (Vertex.SubScope defined)
                in
                ScopeDependencies.add_edge_e g edge
            | Ast.ScopeDef.SubScopeVar (defined, _), Ast.ScopeDef.SubScopeVar (used, _) ->
                (* here we are defining the input of a scope with the output of another subscope *)
                if used = defined then
                  (* subscopes are not recursive functions *)
                  Errors.raise_spanned_error
                    (Format.asprintf
                       "The subscope %a is used when defining one of its inputs, but recursion is \
                        forbidden in Catala"
                       Scopelang.Ast.SubScopeName.format_t defined)
                    fv_def_pos
                else
                  let edge =
                    ScopeDependencies.E.create (Vertex.SubScope used) fv_def_pos
                      (Vertex.SubScope defined)
                  in
                  ScopeDependencies.add_edge_e g edge
            | Ast.ScopeDef.Var defined, Ast.ScopeDef.SubScopeVar (used, _) ->
                (* finally we define a scope var with the output of a subscope *)
                let edge =
                  ScopeDependencies.E.create (Vertex.SubScope used) fv_def_pos (Vertex.Var defined)
                in
                ScopeDependencies.add_edge_e g edge)
          fv g)
      scope.scope_defs g
  in
  g
