(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Graph representation of the dependencies between scopes in the Catala program. Vertices are
    functions, x -> y if x is used in the definition of y. *)

module Pos = Utils.Pos
module Errors = Utils.Errors

module Vertex = struct
  type t = Ast.ScopeName.t

  let hash x = Ast.ScopeName.hash x

  let compare = compare

  let equal x y = Ast.ScopeName.compare x y = 0

  let format_t (fmt : Format.formatter) (x : t) : unit = Ast.ScopeName.format_t fmt x
end

(** On the edges, the label is the expression responsible for the use of the function *)
module Edge = struct
  type t = Pos.t

  let compare = compare

  let default = Pos.no_pos
end

module Dependencies = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
module TopologicalTraversal = Graph.Topological.Make (Dependencies)

module SCC = Graph.Components.Make (Dependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let build_program_dep_graph (prgm : Ast.program) : Dependencies.t =
  let g = Dependencies.empty in
  let g = Ast.ScopeMap.fold (fun v _ g -> Dependencies.add_vertex g v) prgm.program_scopes g in
  Ast.ScopeMap.fold
    (fun scope_name scope g ->
      let subscopes =
        List.fold_left
          (fun acc r ->
            match r with
            | Ast.Definition _ -> acc
            | Ast.Call (subscope, subindex) ->
                if subscope = scope_name then
                  Errors.raise_spanned_error
                    "The scope %a is calling into itself as a subscope, which is forbidden since \
                     Catala does not provide recursion"
                    (Pos.get_position (Ast.ScopeName.get_info scope.Ast.scope_decl_name))
                else
                  Ast.ScopeMap.add subscope
                    (Pos.get_position (Ast.SubScopeName.get_info subindex))
                    acc)
          Ast.ScopeMap.empty scope.Ast.scope_decl_rules
      in
      Ast.ScopeMap.fold
        (fun subscope pos g ->
          let edge = Dependencies.E.create subscope pos scope_name in
          Dependencies.add_edge_e g edge)
        subscopes g)
    prgm.program_scopes g

let check_for_cycle (g : Dependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < Dependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    Errors.raise_multispanned_error "Cyclic dependency detected between scopes!"
      (List.flatten
         (List.map
            (fun v ->
              let var_str, var_info =
                (Format.asprintf "%a" Ast.ScopeName.format_t v, Ast.ScopeName.get_info v)
              in
              let succs = Dependencies.succ_e g v in
              let _, edge_pos, succ = List.find (fun (_, _, succ) -> List.mem succ scc) succs in
              let succ_str = Format.asprintf "%a" Ast.ScopeName.format_t succ in
              [
                (Some ("Cycle variable " ^ var_str ^ ", declared:"), Pos.get_position var_info);
                ( Some ("Used here in the definition of another cycle variable " ^ succ_str ^ ":"),
                  edge_pos );
              ])
            scc))

let get_scope_ordering (g : Dependencies.t) : Ast.ScopeName.t list =
  List.rev (TopologicalTraversal.fold (fun sd acc -> sd :: acc) g [])
