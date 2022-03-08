(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

(** Graph representation of the dependencies between scopes in the Catala
    program. Vertices are functions, x -> y if x is used in the definition of y. *)

open Utils

module SVertex = struct
  type t = Ast.ScopeName.t

  let hash x = Ast.ScopeName.hash x
  let compare = Ast.ScopeName.compare
  let equal x y = Ast.ScopeName.compare x y = 0
end

(** On the edges, the label is the expression responsible for the use of the
    function *)
module SEdge = struct
  type t = Pos.t

  let compare = compare
  let default = Pos.no_pos
end

module SDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (SVertex) (SEdge)

module STopologicalTraversal = Graph.Topological.Make (SDependencies)

module SSCC = Graph.Components.Make (SDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let build_program_dep_graph (prgm : Ast.program) : SDependencies.t =
  let g = SDependencies.empty in
  let g =
    Ast.ScopeMap.fold
      (fun v _ g -> SDependencies.add_vertex g v)
      prgm.program_scopes g
  in
  Ast.ScopeMap.fold
    (fun scope_name scope g ->
      let subscopes =
        List.fold_left
          (fun acc r ->
            match r with
            | Ast.Definition _ | Ast.Assertion _ -> acc
            | Ast.Call (subscope, subindex) ->
                if subscope = scope_name then
                  Errors.raise_spanned_error
                    (Pos.get_position
                       (Ast.ScopeName.get_info scope.Ast.scope_decl_name))
                    "The scope %a is calling into itself as a subscope, which \
                     is forbidden since Catala does not provide recursion"
                    Ast.ScopeName.format_t scope.Ast.scope_decl_name
                else
                  Ast.ScopeMap.add subscope
                    (Pos.get_position (Ast.SubScopeName.get_info subindex))
                    acc)
          Ast.ScopeMap.empty scope.Ast.scope_decl_rules
      in
      Ast.ScopeMap.fold
        (fun subscope pos g ->
          let edge = SDependencies.E.create subscope pos scope_name in
          SDependencies.add_edge_e g edge)
        subscopes g)
    prgm.program_scopes g

let check_for_cycle_in_scope (g : SDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = SSCC.scc_list g in
  if List.length sccs < SDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    let spans =
      List.flatten
        (List.map
           (fun v ->
             let var_str, var_info =
               ( Format.asprintf "%a" Ast.ScopeName.format_t v,
                 Ast.ScopeName.get_info v )
             in
             let succs = SDependencies.succ_e g v in
             let _, edge_pos, succ =
               List.find (fun (_, _, succ) -> List.mem succ scc) succs
             in
             let succ_str = Format.asprintf "%a" Ast.ScopeName.format_t succ in
             [
               ( Some ("Cycle variable " ^ var_str ^ ", declared:"),
                 Pos.get_position var_info );
               ( Some
                   ("Used here in the definition of another cycle variable "
                  ^ succ_str ^ ":"),
                 edge_pos );
             ])
           scc)
    in
    Errors.raise_multispanned_error spans
      "Cyclic dependency detected between scopes!"

let get_scope_ordering (g : SDependencies.t) : Ast.ScopeName.t list =
  List.rev (STopologicalTraversal.fold (fun sd acc -> sd :: acc) g [])

module TVertex = struct
  type t = Struct of Ast.StructName.t | Enum of Ast.EnumName.t

  let hash x =
    match x with
    | Struct x -> Ast.StructName.hash x
    | Enum x -> Ast.EnumName.hash x

  let compare x y =
    match (x, y) with
    | Struct x, Struct y -> Ast.StructName.compare x y
    | Enum x, Enum y -> Ast.EnumName.compare x y
    | Struct _, Enum _ -> 1
    | Enum _, Struct _ -> -1

  let equal x y =
    match (x, y) with
    | Struct x, Struct y -> Ast.StructName.compare x y = 0
    | Enum x, Enum y -> Ast.EnumName.compare x y = 0
    | _ -> false

  let format_t (fmt : Format.formatter) (x : t) : unit =
    match x with
    | Struct x -> Ast.StructName.format_t fmt x
    | Enum x -> Ast.EnumName.format_t fmt x

  let get_info (x : t) =
    match x with
    | Struct x -> Ast.StructName.get_info x
    | Enum x -> Ast.EnumName.get_info x
end

module TVertexSet = Set.Make (TVertex)

(** On the edges, the label is the expression responsible for the use of the
    function *)
module TEdge = struct
  type t = Pos.t

  let compare = compare
  let default = Pos.no_pos
end

module TDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (TVertex) (TEdge)

module TTopologicalTraversal = Graph.Topological.Make (TDependencies)

module TSCC = Graph.Components.Make (TDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let rec get_structs_or_enums_in_type (t : Ast.typ Pos.marked) : TVertexSet.t =
  match Pos.unmark t with
  | Ast.TStruct s -> TVertexSet.singleton (TVertex.Struct s)
  | Ast.TEnum e -> TVertexSet.singleton (TVertex.Enum e)
  | Ast.TArrow (t1, t2) ->
      TVertexSet.union
        (get_structs_or_enums_in_type t1)
        (get_structs_or_enums_in_type t2)
  | Ast.TLit _ | Ast.TAny -> TVertexSet.empty
  | Ast.TArray t1 -> get_structs_or_enums_in_type (Pos.same_pos_as t1 t)

let build_type_graph (structs : Ast.struct_ctx) (enums : Ast.enum_ctx) :
    TDependencies.t =
  let g = TDependencies.empty in
  let g =
    Ast.StructMap.fold
      (fun s fields g ->
        List.fold_left
          (fun g (_, typ) ->
            let def = TVertex.Struct s in
            let g = TDependencies.add_vertex g def in
            let used = get_structs_or_enums_in_type typ in
            TVertexSet.fold
              (fun used g ->
                if TVertex.equal used def then
                  Errors.raise_spanned_error (Pos.get_position typ)
                    "The type %a is defined using itself, which is forbidden \
                     since Catala does not provide recursive types"
                    TVertex.format_t used
                else
                  let edge =
                    TDependencies.E.create used (Pos.get_position typ) def
                  in
                  TDependencies.add_edge_e g edge)
              used g)
          g fields)
      structs g
  in
  let g =
    Ast.EnumMap.fold
      (fun e cases g ->
        List.fold_left
          (fun g (_, typ) ->
            let def = TVertex.Enum e in
            let g = TDependencies.add_vertex g def in
            let used = get_structs_or_enums_in_type typ in
            TVertexSet.fold
              (fun used g ->
                if TVertex.equal used def then
                  Errors.raise_spanned_error (Pos.get_position typ)
                    "The type %a is defined using itself, which is forbidden \
                     since Catala does not provide recursive types"
                    TVertex.format_t used
                else
                  let edge =
                    TDependencies.E.create used (Pos.get_position typ) def
                  in
                  TDependencies.add_edge_e g edge)
              used g)
          g cases)
      enums g
  in
  g

let check_type_cycles (structs : Ast.struct_ctx) (enums : Ast.enum_ctx) :
    TVertex.t list =
  let g = build_type_graph structs enums in
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = TSCC.scc_list g in
  (if List.length sccs < TDependencies.nb_vertex g then
   let scc = List.find (fun scc -> List.length scc > 1) sccs in
   let spans =
     List.flatten
       (List.map
          (fun v ->
            let var_str, var_info =
              (Format.asprintf "%a" TVertex.format_t v, TVertex.get_info v)
            in
            let succs = TDependencies.succ_e g v in
            let _, edge_pos, succ =
              List.find (fun (_, _, succ) -> List.mem succ scc) succs
            in
            let succ_str = Format.asprintf "%a" TVertex.format_t succ in
            [
              ( Some ("Cycle type " ^ var_str ^ ", declared:"),
                Pos.get_position var_info );
              ( Some
                  ("Used here in the definition of another cycle type "
                 ^ succ_str ^ ":"),
                edge_pos );
            ])
          scc)
   in
   Errors.raise_multispanned_error spans
     "Cyclic dependency detected between types!");
  List.rev (TTopologicalTraversal.fold (fun v acc -> v :: acc) g [])
