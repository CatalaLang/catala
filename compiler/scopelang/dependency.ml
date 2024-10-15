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

open Catala_utils
open Shared_ast

type vertex = Scope of ScopeName.t | Topdef of TopdefName.t

module SVertex = struct
  type t = vertex
  (* While we enforce that globals don't depend on scopes, and could therefore
     compute two separate dependency graphs and traverse them one after the
     other, code-wise it's simpler to have a single graph including both *)

  let compare v1 v2 =
    match v1, v2 with
    | Scope s1, Scope s2 -> ScopeName.compare s1 s2
    | Topdef g1, Topdef g2 -> TopdefName.compare g1 g2
    | Scope _, _ -> -1
    | _, Scope _ -> 1
    | Topdef _, _ | _, Topdef _ -> .

  let equal v1 v2 =
    match v1, v2 with
    | Scope s1, Scope s2 -> ScopeName.equal s1 s2
    | Topdef g1, Topdef g2 -> TopdefName.equal g1 g2
    | (Scope _ | Topdef _), _ -> false

  let hash = function Scope s -> ScopeName.id s | Topdef g -> TopdefName.id g

  let format ppf = function
    | Scope s -> ScopeName.format ppf s
    | Topdef g -> TopdefName.format ppf g

  (* let to_string v = Format.asprintf "%a" format v

     let info = function | Scope s -> ScopeName.get_info s | Topdef g ->
     TopdefName.get_info g *)
end

module VMap = Map.Make (SVertex)

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

let rec expr_used_defs e =
  let recurse_subterms e =
    Expr.shallow_fold
      (fun e -> VMap.union (fun _ x _ -> Some x) (expr_used_defs e))
      e VMap.empty
  in
  match e with
  | ELocation (ToplevelVar { name = v, pos }), _ ->
    if TopdefName.path v <> [] then VMap.empty
    else VMap.singleton (Topdef v) pos
  | (EScopeCall { scope; _ }, m) as e ->
    if ScopeName.path scope <> [] then recurse_subterms e
    else VMap.add (Scope scope) (Expr.mark_pos m) (recurse_subterms e)
  | EAbs { binder; _ }, _ ->
    let _, body = Bindlib.unmbind binder in
    expr_used_defs body
  | e -> recurse_subterms e

let rule_used_defs = function
  | Ast.Assertion e
  | Ast.ScopeVarDefinition { e; _ }
  | Ast.SubScopeVarDefinition { e; _ } ->
    (* TODO: maybe this info could be passed on from previous passes without
       walking through all exprs again *)
    expr_used_defs e

let build_program_dep_graph (prgm : 'm Ast.program) : SDependencies.t =
  let g = SDependencies.empty in
  let g =
    TopdefName.Map.fold
      (fun v _ g -> SDependencies.add_vertex g (Topdef v))
      prgm.program_topdefs g
  in
  let g =
    ScopeName.Map.fold
      (fun v _ g -> SDependencies.add_vertex g (Scope v))
      prgm.program_scopes g
  in
  let g =
    TopdefName.Map.fold
      (fun glo_name (expr, _ty, _vis) g ->
        let used_defs = expr_used_defs expr in
        if VMap.mem (Topdef glo_name) used_defs then
          Message.error
            ~pos:(Mark.get (TopdefName.get_info glo_name))
            "The toplevel declaration@ %a@ has@ a@ definition@ that@ refers@ \
             to@ itself,@ which@ is@ not@ supported@ (Catala does not provide \
             recursion)"
            TopdefName.format glo_name;
        VMap.fold
          (fun def pos g ->
            let edge = SDependencies.E.create def pos (Topdef glo_name) in
            SDependencies.add_edge_e g edge)
          used_defs g)
      prgm.program_topdefs g
  in
  ScopeName.Map.fold
    (fun scope_name (scope, _) g ->
      List.fold_left
        (fun g rule ->
          let used_defs = rule_used_defs rule in
          if VMap.mem (Scope scope_name) used_defs then
            Message.error
              ~pos:(Mark.get (ScopeName.get_info scope.Ast.scope_decl_name))
              "The scope@ %a@ is@ calling@ into@ itself@ as@ a@ subscope,@ \
               which@ is@ not@ supported@ (Catala does not provide recursion)"
              ScopeName.format scope.Ast.scope_decl_name;
          VMap.fold
            (fun used_def pos g ->
              let edge =
                SDependencies.E.create used_def pos (Scope scope_name)
              in
              SDependencies.add_edge_e g edge)
            used_defs g)
        g scope.Ast.scope_decl_rules)
    prgm.program_scopes g

let check_for_cycle_in_defs (g : SDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of
     cardinality > 1 *)
  let sccs = SSCC.scc_list g in
  match List.find_opt (function [] | [_] -> false | _ -> true) sccs with
  | None -> ()
  | Some [] -> assert false
  | Some (v0 :: _ as scc) ->
    let module VSet = Set.Make (SVertex) in
    let scc = VSet.of_list scc in
    let rec get_cycle cycle cycle_set v =
      let cycle = v :: cycle in
      let cycle_set = VSet.add v cycle_set in
      let succ = SDependencies.succ g v in
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
    let spans =
      List.map2
        (fun v1 v2 ->
          let msg =
            Format.asprintf "%a is used here in the definition of %a:"
              SVertex.format v1 SVertex.format v2
          in
          let _, edge_pos, _ = SDependencies.find_edge g v1 v2 in
          msg, edge_pos)
        cycle
        (List.tl cycle @ [List.hd cycle])
    in
    Message.error ~extra_pos:spans
      "Cyclic dependency detected between the following scopes:@ @[<hv>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " →@ ")
         SVertex.format)
      (cycle @ [List.hd cycle])

let get_defs_ordering (g : SDependencies.t) : SVertex.t list =
  List.rev (STopologicalTraversal.fold (fun sd acc -> sd :: acc) g [])

(** On the edges, the label is the expression responsible for the use of the
    function *)
module TEdge = struct
  type t = Pos.t

  let compare = compare
  let default = Pos.no_pos
end

module TDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (TypeIdent) (TEdge)

module TTopologicalTraversal = Graph.Topological.Make (TDependencies)

module TSCC = Graph.Components.Make (TDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

let rec get_structs_or_enums_in_type (t : typ) : TypeIdent.Set.t =
  match Mark.remove t with
  | TStruct s -> TypeIdent.Set.singleton (Struct s)
  | TEnum e -> TypeIdent.Set.singleton (Enum e)
  | TArrow (t1, t2) ->
    TypeIdent.Set.union
      (t1
      |> List.map get_structs_or_enums_in_type
      |> List.fold_left TypeIdent.Set.union TypeIdent.Set.empty)
      (get_structs_or_enums_in_type t2)
  | TClosureEnv | TLit _ | TAny -> TypeIdent.Set.empty
  | TOption t1 | TArray t1 | TDefault t1 -> get_structs_or_enums_in_type t1
  | TTuple ts ->
    List.fold_left
      (fun acc t -> TypeIdent.Set.union acc (get_structs_or_enums_in_type t))
      TypeIdent.Set.empty ts

let build_type_graph (structs : struct_ctx) (enums : enum_ctx) : TDependencies.t
    =
  let g = TDependencies.empty in
  let g =
    StructName.Map.fold
      (fun s fields g ->
        StructField.Map.fold
          (fun _ typ g ->
            let def = TypeIdent.Struct s in
            let g = TDependencies.add_vertex g def in
            let used = get_structs_or_enums_in_type typ in
            TypeIdent.Set.fold
              (fun used g ->
                if TypeIdent.equal used def then
                  Message.error ~pos:(Mark.get typ)
                    "The type@ %a@ is@ defined@ using@ itself,@ which@ is@ \
                     not@ supported@ (Catala does not allow recursive types)."
                    TypeIdent.format used
                else
                  let edge = TDependencies.E.create used (Mark.get typ) def in
                  TDependencies.add_edge_e g edge)
              used g)
          fields g)
      structs g
  in
  let g =
    EnumName.Map.fold
      (fun e cases g ->
        EnumConstructor.Map.fold
          (fun _ typ g ->
            let def = TypeIdent.Enum e in
            let g = TDependencies.add_vertex g def in
            let used = get_structs_or_enums_in_type typ in
            TypeIdent.Set.fold
              (fun used g ->
                if TypeIdent.equal used def then
                  Message.error ~pos:(Mark.get typ)
                    "The type@ %a@ is@ defined@ using@ itself,@ which@ is@ \
                     not@ supported@ (Catala does not allow recursive types)."
                    TypeIdent.format used
                else
                  let edge = TDependencies.E.create used (Mark.get typ) def in
                  TDependencies.add_edge_e g edge)
              used g)
          cases g)
      enums g
  in
  g

let check_type_cycles (structs : struct_ctx) (enums : enum_ctx) :
    TypeIdent.t list =
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
                Format.asprintf "%a" TypeIdent.format v, TypeIdent.get_info v
              in
              let succs = TDependencies.succ_e g v in
              let _, edge_pos, succ =
                List.find (fun (_, _, succ) -> List.mem succ scc) succs
              in
              let succ_str = Format.asprintf "%a" TypeIdent.format succ in
              [
                "Cycle type " ^ var_str ^ ", declared:", Mark.get var_info;
                ( "Used here in the definition of another cycle type "
                  ^ succ_str
                  ^ ":",
                  edge_pos );
              ])
            scc)
     in
     Message.error ~extra_pos:spans "Cyclic dependency detected between types.");
  List.rev (TTopologicalTraversal.fold (fun v acc -> v :: acc) g [])
