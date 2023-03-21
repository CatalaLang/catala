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

(** Abstract syntax tree of the desugared representation *)

open Catala_utils
open Shared_ast

(** {1 Names, Maps and Keys} *)

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef = struct
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of SubScopeName.t * ScopeVar.t * Pos.t
        (** In this case, the [ScopeVar.t] lives inside the context of the
            subscope's original declaration *)

  let compare x y =
    match x, y with
    | Var (x, stx), Var (y, sty) -> (
      match ScopeVar.compare x y with
      | 0 -> Option.compare StateName.compare stx sty
      | n -> n)
    | SubScopeVar (x', x, _), SubScopeVar (y', y, _) -> (
      match SubScopeName.compare x' y' with 0 -> ScopeVar.compare x y | n -> n)
    | Var _, _ -> -1
    | _, Var _ -> 1

  let get_position x =
    match x with
    | Var (x, None) -> Marked.get_mark (ScopeVar.get_info x)
    | Var (_, Some sx) -> Marked.get_mark (StateName.get_info sx)
    | SubScopeVar (_, _, pos) -> pos

  let format_t fmt x =
    match x with
    | Var (v, None) -> ScopeVar.format_t fmt v
    | Var (v, Some sv) ->
      Format.fprintf fmt "%a.%a" ScopeVar.format_t v StateName.format_t sv
    | SubScopeVar (s, v, _) ->
      Format.fprintf fmt "%a.%a" SubScopeName.format_t s ScopeVar.format_t v

  let hash x =
    match x with
    | Var (v, None) -> ScopeVar.hash v
    | Var (v, Some sv) -> Int.logxor (ScopeVar.hash v) (StateName.hash sv)
    | SubScopeVar (w, v, _) ->
      Int.logxor (SubScopeName.hash w) (ScopeVar.hash v)
end

module ScopeDefMap : Map.S with type key = ScopeDef.t = Map.Make (ScopeDef)
module ScopeDefSet : Set.S with type elt = ScopeDef.t = Set.Make (ScopeDef)

(** {1 AST} *)

type location = desugared glocation

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare = Expr.compare_location
end)

type expr = (desugared, untyped mark) gexpr

module ExprMap = Map.Make (struct
  type t = expr

  let compare = Expr.compare
end)

type io_input = NoInput | OnlyInput | Reentrant
type io = { io_output : bool Marked.pos; io_input : io_input Marked.pos }

type exception_situation =
  | BaseCase
  | ExceptionToLabel of LabelName.t Marked.pos
  | ExceptionToRule of RuleName.t Marked.pos

type label_situation = ExplicitlyLabeled of LabelName.t Marked.pos | Unlabeled

type rule = {
  rule_id : RuleName.t;
  rule_just : expr boxed;
  rule_cons : expr boxed;
  rule_parameter : (expr Var.t Marked.pos * typ) list Marked.pos option;
  rule_exception : exception_situation;
  rule_label : label_situation;
}

module Rule = struct
  type t = rule

  (** Structural equality (otherwise, you should just compare the [rule_id]
      fields) *)
  let compare r1 r2 =
    match r1.rule_parameter, r2.rule_parameter with
    | None, None -> (
      let j1 = Expr.unbox r1.rule_just in
      let j2 = Expr.unbox r2.rule_just in
      match Expr.compare j1 j2 with
      | 0 ->
        let c1 = Expr.unbox r1.rule_cons in
        let c2 = Expr.unbox r2.rule_cons in
        Expr.compare c1 c2
      | n -> n)
    | Some (l1, _), Some (l2, _) ->
      ListLabels.compare l1 l2 ~cmp:(fun ((v1, _), t1) ((v2, _), t2) ->
          match Type.compare t1 t2 with
          | 0 -> (
            let open Bindlib in
            let b1 = unbox (bind_var v1 (Expr.Box.lift r1.rule_just)) in
            let b2 = unbox (bind_var v2 (Expr.Box.lift r2.rule_just)) in
            let _, j1, j2 = unbind2 b1 b2 in
            match Expr.compare j1 j2 with
            | 0 ->
              let b1 = unbox (bind_var v1 (Expr.Box.lift r1.rule_cons)) in
              let b2 = unbox (bind_var v2 (Expr.Box.lift r2.rule_cons)) in
              let _, c1, c2 = unbind2 b1 b2 in
              Expr.compare c1 c2
            | n -> n)
          | n -> n)
    | None, Some _ -> -1
    | Some _, None -> 1
end

let empty_rule
    (pos : Pos.t)
    (parameters : (Uid.MarkedString.info * typ) list Marked.pos option) : rule =
  {
    rule_just = Expr.box (ELit (LBool false), Untyped { pos });
    rule_cons = Expr.box (ELit LEmptyError, Untyped { pos });
    rule_parameter =
      Option.map
        (Marked.map_under_mark
           (List.map (fun (lbl, typ) -> Marked.map_under_mark Var.make lbl, typ)))
        parameters;
    rule_exception = BaseCase;
    rule_id = RuleName.fresh ("empty", pos);
    rule_label = Unlabeled;
  }

let always_false_rule
    (pos : Pos.t)
    (parameters : (Uid.MarkedString.info * typ) list Marked.pos option) : rule =
  {
    rule_just = Expr.box (ELit (LBool true), Untyped { pos });
    rule_cons = Expr.box (ELit (LBool false), Untyped { pos });
    rule_parameter =
      Option.map
        (Marked.map_under_mark
           (List.map (fun (lbl, typ) -> Marked.map_under_mark Var.make lbl, typ)))
        parameters;
    rule_exception = BaseCase;
    rule_id = RuleName.fresh ("always_false", pos);
    rule_label = Unlabeled;
  }

type assertion = expr boxed
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law
type catala_option = DateRounding of variation_typ

type meta_assertion =
  | FixedBy of reference_typ Marked.pos
  | VariesWith of unit * variation_typ Marked.pos option

type scope_def = {
  scope_def_rules : rule RuleName.Map.t;
  scope_def_typ : typ;
  scope_def_parameters : (Uid.MarkedString.info * typ) list Marked.pos option;
  scope_def_is_condition : bool;
  scope_def_io : io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVar.Map.t;
  scope_sub_scopes : ScopeName.t SubScopeName.Map.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_options : catala_option Marked.pos list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope ScopeName.Map.t;
  program_topdefs : (expr * typ) TopdefName.Map.t;
  program_ctx : decl_ctx;
}

let rec locations_used e : LocationSet.t =
  match e with
  | ELocation l, m -> LocationSet.singleton (l, Expr.mark_pos m)
  | EAbs { binder; _ }, _ ->
    let _, body = Bindlib.unmbind binder in
    locations_used body
  | e ->
    Expr.shallow_fold
      (fun e -> LocationSet.union (locations_used e))
      e LocationSet.empty

let free_variables (def : rule RuleName.Map.t) : Pos.t ScopeDefMap.t =
  let add_locs (acc : Pos.t ScopeDefMap.t) (locs : LocationSet.t) :
      Pos.t ScopeDefMap.t =
    LocationSet.fold
      (fun (loc, loc_pos) acc ->
        let usage =
          match loc with
          | DesugaredScopeVar (v, st) ->
            Some (ScopeDef.Var (Marked.unmark v, st))
          | SubScopeVar (_, sub_index, sub_var) ->
            Some
              (ScopeDef.SubScopeVar
                 ( Marked.unmark sub_index,
                   Marked.unmark sub_var,
                   Marked.get_mark sub_index ))
          | ToplevelVar _ -> None
        in
        match usage with Some u -> ScopeDefMap.add u loc_pos acc | None -> acc)
      locs acc
  in
  RuleName.Map.fold
    (fun _ rule acc ->
      let locs =
        LocationSet.union
          (locations_used (Expr.unbox rule.rule_just))
          (locations_used (Expr.unbox rule.rule_cons))
      in
      add_locs acc locs)
    def ScopeDefMap.empty
