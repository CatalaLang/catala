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
  module Base = struct
    type sub_scope_input_kind =
      | Direct of {
          sub_scope_name : ScopeName.t;
          var_within_sub_scope : ScopeVar.t;
        }
      | NestedSubScope of {
          sub_scope_name : ScopeName.t;
          nested_sub_scope_var_within_sub_scope : ScopeVar.t;
          nested_input_var : sub_scope_input_kind;
        }

    type kind =
      | ScopeVarKind of StateName.t option
      | SubScopeInputKind of sub_scope_input_kind

    type t = {
      scope_def_var_within_scope : ScopeVar.t Mark.pos;
      scope_def_kind : kind;
    }

    let equal_kind k1 k2 =
      match k1, k2 with
      | ScopeVarKind s1, ScopeVarKind s2 -> Option.equal StateName.equal s1 s2
      | SubScopeInputKind i1, SubScopeInputKind i2 ->
        let rec aux i1 i2 =
          match i1, i2 with
          | ( Direct { var_within_sub_scope = v1; _ },
              Direct { var_within_sub_scope = v2; _ } ) ->
            ScopeVar.equal v1 v2
          | ( NestedSubScope
                {
                  nested_sub_scope_var_within_sub_scope = v1;
                  nested_input_var = i1;
                  _;
                },
              NestedSubScope
                {
                  nested_sub_scope_var_within_sub_scope = v2;
                  nested_input_var = i2;
                  _;
                } ) ->
            ScopeVar.equal v1 v2 && aux i1 i2
          | _ -> false
        in
        aux i1 i2
      | (ScopeVarKind _ | SubScopeInputKind _), _ -> false

    let equal
        { scope_def_var_within_scope = v1; scope_def_kind = k1 }
        { scope_def_var_within_scope = v2; scope_def_kind = k2 } =
      ScopeVar.equal (Mark.remove v1) (Mark.remove v2) && equal_kind k1 k2

    let compare_kind k1 k2 =
      match k1, k2 with
      | ScopeVarKind st1, ScopeVarKind st2 ->
        Option.compare StateName.compare st1 st2
      | SubScopeInputKind i1, SubScopeInputKind i2 ->
        let rec aux i1 i2 =
          match i1, i2 with
          | ( Direct { var_within_sub_scope = v1; _ },
              Direct { var_within_sub_scope = v2; _ } ) ->
            ScopeVar.compare v1 v2
          | ( NestedSubScope
                {
                  nested_sub_scope_var_within_sub_scope = v1;
                  nested_input_var = i1;
                  _;
                },
              NestedSubScope
                {
                  nested_sub_scope_var_within_sub_scope = v2;
                  nested_input_var = i2;
                  _;
                } ) ->
            let x = ScopeVar.compare v1 v2 in
            if x <> 0 then x else aux i1 i2
          | Direct _, NestedSubScope _ -> -1
          | NestedSubScope _, Direct _ -> 1
        in
        aux i1 i2
      | ScopeVarKind _, SubScopeInputKind _ -> -1
      | SubScopeInputKind _, ScopeVarKind _ -> 1

    let compare
        { scope_def_var_within_scope = v1; scope_def_kind = k1 }
        { scope_def_var_within_scope = v2; scope_def_kind = k2 } =
      match Mark.compare ScopeVar.compare v1 v2 with
      | 0 -> compare_kind k1 k2
      | n -> n

    let get_position { scope_def_var_within_scope = v; _ } = Mark.get v

    let rec format_kind ppf = function
      | ScopeVarKind None -> ()
      | ScopeVarKind (Some st) -> Format.fprintf ppf "@%a" StateName.format st
      | SubScopeInputKind (Direct { var_within_sub_scope = v; _ }) ->
        Format.fprintf ppf ".%a" ScopeVar.format v
      | SubScopeInputKind
          (NestedSubScope
            {
              nested_sub_scope_var_within_sub_scope = v;
              nested_input_var = i;
              _;
            }) ->
        Format.fprintf ppf ".%a%a" ScopeVar.format v format_kind
          (SubScopeInputKind i)

    let format ppf { scope_def_var_within_scope = v; scope_def_kind = k } =
      ScopeVar.format ppf (Mark.remove v);
      format_kind ppf k

    let rec hash_kind = function
      | ScopeVarKind None -> 0
      | ScopeVarKind (Some st) -> StateName.hash st
      | SubScopeInputKind (Direct { var_within_sub_scope = v; _ }) ->
        ScopeVar.hash v
      | SubScopeInputKind
          (NestedSubScope
            {
              nested_sub_scope_var_within_sub_scope = v;
              nested_input_var = i;
              _;
            }) ->
        Int.logxor (ScopeVar.hash v) (hash_kind (SubScopeInputKind i))

    let hash { scope_def_var_within_scope = v; scope_def_kind = k } =
      Int.logxor (ScopeVar.hash (Mark.remove v)) (hash_kind k)
  end

  include Base
  module Map = Map.Make (Base)
  module Set = Set.Make (Base)
end

module AssertionName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_blue))
    end)
    ()

(** {1 AST} *)

type location = desugared glocation

module LocationSet : Set.S with type elt = location Mark.pos = Set.Make (struct
  type t = location Mark.pos

  let compare = Expr.compare_location
end)

type expr = (desugared, untyped) gexpr

module ExprMap = Map.Make (struct
  type t = expr

  let compare = Expr.compare
  let format = Expr.format
end)

type io = { io_output : bool Mark.pos; io_input : Runtime.io_input Mark.pos }

type exception_situation =
  | BaseCase
  | ExceptionToLabel of LabelName.t Mark.pos
  | ExceptionToRule of RuleName.t Mark.pos

type label_situation = ExplicitlyLabeled of LabelName.t Mark.pos | Unlabeled

type rule = {
  rule_id : RuleName.t;
  rule_just : expr boxed;
  rule_cons : expr boxed;
  rule_parameter : (expr Var.t Mark.pos * typ) list Mark.pos option;
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
      let j1, j1m = r1.rule_just in
      let j2, j2m = r2.rule_just in
      match
        Bindlib.unbox
          (Bindlib.box_apply2
             (fun j1 j2 -> Expr.compare (j1, j1m) (j2, j2m))
             j1 j2)
      with
      | 0 ->
        let c1, c1m = r1.rule_cons in
        let c2, c2m = r2.rule_cons in
        Bindlib.unbox
          (Bindlib.box_apply2
             (fun c1 c2 -> Expr.compare (c1, c1m) (c2, c2m))
             c1 c2)
      | n -> n)
    | Some (l1, _), Some (l2, _) ->
      ListLabels.compare l1 l2 ~cmp:(fun ((v1, _), t1) ((v2, _), t2) ->
          match Type.compare t1 t2 with
          | 0 -> (
            let open Bindlib in
            let b1 = bind_var v1 (Expr.Box.lift r1.rule_just) in
            let b2 = bind_var v2 (Expr.Box.lift r2.rule_just) in
            match
              Bindlib.unbox
                (Bindlib.box_apply2
                   (fun b1 b2 ->
                     let _, j1, j2 = unbind2 b1 b2 in
                     Expr.compare j1 j2)
                   b1 b2)
            with
            | 0 ->
              let b1 = bind_var v1 (Expr.Box.lift r1.rule_cons) in
              let b2 = bind_var v2 (Expr.Box.lift r2.rule_cons) in
              Bindlib.unbox
                (Bindlib.box_apply2
                   (fun b1 b2 ->
                     let _, c1, c2 = unbind2 b1 b2 in
                     Expr.compare c1 c2)
                   b1 b2)
            | n -> n)
          | n -> n)
    | None, Some _ -> -1
    | Some _, None -> 1
end

let empty_rule
    (pos : Pos.t)
    (parameters : (Uid.MarkedString.info * typ) list Mark.pos option) : rule =
  {
    rule_just = Expr.box (ELit (LBool false), Untyped { pos });
    rule_cons = Expr.box (EEmpty, Untyped { pos });
    rule_parameter =
      Option.map
        (Mark.map (List.map (fun (lbl, typ) -> Mark.map Var.make lbl, typ)))
        parameters;
    rule_exception = BaseCase;
    rule_id = RuleName.fresh ("empty", pos);
    rule_label = Unlabeled;
  }

let always_false_rule
    (pos : Pos.t)
    (parameters : (Uid.MarkedString.info * typ) list Mark.pos option) : rule =
  {
    rule_just = Expr.box (ELit (LBool true), Untyped { pos });
    rule_cons = Expr.box (ELit (LBool false), Untyped { pos });
    rule_parameter =
      Option.map
        (Mark.map (List.map (fun (lbl, typ) -> Mark.map Var.make lbl, typ)))
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
  | FixedBy of reference_typ Mark.pos
  | VariesWith of unit * variation_typ Mark.pos option

type scope_def = {
  scope_def_rules : rule RuleName.Map.t;
  scope_def_typ : typ;
  scope_def_parameters : (Uid.MarkedString.info * typ) list Mark.pos option;
  scope_def_is_condition : bool;
  scope_def_io : io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVar.Map.t;
  scope_sub_scopes : ScopeName.t ScopeVar.Map.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDef.Map.t;
  scope_assertions : assertion AssertionName.Map.t;
  scope_options : catala_option Mark.pos list;
  scope_meta_assertions : meta_assertion list;
}

type modul = {
  module_scopes : scope ScopeName.Map.t;
  module_topdefs : (expr option * typ) TopdefName.Map.t;
}

type program = {
  program_module_name : Ident.t Mark.pos option;
  program_ctx : decl_ctx;
  program_modules : modul ModuleName.Map.t;
  program_root : modul;
  program_lang : Global.backend_lang;
}

let rec locations_used e : LocationSet.t =
  match e with
  | ELocation l, m -> LocationSet.singleton (l, Expr.mark_pos m)
  | e ->
    Expr.shallow_fold
      (fun e -> LocationSet.union (locations_used e))
      e LocationSet.empty

let free_variables (def : rule RuleName.Map.t) : Pos.t ScopeDef.Map.t =
  let add_locs (acc : Pos.t ScopeDef.Map.t) (locs : LocationSet.t) :
      Pos.t ScopeDef.Map.t =
    LocationSet.fold
      (fun (loc, loc_pos) acc ->
        let usage =
          match loc with
          | DesugaredScopeVar { name; state } ->
            Some
              {
                ScopeDef.scope_def_var_within_scope = name;
                scope_def_kind = ScopeDef.ScopeVarKind state;
              }
          | ToplevelVar _ -> None
        in
        match usage with
        | Some u -> ScopeDef.Map.add u loc_pos acc
        | None -> acc)
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
    def ScopeDef.Map.empty

let fold_exprs ~(f : 'a -> expr -> 'a) ~(init : 'a) (p : program) : 'a =
  let acc =
    ScopeName.Map.fold
      (fun _ scope acc ->
        let acc =
          ScopeDef.Map.fold
            (fun _ scope_def acc ->
              RuleName.Map.fold
                (fun _ rule acc ->
                  f
                    (f acc (Expr.unbox rule.rule_just))
                    (Expr.unbox rule.rule_cons))
                scope_def.scope_def_rules acc)
            scope.scope_defs acc
        in
        let acc =
          AssertionName.Map.fold
            (fun _ assertion acc -> f acc (Expr.unbox assertion))
            scope.scope_assertions acc
        in
        acc)
      p.program_root.module_scopes init
  in
  TopdefName.Map.fold
    (fun _ (e, _) acc -> Option.fold ~none:acc ~some:(f acc) e)
    p.program_root.module_topdefs acc
