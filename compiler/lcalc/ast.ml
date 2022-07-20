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

open Utils
module D = Dcalc.Ast

type lit =
  | LBool of bool
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of Runtime.date
  | LDuration of Runtime.duration

type except = ConflictError | EmptyError | NoValueProvided | Crash
type 'm mark = 'm D.mark

type 'm marked_expr = ('m expr, 'm) D.marked

and 'm expr =
  | EVar of 'm expr Bindlib.var
  | ETuple of 'm marked_expr list * D.StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of
      'm marked_expr * int * D.StructName.t option * D.typ Marked.pos list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of 'm marked_expr * int * D.EnumName.t * D.typ Marked.pos list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of 'm marked_expr * 'm marked_expr list * D.EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of 'm marked_expr list
  | ELit of lit
  | EAbs of ('m expr, 'm marked_expr) Bindlib.mbinder * D.typ Marked.pos list
  | EApp of 'm marked_expr * 'm marked_expr list
  | EAssert of 'm marked_expr
  | EOp of D.operator
  | EIfThenElse of 'm marked_expr * 'm marked_expr * 'm marked_expr
  | ERaise of except
  | ECatch of 'm marked_expr * except * 'm marked_expr

type 'm program = {
  decl_ctx : Dcalc.Ast.decl_ctx;
  scopes : ('m expr, 'm) Dcalc.Ast.scopes;
}

(* <copy-paste from dcalc/ast.ml> *)

let evar v mark = Bindlib.box_apply (Marked.mark mark) (Bindlib.box_var v)

let etuple args s mark =
  Bindlib.box_apply (fun args -> ETuple (args, s), mark) (Bindlib.box_list args)

let etupleaccess e1 i s typs mark =
  Bindlib.box_apply (fun e1 -> ETupleAccess (e1, i, s, typs), mark) e1

let einj e1 i e_name typs mark =
  Bindlib.box_apply (fun e1 -> EInj (e1, i, e_name, typs), mark) e1

let ematch arg arms e_name mark =
  Bindlib.box_apply2
    (fun arg arms -> EMatch (arg, arms, e_name), mark)
    arg (Bindlib.box_list arms)

let earray args mark =
  Bindlib.box_apply (fun args -> EArray args, mark) (Bindlib.box_list args)

let elit l mark = Bindlib.box (ELit l, mark)

let eabs binder typs mark =
  Bindlib.box_apply (fun binder -> EAbs (binder, typs), mark) binder

let eapp e1 args mark =
  Bindlib.box_apply2
    (fun e1 args -> EApp (e1, args), mark)
    e1 (Bindlib.box_list args)

let eassert e1 mark = Bindlib.box_apply (fun e1 -> EAssert e1, mark) e1
let eop op mark = Bindlib.box (EOp op, mark)

let eifthenelse e1 e2 e3 pos =
  Bindlib.box_apply3 (fun e1 e2 e3 -> EIfThenElse (e1, e2, e3), pos) e1 e2 e3

type 'm var = 'm expr Bindlib.var
type 'm vars = 'm expr Bindlib.mvar

let new_var s = Bindlib.new_var (fun x -> EVar x) s

module Var = struct
  type t = V : 'a var -> t
  (* See Dcalc.Ast.var *)

  let t v = V v
  let get (V v) = Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)
  let compare (V x) (V y) = Bindlib.compare_vars x y
end

module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)

(* </copy-paste> *)

let eraise e1 pos = Bindlib.box (ERaise e1, pos)

let ecatch e1 exn e2 pos =
  Bindlib.box_apply2 (fun e1 e2 -> ECatch (e1, exn, e2), pos) e1 e2

let translate_var v = Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)

let map_expr ctx ~f e =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar v -> evar (translate_var v) (Marked.get_mark e)
  | EApp (e1, args) ->
    eapp (f ctx e1) (List.map (f ctx) args) (Marked.get_mark e)
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    eabs (Bindlib.bind_mvar (Array.map translate_var vars) (f ctx body)) typs m
  | ETuple (args, s) -> etuple (List.map (f ctx) args) s (Marked.get_mark e)
  | ETupleAccess (e1, n, s_name, typs) ->
    etupleaccess ((f ctx) e1) n s_name typs (Marked.get_mark e)
  | EInj (e1, i, e_name, typs) ->
    einj ((f ctx) e1) i e_name typs (Marked.get_mark e)
  | EMatch (arg, arms, e_name) ->
    ematch ((f ctx) arg) (List.map (f ctx) arms) e_name (Marked.get_mark e)
  | EArray args -> earray (List.map (f ctx) args) (Marked.get_mark e)
  | ELit l -> elit l (Marked.get_mark e)
  | EAssert e1 -> eassert ((f ctx) e1) (Marked.get_mark e)
  | EOp op -> Bindlib.box (EOp op, Marked.get_mark e)
  | ERaise exn -> eraise exn (Marked.get_mark e)
  | EIfThenElse (e1, e2, e3) ->
    eifthenelse ((f ctx) e1) ((f ctx) e2) ((f ctx) e3) (Marked.get_mark e)
  | ECatch (e1, exn, e2) -> ecatch (f ctx e1) exn (f ctx e2) (Marked.get_mark e)

let rec map_expr_top_down ~f e =
  map_expr () ~f:(fun () -> map_expr_top_down ~f) (f e)

let map_expr_marks ~f e =
  Bindlib.unbox
  @@ map_expr_top_down ~f:(fun e -> Marked.(mark (f (get_mark e)) (unmark e))) e

let untype_expr e =
  map_expr_marks ~f:(fun m -> Untyped { pos = D.mark_pos m }) e

let untype_program prg =
  {
    prg with
    scopes =
      Bindlib.unbox
        (D.map_exprs_in_scopes
           ~f:(fun e -> Bindlib.box (untype_expr e))
           ~varf:translate_var prg.scopes);
  }

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let box_expr (e : 'm marked_expr) : 'm marked_expr Bindlib.box =
  let rec id_t () e = map_expr () ~f:id_t e in
  id_t () e

let make_var (x, mark) =
  Bindlib.box_apply (fun x -> x, mark) (Bindlib.box_var x)

let make_abs xs e taus mark =
  Bindlib.box_apply (fun b -> EAbs (b, taus), mark) (Bindlib.bind_mvar xs e)

let make_app e u mark =
  Bindlib.box_apply2 (fun e u -> EApp (e, u), mark) e (Bindlib.box_list u)

let make_let_in x tau e1 e2 pos =
  let m_e1 = Marked.get_mark (Bindlib.unbox e1) in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    D.map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> TArrow (m1.ty, m2.ty), m1.pos)
      m_e1 m_e2
  in
  make_app (make_abs [| x |] e2 [tau] m_abs) [e1] m_e2

let make_multiple_let_in xs taus e1s e2 pos =
  (* let m_e1s = List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s in *)
  let m_e1s =
    D.fold_marks List.hd
      (fun tys ->
        D.TTuple (List.map (fun t -> t.D.ty) tys, None), (List.hd tys).D.pos)
      (List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s)
  in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    D.map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> Marked.mark pos (D.TArrow (m1.ty, m2.ty)))
      m_e1s m_e2
  in
  make_app (make_abs xs e2 taus m_abs) e1s m_e2

let ( let+ ) x f = Bindlib.box_apply f x
let ( and+ ) x y = Bindlib.box_pair x y
let option_enum : D.EnumName.t = D.EnumName.fresh ("eoption", Pos.no_pos)

let none_constr : D.EnumConstructor.t =
  D.EnumConstructor.fresh ("ENone", Pos.no_pos)

let some_constr : D.EnumConstructor.t =
  D.EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config : (D.EnumConstructor.t * D.typ Marked.pos) list =
  [none_constr, (D.TLit D.TUnit, Pos.no_pos); some_constr, (D.TAny, Pos.no_pos)]

(* FIXME: proper typing in all the constructors below *)

let make_none m =
  let mark = Marked.mark m in
  let tunit = D.TLit D.TUnit, D.mark_pos m in
  Bindlib.box @@ mark
  @@ EInj
       ( Marked.mark
           (D.map_mark (fun pos -> pos) (fun _ -> tunit) m)
           (ELit LUnit),
         0,
         option_enum,
         [D.TLit D.TUnit, Pos.no_pos; D.TAny, Pos.no_pos] )

let make_some e =
  let m = Marked.get_mark @@ Bindlib.unbox e in
  let mark = Marked.mark m in
  let+ e = e in
  mark
  @@ EInj
       (e, 1, option_enum, [D.TLit D.TUnit, D.mark_pos m; D.TAny, D.mark_pos m])

(** [make_matchopt_with_abs_arms arg e_none e_some] build an expression
    [match arg with |None -> e_none | Some -> e_some] and requires e_some and
    e_none to be in the form [EAbs ...].*)
let make_matchopt_with_abs_arms arg e_none e_some =
  let m = Marked.get_mark @@ Bindlib.unbox arg in
  let mark = Marked.mark m in
  let+ arg = arg and+ e_none = e_none and+ e_some = e_some in
  mark @@ EMatch (arg, [e_none; e_some], option_enum)

(** [make_matchopt pos v tau arg e_none e_some] builds an expression
    [match arg with | None () -> e_none | Some v -> e_some]. It binds v to
    e_some, permitting it to be used inside the expression. There is no
    requirements on the form of both e_some and e_none. *)
let make_matchopt m v tau arg e_none e_some =
  let x = new_var "_" in

  make_matchopt_with_abs_arms arg
    (make_abs (Array.of_list [x]) e_none [D.TLit D.TUnit, D.mark_pos m] m)
    (make_abs (Array.of_list [v]) e_some [tau] m)

let handle_default = Var.t (new_var "handle_default")
let handle_default_opt = Var.t (new_var "handle_default_opt")

type 'm binder = ('m expr, 'm marked_expr) Bindlib.binder
