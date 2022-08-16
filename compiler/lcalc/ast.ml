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
include Shared_ast
module D = Dcalc.Ast

type lit = lcalc glit

type 'm expr = (lcalc, 'm mark) gexpr
and 'm marked_expr = (lcalc, 'm mark) marked_gexpr

type 'm program = ('m expr, 'm) program_generic
type 'm var = 'm expr Var.t
type 'm vars = 'm expr Var.vars

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
    Expr.map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> TArrow (m1.ty, m2.ty), m1.pos)
      m_e1 m_e2
  in
  make_app (make_abs [| x |] e2 [tau] m_abs) [e1] m_e2

let make_multiple_let_in xs taus e1s e2 pos =
  (* let m_e1s = List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s in *)
  let m_e1s =
    Expr.fold_marks List.hd
      (fun tys ->
        TTuple (List.map (fun t -> t.ty) tys, None), (List.hd tys).pos)
      (List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s)
  in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    Expr.map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> Marked.mark pos (TArrow (m1.ty, m2.ty)))
      m_e1s m_e2
  in
  make_app (make_abs xs e2 taus m_abs) e1s m_e2

let ( let+ ) x f = Bindlib.box_apply f x
let ( and+ ) x y = Bindlib.box_pair x y
let option_enum : EnumName.t = EnumName.fresh ("eoption", Pos.no_pos)
let none_constr : EnumConstructor.t = EnumConstructor.fresh ("ENone", Pos.no_pos)
let some_constr : EnumConstructor.t = EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config : (EnumConstructor.t * typ Marked.pos) list =
  [none_constr, (TLit TUnit, Pos.no_pos); some_constr, (TAny, Pos.no_pos)]

(* FIXME: proper typing in all the constructors below *)

let make_none m =
  let mark = Marked.mark m in
  let tunit = TLit TUnit, Expr.mark_pos m in
  Bindlib.box
  @@ mark
  @@ EInj
       ( Marked.mark
           (Expr.map_mark (fun pos -> pos) (fun _ -> tunit) m)
           (ELit LUnit),
         0,
         option_enum,
         [TLit TUnit, Pos.no_pos; TAny, Pos.no_pos] )

let make_some e =
  let m = Marked.get_mark @@ Bindlib.unbox e in
  let mark = Marked.mark m in
  let+ e in
  mark
  @@ EInj
       (e, 1, option_enum, [TLit TUnit, Expr.mark_pos m; TAny, Expr.mark_pos m])

(** [make_matchopt_with_abs_arms arg e_none e_some] build an expression
    [match arg with |None -> e_none | Some -> e_some] and requires e_some and
    e_none to be in the form [EAbs ...].*)
let make_matchopt_with_abs_arms arg e_none e_some =
  let m = Marked.get_mark @@ Bindlib.unbox arg in
  let mark = Marked.mark m in
  let+ arg and+ e_none and+ e_some in
  mark @@ EMatch (arg, [e_none; e_some], option_enum)

(** [make_matchopt pos v tau arg e_none e_some] builds an expression
    [match arg with | None () -> e_none | Some v -> e_some]. It binds v to
    e_some, permitting it to be used inside the expression. There is no
    requirements on the form of both e_some and e_none. *)
let make_matchopt m v tau arg e_none e_some =
  let x = Var.make "_" in

  make_matchopt_with_abs_arms arg
    (make_abs (Array.of_list [x]) e_none [TLit TUnit, Expr.mark_pos m] m)
    (make_abs (Array.of_list [v]) e_some [tau] m)

let handle_default = Var.make "handle_default"
let handle_default_opt = Var.make "handle_default_opt"
