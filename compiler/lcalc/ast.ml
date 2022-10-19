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

type lit = lcalc glit

type 'm naked_expr = (lcalc, 'm mark) naked_gexpr
and 'm expr = (lcalc, 'm mark) gexpr

type 'm program = 'm expr Shared_ast.program

let option_enum : EnumName.t = EnumName.fresh ("eoption", Pos.no_pos)
let none_constr : EnumConstructor.t = EnumConstructor.fresh ("ENone", Pos.no_pos)
let some_constr : EnumConstructor.t = EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config : (EnumConstructor.t * typ) list =
  [none_constr, (TLit TUnit, Pos.no_pos); some_constr, (TAny, Pos.no_pos)]

(* FIXME: proper typing in all the constructors below *)

let make_none m =
  let tunit = TLit TUnit, Expr.mark_pos m in
  Expr.einj
    (Expr.elit LUnit (Expr.with_ty m tunit))
    0 option_enum
    [TLit TUnit, Pos.no_pos; TAny, Pos.no_pos]
    m

let make_some e =
  let m = Marked.get_mark e in
  Expr.einj e 1 option_enum
    [TLit TUnit, Expr.mark_pos m; TAny, Expr.mark_pos m]
    m

(** [make_matchopt_with_abs_arms arg e_none e_some] build an expression
    [match arg with |None -> e_none | Some -> e_some] and requires e_some and
    e_none to be in the form [EAbs ...].*)
let make_matchopt_with_abs_arms arg e_none e_some =
  let m = Marked.get_mark arg in
  Expr.ematch arg [e_none; e_some] option_enum m

(** [make_matchopt pos v tau arg e_none e_some] builds an expression
    [match arg with | None () -> e_none | Some v -> e_some]. It binds v to
    e_some, permitting it to be used inside the expression. There is no
    requirements on the form of both e_some and e_none. *)
let make_matchopt pos v tau arg e_none e_some =
  let x = Var.make "_" in
  make_matchopt_with_abs_arms arg
    (Expr.make_abs [| x |] e_none [TLit TUnit, pos] pos)
    (Expr.make_abs [| v |] e_some [tau] pos)

let handle_default = Var.make "handle_default"
let handle_default_opt = Var.make "handle_default_opt"
