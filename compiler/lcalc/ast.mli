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

open Shared_ast

(** Abstract syntax tree for the lambda calculus *)

(** {1 Abstract syntax tree} *)

type lit = lcalc glit

type 'm naked_expr = (lcalc, 'm mark) naked_gexpr
and 'm expr = (lcalc, 'm mark) gexpr

type 'm program = 'm naked_expr Shared_ast.program

(** {1 Language terms construction}*)

val option_enum : EnumName.t
val none_constr : EnumConstructor.t
val some_constr : EnumConstructor.t
val option_enum_config : (EnumConstructor.t * typ) list
val make_none : 'm mark -> 'm expr Bindlib.box
val make_some : 'm expr Bindlib.box -> 'm expr Bindlib.box

val make_matchopt_with_abs_arms :
  'm expr Bindlib.box ->
  'm expr Bindlib.box ->
  'm expr Bindlib.box ->
  'm expr Bindlib.box

val make_matchopt :
  'm mark ->
  'm naked_expr Var.t ->
  typ ->
  'm expr Bindlib.box ->
  'm expr Bindlib.box ->
  'm expr Bindlib.box ->
  'm expr Bindlib.box
(** [e' = make_matchopt'' pos v e e_none e_some] Builds the term corresponding
    to [match e with | None -> fun () -> e_none |Some -> fun v -> e_some]. *)

(** {1 Special symbols} *)

val handle_default : untyped naked_expr Var.t
val handle_default_opt : untyped naked_expr Var.t
