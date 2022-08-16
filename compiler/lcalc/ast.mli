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
open Shared_ast

(** Abstract syntax tree for the lambda calculus *)

(** {1 Abstract syntax tree} *)

type lit = lcalc glit

type 'm expr = (lcalc, 'm mark) gexpr
and 'm marked_expr = (lcalc, 'm mark) marked_gexpr

type 'm program = ('m expr, 'm) program_generic

(** {1 Variable helpers} *)

type 'm var = 'm expr Var.t
type 'm vars = 'm expr Var.vars

(** {1 Language terms construction}*)

val make_var : ('m var, 'm) marked -> 'm marked_expr Bindlib.box

val make_abs :
  'm vars ->
  'm marked_expr Bindlib.box ->
  typ Marked.pos list ->
  'm mark ->
  'm marked_expr Bindlib.box

val make_app :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

val make_let_in :
  'm var ->
  typ Marked.pos ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  Pos.t ->
  'm marked_expr Bindlib.box

val make_multiple_let_in :
  'm vars ->
  typ Marked.pos list ->
  'm marked_expr Bindlib.box list ->
  'm marked_expr Bindlib.box ->
  Pos.t ->
  'm marked_expr Bindlib.box

val option_enum : EnumName.t
val none_constr : EnumConstructor.t
val some_constr : EnumConstructor.t
val option_enum_config : (EnumConstructor.t * typ Marked.pos) list
val make_none : 'm mark -> 'm marked_expr Bindlib.box
val make_some : 'm marked_expr Bindlib.box -> 'm marked_expr Bindlib.box

val make_matchopt_with_abs_arms :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box

val make_matchopt :
  'm mark ->
  'm var ->
  typ Marked.pos ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box
(** [e' = make_matchopt'' pos v e e_none e_some] Builds the term corresponding
    to [match e with | None -> fun () -> e_none |Some -> fun v -> e_some]. *)

(** {1 Special symbols} *)

val handle_default : untyped var
val handle_default_opt : untyped var
