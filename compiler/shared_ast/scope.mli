(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Functions handling the code item structures of [shared_ast], in particular
    the scopes *)

open Catala_utils
open Definitions

(** {2 Traversal functions} *)

val map_exprs_in_lets :
  ?typ:(typ -> typ) ->
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 scope_body_expr ->
  'expr2 scope_body_expr Bindlib.box
(** Usage
    [map_exprs_in_lets ~f:(fun e -> ...) ~varf:(fun var -> ...) scope_body_expr],
    where [e] is the right-hand-side of a scope let or the result of the scope
    body, and [var] represents the left-hand-side variable of a scope let.
    [~varf] is usually the identity function or [Var.translate] when the map
    sends the expression to a new flavor of the shared AST. If [~reset_types] is
    activated, then the resulting types in the scope let left-hand-sides will be
    reset to [TAny]. *)

val map_exprs :
  ?typ:(typ -> typ) ->
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 code_item_list ->
  'expr2 code_item_list Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)

val fold_exprs :
  f:('acc -> 'expr -> typ -> 'acc) -> init:'acc -> 'expr code_item_list -> 'acc

(** {2 Conversions} *)

val to_expr : decl_ctx -> ('a any, 'm) gexpr scope_body -> ('a, 'm) boxed_gexpr
(** Usage: [to_expr ctx body scope_position] where [scope_position] corresponds
    to the line of the scope declaration for instance. *)

val unfold :
  decl_ctx -> ((_, 'm) gexpr as 'e) code_item_list -> ScopeName.t -> 'e boxed

val typ : _ scope_body -> typ
(** builds the arrow type for the specified scope *)

val input_type : typ -> Runtime.io_input Mark.pos -> typ
(** Returns the correct input type for scope input variables: this is [typ] for
    non-reentrant variables, but for reentrant variables, it is nested in a
    [TDefault], which only applies to the return type on functions. Note that
    this doesn't take thunking into account (thunking is added during the
    scopelang->dcalc translation) *)

(** {2 Analysis and tests} *)

val free_vars_body_expr : 'e scope_body_expr -> 'e Var.Set.t
val free_vars_item : 'e code_item -> 'e Var.Set.t
val free_vars : 'e code_item_list -> 'e Var.Set.t
