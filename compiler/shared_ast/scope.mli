(* This file is part of the Catala compiler, a specification language for tax
<   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
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

val fold_left_lets :
  f:('a -> 'e scope_let -> 'e Var.t -> 'a) ->
  init:'a ->
  'e scope_body_expr ->
  'a
(** Usage:
    [fold_left_lets ~f:(fun acc scope_let scope_let_var -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined. *)

val fold_right_lets :
  f:('expr1 scope_let -> 'expr1 Var.t -> 'a -> 'a) ->
  init:('expr1 -> 'a) ->
  'expr1 scope_body_expr ->
  'a
(** Usage:
    [fold_right_lets ~f:(fun scope_let scope_let_var acc -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined (which are before in the program order). *)

val map_exprs_in_lets :
  ?reset_types:bool ->
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

val fold_left :
  f:('a -> 'expr1 code_item -> 'expr1 Var.t -> 'a) ->
  init:'a ->
  'expr1 code_item_list ->
  'a
(** Usage: [fold_left ~f:(fun acc code_def code_var -> ...) ~init code_def],
    where [code_var] is the variable bound to the code item in the next code
    items to be examined. *)

val fold_right :
  f:('expr1 code_item -> 'expr1 Var.t -> 'a -> 'a) ->
  init:'a ->
  'expr1 code_item_list ->
  'a
(** Usage:
    [fold_right_scope ~f:(fun  scope_def scope_var acc -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined (which are before in the program order). *)

val map :
  f:('e1 code_item -> 'e2 code_item Bindlib.box) ->
  varf:('e1 Var.t -> 'e2 Var.t) ->
  'e1 code_item_list ->
  'e2 code_item_list Bindlib.box

val map_ctx :
  f:('ctx -> 'e1 code_item -> 'ctx * 'e2 code_item Bindlib.box) ->
  varf:('e1 Var.t -> 'e2 Var.t) ->
  'ctx ->
  'e1 code_item_list ->
  'e2 code_item_list Bindlib.box
(** Similar to [map], but a context is passed left-to-right through the given
    function *)

val fold_map :
  f:('ctx -> 'e1 Var.t -> 'e1 code_item -> 'ctx * 'e2 code_item Bindlib.box) ->
  varf:('e1 Var.t -> 'e2 Var.t) ->
  'ctx ->
  'e1 code_item_list ->
  'ctx * 'e2 code_item_list Bindlib.box

val map_exprs :
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 code_item_list ->
  'expr2 code_item_list Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)

val get_body_mark : (_, 'm) gexpr scope_body -> 'm mark

(** {2 Conversions} *)

val to_expr :
  decl_ctx -> ('a any, 'm) gexpr scope_body -> 'm mark -> ('a, 'm) boxed_gexpr
(** Usage: [to_expr ctx body scope_position] where [scope_position] corresponds
    to the line of the scope declaration for instance. *)

type 'e scope_name_or_var = ScopeName of ScopeName.t | ScopeVar of 'e Var.t

val unfold :
  decl_ctx ->
  ((_, 'm) gexpr as 'e) code_item_list ->
  'm mark ->
  'e scope_name_or_var ->
  'e boxed

val build_typ_from_sig :
  decl_ctx -> StructName.t -> StructName.t -> Pos.t -> typ
(** [build_typ_from_sig ctx in_struct out_struct pos] builds the arrow type for
    the specified scope *)

(** {2 Analysis and tests} *)

val free_vars_body_expr : 'e scope_body_expr -> 'e Var.Set.t
val free_vars_item : 'e code_item -> 'e Var.Set.t
val free_vars : 'e code_item_list -> 'e Var.Set.t
