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

(** Functions handling the scope structures of [shared_ast] *)

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
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 scope_body_expr ->
  'expr2 scope_body_expr Bindlib.box

val fold_left :
  f:('a -> 'expr1 scope_def -> 'expr1 Var.t -> 'a) ->
  init:'a ->
  'expr1 scopes ->
  'a
(** Usage: [fold_left ~f:(fun acc scope_def scope_var -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined. *)

val fold_right :
  f:('expr1 scope_def -> 'expr1 Var.t -> 'a -> 'a) ->
  init:'a ->
  'expr1 scopes ->
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

val map_exprs :
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 scopes ->
  'expr2 scopes Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)

val get_body_mark : (_, 'm mark) gexpr scope_body -> 'm mark

(** {2 Conversions} *)

val format :
  ?debug:bool (** [true] for debug printing *) ->
  decl_ctx ->
  Format.formatter ->
  ScopeName.t * 'e scope_body ->
  unit

val to_expr :
  decl_ctx ->
  ('a any, 'm mark) gexpr scope_body ->
  'm mark ->
  ('a, 'm mark) boxed_gexpr
(** Usage: [to_expr ctx body scope_position] where [scope_position] corresponds
    to the line of the scope declaration for instance. *)

type 'e scope_name_or_var = ScopeName of ScopeName.t | ScopeVar of 'e Var.t

val unfold :
  decl_ctx ->
  ((_, 'm mark) gexpr as 'e) scopes ->
  'm mark ->
  'e scope_name_or_var ->
  'e boxed

val build_typ_from_sig :
  decl_ctx -> StructName.t -> StructName.t -> Pos.t -> typ
(** [build_typ_from_sig ctx in_struct out_struct pos] builds the arrow type for
    the specified scope *)

(** {2 Analysis and tests} *)

val free_vars_body_expr : 'e scope_body_expr -> 'e Var.Set.t
val free_vars_body : 'e scope_body -> 'e Var.Set.t
val free_vars : 'e scopes -> 'e Var.Set.t
