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

open Types

(** {2 Traversal functions} *)

val fold_left_lets :
  f:('a -> 'e scope_let -> 'e Bindlib.var -> 'a) ->
  init:'a ->
  'e scope_body_expr ->
  'a
(** Usage:
    [fold_left_lets ~f:(fun acc scope_let scope_let_var -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined. *)

val fold_right_lets :
  f:('expr1 scope_let -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:('expr1 marked -> 'a) ->
  'expr1 scope_body_expr ->
  'a
(** Usage:
    [fold_right_lets ~f:(fun scope_let scope_let_var acc -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined (which are before in the program order). *)

val map_exprs_in_lets :
  f:('expr1 marked -> 'expr2 marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  'expr1 scope_body_expr ->
  'expr2 scope_body_expr Bindlib.box

val fold_left :
  f:('a -> 'expr1 scope_def -> 'expr1 Bindlib.var -> 'a) ->
  init:'a ->
  'expr1 scopes ->
  'a
(** Usage: [fold_left ~f:(fun acc scope_def scope_var -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined. *)

val fold_right :
  f:('expr1 scope_def -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:'a ->
  'expr1 scopes ->
  'a
(** Usage:
    [fold_right_scope ~f:(fun  scope_def scope_var acc -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined (which are before in the program order). *)

val map :
  f:('e scope_def -> 'e scope_def Bindlib.box) ->
  'e scopes ->
  'e scopes Bindlib.box

val map_exprs :
  f:('expr1 marked -> 'expr2 marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  'expr1 scopes ->
  'expr2 scopes Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)

(** {2 Other helpers} *)
