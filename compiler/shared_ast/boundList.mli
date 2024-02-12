(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Bound lists are non-empty linked lists where each element is a binder onto
    the next. They are useful for ordered program definitions, like nested
    let-ins.

    [let a = e1 in e2] is thus represented as [Cons (e1, {a. Last e2})].

    The following provides a few utility functions for their traversal and
    manipulation. In particular, [map] functions take care of unbinding, then
    properly rebinding the variables. *)

open Definitions

type ('e, 'elt, 'last) t = ('e, 'elt, 'last) bound_list =
  | Last of 'last
  | Cons of 'elt * ('e, ('e, 'elt, 'last) t) binder

val last : (_, _, 'a) t -> 'a
val iter : f:('e Var.t -> 'elt -> unit) -> ('e, 'elt, 'last) t -> 'last
val find : f:('elt -> 'a option) -> (_, 'elt, _) t -> 'a

val fold_left :
  f:('acc -> 'elt -> 'e Var.t -> 'acc) ->
  init:'acc ->
  ('e, 'elt, 'last) t ->
  'acc * 'last

val fold_left2 :
  f:('acc -> 'elt1 -> 'elt2 -> 'e Var.t -> 'acc) ->
  init:'acc ->
  ('e, 'elt1, 'last1) t ->
  ('e, 'elt2, 'last2) t ->
  'acc * ('last1 * 'last2)

val fold_right :
  f:('elt -> 'e Var.t -> 'acc -> 'acc) ->
  init:('last -> 'acc) ->
  ('e, 'elt, 'last) t ->
  'acc

val fold_lr :
  top:'dacc ->
  down:('e Var.t -> 'elt -> 'dacc -> 'dacc) ->
  bottom:('last -> 'dacc -> 'uacc) ->
  up:('e Var.t -> 'elt -> 'uacc -> 'uacc) ->
  ('e, 'elt, 'last) t ->
  'uacc
(** Bi-directional fold: [down] accumulates downwards, starting from [top]; upon
    reaching [last], [bottom] is called; then [up] accumulates on the way back
    up *)

val map :
  f:('e1 Var.t -> 'elt1 -> 'e2 Var.t * 'elt2 Bindlib.box) ->
  last:('last1 -> 'last2 Bindlib.box) ->
  ('e1, 'elt1, 'last1) t ->
  ('e2, 'elt2, 'last2) t Bindlib.box

val fold_map :
  f:('ctx -> 'e1 Var.t -> 'elt1 -> 'ctx * 'e2 Var.t * 'elt2 Bindlib.box) ->
  last:('ctx -> 'last1 -> 'ret * 'last2 Bindlib.box) ->
  init:'ctx ->
  ('e1, 'elt1, 'last1) t ->
  'ret * ('e2, 'elt2, 'last2) t Bindlib.box

val equal :
  f:('elt -> 'elt -> bool) ->
  last:('last -> 'last -> bool) ->
  (('e, 'elt, 'last) t as 'l) ->
  'l ->
  bool

val compare :
  f:('elt -> 'elt -> int) ->
  last:('last -> 'last -> int) ->
  (('e, 'elt, 'last) t as 'l) ->
  'l ->
  int
