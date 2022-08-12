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

(** Functions handling the types of [shared_ast] *)

open Utils
open Types

(** {2 Boxed constructors} *)

val evar :
  (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) gexpr Bindlib.var ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val etuple :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box list ->
  StructName.t option ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val etupleaccess :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  int ->
  StructName.t option ->
  marked_typ list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val einj :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  int ->
  EnumName.t ->
  marked_typ list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val ematch :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box list ->
  EnumName.t ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val earray :
  ('a, 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val elit : 'a glit -> 't -> ('a, 't) marked_gexpr Bindlib.box

val eabs :
  ( (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) gexpr,
    ('a, 't) marked_gexpr )
  Bindlib.mbinder
  Bindlib.box ->
  marked_typ list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eapp :
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eassert :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eop : operator -> 't -> ('a, 't) marked_gexpr Bindlib.box

val edefault :
  (([< desugared | scopelang | dcalc ] as 'a), 't) marked_gexpr Bindlib.box list ->
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eifthenelse :
  (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) marked_gexpr
  Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eerroronempty :
  (([< desugared | scopelang | dcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

(** ---------- *)

val map :
  'ctx ->
  f:('ctx -> ('a, 't1) marked_gexpr -> ('a, 't2) marked_gexpr Bindlib.box) ->
  (('a, 't1) gexpr, 't2) Marked.t ->
  ('a, 't2) marked_gexpr Bindlib.box

val map_top_down :
  f:(('a, 't1) marked_gexpr -> (('a, 't1) gexpr, 't2) Marked.t) ->
  ('a, 't1) marked_gexpr ->
  ('a, 't2) marked_gexpr Bindlib.box
(** Recursively applies [f] to the nodes of the expression tree. The type
    returned by [f] is hybrid since the mark at top-level has been rewritten,
    but not yet the marks in the subtrees. *)

val map_marks :
  f:('t1 -> 't2) -> ('a, 't1) marked_gexpr -> ('a, 't2) marked_gexpr Bindlib.box

val fold_left_scope_lets :
  f:('a -> ('expr, 'm) scope_let -> 'expr Bindlib.var -> 'a) ->
  init:'a ->
  ('expr, 'm) scope_body_expr ->
  'a
(** Usage:
    [fold_left_scope_lets ~f:(fun acc scope_let scope_let_var -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined. *)

val fold_right_scope_lets :
  f:(('expr1, 'm1) scope_let -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:(('expr1, 'm1) marked -> 'a) ->
  ('expr1, 'm1) scope_body_expr ->
  'a
(** Usage:
    [fold_right_scope_lets ~f:(fun scope_let scope_let_var acc -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined (which are before in the program order). *)

val map_exprs_in_scope_lets :
  f:(('expr1, 'm1) marked -> ('expr2, 'm2) marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  ('expr1, 'm1) scope_body_expr ->
  ('expr2, 'm2) scope_body_expr Bindlib.box

val fold_left_scope_defs :
  f:('a -> ('expr1, 'm1) scope_def -> 'expr1 Bindlib.var -> 'a) ->
  init:'a ->
  ('expr1, 'm1) scopes ->
  'a
(** Usage:
    [fold_left_scope_defs ~f:(fun acc scope_def scope_var -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined. *)

val fold_right_scope_defs :
  f:(('expr1, 'm1) scope_def -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:'a ->
  ('expr1, 'm1) scopes ->
  'a
(** Usage:
    [fold_right_scope_defs ~f:(fun  scope_def scope_var acc -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined (which are before in the program order). *)

val map_scope_defs :
  f:(('expr, 'm) scope_def -> ('expr, 'm) scope_def Bindlib.box) ->
  ('expr, 'm) scopes ->
  ('expr, 'm) scopes Bindlib.box

val map_exprs_in_scopes :
  f:(('expr1, 'm1) marked -> ('expr2, 'm2) marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  ('expr1, 'm1) scopes ->
  ('expr2, 'm2) scopes Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)
