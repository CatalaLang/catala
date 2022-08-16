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

val evar : ('a, 't) gexpr Bindlib.var -> 't -> ('a, 't) marked_gexpr Bindlib.box

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
  (([< any ] as 'a), 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val elit : ([< any ] as 'a) glit -> 't -> ('a, 't) marked_gexpr Bindlib.box

val eabs :
  ((([< any ] as 'a), 't) gexpr, ('a, 't) marked_gexpr) Bindlib.mbinder
  Bindlib.box ->
  marked_typ list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eapp :
  (([< any ] as 'a), 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eassert :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eop : operator -> 't -> ([< any ], 't) marked_gexpr Bindlib.box

val edefault :
  (([< desugared | scopelang | dcalc ] as 'a), 't) marked_gexpr Bindlib.box list ->
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eifthenelse :
  (([< any ] as 'a), 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eerroronempty :
  (([< desugared | scopelang | dcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val ecatch :
  (lcalc, 't) marked_gexpr Bindlib.box ->
  except ->
  (lcalc, 't) marked_gexpr Bindlib.box ->
  't ->
  (lcalc, 't) marked_gexpr Bindlib.box

val eraise : except -> 't -> (lcalc, 't) marked_gexpr Bindlib.box

(** Manipulation of marks *)

val no_mark : 'm mark -> 'm mark
val mark_pos : 'm mark -> Pos.t
val pos : ('e, _) gexpr marked -> Pos.t
val ty : (_, typed mark) Marked.t -> marked_typ
val with_ty : marked_typ -> ('a, _ mark) Marked.t -> ('a, typed mark) Marked.t

val map_mark :
  (Pos.t -> Pos.t) -> (marked_typ -> marked_typ) -> 'm mark -> 'm mark

val map_mark2 :
  (Pos.t -> Pos.t -> Pos.t) ->
  (typed -> typed -> marked_typ) ->
  'm mark ->
  'm mark ->
  'm mark

val fold_marks :
  (Pos.t list -> Pos.t) -> (typed list -> marked_typ) -> 'm mark list -> 'm mark

val get_scope_body_mark : (_, 'm mark) gexpr scope_body -> 'm mark

val untype :
  ('a, 'm mark) marked_gexpr -> ('a, untyped mark) marked_gexpr Bindlib.box

val untype_program :
  (([< any ] as 'a), 'm mark) gexpr program -> ('a, untyped mark) gexpr program

(** {2 Handling of boxing} *)

val box : ('a, 't) marked_gexpr -> ('a, 't) marked_gexpr Bindlib.box

(** {2 Traversal functions} *)

val map :
  'ctx ->
  f:('ctx -> ('a, 't1) marked_gexpr -> ('a, 't2) marked_gexpr Bindlib.box) ->
  (('a, 't1) gexpr, 't2) Marked.t ->
  ('a, 't2) marked_gexpr Bindlib.box
(** Flat (non-recursive) mapping on expressions.

    If you want to apply a map transform to an expression, you can save up
    writing a painful match over all the cases of the AST. For instance, if you
    want to remove all errors on empty, you can write

    {[
      let remove_error_empty =
        let rec f () e =
          match Marked.unmark e with
          | ErrorOnEmpty e1 -> Expr.map () f e1
          | _ -> Expr.map () f e
        in
        f () e
    ]}

    The first argument of map_expr is an optional context that you can carry
    around during your map traversal. *)

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
  f:('a -> 'e scope_let -> 'e Bindlib.var -> 'a) ->
  init:'a ->
  'e scope_body_expr ->
  'a
(** Usage:
    [fold_left_scope_lets ~f:(fun acc scope_let scope_let_var -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined. *)

val fold_right_scope_lets :
  f:('expr1 scope_let -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:('expr1 marked -> 'a) ->
  'expr1 scope_body_expr ->
  'a
(** Usage:
    [fold_right_scope_lets ~f:(fun scope_let scope_let_var acc -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined (which are before in the program order). *)

val map_exprs_in_scope_lets :
  f:('expr1 marked -> 'expr2 marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  'expr1 scope_body_expr ->
  'expr2 scope_body_expr Bindlib.box

val fold_left_scope_defs :
  f:('a -> 'expr1 scope_def -> 'expr1 Bindlib.var -> 'a) ->
  init:'a ->
  'expr1 scopes ->
  'a
(** Usage:
    [fold_left_scope_defs ~f:(fun acc scope_def scope_var -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined. *)

val fold_right_scope_defs :
  f:('expr1 scope_def -> 'expr1 Bindlib.var -> 'a -> 'a) ->
  init:'a ->
  'expr1 scopes ->
  'a
(** Usage:
    [fold_right_scope_defs ~f:(fun  scope_def scope_var acc -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined (which are before in the program order). *)

val map_scope_defs :
  f:('e scope_def -> 'e scope_def Bindlib.box) ->
  'e scopes ->
  'e scopes Bindlib.box

val map_exprs_in_scopes :
  f:('expr1 marked -> 'expr2 marked Bindlib.box) ->
  varf:('expr1 Bindlib.var -> 'expr2 Bindlib.var) ->
  'expr1 scopes ->
  'expr2 scopes Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)
