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

(** Functions handling the expressions of [shared_ast] *)

open Utils
open Types

(** {2 Boxed constructors} *)

val box : ('a, 't) marked_gexpr -> ('a, 't) marked_gexpr Bindlib.box
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
  ('a any, 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val elit : 'a any glit -> 't -> ('a, 't) marked_gexpr Bindlib.box

val eabs :
  (('a any, 't) gexpr, ('a, 't) marked_gexpr) Bindlib.mbinder Bindlib.box ->
  marked_typ list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eapp :
  ('a any, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box list ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eassert :
  (([< dcalc | lcalc ] as 'a), 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eop : operator -> 't -> (_ any, 't) marked_gexpr Bindlib.box

val edefault :
  (([< desugared | scopelang | dcalc ] as 'a), 't) marked_gexpr Bindlib.box list ->
  ('a, 't) marked_gexpr Bindlib.box ->
  ('a, 't) marked_gexpr Bindlib.box ->
  't ->
  ('a, 't) marked_gexpr Bindlib.box

val eifthenelse :
  ('a any, 't) marked_gexpr Bindlib.box ->
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

val untype :
  ('a, 'm mark) marked_gexpr -> ('a, untyped mark) marked_gexpr Bindlib.box

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

(** {2 Expression building helpers} *)

val make_var : 'a Bindlib.var * 'b -> ('a * 'b) Bindlib.box

val make_abs :
  'e Var.vars ->
  (('a, 'm mark) gexpr as 'e) marked Bindlib.box ->
  typ Marked.pos list ->
  'm mark ->
  'e marked Bindlib.box

val make_app :
  ((_ any, 'm mark) gexpr as 'e) marked Bindlib.box ->
  'e marked Bindlib.box list ->
  'm mark ->
  'e marked Bindlib.box

val empty_thunked_term :
  'm mark -> ([< dcalc | desugared | scopelang ], 'm mark) gexpr marked

val make_let_in :
  'e Bindlib.var ->
  typ Utils.Marked.pos ->
  ((_ any, 'm mark) gexpr as 'e) marked Bindlib.box ->
  'e marked Bindlib.box ->
  Utils.Pos.t ->
  'e marked Bindlib.box

(** {2 Transformations} *)

val remove_logging_calls :
  ((_ any, 't) gexpr as 'e) marked -> 'e marked Bindlib.box
(** Removes all calls to [Log] unary operators in the AST, replacing them by
    their argument. *)

val format :
  ?debug:bool (** [true] for debug printing *) ->
  decl_ctx ->
  Format.formatter ->
  'e marked ->
  unit

(** {2 Analysis and tests} *)

val is_value : (_ any, 'm mark) gexpr marked -> bool

val equal : ('a, 'm mark) gexpr marked -> ('a, 'm mark) gexpr marked -> bool
(** Determines if two expressions are equal, omitting their position information *)

val free_vars : 'e marked -> 'e Var.Set.t

val size : (_ any, 't) gexpr marked -> int
(** Used by the optimizer to know when to stop *)
