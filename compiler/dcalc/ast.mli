(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree of the default calculus intermediate representation *)

open Utils
include module type of Shared_ast
include module type of Shared_ast.Expr

type lit = dcalc glit

type 'm expr = (dcalc, 'm mark) gexpr
and 'm marked_expr = (dcalc, 'm mark) marked_gexpr

type 'm program = ('m expr, 'm) program_generic

(** {1 Helpers} *)

(** {2 Variables} *)

type 'm var = 'm expr Var.t
type 'm vars = 'm expr Var.vars

val free_vars_expr : 'm marked_expr -> 'm expr Var.Set.t

val free_vars_scope_body_expr :
  ('m expr, 'm) scope_body_expr -> 'm expr Var.Set.t

val free_vars_scope_body : ('m expr, 'm) scope_body -> 'm expr Var.Set.t
val free_vars_scopes : ('m expr, 'm) scopes -> 'm expr Var.Set.t
val make_var : ('m var, 'm) marked -> 'm marked_expr Bindlib.box

(** {2 Manipulation of marks} *)

val no_mark : 'm mark -> 'm mark
val mark_pos : 'm mark -> Pos.t
val pos : ('a, 'm) marked -> Pos.t
val ty : ('a, typed) marked -> marked_typ
val with_ty : marked_typ -> ('a, 'm) marked -> ('a, typed) marked

(** All the following functions will resolve the types if called on an
    [Inferring] type *)

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

val get_scope_body_mark : ('expr, 'm) scope_body -> 'm mark
val untype_expr : 'm marked_expr -> untyped marked_expr Bindlib.box
val untype_program : 'm program -> untyped program

(** {2 Boxed constructors} *)

val evar : 'm expr Bindlib.var -> 'm mark -> 'm marked_expr Bindlib.box

val etuple :
  'm marked_expr Bindlib.box list ->
  StructName.t option ->
  'm mark ->
  'm marked_expr Bindlib.box

val etupleaccess :
  'm marked_expr Bindlib.box ->
  int ->
  StructName.t option ->
  marked_typ list ->
  'm mark ->
  'm marked_expr Bindlib.box

val einj :
  'm marked_expr Bindlib.box ->
  int ->
  EnumName.t ->
  marked_typ list ->
  'm mark ->
  'm marked_expr Bindlib.box

val ematch :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  EnumName.t ->
  'm mark ->
  'm marked_expr Bindlib.box

val earray :
  'm marked_expr Bindlib.box list -> 'm mark -> 'm marked_expr Bindlib.box

val elit : lit -> 'm mark -> 'm marked_expr Bindlib.box

val eabs :
  ('m expr, 'm marked_expr) Bindlib.mbinder Bindlib.box ->
  marked_typ list ->
  'm mark ->
  'm marked_expr Bindlib.box

val eapp :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

val eassert :
  'm marked_expr Bindlib.box -> 'm mark -> 'm marked_expr Bindlib.box

val eop : operator -> 'm mark -> 'm marked_expr Bindlib.box

val edefault :
  'm marked_expr Bindlib.box list ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm mark ->
  'm marked_expr Bindlib.box

val eifthenelse :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm mark ->
  'm marked_expr Bindlib.box

val eerroronempty :
  'm marked_expr Bindlib.box -> 'm mark -> 'm marked_expr Bindlib.box

type ('expr, 'm) box_expr_sig =
  ('expr, 'm) marked -> ('expr, 'm) marked Bindlib.box

val box_expr : ('m expr, 'm) box_expr_sig

(**{2 Program traversal}*)

(** Be careful when using these traversal functions, as the bound variables they
    open will be different at each traversal. *)

val map_expr :
  'a ->
  f:('a -> 'm1 marked_expr -> 'm2 marked_expr Bindlib.box) ->
  ('m1 expr, 'm2 mark) Marked.t ->
  'm2 marked_expr Bindlib.box
(** If you want to apply a map transform to an expression, you can save up
    writing a painful match over all the cases of the AST. For instance, if you
    want to remove all errors on empty, you can write

    {[
      let remove_error_empty =
        let rec f () e =
          match Marked.unmark e with
          | ErrorOnEmpty e1 -> map_expr () f e1
          | _ -> map_expr () f e
        in
        f () e
    ]}

    The first argument of map_expr is an optional context that you can carry
    around during your map traversal. *)

val map_expr_top_down :
  f:('m1 marked_expr -> ('m1 expr, 'm2 mark) Marked.t) ->
  'm1 marked_expr ->
  'm2 marked_expr Bindlib.box
(** Recursively applies [f] to the nodes of the expression tree. The type
    returned by [f] is hybrid since the mark at top-level has been rewritten,
    but not yet the marks in the subtrees. *)

val map_expr_marks :
  f:('m1 mark -> 'm2 mark) -> 'm1 marked_expr -> 'm2 marked_expr Bindlib.box

(** {2 Boxed term constructors} *)

type ('e, 'm) make_abs_sig =
  'e Bindlib.mvar ->
  ('e, 'm) marked Bindlib.box ->
  marked_typ list ->
  'm mark ->
  ('e, 'm) marked Bindlib.box

val make_abs : ('m expr, 'm) make_abs_sig

val make_app :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

type ('expr, 'm) make_let_in_sig =
  'expr Bindlib.var ->
  marked_typ ->
  ('expr, 'm) marked Bindlib.box ->
  ('expr, 'm) marked Bindlib.box ->
  Pos.t ->
  ('expr, 'm) marked Bindlib.box

val make_let_in : ('m expr, 'm) make_let_in_sig

(**{2 Other}*)

val empty_thunked_term : 'm mark -> 'm marked_expr
val is_value : 'm marked_expr -> bool

val equal_exprs : 'm marked_expr -> 'm marked_expr -> bool
(** Determines if two expressions are equal, omitting their position information *)

(** {1 AST manipulation helpers}*)

val build_whole_scope_expr :
  box_expr:('expr, 'm) box_expr_sig ->
  make_abs:('expr, 'm) make_abs_sig ->
  make_let_in:('expr, 'm) make_let_in_sig ->
  decl_ctx ->
  ('expr, 'm) scope_body ->
  'm mark ->
  ('expr, 'm) marked Bindlib.box
(** Usage: [build_whole_scope_expr ctx body scope_position] where
    [scope_position] corresponds to the line of the scope declaration for
    instance. *)

type 'expr scope_name_or_var =
  | ScopeName of ScopeName.t
  | ScopeVar of 'expr Bindlib.var

val unfold_scopes :
  box_expr:('expr, 'm) box_expr_sig ->
  make_abs:('expr, 'm) make_abs_sig ->
  make_let_in:('expr, 'm) make_let_in_sig ->
  decl_ctx ->
  ('expr, 'm) scopes ->
  'm mark ->
  'expr scope_name_or_var ->
  ('expr, 'm) marked Bindlib.box

val build_whole_program_expr :
  box_expr:('expr, 'm) box_expr_sig ->
  make_abs:('expr, 'm) make_abs_sig ->
  make_let_in:('expr, 'm) make_let_in_sig ->
  ('expr, 'm) program_generic ->
  ScopeName.t ->
  ('expr, 'm) marked Bindlib.box
(** Usage: [build_whole_program_expr program main_scope] builds an expression
    corresponding to the main program and returning the main scope as a
    function. *)

val expr_size : 'm marked_expr -> int
(** Used by the optimizer to know when to stop *)

val remove_logging_calls : 'm marked_expr -> 'm marked_expr Bindlib.box
(** Removes all calls to [Log] unary operators in the AST, replacing them by
    their argument. *)

val build_scope_typ_from_sig :
  decl_ctx -> StructName.t -> StructName.t -> Pos.t -> typ Marked.pos
(** [build_scope_typ_from_sig ctx in_struct out_struct pos] builds the arrow
    type for the specified scope *)
