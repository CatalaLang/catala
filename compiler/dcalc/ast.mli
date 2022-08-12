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
open Shared_ast

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

type ('expr, 'm) box_expr_sig =
  ('expr, 'm) marked -> ('expr, 'm) marked Bindlib.box

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
