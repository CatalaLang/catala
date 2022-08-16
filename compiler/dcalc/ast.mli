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

type 'm program = 'm expr Shared_ast.program

(** {1 Helpers} *)

(** {2 Variables} *)

type 'm var = 'm expr Var.t
type 'm vars = 'm expr Var.vars

val free_vars_expr : 'm marked_expr -> 'm expr Var.Set.t
val free_vars_scope_body_expr : 'm expr scope_body_expr -> 'm expr Var.Set.t
val free_vars_scope_body : 'm expr scope_body -> 'm expr Var.Set.t
val free_vars_scopes : 'm expr scopes -> 'm expr Var.Set.t
val make_var : ('m var, 'm mark) Marked.t -> 'm expr marked Bindlib.box

type 'e box_expr_sig = 'e marked -> 'e marked Bindlib.box

(** {2 Boxed term constructors} *)

type 'e make_abs_sig =
  'e Bindlib.mvar ->
  'e marked Bindlib.box ->
  typ Marked.pos list ->
  'm mark ->
  'e marked Bindlib.box
  constraint 'e = ('a, 'm mark) gexpr

val make_abs : 'm expr make_abs_sig

val make_app :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

type 'e make_let_in_sig =
  'e Bindlib.var ->
  marked_typ ->
  'e marked Bindlib.box ->
  'e marked Bindlib.box ->
  Pos.t ->
  'e marked Bindlib.box
  constraint 'e = (_, _) gexpr

val make_let_in : 'm expr make_let_in_sig

(**{2 Other}*)

val empty_thunked_term : 'm mark -> 'm marked_expr
val is_value : 'm marked_expr -> bool

val equal_exprs : 'm marked_expr -> 'm marked_expr -> bool
(** Determines if two expressions are equal, omitting their position information *)

(** {1 AST manipulation helpers}*)

val build_whole_scope_expr :
  box_expr:'e box_expr_sig ->
  make_abs:'e make_abs_sig ->
  make_let_in:'e make_let_in_sig ->
  decl_ctx ->
  (('a, 'm mark) gexpr as 'e) scope_body ->
  'm mark ->
  'e marked Bindlib.box
(** Usage: [build_whole_scope_expr ctx body scope_position] where
    [scope_position] corresponds to the line of the scope declaration for
    instance. *)

type 'expr scope_name_or_var =
  | ScopeName of ScopeName.t
  | ScopeVar of 'expr Bindlib.var

val unfold_scopes :
  box_expr:'e box_expr_sig ->
  make_abs:'e make_abs_sig ->
  make_let_in:'e make_let_in_sig ->
  decl_ctx ->
  (('a, 'm mark) gexpr as 'e) scopes ->
  'm mark ->
  'e scope_name_or_var ->
  'e marked Bindlib.box

val build_whole_program_expr :
  box_expr:'e box_expr_sig ->
  make_abs:'e make_abs_sig ->
  make_let_in:'e make_let_in_sig ->
  'e Shared_ast.program ->
  ScopeName.t ->
  (('a, 'm mark) gexpr as 'e) marked Bindlib.box
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
