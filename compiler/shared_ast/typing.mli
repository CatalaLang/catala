(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Typing for the default calculus. Because of the error terms, we perform type
    inference using the classical W algorithm with union-find unification. *)

open Definitions

module Env : sig
  type 'e t

  val empty : ?fail_on_any:bool -> ?assume_op_types:bool -> decl_ctx -> 'e t
  (** The [~fail_on_any] labeled parameter controls the behavior of the typer in
      the case where polymorphic expressions are still found after typing: if
      [false], it allows them (giving them [TAny] and losing typing
      information); if set to [true] (the default), it aborts.

      The [~assume_op_types] flag (default false) ignores the expected built-in
      types of polymorphic operators, and will assume correct the type
      information included in [EAppOp] nodes. This is useful after
      monomorphisation, which changes the expected types for these operators. *)

  val add_var : 'e Var.t -> typ -> 'e t -> 'e t
  val add_toplevel_var : TopdefName.t -> typ -> 'e t -> 'e t
  val add_scope_var : ScopeVar.t -> typ -> 'e t -> 'e t

  val add_scope :
    ScopeName.t ->
    vars:typ ScopeVar.Map.t ->
    in_vars:typ ScopeVar.Map.t ->
    'e t ->
    'e t

  val open_scope : ScopeName.t -> 'e t -> 'e t

  val dump : Format.formatter -> 'e t -> unit
  (** For debug purposes *)
end

val expr :
  decl_ctx ->
  ?env:'e Env.t ->
  ?typ:typ ->
  (('a, 'm) gexpr as 'e) ->
  ('a, typed) boxed_gexpr
(** Infers and marks the types for the given expression. If [typ] is provided,
    it is assumed to be the outer type and used for inference top-down.

    If the input expression already has type annotations, the full inference is
    still done, but with unification with the existing annotations at every
    step. This can be used for double-checking after AST transformations and
    filling the gaps ([TAny]) if any. Use [Expr.untype] first if this is not
    what you want.

    Note that typing also transparently performs the following changes to the
    AST nodes, outside of typing annotations:
    - disambiguation of constructors: [EDStructAccess] nodes are translated into
      [EStructAccess] with the suitable structure and field idents (this only
      concerns [desugared] expressions).
    - disambiguation of structure names in [EDStructAmend] nodes ([desugared] as
      well)
    - resolution of tuple size (when equal to 0) on [ETupleAccess] nodes
    - resolution of operator types, which are stored (monomorphised) back in the
      [EAppOp] nodes
    - resolution of function application input types on the [EApp] nodes, when
      that was originally empty ([[]]): this documents the arity of the function
      application, taking de-tuplification into account.
    - [TAny] appearing within nodes are refined to more precise types, e.g. on
      `EAbs` nodes (but be careful with this, it may only work for specific
      structures of generated code ; having [~fail_on_any:true] set in the
      environment (this is the default) checks that it didn't cause problems) *)

val check_expr :
  decl_ctx ->
  ?env:'e Env.t ->
  ?typ:typ ->
  (('a, 'm) gexpr as 'e) ->
  ('a, untyped) boxed_gexpr
(** Same as [expr], but doesn't annotate the returned expression. Equivalent to
    [Typing.expr |> Expr.untype], but more efficient. This can be useful for
    type-checking and disambiguation (some AST nodes are updated with missing
    information, e.g. any [TAny] appearing in the AST is replaced) *)

val program :
  ?fail_on_any:bool ->
  ?assume_op_types:bool ->
  ?internal_check:bool ->
  ('a, 'm) gexpr program ->
  ('a, typed) gexpr program
(** Typing on whole programs (as defined in Shared_ast.program, i.e. for the
    later dcalc/lcalc stages).

    Any existing type annotations are checked for unification. Use
    [Program.untype] to remove them beforehand if this is not the desired
    behaviour.

    If [internal_check] is set to [true], typing errors will be marked as internal, and the faulty program will be printed if '--debug' is set. *)
