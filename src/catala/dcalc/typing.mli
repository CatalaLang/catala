(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Typing for the default calculus. Because of the error terms, we perform type inference using the
    classical W algorithm with union-find unification. *)

open Utils

(** {1 Types and unification} *)

module Any : Uid.Id with type info = unit

(** We do not reuse {!type: Dcalc.Ast.typ} because we have to include a new [TAny] variant. Indeed,
    error terms can have any type and this has to be captured by the type sytem. *)
type typ =
  | TLit of Ast.typ_lit
  | TArrow of typ Pos.marked UnionFind.elem * typ Pos.marked UnionFind.elem
  | TTuple of typ Pos.marked UnionFind.elem list * Ast.StructName.t option
  | TEnum of typ Pos.marked UnionFind.elem list * Ast.EnumName.t
  | TArray of typ Pos.marked UnionFind.elem
  | TAny of Any.t

val typ_needs_parens : typ Utils.Pos.marked UnionFind.elem -> bool

val format_typ : Ast.decl_ctx -> Format.formatter -> typ Utils.Pos.marked UnionFind.elem -> unit

val unify :
  Ast.decl_ctx -> typ Utils.Pos.marked UnionFind.elem -> typ Utils.Pos.marked UnionFind.elem -> unit
(** Raises an error if unification cannot be performed *)

val op_type : Ast.operator Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem
(** Operators have a single type, instead of being polymorphic with constraints. This allows us to
    have a simpler type system, while we argue the syntactic burden of operator annotations helps
    the programmer visualize the type flow in the code. *)

val ast_to_typ : Ast.typ -> typ

val typ_to_ast : typ Utils.Pos.marked UnionFind.elem -> Ast.typ Utils.Pos.marked

(** {1 Double-directed typing} *)

type env = typ Utils.Pos.marked UnionFind.elem Ast.VarMap.t

val typecheck_expr_bottom_up :
  Ast.decl_ctx -> env -> Ast.expr Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem
(** Infers the most permissive type from an expression *)

val typecheck_expr_top_down :
  Ast.decl_ctx -> env -> Ast.expr Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem -> unit
(** Checks whether the expression can be typed with the provided type *)

val infer_type : Ast.decl_ctx -> Ast.expr Utils.Pos.marked -> Ast.typ Utils.Pos.marked

val check_type : Ast.decl_ctx -> Ast.expr Utils.Pos.marked -> Ast.typ Utils.Pos.marked -> unit
