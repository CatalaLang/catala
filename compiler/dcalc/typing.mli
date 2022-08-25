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

open Shared_ast

val infer_types :
  decl_ctx -> untyped Ast.expr -> typed Ast.expr Bindlib.box
(** Infers types everywhere on the given expression, and adds (or replaces) type
    annotations on each node *)

val infer_type : decl_ctx -> 'm Ast.expr -> typ
(** Gets the outer type of the given expression, using either the existing
    annotations or inference *)

val check_type : decl_ctx -> 'm Ast.expr -> typ -> unit
val infer_types_program : untyped Ast.program -> typed Ast.program
