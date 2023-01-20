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

(** Reference interpreter for the default calculus *)

open Catala_utils
open Shared_ast

val evaluate_expr : decl_ctx -> 'm Ast.expr -> 'm Ast.expr
(** Evaluates an expression according to the semantics of the default calculus. *)

val interpret_program :
  decl_ctx -> 'm Ast.expr -> (Uid.MarkedString.info * 'm Ast.expr) list
(** Interprets a program. This function expects an expression typed as a
    function whose argument are all thunked. The function is executed by
    providing for each argument a thunked empty default. Returns a list of all
    the computed values for the scope variables of the executed scope. *)
