(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This transformation expands the equality operator, that is polymorphic and
    needs code generation on the backends that don't natively support it ; note
    that this is a place-holder, generating inline expansions, and is planned to
    be replaced with a more serious implementation that generates specific
    functions. In particular, currently, comparison of enums is quadratic in
    size. *)

open Shared_ast

val expr : decl_ctx -> 'm Ast.expr -> 'm Ast.expr boxed
val program : 'm Ast.program -> 'm Ast.program
