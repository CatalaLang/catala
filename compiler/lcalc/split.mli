(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Shared_ast

val split_program :
  threshold:int -> (lcalc, typed) gexpr program -> (lcalc, typed) gexpr program
(** Splits the program ensuring that expressions are not larger to the given
    [threshold]. We approximate an expression size as its number of AST nodes.
    Split expressions will be lifted as topdefs and original expressions are
    replaced by calls to the generated functions. We iterate until every
    program's expression satisfies the size requirements.

    This is necessary in the java backend as there is a bytecode size limit in
    each java's method. However, this mechanism is generic enough to be ported
    to other backends. *)
