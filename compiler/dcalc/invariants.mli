(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Alain Delaët <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This file makes explicit few structural invariants of the dcalc asbtract
    syntax tree. Those invariants have been checked on all tests and examples of
    catala. The behavior of the compiler on programs that don't follow those
    invariant in undefined. *)

open Shared_ast
open Ast

val check_all_invariants : typed program -> bool
(** Check all invariants. Return true if all the invariants are correct. Print
    errors messages for each unsucessfull invariants. Here are all the
    invariants checked:

    - [invariant_default_no_arrow] check no default term has a function type.
    - [invariant_no_partial_evaluation] check there is no partial function.
    - [invariant_no_return_a_function] check no function return a function.
    - [invariant_app_inversion] : if the term is an function application, then
      there is only 6 possibility : it is a let binding, it is an operator
      application, it is an variable application, it is a struct access function
      application (sub-scope call), it is a operator application with trace, or
      an external function.
    - [invariant_match_inversion] : if a term is a match, then every branch is
      an function abstraction.
    - [invariant_typing_default]: the type TDefault can only appear in some
      positions.

    The function prints as a side effect the different errors.*)
