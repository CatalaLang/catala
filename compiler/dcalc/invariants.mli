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

type invariant_status = Fail | Pass | Ignore
type invariant_expr = typed expr -> invariant_status

val check_invariant : string * invariant_expr -> typed program -> bool
(** Expression invariants are then lifted toward program invariants using
    catala's utility combinators using the [check invariant] function. *)

val invariant_default_no_arrow : unit -> string * invariant_expr
(** [invariant_default_no_arrow] check no default term has a function type. *)

val invariant_no_partial_evaluation : unit -> string * invariant_expr
(** [invariant_no_partial_evaluation] check there is no partial function. *)

val invariant_no_return_a_function : unit -> string * invariant_expr
(** [invariant_no_return_a_function] check no function return a function. *)

val invariant_app_inversion : unit -> string * invariant_expr
(** [invariant_app_inversion] : if the term is an function application, then
    there is only 5 possibility : it is a let binding, it is an operator
    application, it is an variable application, it is a struct access function
    application (sub-scope call), or it is a operator application with trace. *)

val invariant_match_inversion : unit -> string * invariant_expr
(** [invariant_match_inversion] : if a term is a match, then every branch is an
    function abstraction. *)