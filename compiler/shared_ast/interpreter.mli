(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët
   <alain.delaet--tixeuil@inria.Fr>, Louis Gesbert <louis.gesbert@inria.fr>

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
open Definitions

exception CatalaException of except

val evaluate_operator :
  ((((_, _, _) interpr_kind as 'a), 'm) gexpr -> ('a, 'm) gexpr) ->
  'a operator ->
  'm mark ->
  Cli.backend_lang ->
  ('a, 'm) gexpr list ->
  ('a, 'm) gexpr
(** Evaluates the result of applying the given operator to the given arguments,
    which are expected to be already reduced to values. The first argument is
    used to evaluate expressions and called when reducing e.g. the [map]
    operator. *)

val evaluate_expr :
  decl_ctx ->
  Cli.backend_lang ->
  (('a, 'b, _) interpr_kind, 'm) gexpr ->
  (('a, 'b, yes) interpr_kind, 'm) gexpr
(** Evaluates an expression according to the semantics of the default calculus. *)

val interpret_program_dcalc :
  (dcalc, 'm) gexpr program ->
  ScopeName.t ->
  (Uid.MarkedString.info * ((yes, no, yes) interpr_kind, 'm) gexpr) list
(** Interprets a program. This function expects an expression typed as a
    function whose argument are all thunked. The function is executed by
    providing for each argument a thunked empty default. Returns a list of all
    the computed values for the scope variables of the executed scope. *)

val interpret_program_lcalc :
  (lcalc, 'm) gexpr program ->
  ScopeName.t ->
  (Uid.MarkedString.info * ((no, yes, yes) interpr_kind, 'm) gexpr) list
(** Interprets a program. This function expects an expression typed as a
    function whose argument are all thunked. The function is executed by
    providing for each argument a thunked empty default. Returns a list of all
    the computed values for the scope variables of the executed scope. *)

val delcustom :
  (('a, 'b, 'c) interpr_kind, 'm) gexpr -> (('a, 'b, no) interpr_kind, 'm) gexpr
(** Runtime check that the term contains no custom terms (raises
    [Invalid_argument] if that is the case *)

val load_runtime_modules : _ program -> unit
(** Dynlink the runtime modules required by the given program, in order to make
    them callable by the interpreter. *)
