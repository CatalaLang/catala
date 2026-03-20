(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria,
   contributors: Alain Delaët <alain.delaet--tixeuil@inria.fr>, Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Optimization passes for default calculus and lambda calculus programs and
    expressions *)

open Definitions

val optimize_expr :
  decl_ctx ->
  (('a, 'b) dcalc_lcalc, 'm) gexpr ->
  (('a, 'b) dcalc_lcalc, 'm) boxed_gexpr

val optimize_program :
  (('a, 'b) dcalc_lcalc, 'm) gexpr program ->
  (('a, 'b) dcalc_lcalc, 'm) gexpr program

(** {1 Tests}*)

val test_iota_reduction_1 : unit -> unit
val test_iota_reduction_2 : unit -> unit
