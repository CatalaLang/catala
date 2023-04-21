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

open Ast

(** Optimization passes for lambda calculus programs and expressions. This
    contains the following optimization:

    * iota_expr: optimizations regarding of the interaction between match and
    constructions injectors. Implemented to simplify the monadic encoding of
    catala

    [match E x with | E y -> e1] = [e1\[y |-> x\]]

    [match e with | A y -> y | B y -> B y] = [e]

    * iota2_expr: optimization regarding nested matches arguments. Implemented
    simplify the monadic encoding of catala

    [match match e with A x1 -> A e1 | B x2 -> B e2 with | A x3 -> e3 | B x4 -> e4]
    = [match e with A x1 -> e3\[x3|-> e1\] B x2 -> e3\[x4|-> e2\]]

    * fold_expr: Optimization regarding folding on already defined arrays of
    size 0 or 1.

    * beta_expr: beta substitution

    * peephole_expr: various optimizations with respect to partial evaluation. *)

val optimize_program : 'm program -> Shared_ast.untyped program
(** Warning/todo: no effort was yet made to ensure correct propagation of type
    annotations in the typed case *)

(**{1 Tests}*)

val test_lcalc_optims2 : unit -> unit
val test_lcalc_optims1 : unit -> unit
