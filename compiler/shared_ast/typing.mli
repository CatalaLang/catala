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

open Definitions

type 'e env

val expr :
  decl_ctx ->
  ?env:'e env ->
  ?typ:typ ->
  (('a, 'm mark) gexpr as 'e) ->
  ('a, typed mark) gexpr box
(** Infers and marks the types for the given expression. If [typ] is provided,
    it is assumed to be the outer type and used for inference top-down. *)

val program : ('a, untyped mark) gexpr program -> ('a, typed mark) gexpr program
(** Typing on whole programs *)
