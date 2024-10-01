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

(** This module scans a program for "test" scopes, detected as scopes without
    any undefined inputs. It runs the interpreter to compute their results, then
    inserts assertion in the code that ensure correctness of said results. *)

open Catala_utils
open Shared_ast

val scope :
  decl_ctx ->
  Global.backend_lang ->
  ((dcalc, 'm) gexpr boxed -> (dcalc, 'm) gexpr boxed) ->
  ScopeName.t ->
  (dcalc, 'm) gexpr scope_body ->
  (dcalc, 'm) gexpr scope_body Bindlib.box
(** If the given scope has any inputs, does nothing but reboxing. Otherwise,
    this function runs the interpreter on the scope to determine its outputs,
    then inserts assertions within the scope.

    NOTE: scopes with context variables are *not* treated at the moment ;
    functional values are ignored in the equality assertions *)

val program : 'm Ast.program -> 'm Ast.program
