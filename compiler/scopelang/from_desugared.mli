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

(** Translation from {!module: Desugared.Ast} to {!module: Scopelang.Ast} *)

val build_exceptions_graph :
  Desugared.Ast.program ->
  Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t
(** This function builds all the exceptions dependency graphs for all variables
    of all scopes. *)

val translate_program :
  Desugared.Ast.program ->
  Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t ->
  Shared_ast.untyped Ast.program
(** This functions returns the translated program as well as all the graphs of
    exceptions inferred for each scope variable of the program. *)
