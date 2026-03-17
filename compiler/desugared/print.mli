(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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

type exception_tree =
  | Leaf of Dependency.ExceptionVertex.t
  | Node of exception_tree list * Dependency.ExceptionVertex.t

val exceptions_graph :
  Shared_ast.ScopeName.t ->
  Ast.ScopeDef.t ->
  Dependency.ExceptionsDependencies.t ->
  unit
(** Prints the exception graph of a variable to the terminal *)

val exceptions_graph_json :
  Shared_ast.ScopeName.t ->
  Ast.ScopeDef.t ->
  Dependency.ExceptionsDependencies.t ->
  unit
(** Prints the exception graph of a variable as JSON to stdout *)

val build_exception_tree :
  Dependency.ExceptionsDependencies.t -> exception_tree list
(** Builds the exception tree from the graph *)

val exception_tree_to_json : exception_tree -> Yojson.Safe.t
(** Serialises an exception tree node to a JSON value *)
