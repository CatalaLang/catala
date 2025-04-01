(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
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

(** Formats a lambda calculus program into a valid Java program *)

open Shared_ast

val renaming : Renaming.t

val generate_program :
  output_dir:string ->
  input_file:Catala_utils.File.t ->
  Ast.program ->
  TypeIdent.t list ->
  unit
(** Usage
    [generate_program ~output_dir ~input_file p type_dependencies_ordering]

    [output_dir] specifies where to generate the directory containing the Java
    code. *)
