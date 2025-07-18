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

(** Formats a statement calculus program into a valid C89 program *)

open Catala_utils
open Shared_ast

val renaming : Renaming.t

val format_program :
  File.t option -> Format.formatter -> Ast.program -> TypeIdent.t list -> unit
(** Usage [format_program fmt p type_dependencies_ordering] *)
