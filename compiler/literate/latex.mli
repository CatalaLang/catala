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

(** This modules weaves the source code and the legislative text together into a
    document that law professionals can understand. *)

open Catala_utils

(** {1 Helpers} *)

val wrap_latex :
  string list ->
  Cli.backend_lang ->
  Format.formatter ->
  (Format.formatter -> unit) ->
  unit
(** Usage: [wrap_latex source_files language fmt wrapped]

    Prints an LaTeX complete documùent structure around the [wrapped] content. *)

(** {1 API} *)

val ast_to_latex :
  Cli.backend_lang ->
  print_only_law:bool ->
  Format.formatter ->
  Surface.Ast.program ->
  unit
