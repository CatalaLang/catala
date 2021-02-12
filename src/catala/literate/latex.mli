(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** This modules weaves the source code and the legislative text together into a document that law
    professionals can understand. *)

open Utils

(** {1 Helpers} *)

(** Espaces various LaTeX-sensitive characters *)
val pre_latexify : string -> string

(** Usage: [wrap_latex source_files custom_pygments language fmt wrapped]

    Prints an LaTeX complete documùent structure around the [wrapped] content. *)
val wrap_latex :
  string list ->
  string option ->
  Cli.backend_lang -> Format.formatter -> (Format.formatter -> unit) -> unit

(** Replaces math operators by their nice unicode counterparts *)
val math_syms_replace : string -> string

(** {1 Weaving} *)

val law_article_item_to_latex :
  Cli.backend_lang -> Format.formatter -> Surface.Ast.law_article_item -> unit
val law_structure_to_latex :
  Cli.backend_lang -> Format.formatter -> Surface.Ast.law_structure -> unit
val program_item_to_latex :
  Cli.backend_lang -> Format.formatter -> Surface.Ast.program_item -> unit

(** {1 API} *)

val ast_to_latex : Cli.backend_lang -> Format.formatter -> Surface.Ast.program -> unit
