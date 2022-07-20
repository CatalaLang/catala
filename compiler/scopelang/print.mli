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

open Utils

val format_var : Format.formatter -> Ast.Var.t -> unit
val format_location : Format.formatter -> Ast.location -> unit
val format_typ : Format.formatter -> Ast.typ Marked.pos -> unit

val format_expr :
  ?debug:bool (** [true] for debug printing *) ->
  Format.formatter ->
  Ast.expr Marked.pos ->
  unit

val format_scope :
  ?debug:bool (** [true] for debug printing *) ->
  Format.formatter ->
  Ast.ScopeName.t * Ast.scope_decl ->
  unit

val format_program :
  ?debug:bool (** [true] for debug printing *) ->
  Format.formatter ->
  Ast.program ->
  unit
