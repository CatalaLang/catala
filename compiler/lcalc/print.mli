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

open Utils

(** {1 Helpers} *)

val is_uppercase : CamomileLibraryDefault.Camomile.UChar.t -> bool

val begins_with_uppercase : string -> bool

(** {1 Formatters} *)

val format_lit : Format.formatter -> Ast.lit Pos.marked -> unit

val format_var : Format.formatter -> Ast.Var.t -> unit

val format_expr :
  Dcalc.Ast.decl_ctx -> ?debug:bool -> Format.formatter -> Ast.expr Pos.marked -> unit

val format_scope : Dcalc.Ast.decl_ctx -> ?debug:bool -> Format.formatter -> Ast.scope_body -> unit
