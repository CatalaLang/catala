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
open Ast

(** Formats a lambda calculus program into a valid OCaml program *)

val avoid_keywords : string -> string

val find_struct :
  Dcalc.Ast.StructName.t ->
  Dcalc.Ast.decl_ctx ->
  (Dcalc.Ast.StructFieldName.t * Dcalc.Ast.typ Pos.marked) list

val find_enum :
  Dcalc.Ast.EnumName.t ->
  Dcalc.Ast.decl_ctx ->
  (Dcalc.Ast.EnumConstructor.t * Dcalc.Ast.typ Pos.marked) list

val typ_needs_parens : Dcalc.Ast.typ Pos.marked -> bool
val needs_parens : expr Pos.marked -> bool
val format_enum_name : Format.formatter -> Dcalc.Ast.EnumName.t -> unit

val format_enum_cons_name :
  Format.formatter -> Dcalc.Ast.EnumConstructor.t -> unit

val format_struct_name : Format.formatter -> Dcalc.Ast.StructName.t -> unit

val format_struct_field_name :
  Format.formatter ->
  Dcalc.Ast.StructName.t option * Dcalc.Ast.StructFieldName.t ->
  unit

val format_to_struct_type : Format.formatter -> Dcalc.Ast.StructName.t -> unit
val format_lit : Format.formatter -> lit Pos.marked -> unit
val format_uid_list : Format.formatter -> Uid.MarkedString.info list -> unit
val format_var : Format.formatter -> Var.t -> unit

val format_program :
  Format.formatter ->
  'm Ast.program ->
  Scopelang.Dependency.TVertex.t list ->
  unit
(** Usage [format_program fmt p type_dependencies_ordering] *)
