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

val format_lit : Format.formatter -> Ast.lit Pos.marked -> unit

val format_op_kind : Format.formatter -> Dcalc.Ast.op_kind -> unit

val format_log_entry : Format.formatter -> Dcalc.Ast.log_entry -> unit

val format_binop : Format.formatter -> Dcalc.Ast.binop Pos.marked -> unit

val format_ternop : Format.formatter -> Dcalc.Ast.ternop Pos.marked -> unit

val format_unop : Format.formatter -> Dcalc.Ast.unop Pos.marked -> unit

val to_ascii : string -> string

val to_lowercase : string -> string

val format_struct_name : Format.formatter -> Dcalc.Ast.StructName.t -> unit

val format_struct_field_name : Format.formatter -> Dcalc.Ast.StructFieldName.t -> unit

val format_enum_name : Format.formatter -> Dcalc.Ast.EnumName.t -> unit

val format_enum_cons_name : Format.formatter -> Dcalc.Ast.EnumConstructor.t -> unit

val typ_needs_parens : Dcalc.Ast.typ Pos.marked -> bool

val format_typ : Format.formatter -> Dcalc.Ast.typ Pos.marked -> unit

val format_var : Format.formatter -> Ast.Var.t -> unit

val needs_parens : Ast.expr Pos.marked -> bool

val format_exception : Format.formatter -> Ast.except -> unit

val format_expr : Dcalc.Ast.decl_ctx -> Format.formatter -> Ast.expr Pos.marked -> unit

val format_ctx :
  Scopelang.Dependency.TVertex.t list -> Format.formatter -> Dcalc.Ast.decl_ctx -> unit

val format_program : Format.formatter -> Ast.program -> Scopelang.Dependency.TVertex.t list -> unit
