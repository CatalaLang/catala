(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>, Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Interfacing with the Z3 SMT solver *)

module StringMap : Map.S with type key = string

type context = {
  ctx_z3 : Z3.context;
  ctx_decl : Dcalc.Ast.decl_ctx;
  ctx_var : Dcalc.Ast.typ Utils.Pos.marked Dcalc.Ast.VarMap.t;
  ctx_funcdecl : Z3.FuncDecl.func_decl Dcalc.Ast.VarMap.t;
  ctx_z3vars : Dcalc.Ast.Var.t StringMap.t;
  ctx_z3datatypes : Z3.Sort.sort Dcalc.Ast.EnumMap.t;
  ctx_z3matchsubsts : Z3.Expr.expr Dcalc.Ast.VarMap.t;
  ctx_z3structs : Z3.Sort.sort Dcalc.Ast.StructMap.t;
}

val translate_expr : context -> Dcalc.Ast.expr Utils.Pos.marked -> context * Z3.Expr.expr

module Backend : sig
  type backend_context = context

  type vc_encoding = Z3.Expr.expr

  val print_encoding : vc_encoding -> string

  type model = Z3.Model.model

  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  val solve_vc_encoding : backend_context -> vc_encoding -> solver_result

  val print_model : backend_context -> model -> string

  val is_model_empty : model -> bool
end

module Io : sig
  type vc_encoding_result = Io.SolverIO(Backend).vc_encoding_result =
    | Success of Backend.vc_encoding * Backend.backend_context
    | Fail of string

  val print_positive_result : Conditions.verification_condition -> string

  val print_negative_result :
    Conditions.verification_condition -> Backend.backend_context -> Backend.model option -> string

  val encode_and_check_vc :
    Dcalc.Ast.decl_ctx -> Conditions.verification_condition * vc_encoding_result -> unit
end
