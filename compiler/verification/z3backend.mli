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

module Backend :
  Io.Backend
    with type vc_encoding = Z3.Expr.expr
     and type backend_context = context
     and type model = Z3.Model.model

module Io :
  Io.SolverIo
    with type vc_encoding = Z3.Expr.expr
     and type backend_context = context
     and type model = Z3.Model.model
