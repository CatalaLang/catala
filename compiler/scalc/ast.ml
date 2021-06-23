(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module D = Dcalc.Ast
module L = Lcalc.Ast

module FuncName = Uid.Make (Uid.MarkedString) ()

module LocalVarName = Uid.Make (Uid.MarkedString) ()

type expr =
  | EVar of LocalVarName.t
  | EFunc of FuncName.t
  | EStruct of expr Pos.marked list * D.StructName.t
  | EStructFieldAccess of expr Pos.marked * D.StructFieldName.t * D.StructName.t
  | EInj of expr Pos.marked * D.EnumConstructor.t * D.EnumName.t
  | EArray of expr Pos.marked list
  | ELit of L.lit
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator

type stmt =
  | SInnerFuncDef of func
  | SLocalDecl of LocalVarName.t Pos.marked * D.typ Pos.marked
  | SLocalDef of LocalVarName.t Pos.marked * expr Pos.marked
  | STryExcept of block * L.except * block
  | SRaise of L.except
  | SIfThenElse of expr Pos.marked * block * block
  | SSwitch of expr Pos.marked * D.EnumName.t * block list
      (** Each block corresponds to one case of the enum *)
  | SReturn of expr
  | SAssert of expr

and block = stmt Pos.marked list

and func = FuncName.t * (LocalVarName.t Pos.marked * D.typ Pos.marked) list * block

type program = { decl_ctx : D.decl_ctx; scopes : func list }
