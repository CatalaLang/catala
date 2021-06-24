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

module TopLevelName = Uid.Make (Uid.MarkedString) ()

module LocalName = Uid.Make (Uid.MarkedString) ()

type expr =
  | EVar of LocalName.t
  | EFunc of TopLevelName.t
  | EStruct of expr Pos.marked list * D.StructName.t
  | EStructFieldAccess of expr Pos.marked * D.StructFieldName.t * D.StructName.t
  | EInj of expr Pos.marked * D.EnumConstructor.t * D.EnumName.t
  | EArray of expr Pos.marked list
  | ELit of L.lit
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator

type stmt =
  | SInnerFuncDef of LocalName.t Pos.marked * func
  | SLocalDecl of LocalName.t Pos.marked * D.typ Pos.marked
  | SLocalDef of LocalName.t Pos.marked * expr Pos.marked
  | STryExcept of block * L.except * block
  | SRaise of L.except
  | SIfThenElse of expr Pos.marked * block * block
  | SSwitch of
      expr Pos.marked
      * D.EnumName.t
      * (block (* Statements corresponding to arm closure body*)
        * (* Variable instantiated with enum payload *) LocalName.t)
        list  (** Each block corresponds to one case of the enum *)
  | SReturn of expr
  | SAssert of expr

and block = stmt Pos.marked list

and func = { func_params : (LocalName.t Pos.marked * D.typ Pos.marked) list; func_body : block }

type program = { decl_ctx : D.decl_ctx; scopes : (TopLevelName.t * func) list }
