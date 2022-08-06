(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
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
module D = Dcalc.Ast
module L = Lcalc.Ast
module TopLevelName = Uid.Make (Uid.MarkedString) ()
module LocalName = Uid.Make (Uid.MarkedString) ()

let dead_value = LocalName.fresh ("dead_value", Pos.no_pos)
let handle_default = TopLevelName.fresh ("handle_default", Pos.no_pos)
let handle_default_opt = TopLevelName.fresh ("handle_default_opt", Pos.no_pos)

type expr =
  | EVar of LocalName.t
  | EFunc of TopLevelName.t
  | EStruct of expr Marked.pos list * D.StructName.t
  | EStructFieldAccess of expr Marked.pos * D.StructFieldName.t * D.StructName.t
  | EInj of expr Marked.pos * D.EnumConstructor.t * D.EnumName.t
  | EArray of expr Marked.pos list
  | ELit of L.lit
  | EApp of expr Marked.pos * expr Marked.pos list
  | EOp of Dcalc.Ast.operator

type stmt =
  | SInnerFuncDef of LocalName.t Marked.pos * func
  | SLocalDecl of LocalName.t Marked.pos * D.typ Marked.pos
  | SLocalDef of LocalName.t Marked.pos * expr Marked.pos
  | STryExcept of block * L.except * block
  | SRaise of L.except
  | SIfThenElse of expr Marked.pos * block * block
  | SSwitch of
      expr Marked.pos
      * D.EnumName.t
      * (block (* Statements corresponding to arm closure body*)
        * (* Variable instantiated with enum payload *) LocalName.t)
        list  (** Each block corresponds to one case of the enum *)
  | SReturn of expr
  | SAssert of expr

and block = stmt Marked.pos list

and func = {
  func_params : (LocalName.t Marked.pos * D.typ Marked.pos) list;
  func_body : block;
}

type scope_body = {
  scope_body_name : Dcalc.Ast.ScopeName.t;
  scope_body_var : TopLevelName.t;
  scope_body_func : func;
}

type program = { decl_ctx : D.decl_ctx; scopes : scope_body list }
