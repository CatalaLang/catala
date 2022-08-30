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
open Shared_ast
module D = Dcalc.Ast
module L = Lcalc.Ast
module TopLevelName = Uid.Make (Uid.MarkedString) ()
module LocalName = Uid.Make (Uid.MarkedString) ()

let dead_value = LocalName.fresh ("dead_value", Pos.no_pos)
let handle_default = TopLevelName.fresh ("handle_default", Pos.no_pos)
let handle_default_opt = TopLevelName.fresh ("handle_default_opt", Pos.no_pos)

type expr = naked_expr Marked.pos

and naked_expr =
  | EVar of LocalName.t
  | EFunc of TopLevelName.t
  | EStruct of expr list * StructName.t
  | EStructFieldAccess of expr * StructFieldName.t * StructName.t
  | EInj of expr * EnumConstructor.t * EnumName.t
  | EArray of expr list
  | ELit of L.lit
  | EApp of expr * expr list
  | EOp of operator

type stmt =
  | SInnerFuncDef of LocalName.t Marked.pos * func
  | SLocalDecl of LocalName.t Marked.pos * typ
  | SLocalDef of LocalName.t Marked.pos * expr
  | STryExcept of block * except * block
  | SRaise of except
  | SIfThenElse of expr * block * block
  | SSwitch of
      expr
      * EnumName.t
      * (block (* Statements corresponding to arm closure body*)
        * (* Variable instantiated with enum payload *) LocalName.t)
        list  (** Each block corresponds to one case of the enum *)
  | SReturn of naked_expr
  | SAssert of naked_expr

and block = stmt Marked.pos list

and func = {
  func_params : (LocalName.t Marked.pos * typ) list;
  func_body : block;
}

type scope_body = {
  scope_body_name : ScopeName.t;
  scope_body_var : TopLevelName.t;
  scope_body_func : func;
}

type program = { decl_ctx : decl_ctx; scopes : scope_body list }
