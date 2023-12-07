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

open Catala_utils
open Shared_ast
module D = Dcalc.Ast
module L = Lcalc.Ast

module FuncName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 green))
    end)
    ()

module VarName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_green))
    end)
    ()

let dead_value = VarName.fresh ("dead_value", Pos.no_pos)
let handle_default = FuncName.fresh ("handle_default", Pos.no_pos)
let handle_default_opt = FuncName.fresh ("handle_default_opt", Pos.no_pos)

type operator =
  < overloaded : no ; monomorphic : yes ; polymorphic : yes ; resolved : yes >
  Shared_ast.operator

type expr = naked_expr Mark.pos

and naked_expr =
  | EVar of VarName.t
  | EFunc of FuncName.t
  | EStruct of expr list * StructName.t
  | EStructFieldAccess of expr * StructField.t * StructName.t
  | ETuple of expr list
  | ETupleAccess of expr * int
  | EInj of expr * EnumConstructor.t * EnumName.t
  | EArray of expr list
  | ELit of lit
  | EApp of expr * expr list
  | EOp of operator

type stmt =
  | SInnerFuncDef of VarName.t Mark.pos * func
  | SLocalDecl of VarName.t Mark.pos * typ
  | SLocalDef of VarName.t Mark.pos * expr
  | STryExcept of block * except * block
  | SRaise of except
  | SIfThenElse of expr * block * block
  | SSwitch of
      expr
      * EnumName.t
      * (block (* Statements corresponding to arm closure body*)
        * (* Variable instantiated with enum payload *) VarName.t)
        list  (** Each block corresponds to one case of the enum *)
  | SReturn of naked_expr
  | SAssert of naked_expr
  | SSpecialOp of special_operator

and special_operator = OHandleDefaultOpt of expr list * expr * block
and block = stmt Mark.pos list

and func = {
  func_params : (VarName.t Mark.pos * typ) list;
  func_body : block;
  func_return_typ : typ;
}

type scope_body = {
  scope_body_name : ScopeName.t;
  scope_body_var : FuncName.t;
  scope_body_func : func;
}

type code_item =
  | SVar of { var : VarName.t; expr : expr; typ : typ }
  | SFunc of { var : FuncName.t; func : func }
  | SScope of scope_body

type program = { decl_ctx : decl_ctx; code_items : code_item list }
