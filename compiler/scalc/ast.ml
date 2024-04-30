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
      let style = Ocolor_types.Default_fg
    end)
    ()

let dead_value = VarName.fresh ("dead_value", Pos.no_pos)
let handle_default = FuncName.fresh ("handle_default", Pos.no_pos)
let handle_default_opt = FuncName.fresh ("handle_default_opt", Pos.no_pos)

type operator = Shared_ast.lcalc Shared_ast.operator

type expr = naked_expr Mark.pos

and naked_expr =
  | EVar of VarName.t
  | EFunc of FuncName.t
  | EStruct of { fields : expr StructField.Map.t; name : StructName.t }
  | EStructFieldAccess of {
      e1 : expr;
      field : StructField.t;
      name : StructName.t;
    }
  | ETuple of expr list
  | ETupleAccess of { e1 : expr; index : int }
  | EInj of {
      e1 : expr;
      cons : EnumConstructor.t;
      name : EnumName.t;
      expr_typ : typ;
    }
  | EArray of expr list
  | ELit of lit
  | EApp of { f : expr; args : expr list }
  | EAppOp of { op : operator Mark.pos; args : expr list }
  | EExternal of { modname : VarName.t Mark.pos; name : string Mark.pos }

type stmt =
  | SInnerFuncDef of { name : VarName.t Mark.pos; func : func }
  | SLocalDecl of { name : VarName.t Mark.pos; typ : typ }
  | SLocalInit of { name : VarName.t Mark.pos; typ : typ; expr : expr }
  | SLocalDef of { name : VarName.t Mark.pos; expr : expr; typ : typ }
  | STryWEmpty of { try_block : block; with_block : block }
  | SRaiseEmpty
  | SFatalError of Runtime.error
  | SIfThenElse of { if_expr : expr; then_block : block; else_block : block }
  | SSwitch of {
      switch_expr : expr;
      switch_expr_typ : typ;
      enum_name : EnumName.t;
      switch_cases : switch_case list;
    }
  | SReturn of naked_expr
  | SAssert of naked_expr
  | SSpecialOp of special_operator

and special_operator =
  | OHandleDefaultOpt of {
      exceptions : expr list;
      just : expr;
      cons : block;
      return_typ : typ;
    }

and block = stmt Mark.pos list

and switch_case = {
  case_block : block;
  payload_var_name : VarName.t;
  payload_var_typ : typ;
}

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

type ctx = { decl_ctx : decl_ctx; modules : VarName.t ModuleName.Map.t }

type program = {
  ctx : ctx;
  code_items : code_item list;
  module_name : ModuleName.t option;
}
