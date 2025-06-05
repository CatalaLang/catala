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
  | ETupleAccess of { e1 : expr; index : int; typ : typ }
  | EInj of {
      e1 : expr;
      cons : EnumConstructor.t;
      name : EnumName.t;
      expr_typ : typ;
    }
  | EArray of expr list
  | ELit of lit
  | EPosLit
  | EApp of { f : expr; args : expr list }
  | EAppOp of { op : operator Mark.pos; args : expr list; tys : typ list }
  | EExternal of { modname : VarName.t Mark.pos; name : string Mark.pos }

type stmt =
  | SInnerFuncDef of { name : VarName.t Mark.pos; func : func }
  | SLocalDecl of { name : VarName.t Mark.pos; typ : typ }
  | SLocalInit of { name : VarName.t Mark.pos; typ : typ; expr : expr }
  | SLocalDef of { name : VarName.t Mark.pos; typ : typ; expr : expr }
  | SFatalError of { pos_expr : expr; error : Runtime.error }
      (** [pos_expr] here is the position reified into an expression *)
  | SIfThenElse of { if_expr : expr; then_block : block; else_block : block }
  | SSwitch of {
      switch_var : VarName.t;
      switch_var_typ : typ;
      enum_name : EnumName.t;
      switch_cases : switch_case list;
    }
  | SReturn of expr
  | SAssert of { pos_expr : expr; expr : expr }
      (** [pos_expr] here is the position reified into an expression *)
  | SSpecialOp of special_operator

and special_operator = |
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
  scope_body_visibility : visibility;
}

type code_item =
  | SVar of { var : VarName.t; expr : expr; typ : typ; visibility : visibility }
  | SFunc of { var : FuncName.t; func : func; visibility : visibility }
  | SScope of scope_body

type ctx = { decl_ctx : decl_ctx; modules : VarName.t ModuleName.Map.t }

type program = {
  ctx : ctx;
  code_items : code_item list;
  tests : code_item list * (ScopeName.t * block) list;
      (** The first element may contain lifted closures. It can be assumed to be
          empty when closure conversion is disabled. *)
  module_name : (ModuleName.t * module_intf_id) option;
}
