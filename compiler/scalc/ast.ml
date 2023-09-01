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
  | EVar : VarName.t -> naked_expr
  | EFunc : FuncName.t -> naked_expr
  | EStruct : expr list * StructName.t -> naked_expr
  | EStructFieldAccess : expr * StructField.t * StructName.t -> naked_expr
  | EInj : expr * EnumConstructor.t * EnumName.t -> naked_expr
  | EArray : expr list -> naked_expr
  | ELit : lit -> naked_expr
  | EApp : expr * expr list -> naked_expr
  | EOp : operator -> naked_expr

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

and block = stmt Mark.pos list
and func = { func_params : (VarName.t Mark.pos * typ) list; func_body : block }

type scope_body = {
  scope_body_name : ScopeName.t;
  scope_body_var : FuncName.t;
  scope_body_func : func;
}

type code_item =
  | SVar of { var : VarName.t; expr : expr }
  | SFunc of { var : FuncName.t; func : func }
  | SScope of scope_body

type program = { decl_ctx : decl_ctx; code_items : code_item list }
