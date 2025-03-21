(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

(** Abstract syntax tree of the scope language *)

open Catala_utils
open Shared_ast

(** {1 Identifiers} *)

type location = scopelang glocation

module LocationSet : Set.S with type elt = location Mark.pos

(** {1 Abstract syntax tree} *)

type 'm expr = (scopelang, 'm) gexpr

val locations_used : 'm expr -> LocationSet.t

type 'm rule =
  | ScopeVarDefinition of {
      var : ScopeVar.t * Pos.t list;
          (** Scope variable and its list of definitions' positions *)
      typ : typ;
      io : Desugared.Ast.io;
      e : 'm expr;
    }
  | SubScopeVarDefinition of {
      var : ScopeVar.t * Pos.t list;  (** Variable within the current scope *)
      var_within_origin_scope : ScopeVar.t;
      typ : typ; (* non-thunked at this point for reentrant vars *)
      e : 'm expr;
    }
  | Assertion of 'm expr

type scope_var_ty = {
  svar_in_ty : typ;
  svar_out_ty : typ;
  svar_io : Desugared.Ast.io;
}

type 'm scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : scope_var_ty ScopeVar.Map.t;
  scope_decl_rules : 'm rule list;
  scope_options : Desugared.Ast.catala_option Mark.pos list;
  scope_visibility : visibility;
}

type 'm program = {
  program_module_name : (ModuleName.t * module_intf_id) option;
  program_ctx : decl_ctx;
  program_modules : 'm scope_decl Mark.pos ScopeName.Map.t ModuleName.Map.t;
  program_scopes : 'm scope_decl Mark.pos ScopeName.Map.t;
  program_topdefs :
    ('m expr * typ * visibility * bool (* external if [true] *))
    TopdefName.Map.t;
  program_lang : Global.backend_lang;
}

val type_program : 'm program -> typed program
