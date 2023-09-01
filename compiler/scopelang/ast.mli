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
  | Definition of location Mark.pos * typ * Desugared.Ast.io * 'm expr
  | Assertion of 'm expr
  | Call of ScopeName.t * SubScopeName.t * 'm mark

type 'm scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ * Desugared.Ast.io) ScopeVar.Map.t;
  scope_decl_rules : 'm rule list;
  scope_options : Desugared.Ast.catala_option Mark.pos list;
}

type 'm program = {
  program_scopes : 'm scope_decl Mark.pos ScopeName.Map.t;
  program_topdefs : ('m expr * typ) TopdefName.Map.t;
  program_modules : nil program ModuleName.Map.t;
  (* Using [nil] here ensure that program interfaces don't contain any
     expressions. They won't contain any rules or topdefs, but will still have
     the scope signatures needed to respect the call convention *)
  program_ctx : decl_ctx;
}

val type_program : 'm program -> typed program
