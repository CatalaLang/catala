(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Pos = Utils.Pos

(* Scopes *)
type binder = Uid.LocalVar.t

type definition = unit

let empty_def (_pos : Pos.t) (_typ : Dcalc.Ast.typ) : definition = assert false

type assertion = unit

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of unit * variation_typ Pos.marked option

type scope = {
  scope_uid : Uid.Scope.t;
  scope_defs : definition Uid.ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

let empty_scope (uid : Uid.Scope.t) : scope =
  {
    scope_uid = uid;
    scope_defs = Uid.ScopeDefMap.empty;
    scope_assertions = [];
    scope_meta_assertions = [];
  }

type program = scope Uid.ScopeMap.t
