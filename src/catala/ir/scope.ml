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

(* Identifiers *)

type uid = Context.uid

type ident = string

type qident = ident list

module UidMap = Uid.UidMap
module IdentMap = Context.IdentMap

(* Scopes *)
type binder = string Pos.marked

type definition = { definition_parameter : binder option; definition_term : Lambda.default_term }

type assertion = Lambda.term

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of Lambda.term * variation_typ Pos.marked option

type scope = {
  scope_uid : uid;
  uid_to_var : ident UidMap.t;
  var_to_uid : uid IdentMap.t;
  uid_typ : Context.typ UidMap.t;
  scope_defs : definition list UidMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list UidMap.t;
}

type program = scope UidMap.t
