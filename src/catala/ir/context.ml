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

exception UnknownTyp of string

exception ContextError of string

type uid = Uid.t

module UidSet = Uid.UidSet
module UidMap = Uid.UidMap

type ident = string

module IdentMap = Map.Make (String)

type qident = ident list

type typ = Ast.typ

type uid_sort =
  | IdStruct
  | IdEnumName
  | IdScope
  | IdVar
  | IdEnumCase
  | IdStructName
  | IdScopeName
  | IdStructField
  | IdScopeContextItem
  | IdScopeContextScope

type uid_data = { uid_typ : typ option; uid_sort : uid_sort }

(* Note that the uid of the subscope should have the sort IdScopeName *)
type scope_include_data = { condition : Ast.expression option; sub_scope : ident }

type context = {
  ident_to_uid : uid list IdentMap.t;
  struct_decl : UidSet.t UidMap.t;
  enum_decl : UidSet.t UidMap.t;
  enum_cases : uid UidMap.t;
  scope_decl : UidSet.t UidMap.t;
  scope_include_data : scope_include_data UidMap.t;
  uid_data : uid_data UidMap.t;
}
