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

type uid = Uid.t

module UidMap = Uid.UidMap
module UidSet = Uid.UidSet

type ident = string

module IdentMap = Map.Make (String)

type typ = Lambda.typ

type uid_sort = IdScope | IdScopeVar | IdSubScope of uid

type uid_data = { uid_typ : typ; uid_sort : uid_sort }

type scope_context = { var_id_to_uid : uid IdentMap.t }

type context = {
  scope_id_to_uid : uid IdentMap.t;
  scopes : scope_context UidMap.t;
  data : uid_data UidMap.t;
}

(** Get the type associated to an uid *)
let get_uid_typ (_ctxt : context) (_uid : uid) : typ option = assert false

(** Get the variable uid inside the scope given in argument *)
let get_var_uid (_scope : uid) (_ctxt : context) (_x : ident) : uid option = assert false

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid (_scope : uid) (_ctxt : context) (_y : ident) : uid option = assert false
