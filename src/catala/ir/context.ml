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

type uid_sort = IdScope | IdScopeVar | IdSubScope of uid | IdBinder

type uid_data = { uid_typ : typ; uid_sort : uid_sort }

type scope_context = { var_id_to_uid : uid IdentMap.t }

type context = {
  scope_id_to_uid : uid IdentMap.t;
  scopes : scope_context UidMap.t;
  data : uid_data UidMap.t;
}

(** Derive the context from metadata *)
let form_context (_prgm : Ast.program) : context = assert false

(** Get the type associated to an uid *)
let get_uid_typ (ctxt : context) (uid : uid) : typ option =
  UidMap.find_opt uid ctxt.data |> Option.map (fun data -> data.uid_typ)

(** Get the variable uid inside the scope given in argument *)
let get_var_uid (scope_uid : uid) (ctxt : context) (x : ident) : uid option =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt x scope.var_id_to_uid with
  | None -> None
  | Some uid ->
      (* Checks that the uid has sort IdScopeVar or IdScopeBinder *)
      let data = UidMap.find uid ctxt.data in
      if data.uid_sort <> IdScopeVar || data.uid_sort <> IdBinder then None else Some uid

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid (scope_uid : uid) (ctxt : context) (y : ident) : uid option =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt y scope.var_id_to_uid with
  | None -> None
  | Some sub_uid -> (
      match (UidMap.find sub_uid ctxt.data).uid_sort with
      | IdSubScope subscope_uid -> Some subscope_uid
      | _ -> None )
