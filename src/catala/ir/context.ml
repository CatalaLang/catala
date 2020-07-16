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

(** Process a subscope declaration *)
let process_subscope_decl (_ctxt : context) (_decl : Ast.scope_decl_context_scope) : context =
  assert false

(** Process data declaration *)
let process_data_decl (_ctxt : context) (_decl : Ast.scope_decl_context_data) : context =
  assert false

(** Process an item declaration *)
let process_item_decl (ctxt : context) (decl : Ast.scope_decl_context_item) : context =
  match decl with
  | Ast.ContextData data_decl -> process_data_decl ctxt data_decl
  | Ast.ContextScope sub_decl -> process_subscope_decl ctxt sub_decl

(** Process a scope declaration *)
let process_scope_decl (_ctxt : context) (_decl : Ast.scope_decl) : context = assert false

(** Process a code item : for now it only handles scope decls *)
let process_code_item (ctxt : context) (item : Ast.code_item) : context =
  match item with ScopeDecl decl -> process_scope_decl ctxt decl | _ -> ctxt

(** Process a code block *)
let process_code_block (ctxt : context) (block : Ast.code_block) : context =
  List.fold_left (fun ctxt decl -> Pos.unmark decl |> process_code_item ctxt) ctxt block

(** Process a program item *)
let process_program_item (ctxt : context) (item : Ast.program_item) : context =
  match item with
  | CodeBlock (block, _) | MetadataBlock (block, _) -> process_code_block ctxt block
  | _ -> ctxt

(** Derive the context from metadata *)
let form_context (prgm : Ast.program) : context =
  let empty_ctxt =
    { scope_id_to_uid = IdentMap.empty; scopes = UidMap.empty; data = UidMap.empty }
  in
  List.fold_left process_program_item empty_ctxt prgm.program_items

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
