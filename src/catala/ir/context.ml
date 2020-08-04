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

type scope_uid = Uid.t

type var_uid = Uid.t

type sub_scope_uid = Uid.t

module UidMap = Uid.UidMap
module UidSet = Uid.UidSet

type ident = string

module IdentMap = Map.Make (String)

type typ = Lambda.typ

type sort =
  | IdScope
  | IdScopeVar
  | IdSubScope of uid
  | IdSubScopeVar of var_uid * sub_scope_uid
  | IdBinder

type uid_data = { uid_typ : typ; uid_sort : sort }

type scope_context = { var_id_to_uid : uid IdentMap.t; uid_set : UidSet.t }

type context = {
  scope_id_to_uid : uid IdentMap.t;
  scopes : scope_context UidMap.t;
  data : uid_data UidMap.t;
}

let subscope_ident (y : string) (x : string) : string = y ^ "::" ^ x

exception UnsupportedFeature of string * Pos.t

exception UndefinedIdentifier of string * Pos.t

(** Get the type associated to an uid *)
let get_uid_typ (ctxt : context) (uid : uid) : typ = (UidMap.find uid ctxt.data).uid_typ

(** Get the sort associated to an uid *)
let get_uid_sort (ctxt : context) (uid : uid) : sort = (UidMap.find uid ctxt.data).uid_sort

(** Process a subscope declaration *)
let process_subscope_decl (scope : uid) (ctxt : context) (decl : Ast.scope_decl_context_scope) :
    context =
  let name, decl_pos = decl.scope_decl_context_scope_name in
  let subscope, s_pos = decl.scope_decl_context_scope_sub_scope in
  (* First check that the designated subscope is a scope *)
  let sub_uid =
    match IdentMap.find_opt subscope ctxt.scope_id_to_uid with
    | None -> raise (UndefinedIdentifier (subscope, s_pos))
    | Some uid -> (
        match get_uid_sort ctxt uid with
        | IdScope -> uid
        | _ -> raise (UndefinedIdentifier ("...", s_pos)) )
  in
  let scope_ctxt = UidMap.find scope ctxt.scopes in
  let subscope_ctxt = UidMap.find sub_uid ctxt.scopes in
  match IdentMap.find_opt name scope_ctxt.var_id_to_uid with
  | Some _ -> assert false (* Variable is already used in this scope *)
  | None ->
      let sub_scope_uid = Uid.fresh name decl_pos in
      let scope_ctxt =
        {
          var_id_to_uid = IdentMap.add name sub_scope_uid scope_ctxt.var_id_to_uid;
          uid_set = UidSet.add sub_scope_uid scope_ctxt.uid_set;
        }
      in
      let ctxt =
        {
          ctxt with
          scopes = UidMap.add scope scope_ctxt ctxt.scopes;
          data =
            UidMap.add sub_scope_uid
              { uid_typ = Lambda.TDummy; uid_sort = IdSubScope sub_uid }
              ctxt.data;
        }
      in
      (* Now duplicate all variables from the subscope *)
      IdentMap.fold
        (fun sub_var sub_uid ctxt ->
          let fresh_varname = subscope_ident name sub_var in
          (* We use the same pos as the subscope declaration *)
          let fresh_uid = Uid.fresh fresh_varname decl_pos in
          let scope_ctxt = UidMap.find scope ctxt.scopes in
          let scope_ctxt =
            {
              var_id_to_uid = IdentMap.add fresh_varname fresh_uid scope_ctxt.var_id_to_uid;
              uid_set = UidSet.add fresh_uid scope_ctxt.uid_set;
            }
          in
          let sub_data = UidMap.find sub_uid ctxt.data in
          (* Add a reference to the subvar *)
          let data = { sub_data with uid_sort = IdSubScopeVar (sub_uid, sub_scope_uid) } in
          {
            ctxt with
            scopes = UidMap.add scope scope_ctxt ctxt.scopes;
            data = UidMap.add fresh_uid data ctxt.data;
          })
        subscope_ctxt.var_id_to_uid ctxt

(** Process data declaration *)
let process_data_decl (scope : uid) (ctxt : context) (decl : Ast.scope_decl_context_data) : context
    =
  (* First check the type of the context data *)
  let typ, typ_pos = decl.scope_decl_context_item_typ in
  let ltyp =
    match typ with
    | Ast.Base Ast.Condition -> Lambda.TBool
    | Ast.Base (Ast.Data (Ast.Collection _)) ->
        raise (UnsupportedFeature ("Collection type", typ_pos))
    | Ast.Base (Ast.Data (Ast.Optional _)) -> raise (UnsupportedFeature ("Option type", typ_pos))
    | Ast.Base (Ast.Data (Ast.Primitive prim)) -> (
        match prim with
        | Ast.Integer | Ast.Decimal | Ast.Money | Ast.Date -> Lambda.TInt
        | Ast.Boolean -> Lambda.TBool
        | Ast.Text -> raise (UnsupportedFeature ("Text type", typ_pos))
        | Ast.Named _ -> raise (UnsupportedFeature ("Struct or enum types", typ_pos)) )
    | Ast.Func _ -> raise (UnsupportedFeature ("Function types", typ_pos))
  in
  let name, pos = decl.scope_decl_context_item_name in
  let scope_ctxt = UidMap.find scope ctxt.scopes in
  match IdentMap.find_opt name scope_ctxt.var_id_to_uid with
  | Some _ -> (* Variable is already used in this scope *) assert false
  | None ->
      (* We now can get a fresh uid for the data *)
      let uid = Uid.fresh name pos in
      let scope_ctxt =
        {
          var_id_to_uid = IdentMap.add name uid scope_ctxt.var_id_to_uid;
          uid_set = UidSet.add uid scope_ctxt.uid_set;
        }
      in
      {
        ctxt with
        scopes = UidMap.add scope scope_ctxt ctxt.scopes;
        data = UidMap.add uid { uid_typ = ltyp; uid_sort = IdScopeVar } ctxt.data;
      }

(** Process an item declaration *)
let process_item_decl (scope : uid) (ctxt : context) (decl : Ast.scope_decl_context_item) : context
    =
  match decl with
  | Ast.ContextData data_decl -> process_data_decl scope ctxt data_decl
  | Ast.ContextScope sub_decl -> process_subscope_decl scope ctxt sub_decl

(** Process a scope declaration *)
let process_scope_decl (ctxt : context) (decl : Ast.scope_decl) : context =
  let name, pos = decl.scope_decl_name in
  (* Checks if the name is already used *)
  match IdentMap.find_opt name ctxt.scope_id_to_uid with
  | Some _ -> assert false
  | None ->
      let scope_uid = Uid.fresh name pos in
      let ctxt =
        {
          scope_id_to_uid = IdentMap.add name scope_uid ctxt.scope_id_to_uid;
          data = UidMap.add scope_uid { uid_typ = Lambda.TDummy; uid_sort = IdScope } ctxt.data;
          scopes =
            UidMap.add scope_uid
              { var_id_to_uid = IdentMap.empty; uid_set = UidSet.empty }
              ctxt.scopes;
        }
      in
      List.fold_left
        (fun ctxt item -> process_item_decl scope_uid ctxt (Pos.unmark item))
        ctxt decl.scope_decl_context

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

(** Get the variable uid inside the scope given in argument *)
let get_var_uid (scope_uid : uid) (ctxt : context) ((x, pos) : ident Pos.marked) : uid =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt x scope.var_id_to_uid with
  | None -> Errors.unknown_identifier x pos
  | Some uid -> (
      (* Checks that the uid has sort IdScopeVar or IdScopeBinder *)
      match get_uid_sort ctxt uid with
      | IdScopeVar | IdBinder | IdSubScopeVar _ -> uid
      | _ ->
          let err_msg =
            Printf.sprintf "Identifier \"%s\" should be a variable, but it isn't\n%s" x
              (Pos.to_string pos)
          in
          Errors.context_error err_msg )

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid (scope_uid : uid) (ctxt : context) ((y, pos) : ident Pos.marked) : uid * uid =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt y scope.var_id_to_uid with
  | None -> Errors.unknown_identifier y pos
  | Some sub_uid -> (
      match get_uid_sort ctxt sub_uid with
      | IdSubScope scope_ref -> (sub_uid, scope_ref)
      | _ ->
          let err_msg =
            Printf.sprintf "Identifier \"%s\" should be a subscope, but it isn't\n%s" y
              (Pos.retrieve_loc_text pos)
          in
          Errors.context_error err_msg )

(** Checks if the var_uid belongs to the scope scope_uid *)
let belongs_to (ctxt : context) (uid : var_uid) (scope_uid : scope_uid) : bool =
  let scope = UidMap.find scope_uid ctxt.scopes in
  UidSet.mem uid scope.uid_set
