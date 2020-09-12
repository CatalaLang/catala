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

type typ = Lambda_ast.typ

type sort =
  | IdScope
  | IdScopeVar of uid option
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

let raise_unsupported_feature (msg : string) (pos : Pos.t) =
  Errors.raise_spanned_error (Printf.sprintf "Unsupported feature: %s" msg) pos

let raise_undefined_identifier (msg : string) (pos : Pos.t) =
  Errors.raise_spanned_error (Printf.sprintf "Undefined identifier: %s" msg) pos

let raise_unknown_identifier (msg : string) (pos : Pos.t) =
  Errors.raise_spanned_error (Printf.sprintf "Unknown identifier: %s" msg) pos

(** Get the type associated to an uid *)
let get_uid_typ (ctxt : context) (uid : uid) : typ = (UidMap.find uid ctxt.data).uid_typ

(** Get the sort associated to an uid *)
let get_uid_sort (ctxt : context) (uid : uid) : sort = (UidMap.find uid ctxt.data).uid_sort

(** Process a subscope declaration *)
let process_subscope_decl (scope : uid) (ctxt : context)
    (decl : Catala_ast.scope_decl_context_scope) : context =
  let name, decl_pos = decl.scope_decl_context_scope_name in
  let subscope, s_pos = decl.scope_decl_context_scope_sub_scope in
  (* First check that the designated subscope is a scope *)
  let sub_uid =
    match IdentMap.find_opt subscope ctxt.scope_id_to_uid with
    | None -> raise_undefined_identifier subscope s_pos
    | Some uid -> (
        match get_uid_sort ctxt uid with
        | IdScope -> uid
        | _ -> raise_undefined_identifier "..." s_pos )
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
              { uid_typ = Lambda_ast.TDummy; uid_sort = IdSubScope sub_uid }
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

let process_base_typ ((typ, typ_pos) : Catala_ast.base_typ Pos.marked) : Lambda_ast.typ =
  match typ with
  | Catala_ast.Condition -> Lambda_ast.TBool
  | Catala_ast.Data (Catala_ast.Collection _) -> raise_unsupported_feature "collection type" typ_pos
  | Catala_ast.Data (Catala_ast.Optional _) -> raise_unsupported_feature "option type" typ_pos
  | Catala_ast.Data (Catala_ast.Primitive prim) -> (
      match prim with
      | Catala_ast.Integer | Catala_ast.Decimal | Catala_ast.Money | Catala_ast.Date ->
          Lambda_ast.TInt
      | Catala_ast.Boolean -> Lambda_ast.TBool
      | Catala_ast.Text -> raise_unsupported_feature "text type" typ_pos
      | Catala_ast.Named _ -> raise_unsupported_feature "struct or enum types" typ_pos )

let process_type ((typ, typ_pos) : Catala_ast.typ Pos.marked) : Lambda_ast.typ =
  match typ with
  | Catala_ast.Base base_typ -> process_base_typ (base_typ, typ_pos)
  | Catala_ast.Func { arg_typ; return_typ } ->
      Lambda_ast.TArrow (process_base_typ arg_typ, process_base_typ return_typ)

(** Process data declaration *)
let process_data_decl (scope : uid) (ctxt : context) (decl : Catala_ast.scope_decl_context_data) :
    context =
  (* First check the type of the context data *)
  let lambda_typ = process_type decl.scope_decl_context_item_typ in
  let name, pos = decl.scope_decl_context_item_name in
  let scope_ctxt = UidMap.find scope ctxt.scopes in
  match IdentMap.find_opt name scope_ctxt.var_id_to_uid with
  | Some _ -> (* Variable is already used in this scope *) assert false
  | None -> (
      let uid = Uid.fresh name pos in
      let scope_ctxt =
        {
          var_id_to_uid = IdentMap.add name uid scope_ctxt.var_id_to_uid;
          uid_set = UidSet.add uid scope_ctxt.uid_set;
        }
      in

      match lambda_typ with
      | TArrow (arg_typ, _) ->
          (* We now can get a fresh uid for the data *)
          let arg_uid = Uid.fresh (Printf.sprintf "ARG_OF(%s)" name) pos in
          let arg_data = { uid_typ = arg_typ; uid_sort = IdBinder } in
          let var_data = { uid_typ = lambda_typ; uid_sort = IdScopeVar (Some arg_uid) } in
          let data = ctxt.data |> UidMap.add uid var_data |> UidMap.add arg_uid arg_data in
          { ctxt with scopes = UidMap.add scope scope_ctxt ctxt.scopes; data }
      | _ ->
          {
            ctxt with
            scopes = UidMap.add scope scope_ctxt ctxt.scopes;
            data = UidMap.add uid { uid_typ = lambda_typ; uid_sort = IdScopeVar None } ctxt.data;
          } )

(** Process an item declaration *)
let process_item_decl (scope : uid) (ctxt : context) (decl : Catala_ast.scope_decl_context_item) :
    context =
  match decl with
  | Catala_ast.ContextData data_decl -> process_data_decl scope ctxt data_decl
  | Catala_ast.ContextScope sub_decl -> process_subscope_decl scope ctxt sub_decl

(** Process a scope declaration *)
let process_scope_decl (ctxt : context) (decl : Catala_ast.scope_decl) : context =
  let name, pos = decl.scope_decl_name in
  (* Checks if the name is already used *)
  match IdentMap.find_opt name ctxt.scope_id_to_uid with
  | Some _ -> assert false
  | None ->
      let scope_uid = Uid.fresh name pos in
      let ctxt =
        {
          scope_id_to_uid = IdentMap.add name scope_uid ctxt.scope_id_to_uid;
          data = UidMap.add scope_uid { uid_typ = Lambda_ast.TDummy; uid_sort = IdScope } ctxt.data;
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
let process_code_item (ctxt : context) (item : Catala_ast.code_item) : context =
  match item with ScopeDecl decl -> process_scope_decl ctxt decl | _ -> ctxt

(** Process a code block *)
let process_code_block (ctxt : context) (block : Catala_ast.code_block) : context =
  List.fold_left (fun ctxt decl -> Pos.unmark decl |> process_code_item ctxt) ctxt block

(** Process a program item *)
let process_program_item (ctxt : context) (item : Catala_ast.program_item) : context =
  match item with
  | CodeBlock (block, _) | MetadataBlock (block, _) -> process_code_block ctxt block
  | _ -> ctxt

(** Derive the context from metadata *)
let form_context (prgm : Catala_ast.program) : context =
  let empty_ctxt =
    { scope_id_to_uid = IdentMap.empty; scopes = UidMap.empty; data = UidMap.empty }
  in
  List.fold_left process_program_item empty_ctxt prgm.program_items

(** Get the variable uid inside the scope given in argument *)
let get_var_uid (scope_uid : uid) (ctxt : context) ((x, pos) : ident Pos.marked) : uid =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt x scope.var_id_to_uid with
  | None -> raise_undefined_identifier x pos
  | Some uid -> (
      (* Checks that the uid has sort IdScopeVar or IdScopeBinder *)
      match get_uid_sort ctxt uid with
      | IdScopeVar _ | IdBinder | IdSubScopeVar _ -> uid
      | _ ->
          let err_msg = Printf.sprintf "Identifier \"%s\" should be a variable, but it isn't" x in
          Errors.raise_spanned_error err_msg pos )

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid (scope_uid : uid) (ctxt : context) ((y, pos) : ident Pos.marked) : uid * uid =
  let scope = UidMap.find scope_uid ctxt.scopes in
  match IdentMap.find_opt y scope.var_id_to_uid with
  | None -> raise_unknown_identifier y pos
  | Some sub_uid -> (
      match get_uid_sort ctxt sub_uid with
      | IdSubScope scope_ref -> (sub_uid, scope_ref)
      | _ ->
          let err_msg = Printf.sprintf "Identifier \"%s\" should be a subscope, but it isn't" y in
          Errors.raise_spanned_error err_msg pos )

(** Checks if the var_uid belongs to the scope scope_uid *)
let belongs_to (ctxt : context) (uid : var_uid) (scope_uid : scope_uid) : bool =
  let scope = UidMap.find scope_uid ctxt.scopes in
  UidSet.mem uid scope.uid_set

(** Adds a binding to the context *)
let add_binding (ctxt : context) (scope_uid : Uid.t) (fun_uid : Uid.t)
    (bind_name : ident Pos.marked option) : context * Uid.t option =
  match bind_name with
  | None -> (ctxt, None)
  | Some name ->
      let name = Pos.unmark name in
      let scope_ctxt = UidMap.find scope_uid ctxt.scopes in
      let arg_uid =
        match get_uid_sort ctxt fun_uid with
        | IdScopeVar (Some arg_uid) -> arg_uid
        | _ ->
            Errors.raise_spanned_error
              (Printf.sprintf "Var %s is supposed to be a function but it isn't"
                 (Uid.get_ident fun_uid))
              (Uid.get_pos fun_uid)
      in
      let scope_ctxt =
        { scope_ctxt with var_id_to_uid = IdentMap.add name arg_uid scope_ctxt.var_id_to_uid }
      in
      ({ ctxt with scopes = UidMap.add scope_uid scope_ctxt ctxt.scopes }, Some arg_uid)
