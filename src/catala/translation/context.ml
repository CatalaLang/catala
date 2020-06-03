(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(* Introduce a context for all translations *)

type uid = int

type ident = string

type qident = ident list

module UidMap = Map.Make (Int)
module UidSet = Set.Make (Int)
module VarMap = Map.Make (Int)

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
  mutable counter : uid;
  ident_to_uid : (ident, uid) Hashtbl.t;
  struct_decl : UidSet.t UidMap.t;
  enum_decl : UidSet.t UidMap.t;
  enum_cases : uid UidMap.t;
  scope_decl : UidSet.t UidMap.t;
  scope_include_data : scope_include_data UidMap.t;
  uid_data : uid_data UidMap.t;
}

let get_ident_sort (context : context) (str : string) : uid_sort list =
  let uid_match = Hashtbl.find_all context.ident_to_uid str in
  List.map (fun uid -> (UidMap.find uid context.uid_data).uid_sort) uid_match

let add_ident (context : context) (ident : ident) : uid =
  let uid = context.counter in
  context.counter <- context.counter + 1;
  Hashtbl.add context.ident_to_uid ident uid;
  uid

exception UnknownTyp of string

(** Checks that types of the form Named constructor refer to existing structs or enum cases **)
let check_typ_well_formed (context : context) (typ : typ) : unit =
  let check_prim_typ = function
    | Ast.Named cons ->
        let sorts = get_ident_sort context cons in
        let well_typed =
          List.fold_left
            (fun k s -> match s with IdStructName | IdEnumName | IdEnumCase -> true | _ -> k)
            false sorts
        in
        if well_typed then () else raise (UnknownTyp cons)
    | _ -> ()
  in

  let rec check_base_typ_data = function
    | Ast.Primitive prim -> check_prim_typ prim
    | Ast.Collection t | Ast.Optional t -> check_base_typ_data (Pos.unmark t)
  in

  let check_base_typ = function
    | Ast.Condition -> ()
    | Ast.Data typ_data -> check_base_typ_data typ_data
  in

  match typ with
  | Ast.Base typ -> check_base_typ typ
  | Ast.Func { arg_typ; return_typ } ->
      let _ = check_base_typ (Pos.unmark arg_typ) in
      check_base_typ (Pos.unmark return_typ)

exception ContextError of string

(** Process a struct declaration, checking for wellformedness of types **)
let process_struct_decl (context : context) (s : Ast.struct_decl) : context =
  (* We don't accept to have twice the same name *)
  let s_name = Pos.unmark s.struct_decl_name in
  let _ =
    if List.length (get_ident_sort context s_name) > 0 then
      raise (ContextError ("Struct name " ^ s_name ^ " is already used"))
    else ()
  in
  let s_uid = add_ident context s_name in
  let s_data = { uid_typ = None; uid_sort = IdStructName } in
  let context =
    {
      context with
      struct_decl = UidMap.add s_uid UidSet.empty context.struct_decl;
      uid_data = UidMap.add s_uid s_data context.uid_data;
    }
  in

  let process_field context (field : Ast.struct_decl_field Pos.marked) : context =
    let field = Pos.unmark field in
    let field_name = Pos.unmark field.struct_decl_field_name in
    let field_typ = Pos.unmark field.struct_decl_field_typ in
    (* Check type *)
    let _ =
      try check_typ_well_formed context field_typ
      with UnknownTyp cons ->
        Errors.type_unknown_error (Pos.get_position field.struct_decl_field_typ) cons
    in
    (* An ident can be used for different structs, but not for the same one *)
    let possible_uid = UidSet.of_list (Hashtbl.find_all context.ident_to_uid field_name) in
    let fields_uid = UidMap.find s_uid context.struct_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uid fields_uid)) then
        raise (ContextError "Struct field is already used ")
      else ()
    in
    let field_uid = add_ident context field_name in
    let field_data = { uid_typ = Some field_typ; uid_sort = IdStructField } in
    let s_decl = UidSet.add field_uid (UidMap.find s_uid context.struct_decl) in
    {
      context with
      struct_decl = UidMap.add s_uid s_decl context.struct_decl;
      uid_data = UidMap.add field_uid field_data context.uid_data;
    }
  in

  List.fold_left process_field context s.struct_decl_fields

(** Process an enum declaration, checking for wellformedness of types *)
let process_enum_decl (context : context) (e : Ast.enum_decl) : context =
  let e_name = Pos.unmark e.enum_decl_name in
  let _ =
    if List.length (get_ident_sort context e_name) > 0 then
      raise (ContextError ("Enum name " ^ e_name ^ " is already used"))
    else ()
  in
  let e_uid = add_ident context e_name in
  let e_data = { uid_typ = None; uid_sort = IdEnumName } in
  let context =
    {
      context with
      struct_decl = UidMap.add e_uid UidSet.empty context.enum_decl;
      uid_data = UidMap.add e_uid e_data context.uid_data;
    }
  in

  let process_enum_case context (case : Ast.enum_decl_case Pos.marked) =
    let case = Pos.unmark case in
    let case_name = Pos.unmark case.enum_decl_case_name in
    let case_typ =
      match case.enum_decl_case_typ with
      | None -> None
      | Some t -> (
          try
            let t' = Pos.unmark t in
            let _ = check_typ_well_formed context t' in
            Some t'
          with UnknownTyp cons -> Errors.type_unknown_error (Pos.get_position t) cons )
    in
    (* For now, there can't be two enum cases with the same name *)
    let _ =
      if List.length (Hashtbl.find_all context.ident_to_uid case_name) > 0 then
        raise (ContextError ("EnumCase name " ^ case_name ^ " is already used"))
      else ()
    in
    let case_uid = add_ident context case_name in
    let case_data = { uid_typ = case_typ; uid_sort = IdEnumCase } in
    let e_decl = UidSet.add case_uid (UidMap.find e_uid context.enum_decl) in
    {
      context with
      enum_decl = UidMap.add e_uid e_decl context.enum_decl;
      uid_data = UidMap.add case_uid case_data context.uid_data;
    }
  in

  List.fold_left process_enum_case context e.enum_decl_cases

(** Process a scope declaration, checking for wellformedness of types *)
let process_scope_decl (context : context) (s : Ast.scope_decl) : context =
  let s_name = Pos.unmark s.scope_decl_name in
  let _ =
    if List.length (get_ident_sort context s_name) > 0 then
      raise (ContextError ("Scope name " ^ s_name ^ " is already used"))
    else ()
  in
  let s_uid = add_ident context s_name in
  let s_data = { uid_typ = None; uid_sort = IdScopeName } in
  let context =
    {
      context with
      scope_decl = UidMap.add s_uid UidSet.empty context.scope_decl;
      uid_data = UidMap.add s_uid s_data context.uid_data;
    }
  in

  let process_context_data context (item : Ast.scope_decl_context_data) =
    let item_name = Pos.unmark item.scope_decl_context_item_name in
    let item_typ = Pos.unmark item.scope_decl_context_item_typ in
    let _ =
      try check_typ_well_formed context item_typ
      with UnknownTyp cons ->
        Errors.type_unknown_error (Pos.get_position item.scope_decl_context_item_typ) cons
    in
    (* An ident can be used for different scopes, but not for the same one *)
    let possible_uids = UidSet.of_list (Hashtbl.find_all context.ident_to_uid item_name) in
    let scope_uids = UidMap.find s_uid context.scope_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uids scope_uids)) then
        raise (ContextError "Scope context item name is already used ")
      else ()
    in
    let item_uid = add_ident context item_name in
    let item_data = { uid_typ = Some item_typ; uid_sort = IdScopeContextItem } in
    let s_decl = UidSet.add item_uid (UidMap.find s_uid context.scope_decl) in
    {
      context with
      scope_decl = UidMap.add s_uid s_decl context.scope_decl;
      uid_data = UidMap.add item_uid item_data context.uid_data;
    }
  in

  let process_context_scope context (item : Ast.scope_decl_context_scope) =
    let item_name = Pos.unmark item.scope_decl_context_scope_name in
    let item_sub_scope = Pos.unmark item.scope_decl_context_scope_sub_scope in
    let item_condition =
      match item.scope_decl_context_scope_condition with
      | None -> None
      | Some expr -> Some (Pos.unmark expr)
    in
    (* Check that the subscope exists *)
    let _ =
      if not (List.exists (fun s -> s = IdScopeName) (get_ident_sort context item_sub_scope)) then
        raise (ContextError ("Scope " ^ item_sub_scope ^ " does not exist"))
      else ()
    in

    (* An ident can be used for different scopes, but not for the same one *)
    let possible_uids = UidSet.of_list (Hashtbl.find_all context.ident_to_uid item_name) in
    let scope_uids = UidMap.find s_uid context.scope_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uids scope_uids)) then
        raise (ContextError "Scope context item name is already used ")
      else ()
    in
    let item_uid = add_ident context item_name in
    let item_data = { uid_typ = None; uid_sort = IdScopeContextScope } in
    let include_data = { condition = item_condition; sub_scope = item_sub_scope } in
    let s_decl = UidSet.add item_uid (UidMap.find s_uid context.scope_decl) in
    {
      context with
      scope_decl = UidMap.add s_uid s_decl context.scope_decl;
      uid_data = UidMap.add item_uid item_data context.uid_data;
      scope_include_data = UidMap.add item_uid include_data context.scope_include_data;
    }
  in

  let process_context_item context (item : Ast.scope_decl_context_item Pos.marked) =
    match Pos.unmark item with
    | ContextData data -> process_context_data context data
    | ContextScope scope -> process_context_scope context scope
  in

  List.fold_left process_context_item context s.scope_decl_context

(* Process one declaration, checking for wellformedness of types *)
let process_decl (context : context) (decl : Ast.code_item) : context =
  match decl with
  | ScopeUse _ -> context
  | ScopeDecl decl -> process_scope_decl context decl
  | StructDecl decl -> process_struct_decl context decl
  | EnumDecl decl -> process_enum_decl context decl

(** Process all declarations of a given program to form its context *)
let form_context (prgm : Ast.program) : context =
  let context : context =
    {
      counter = 0;
      ident_to_uid = Hashtbl.create 1000;
      struct_decl = UidMap.empty;
      enum_decl = UidMap.empty;
      enum_cases = UidMap.empty;
      scope_decl = UidMap.empty;
      scope_include_data = UidMap.empty;
      uid_data = UidMap.empty;
    }
  in

  let process_program_item context = function
    | Ast.MetadataBlock (block, _) ->
        List.fold_left (fun context item -> process_decl context (Pos.unmark item)) context block
    | _ -> context
  in

  List.fold_left process_program_item context prgm.program_items
