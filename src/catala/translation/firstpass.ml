(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the license *)

(** Processes program to :

    - form the context of a program from metadata blocks
    - regroup rules or definitions (aka defaults) of the same variable under the same hood
    - put uids on variables **)

module UidMap = Context.UidMap
module UidSet = Context.UidSet
module IdentMap = Context.IdentMap

type uid = Context.uid

type ident = Context.ident

type uid_sort = Context.uid_sort

type typ = Context.typ

(* CONTEXT FORMATION *)

(** TODO : add some comment here *)
let get_ident_sort (context : Context.context) (str : string) : Context.uid_sort list =
  let uid_match =
    match IdentMap.find_opt str context.ident_to_uid with None -> [] | Some uids -> uids
  in
  List.map (fun uid -> (UidMap.find uid context.uid_data).uid_sort) uid_match

(** idem *)
let find_uid_list (ident : ident) (context : Context.context) : uid list =
  match IdentMap.find_opt ident context.ident_to_uid with None -> [] | Some l -> l

let add_ident (context : Context.context) (ident : ident) : Context.context * uid =
  let uid = Uid.fresh () in
  let uid_list = uid :: find_uid_list ident context in
  ({ context with ident_to_uid = IdentMap.add ident uid_list context.ident_to_uid }, uid)

(** Checks that types of the form Named constructor refer to existing structs or enum cases **)
let check_typ_well_formed (context : Context.context) (typ : typ) : unit =
  let check_prim_typ : Ast.primitive_typ -> unit = function
    | Ast.Named cons ->
        let sorts = get_ident_sort context cons in
        let well_typed =
          List.fold_left
            (fun k s ->
              match s with
              | Context.IdStructName | Context.IdEnumName | Context.IdEnumCase -> true
              | _ -> k)
            false sorts
        in
        if well_typed then () else raise (Context.UnknownTyp cons)
    | _ -> ()
  in

  let rec check_base_typ_data : Ast.base_typ_data -> unit = function
    | Ast.Primitive prim -> check_prim_typ prim
    | Ast.Collection t | Ast.Optional t -> check_base_typ_data (Pos.unmark t)
  in

  let check_base_typ : Ast.base_typ -> unit = function
    | Ast.Condition -> ()
    | Ast.Data typ_data -> check_base_typ_data typ_data
  in

  match typ with
  | Ast.Base typ -> check_base_typ typ
  | Ast.Func { arg_typ; return_typ } ->
      let _ = check_base_typ (Pos.unmark arg_typ) in
      check_base_typ (Pos.unmark return_typ)

(** Process a struct declaration, checking for wellformedness of types **)
let process_struct_decl (context : Context.context) (s : Ast.struct_decl) : Context.context =
  (* We don't accept to have twice the same name *)
  let s_name = Pos.unmark s.struct_decl_name in
  let _ =
    if List.length (get_ident_sort context s_name) > 0 then
      raise (Context.ContextError ("Struct name " ^ s_name ^ " is already used"))
    else ()
  in
  let context, s_uid = add_ident context s_name in
  let s_data : Context.uid_data = { uid_typ = None; uid_sort = IdStructName } in
  let context =
    {
      context with
      struct_decl = UidMap.add s_uid UidSet.empty context.struct_decl;
      uid_data = UidMap.add s_uid s_data context.uid_data;
    }
  in

  let process_field context (field : Ast.struct_decl_field Pos.marked) : Context.context =
    let field = Pos.unmark field in
    let field_name = Pos.unmark field.struct_decl_field_name in
    let field_typ = Pos.unmark field.struct_decl_field_typ in
    (* Check type *)
    let _ =
      try check_typ_well_formed context field_typ
      with Context.UnknownTyp cons ->
        Errors.type_unknown_error (Pos.get_position field.struct_decl_field_typ) cons
    in
    (* An ident can be used for different structs, but not for the same one *)
    let possible_uid = UidSet.of_list (find_uid_list field_name context) in
    let fields_uid = UidMap.find s_uid context.struct_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uid fields_uid)) then
        raise (Context.ContextError "Struct field is already used ")
      else ()
    in
    let context, field_uid = add_ident context field_name in
    let field_data : Context.uid_data = { uid_typ = Some field_typ; uid_sort = IdStructField } in
    let s_decl = UidSet.add field_uid (UidMap.find s_uid context.struct_decl) in
    {
      context with
      struct_decl = UidMap.add s_uid s_decl context.struct_decl;
      uid_data = UidMap.add field_uid field_data context.uid_data;
    }
  in

  List.fold_left process_field context s.struct_decl_fields

(** Process an enum declaration, checking for wellformedness of types *)
let process_enum_decl (context : Context.context) (e : Ast.enum_decl) : Context.context =
  let e_name = Pos.unmark e.enum_decl_name in
  let _ =
    if List.length (get_ident_sort context e_name) > 0 then
      raise (Context.ContextError ("Enum name " ^ e_name ^ " is already used"))
    else ()
  in
  let context, e_uid = add_ident context e_name in
  let e_data : Context.uid_data = { uid_typ = None; uid_sort = IdEnumName } in
  let context =
    {
      context with
      struct_decl = UidMap.add e_uid UidSet.empty context.enum_decl;
      uid_data = UidMap.add e_uid e_data context.uid_data;
    }
  in

  let process_enum_case (context : Context.context) (case : Ast.enum_decl_case Pos.marked) :
      Context.context =
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
          with Context.UnknownTyp cons -> Errors.type_unknown_error (Pos.get_position t) cons )
    in
    (* For now, there can't be two enum cases with the same name *)
    let _ =
      if List.length (find_uid_list case_name context) > 0 then
        raise (Context.ContextError ("EnumCase name " ^ case_name ^ " is already used"))
      else ()
    in
    let context, case_uid = add_ident context case_name in
    let case_data : Context.uid_data = { uid_typ = case_typ; uid_sort = IdEnumCase } in
    let e_decl = UidSet.add case_uid (UidMap.find e_uid context.enum_decl) in
    {
      context with
      enum_decl = UidMap.add e_uid e_decl context.enum_decl;
      uid_data = UidMap.add case_uid case_data context.uid_data;
    }
  in

  List.fold_left process_enum_case context e.enum_decl_cases

(** Process a scope declaration, checking for wellformedness of types *)
let process_scope_decl (context : Context.context) (s : Ast.scope_decl) : Context.context =
  let s_name = Pos.unmark s.scope_decl_name in
  let _ =
    if List.length (get_ident_sort context s_name) > 0 then
      raise (Context.ContextError ("Scope name " ^ s_name ^ " is already used"))
    else ()
  in
  let context, s_uid = add_ident context s_name in
  let s_data : Context.uid_data = { uid_typ = None; uid_sort = IdScopeName } in
  let context =
    {
      context with
      scope_decl = UidMap.add s_uid UidSet.empty context.scope_decl;
      uid_data = UidMap.add s_uid s_data context.uid_data;
    }
  in

  let process_context_data (context : Context.context) (item : Ast.scope_decl_context_data) :
      Context.context =
    let item_name = Pos.unmark item.scope_decl_context_item_name in
    let item_typ = Pos.unmark item.scope_decl_context_item_typ in
    let _ =
      try check_typ_well_formed context item_typ
      with Context.UnknownTyp cons ->
        Errors.type_unknown_error (Pos.get_position item.scope_decl_context_item_typ) cons
    in
    (* An ident can be used for different scopes, but not for the same one *)
    let possible_uids = UidSet.of_list (find_uid_list item_name context) in
    let scope_uids = UidMap.find s_uid context.scope_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uids scope_uids)) then
        raise (Context.ContextError "Scope context item name is already used ")
      else ()
    in
    let context, item_uid = add_ident context item_name in
    let item_data : Context.uid_data = { uid_typ = Some item_typ; uid_sort = IdScopeContextItem } in
    let s_decl = UidSet.add item_uid (UidMap.find s_uid context.scope_decl) in
    {
      context with
      scope_decl = UidMap.add s_uid s_decl context.scope_decl;
      uid_data = UidMap.add item_uid item_data context.uid_data;
    }
  in

  let process_context_scope (context : Context.context) (item : Ast.scope_decl_context_scope) :
      Context.context =
    let item_name = Pos.unmark item.scope_decl_context_scope_name in
    let item_sub_scope = Pos.unmark item.scope_decl_context_scope_sub_scope in
    let item_condition =
      match item.scope_decl_context_scope_condition with
      | None -> None
      | Some expr -> Some (Pos.unmark expr)
    in
    (* Check that the subscope exists *)
    let _ =
      if
        not (List.exists (fun s -> s = Context.IdScopeName) (get_ident_sort context item_sub_scope))
      then raise (Context.ContextError ("Scope " ^ item_sub_scope ^ " does not exist"))
      else ()
    in

    (* An ident can be used for different scopes, but not for the same one *)
    let possible_uids = UidSet.of_list (find_uid_list item_name context) in
    let scope_uids = UidMap.find s_uid context.scope_decl in
    let _ =
      if not (UidSet.is_empty (UidSet.inter possible_uids scope_uids)) then
        raise (Context.ContextError "Scope context item name is already used ")
      else ()
    in
    let context, item_uid = add_ident context item_name in
    let item_data : Context.uid_data = { uid_typ = None; uid_sort = IdScopeContextScope } in
    let include_data : Context.scope_include_data =
      { condition = item_condition; sub_scope = item_sub_scope }
    in
    let s_decl = UidSet.add item_uid (UidMap.find s_uid context.scope_decl) in
    {
      context with
      scope_decl = UidMap.add s_uid s_decl context.scope_decl;
      uid_data = UidMap.add item_uid item_data context.uid_data;
      scope_include_data = UidMap.add item_uid include_data context.scope_include_data;
    }
  in

  let process_context_item (context : Context.context)
      (item : Ast.scope_decl_context_item Pos.marked) : Context.context =
    match Pos.unmark item with
    | ContextData data -> process_context_data context data
    | ContextScope scope -> process_context_scope context scope
  in

  List.fold_left process_context_item context s.scope_decl_context

(** Check wellformedness of qidents TODO : make it recursive to change scope *)
let check_qident (context : Context.context) (scope_uid : Context.uid) (qid : Ast.qident) :
    Context.ident * Context.typ =
  let rec remove_marks = function [] -> [] | x :: xx -> Pos.unmark x :: remove_marks xx in

  let qid = remove_marks qid.qident_path in

  let rec check_inclusion parent_uid = function
    | [] -> parent_uid
    | ident :: id_list -> (
        match parent_uid with
        | None -> (
            (* base case, we need to check that the ident is in the scope *)
            let uids = UidSet.of_list (find_uid_list ident context) in
            let scope_params = UidMap.find scope_uid context.scope_decl in
            match UidSet.choose_opt (UidSet.inter uids scope_params) with
            | None -> raise (Context.ContextError "...")
            | Some uid -> check_inclusion (Some uid) id_list )
        | Some parent_uid -> Some parent_uid )
  in

  let uid =
    match check_inclusion None qid with
    | None -> raise (Context.ContextError "???")
    | Some uid -> uid
  in
  let typ =
    match (UidMap.find uid context.uid_data).uid_typ with
    | None -> raise (Context.ContextError "qident should have a type")
    | Some t -> t
  in
  (String.concat "." qid, typ)

(** Check the ident with check_qident, and add it to the current scope *)
let uid_of_ident (context : Context.context) (scope : Ir.scope) (qident : Ast.qident) :
    Uid.t * Ir.scope =
  let str, typ = check_qident context scope.scope_uid qident in
  match IdentMap.find_opt str scope.var_to_uid with
  | Some uid -> (uid, scope) (* TODO : do i need to check that types match here ? *)
  | None ->
      let uid = Uid.fresh () in
      ( uid,
        {
          scope with
          var_to_uid = IdentMap.add str uid scope.var_to_uid;
          uid_to_var = UidMap.add uid str scope.uid_to_var;
          uid_typ = UidMap.add uid typ scope.uid_typ;
        } )

(** Transform expressions from ast to ir *)
let process_expression (_context : Context.context) (_scope : Ir.scope)
    (_expr : Ast.expression Pos.marked) : Ir.expression * Ir.scope =
  (* This function returns the scope because it might encounter a qident that has not been processed
     yet *)
  assert false

and process_match_cases (_context : Context.context) (_scope : Ir.scope) (_cases : Ast.match_cases)
    : Ir.match_cases =
  assert false

and process_match_case (_context : Context.context) (_scope : Ir.scope) (_case : Ast.match_case) :
    Ir.match_case =
  assert false

(** Process a rule *)
let process_rule (rule : Ast.rule) (context : Context.context) (scope : Ir.scope)
    (precondition : Ir.expression option) : Ir.scope =
  let uid, scope = uid_of_ident context scope (Pos.unmark rule.rule_name) in
  let condition, scope =
    match rule.rule_condition with
    | Some condition -> process_expression context scope condition
    | None -> ((Ir.Literal (Ast.BoolLiteral true), Pos.no_pos), scope)
  in
  let condition =
    match precondition with
    | Some pre -> (Ir.Binop ((Ir.And, Pos.no_pos), condition, pre), Pos.no_pos)
    | None -> condition
  in
  let rule : Ir.rule =
    {
      rule_parameter = rule.rule_parameter;
      rule_condition = condition;
      rule_consequence = rule.rule_consequence;
    }
  in

  { scope with scope_rules = Uid.map_add_list uid rule scope.scope_rules }

(** Process a definition *)
let process_def (def : Ast.definition) (context : Context.context) (scope : Ir.scope)
    (precondition : Ir.expression option) : Ir.scope =
  let uid, scope = uid_of_ident context scope (Pos.unmark def.definition_name) in

  let condition, scope =
    match def.definition_condition with
    | Some condition -> process_expression context scope condition
    | None -> ((Ir.Literal (Ast.BoolLiteral true), Pos.no_pos), scope)
  in
  let condition =
    match precondition with
    | Some pre -> (Ir.Binop ((Ir.And, Pos.no_pos), condition, pre), Pos.no_pos)
    | None -> condition
  in

  let def_expr, scope = process_expression context scope def.definition_expr in

  let def : Ir.definition =
    {
      definition_parameter = def.definition_parameter;
      definition_condition = condition;
      definition_expr = def_expr;
    }
  in

  { scope with scope_defs = Uid.map_add_list uid def scope.scope_defs }

(** Proccess an assertion *)
let process_assert (assertion : Ast.assertion) (context : Context.context) (scope : Ir.scope)
    (precondition : Ir.expression option) : Ir.scope =
  let condition, scope =
    match assertion.assertion_condition with
    | Some condition -> process_expression context scope condition
    | None -> ((Ir.Literal (Ast.BoolLiteral true), Pos.no_pos), scope)
  in
  let condition =
    match precondition with
    | Some pre -> (Ir.Binop ((Ir.And, Pos.no_pos), condition, pre), Pos.no_pos)
    | None -> condition
  in

  let content, scope = process_expression context scope assertion.assertion_content in

  let assertion =
    (Ir.IfThenElse (condition, content, (Ir.Literal (Ast.BoolLiteral true), Pos.no_pos)), Pos.no_pos)
  in

  { scope with scope_assertions = assertion :: scope.scope_assertions }

(** Process a meta-assertion *)
let process_meta_assert (_meta_assert : Ast.meta_assertion) (_context : Context.context)
    (_scope : Ir.scope) (_ : Ir.expression option) : Ir.scope =
  assert false

(** Process an ast scope use item *)
let process_scope_use_item (context : Context.context) (precondition : Ir.expression option)
    (scope : Ir.scope) (item : Ast.scope_use_item) : Ir.scope =
  ( match item with
  | Ast.Rule rule -> process_rule rule
  | Ast.Definition def -> process_def def
  | Ast.Assertion assertion -> process_assert assertion
  | Ast.MetaAssertion meta_assert -> process_meta_assert meta_assert )
    context scope precondition

(** Process a scope use *)
let process_scope_use (context : Context.context) (prgm : Ir.program)
    ({ scope_use_name; scope_use_condition; scope_use_items } : Ast.scope_use) : Ir.program =
  let name = Pos.unmark scope_use_name in
  let scope_uid =
    match
      List.filter
        (fun uid ->
          match UidMap.find_opt uid context.scope_decl with Some _ -> true | None -> false)
        (find_uid_list name context)
    with
    | [] -> raise (Context.ContextError ("Unknown scope name " ^ name))
    | [ uid ] -> uid
    | _ -> raise (Context.ContextError "Shouldnt happen")
  in
  let scope =
    match UidMap.find_opt scope_uid prgm with
    | Some s -> s
    | None ->
        {
          scope_uid;
          uid_to_var = UidMap.empty;
          var_to_uid = IdentMap.empty;
          uid_typ = UidMap.empty;
          scope_rules = UidMap.empty;
          scope_defs = UidMap.empty;
          scope_assertions = [];
          scope_meta_assertions = UidMap.empty;
        }
  in

  let scope, precondition =
    match scope_use_condition with
    | None -> (scope, None)
    | Some x ->
        let expr, scope = process_expression context scope x in
        (scope, Some expr)
  in

  let scope =
    List.fold_left
      (process_scope_use_item context precondition)
      scope
      (List.map Pos.unmark scope_use_items)
  in

  UidMap.add scope_uid scope prgm

type prgm_with_context = Context.context * Ir.program

(** First pass *)
let firstpass (prgm : Ast.program) : prgm_with_context =
  let context : Context.context =
    {
      ident_to_uid = IdentMap.empty;
      struct_decl = UidMap.empty;
      enum_decl = UidMap.empty;
      enum_cases = UidMap.empty;
      scope_decl = UidMap.empty;
      scope_include_data = UidMap.empty;
      uid_data = UidMap.empty;
    }
  in

  let prgm_use = UidMap.empty in

  let process_decl ((context, prgm_use) : prgm_with_context) (decl : Ast.code_item) :
      prgm_with_context =
    match decl with
    | ScopeUse s_use -> (context, process_scope_use context prgm_use s_use)
    | ScopeDecl decl -> (process_scope_decl context decl, prgm_use)
    | StructDecl decl -> (process_struct_decl context decl, prgm_use)
    | EnumDecl decl -> (process_enum_decl context decl, prgm_use)
  in

  let process_program_item ((context, prgm) : prgm_with_context) :
      Ast.program_item -> prgm_with_context = function
    | Ast.MetadataBlock (block, _) | Ast.CodeBlock (block, _) ->
        List.fold_left
          (fun prgm_w_context item -> process_decl prgm_w_context (Pos.unmark item))
          (context, prgm) block
    | _ -> (context, prgm)
  in

  List.fold_left process_program_item (context, prgm_use) prgm.program_items
