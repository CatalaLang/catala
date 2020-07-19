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

open Ast
open Lambda

let subscope_ident (y : string) (x : string) : string = y ^ "::" ^ x

(** The optional argument subdef allows to choose between differents uids in case the expression is
    a redefinition of a subvariable *)
let rec expr_to_lambda ?(subdef : uid option) (scope : Context.uid) (ctxt : Context.context)
    ((expr, pos) : Ast.expression Pos.marked) : Lambda.term =
  let rec_helper = expr_to_lambda ?subdef scope ctxt in
  match expr with
  | IfThenElse (e_if, e_then, e_else) ->
      ((EIfThenElse (rec_helper e_if, rec_helper e_then, rec_helper e_else), pos), None)
  | Binop (op, e1, e2) ->
      let op_term = (Pos.same_pos_as (EOp (Binop (Pos.unmark op))) op, None) in
      let op_1 = ((EApp (op_term, rec_helper e1), pos), None) in
      ((EApp (op_1, rec_helper e2), pos), None)
  | Unop (op, e) ->
      let op_term = (Pos.same_pos_as (EOp (Unop (Pos.unmark op))) op, None) in
      ((EApp (op_term, rec_helper e), pos), None)
  | Literal l -> ((ELiteral l, pos), None)
  | Ident x -> (
      match Context.get_var_uid scope ctxt x with
      | None -> assert false
      | Some uid -> ((EVar uid, pos), None) )
  | Dotted (e, x) -> (
      (* For now we only accept dotted identifiers of the type y.x where y is a sub-scope *)
      match Pos.unmark e with
      | Ident y -> (
          let sub_uid =
            match Context.get_subscope_uid scope ctxt y with
            | None -> assert false
            | Some uid -> uid
          in
          match subdef with
          | None -> (
              (* No redefinition : take the uid from the current scope *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) )
          | Some uid when uid <> sub_uid -> (
              (* Redefinition of a var from another scope : uid from the current scope *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) )
          | Some sub_uid -> (
              (* Redefinition of a var from the same scope, uid from the subscope *)
              match Context.get_var_uid sub_uid ctxt (Pos.unmark x) with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) ) )
      | _ -> assert false )
  | _ -> assert false

(** Checks that a term is well typed and annotate it *)
let rec typing (ctxt : Context.context) (((t, pos), _) : Lambda.term) : Lambda.term * Lambda.typ =
  match t with
  | EVar uid ->
      let typ = match Context.get_uid_typ ctxt uid with None -> assert false | Some typ -> typ in
      let term = ((EVar uid, pos), Some typ) in
      (term, typ)
  | EFun (binding, body) ->
      (* Note that given the context formation process, the binder will already be present in the
         context (since we are working with uids), however it's added there for the sake of clarity *)
      let uid, arg_typ = binding in
      let uid_data : Context.uid_data = { uid_typ = arg_typ; uid_sort = Context.IdBinder } in
      let body, ret_typ = typing { ctxt with data = Uid.UidMap.add uid uid_data ctxt.data } body in
      let fun_typ = TArrow (arg_typ, ret_typ) in
      (((EFun (binding, body), pos), Some fun_typ), fun_typ)
  | EApp (t1, t2) -> (
      let t1, typ1 = typing ctxt t1 in
      let t2, typ2 = typing ctxt t2 in
      match typ1 with
      | TArrow (arg_typ, ret_typ) ->
          if arg_typ <> typ2 then assert false else (((EApp (t1, t2), pos), Some ret_typ), ret_typ)
      | TBool | TInt | TDummy -> assert false )
  | EIfThenElse (t_if, t_then, t_else) ->
      let t_if, typ_if = typing ctxt t_if in
      let t_then, typ_then = typing ctxt t_then in
      let t_else, typ_else = typing ctxt t_else in
      if typ_if <> TBool then assert false
      else if typ_then <> typ_else then assert false
      else (((EIfThenElse (t_if, t_then, t_else), pos), Some typ_then), typ_then)
  | ELiteral l ->
      let typ = match l with Number _ | MoneyAmount _ | Date _ -> TInt | Bool _ -> TBool in
      (((t, pos), Some typ), typ)
  | EOp op ->
      let typ =
        match op with
        | Binop binop -> (
            match binop with
            | And | Or -> TArrow (TBool, TArrow (TBool, TBool))
            | Add | Sub | Mult | Div -> TArrow (TInt, TArrow (TInt, TInt))
            | Lt | Lte | Gt | Gte | Eq | Neq -> TArrow (TInt, TArrow (TInt, TBool)) )
        | Unop Minus -> TArrow (TInt, TInt)
        | Unop Not -> TArrow (TBool, TBool)
      in
      (((t, pos), Some typ), typ)

(* Translation from the parsed ast to the scope language *)

let merge_conditions (precond : Lambda.term option) (cond : Lambda.term option) : Lambda.term =
  match (precond, cond) with
  | Some precond, Some cond ->
      let op_term = ((EOp (Binop And), Pos.no_pos), None) in
      let term = ((EApp (op_term, precond), Pos.no_pos), None) in
      ((EApp (term, cond), Pos.no_pos), None)
  | Some cond, None | None, Some cond -> cond
  | None, None -> ((ELiteral (Ast.Bool true), Pos.no_pos), Some TBool)

(** Process a rule from the surface language *)
let process_rule (precond : Lambda.term option) (scope : uid) (ctxt : Context.context)
    (prgm : Scope.program) (rule : Ast.rule) : Scope.program =
  (* For now we rule out functions *)
  let () = match rule.rule_parameter with Some _ -> assert false | None -> () in
  let consequence_term = ((ELiteral (Ast.Bool rule.rule_consequence), Pos.no_pos), Some TBool) in
  let scope_prgm = UidMap.find scope prgm in
  let scope_prgm =
    match Pos.unmark rule.rule_name with
    | [ x ] ->
        let x_uid =
          match Context.get_var_uid scope ctxt (Pos.unmark x) with
          | None -> assert false
          | Some uid -> uid
        in
        let x_def =
          match UidMap.find_opt x_uid scope_prgm.scope_defs with
          | None -> Lambda.empty_default_term
          | Some def -> def
        in
        (* Process the condition *)
        let cond =
          match rule.rule_condition with
          | Some cond ->
              let cond, typ = typing ctxt (expr_to_lambda scope ctxt cond) in
              if typ = TBool then Some cond else assert false
          | None -> None
        in
        let condition, _ = typing ctxt (merge_conditions precond cond) in
        let x_def = Lambda.add_default condition consequence_term x_def in
        { scope_prgm with scope_defs = UidMap.add x_uid x_def scope_prgm.scope_defs }
    | [ y; x ] ->
        let subscope_uid =
          match Context.get_subscope_uid scope ctxt (Pos.unmark y) with
          | None -> assert false
          | Some uid -> uid
        in
        let x_uid =
          match Context.get_var_uid subscope_uid ctxt (Pos.unmark x) with
          | None -> assert false
          | Some uid -> uid
        in
        let y_subdef =
          match UidMap.find_opt subscope_uid scope_prgm.scope_sub_defs with
          | Some defs -> defs
          | None -> UidMap.empty
        in
        let x_redef =
          match UidMap.find_opt x_uid y_subdef with
          | None -> Lambda.empty_default_term
          | Some redef -> redef
        in
        (* Process the condition *)
        let cond =
          match rule.rule_condition with
          | Some cond ->
              let cond, typ = typing ctxt (expr_to_lambda ~subdef:subscope_uid scope ctxt cond) in
              if typ = TBool then Some cond else assert false
          | None -> None
        in
        let condition, _ = typing ctxt (merge_conditions precond cond) in
        let x_redef = Lambda.add_default condition consequence_term x_redef in
        let y_subdef = UidMap.add x_uid x_redef y_subdef in
        {
          scope_prgm with
          scope_sub_defs = UidMap.add subscope_uid y_subdef scope_prgm.scope_sub_defs;
        }
    | _ -> assert false
  in
  UidMap.add scope scope_prgm prgm

let process_def (precond : Lambda.term option) (scope : uid) (ctxt : Context.context)
    (prgm : Scope.program) (def : Ast.definition) : Scope.program =
  (* For now we rule out functions *)
  let () = match def.definition_parameter with Some _ -> assert false | None -> () in
  (* We first check either it is a variable or a subvariable *)
  let scope_prgm = UidMap.find scope prgm in
  let scope_prgm =
    match Pos.unmark def.definition_name with
    | [ x ] ->
        let x_uid =
          match Context.get_var_uid scope ctxt (Pos.unmark x) with
          | None -> assert false
          | Some uid -> uid
        in
        let x_def =
          match UidMap.find_opt x_uid scope_prgm.scope_defs with
          | None -> Lambda.empty_default_term
          | Some def -> def
        in
        let cond =
          match def.definition_condition with
          | Some cond ->
              let cond, typ = typing ctxt (expr_to_lambda scope ctxt cond) in
              if typ = TBool then Some cond else assert false
          | None -> None
        in
        let condition, _ = typing ctxt (merge_conditions precond cond) in
        let body, _ = typing ctxt (expr_to_lambda scope ctxt def.definition_expr) in
        let x_def = Lambda.add_default condition body x_def in
        { scope_prgm with scope_defs = UidMap.add x_uid x_def scope_prgm.scope_defs }
    | [ y; x ] ->
        let subscope_uid =
          match Context.get_subscope_uid scope ctxt (Pos.unmark y) with
          | None -> assert false
          | Some uid -> uid
        in
        let x_uid =
          match Context.get_var_uid subscope_uid ctxt (Pos.unmark x) with
          | None -> assert false
          | Some uid -> uid
        in
        let y_subdef =
          match UidMap.find_opt subscope_uid scope_prgm.scope_sub_defs with
          | Some defs -> defs
          | None -> UidMap.empty
        in
        let x_redef =
          match UidMap.find_opt x_uid y_subdef with
          | None -> Lambda.empty_default_term
          | Some redef -> redef
        in
        let cond =
          match def.definition_condition with
          | Some cond ->
              let cond, typ = typing ctxt (expr_to_lambda ~subdef:subscope_uid scope ctxt cond) in
              if typ = TBool then Some cond else assert false
          | None -> None
        in
        let condition, _ = typing ctxt (merge_conditions precond cond) in
        let body, _ =
          typing ctxt (expr_to_lambda ~subdef:subscope_uid scope ctxt def.definition_expr)
        in
        let x_redef = Lambda.add_default condition body x_redef in
        let y_subdef = UidMap.add x_uid x_redef y_subdef in
        {
          scope_prgm with
          scope_sub_defs = UidMap.add subscope_uid y_subdef scope_prgm.scope_sub_defs;
        }
    | _ -> assert false
  in
  UidMap.add scope scope_prgm prgm

let process_scope_use_item (cond : Lambda.term option) (scope : uid) (ctxt : Context.context)
    (prgm : Scope.program) (item : Ast.scope_use_item Pos.marked) : Scope.program =
  match Pos.unmark item with
  | Ast.Rule rule -> process_rule cond scope ctxt prgm rule
  | Ast.Definition def -> process_def cond scope ctxt prgm def
  | _ -> prgm

let process_scope_use (ctxt : Context.context) (prgm : Scope.program) (use : Ast.scope_use) :
    Scope.program =
  let name, _ = use.scope_use_name in
  let scope_uid = Context.IdentMap.find name ctxt.scope_id_to_uid in
  (* Make sure the scope exists *)
  let prgm =
    match UidMap.find_opt scope_uid prgm with
    | Some _ -> prgm
    | None -> UidMap.add scope_uid Scope.empty_scope prgm
  in
  let cond =
    match use.scope_use_condition with
    | Some expr ->
        let untyped_term = expr_to_lambda scope_uid ctxt expr in
        let term, typ = typing ctxt untyped_term in
        if typ = TBool then Some term else assert false
    | None -> None
  in
  List.fold_left (process_scope_use_item cond scope_uid ctxt) prgm use.scope_use_items

(** Scopes processing *)
let translate_program_to_scope (ctxt : Context.context) (prgm : Ast.program) : Scope.program =
  let empty_prgm = UidMap.empty in
  let processer (prgm : Scope.program) (item : Ast.program_item) : Scope.program =
    match item with
    | CodeBlock (block, _) | MetadataBlock (block, _) ->
        List.fold_left
          (fun prgm item ->
            match Pos.unmark item with ScopeUse use -> process_scope_use ctxt prgm use | _ -> prgm)
          prgm block
    | _ -> prgm
  in
  List.fold_left processer empty_prgm prgm.program_items
