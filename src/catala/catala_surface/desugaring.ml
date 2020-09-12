(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr> Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Catala_ast
open Lambda_ast

(** The optional argument subdef allows to choose between differents uids in case the expression is
    a redefinition of a subvariable *)
let rec expr_to_lambda (scope : Uid.Scope.t) (def_key : Uid.ScopeDef.t option)
    (ctxt : Name_resolution.context) ((expr, pos) : Catala_ast.expression Pos.marked) :
    Lambda_ast.term =
  let scope_ctxt = Uid.ScopeMap.find scope ctxt.scopes in
  let rec_helper = expr_to_lambda scope def_key ctxt in
  match expr with
  | IfThenElse (e_if, e_then, e_else) ->
      ((EIfThenElse (rec_helper e_if, rec_helper e_then, rec_helper e_else), pos), TDummy)
  | Binop (op, e1, e2) ->
      let op_term = (Pos.same_pos_as (EOp (Binop (Pos.unmark op))) op, TDummy) in
      ((EApp (op_term, [ rec_helper e1; rec_helper e2 ]), pos), TDummy)
  | Unop (op, e) ->
      let op_term = (Pos.same_pos_as (EOp (Unop (Pos.unmark op))) op, TDummy) in
      ((EApp (op_term, [ rec_helper e ]), pos), TDummy)
  | Literal l ->
      let untyped_term =
        match l with
        | Number ((Int i, _), _) -> EInt i
        | Number ((Dec (i, f), _), _) -> EDec (i, f)
        | Bool b -> EBool b
        | _ -> Name_resolution.raise_unsupported_feature "literal" pos
      in
      ((untyped_term, pos), TDummy)
  | Ident x -> (
      (* first we check whether this is a local var, then we resort to scope-wide variables *)
      match def_key with
      | Some def_key -> (
          let def_ctxt = Uid.ScopeDefMap.find def_key scope_ctxt.definitions in
          match Uid.IdentMap.find_opt x def_ctxt.var_idmap with
          | None -> (
              match Uid.IdentMap.find_opt x scope_ctxt.var_idmap with
              | Some uid -> ((EVar (None, uid), pos), TDummy)
              | None ->
                  Name_resolution.raise_unknown_identifier "for a local or scope-wide variable"
                    (x, pos) )
          | Some uid -> ((ELocalVar uid, pos), TDummy) )
      | None -> (
          match Uid.IdentMap.find_opt x scope_ctxt.var_idmap with
          | Some uid -> ((EVar (None, uid), pos), TDummy)
          | None -> Name_resolution.raise_unknown_identifier "for a scope-wide variable" (x, pos) )
      )
  | Dotted (e, x) -> (
      (* For now we only accept dotted identifiers of the type y.x where y is a sub-scope *)
      match Pos.unmark e with
      | Ident y ->
          let subscope_uid : Uid.SubScope.t =
            Name_resolution.get_subscope_uid scope ctxt (Pos.same_pos_as y e)
          in
          let subscope_real_uid : Uid.Scope.t =
            Uid.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
          in
          let subscope_var_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
          ((EVar (Some subscope_uid, subscope_var_uid), pos), TDummy)
      | _ ->
          Name_resolution.raise_unsupported_feature
            "left hand side of a dotted expression should be an identifier" pos )
  | FunCall (f, arg) -> ((EApp (rec_helper f, [ rec_helper arg ]), pos), TDummy)
  | _ -> Name_resolution.raise_unsupported_feature "unsupported expression" pos

(* Translation from the parsed ast to the scope language *)

let merge_conditions (precond : Lambda_ast.term option) (cond : Lambda_ast.term option)
    (default_pos : Pos.t) : Lambda_ast.term =
  match (precond, cond) with
  | Some precond, Some cond ->
      let op_term = ((EOp (Binop And), Pos.get_position (fst precond)), TDummy) in
      ((EApp (op_term, [ precond; cond ]), Pos.get_position (fst precond)), TDummy)
  | Some cond, None | None, Some cond -> cond
  | None, None -> ((EBool true, default_pos), TBool)

let process_default (ctxt : Name_resolution.context) (scope : Uid.Scope.t)
    (def_key : Uid.ScopeDef.t) (def : Lambda_ast.default_term) (param_uid : Uid.LocalVar.t option)
    (precond : Lambda_ast.term option) (just : Catala_ast.expression Pos.marked option)
    (body : Catala_ast.expression Pos.marked) : Lambda_ast.default_term =
  let just =
    match just with
    | Some cond -> Some (expr_to_lambda scope (Some def_key) ctxt cond)
    | None -> None
  in
  let condition = merge_conditions precond just (Pos.get_position body) in
  let body = expr_to_lambda scope (Some def_key) ctxt body in
  (* if there's a parameter, we have to wrap the justifiction and the body in a func *)
  let condition, body =
    match param_uid with
    | None -> (condition, body)
    | Some param_uid ->
        ( ((EFun ([ (param_uid, TDummy) ], condition), Pos.get_position (fst condition)), TDummy),
          ((EFun ([ (param_uid, TDummy) ], body), Pos.get_position (fst body)), TDummy) )
  in
  Lambda_ast.add_default condition body def

(* Process a definition *)
let process_def (precond : Lambda_ast.term option) (scope_uid : Uid.Scope.t)
    (ctxt : Name_resolution.context) (prgm : Scope_ast.program) (def : Catala_ast.definition) :
    Scope_ast.program =
  let scope : Scope_ast.scope = Uid.ScopeMap.find scope_uid prgm in
  let scope_ctxt = Uid.ScopeMap.find scope_uid ctxt.scopes in
  let default_pos = Pos.get_position def.definition_expr in
  let param_uid (def_uid : Uid.ScopeDef.t) : Uid.LocalVar.t option =
    match def.definition_parameter with
    | None -> None
    | Some param ->
        let def_ctxt = Uid.ScopeDefMap.find def_uid scope_ctxt.definitions in
        Some (Uid.IdentMap.find (Pos.unmark param) def_ctxt.var_idmap)
  in
  let def_key =
    match Pos.unmark def.definition_name with
    | [ x ] ->
        let x_uid = Name_resolution.get_var_uid scope_uid ctxt x in
        Uid.ScopeDef.Var x_uid
    | [ y; x ] ->
        let subscope_uid : Uid.SubScope.t = Name_resolution.get_subscope_uid scope_uid ctxt y in
        let subscope_real_uid : Uid.Scope.t =
          Uid.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
        in
        let x_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
        Uid.ScopeDef.SubScopeVar (subscope_uid, x_uid)
    | _ -> Errors.raise_spanned_error "Structs are not handled yet" default_pos
  in
  let scope_updated =
    let x_def =
      match Uid.ScopeDefMap.find_opt def_key scope.scope_defs with
      | Some def -> def
      | None ->
          let typ = Name_resolution.get_def_typ ctxt def_key in
          Scope_ast.empty_def default_pos typ
    in
    let x_def =
      Lambda_ast.map_untype
        (fun t ->
          match t with
          | EDefault default ->
              EDefault
                (process_default ctxt scope_uid def_key default (param_uid def_key) precond
                   def.definition_condition def.definition_expr)
          | _ -> assert false
          (* should not happen *))
        x_def
    in
    { scope with scope_defs = Uid.ScopeDefMap.add def_key x_def scope.scope_defs }
  in
  Uid.ScopeMap.add scope_uid scope_updated prgm

(** Process a rule from the surface language *)
let process_rule (precond : Lambda_ast.term option) (scope : Uid.Scope.t)
    (ctxt : Name_resolution.context) (prgm : Scope_ast.program) (rule : Catala_ast.rule) :
    Scope_ast.program =
  let consequence_expr = Catala_ast.Literal (Catala_ast.Bool (Pos.unmark rule.rule_consequence)) in
  let def =
    {
      definition_name = rule.rule_name;
      definition_parameter = rule.rule_parameter;
      definition_condition = rule.rule_condition;
      definition_expr = (consequence_expr, Pos.get_position rule.rule_consequence);
    }
  in
  process_def precond scope ctxt prgm def

let process_scope_use_item (cond : Lambda_ast.term option) (scope : Uid.Scope.t)
    (ctxt : Name_resolution.context) (prgm : Scope_ast.program)
    (item : Catala_ast.scope_use_item Pos.marked) : Scope_ast.program =
  match Pos.unmark item with
  | Catala_ast.Rule rule -> process_rule cond scope ctxt prgm rule
  | Catala_ast.Definition def -> process_def cond scope ctxt prgm def
  | _ -> prgm

let process_scope_use (ctxt : Name_resolution.context) (prgm : Scope_ast.program)
    (use : Catala_ast.scope_use) : Scope_ast.program =
  let name = fst use.scope_use_name in
  let scope_uid = Uid.IdentMap.find name ctxt.scope_idmap in
  (* Make sure the scope exists *)
  let prgm =
    match Uid.ScopeMap.find_opt scope_uid prgm with
    | Some _ -> prgm
    | None -> Uid.ScopeMap.add scope_uid (Scope_ast.empty_scope scope_uid) prgm
  in
  let cond =
    match use.scope_use_condition with
    | Some expr ->
        let untyped_term = expr_to_lambda scope_uid None ctxt expr in
        Some untyped_term
    | None -> None
  in
  List.fold_left (process_scope_use_item cond scope_uid ctxt) prgm use.scope_use_items

(** Scopes processing *)
let translate_program_to_scope (ctxt : Name_resolution.context) (prgm : Catala_ast.program) :
    Scope_ast.program =
  let empty_prgm = Uid.ScopeMap.empty in
  let processer (prgm : Scope_ast.program) (item : Catala_ast.program_item) : Scope_ast.program =
    match item with
    | CodeBlock (block, _) | MetadataBlock (block, _) ->
        List.fold_left
          (fun prgm item ->
            match Pos.unmark item with ScopeUse use -> process_scope_use ctxt prgm use | _ -> prgm)
          prgm block
    | _ -> prgm
  in
  List.fold_left processer empty_prgm prgm.program_items
