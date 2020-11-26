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

module Pos = Utils.Pos
module Errors = Utils.Errors

(** The optional argument subdef allows to choose between differents uids in case the expression is
    a redefinition of a subvariable *)

let translate_binop (op : Ast.binop) : Dcalc.Ast.binop =
  match op with
  | And -> And
  | Or -> Or
  | Add -> Add
  | Sub -> Sub
  | Mult -> Mult
  | Div -> Div
  | Lt -> Lt
  | Lte -> Lte
  | Gt -> Gt
  | Gte -> Gte
  | Eq -> Eq
  | Neq -> Neq

let translate_unop (op : Ast.unop) : Dcalc.Ast.unop = match op with Not -> Not | Minus -> Minus

let rec translate_expr (scope : Scopelang.Ast.ScopeName.t)
    (def_key : Desugared.Ast.ScopeDef.t option) (ctxt : Name_resolution.context)
    ((expr, pos) : Ast.expression Pos.marked) : Scopelang.Ast.expr Pos.marked =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  let rec_helper = translate_expr scope def_key ctxt in
  match expr with
  | IfThenElse (e_if, e_then, e_else) ->
      (EIfThenElse (rec_helper e_if, rec_helper e_then, rec_helper e_else), pos)
  | Binop (op, e1, e2) ->
      let op_term =
        Pos.same_pos_as (Scopelang.Ast.EOp (Dcalc.Ast.Binop (translate_binop (Pos.unmark op)))) op
      in
      (EApp (op_term, [ rec_helper e1; rec_helper e2 ]), pos)
  | Unop (op, e) ->
      let op_term =
        Pos.same_pos_as (Scopelang.Ast.EOp (Dcalc.Ast.Unop (translate_unop (Pos.unmark op)))) op
      in
      (EApp (op_term, [ rec_helper e ]), pos)
  | Literal l ->
      let untyped_term =
        match l with
        | Number ((Int i, _), _) -> Scopelang.Ast.ELit (Dcalc.Ast.LInt i)
        | Number ((Dec (_i, _f), _), _) -> Name_resolution.raise_unsupported_feature "decimal" pos
        | Bool b -> Scopelang.Ast.ELit (Dcalc.Ast.LBool b)
        | _ -> Name_resolution.raise_unsupported_feature "literal" pos
      in
      (untyped_term, pos)
  | Ident x -> (
      (* first we check whether this is a local var, then we resort to scope-wide variables *)
      match def_key with
      | Some def_key -> (
          let def_ctxt = Desugared.Ast.ScopeDefMap.find def_key scope_ctxt.definitions in
          match Desugared.Ast.IdentMap.find_opt x def_ctxt.var_idmap with
          | None -> (
              match Desugared.Ast.IdentMap.find_opt x scope_ctxt.var_idmap with
              | Some uid -> (Scopelang.Ast.ELocation (ScopeVar (uid, pos)), pos)
              | None ->
                  Name_resolution.raise_unknown_identifier "for a\n   local or scope-wide variable"
                    (x, pos) )
          | Some uid -> (Scopelang.Ast.EVar uid, pos) )
      | None -> (
          match Desugared.Ast.IdentMap.find_opt x scope_ctxt.var_idmap with
          | Some uid -> (Scopelang.Ast.ELocation (ScopeVar (uid, pos)), pos)
          | None -> Name_resolution.raise_unknown_identifier "for a scope-wide variable" (x, pos) )
      )
  | Dotted (e, x) -> (
      (* For now we only accept dotted identifiers of the type y.x where y is a sub-scope *)
      match Pos.unmark e with
      | Ident y ->
          let subscope_uid : Scopelang.Ast.SubScopeName.t =
            Name_resolution.get_subscope_uid scope ctxt (Pos.same_pos_as y e)
          in
          let subscope_real_uid : Scopelang.Ast.ScopeName.t =
            Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
          in
          let subscope_var_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
          ( Scopelang.Ast.ELocation
              (SubScopeVar (subscope_real_uid, (subscope_uid, pos), (subscope_var_uid, pos))),
            pos )
      | _ ->
          Name_resolution.raise_unsupported_feature
            "left hand side of a dotted expression should be an\n\n   identifier" pos )
  | FunCall (f, arg) -> (EApp (rec_helper f, [ rec_helper arg ]), pos)
  | _ -> Name_resolution.raise_unsupported_feature "unsupported expression" pos

(* Translation from the parsed ast to the scope language *)

let merge_conditions (precond : Scopelang.Ast.expr Pos.marked option)
    (cond : Scopelang.Ast.expr Pos.marked option) (default_pos : Pos.t) :
    Scopelang.Ast.expr Pos.marked =
  match (precond, cond) with
  | Some precond, Some cond ->
      let op_term = (Scopelang.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.And), Pos.get_position precond) in
      (Scopelang.Ast.EApp (op_term, [ precond; cond ]), Pos.get_position precond)
  | Some cond, None | None, Some cond -> cond
  | None, None -> (Scopelang.Ast.ELit (Dcalc.Ast.LBool true), default_pos)

let process_default (ctxt : Name_resolution.context) (scope : Scopelang.Ast.ScopeName.t)
    (def_key : Desugared.Ast.ScopeDef.t) (param_uid : Scopelang.Ast.Var.t option)
    (precond : Scopelang.Ast.expr Pos.marked option) (just : Ast.expression Pos.marked option)
    (cons : Ast.expression Pos.marked) : Desugared.Ast.rule =
  let just =
    match just with
    | Some just -> Some (translate_expr scope (Some def_key) ctxt just)
    | None -> None
  in
  let just = merge_conditions precond just (Pos.get_position cons) in
  let cons = translate_expr scope (Some def_key) ctxt cons in
  {
    just;
    cons;
    parameter =
      (let def_key_typ = Name_resolution.get_def_typ ctxt def_key in
       match (Pos.unmark def_key_typ, param_uid) with
       | Dcalc.Ast.TArrow (t_in, _), Some param_uid -> Some (param_uid, t_in)
       | Dcalc.Ast.TArrow _, None ->
           Errors.raise_spanned_error
             "this definition has a function type but the parameter is missing"
             (Pos.get_position cons)
       | _, Some _ ->
           Errors.raise_spanned_error
             "this definition has a parameter but its type is not a function"
             (Pos.get_position cons)
       | _ -> None);
    parent_rule =
      None (* for now we don't have a priority mechanism in the syntax but it will happen soon *);
  }

(* Process a definition *)
let process_def (precond : Scopelang.Ast.expr Pos.marked option)
    (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (def : Ast.definition) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope = Scopelang.Ast.ScopeMap.find scope_uid prgm in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let default_pos = Pos.get_position def.definition_expr in
  let param_uid (def_uid : Desugared.Ast.ScopeDef.t) : Scopelang.Ast.Var.t option =
    match def.definition_parameter with
    | None -> None
    | Some param ->
        let def_ctxt = Desugared.Ast.ScopeDefMap.find def_uid scope_ctxt.definitions in
        Some (Desugared.Ast.IdentMap.find (Pos.unmark param) def_ctxt.var_idmap)
  in
  let def_key =
    match Pos.unmark def.definition_name with
    | [ x ] ->
        let x_uid = Name_resolution.get_var_uid scope_uid ctxt x in
        Desugared.Ast.ScopeDef.Var x_uid
    | [ y; x ] ->
        let subscope_uid : Scopelang.Ast.SubScopeName.t =
          Name_resolution.get_subscope_uid scope_uid ctxt y
        in
        let subscope_real_uid : Scopelang.Ast.ScopeName.t =
          Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
        in
        let x_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
        Desugared.Ast.ScopeDef.SubScopeVar (subscope_uid, x_uid)
    | _ -> Errors.raise_spanned_error "Structs are not handled yet" default_pos
  in
  let scope_updated =
    let x_def, x_type =
      match Desugared.Ast.ScopeDefMap.find_opt def_key scope.scope_defs with
      | Some def -> def
      | None -> (Desugared.Ast.RuleMap.empty, Name_resolution.get_def_typ ctxt def_key)
    in
    let rule_name =
      Desugared.Ast.RuleName.fresh
        (Pos.map_under_mark
           (fun qident -> String.concat "." (List.map (fun i -> Pos.unmark i) qident))
           def.definition_name)
    in
    let x_def =
      Desugared.Ast.RuleMap.add rule_name
        (process_default ctxt scope_uid def_key (param_uid def_key) precond def.definition_condition
           def.definition_expr)
        x_def
    in
    {
      scope with
      scope_defs = Desugared.Ast.ScopeDefMap.add def_key (x_def, x_type) scope.scope_defs;
    }
  in
  Scopelang.Ast.ScopeMap.add scope_uid scope_updated prgm

(** Process a rule from the surface language *)
let process_rule (precond : Scopelang.Ast.expr Pos.marked option)
    (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (rule : Ast.rule) : Desugared.Ast.program =
  let consequence_expr = Ast.Literal (Ast.Bool (Pos.unmark rule.rule_consequence)) in
  let def =
    {
      Ast.definition_name = rule.rule_name;
      Ast.definition_parameter = rule.rule_parameter;
      Ast.definition_condition = rule.rule_condition;
      Ast.definition_expr = (consequence_expr, Pos.get_position rule.rule_consequence);
    }
  in
  process_def precond scope ctxt prgm def

let process_scope_use_item (precond : Ast.expression Pos.marked option)
    (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (item : Ast.scope_use_item Pos.marked) : Desugared.Ast.program =
  let precond = Option.map (translate_expr scope None ctxt) precond in
  match Pos.unmark item with
  | Ast.Rule rule -> process_rule precond scope ctxt prgm rule
  | Ast.Definition def -> process_def precond scope ctxt prgm def
  | _ -> prgm

let process_scope_use (ctxt : Name_resolution.context) (prgm : Desugared.Ast.program)
    (use : Ast.scope_use) : Desugared.Ast.program =
  let name = fst use.scope_use_name in
  let scope_uid = Desugared.Ast.IdentMap.find name ctxt.scope_idmap in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let scope_vars =
    List.fold_left
      (fun acc (_, var) -> Scopelang.Ast.ScopeVarSet.add var acc)
      Scopelang.Ast.ScopeVarSet.empty
      (Desugared.Ast.IdentMap.bindings scope_ctxt.var_idmap)
  in
  (* Make sure the scope exists *)
  let prgm =
    match Scopelang.Ast.ScopeMap.find_opt scope_uid prgm with
    | Some _ -> prgm
    | None ->
        Scopelang.Ast.ScopeMap.add scope_uid
          (Desugared.Ast.empty_scope scope_uid scope_vars scope_ctxt.sub_scopes)
          prgm
  in
  let precond = use.scope_use_condition in
  List.fold_left (process_scope_use_item precond scope_uid ctxt) prgm use.scope_use_items

(** Scopes processing *)
let desugar_program (ctxt : Name_resolution.context) (prgm : Ast.program) : Desugared.Ast.program =
  let empty_prgm = Scopelang.Ast.ScopeMap.empty in
  let processer_article_item (prgm : Desugared.Ast.program) (item : Ast.law_article_item) :
      Desugared.Ast.program =
    match item with
    | CodeBlock (block, _) ->
        List.fold_left
          (fun prgm item ->
            match Pos.unmark item with
            | Ast.ScopeUse use -> process_scope_use ctxt prgm use
            | _ -> prgm)
          prgm block
    | _ -> prgm
  in
  let rec processer_structure (prgm : Desugared.Ast.program) (item : Ast.law_structure) :
      Desugared.Ast.program =
    match item with
    | LawHeading (_, children) ->
        List.fold_left (fun prgm child -> processer_structure prgm child) prgm children
    | LawArticle (_, children) ->
        List.fold_left (fun prgm child -> processer_article_item prgm child) prgm children
    | MetadataBlock (b, c) -> processer_article_item prgm (CodeBlock (b, c))
    | IntermediateText _ -> prgm
  in

  let processer_item (prgm : Desugared.Ast.program) (item : Ast.program_item) :
      Desugared.Ast.program =
    match item with LawStructure s -> processer_structure prgm s
  in

  List.fold_left processer_item empty_prgm prgm.program_items
