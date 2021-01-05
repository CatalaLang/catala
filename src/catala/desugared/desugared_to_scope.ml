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

(** Translation from {!module: Desugared.Ast} to {!module: Scopelang.Ast} *)

module Pos = Utils.Pos
module Errors = Utils.Errors
module Cli = Utils.Cli

(** {1 Rule tree construction} *)

type rule_tree = Leaf of Ast.rule | Node of rule_tree list * Ast.rule

(** Transforms a flat list of rules into a tree, taking into account the priorities declared between
    rules

    {e Invariant:} there are no exceptions cycles

    {e Invariant:} there are no dandling exception pointers in the rules *)
let rec def_map_to_tree (def_info : Ast.ScopeDef.t)
    (is_def_func : Scopelang.Ast.typ Pos.marked option) (def : Ast.rule Ast.RuleMap.t) :
    rule_tree list =
  (* first we look to the rules that don't have any exceptions *)
  let has_no_exception (r : Ast.RuleName.t) _ =
    not
      (Ast.RuleMap.exists
         (fun _ r' -> match r'.Ast.exception_to_rule with Some r_ex -> r_ex = r | None -> false)
         def)
  in
  let no_exceptions = Ast.RuleMap.filter has_no_exception def in
  (* Then, for each top-level rule (that has no exceptions), we build a rule tree *)
  (* Among the top-level rules are the base rules that are exceptions to nothing *)
  let base_rules, rules_that_are_exceptions =
    Ast.RuleMap.partition (fun _ r -> Option.is_none r.Ast.exception_to_rule) no_exceptions
  in
  let base_trees : rule_tree Ast.RuleMap.t =
    Ast.RuleMap.map
      (fun r ->
        (* we look at the the eventual rule of which r is an exception *)
        match r.Ast.exception_to_rule with None -> Leaf r | Some _ -> assert false
        (* should not happen *))
      base_rules
  in
  (* Now let's deal with the rules that are exceptions but have no exception. We have to bucket
     these, each bucket containing all the rules that are exception to the same rule *)
  let exception_targets =
    Ast.RuleMap.fold
      (fun _ r acc ->
        match r.Ast.exception_to_rule with
        | None -> assert false (* should not happen *)
        | Some r' -> Ast.RuleMap.add r' () acc)
      rules_that_are_exceptions Ast.RuleMap.empty
  in
  (* In each bucket corresponding to an exception target, we have all the rules that are exceptions
     to the target *)
  let exception_trees =
    Ast.RuleMap.mapi
      (fun r' _ ->
        (* we recursively call the function of a def where we have removed exception edges: this is
           why the function should terminate *)
        let def_rec =
          Ast.RuleMap.map
            (fun r ->
              {
                r with
                Ast.exception_to_rule =
                  ( match r.Ast.exception_to_rule with
                  | None -> None
                  | Some r'' -> if r'' = r' then None else Some r'' );
              })
            def
        in
        let def_rec =
          Ast.RuleMap.filter (fun r _ -> not (Ast.RuleMap.mem r exception_targets)) def_rec
        in
        let exceptions = def_map_to_tree def_info is_def_func def_rec in
        Node (exceptions, Ast.RuleMap.find r' def))
      exception_targets
  in
  List.map snd (Ast.RuleMap.bindings base_trees)
  @ List.map snd (Ast.RuleMap.bindings exception_trees)

(** From the {!type: rule_tree}, builds an {!constructor: Dcalc.Ast.EDefault} expression in the
    scope language. The [~toplevel] parameter is used to know when to place the toplevel binding in
    the case of functions. *)
let rec rule_tree_to_expr ~(toplevel : bool) (def_pos : Pos.t)
    (is_func : Scopelang.Ast.Var.t option) (tree : rule_tree) :
    Scopelang.Ast.expr Pos.marked Bindlib.box =
  let exceptions, rule =
    match tree with Leaf r -> ([], r) | Node (exceptions, r) -> (exceptions, r)
  in
  (* because each rule has its own variable parameter and we want to convert the whole rule tree
     into a function, we need to perform some alpha-renaming of all the expressions *)
  let substitute_parameter (e : Scopelang.Ast.expr Pos.marked Bindlib.box) :
      Scopelang.Ast.expr Pos.marked Bindlib.box =
    match (is_func, rule.parameter) with
    | Some new_param, Some (old_param, _) ->
        let binder = Bindlib.bind_var old_param e in
        Bindlib.box_apply2
          (fun binder new_param -> Bindlib.subst binder new_param)
          binder (Bindlib.box_var new_param)
    | None, None -> e
    | _ -> assert false
    (* should not happen *)
  in
  let just = substitute_parameter rule.Ast.just in
  let cons = substitute_parameter rule.Ast.cons in
  let exceptions =
    Bindlib.box_list (List.map (rule_tree_to_expr ~toplevel:false def_pos is_func) exceptions)
  in
  let default =
    Bindlib.box_apply3
      (fun exceptions just cons ->
        (Scopelang.Ast.EDefault (exceptions, just, cons), Pos.get_position just))
      exceptions just cons
  in
  match (is_func, rule.parameter) with
  | None, None -> default
  | Some new_param, Some (_, typ) ->
      if toplevel then
        (* When we're creating a function from multiple defaults, we must check that the result
           returned by the function is not empty *)
        let default =
          Bindlib.box_apply
            (fun (default : Scopelang.Ast.expr * Pos.t) ->
              ( Scopelang.Ast.EApp
                  ((Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.ErrorOnEmpty), def_pos), [ default ]),
                def_pos ))
            default
        in
        Scopelang.Ast.make_abs (Array.of_list [ new_param ]) default def_pos [ typ ] def_pos
      else default
  | _ -> (* should not happen *) assert false

(** {1 AST translation} *)

(** Translates a definition inside a scope, the resulting expression should be an {!constructor:
    Dcalc.Ast.EDefault} *)
let translate_def (def_info : Ast.ScopeDef.t) (def : Ast.rule Ast.RuleMap.t)
    (typ : Scopelang.Ast.typ Pos.marked) (is_cond : bool) : Scopelang.Ast.expr Pos.marked =
  (* Here, we have to transform this list of rules into a default tree. *)
  let is_func _ (r : Ast.rule) : bool = Option.is_some r.Ast.parameter in
  let all_rules_func = Ast.RuleMap.for_all is_func def in
  let all_rules_not_func = Ast.RuleMap.for_all (fun n r -> not (is_func n r)) def in
  let is_def_func : Scopelang.Ast.typ Pos.marked option =
    if all_rules_func && Ast.RuleMap.cardinal def > 0 then
      match Pos.unmark typ with
      | Scopelang.Ast.TArrow (t_param, _) -> Some t_param
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf
               "The definitions of %a are function but its type, %a, is not a function type"
               Ast.ScopeDef.format_t def_info Scopelang.Print.format_typ typ)
            (Pos.get_position typ)
    else if all_rules_not_func then None
    else
      Errors.raise_multispanned_error
        "some definitions of the same variable are functions while others aren't"
        ( List.map
            (fun (_, r) ->
              (Some "This definition is a function:", Pos.get_position (Bindlib.unbox r.Ast.cons)))
            (Ast.RuleMap.bindings (Ast.RuleMap.filter is_func def))
        @ List.map
            (fun (_, r) ->
              ( Some "This definition is not a function:",
                Pos.get_position (Bindlib.unbox r.Ast.cons) ))
            (Ast.RuleMap.bindings (Ast.RuleMap.filter (fun n r -> not (is_func n r)) def)) )
  in
  let top_list = def_map_to_tree def_info is_def_func def in
  let top_value =
    (if is_cond then Ast.always_false_rule else Ast.empty_rule) Pos.no_pos is_def_func
  in
  Bindlib.unbox
    (rule_tree_to_expr ~toplevel:true
       (Ast.ScopeDef.get_position def_info)
       (Option.map (fun _ -> Scopelang.Ast.Var.make ("ρ", Pos.no_pos)) is_def_func)
       ( match top_list with
       | [] ->
           (* In this case, there are no rules to define the expression *)
           Leaf top_value
       | _ -> Node (top_list, top_value) ))

(** Translates a scope *)
let translate_scope (scope : Ast.scope) : Scopelang.Ast.scope_decl =
  let scope_dependencies = Dependency.build_scope_dependencies scope in
  Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering = Dependency.correct_computation_ordering scope_dependencies in
  let scope_decl_rules =
    List.flatten
      (List.map
         (fun vertex ->
           match vertex with
           | Dependency.Vertex.Var (var : Scopelang.Ast.ScopeVar.t) ->
               let var_def, var_typ, is_cond =
                 Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs
               in
               let expr_def = translate_def (Ast.ScopeDef.Var var) var_def var_typ is_cond in
               [
                 Scopelang.Ast.Definition
                   ( ( Scopelang.Ast.ScopeVar
                         (var, Pos.get_position (Scopelang.Ast.ScopeVar.get_info var)),
                       Pos.get_position (Scopelang.Ast.ScopeVar.get_info var) ),
                     var_typ,
                     expr_def );
               ]
           | Dependency.Vertex.SubScope sub_scope_index ->
               (* Before calling the sub_scope, we need to include all the re-definitions of
                  subscope parameters*)
               let sub_scope =
                 Scopelang.Ast.SubScopeMap.find sub_scope_index scope.scope_sub_scopes
               in
               let sub_scope_vars_redefs =
                 Ast.ScopeDefMap.mapi
                   (fun def_key (def, def_typ, is_cond) ->
                     match def_key with
                     | Ast.ScopeDef.Var _ -> assert false (* should not happen *)
                     | Ast.ScopeDef.SubScopeVar (_, sub_scope_var) ->
                         let expr_def = translate_def def_key def def_typ is_cond in
                         let subscop_real_name =
                           Scopelang.Ast.SubScopeMap.find sub_scope_index scope.scope_sub_scopes
                         in
                         let var_pos =
                           Pos.get_position (Scopelang.Ast.ScopeVar.get_info sub_scope_var)
                         in
                         Scopelang.Ast.Definition
                           ( ( Scopelang.Ast.SubScopeVar
                                 ( subscop_real_name,
                                   (sub_scope_index, var_pos),
                                   (sub_scope_var, var_pos) ),
                               var_pos ),
                             def_typ,
                             expr_def ))
                   (Ast.ScopeDefMap.filter
                      (fun def_key _def ->
                        match def_key with
                        | Ast.ScopeDef.Var _ -> false
                        | Ast.ScopeDef.SubScopeVar (sub_scope_index', _) ->
                            sub_scope_index = sub_scope_index')
                      scope.scope_defs)
               in
               let sub_scope_vars_redefs =
                 List.map snd (Ast.ScopeDefMap.bindings sub_scope_vars_redefs)
               in
               sub_scope_vars_redefs @ [ Scopelang.Ast.Call (sub_scope, sub_scope_index) ])
         scope_ordering)
  in
  (* Then, after having computed all the scopes variables, we add the assertions *)
  let scope_decl_rules =
    scope_decl_rules
    @ List.map (fun e -> Scopelang.Ast.Assertion (Bindlib.unbox e)) scope.Ast.scope_assertions
  in
  let scope_sig =
    Scopelang.Ast.ScopeVarSet.fold
      (fun var acc ->
        let _, typ, _ = Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs in
        Scopelang.Ast.ScopeVarMap.add var typ acc)
      scope.scope_vars Scopelang.Ast.ScopeVarMap.empty
  in
  {
    Scopelang.Ast.scope_decl_name = scope.scope_uid;
    Scopelang.Ast.scope_decl_rules;
    Scopelang.Ast.scope_sig;
  }

(** {1 API} *)

let translate_program (pgrm : Ast.program) : Scopelang.Ast.program =
  {
    Scopelang.Ast.program_scopes = Scopelang.Ast.ScopeMap.map translate_scope pgrm.program_scopes;
    Scopelang.Ast.program_structs = pgrm.program_structs;
    Scopelang.Ast.program_enums = pgrm.program_enums;
  }
