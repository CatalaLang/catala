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

module Pos = Utils.Pos
module Errors = Utils.Errors
module Cli = Utils.Cli

type rule_tree = Leaf of Ast.rule | Node of Ast.rule * rule_tree list

(* invariant: one rule in def does not have any parent rule *)
(* invariant: there are no dandling pointer parents in the rules *)
let rec def_map_to_tree (def : Ast.rule Ast.RuleMap.t) : rule_tree =
  (* first we look to the only rule that does not have any parent *)
  let has_no_parent _ (r : Ast.rule) = Option.is_none r.Ast.parent_rule in
  let no_parent = Ast.RuleMap.filter has_no_parent def in
  let no_parent_name, no_parent =
    if Ast.RuleMap.cardinal no_parent = 1 then Ast.RuleMap.choose no_parent else assert false
  in
  let def = Ast.RuleMap.remove no_parent_name def in
  (* we look for all the direct children of no_parent *)
  let children, rest =
    Ast.RuleMap.partition (fun _ r -> r.Ast.parent_rule = Some no_parent_name) def
  in
  if Ast.RuleMap.cardinal children = 0 then Leaf no_parent
    (* it doesn't matter that [rest] contains more rules since each rule in [rest] is supposed to
       have a parent rule containted in the original tree, so it will get treated at some point *)
  else
    let children_no_parent =
      Ast.RuleMap.map (fun r -> { r with Ast.parent_rule = None }) children
    in
    let tree_children =
      List.map
        (fun (child_no_parent_name, child_no_parent) ->
          def_map_to_tree (Ast.RuleMap.add child_no_parent_name child_no_parent rest))
        (Ast.RuleMap.bindings children_no_parent)
    in
    Node (no_parent, tree_children)

let rec rule_tree_to_expr ~(toplevel : bool) (is_func : Scopelang.Ast.Var.t option)
    (tree : rule_tree) : Scopelang.Ast.expr Pos.marked Bindlib.box =
  let rule, children = match tree with Leaf r -> (r, []) | Node (r, child) -> (r, child) in
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
  let children = Bindlib.box_list (List.map (rule_tree_to_expr ~toplevel:false is_func) children) in
  let default =
    Bindlib.box_apply3
      (fun just cons children ->
        (Scopelang.Ast.EDefault (just, cons, children), Pos.get_position just))
      just cons children
  in
  match (is_func, rule.parameter) with
  | None, None -> default
  | Some new_param, Some (_, typ) ->
      if toplevel then
        Scopelang.Ast.make_abs (Array.of_list [ new_param ]) default Pos.no_pos [ typ ] Pos.no_pos
      else default
  | _ -> assert false

(* should not happen *)

let translate_def (def_info : Ast.ScopeDef.t) (def : Ast.rule Ast.RuleMap.t)
    (typ : Scopelang.Ast.typ Pos.marked) : Scopelang.Ast.expr Pos.marked =
  (* Here, we have to transform this list of rules into a default tree. *)
  (* Because we can have multiple rules at the top-level and our syntax does not allow that, we
     insert a dummy rule at the top *)
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
  let dummy_rule = Ast.empty_rule Pos.no_pos is_def_func in
  let dummy_rule_name = Ast.RuleName.fresh ("dummy", Pos.no_pos) in
  let def =
    Ast.RuleMap.add dummy_rule_name dummy_rule
      (Ast.RuleMap.map
         (fun r ->
           match r.Ast.parent_rule with
           | Some _ -> r
           | None -> { r with parent_rule = Some dummy_rule_name })
         def)
  in
  let def_tree = def_map_to_tree def in
  Bindlib.unbox
    (rule_tree_to_expr ~toplevel:true
       (Option.map (fun _ -> Scopelang.Ast.Var.make ("ρ", Pos.no_pos)) is_def_func)
       def_tree)

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
               let var_def, var_typ =
                 Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs
               in
               let expr_def = translate_def (Ast.ScopeDef.Var var) var_def var_typ in
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
                   (fun def_key (def, def_typ) ->
                     match def_key with
                     | Ast.ScopeDef.Var _ -> assert false (* should not happen *)
                     | Ast.ScopeDef.SubScopeVar (_, sub_scope_var) ->
                         let expr_def = translate_def def_key def def_typ in
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
        let _, typ = Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs in
        Scopelang.Ast.ScopeVarMap.add var typ acc)
      scope.scope_vars Scopelang.Ast.ScopeVarMap.empty
  in
  {
    Scopelang.Ast.scope_decl_name = scope.scope_uid;
    Scopelang.Ast.scope_decl_rules;
    Scopelang.Ast.scope_sig;
  }

let translate_program (pgrm : Ast.program) : Scopelang.Ast.program =
  {
    Scopelang.Ast.program_scopes = Scopelang.Ast.ScopeMap.map translate_scope pgrm.program_scopes;
    Scopelang.Ast.program_structs = pgrm.program_structs;
    Scopelang.Ast.program_enums = pgrm.program_enums;
  }
