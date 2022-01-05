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

open Utils

(** {1 Rule tree construction} *)

type rule_tree = Leaf of Ast.rule | Node of rule_tree list * Ast.rule

(** Transforms a flat list of rules into a tree, taking into account the priorities declared between
    rules *)
let def_map_to_tree (def_info : Ast.ScopeDef.t) (def : Ast.rule Ast.RuleMap.t) : rule_tree list =
  let exc_graph = Dependency.build_exceptions_graph def def_info in
  Dependency.check_for_exception_cycle exc_graph;
  (* we start by the base cases: they are the vertices which have no successors *)
  let base_cases =
    Dependency.ExceptionsDependencies.fold_vertex
      (fun v base_cases ->
        if Dependency.ExceptionsDependencies.out_degree exc_graph v = 0 then v :: base_cases
        else base_cases)
      exc_graph []
  in
  let rec build_tree (base_case : Ast.RuleName.t) : rule_tree =
    let exceptions = Dependency.ExceptionsDependencies.pred exc_graph base_case in
    match exceptions with
    | [] -> Leaf (Ast.RuleMap.find base_case def)
    | _ -> Node (List.map build_tree exceptions, Ast.RuleMap.find base_case def)
  in
  List.map build_tree base_cases

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
    match (is_func, rule.rule_parameter) with
    | Some new_param, Some (old_param, _) ->
        let binder = Bindlib.bind_var old_param e in
        Bindlib.box_apply2
          (fun binder new_param -> Bindlib.subst binder new_param)
          binder (Bindlib.box_var new_param)
    | None, None -> e
    | _ -> assert false
    (* should not happen *)
  in
  let just = substitute_parameter rule.Ast.rule_just in
  let cons = substitute_parameter rule.Ast.rule_cons in
  let exceptions =
    Bindlib.box_list (List.map (rule_tree_to_expr ~toplevel:false def_pos is_func) exceptions)
  in
  let default =
    Bindlib.box_apply3
      (fun exceptions just cons ->
        (Scopelang.Ast.EDefault (exceptions, just, cons), Pos.get_position just))
      exceptions just cons
  in
  match (is_func, rule.rule_parameter) with
  | None, None -> default
  | Some new_param, Some (_, typ) ->
      if toplevel then
        (* When we're creating a function from multiple defaults, we must check that the result
           returned by the function is not empty *)
        let default =
          Bindlib.box_apply
            (fun (default : Scopelang.Ast.expr * Pos.t) ->
              (Scopelang.Ast.ErrorOnEmpty default, def_pos))
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
  let is_func _ (r : Ast.rule) : bool = Option.is_some r.Ast.rule_parameter in
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
        (List.map
           (fun (_, r) ->
             ( Some "This definition is a function:",
               Pos.get_position (Bindlib.unbox r.Ast.rule_cons) ))
           (Ast.RuleMap.bindings (Ast.RuleMap.filter is_func def))
        @ List.map
            (fun (_, r) ->
              ( Some "This definition is not a function:",
                Pos.get_position (Bindlib.unbox r.Ast.rule_cons) ))
            (Ast.RuleMap.bindings (Ast.RuleMap.filter (fun n r -> not (is_func n r)) def)))
  in
  let top_list = def_map_to_tree def_info def in
  let top_value =
    (if is_cond then Ast.always_false_rule else Ast.empty_rule) Pos.no_pos is_def_func
  in
  Bindlib.unbox
    (rule_tree_to_expr ~toplevel:true
       (Ast.ScopeDef.get_position def_info)
       (Option.map (fun _ -> Scopelang.Ast.Var.make ("param", Pos.no_pos)) is_def_func)
       (match top_list with
       | [] ->
           (* In this case, there are no rules to define the expression *)
           Leaf top_value
       | _ -> Node (top_list, top_value)))

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
               let scope_def = Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs in
               let var_def = scope_def.scope_def_rules in
               let var_typ = scope_def.scope_def_typ in
               let is_cond = scope_def.scope_def_is_condition in
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
                   (fun def_key scope_def ->
                     let def = scope_def.Ast.scope_def_rules in
                     let def_typ = scope_def.scope_def_typ in
                     let is_cond = scope_def.scope_def_is_condition in
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
        let typ = (Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs).scope_def_typ in
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
