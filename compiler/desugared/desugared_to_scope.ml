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

(** Intermediate representation for the exception tree of rules for a particular scope definition. *)
type rule_tree =
  | Leaf of Ast.rule list  (** Rules defining a base case piecewise. List is non-empty. *)
  | Node of rule_tree list * Ast.rule list
      (** A list of exceptions to a non-empty list of rules defining a base case piecewise. *)

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
  let rec build_tree (base_cases : Ast.RuleSet.t) : rule_tree =
    let exceptions = Dependency.ExceptionsDependencies.pred exc_graph base_cases in
    let base_case_as_rule_list =
      List.map (fun r -> Ast.RuleMap.find r def) (List.of_seq (Ast.RuleSet.to_seq base_cases))
    in
    match exceptions with
    | [] -> Leaf base_case_as_rule_list
    | _ -> Node (List.map build_tree exceptions, base_case_as_rule_list)
  in
  List.map build_tree base_cases

(** From the {!type: rule_tree}, builds an {!constructor: Dcalc.Ast.EDefault} expression in the
    scope language. The [~toplevel] parameter is used to know when to place the toplevel binding in
    the case of functions. *)
let rec rule_tree_to_expr ~(toplevel : bool) (def_pos : Pos.t)
    (is_func : Scopelang.Ast.Var.t option) (tree : rule_tree) :
    Scopelang.Ast.expr Pos.marked Bindlib.box =
  let exceptions, base_rules =
    match tree with Leaf r -> ([], r) | Node (exceptions, r) -> (exceptions, r)
  in
  (* because each rule has its own variable parameter and we want to convert the whole rule tree
     into a function, we need to perform some alpha-renaming of all the expressions *)
  let substitute_parameter (e : Scopelang.Ast.expr Pos.marked Bindlib.box) (rule : Ast.rule) :
      Scopelang.Ast.expr Pos.marked Bindlib.box =
    match (is_func, rule.Ast.rule_parameter) with
    | Some new_param, Some (old_param, _) ->
        let binder = Bindlib.bind_var old_param e in
        Bindlib.box_apply2
          (fun binder new_param -> Bindlib.subst binder new_param)
          binder (Bindlib.box_var new_param)
    | None, None -> e
    | _ -> assert false
    (* should not happen *)
  in
  let base_just_list =
    List.map (fun rule -> substitute_parameter rule.Ast.rule_just rule) base_rules
  in
  let base_cons_list =
    List.map (fun rule -> substitute_parameter rule.Ast.rule_cons rule) base_rules
  in
  let default_containing_base_cases =
    Bindlib.box_apply2
      (fun base_just_list base_cons_list ->
        ( Scopelang.Ast.EDefault
            ( List.map2
                (fun base_just base_cons ->
                  (Scopelang.Ast.EDefault ([], base_just, base_cons), Pos.get_position base_just))
                base_just_list base_cons_list,
              (Scopelang.Ast.ELit (Dcalc.Ast.LBool false), def_pos),
              (Scopelang.Ast.ELit Dcalc.Ast.LEmptyError, def_pos) ),
          def_pos ))
      (Bindlib.box_list base_just_list) (Bindlib.box_list base_cons_list)
  in
  let exceptions =
    Bindlib.box_list (List.map (rule_tree_to_expr ~toplevel:false def_pos is_func) exceptions)
  in
  let default =
    Bindlib.box_apply2
      (fun exceptions default_containing_base_cases ->
        ( Scopelang.Ast.EDefault
            ( exceptions,
              (Scopelang.Ast.ELit (Dcalc.Ast.LBool true), def_pos),
              default_containing_base_cases ),
          def_pos ))
      exceptions default_containing_base_cases
  in
  match (is_func, (List.hd base_rules).Ast.rule_parameter) with
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
    (typ : Scopelang.Ast.typ Pos.marked) (io : Scopelang.Ast.io) ~(is_cond : bool)
    ~(is_subscope_var : bool) : Scopelang.Ast.expr Pos.marked =
  (* Here, we have to transform this list of rules into a default tree. *)
  let is_def_func = match Pos.unmark typ with Scopelang.Ast.TArrow (_, _) -> true | _ -> false in
  let is_rule_func _ (r : Ast.rule) : bool = Option.is_some r.Ast.rule_parameter in
  let all_rules_func = Ast.RuleMap.for_all is_rule_func def in
  let all_rules_not_func = Ast.RuleMap.for_all (fun n r -> not (is_rule_func n r)) def in
  let is_def_func_param_typ : Scopelang.Ast.typ Pos.marked option =
    if is_def_func && all_rules_func then
      match Pos.unmark typ with
      | Scopelang.Ast.TArrow (t_param, _) -> Some t_param
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf
               "The definitions of %a are function but its type, %a, is not a function type"
               Ast.ScopeDef.format_t def_info Scopelang.Print.format_typ typ)
            (Pos.get_position typ)
    else if (not is_def_func) && all_rules_not_func then None
    else
      Errors.raise_multispanned_error
        "some definitions of the same variable are functions while others aren't"
        (List.map
           (fun (_, r) ->
             ( Some "This definition is a function:",
               Pos.get_position (Bindlib.unbox r.Ast.rule_cons) ))
           (Ast.RuleMap.bindings (Ast.RuleMap.filter is_rule_func def))
        @ List.map
            (fun (_, r) ->
              ( Some "This definition is not a function:",
                Pos.get_position (Bindlib.unbox r.Ast.rule_cons) ))
            (Ast.RuleMap.bindings (Ast.RuleMap.filter (fun n r -> not (is_rule_func n r)) def)))
  in
  let top_list = def_map_to_tree def_info def in
  let top_value =
    (if is_cond then Ast.always_false_rule else Ast.empty_rule) Pos.no_pos is_def_func_param_typ
  in
  if
    Ast.RuleMap.cardinal def = 0
    && is_subscope_var
    (* Here we have a special case for the empty definitions. Indeed, we could use the code for the
       regular case below that would create a convoluted default always returning empty error, and
       this would be correct. But it gets more complicated with functions. Indeed, if we create an
       empty definition for a subscope argument whose type is a function, we get something like [fun
       () -> (fun real_param -> < ... >)] that is passed as an argument to the subscope. The
       sub-scope de-thunks but the de-thunking does not return empty error, signalling there is not
       reentrant variable, because functions are values! So the subscope does not see that there is
       not reentrant variable and does not pick its internal definition instead. See
       [test/test_scope/subscope_function_arg_not_defined.catala_en] for a test case exercising that
       subtlety.

       To avoid this complication we special case here and put an empty error for all subscope
       variables that are not defined. It covers the subtlety with functions described above but
       also conditions with the false default value. *)
    && not
         (is_cond
         && match Pos.unmark io.Scopelang.Ast.io_input with OnlyInput -> true | _ -> false)
    (* However, this special case suffers from an exception: when a condition is defined as an
       OnlyInput to a subscope, since the [false] default value will not be provided by the calee
       scope, it has to be placed in the caller. *)
  then (ELit LEmptyError, Pos.no_pos)
  else
    Bindlib.unbox
      (rule_tree_to_expr ~toplevel:true
         (Ast.ScopeDef.get_position def_info)
         (Option.map (fun _ -> Scopelang.Ast.Var.make ("param", Pos.no_pos)) is_def_func_param_typ)
         (match top_list with
         | [] ->
             (* In this case, there are no rules to define the expression *)
             Leaf [ top_value ]
         | _ -> Node (top_list, [ top_value ])))

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
           | Dependency.Vertex.Var (var : Scopelang.Ast.ScopeVar.t) -> (
               let scope_def = Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs in
               let var_def = scope_def.scope_def_rules in
               let var_typ = scope_def.scope_def_typ in
               let is_cond = scope_def.scope_def_is_condition in
               match Pos.unmark scope_def.Ast.scope_def_io.io_input with
               | OnlyInput when not (Ast.RuleMap.is_empty var_def) ->
                   (* If the variable is tagged as input, then it shall not be redefined. *)
                   Errors.raise_multispanned_error
                     "It is impossible to give a definition to a scope variable tagged as input."
                     (( Some "Relevant variable:",
                        Pos.get_position (Scopelang.Ast.ScopeVar.get_info var) )
                     :: List.map
                          (fun (rule, _) ->
                            ( Some "Incriminated variable definition:",
                              Pos.get_position (Ast.RuleName.get_info rule) ))
                          (Ast.RuleMap.bindings var_def))
               | OnlyInput -> [] (* we do not provide any definition for an input-only variable *)
               | _ ->
                   let expr_def =
                     translate_def (Ast.ScopeDef.Var var) var_def var_typ scope_def.Ast.scope_def_io
                       ~is_cond ~is_subscope_var:false
                   in
                   [
                     Scopelang.Ast.Definition
                       ( ( Scopelang.Ast.ScopeVar
                             (var, Pos.get_position (Scopelang.Ast.ScopeVar.get_info var)),
                           Pos.get_position (Scopelang.Ast.ScopeVar.get_info var) ),
                         var_typ,
                         expr_def );
                   ])
           | Dependency.Vertex.SubScope sub_scope_index ->
               (* Before calling the sub_scope, we need to include all the re-definitions of
                  subscope parameters*)
               let sub_scope =
                 Scopelang.Ast.SubScopeMap.find sub_scope_index scope.scope_sub_scopes
               in
               let sub_scope_vars_redefs_candidates =
                 Ast.ScopeDefMap.filter
                   (fun def_key scope_def ->
                     match def_key with
                     | Ast.ScopeDef.Var _ -> false
                     | Ast.ScopeDef.SubScopeVar (sub_scope_index', _) ->
                         sub_scope_index = sub_scope_index'
                         (* We exclude subscope variables that have 0 re-definitions and are not
                            visible in the input of the subscope *)
                         && not
                              ((match Pos.unmark scope_def.Ast.scope_def_io.io_input with
                               | Scopelang.Ast.NoInput -> true
                               | _ -> false)
                              && Ast.RuleMap.is_empty scope_def.scope_def_rules))
                   scope.scope_defs
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
                         (* This definition redefines a variable of the correct subscope. But we
                            have to check that this redefinition is allowed with respect to the io
                            parameters of that subscope variable. *)
                         (match Pos.unmark scope_def.Ast.scope_def_io.io_input with
                         | Scopelang.Ast.NoInput ->
                             Errors.raise_multispanned_error
                               "It is impossible to give a definition to a subscope variable not \
                                tagged as input or context."
                               ((Some "Relevant subscope:", Ast.ScopeDef.get_position def_key)
                               :: ( Some "Relevant variable:",
                                    Pos.get_position (Scopelang.Ast.ScopeVar.get_info sub_scope_var)
                                  )
                               :: List.map
                                    (fun (rule, _) ->
                                      ( Some "Suscope variable definition:",
                                        Pos.get_position (Ast.RuleName.get_info rule) ))
                                    (Ast.RuleMap.bindings def))
                         | _ -> ());
                         (* Now that all is good, we can proceed with translating this redefinition
                            to a proper Scopelang term. *)
                         let expr_def =
                           translate_def def_key def def_typ scope_def.Ast.scope_def_io ~is_cond
                             ~is_subscope_var:true
                         in
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
                   sub_scope_vars_redefs_candidates
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
        let scope_def = Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs in
        let typ = scope_def.scope_def_typ in
        Scopelang.Ast.ScopeVarMap.add var (typ, scope_def.scope_def_io) acc)
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
