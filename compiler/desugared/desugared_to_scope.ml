(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Translation from {!module: Desugared.Ast} to {!module: Scopelang.Ast} *)

open Utils

(** {1 Expression translation}*)

type target_scope_vars =
  | WholeVar of Scopelang.Ast.ScopeVar.t
  | States of (Ast.StateName.t * Scopelang.Ast.ScopeVar.t) list

type ctx = {
  scope_var_mapping : target_scope_vars Ast.ScopeVarMap.t;
  var_mapping : Scopelang.Ast.Var.t Ast.VarMap.t;
}

let tag_with_log_entry
    (e : Scopelang.Ast.expr Marked.pos)
    (l : Dcalc.Ast.log_entry)
    (markings : Utils.Uid.MarkedString.info list) :
    Scopelang.Ast.expr Marked.pos =
  ( Scopelang.Ast.EApp
      ( ( Scopelang.Ast.EOp (Dcalc.Ast.Unop (Dcalc.Ast.Log (l, markings))),
          Marked.get_mark e ),
        [e] ),
    Marked.get_mark e )

let rec translate_expr (ctx : ctx) (e : Ast.expr Marked.pos) :
    Scopelang.Ast.expr Marked.pos Bindlib.box =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | Ast.ELocation (SubScopeVar (s_name, ss_name, s_var)) ->
    (* When referring to a subscope variable in an expression, we are referring
       to the output, hence we take the last state. *)
    let new_s_var =
      match
        Ast.ScopeVarMap.find (Marked.unmark s_var) ctx.scope_var_mapping
      with
      | WholeVar new_s_var -> Marked.same_mark_as new_s_var s_var
      | States states ->
        Marked.same_mark_as (snd (List.hd (List.rev states))) s_var
    in
    Bindlib.box
      (Scopelang.Ast.ELocation (SubScopeVar (s_name, ss_name, new_s_var)), m)
  | Ast.ELocation (ScopeVar (s_var, None)) ->
    Bindlib.box
      ( Scopelang.Ast.ELocation
          (ScopeVar
             (match
                Ast.ScopeVarMap.find (Marked.unmark s_var) ctx.scope_var_mapping
              with
             | WholeVar new_s_var -> Marked.same_mark_as new_s_var s_var
             | States _ -> failwith "should not happen")),
        m )
  | Ast.ELocation (ScopeVar (s_var, Some state)) ->
    Bindlib.box
      ( Scopelang.Ast.ELocation
          (ScopeVar
             (match
                Ast.ScopeVarMap.find (Marked.unmark s_var) ctx.scope_var_mapping
              with
             | WholeVar _ -> failwith "should not happen"
             | States states ->
               Marked.same_mark_as (List.assoc state states) s_var)),
        m )
  | Ast.EVar v ->
    Bindlib.box_apply
      (fun v -> Marked.same_mark_as v e)
      (Bindlib.box_var (Ast.VarMap.find v ctx.var_mapping))
  | EStruct (s_name, fields) ->
    Bindlib.box_apply
      (fun new_fields -> Scopelang.Ast.EStruct (s_name, new_fields), m)
      (Scopelang.Ast.StructFieldMapLift.lift_box
         (Scopelang.Ast.StructFieldMap.map (translate_expr ctx) fields))
  | EStructAccess (e1, s_name, f_name) ->
    Bindlib.box_apply
      (fun new_e1 -> Scopelang.Ast.EStructAccess (new_e1, s_name, f_name), m)
      (translate_expr ctx e1)
  | EEnumInj (e1, cons, e_name) ->
    Bindlib.box_apply
      (fun new_e1 -> Scopelang.Ast.EEnumInj (new_e1, cons, e_name), m)
      (translate_expr ctx e1)
  | EMatch (e1, e_name, arms) ->
    Bindlib.box_apply2
      (fun new_e1 new_arms ->
        Scopelang.Ast.EMatch (new_e1, e_name, new_arms), m)
      (translate_expr ctx e1)
      (Scopelang.Ast.EnumConstructorMapLift.lift_box
         (Scopelang.Ast.EnumConstructorMap.map (translate_expr ctx) arms))
  | ELit l -> Bindlib.box (Scopelang.Ast.ELit l, m)
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    let new_vars =
      Array.map (fun var -> Scopelang.Ast.Var.make (Bindlib.name_of var)) vars
    in
    let ctx =
      List.fold_left2
        (fun ctx var new_var ->
          { ctx with var_mapping = Ast.VarMap.add var new_var ctx.var_mapping })
        ctx (Array.to_list vars) (Array.to_list new_vars)
    in
    Bindlib.box_apply
      (fun new_binder -> Scopelang.Ast.EAbs (new_binder, typs), m)
      (Bindlib.bind_mvar new_vars (translate_expr ctx body))
  | EApp (e1, args) ->
    Bindlib.box_apply2
      (fun new_e1 new_args -> Scopelang.Ast.EApp (new_e1, new_args), m)
      (translate_expr ctx e1)
      (Bindlib.box_list (List.map (translate_expr ctx) args))
  | EOp op -> Bindlib.box (Scopelang.Ast.EOp op, m)
  | EDefault (excepts, just, cons) ->
    Bindlib.box_apply3
      (fun new_excepts new_just new_cons ->
        Scopelang.Ast.make_default ~pos:m new_excepts new_just new_cons)
      (Bindlib.box_list (List.map (translate_expr ctx) excepts))
      (translate_expr ctx just) (translate_expr ctx cons)
  | EIfThenElse (e1, e2, e3) ->
    Bindlib.box_apply3
      (fun new_e1 new_e2 new_e3 ->
        Scopelang.Ast.EIfThenElse (new_e1, new_e2, new_e3), m)
      (translate_expr ctx e1) (translate_expr ctx e2) (translate_expr ctx e3)
  | EArray args ->
    Bindlib.box_apply
      (fun new_args -> Scopelang.Ast.EArray new_args, m)
      (Bindlib.box_list (List.map (translate_expr ctx) args))
  | ErrorOnEmpty e1 ->
    Bindlib.box_apply
      (fun new_e1 -> Scopelang.Ast.ErrorOnEmpty new_e1, m)
      (translate_expr ctx e1)

(** {1 Rule tree construction} *)

(** Intermediate representation for the exception tree of rules for a particular
    scope definition. *)
type rule_tree =
  | Leaf of Ast.rule list
      (** Rules defining a base case piecewise. List is non-empty. *)
  | Node of rule_tree list * Ast.rule list
      (** [Node (exceptions, base_case)] is a list of exceptions to a non-empty
          list of rules defining a base case piecewise. *)

(** Transforms a flat list of rules into a tree, taking into account the
    priorities declared between rules *)
let def_map_to_tree (def_info : Ast.ScopeDef.t) (def : Ast.rule Ast.RuleMap.t) :
    rule_tree list =
  let exc_graph = Dependency.build_exceptions_graph def def_info in
  Dependency.check_for_exception_cycle exc_graph;
  (* we start by the base cases: they are the vertices which have no
     successors *)
  let base_cases =
    Dependency.ExceptionsDependencies.fold_vertex
      (fun v base_cases ->
        if Dependency.ExceptionsDependencies.out_degree exc_graph v = 0 then
          v :: base_cases
        else base_cases)
      exc_graph []
  in
  let rec build_tree (base_cases : Ast.RuleSet.t) : rule_tree =
    let exceptions =
      Dependency.ExceptionsDependencies.pred exc_graph base_cases
    in
    let base_case_as_rule_list =
      List.map
        (fun r -> Ast.RuleMap.find r def)
        (Ast.RuleSet.elements base_cases)
    in
    match exceptions with
    | [] -> Leaf base_case_as_rule_list
    | _ -> Node (List.map build_tree exceptions, base_case_as_rule_list)
  in
  List.map build_tree base_cases

(** From the {!type: rule_tree}, builds an {!constructor: Dcalc.Ast.EDefault}
    expression in the scope language. The [~toplevel] parameter is used to know
    when to place the toplevel binding in the case of functions. *)
let rec rule_tree_to_expr
    ~(toplevel : bool)
    (ctx : ctx)
    (def_pos : Pos.t)
    (is_func : Ast.Var.t option)
    (tree : rule_tree) : Scopelang.Ast.expr Marked.pos Bindlib.box =
  let exceptions, base_rules =
    match tree with Leaf r -> [], r | Node (exceptions, r) -> exceptions, r
  in
  (* because each rule has its own variable parameter and we want to convert the
     whole rule tree into a function, we need to perform some alpha-renaming of
     all the expressions *)
  let substitute_parameter
      (e : Ast.expr Marked.pos Bindlib.box)
      (rule : Ast.rule) : Ast.expr Marked.pos Bindlib.box =
    match is_func, rule.Ast.rule_parameter with
    | Some new_param, Some (old_param, _) ->
      let binder = Bindlib.bind_var old_param e in
      Bindlib.box_apply2
        (fun binder new_param -> Bindlib.subst binder new_param)
        binder
        (Bindlib.box_var new_param)
    | None, None -> e
    | _ -> assert false
    (* should not happen *)
  in
  let ctx =
    match is_func with
    | None -> ctx
    | Some new_param -> (
      match Ast.VarMap.find_opt new_param ctx.var_mapping with
      | None ->
        let new_param_scope =
          Scopelang.Ast.Var.make (Bindlib.name_of new_param)
        in
        {
          ctx with
          var_mapping = Ast.VarMap.add new_param new_param_scope ctx.var_mapping;
        }
      | Some _ ->
        (* We only create a mapping if none exists because [rule_tree_to_expr]
           is called recursively on the exceptions of the tree and we don't want
           to create a new Scopelang variable for the parameter at each tree
           level. *)
        ctx)
  in
  let base_just_list =
    List.map
      (fun rule -> substitute_parameter rule.Ast.rule_just rule)
      base_rules
  in
  let base_cons_list =
    List.map
      (fun rule -> substitute_parameter rule.Ast.rule_cons rule)
      base_rules
  in
  let translate_and_unbox_list (list : Ast.expr Marked.pos Bindlib.box list) :
      Scopelang.Ast.expr Marked.pos Bindlib.box list =
    List.map
      (fun e ->
        (* There are two levels of boxing here, the outermost is introduced by
           the [translate_expr] function for which all of the bindings should
           have been closed by now, so we can safely unbox. *)
        Bindlib.unbox (Bindlib.box_apply (translate_expr ctx) e))
      list
  in
  let default_containing_base_cases =
    Bindlib.box_apply2
      (fun base_just_list base_cons_list ->
        Scopelang.Ast.make_default
          (List.map2
             (fun base_just base_cons ->
               Scopelang.Ast.make_default ~pos:def_pos []
                 (* Here we insert the logging command that records when a
                    decision is taken for the value of a variable. *)
                 (tag_with_log_entry base_just Dcalc.Ast.PosRecordIfTrueBool [])
                 base_cons)
             base_just_list base_cons_list)
          (Scopelang.Ast.ELit (Dcalc.Ast.LBool false), def_pos)
          (Scopelang.Ast.ELit Dcalc.Ast.LEmptyError, def_pos))
      (Bindlib.box_list (translate_and_unbox_list base_just_list))
      (Bindlib.box_list (translate_and_unbox_list base_cons_list))
  in
  let exceptions =
    Bindlib.box_list
      (List.map
         (rule_tree_to_expr ~toplevel:false ctx def_pos is_func)
         exceptions)
  in
  let default =
    Bindlib.box_apply2
      (fun exceptions default_containing_base_cases ->
        Scopelang.Ast.make_default exceptions
          (Scopelang.Ast.ELit (Dcalc.Ast.LBool true), def_pos)
          default_containing_base_cases)
      exceptions default_containing_base_cases
  in
  match is_func, (List.hd base_rules).Ast.rule_parameter with
  | None, None -> default
  | Some new_param, Some (_, typ) ->
    if toplevel then
      (* When we're creating a function from multiple defaults, we must check
         that the result returned by the function is not empty *)
      let default =
        Bindlib.box_apply
          (fun (default : Scopelang.Ast.expr * Pos.t) ->
            Scopelang.Ast.ErrorOnEmpty default, def_pos)
          default
      in
      Scopelang.Ast.make_abs
        (Array.of_list [Ast.VarMap.find new_param ctx.var_mapping])
        default [typ] def_pos
    else default
  | _ -> (* should not happen *) assert false

(** {1 AST translation} *)

(** Translates a definition inside a scope, the resulting expression should be
    an {!constructor: Dcalc.Ast.EDefault} *)
let translate_def
    (ctx : ctx)
    (def_info : Ast.ScopeDef.t)
    (def : Ast.rule Ast.RuleMap.t)
    (typ : Scopelang.Ast.typ Marked.pos)
    (io : Scopelang.Ast.io)
    ~(is_cond : bool)
    ~(is_subscope_var : bool) : Scopelang.Ast.expr Marked.pos =
  (* Here, we have to transform this list of rules into a default tree. *)
  let is_def_func =
    match Marked.unmark typ with
    | Scopelang.Ast.TArrow (_, _) -> true
    | _ -> false
  in
  let is_rule_func _ (r : Ast.rule) : bool =
    Option.is_some r.Ast.rule_parameter
  in
  let all_rules_func = Ast.RuleMap.for_all is_rule_func def in
  let all_rules_not_func =
    Ast.RuleMap.for_all (fun n r -> not (is_rule_func n r)) def
  in
  let is_def_func_param_typ : Scopelang.Ast.typ Marked.pos option =
    if is_def_func && all_rules_func then
      match Marked.unmark typ with
      | Scopelang.Ast.TArrow (t_param, _) -> Some t_param
      | _ ->
        Errors.raise_spanned_error (Marked.get_mark typ)
          "The definitions of %a are function but its type, %a, is not a \
           function type"
          Ast.ScopeDef.format_t def_info Scopelang.Print.format_typ typ
    else if (not is_def_func) && all_rules_not_func then None
    else
      let spans =
        List.map
          (fun (_, r) ->
            ( Some "This definition is a function:",
              Marked.get_mark (Bindlib.unbox r.Ast.rule_cons) ))
          (Ast.RuleMap.bindings (Ast.RuleMap.filter is_rule_func def))
        @ List.map
            (fun (_, r) ->
              ( Some "This definition is not a function:",
                Marked.get_mark (Bindlib.unbox r.Ast.rule_cons) ))
            (Ast.RuleMap.bindings
               (Ast.RuleMap.filter (fun n r -> not (is_rule_func n r)) def))
      in
      Errors.raise_multispanned_error spans
        "some definitions of the same variable are functions while others \
         aren't"
  in
  let top_list = def_map_to_tree def_info def in
  let is_input =
    match Marked.unmark io.Scopelang.Ast.io_input with
    | OnlyInput -> true
    | _ -> false
  in
  let top_value =
    if is_cond && ((not is_subscope_var) || (is_subscope_var && is_input)) then
      (* We add the bottom [false] value for conditions, only for the scope
         where the condition is declared. Except when the variable is an input,
         where we want the [false] to be added at each caller parent scope. *)
      Some
        (Ast.always_false_rule
           (Ast.ScopeDef.get_position def_info)
           is_def_func_param_typ)
    else None
  in
  if
    Ast.RuleMap.cardinal def = 0
    && is_subscope_var
    (* Here we have a special case for the empty definitions. Indeed, we could
       use the code for the regular case below that would create a convoluted
       default always returning empty error, and this would be correct. But it
       gets more complicated with functions. Indeed, if we create an empty
       definition for a subscope argument whose type is a function, we get
       something like [fun () -> (fun real_param -> < ... >)] that is passed as
       an argument to the subscope. The sub-scope de-thunks but the de-thunking
       does not return empty error, signalling there is not reentrant variable,
       because functions are values! So the subscope does not see that there is
       not reentrant variable and does not pick its internal definition instead.
       See [test/test_scope/subscope_function_arg_not_defined.catala_en] for a
       test case exercising that subtlety.

       To avoid this complication we special case here and put an empty error
       for all subscope variables that are not defined. It covers the subtlety
       with functions described above but also conditions with the false default
       value. *)
    && not (is_cond && is_input)
    (* However, this special case suffers from an exception: when a condition is
       defined as an OnlyInput to a subscope, since the [false] default value
       will not be provided by the calee scope, it has to be placed in the
       caller. *)
  then ELit LEmptyError, Ast.ScopeDef.get_position def_info
  else
    Bindlib.unbox
      (rule_tree_to_expr ~toplevel:true ctx
         (Ast.ScopeDef.get_position def_info)
         (Option.map (fun _ -> Ast.Var.make "param") is_def_func_param_typ)
         (match top_list, top_value with
         | [], None ->
           (* In this case, there are no rules to define the expression and no
              default value so we put an empty rule. *)
           Leaf [Ast.empty_rule (Marked.get_mark typ) is_def_func_param_typ]
         | [], Some top_value ->
           (* In this case, there are no rules to define the expression but a
              default value so we put it. *)
           Leaf [top_value]
         | _, Some top_value ->
           (* When there are rules + a default value, we put the rules as
              exceptions to the default value *)
           Node (top_list, [top_value])
         | [top_tree], None -> top_tree
         | _, None ->
           Node
             ( top_list,
               [Ast.empty_rule (Marked.get_mark typ) is_def_func_param_typ] )))

(** Translates a scope *)
let translate_scope (ctx : ctx) (scope : Ast.scope) : Scopelang.Ast.scope_decl =
  let scope_dependencies = Dependency.build_scope_dependencies scope in
  Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering =
    Dependency.correct_computation_ordering scope_dependencies
  in
  let scope_decl_rules =
    List.flatten
      (List.map
         (fun vertex ->
           match vertex with
           | Dependency.Vertex.Var (var, state) -> (
             let scope_def =
               Ast.ScopeDefMap.find
                 (Ast.ScopeDef.Var (var, state))
                 scope.scope_defs
             in
             let var_def = scope_def.scope_def_rules in
             let var_typ = scope_def.scope_def_typ in
             let is_cond = scope_def.scope_def_is_condition in
             match Marked.unmark scope_def.Ast.scope_def_io.io_input with
             | OnlyInput when not (Ast.RuleMap.is_empty var_def) ->
               (* If the variable is tagged as input, then it shall not be
                  redefined. *)
               Errors.raise_multispanned_error
                 (( Some "Incriminated variable:",
                    Marked.get_mark (Ast.ScopeVar.get_info var) )
                 :: List.map
                      (fun (rule, _) ->
                        ( Some "Incriminated variable definition:",
                          Marked.get_mark (Ast.RuleName.get_info rule) ))
                      (Ast.RuleMap.bindings var_def))
                 "It is impossible to give a definition to a scope variable \
                  tagged as input."
             | OnlyInput ->
               []
               (* we do not provide any definition for an input-only variable *)
             | _ ->
               let expr_def =
                 translate_def ctx
                   (Ast.ScopeDef.Var (var, state))
                   var_def var_typ scope_def.Ast.scope_def_io ~is_cond
                   ~is_subscope_var:false
               in
               let scope_var =
                 match
                   Ast.ScopeVarMap.find var ctx.scope_var_mapping, state
                 with
                 | WholeVar v, None -> v
                 | States states, Some state -> List.assoc state states
                 | _ -> failwith "should not happen"
               in
               [
                 Scopelang.Ast.Definition
                   ( ( Scopelang.Ast.ScopeVar
                         ( scope_var,
                           Marked.get_mark
                             (Scopelang.Ast.ScopeVar.get_info scope_var) ),
                       Marked.get_mark
                         (Scopelang.Ast.ScopeVar.get_info scope_var) ),
                     var_typ,
                     scope_def.Ast.scope_def_io,
                     expr_def );
               ])
           | Dependency.Vertex.SubScope sub_scope_index ->
             (* Before calling the sub_scope, we need to include all the
                re-definitions of subscope parameters*)
             let sub_scope =
               Scopelang.Ast.SubScopeMap.find sub_scope_index
                 scope.scope_sub_scopes
             in
             let sub_scope_vars_redefs_candidates =
               Ast.ScopeDefMap.filter
                 (fun def_key scope_def ->
                   match def_key with
                   | Ast.ScopeDef.Var _ -> false
                   | Ast.ScopeDef.SubScopeVar (sub_scope_index', _) ->
                     sub_scope_index = sub_scope_index'
                     (* We exclude subscope variables that have 0 re-definitions
                        and are not visible in the input of the subscope *)
                     && not
                          ((match
                              Marked.unmark scope_def.Ast.scope_def_io.io_input
                            with
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
                     (* This definition redefines a variable of the correct
                        subscope. But we have to check that this redefinition is
                        allowed with respect to the io parameters of that
                        subscope variable. *)
                     (match
                        Marked.unmark scope_def.Ast.scope_def_io.io_input
                      with
                     | Scopelang.Ast.NoInput ->
                       Errors.raise_multispanned_error
                         (( Some "Incriminated subscope:",
                            Ast.ScopeDef.get_position def_key )
                         :: ( Some "Incriminated variable:",
                              Marked.get_mark
                                (Ast.ScopeVar.get_info sub_scope_var) )
                         :: List.map
                              (fun (rule, _) ->
                                ( Some
                                    "Incriminated subscope variable definition:",
                                  Marked.get_mark (Ast.RuleName.get_info rule) ))
                              (Ast.RuleMap.bindings def))
                         "It is impossible to give a definition to a subscope \
                          variable not tagged as input or context."
                     | OnlyInput when Ast.RuleMap.is_empty def && not is_cond ->
                       (* If the subscope variable is tagged as input, then it
                          shall be defined. *)
                       Errors.raise_multispanned_error
                         [
                           ( Some "Incriminated subscope:",
                             Ast.ScopeDef.get_position def_key );
                           ( Some "Incriminated variable:",
                             Marked.get_mark
                               (Ast.ScopeVar.get_info sub_scope_var) );
                         ]
                         "This subscope variable is a mandatory input but no \
                          definition was provided."
                     | _ -> ());
                     (* Now that all is good, we can proceed with translating
                        this redefinition to a proper Scopelang term. *)
                     let expr_def =
                       translate_def ctx def_key def def_typ
                         scope_def.Ast.scope_def_io ~is_cond
                         ~is_subscope_var:true
                     in
                     let subscop_real_name =
                       Scopelang.Ast.SubScopeMap.find sub_scope_index
                         scope.scope_sub_scopes
                     in
                     let var_pos =
                       Marked.get_mark (Ast.ScopeVar.get_info sub_scope_var)
                     in
                     Scopelang.Ast.Definition
                       ( ( Scopelang.Ast.SubScopeVar
                             ( subscop_real_name,
                               (sub_scope_index, var_pos),
                               match
                                 Ast.ScopeVarMap.find sub_scope_var
                                   ctx.scope_var_mapping
                               with
                               | WholeVar v -> v, var_pos
                               | States states ->
                                 (* When defining a sub-scope variable, we
                                    always define its first state in the
                                    sub-scope. *)
                                 snd (List.hd states), var_pos ),
                           var_pos ),
                         def_typ,
                         scope_def.Ast.scope_def_io,
                         expr_def ))
                 sub_scope_vars_redefs_candidates
             in
             let sub_scope_vars_redefs =
               List.map snd (Ast.ScopeDefMap.bindings sub_scope_vars_redefs)
             in
             sub_scope_vars_redefs
             @ [Scopelang.Ast.Call (sub_scope, sub_scope_index)])
         scope_ordering)
  in
  (* Then, after having computed all the scopes variables, we add the
     assertions. TODO: the assertions should be interleaved with the
     definitions! *)
  let scope_decl_rules =
    scope_decl_rules
    @ List.map
        (fun e ->
          let scope_e = translate_expr ctx e in
          Bindlib.unbox
            (Bindlib.box_apply
               (fun scope_e -> Scopelang.Ast.Assertion scope_e)
               scope_e))
        (Bindlib.unbox (Bindlib.box_list scope.Ast.scope_assertions))
  in
  let scope_sig =
    Ast.ScopeVarMap.fold
      (fun var (states : Ast.var_or_states) acc ->
        match states with
        | WholeVar ->
          let scope_def =
            Ast.ScopeDefMap.find (Ast.ScopeDef.Var (var, None)) scope.scope_defs
          in
          let typ = scope_def.scope_def_typ in
          Scopelang.Ast.ScopeVarMap.add
            (match Ast.ScopeVarMap.find var ctx.scope_var_mapping with
            | WholeVar v -> v
            | States _ -> failwith "should not happen")
            (typ, scope_def.scope_def_io)
            acc
        | States states ->
          (* What happens in the case of variables with multiple states is
             interesting. We need to create as many Scopelang.Var entries in the
             scope signature as there are states. *)
          List.fold_left
            (fun acc (state : Ast.StateName.t) ->
              let scope_def =
                Ast.ScopeDefMap.find
                  (Ast.ScopeDef.Var (var, Some state))
                  scope.scope_defs
              in
              Scopelang.Ast.ScopeVarMap.add
                (match Ast.ScopeVarMap.find var ctx.scope_var_mapping with
                | WholeVar _ -> failwith "should not happen"
                | States states' -> List.assoc state states')
                (scope_def.scope_def_typ, scope_def.scope_def_io)
                acc)
            acc states)
      scope.scope_vars Scopelang.Ast.ScopeVarMap.empty
  in
  {
    Scopelang.Ast.scope_decl_name = scope.scope_uid;
    Scopelang.Ast.scope_decl_rules;
    Scopelang.Ast.scope_sig;
  }

(** {1 API} *)

let translate_program (pgrm : Ast.program) : Scopelang.Ast.program =
  (* First we give mappings to all the locations between Desugared and
     Scopelang. This involves creating a new Scopelang scope variable for every
     state of a Desugared variable. *)
  let ctx =
    Scopelang.Ast.ScopeMap.fold
      (fun _scope scope_decl ctx ->
        Ast.ScopeVarMap.fold
          (fun scope_var (states : Ast.var_or_states) ctx ->
            match states with
            | Ast.WholeVar ->
              {
                ctx with
                scope_var_mapping =
                  Ast.ScopeVarMap.add scope_var
                    (WholeVar
                       (Scopelang.Ast.ScopeVar.fresh
                          (Ast.ScopeVar.get_info scope_var)))
                    ctx.scope_var_mapping;
              }
            | States states ->
              {
                ctx with
                scope_var_mapping =
                  Ast.ScopeVarMap.add scope_var
                    (States
                       (List.map
                          (fun state ->
                            ( state,
                              Scopelang.Ast.ScopeVar.fresh
                                (let state_name, state_pos =
                                   Ast.StateName.get_info state
                                 in
                                 ( Marked.unmark
                                     (Ast.ScopeVar.get_info scope_var)
                                   ^ "_"
                                   ^ state_name,
                                   state_pos )) ))
                          states))
                    ctx.scope_var_mapping;
              })
          scope_decl.Ast.scope_vars ctx)
      pgrm.Ast.program_scopes
      {
        scope_var_mapping = Ast.ScopeVarMap.empty;
        var_mapping = Ast.VarMap.empty;
      }
  in
  {
    Scopelang.Ast.program_scopes =
      Scopelang.Ast.ScopeMap.map (translate_scope ctx) pgrm.program_scopes;
    Scopelang.Ast.program_structs = pgrm.program_structs;
    Scopelang.Ast.program_enums = pgrm.program_enums;
  }
