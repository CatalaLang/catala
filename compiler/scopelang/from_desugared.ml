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

open Catala_utils
open Shared_ast

(** {1 Expression translation}*)

type target_scope_vars =
  | WholeVar of ScopeVar.t
  | States of (StateName.t * ScopeVar.t) list

type ctx = {
  decl_ctx : decl_ctx;
  scope_var_mapping : target_scope_vars ScopeVar.Map.t;
  var_mapping : (Desugared.Ast.expr, untyped Ast.expr Var.t) Var.Map.t;
}

let tag_with_log_entry
    (e : untyped Ast.expr boxed)
    (l : log_entry)
    (markings : Uid.MarkedString.info list) : untyped Ast.expr boxed =
  Expr.eapp
    (Expr.eop (Log (l, markings)) [TAny, Expr.pos e] (Marked.get_mark e))
    [e] (Marked.get_mark e)

let rec translate_expr (ctx : ctx) (e : Desugared.Ast.expr) :
    untyped Ast.expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | ELocation (SubScopeVar (s_name, ss_name, s_var)) ->
    (* When referring to a subscope variable in an expression, we are referring
       to the output, hence we take the last state. *)
    let new_s_var =
      match ScopeVar.Map.find (Marked.unmark s_var) ctx.scope_var_mapping with
      | WholeVar new_s_var -> Marked.same_mark_as new_s_var s_var
      | States states ->
        Marked.same_mark_as (snd (List.hd (List.rev states))) s_var
    in
    Expr.elocation (SubScopeVar (s_name, ss_name, new_s_var)) m
  | ELocation (DesugaredScopeVar (s_var, None)) ->
    Expr.elocation
      (ScopelangScopeVar
         (match
            ScopeVar.Map.find (Marked.unmark s_var) ctx.scope_var_mapping
          with
         | WholeVar new_s_var -> Marked.same_mark_as new_s_var s_var
         | States _ -> failwith "should not happen"))
      m
  | ELocation (DesugaredScopeVar (s_var, Some state)) ->
    Expr.elocation
      (ScopelangScopeVar
         (match
            ScopeVar.Map.find (Marked.unmark s_var) ctx.scope_var_mapping
          with
         | WholeVar _ -> failwith "should not happen"
         | States states -> Marked.same_mark_as (List.assoc state states) s_var))
      m
  | EVar v -> Expr.evar (Var.Map.find v ctx.var_mapping) m
  | EStruct { name; fields } ->
    Expr.estruct name (StructField.Map.map (translate_expr ctx) fields) m
  | EDStructAccess { name_opt = None; _ } ->
    (* Note: this could only happen if disambiguation was disabled. If we want
       to support it, we should still allow this case when the field has only
       one possible matching structure *)
    Errors.raise_spanned_error (Expr.mark_pos m)
      "Ambiguous structure field access"
  | EDStructAccess { e; field; name_opt = Some name } ->
    let e' = translate_expr ctx e in
    let field =
      try
        StructName.Map.find name
          (IdentName.Map.find field ctx.decl_ctx.ctx_struct_fields)
      with Not_found ->
        (* Should not happen after disambiguation *)
        Errors.raise_spanned_error (Expr.mark_pos m)
          "Field %s does not belong to structure %a" field StructName.format_t
          name
    in
    Expr.estructaccess e' field name m
  | EInj { e; cons; name } -> Expr.einj (translate_expr ctx e) cons name m
  | EMatch { e; name; cases } ->
    Expr.ematch (translate_expr ctx e) name
      (EnumConstructor.Map.map (translate_expr ctx) cases)
      m
  | EScopeCall { scope; args } ->
    Expr.escopecall scope
      (ScopeVar.Map.fold
         (fun v e args' ->
           let v' =
             match ScopeVar.Map.find v ctx.scope_var_mapping with
             | WholeVar v' -> v'
             | States ((_, v') :: _) ->
               (* When there are multiple states, the input is always the first
                  one *)
               v'
             | States [] -> assert false
           in
           ScopeVar.Map.add v' (translate_expr ctx e) args')
         args ScopeVar.Map.empty)
      m
  | ELit
      (( LBool _ | LEmptyError | LInt _ | LRat _ | LMoney _ | LUnit | LDate _
       | LDuration _ ) as l) ->
    Expr.elit l m
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let new_vars = Array.map (fun var -> Var.make (Bindlib.name_of var)) vars in
    let ctx =
      List.fold_left2
        (fun ctx var new_var ->
          { ctx with var_mapping = Var.Map.add var new_var ctx.var_mapping })
        ctx (Array.to_list vars) (Array.to_list new_vars)
    in
    Expr.eabs (Expr.bind new_vars (translate_expr ctx body)) tys m
  | EApp { f = EOp { op; tys }, m1; args } ->
    let args = List.map (translate_expr ctx) args in
    Operator.kind_dispatch op
      ~monomorphic:(fun op -> Expr.eapp (Expr.eop op tys m1) args m)
      ~polymorphic:(fun op -> Expr.eapp (Expr.eop op tys m1) args m)
      ~overloaded:(fun op ->
        match
          Operator.resolve_overload ctx.decl_ctx
            (Marked.mark (Expr.pos e) op)
            tys
        with
        | op, `Straight -> Expr.eapp (Expr.eop op tys m1) args m
        | op, `Reversed ->
          Expr.eapp (Expr.eop op (List.rev tys) m1) (List.rev args) m)
  | EOp _ -> assert false (* Only allowed within [EApp] *)
  | EApp { f; args } ->
    Expr.eapp (translate_expr ctx f) (List.map (translate_expr ctx) args) m
  | EDefault { excepts; just; cons } ->
    Expr.edefault
      (List.map (translate_expr ctx) excepts)
      (translate_expr ctx just) (translate_expr ctx cons) m
  | EIfThenElse { cond; etrue; efalse } ->
    Expr.eifthenelse (translate_expr ctx cond) (translate_expr ctx etrue)
      (translate_expr ctx efalse)
      m
  | EArray args -> Expr.earray (List.map (translate_expr ctx) args) m
  | EErrorOnEmpty e1 -> Expr.eerroronempty (translate_expr ctx e1) m

(** {1 Rule tree construction} *)

(** Intermediate representation for the exception tree of rules for a particular
    scope definition. *)
type rule_tree =
  | Leaf of Desugared.Ast.rule list
      (** Rules defining a base case piecewise. List is non-empty. *)
  | Node of rule_tree list * Desugared.Ast.rule list
      (** [Node (exceptions, base_case)] is a list of exceptions to a non-empty
          list of rules defining a base case piecewise. *)

(** Transforms a flat list of rules into a tree, taking into account the
    priorities declared between rules *)
let def_map_to_tree
    (def_info : Desugared.Ast.ScopeDef.t)
    (def : Desugared.Ast.rule RuleName.Map.t) : rule_tree list =
  let exc_graph = Desugared.Dependency.build_exceptions_graph def def_info in
  Desugared.Dependency.check_for_exception_cycle exc_graph;
  (* we start by the base cases: they are the vertices which have no
     successors *)
  let base_cases =
    Desugared.Dependency.ExceptionsDependencies.fold_vertex
      (fun v base_cases ->
        if
          Desugared.Dependency.ExceptionsDependencies.out_degree exc_graph v = 0
        then v :: base_cases
        else base_cases)
      exc_graph []
  in
  let rec build_tree (base_cases : RuleName.Set.t) : rule_tree =
    let exceptions =
      Desugared.Dependency.ExceptionsDependencies.pred exc_graph base_cases
    in
    let base_case_as_rule_list =
      List.map
        (fun r -> RuleName.Map.find r def)
        (RuleName.Set.elements base_cases)
    in
    match exceptions with
    | [] -> Leaf base_case_as_rule_list
    | _ -> Node (List.map build_tree exceptions, base_case_as_rule_list)
  in
  List.map build_tree base_cases

(** From the {!type: rule_tree}, builds an {!constructor: Dcalc.EDefault}
    expression in the scope language. The [~toplevel] parameter is used to know
    when to place the toplevel binding in the case of functions. *)
let rec rule_tree_to_expr
    ~(toplevel : bool)
    ~(is_reentrant_var : bool)
    (ctx : ctx)
    (def_pos : Pos.t)
    (is_func : Desugared.Ast.expr Var.t option)
    (tree : rule_tree) : untyped Ast.expr boxed =
  let emark = Untyped { pos = def_pos } in
  let exceptions, base_rules =
    match tree with Leaf r -> [], r | Node (exceptions, r) -> exceptions, r
  in
  (* because each rule has its own variable parameter and we want to convert the
     whole rule tree into a function, we need to perform some alpha-renaming of
     all the expressions *)
  let substitute_parameter
      (e : Desugared.Ast.expr boxed)
      (rule : Desugared.Ast.rule) : Desugared.Ast.expr boxed =
    match is_func, rule.Desugared.Ast.rule_parameter with
    | Some new_param, Some (old_param, _) ->
      let binder = Bindlib.bind_var old_param (Marked.unmark e) in
      Marked.mark (Marked.get_mark e)
      @@ Bindlib.box_apply2
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
      match Var.Map.find_opt new_param ctx.var_mapping with
      | None ->
        let new_param_scope = Var.make (Bindlib.name_of new_param) in
        {
          ctx with
          var_mapping = Var.Map.add new_param new_param_scope ctx.var_mapping;
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
      (fun rule -> substitute_parameter rule.Desugared.Ast.rule_just rule)
      base_rules
  in
  let base_cons_list =
    List.map
      (fun rule -> substitute_parameter rule.Desugared.Ast.rule_cons rule)
      base_rules
  in
  let translate_and_unbox_list (list : Desugared.Ast.expr boxed list) :
      untyped Ast.expr boxed list =
    List.map
      (fun e ->
        (* There are two levels of boxing here, the outermost is introduced by
           the [translate_expr] function for which all of the bindings should
           have been closed by now, so we can safely unbox. *)
        translate_expr ctx (Expr.unbox e))
      list
  in
  let default_containing_base_cases =
    Expr.make_default
      (List.map2
         (fun base_just base_cons ->
           Expr.make_default []
             (* Here we insert the logging command that records when a decision
                is taken for the value of a variable. *)
             (tag_with_log_entry base_just PosRecordIfTrueBool [])
             base_cons emark)
         (translate_and_unbox_list base_just_list)
         (translate_and_unbox_list base_cons_list))
      (Expr.elit (LBool false) emark)
      (Expr.elit LEmptyError emark)
      emark
  in
  let exceptions =
    List.map
      (rule_tree_to_expr ~toplevel:false ~is_reentrant_var ctx def_pos is_func)
      exceptions
  in
  let default =
    Expr.make_default exceptions
      (Expr.elit (LBool true) emark)
      default_containing_base_cases emark
  in
  match is_func, (List.hd base_rules).Desugared.Ast.rule_parameter with
  | None, None -> default
  | Some new_param, Some (_, typ) ->
    if toplevel then
      (* When we're creating a function from multiple defaults, we must check
         that the result returned by the function is not empty, unless we're
         dealing with a context variable which is reentrant (either in the
         caller or callee). In this case the ErrorOnEmpty will be added later in
         the scopelang->dcalc translation. *)
      let default =
        if is_reentrant_var then default else Expr.eerroronempty default emark
      in
      Expr.make_abs
        [| Var.Map.find new_param ctx.var_mapping |]
        default [typ] def_pos
    else default
  | _ -> (* should not happen *) assert false

(** {1 AST translation} *)

(** Translates a definition inside a scope, the resulting expression should be
    an {!constructor: Dcalc.EDefault} *)
let translate_def
    (ctx : ctx)
    (def_info : Desugared.Ast.ScopeDef.t)
    (def : Desugared.Ast.rule RuleName.Map.t)
    (typ : typ)
    (io : Desugared.Ast.io)
    ~(is_cond : bool)
    ~(is_subscope_var : bool) : untyped Ast.expr boxed =
  (* Here, we have to transform this list of rules into a default tree. *)
  let is_def_func =
    match Marked.unmark typ with TArrow (_, _) -> true | _ -> false
  in
  let is_rule_func _ (r : Desugared.Ast.rule) : bool =
    Option.is_some r.Desugared.Ast.rule_parameter
  in
  let all_rules_func = RuleName.Map.for_all is_rule_func def in
  let all_rules_not_func =
    RuleName.Map.for_all (fun n r -> not (is_rule_func n r)) def
  in
  let is_def_func_param_typ : typ option =
    if is_def_func && all_rules_func then
      match Marked.unmark typ with
      | TArrow (t_param, _) -> Some t_param
      | _ ->
        Errors.raise_spanned_error (Marked.get_mark typ)
          "The definitions of %a are function but it doesn't have a function \
           type"
          Desugared.Ast.ScopeDef.format_t def_info
    else if (not is_def_func) && all_rules_not_func then None
    else
      let spans =
        List.map
          (fun (_, r) ->
            ( Some "This definition is a function:",
              Expr.pos r.Desugared.Ast.rule_cons ))
          (RuleName.Map.bindings (RuleName.Map.filter is_rule_func def))
        @ List.map
            (fun (_, r) ->
              ( Some "This definition is not a function:",
                Expr.pos r.Desugared.Ast.rule_cons ))
            (RuleName.Map.bindings
               (RuleName.Map.filter (fun n r -> not (is_rule_func n r)) def))
      in
      Errors.raise_multispanned_error spans
        "some definitions of the same variable are functions while others \
         aren't"
  in
  let top_list = def_map_to_tree def_info def in
  let is_input =
    match Marked.unmark io.Desugared.Ast.io_input with
    | OnlyInput -> true
    | _ -> false
  in
  let is_reentrant =
    match Marked.unmark io.Desugared.Ast.io_input with
    | Reentrant -> true
    | _ -> false
  in
  let top_value =
    if is_cond && ((not is_subscope_var) || (is_subscope_var && is_input)) then
      (* We add the bottom [false] value for conditions, only for the scope
         where the condition is declared. Except when the variable is an input,
         where we want the [false] to be added at each caller parent scope. *)
      Some
        (Desugared.Ast.always_false_rule
           (Desugared.Ast.ScopeDef.get_position def_info)
           is_def_func_param_typ)
    else None
  in
  if
    RuleName.Map.cardinal def = 0
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
  then
    let m = Untyped { pos = Desugared.Ast.ScopeDef.get_position def_info } in
    let empty_error = Expr.elit LEmptyError m in
    match is_def_func_param_typ with
    | Some ty ->
      Expr.make_abs [| Var.make "_" |] empty_error [ty] (Expr.mark_pos m)
    | _ -> empty_error
  else
    rule_tree_to_expr ~toplevel:true ~is_reentrant_var:is_reentrant ctx
      (Desugared.Ast.ScopeDef.get_position def_info)
      (Option.map (fun _ -> Var.make "param") is_def_func_param_typ)
      (match top_list, top_value with
      | [], None ->
        (* In this case, there are no rules to define the expression and no
           default value so we put an empty rule. *)
        Leaf
          [Desugared.Ast.empty_rule (Marked.get_mark typ) is_def_func_param_typ]
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
            [
              Desugared.Ast.empty_rule (Marked.get_mark typ)
                is_def_func_param_typ;
            ] ))

let translate_rule ctx (scope : Desugared.Ast.scope) = function
  | Desugared.Dependency.Vertex.Var (var, state) -> (
    let scope_def =
      Desugared.Ast.ScopeDefMap.find
        (Desugared.Ast.ScopeDef.Var (var, state))
        scope.scope_defs
    in
    let var_def = scope_def.scope_def_rules in
    let var_typ = scope_def.scope_def_typ in
    let is_cond = scope_def.scope_def_is_condition in
    match Marked.unmark scope_def.Desugared.Ast.scope_def_io.io_input with
    | OnlyInput when not (RuleName.Map.is_empty var_def) ->
      (* If the variable is tagged as input, then it shall not be redefined. *)
      Errors.raise_multispanned_error
        ((Some "Incriminated variable:", Marked.get_mark (ScopeVar.get_info var))
        :: List.map
             (fun (rule, _) ->
               ( Some "Incriminated variable definition:",
                 Marked.get_mark (RuleName.get_info rule) ))
             (RuleName.Map.bindings var_def))
        "It is impossible to give a definition to a scope variable tagged as \
         input."
    | OnlyInput -> []
    (* we do not provide any definition for an input-only variable *)
    | _ ->
      let expr_def =
        translate_def ctx
          (Desugared.Ast.ScopeDef.Var (var, state))
          var_def var_typ scope_def.Desugared.Ast.scope_def_io ~is_cond
          ~is_subscope_var:false
      in
      let scope_var =
        match ScopeVar.Map.find var ctx.scope_var_mapping, state with
        | WholeVar v, None -> v
        | States states, Some state -> List.assoc state states
        | _ -> failwith "should not happen"
      in
      [
        Ast.Definition
          ( ( ScopelangScopeVar
                (scope_var, Marked.get_mark (ScopeVar.get_info scope_var)),
              Marked.get_mark (ScopeVar.get_info scope_var) ),
            var_typ,
            scope_def.Desugared.Ast.scope_def_io,
            Expr.unbox expr_def );
      ])
  | Desugared.Dependency.Vertex.SubScope sub_scope_index ->
    (* Before calling the sub_scope, we need to include all the re-definitions
       of subscope parameters*)
    let sub_scope =
      SubScopeName.Map.find sub_scope_index scope.scope_sub_scopes
    in
    let sub_scope_vars_redefs_candidates =
      Desugared.Ast.ScopeDefMap.filter
        (fun def_key scope_def ->
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> false
          | Desugared.Ast.ScopeDef.SubScopeVar (sub_scope_index', _, _) ->
            sub_scope_index = sub_scope_index'
            (* We exclude subscope variables that have 0 re-definitions and are
               not visible in the input of the subscope *)
            && not
                 ((match
                     Marked.unmark scope_def.Desugared.Ast.scope_def_io.io_input
                   with
                  | Desugared.Ast.NoInput -> true
                  | _ -> false)
                 && RuleName.Map.is_empty scope_def.scope_def_rules))
        scope.scope_defs
    in
    let sub_scope_vars_redefs =
      Desugared.Ast.ScopeDefMap.mapi
        (fun def_key scope_def ->
          let def = scope_def.Desugared.Ast.scope_def_rules in
          let def_typ = scope_def.scope_def_typ in
          let is_cond = scope_def.scope_def_is_condition in
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> assert false (* should not happen *)
          | Desugared.Ast.ScopeDef.SubScopeVar (sscope, sub_scope_var, pos) ->
            (* This definition redefines a variable of the correct subscope. But
               we have to check that this redefinition is allowed with respect
               to the io parameters of that subscope variable. *)
            (match
               Marked.unmark scope_def.Desugared.Ast.scope_def_io.io_input
             with
            | Desugared.Ast.NoInput ->
              Errors.raise_multispanned_error
                (( Some "Incriminated subscope:",
                   Marked.get_mark (SubScopeName.get_info sscope) )
                :: ( Some "Incriminated variable:",
                     Marked.get_mark (ScopeVar.get_info sub_scope_var) )
                :: List.map
                     (fun (rule, _) ->
                       ( Some "Incriminated subscope variable definition:",
                         Marked.get_mark (RuleName.get_info rule) ))
                     (RuleName.Map.bindings def))
                "It is impossible to give a definition to a subscope variable \
                 not tagged as input or context."
            | OnlyInput when RuleName.Map.is_empty def && not is_cond ->
              (* If the subscope variable is tagged as input, then it shall be
                 defined. *)
              Errors.raise_multispanned_error
                [
                  ( Some "Incriminated subscope:",
                    Marked.get_mark (SubScopeName.get_info sscope) );
                  Some "Incriminated variable:", pos;
                ]
                "This subscope variable is a mandatory input but no definition \
                 was provided."
            | _ -> ());
            (* Now that all is good, we can proceed with translating this
               redefinition to a proper Scopelang term. *)
            let expr_def =
              translate_def ctx def_key def def_typ
                scope_def.Desugared.Ast.scope_def_io ~is_cond
                ~is_subscope_var:true
            in
            let subscop_real_name =
              SubScopeName.Map.find sub_scope_index scope.scope_sub_scopes
            in
            let var_pos = Desugared.Ast.ScopeDef.get_position def_key in
            Ast.Definition
              ( ( SubScopeVar
                    ( subscop_real_name,
                      (sub_scope_index, var_pos),
                      match
                        ScopeVar.Map.find sub_scope_var ctx.scope_var_mapping
                      with
                      | WholeVar v -> v, var_pos
                      | States states ->
                        (* When defining a sub-scope variable, we always define
                           its first state in the sub-scope. *)
                        snd (List.hd states), var_pos ),
                  var_pos ),
                def_typ,
                scope_def.Desugared.Ast.scope_def_io,
                Expr.unbox expr_def ))
        sub_scope_vars_redefs_candidates
    in
    let sub_scope_vars_redefs =
      List.map snd (Desugared.Ast.ScopeDefMap.bindings sub_scope_vars_redefs)
    in
    sub_scope_vars_redefs
    @ [
        Ast.Call
          ( sub_scope,
            sub_scope_index,
            Untyped
              { pos = Marked.get_mark (SubScopeName.get_info sub_scope_index) }
          );
      ]

(** Translates a scope *)
let translate_scope (ctx : ctx) (scope : Desugared.Ast.scope) :
    untyped Ast.scope_decl =
  let scope_dependencies =
    Desugared.Dependency.build_scope_dependencies scope
  in
  Desugared.Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering =
    Desugared.Dependency.correct_computation_ordering scope_dependencies
  in
  let scope_decl_rules =
    List.flatten (List.map (translate_rule ctx scope) scope_ordering)
  in
  (* Then, after having computed all the scopes variables, we add the
     assertions. TODO: the assertions should be interleaved with the
     definitions! *)
  let scope_decl_rules =
    scope_decl_rules
    @ List.map
        (fun e ->
          let scope_e = translate_expr ctx (Expr.unbox e) in
          Ast.Assertion (Expr.unbox scope_e))
        scope.Desugared.Ast.scope_assertions
  in
  let scope_sig =
    ScopeVar.Map.fold
      (fun var (states : Desugared.Ast.var_or_states) acc ->
        match states with
        | WholeVar ->
          let scope_def =
            Desugared.Ast.ScopeDefMap.find
              (Desugared.Ast.ScopeDef.Var (var, None))
              scope.scope_defs
          in
          let typ = scope_def.scope_def_typ in
          ScopeVar.Map.add
            (match ScopeVar.Map.find var ctx.scope_var_mapping with
            | WholeVar v -> v
            | States _ -> failwith "should not happen")
            (typ, scope_def.scope_def_io)
            acc
        | States states ->
          (* What happens in the case of variables with multiple states is
             interesting. We need to create as many Var entries in the scope
             signature as there are states. *)
          List.fold_left
            (fun acc (state : StateName.t) ->
              let scope_def =
                Desugared.Ast.ScopeDefMap.find
                  (Desugared.Ast.ScopeDef.Var (var, Some state))
                  scope.scope_defs
              in
              ScopeVar.Map.add
                (match ScopeVar.Map.find var ctx.scope_var_mapping with
                | WholeVar _ -> failwith "should not happen"
                | States states' -> List.assoc state states')
                (scope_def.scope_def_typ, scope_def.scope_def_io)
                acc)
            acc states)
      scope.scope_vars ScopeVar.Map.empty
  in
  let pos = Marked.get_mark (ScopeName.get_info scope.scope_uid) in
  {
    Ast.scope_decl_name = scope.scope_uid;
    Ast.scope_decl_rules;
    Ast.scope_sig;
    Ast.scope_mark = Untyped { pos };
  }

(** {1 API} *)

let translate_program (pgrm : Desugared.Ast.program) : untyped Ast.program =
  (* First we give mappings to all the locations between Desugared and This
     involves creating a new Scopelang scope variable for every state of a
     Desugared variable. *)
  let ctx =
    (* Todo: since we rename all scope vars at this point, it would be better to
       have different types for Desugared.ScopeVar.t and Scopelang.ScopeVar.t *)
    ScopeName.Map.fold
      (fun _scope scope_decl ctx ->
        ScopeVar.Map.fold
          (fun scope_var (states : Desugared.Ast.var_or_states) ctx ->
            let var_name, var_pos = ScopeVar.get_info scope_var in
            let new_var =
              match states with
              | Desugared.Ast.WholeVar ->
                WholeVar (ScopeVar.fresh (var_name, var_pos))
              | States states ->
                let var_prefix = var_name ^ "_" in
                let state_var state =
                  ScopeVar.fresh
                    (Marked.map_under_mark (( ^ ) var_prefix)
                       (StateName.get_info state))
                in
                States (List.map (fun state -> state, state_var state) states)
            in
            {
              ctx with
              scope_var_mapping =
                ScopeVar.Map.add scope_var new_var ctx.scope_var_mapping;
            })
          scope_decl.Desugared.Ast.scope_vars ctx)
      pgrm.Desugared.Ast.program_scopes
      {
        scope_var_mapping = ScopeVar.Map.empty;
        var_mapping = Var.Map.empty;
        decl_ctx = pgrm.program_ctx;
      }
  in
  let ctx_scopes =
    ScopeName.Map.map
      (fun out_str ->
        let out_struct_fields =
          ScopeVar.Map.fold
            (fun var fld out_map ->
              let var' =
                match ScopeVar.Map.find var ctx.scope_var_mapping with
                | WholeVar v -> v
                | States l -> snd (List.hd (List.rev l))
              in
              ScopeVar.Map.add var' fld out_map)
            out_str.out_struct_fields ScopeVar.Map.empty
        in
        { out_str with out_struct_fields })
      pgrm.Desugared.Ast.program_ctx.ctx_scopes
  in
  {
    Ast.program_scopes =
      ScopeName.Map.map (translate_scope ctx) pgrm.program_scopes;
    program_ctx = { pgrm.program_ctx with ctx_scopes };
  }
