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
module D = Desugared.Ast

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
  if Cli.globals.trace then
    Expr.eapp
      (Expr.eop (Log (l, markings)) [TAny, Expr.pos e] (Mark.get e))
      [e] (Mark.get e)
  else e

let rec translate_expr (ctx : ctx) (e : Desugared.Ast.expr) :
    untyped Ast.expr boxed =
  let m = Mark.get e in
  match Mark.remove e with
  | EVar v -> Expr.evar (Var.Map.find v ctx.var_mapping) m
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
  | ELocation (SubScopeVar (s_name, ss_name, s_var)) ->
    (* When referring to a subscope variable in an expression, we are referring
       to the output, hence we take the last state. *)
    let new_s_var =
      match ScopeVar.Map.find (Mark.remove s_var) ctx.scope_var_mapping with
      | WholeVar new_s_var -> Mark.copy s_var new_s_var
      | States states -> Mark.copy s_var (snd (List.hd (List.rev states)))
    in
    Expr.elocation (SubScopeVar (s_name, ss_name, new_s_var)) m
  | ELocation (DesugaredScopeVar (s_var, None)) ->
    Expr.elocation
      (ScopelangScopeVar
         (match ScopeVar.Map.find (Mark.remove s_var) ctx.scope_var_mapping with
         | WholeVar new_s_var -> Mark.copy s_var new_s_var
         | States _ -> failwith "should not happen"))
      m
  | ELocation (DesugaredScopeVar (s_var, Some state)) ->
    Expr.elocation
      (ScopelangScopeVar
         (match ScopeVar.Map.find (Mark.remove s_var) ctx.scope_var_mapping with
         | WholeVar _ -> failwith "should not happen"
         | States states -> Mark.copy s_var (List.assoc state states)))
      m
  | ELocation (ToplevelVar v) -> Expr.elocation (ToplevelVar v) m
  | EDStructAccess { name_opt = None; _ } ->
    (* Note: this could only happen if disambiguation was disabled. If we want
       to support it, we should still allow this case when the field has only
       one possible matching structure *)
    Message.raise_spanned_error (Expr.mark_pos m)
      "Ambiguous structure field access"
  | EDStructAccess { e; field; name_opt = Some name } ->
    let e' = translate_expr ctx e in
    let field =
      try
        StructName.Map.find name
          (Ident.Map.find field ctx.decl_ctx.ctx_struct_fields)
      with Not_found ->
        (* Should not happen after disambiguation *)
        Message.raise_spanned_error (Expr.mark_pos m)
          "Field @{<yellow>\"%s\"@} does not belong to structure \
           @{<yellow>\"%a\"@}"
          field StructName.format_t name
    in
    Expr.estructaccess e' field name m
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
  | EApp { f = EOp { op; tys }, m1; args } ->
    let args = List.map (translate_expr ctx) args in
    Operator.kind_dispatch op
      ~monomorphic:(fun op -> Expr.eapp (Expr.eop op tys m1) args m)
      ~polymorphic:(fun op -> Expr.eapp (Expr.eop op tys m1) args m)
      ~overloaded:(fun op ->
        match
          Operator.resolve_overload ctx.decl_ctx (Mark.add (Expr.pos e) op) tys
        with
        | op, `Straight -> Expr.eapp (Expr.eop op tys m1) args m
        | op, `Reversed ->
          Expr.eapp (Expr.eop op (List.rev tys) m1) (List.rev args) m)
  | EOp _ -> assert false (* Only allowed within [EApp] *)
  | ( EStruct _ | ETuple _ | ETupleAccess _ | EInj _ | EMatch _ | ELit _
    | EApp _ | EDefault _ | EIfThenElse _ | EArray _ | EEmptyError
    | EErrorOnEmpty _ | EExternal _ ) as e ->
    Expr.map ~f:(translate_expr ctx) (e, m)

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
let def_to_exception_graph
    (def_info : Desugared.Ast.ScopeDef.t)
    (def : Desugared.Ast.rule RuleName.Map.t) :
    Desugared.Dependency.ExceptionsDependencies.t =
  let exc_graph = Desugared.Dependency.build_exceptions_graph def def_info in
  Desugared.Dependency.check_for_exception_cycle def exc_graph;
  exc_graph

let rule_to_exception_graph (scope : Desugared.Ast.scope) = function
  | Desugared.Dependency.Vertex.Var (var, state) -> (
    let scope_def =
      Desugared.Ast.ScopeDef.Map.find
        (Desugared.Ast.ScopeDef.Var (var, state))
        scope.scope_defs
    in
    let var_def = scope_def.D.scope_def_rules in
    match Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input with
    | OnlyInput when not (RuleName.Map.is_empty var_def) ->
      (* If the variable is tagged as input, then it shall not be redefined. *)
      Message.raise_multispanned_error
        ((Some "Incriminated variable:", Mark.get (ScopeVar.get_info var))
        :: List.map
             (fun (rule, _) ->
               ( Some "Incriminated variable definition:",
                 Mark.get (RuleName.get_info rule) ))
             (RuleName.Map.bindings var_def))
        "It is impossible to give a definition to a scope variable tagged as \
         input."
    | OnlyInput -> Desugared.Ast.ScopeDef.Map.empty
    (* we do not provide any definition for an input-only variable *)
    | _ ->
      Desugared.Ast.ScopeDef.Map.singleton
        (Desugared.Ast.ScopeDef.Var (var, state))
        (def_to_exception_graph
           (Desugared.Ast.ScopeDef.Var (var, state))
           var_def))
  | Desugared.Dependency.Vertex.SubScope sub_scope_index ->
    (* Before calling the sub_scope, we need to include all the re-definitions
       of subscope parameters*)
    let sub_scope_vars_redefs_candidates =
      Desugared.Ast.ScopeDef.Map.filter
        (fun def_key scope_def ->
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> false
          | Desugared.Ast.ScopeDef.SubScopeVar (sub_scope_index', _, _) ->
            sub_scope_index = sub_scope_index'
            (* We exclude subscope variables that have 0 re-definitions and are
               not visible in the input of the subscope *)
            && not
                 ((match
                     Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input
                   with
                  | NoInput -> true
                  | _ -> false)
                 && RuleName.Map.is_empty scope_def.scope_def_rules))
        scope.scope_defs
    in
    let sub_scope_vars_redefs =
      Desugared.Ast.ScopeDef.Map.mapi
        (fun def_key scope_def ->
          let def = scope_def.Desugared.Ast.scope_def_rules in
          let is_cond = scope_def.scope_def_is_condition in
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> assert false (* should not happen *)
          | Desugared.Ast.ScopeDef.SubScopeVar (sscope, sub_scope_var, pos) ->
            (* This definition redefines a variable of the correct subscope. But
               we have to check that this redefinition is allowed with respect
               to the io parameters of that subscope variable. *)
            (match
               Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input
             with
            | NoInput ->
              Message.raise_multispanned_error
                (( Some "Incriminated subscope:",
                   Mark.get (SubScopeName.get_info sscope) )
                :: ( Some "Incriminated variable:",
                     Mark.get (ScopeVar.get_info sub_scope_var) )
                :: List.map
                     (fun (rule, _) ->
                       ( Some "Incriminated subscope variable definition:",
                         Mark.get (RuleName.get_info rule) ))
                     (RuleName.Map.bindings def))
                "It is impossible to give a definition to a subscope variable \
                 not tagged as input or context."
            | OnlyInput when RuleName.Map.is_empty def && not is_cond ->
              (* If the subscope variable is tagged as input, then it shall be
                 defined. *)
              Message.raise_multispanned_error
                [
                  ( Some "Incriminated subscope:",
                    Mark.get (SubScopeName.get_info sscope) );
                  Some "Incriminated variable:", pos;
                ]
                "This subscope variable is a mandatory input but no definition \
                 was provided."
            | _ -> ());
            let exc_graph = def_to_exception_graph def_key def in
            let var_pos = Desugared.Ast.ScopeDef.get_position def_key in
            exc_graph, sub_scope_var, var_pos)
        sub_scope_vars_redefs_candidates
    in
    List.fold_left
      (fun exc_graphs (new_exc_graph, subscope_var, var_pos) ->
        Desugared.Ast.ScopeDef.Map.add
          (Desugared.Ast.ScopeDef.SubScopeVar
             (sub_scope_index, subscope_var, var_pos))
          new_exc_graph exc_graphs)
      Desugared.Ast.ScopeDef.Map.empty
      (List.map snd (Desugared.Ast.ScopeDef.Map.bindings sub_scope_vars_redefs))
  | Assertion _ ->
    Desugared.Ast.ScopeDef.Map.empty (* no exceptions for assertions *)

let scope_to_exception_graphs (scope : Desugared.Ast.scope) :
    Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t =
  let scope_dependencies =
    Desugared.Dependency.build_scope_dependencies scope
  in
  Desugared.Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering =
    Desugared.Dependency.correct_computation_ordering scope_dependencies
  in
  List.fold_left
    (fun exceptions_graphs scope_def_key ->
      let new_exceptions_graphs = rule_to_exception_graph scope scope_def_key in
      Desugared.Ast.ScopeDef.Map.union
        (fun _ _ _ -> assert false (* there should not be key conflicts *))
        new_exceptions_graphs exceptions_graphs)
    Desugared.Ast.ScopeDef.Map.empty scope_ordering

let build_exceptions_graph (pgrm : Desugared.Ast.program) :
    Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t =
  ScopeName.Map.fold
    (fun _ scope exceptions_graph ->
      let new_exceptions_graphs = scope_to_exception_graphs scope in
      Desugared.Ast.ScopeDef.Map.union
        (fun _ _ _ -> assert false (* key conflicts should not happen*))
        new_exceptions_graphs exceptions_graph)
    pgrm.program_scopes Desugared.Ast.ScopeDef.Map.empty

(** Transforms a flat list of rules into a tree, taking into account the
    priorities declared between rules *)
let def_map_to_tree
    (def : Desugared.Ast.rule RuleName.Map.t)
    (exc_graph : Desugared.Dependency.ExceptionsDependencies.t) : rule_tree list
    =
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
  let rec build_tree (base_cases : Desugared.Dependency.ExceptionVertex.t) :
      rule_tree =
    let exceptions =
      Desugared.Dependency.ExceptionsDependencies.pred exc_graph base_cases
    in
    let base_case_as_rule_list =
      List.map
        (fun (r, _) -> RuleName.Map.find r def)
        (RuleName.Map.bindings base_cases.rules)
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
    (params : Desugared.Ast.expr Var.t list option)
    (tree : rule_tree) : untyped Ast.expr boxed =
  let emark = Untyped { pos = def_pos } in
  let exceptions, base_rules =
    match tree with Leaf r -> [], r | Node (exceptions, r) -> exceptions, r
  in
  (* because each rule has its own variables parameters and we want to convert
     the whole rule tree into a function, we need to perform some alpha-renaming
     of all the expressions *)
  let substitute_parameter
      (e : Desugared.Ast.expr boxed)
      (rule : Desugared.Ast.rule) : Desugared.Ast.expr boxed =
    match params, rule.Desugared.Ast.rule_parameter with
    | Some new_params, Some (old_params_with_types, _) ->
      let old_params, _ = List.split old_params_with_types in
      let old_params = Array.of_list (List.map Mark.remove old_params) in
      let new_params = Array.of_list new_params in
      let binder = Bindlib.bind_mvar old_params (Mark.remove e) in
      Mark.add (Mark.get e)
      @@ Bindlib.box_apply2
           (fun binder new_param -> Bindlib.msubst binder new_param)
           binder
           (new_params |> Array.map Bindlib.box_var |> Bindlib.box_array)
    | None, None -> e
    | _ -> assert false
    (* should not happen *)
  in
  let ctx =
    match params with
    | None -> ctx
    | Some new_params ->
      ListLabels.fold_left new_params ~init:ctx ~f:(fun ctx new_param ->
          match Var.Map.find_opt new_param ctx.var_mapping with
          | None ->
            let new_param_scope = Var.make (Bindlib.name_of new_param) in
            {
              ctx with
              var_mapping =
                Var.Map.add new_param new_param_scope ctx.var_mapping;
            }
          | Some _ ->
            (* We only create a mapping if none exists because
               [rule_tree_to_expr] is called recursively on the exceptions of
               the tree and we don't want to create a new Scopelang variable for
               the parameter at each tree level. *)
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
      (Expr.eemptyerror emark) emark
  in
  let exceptions =
    List.map
      (rule_tree_to_expr ~toplevel:false ~is_reentrant_var ctx def_pos params)
      exceptions
  in
  let default =
    Expr.make_default exceptions
      (Expr.elit (LBool true) emark)
      default_containing_base_cases emark
  in
  match params, (List.hd base_rules).Desugared.Ast.rule_parameter with
  | None, None -> default
  | Some new_params, Some (ls, _) ->
    let _, tys = List.split ls in
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
        (new_params
        |> List.map (fun x -> Var.Map.find x ctx.var_mapping)
        |> Array.of_list)
        default tys def_pos
    else default
  | _ -> (* should not happen *) assert false

(** {1 AST translation} *)

(** Translates a definition inside a scope, the resulting expression should be
    an {!constructor: Dcalc.EDefault} *)
let translate_def
    ~(is_cond : bool)
    ~(is_subscope_var : bool)
    (ctx : ctx)
    (def_info : Desugared.Ast.ScopeDef.t)
    (def : Desugared.Ast.rule RuleName.Map.t)
    (params : (Uid.MarkedString.info * typ) list Mark.pos option)
    (typ : typ)
    (io : Desugared.Ast.io)
    (exc_graph : Desugared.Dependency.ExceptionsDependencies.t) :
    untyped Ast.expr boxed =
  (* Here, we have to transform this list of rules into a default tree. *)
  let top_list = def_map_to_tree def exc_graph in
  let is_input =
    match Mark.remove io.Desugared.Ast.io_input with
    | OnlyInput -> true
    | _ -> false
  in
  let is_reentrant =
    match Mark.remove io.Desugared.Ast.io_input with
    | Reentrant -> true
    | _ -> false
  in
  let top_value : Desugared.Ast.rule option =
    if is_cond && ((not is_subscope_var) || (is_subscope_var && is_input)) then
      (* We add the bottom [false] value for conditions, only for the scope
         where the condition is declared. Except when the variable is an input,
         where we want the [false] to be added at each caller parent scope. *)
      Some
        (Desugared.Ast.always_false_rule
           (Desugared.Ast.ScopeDef.get_position def_info)
           params)
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
    let empty_error = Expr.eemptyerror m in
    match params with
    | Some (ps, _) ->
      let labels, tys = List.split ps in
      Expr.make_abs
        (Array.of_list
           (List.map (fun lbl -> Var.make (Mark.remove lbl)) labels))
        empty_error tys (Expr.mark_pos m)
    | _ -> empty_error
  else
    rule_tree_to_expr ~toplevel:true ~is_reentrant_var:is_reentrant ctx
      (Desugared.Ast.ScopeDef.get_position def_info)
      (Option.map
         (fun (ps, _) ->
           (List.map (fun (lbl, _) -> Var.make (Mark.remove lbl))) ps)
         params)
      (match top_list, top_value with
      | [], None ->
        (* In this case, there are no rules to define the expression and no
           default value so we put an empty rule. *)
        Leaf [Desugared.Ast.empty_rule (Mark.get typ) params]
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
        Node (top_list, [Desugared.Ast.empty_rule (Mark.get typ) params]))

let translate_rule
    ctx
    (scope : Desugared.Ast.scope)
    (exc_graphs :
      Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t)
    = function
  | Desugared.Dependency.Vertex.Var (var, state) -> (
    let scope_def =
      Desugared.Ast.ScopeDef.Map.find
        (Desugared.Ast.ScopeDef.Var (var, state))
        scope.scope_defs
    in
    let var_def = scope_def.D.scope_def_rules in
    let var_params = scope_def.D.scope_def_parameters in
    let var_typ = scope_def.D.scope_def_typ in
    let is_cond = scope_def.D.scope_def_is_condition in
    match Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input with
    | OnlyInput when not (RuleName.Map.is_empty var_def) ->
      assert false (* error already raised *)
    | OnlyInput -> []
    (* we do not provide any definition for an input-only variable *)
    | _ ->
      let scope_def_key = Desugared.Ast.ScopeDef.Var (var, state) in
      let expr_def =
        translate_def ctx scope_def_key var_def var_params var_typ
          scope_def.Desugared.Ast.scope_def_io
          (Desugared.Ast.ScopeDef.Map.find scope_def_key exc_graphs)
          ~is_cond ~is_subscope_var:false
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
                (scope_var, Mark.get (ScopeVar.get_info scope_var)),
              Mark.get (ScopeVar.get_info scope_var) ),
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
      Desugared.Ast.ScopeDef.Map.filter
        (fun def_key scope_def ->
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> false
          | Desugared.Ast.ScopeDef.SubScopeVar (sub_scope_index', _, _) ->
            sub_scope_index = sub_scope_index'
            (* We exclude subscope variables that have 0 re-definitions and are
               not visible in the input of the subscope *)
            && not
                 ((match
                     Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input
                   with
                  | NoInput -> true
                  | _ -> false)
                 && RuleName.Map.is_empty scope_def.scope_def_rules))
        scope.scope_defs
    in
    let sub_scope_vars_redefs =
      Desugared.Ast.ScopeDef.Map.mapi
        (fun def_key scope_def ->
          let def = scope_def.Desugared.Ast.scope_def_rules in
          let def_typ = scope_def.scope_def_typ in
          let is_cond = scope_def.scope_def_is_condition in
          match def_key with
          | Desugared.Ast.ScopeDef.Var _ -> assert false (* should not happen *)
          | Desugared.Ast.ScopeDef.SubScopeVar (_, sub_scope_var, var_pos) ->
            (* This definition redefines a variable of the correct subscope. But
               we have to check that this redefinition is allowed with respect
               to the io parameters of that subscope variable. *)
            (match
               Mark.remove scope_def.Desugared.Ast.scope_def_io.io_input
             with
            | NoInput -> assert false (* error already raised *)
            | OnlyInput when RuleName.Map.is_empty def && not is_cond ->
              assert false (* error already raised *)
            | _ -> ());
            (* Now that all is good, we can proceed with translating this
               redefinition to a proper Scopelang term. *)
            let expr_def =
              translate_def ctx def_key def scope_def.D.scope_def_parameters
                def_typ scope_def.Desugared.Ast.scope_def_io
                (Desugared.Ast.ScopeDef.Map.find def_key exc_graphs)
                ~is_cond ~is_subscope_var:true
            in
            let subscop_real_name =
              SubScopeName.Map.find sub_scope_index scope.scope_sub_scopes
            in
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
      List.map snd (Desugared.Ast.ScopeDef.Map.bindings sub_scope_vars_redefs)
    in
    sub_scope_vars_redefs
    @ [
        Ast.Call
          ( sub_scope,
            sub_scope_index,
            Untyped { pos = Mark.get (SubScopeName.get_info sub_scope_index) }
          );
      ]
  | Assertion a_name ->
    let assertion_expr =
      Desugared.Ast.AssertionName.Map.find a_name scope.scope_assertions
    in
    (* we unbox here because assertions do not have free variables (at this
       point Bindlib variables are only for fuhnction parameters)*)
    let assertion_expr = translate_expr ctx (Expr.unbox assertion_expr) in
    [Ast.Assertion (Expr.unbox assertion_expr)]

(** Translates a scope *)
let translate_scope
    (ctx : ctx)
    (scope : Desugared.Ast.scope)
    (exc_graphs :
      Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t)
    : untyped Ast.scope_decl =
  let scope_dependencies =
    Desugared.Dependency.build_scope_dependencies scope
  in
  Desugared.Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering =
    Desugared.Dependency.correct_computation_ordering scope_dependencies
  in
  let scope_decl_rules =
    List.fold_left
      (fun scope_decl_rules scope_def_key ->
        let new_rules = translate_rule ctx scope exc_graphs scope_def_key in
        scope_decl_rules @ new_rules)
      [] scope_ordering
  in
  let scope_sig =
    ScopeVar.Map.fold
      (fun var (states : Desugared.Ast.var_or_states) acc ->
        match states with
        | WholeVar ->
          let scope_def =
            Desugared.Ast.ScopeDef.Map.find
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
                Desugared.Ast.ScopeDef.Map.find
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
  let pos = Mark.get (ScopeName.get_info scope.scope_uid) in
  {
    Ast.scope_decl_name = scope.scope_uid;
    Ast.scope_decl_rules;
    Ast.scope_sig;
    Ast.scope_mark = Untyped { pos };
    Ast.scope_options = scope.scope_options;
  }

(** {1 API} *)

let translate_program
    (pgrm : Desugared.Ast.program)
    (exc_graphs :
      Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t)
    : untyped Ast.program =
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
                    (Mark.map (( ^ ) var_prefix) (StateName.get_info state))
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
  let program_scopes =
    ScopeName.Map.fold
      (fun scope_name scope new_program_scopes ->
        let new_program_scope = translate_scope ctx scope exc_graphs in
        ScopeName.Map.add scope_name new_program_scope new_program_scopes)
      pgrm.program_scopes ScopeName.Map.empty
  in
  let program_topdefs =
    TopdefName.Map.mapi
      (fun id -> function
        | Some e, ty -> Expr.unbox (translate_expr ctx e), ty
        | None, (_, pos) ->
          Message.raise_spanned_error pos "No definition found for %a"
            TopdefName.format_t id)
      pgrm.program_topdefs
  in
  {
    Ast.program_topdefs;
    program_scopes;
    program_ctx = { pgrm.program_ctx with ctx_scopes };
  }
