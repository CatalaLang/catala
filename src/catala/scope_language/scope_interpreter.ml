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

(** The vertices of the scope dependency graph are either :

    - the variables of the scope ;
    - the subscopes of the scope.

    Indeed, during interpretation, subscopes are executed atomically.

    In the graph, x -> y if x is used in the definition of y. *)

module Vertex = struct
  type t = Var of Uid.Var.t | SubScope of Uid.SubScope.t

  let hash x = match x with Var x -> Uid.Var.hash x | SubScope x -> Uid.SubScope.hash x

  let compare = compare

  let equal x y =
    match (x, y) with
    | Var x, Var y -> Uid.Var.compare x y = 0
    | SubScope x, SubScope y -> Uid.SubScope.compare x y = 0
    | _ -> false
end

(** On the edges, the label is the expression responsible for the use of the variable *)
module Edge = struct
  type t = Pos.t

  let compare = compare

  let default = Pos.no_pos
end

module ScopeDependencies = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
module TopologicalTraversal = Graph.Topological.Make (ScopeDependencies)

module SCC = Graph.Components.Make (ScopeDependencies)
(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)

(** Outputs an error in case of cycles. *)
let check_for_cycle (g : ScopeDependencies.t) : unit =
  (* if there is a cycle, there will be an strongly connected component of cardinality > 1 *)
  let sccs = SCC.scc_list g in
  if List.length sccs < ScopeDependencies.nb_vertex g then
    let scc = List.find (fun scc -> List.length scc > 1) sccs in
    Errors.raise_multispanned_error "cyclic dependency dected between variables!"
      (List.flatten
         (List.map
            (fun v ->
              let var_str, var_info =
                match v with
                | Vertex.Var v -> (Uid.Var.format_t v, Uid.Var.get_info v)
                | Vertex.SubScope v -> (Uid.SubScope.format_t v, Uid.SubScope.get_info v)
              in
              let succs = ScopeDependencies.succ_e g v in
              let _, edge_pos, succ = List.find (fun (_, _, succ) -> List.mem succ scc) succs in
              let succ_str =
                match succ with
                | Vertex.Var v -> Uid.Var.format_t v
                | Vertex.SubScope v -> Uid.SubScope.format_t v
              in
              [
                (Some ("cycle variable " ^ var_str ^ ", declared:"), Pos.get_position var_info);
                ( Some ("used here in the definition of another cycle variable " ^ succ_str ^ ":"),
                  edge_pos );
              ])
            scc))

let build_scope_dependencies (scope : Scope_ast.scope) (ctxt : Name_resolution.context) :
    ScopeDependencies.t =
  let g = ScopeDependencies.empty in
  let scope_uid = scope.scope_uid in
  (* Add all the vertices to the graph *)
  let scope_ctxt = Uid.ScopeMap.find scope_uid ctxt.scopes in
  let g =
    Uid.IdentMap.fold
      (fun _ (v : Uid.Var.t) g -> ScopeDependencies.add_vertex g (Vertex.Var v))
      scope_ctxt.var_idmap g
  in
  let g =
    Uid.IdentMap.fold
      (fun _ (v : Uid.SubScope.t) g -> ScopeDependencies.add_vertex g (Vertex.SubScope v))
      scope_ctxt.sub_scopes_idmap g
  in
  let g =
    Uid.ScopeDefMap.fold
      (fun def_key def g ->
        let fv = Lambda_ast.term_fv def in
        Uid.ScopeDefSet.fold
          (fun fv_def g ->
            match (def_key, fv_def) with
            | Uid.ScopeDef.Var defined, Uid.ScopeDef.Var used ->
                (* simple case *)
                ScopeDependencies.add_edge g (Vertex.Var used) (Vertex.Var defined)
            | Uid.ScopeDef.SubScopeVar (defined, _), Uid.ScopeDef.Var used ->
                (* here we are defining the input of a subscope using a var of the scope *)
                ScopeDependencies.add_edge g (Vertex.Var used) (Vertex.SubScope defined)
            | Uid.ScopeDef.SubScopeVar (defined, _), Uid.ScopeDef.SubScopeVar (used, _) ->
                (* here we are defining the input of a scope with the output of another subscope *)
                ScopeDependencies.add_edge g (Vertex.SubScope used) (Vertex.SubScope defined)
            | Uid.ScopeDef.Var defined, Uid.ScopeDef.SubScopeVar (used, _) ->
                (* finally we define a scope var with the output of a subscope *)
                ScopeDependencies.add_edge g (Vertex.SubScope used) (Vertex.Var defined))
          fv g)
      scope.scope_defs g
  in
  g

let rec rewrite_subscope_redef_before_call (ctxt : Name_resolution.context)
    (parent_scope : Uid.Scope.t) (subscope : Scope_ast.scope)
    ((redef, redef_ty) : Scope_ast.definition) : Scope_ast.definition =
  let parent_scope_ctx = Uid.ScopeMap.find parent_scope ctxt.scopes in
  let rec_call = rewrite_subscope_redef_before_call ctxt parent_scope subscope in
  match Pos.unmark redef with
  | Lambda_ast.EVar (prefix, var) -> (
      match prefix with
      | Lambda_ast.NoPrefix ->
          (* this is a variable of the parent scope, we add the prefix *)
          ( Pos.same_pos_as (Lambda_ast.EVar (Lambda_ast.CallerPrefix (1, None), var)) redef,
            redef_ty )
      | Lambda_ast.SubScopePrefix parent_sub ->
          let parent_sub_real = Uid.SubScopeMap.find parent_sub parent_scope_ctx.sub_scopes in
          (* two cases here *)
          if parent_sub_real = subscope.scope_uid then
            (* we remove the prefix since we're calling this precise subscope *)
            (Pos.same_pos_as (Lambda_ast.EVar (Lambda_ast.NoPrefix, var)) redef, redef_ty)
          else
            (* we add the caller prefix*)
            ( Pos.same_pos_as
                (Lambda_ast.EVar (Lambda_ast.CallerPrefix (1, Some parent_sub), var))
                redef,
              redef_ty )
      | Lambda_ast.CallerPrefix (i, grand_parent_sub) ->
          (* In this tricky case, we are trying to call a subscope while being executed as a
             subscope of a "grand-parent" scope. See [tests/scopes/grand_parent_scope.catala] for an
             exemple. What we do in this case is that we propagate the prefix while adding 1 to the
             generation counter *)
          ( Pos.same_pos_as
              (Lambda_ast.EVar (Lambda_ast.CallerPrefix (i + 1, grand_parent_sub), var))
              redef,
            redef_ty ) )
  | Lambda_ast.EInt _ | Lambda_ast.EBool _ | Lambda_ast.EDec _ | Lambda_ast.EOp _
  | Lambda_ast.ELocalVar _ ->
      (redef, redef_ty)
  | Lambda_ast.EFun (bindings, body) ->
      (Pos.same_pos_as (Lambda_ast.EFun (bindings, rec_call body)) redef, redef_ty)
  | Lambda_ast.EApp (f, args) ->
      (Pos.same_pos_as (Lambda_ast.EApp (rec_call f, List.map rec_call args)) redef, redef_ty)
  | Lambda_ast.EIfThenElse (if_t, then_t, else_t) ->
      ( Pos.same_pos_as
          (Lambda_ast.EIfThenElse (rec_call if_t, rec_call then_t, rec_call else_t))
          redef,
        redef_ty )
  | Lambda_ast.EDefault default ->
      ( Pos.same_pos_as
          (Lambda_ast.EDefault
             {
               default with
               defaults = List.map (fun (x, y) -> (rec_call x, rec_call y)) default.defaults;
             })
          redef,
        redef_ty )

(** In this function, the keys of the [redefs] maps are variables of the [subscope] *)
let merge_var_redefs_before_subscope_call (ctxt : Name_resolution.context)
    (parent_scope : Uid.Scope.t) (subscope : Scope_ast.scope)
    (redefs : Scope_ast.definition Uid.VarMap.t) : Scope_ast.scope =
  let merge_defaults : Lambda_ast.term -> Lambda_ast.term -> Lambda_ast.term =
    Lambda_ast.map_untype2 (fun old_t new_t ->
        match (old_t, new_t) with
        | EDefault old_def, EDefault new_def ->
            EDefault (Lambda_ast.merge_default_terms old_def new_def)
        | _ -> assert false
        (* should not happen *))
  in
  (* when merging redefinitions inside a subscope for execution, we need to annotate the variables
     of the parent scope with the caller prefix *)
  {
    subscope with
    scope_defs =
      Uid.VarMap.fold
        (fun new_def_var new_def sub_defs ->
          let new_def = rewrite_subscope_redef_before_call ctxt parent_scope subscope new_def in
          match Uid.ScopeDefMap.find_opt (Uid.ScopeDef.Var new_def_var) sub_defs with
          | None -> Uid.ScopeDefMap.add (Uid.ScopeDef.Var new_def_var) new_def sub_defs
          | Some old_def ->
              let def = merge_defaults old_def new_def in
              Uid.ScopeDefMap.add (Uid.ScopeDef.Var new_def_var) def sub_defs)
        redefs subscope.scope_defs;
  }

let rewrite_context_before_executing_subscope (subscope : Uid.SubScope.t)
    (exec_context : Lambda_interpreter.exec_context) : Lambda_interpreter.exec_context =
  Lambda_interpreter.ExecContext.fold
    (fun key value acc ->
      match key with
      | Lambda_interpreter.ExecContextKey.LocalVar _ ->
          (* we can forget local vars when entering a subscope *)
          acc
      | Lambda_interpreter.ExecContextKey.ScopeVar (prefix, var) ->
          let new_prefix =
            (* note: this has to match with the behavior of [rewrite_subscope_redef_before_call] *)
            match prefix with
            | Lambda_ast.NoPrefix -> Lambda_ast.CallerPrefix (1, None)
            | Lambda_ast.CallerPrefix (i, sub) -> Lambda_ast.CallerPrefix (i + 1, sub)
            | Lambda_ast.SubScopePrefix sub ->
                if sub = subscope then Lambda_ast.NoPrefix else Lambda_ast.CallerPrefix (1, Some sub)
          in
          Lambda_interpreter.ExecContext.add
            (Lambda_interpreter.ExecContextKey.ScopeVar (new_prefix, var))
            value acc)
    exec_context Lambda_interpreter.ExecContext.empty

let rewrite_context_after_executing_subscope (subscope : Uid.SubScope.t)
    (exec_context : Lambda_interpreter.exec_context) : Lambda_interpreter.exec_context =
  Lambda_interpreter.ExecContext.fold
    (fun key value acc ->
      match key with
      | Lambda_interpreter.ExecContextKey.LocalVar _ ->
          (* we can forget local vars when entering a subscope *)
          acc
      | Lambda_interpreter.ExecContextKey.ScopeVar (prefix, var) -> (
          let new_prefix =
            match prefix with
            | Lambda_ast.NoPrefix -> Some (Lambda_ast.SubScopePrefix subscope)
            | Lambda_ast.CallerPrefix (i, sub) -> (
                if i > 1 then Some (Lambda_ast.CallerPrefix (i - 1, sub))
                else
                  match sub with
                  | None -> Some Lambda_ast.NoPrefix
                  | Some sub -> Some (Lambda_ast.SubScopePrefix sub) )
            | Lambda_ast.SubScopePrefix _ -> None
            (* we drop the subscope's subscopes since they can't be accessed *)
          in
          match new_prefix with
          | None -> acc
          | Some new_prefix ->
              Lambda_interpreter.ExecContext.add
                (Lambda_interpreter.ExecContextKey.ScopeVar (new_prefix, var))
                value acc ))
    exec_context Lambda_interpreter.ExecContext.empty

let rec execute_scope ?(exec_context = Lambda_interpreter.empty_exec_ctxt)
    (ctxt : Name_resolution.context) (prgm : Scope_ast.program) (scope_prgm : Scope_ast.scope) :
    Lambda_interpreter.exec_context =
  let scope_ctxt = Uid.ScopeMap.find scope_prgm.scope_uid ctxt.scopes in
  let deps = build_scope_dependencies scope_prgm ctxt in
  check_for_cycle deps;
  TopologicalTraversal.fold
    (fun v exec_context ->
      match v with
      | Vertex.Var var -> (
          match Uid.ScopeDefMap.find_opt (Uid.ScopeDef.Var var) scope_prgm.scope_defs with
          | Some def ->
              (* we evaluate a variable of the scope, no tricky business here *)
              Lambda_interpreter.ExecContext.add
                (Lambda_interpreter.ExecContextKey.ScopeVar (Lambda_ast.NoPrefix, var))
                ( Lambda_interpreter.eval_term (Uid.ScopeDef.Var var) exec_context def
                |> Lambda_ast.untype )
                exec_context
          | None -> assert false (* should not happen *) )
      | Vertex.SubScope subscope_uid ->
          (* this is the tricky case where we have to the the bookkeeping of rewriting the context
             and additional defaults that we pass for the subscope for execution. See formalization
             for more details *)
          let subscope_real_uid = Uid.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes in
          let subscope = Uid.ScopeMap.find subscope_real_uid prgm in
          let redefs_to_include_to_subscope =
            Uid.ScopeDefMap.fold
              (fun def_key def acc ->
                match def_key with
                | Uid.ScopeDef.Var _ -> acc
                | Uid.ScopeDef.SubScopeVar (def_sub_uid, var) ->
                    if def_sub_uid = subscope_uid then Uid.VarMap.add var def acc else acc)
              scope_prgm.scope_defs Uid.VarMap.empty
          in
          let subscope =
            merge_var_redefs_before_subscope_call ctxt scope_prgm.scope_uid subscope
              redefs_to_include_to_subscope
          in
          let exec_context = rewrite_context_before_executing_subscope subscope_uid exec_context in
          let exec_context = execute_scope ~exec_context ctxt prgm subscope in
          rewrite_context_after_executing_subscope subscope_uid exec_context)
    deps exec_context
