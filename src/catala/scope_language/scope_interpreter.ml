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

module G = Graph.Pack.Digraph
module UidMap = Uid.UidMap
module UidSet = Uid.UidSet

(** Returns the scheduling of the scope variables, if y is a subscope and x a variable of y, then we
    have two different variable y.x(internal) and y.x(result) and the ordering y.x(internal) -> y ->
    y.x(result) *)
let build_scope_schedule (ctxt : Name_resolution.context) (scope : Scope_ast.scope) : G.t =
  let g = G.create ~size:100 () in
  let scope_uid = scope.scope_uid in
  (* Add all the vertices to the graph *)
  let vertices =
    UidSet.fold
      (fun uid verts ->
        match Name_resolution.get_uid_sort ctxt uid with
        | IdScopeVar _ | IdSubScope _ -> UidMap.add uid (G.V.create uid) verts
        | _ -> verts)
      (UidMap.find scope_uid ctxt.scopes).uid_set UidMap.empty
  in
  UidMap.iter (fun _ v -> G.add_vertex g v) vertices;
  (* Process definitions dependencies. There are two types of dependencies : var -> var; sub_scope
     -> var *)
  UidMap.iter
    (fun var_uid def ->
      let fv = Lambda_ast.term_fv def in
      UidSet.iter
        (fun uid ->
          if Name_resolution.belongs_to ctxt uid scope_uid then
            let data = UidMap.find uid ctxt.data in
            let from_uid =
              match data.uid_sort with
              | IdScopeVar _ -> uid
              | IdSubScopeVar (_, sub_scope_uid) -> sub_scope_uid
              | _ -> assert false
            in
            G.add_edge g (UidMap.find from_uid vertices) (UidMap.find var_uid vertices)
          else ())
        fv)
    scope.scope_defs;
  (* Process sub-definitions dependencies. Only one kind of dependencies : var -> sub_scopes*)
  UidMap.iter
    (fun sub_scope_uid defs ->
      UidMap.iter
        (fun _ def ->
          let fv = Lambda_ast.term_fv def in
          UidSet.iter
            (fun var_uid ->
              (* Process only uid from the current scope (not the subscope) *)
              if Name_resolution.belongs_to ctxt var_uid scope_uid then
                G.add_edge g (UidMap.find var_uid vertices) (UidMap.find sub_scope_uid vertices)
              else ())
            fv)
        defs)
    scope.scope_sub_defs;
  g

let merge_var_redefs (subscope : Scope_ast.scope) (redefs : Scope_ast.definition UidMap.t) :
    Scope_ast.scope =
  let merge_defaults : Lambda_ast.term -> Lambda_ast.term -> Lambda_ast.term =
    Lambda_ast.map_untype2 (fun old_t new_t ->
        match (old_t, new_t) with
        | EDefault old_def, EDefault new_def ->
            EDefault (Lambda_ast.merge_default_terms old_def new_def)
        | EFun ([ bind ], old_t), EFun (_, new_t) ->
            let body =
              Lambda_ast.map_untype2
                (fun old_t new_t ->
                  match (old_t, new_t) with
                  | EDefault old_def, EDefault new_def ->
                      EDefault (Lambda_ast.merge_default_terms old_def new_def)
                  | _ -> assert false)
                old_t new_t
            in
            EFun ([ bind ], body)
        | _ -> assert false)
  in

  {
    subscope with
    scope_defs =
      UidMap.fold
        (fun uid new_def sub_defs ->
          match UidMap.find_opt uid sub_defs with
          | None -> UidMap.add uid new_def sub_defs
          | Some old_def ->
              let def = merge_defaults old_def new_def in
              UidMap.add uid def sub_defs)
        redefs subscope.scope_defs;
  }

let rec execute_scope ?(exec_context = Lambda_interpreter.empty_exec_ctxt)
    (ctxt : Name_resolution.context) (prgm : Scope_ast.program) (scope_prgm : Scope_ast.scope) :
    Lambda_interpreter.exec_context =
  let schedule = build_scope_schedule ctxt scope_prgm in

  (* Printf.printf "Scheduling : "; *)
  (* G.Topological.iter (fun v_uid -> Printf.printf "%s; " (G.V.label v_uid |> Uid.get_ident))
     schedule; *)
  (* Printf.printf "\n"; *)
  G.Topological.fold
    (fun v_uid exec_context ->
      let uid = G.V.label v_uid in
      match Name_resolution.get_uid_sort ctxt uid with
      | IdScopeVar _ -> (
          match UidMap.find_opt uid scope_prgm.scope_defs with
          | Some def ->
              UidMap.add uid
                (Lambda_interpreter.eval_term uid exec_context def |> Lambda_ast.untype)
                exec_context
          | None ->
              Errors.raise_multispanned_error
                (Printf.sprintf "Variable %s is undefined in scope %s" (Uid.get_ident uid)
                   (Uid.get_ident scope_prgm.scope_uid))
                [ (None, Uid.get_pos scope_prgm.scope_uid); (None, Uid.get_pos uid) ] )
      | IdSubScope sub_scope_ref ->
          (* Merge the new definitions *)
          let sub_scope_prgm =
            match UidMap.find_opt sub_scope_ref prgm with
            | Some sub_scope -> sub_scope
            | None ->
                Errors.raise_multispanned_error
                  (Printf.sprintf
                     "The subscope %s of %s has no definition inside it, and therefore cannot be \
                      executed"
                     (Uid.get_ident scope_prgm.scope_uid)
                     (Uid.get_ident sub_scope_ref))
                  [ (None, Uid.get_pos scope_prgm.scope_uid); (None, Uid.get_pos sub_scope_ref) ]
          in
          let redefs =
            match UidMap.find_opt uid scope_prgm.scope_sub_defs with
            | Some defs -> defs
            | None -> UidMap.empty
          in
          let new_sub_scope_prgm = merge_var_redefs sub_scope_prgm redefs in
          (* Scope_ast.print_scope new_sub_scope_prgm; *)
          let out_context = execute_scope ctxt ~exec_context prgm new_sub_scope_prgm in
          (* Now let's merge back the value from the output context *)
          UidSet.fold
            (fun var_uid exec_context ->
              match Name_resolution.get_uid_sort ctxt var_uid with
              | IdSubScopeVar (ref_uid, scope_ref) ->
                  if uid = scope_ref then
                    match Name_resolution.get_uid_sort ctxt ref_uid with
                    | IdScopeVar _ | IdSubScopeVar _ ->
                        let value = UidMap.find ref_uid out_context in
                        UidMap.add var_uid value exec_context
                    | _ -> exec_context
                  else exec_context
              | _ -> exec_context)
            (UidMap.find scope_prgm.scope_uid ctxt.scopes).uid_set exec_context
      | _ -> assert false)
    schedule exec_context
