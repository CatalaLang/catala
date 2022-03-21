(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
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

open Ast
open Utils

type closure = { name : Var.t; expr : expr Pos.marked Bindlib.box }
type ctx = { name_context : string }

let rec hoist_closures_expr (ctx : ctx) (e : expr Pos.marked) :
    expr Pos.marked Bindlib.box * closure list =
  match Pos.unmark e with
  | EVar v ->
      ( Bindlib.box_apply
          (fun new_v -> (new_v, Pos.get_position v))
          (Bindlib.box_var (Pos.unmark v)),
        [] )
  | ETuple (args, s) ->
      let new_args, closures =
        List.fold_left
          (fun (new_args, closures) arg ->
            let new_arg, new_closures = hoist_closures_expr ctx arg in
            (new_arg :: new_args, new_closures @ closures))
          ([], []) args
      in
      ( Bindlib.box_apply
          (fun new_args -> (ETuple (List.rev new_args, s), Pos.get_position e))
          (Bindlib.box_list new_args),
        closures )
  | _ -> (Bindlib.box e, [])

(** Returns the expression with closed closures and the set of free variables
    inside this new expression. *)
let rec close_closures_expr (ctx : ctx) (e : expr Pos.marked) :
    expr Pos.marked Bindlib.box * VarSet.t =
  match Pos.unmark e with
  | EVar v ->
      ( Bindlib.box_apply
          (fun new_v -> (new_v, Pos.get_position v))
          (Bindlib.box_var (Pos.unmark v)),
        VarSet.singleton (Pos.unmark v) )
  | ETuple (args, s) ->
      let new_args, free_vars =
        List.fold_left
          (fun (new_args, free_vars) arg ->
            let new_arg, new_free_vars = close_closures_expr ctx arg in
            (new_arg :: new_args, VarSet.union new_free_vars free_vars))
          ([], VarSet.empty) args
      in
      ( Bindlib.box_apply
          (fun new_args -> (ETuple (List.rev new_args, s), Pos.get_position e))
          (Bindlib.box_list new_args),
        free_vars )
  | EAbs ((binder, binder_pos), typs) ->
      (* This is a closure we'll have to close *)
      let vars, body = Bindlib.unmbind binder in
      let new_body, body_vars = close_closures_expr ctx body in
      let extra_vars =
        VarSet.diff body_vars (VarSet.of_list (Array.to_list vars))
      in
      let extra_typs =
        List.map
          (fun _ -> (Dcalc.Ast.TAny, binder_pos))
          (VarSet.elements extra_vars)
      in
      let new_binder =
        Bindlib.bind_mvar
          (Array.concat [ vars; Array.of_list (VarSet.elements extra_vars) ])
          new_body
      in
      ( Bindlib.box_apply
          (fun new_binder ->
            ( EAbs ((new_binder, binder_pos), typs @ extra_typs),
              Pos.get_position e ))
          new_binder,
        extra_vars )
  | _ -> (Bindlib.box e, VarSet.empty)

let closure_conversion (p : program) : program Bindlib.box * closure list =
  let new_scopes, closures =
    List.fold_left
      (fun ((acc_new_scopes, acc_closures) :
             scope_body Bindlib.box list * closure list) (scope : scope_body) ->
        match Pos.unmark scope.scope_body_expr with
        | EAbs ((binder, binder_pos), typs) ->
            (* We do not hoist the outer-most EAbs which is the scope function
               itself *)
            let vars, body = Bindlib.unmbind binder in
            let ctx =
              {
                name_context =
                  Pos.unmark
                    (Dcalc.Ast.ScopeName.get_info scope.scope_body_name);
              }
            in
            let new_body_expr, _ = close_closures_expr ctx body in
            let new_binder = Bindlib.bind_mvar vars new_body_expr in
            ( Bindlib.box_apply
                (fun new_binder ->
                  {
                    scope with
                    scope_body_expr =
                      ( EAbs ((new_binder, binder_pos), typs),
                        Pos.get_position scope.scope_body_expr );
                  })
                new_binder
              :: acc_new_scopes,
              [] @ acc_closures )
        | _ -> failwith "should not happen")
      ([], []) p.scopes
  in
  ( Bindlib.box_apply
      (fun new_scopes -> { p with scopes = List.rev new_scopes })
      (Bindlib.box_list new_scopes),
    closures )
