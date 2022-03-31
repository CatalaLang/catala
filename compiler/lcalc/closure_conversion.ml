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
type ctx = { name_context : string; globally_bound_vars : VarSet.t }

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
    inside this new expression. Implementation guided by
    http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=9. *)
let rec closure_conversion_expr (ctx : ctx) (e : expr Pos.marked) :
    expr Pos.marked Bindlib.box * VarSet.t =
  match Pos.unmark e with
  | EVar v ->
      ( Bindlib.box_apply
          (fun new_v -> (new_v, Pos.get_position v))
          (Bindlib.box_var (Pos.unmark v)),
        VarSet.diff (VarSet.singleton (Pos.unmark v)) ctx.globally_bound_vars )
  | ETuple (args, s) ->
      let new_args, free_vars =
        List.fold_left
          (fun (new_args, free_vars) arg ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union new_free_vars free_vars))
          ([], VarSet.empty) args
      in
      ( Bindlib.box_apply
          (fun new_args -> (ETuple (List.rev new_args, s), Pos.get_position e))
          (Bindlib.box_list new_args),
        free_vars )
  | ETupleAccess (e1, n, s, typs) ->
      let new_e1, free_vars = closure_conversion_expr ctx e1 in
      ( Bindlib.box_apply
          (fun new_e1 ->
            (ETupleAccess (new_e1, n, s, typs), Pos.get_position e))
          new_e1,
        free_vars )
  | EInj (e1, n, e_name, typs) ->
      let new_e1, free_vars = closure_conversion_expr ctx e1 in
      ( Bindlib.box_apply
          (fun new_e1 -> (EInj (new_e1, n, e_name, typs), Pos.get_position e))
          new_e1,
        free_vars )
  | EMatch (e1, arms, e_name) ->
      let new_e1, free_vars = closure_conversion_expr ctx e1 in
      (* We do not close the clotures inside the arms of the match expression,
         since they get a special treatment at compilation to Scalc. *)
      let new_arms, free_vars =
        List.fold_right
          (fun arm (new_arms, free_vars) ->
            match Pos.unmark arm with
            | EAbs ((binder, binder_pos), typs) ->
                let vars, body = Bindlib.unmbind binder in
                let new_body, new_free_vars =
                  closure_conversion_expr ctx body
                in
                let new_binder = Bindlib.bind_mvar vars new_body in
                ( Bindlib.box_apply
                    (fun new_binder ->
                      ( EAbs ((new_binder, binder_pos), typs),
                        Pos.get_position arm ))
                    new_binder
                  :: new_arms,
                  VarSet.union free_vars new_free_vars )
            | _ -> failwith "should not happen")
          arms ([], free_vars)
      in
      ( Bindlib.box_apply2
          (fun new_e1 new_arms ->
            (EMatch (new_e1, new_arms, e_name), Pos.get_position e))
          new_e1
          (Bindlib.box_list new_arms),
        free_vars )
  | EArray args ->
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union free_vars new_free_vars))
          args ([], VarSet.empty)
      in
      ( Bindlib.box_apply
          (fun new_args -> (EArray new_args, Pos.get_position e))
          (Bindlib.box_list new_args),
        free_vars )
  | ELit l -> (Bindlib.box (ELit l, Pos.get_position e), VarSet.empty)
  | EApp ((EAbs ((binder, binder_pos), typs_abs), e1_pos), args) ->
      (* let-binding, we should not close these *)
      let vars, body = Bindlib.unmbind binder in
      let new_body, free_vars = closure_conversion_expr ctx body in
      let new_binder = Bindlib.bind_mvar vars new_body in
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union free_vars new_free_vars))
          args ([], free_vars)
      in
      ( Bindlib.box_apply2
          (fun new_binder new_args ->
            ( EApp
                ((EAbs ((new_binder, binder_pos), typs_abs), e1_pos), new_args),
              Pos.get_position e ))
          new_binder
          (Bindlib.box_list new_args),
        free_vars )
  | EAbs ((binder, binder_pos), typs) ->
      (* λ x.t *)
      (* Converting the closure. *)
      let vars, body = Bindlib.unmbind binder in
      (* t *)
      let new_body, body_vars = closure_conversion_expr ctx body in
      (* [[t]] *)
      let extra_vars =
        VarSet.diff body_vars (VarSet.of_list (Array.to_list vars))
      in
      let extra_vars_list = VarSet.elements extra_vars in
      (* x1, ..., xn *)
      let code_var = Var.make (ctx.name_context, binder_pos) in
      (* code *)
      let inner_c_var = Var.make ("env", binder_pos) in
      let new_closure_body =
        make_multiple_let_in
          (Array.of_list extra_vars_list)
          (List.init (List.length extra_vars_list) (fun _ ->
               (Dcalc.Ast.TAny, binder_pos)))
          (List.mapi
             (fun i _ ->
               Bindlib.box_apply
                 (fun inner_c_var ->
                   ( ETupleAccess
                       ( (inner_c_var, binder_pos),
                         i + 1,
                         None,
                         List.init
                           (List.length extra_vars_list + 1)
                           (fun _ -> (Dcalc.Ast.TAny, binder_pos)) ),
                     binder_pos ))
                 (Bindlib.box_var inner_c_var))
             extra_vars_list)
          new_body binder_pos
      in
      let new_closure =
        make_abs
          (Array.concat [ Array.make 1 inner_c_var; vars ])
          new_closure_body binder_pos
          ((Dcalc.Ast.TAny, binder_pos) :: typs)
          (Pos.get_position e)
      in
      ( make_let_in code_var
          (Dcalc.Ast.TAny, Pos.get_position e)
          new_closure
          (Bindlib.box_apply2
             (fun code_var extra_vars ->
               ( ETuple
                   ( (code_var, binder_pos)
                     :: List.map
                          (fun extra_var -> (extra_var, binder_pos))
                          extra_vars,
                     None ),
                 Pos.get_position e ))
             (Bindlib.box_var code_var)
             (Bindlib.box_list
                (List.map
                   (fun extra_var -> Bindlib.box_var extra_var)
                   extra_vars_list))),
        extra_vars )
  | EApp ((EOp op, pos_op), args) ->
      (* This corresponds to an operator call, which we don't want to
         transform*)
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union free_vars new_free_vars))
          args ([], VarSet.empty)
      in
      ( Bindlib.box_apply
          (fun new_e2 -> (EApp ((EOp op, pos_op), new_e2), Pos.get_position e))
          (Bindlib.box_list new_args),
        free_vars )
  | EApp ((EVar (v, _), v_pos), args) when VarSet.mem v ctx.globally_bound_vars
    ->
      (* This corresponds to a scope call, which we don't want to transform*)
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union free_vars new_free_vars))
          args ([], VarSet.empty)
      in
      ( Bindlib.box_apply2
          (fun new_v new_e2 ->
            (EApp ((new_v, v_pos), new_e2), Pos.get_position e))
          (Bindlib.box_var v)
          (Bindlib.box_list new_args),
        free_vars )
  | EApp (e1, args) ->
      let new_e1, free_vars = closure_conversion_expr ctx e1 in
      let env_var = Var.make ("env", Pos.get_position e1) in
      let code_var = Var.make ("code", Pos.get_position e1) in
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = closure_conversion_expr ctx arg in
            (new_arg :: new_args, VarSet.union free_vars new_free_vars))
          args ([], free_vars)
      in
      let call_expr =
        make_let_in code_var
          (Dcalc.Ast.TAny, Pos.get_position e)
          (Bindlib.box_apply
             (fun env_var ->
               ( ETupleAccess
                   ((env_var, Pos.get_position e1), 0, None, [ (*TODO: fill?*) ]),
                 Pos.get_position e ))
             (Bindlib.box_var env_var))
          (Bindlib.box_apply3
             (fun code_var env_var new_args ->
               ( EApp
                   ( (code_var, Pos.get_position e1),
                     (env_var, Pos.get_position e1) :: new_args ),
                 Pos.get_position e ))
             (Bindlib.box_var code_var) (Bindlib.box_var env_var)
             (Bindlib.box_list new_args))
      in
      ( make_let_in env_var (Dcalc.Ast.TAny, Pos.get_position e) new_e1 call_expr,
        free_vars )
  | EAssert e1 ->
      let new_e1, free_vars = closure_conversion_expr ctx e1 in
      ( Bindlib.box_apply
          (fun new_e1 -> (EAssert new_e1, Pos.get_position e))
          new_e1,
        free_vars )
  | EOp op -> (Bindlib.box (EOp op, Pos.get_position e), VarSet.empty)
  | EIfThenElse (e1, e2, e3) ->
      let new_e1, free_vars1 = closure_conversion_expr ctx e1 in
      let new_e2, free_vars2 = closure_conversion_expr ctx e2 in
      let new_e3, free_vars3 = closure_conversion_expr ctx e3 in
      ( Bindlib.box_apply3
          (fun new_e1 new_e2 new_e3 ->
            (EIfThenElse (new_e1, new_e2, new_e3), Pos.get_position e))
          new_e1 new_e2 new_e3,
        VarSet.union (VarSet.union free_vars1 free_vars2) free_vars3 )
  | ERaise except ->
      (Bindlib.box (ERaise except, Pos.get_position e), VarSet.empty)
  | ECatch (e1, except, e2) ->
      let new_e1, free_vars1 = closure_conversion_expr ctx e1 in
      let new_e2, free_vars2 = closure_conversion_expr ctx e2 in
      ( Bindlib.box_apply2
          (fun new_e1 new_e2 ->
            (ECatch (new_e1, except, new_e2), Pos.get_position e))
          new_e1 new_e2,
        VarSet.union free_vars1 free_vars2 )

let closure_conversion (p : program) : program Bindlib.box * closure list =
  let all_scope_variables =
    List.fold_left
      (fun acc scope -> VarSet.add scope.scope_body_var acc)
      VarSet.empty p.scopes
  in
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
                globally_bound_vars =
                  VarSet.union all_scope_variables
                    (VarSet.of_list [ handle_default; handle_default_opt ]);
              }
            in
            let new_body_expr, _ = closure_conversion_expr ctx body in
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
