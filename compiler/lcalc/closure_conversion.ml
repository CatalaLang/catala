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

open Utils
open Shared_ast
open Ast
module D = Dcalc.Ast

(** TODO: This version is not yet debugged and ought to be specialized when
    Lcalc has more structure. *)

type 'm ctx = { name_context : string; globally_bound_vars : 'm expr Var.Set.t }

(** Returns the expression with closed closures and the set of free variables
    inside this new expression. Implementation guided by
    http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=9. *)
let closure_conversion_expr (type m) (ctx : m ctx) (e : m expr) : m expr boxed =
  let rec aux e =
    let m = Marked.get_mark e in
    match Marked.unmark e with
    | EVar v ->
      ( (Bindlib.box_var v, m),
        if Var.Set.mem v ctx.globally_bound_vars then Var.Set.empty
        else Var.Set.singleton v )
    | ETuple (args, s) ->
      let new_args, free_vars =
        List.fold_left
          (fun (new_args, free_vars) arg ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union new_free_vars free_vars)
          ([], Var.Set.empty) args
      in
      Expr.etuple (List.rev new_args) s m, free_vars
    | ETupleAccess (e1, n, s, typs) ->
      let new_e1, free_vars = aux e1 in
      Expr.etupleaccess new_e1 n s typs m, free_vars
    | EInj (e1, n, e_name, typs) ->
      let new_e1, free_vars = aux e1 in
      Expr.einj new_e1 n e_name typs m, free_vars
    | EMatch (e1, arms, e_name) ->
      let new_e1, free_vars = aux e1 in
      (* We do not close the clotures inside the arms of the match expression,
         since they get a special treatment at compilation to Scalc. *)
      let new_arms, free_vars =
        List.fold_right
          (fun arm (new_arms, free_vars) ->
            match Marked.unmark arm with
            | EAbs (binder, typs) ->
              let vars, body = Bindlib.unmbind binder in
              let new_body, new_free_vars = aux body in
              let new_binder = Expr.bind vars new_body in
              ( Expr.eabs new_binder typs (Marked.get_mark arm) :: new_arms,
                Var.Set.union free_vars new_free_vars )
            | _ -> failwith "should not happen")
          arms ([], free_vars)
      in
      Expr.ematch new_e1 new_arms e_name m, free_vars
    | EArray args ->
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union free_vars new_free_vars)
          args ([], Var.Set.empty)
      in
      Expr.earray new_args m, free_vars
    | ELit l -> Expr.elit l m, Var.Set.empty
    | EApp ((EAbs (binder, typs_abs), e1_pos), args) ->
      (* let-binding, we should not close these *)
      let vars, body = Bindlib.unmbind binder in
      let new_body, free_vars = aux body in
      let new_binder = Expr.bind vars new_body in
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union free_vars new_free_vars)
          args ([], free_vars)
      in
      Expr.eapp (Expr.eabs new_binder typs_abs e1_pos) new_args m, free_vars
    | EAbs (binder, typs) ->
      (* λ x.t *)
      let binder_mark = m in
      let binder_pos = Expr.mark_pos binder_mark in
      (* Converting the closure. *)
      let vars, body = Bindlib.unmbind binder in
      (* t *)
      let new_body, body_vars = aux body in
      (* [[t]] *)
      let extra_vars =
        Var.Set.diff body_vars (Var.Set.of_list (Array.to_list vars))
      in
      let extra_vars_list = Var.Set.elements extra_vars in
      (* x1, ..., xn *)
      let code_var = Var.make ctx.name_context in
      (* code *)
      let inner_c_var = Var.make "env" in
      let any_ty = TAny, binder_pos in
      let new_closure_body =
        Expr.make_multiple_let_in
          (Array.of_list extra_vars_list)
          (List.map (fun _ -> any_ty) extra_vars_list)
          (List.mapi
             (fun i _ ->
               Expr.etupleaccess
                 (Expr.evar inner_c_var binder_mark)
                 (i + 1) None
                 (List.map (fun _ -> any_ty) extra_vars_list)
                 binder_mark)
             extra_vars_list)
          new_body
          (Expr.mark_pos binder_mark)
      in
      let new_closure =
        Expr.make_abs
          (Array.concat [Array.make 1 inner_c_var; vars])
          new_closure_body
          ((TAny, binder_pos) :: typs)
          (Expr.pos e)
      in
      ( Expr.make_let_in code_var
          (TAny, Expr.pos e)
          new_closure
          (Expr.etuple
             ((Bindlib.box_var code_var, binder_mark)
             :: List.map
                  (fun extra_var -> Bindlib.box_var extra_var, binder_mark)
                  extra_vars_list)
             None m)
          (Expr.pos e),
        extra_vars )
    | EApp ((EOp op, pos_op), args) ->
      (* This corresponds to an operator call, which we don't want to
         transform*)
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union free_vars new_free_vars)
          args ([], Var.Set.empty)
      in
      Expr.eapp (Expr.eop op pos_op) new_args m, free_vars
    | EApp ((EVar v, v_pos), args) when Var.Set.mem v ctx.globally_bound_vars ->
      (* This corresponds to a scope call, which we don't want to transform*)
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union free_vars new_free_vars)
          args ([], Var.Set.empty)
      in
      Expr.eapp (Bindlib.box_var v, v_pos) new_args m, free_vars
    | EApp (e1, args) ->
      let new_e1, free_vars = aux e1 in
      let env_var = Var.make "env" in
      let code_var = Var.make "code" in
      let new_args, free_vars =
        List.fold_right
          (fun arg (new_args, free_vars) ->
            let new_arg, new_free_vars = aux arg in
            new_arg :: new_args, Var.Set.union free_vars new_free_vars)
          args ([], free_vars)
      in
      let call_expr =
        let m1 = Marked.get_mark e1 in
        Expr.make_let_in code_var
          (TAny, Expr.pos e)
          (Expr.etupleaccess
             (Bindlib.box_var env_var, m1)
             0 None [ (*TODO: fill?*) ]
             m)
          (Expr.eapp
             (Bindlib.box_var code_var, m1)
             ((Bindlib.box_var env_var, m1) :: new_args)
             m)
          (Expr.pos e)
      in
      ( Expr.make_let_in env_var (TAny, Expr.pos e) new_e1 call_expr (Expr.pos e),
        free_vars )
    | EAssert e1 ->
      let new_e1, free_vars = aux e1 in
      Expr.eassert new_e1 m, free_vars
    | EOp op -> Expr.eop op m, Var.Set.empty
    | EIfThenElse (e1, e2, e3) ->
      let new_e1, free_vars1 = aux e1 in
      let new_e2, free_vars2 = aux e2 in
      let new_e3, free_vars3 = aux e3 in
      ( Expr.eifthenelse new_e1 new_e2 new_e3 m,
        Var.Set.union (Var.Set.union free_vars1 free_vars2) free_vars3 )
    | ERaise except -> Expr.eraise except m, Var.Set.empty
    | ECatch (e1, except, e2) ->
      let new_e1, free_vars1 = aux e1 in
      let new_e2, free_vars2 = aux e2 in
      Expr.ecatch new_e1 except new_e2 m, Var.Set.union free_vars1 free_vars2
  in
  let e', _vars = aux e in
  e'

let closure_conversion (p : 'm program) : 'm program Bindlib.box =
  let new_scopes, _ =
    Scope.fold_left
      ~f:(fun (acc_new_scopes, global_vars) scope scope_var ->
        (* [acc_new_scopes] represents what has been translated in the past, it
           needs a continuation to attach the rest of the translated scopes. *)
        let scope_input_var, scope_body_expr =
          Bindlib.unbind scope.scope_body.scope_body_expr
        in
        let global_vars = Var.Set.add scope_var global_vars in
        let ctx =
          {
            name_context = Marked.unmark (ScopeName.get_info scope.scope_name);
            globally_bound_vars = global_vars;
          }
        in
        let new_scope_lets =
          Scope.map_exprs_in_lets
            ~f:(closure_conversion_expr ctx)
            ~varf:(fun v -> v)
            scope_body_expr
        in
        let new_scope_body_expr =
          Bindlib.bind_var scope_input_var new_scope_lets
        in
        ( (fun next ->
            acc_new_scopes
              (Bindlib.box_apply2
                 (fun new_scope_body_expr next ->
                   ScopeDef
                     {
                       scope with
                       scope_body =
                         {
                           scope.scope_body with
                           scope_body_expr = new_scope_body_expr;
                         };
                       scope_next = next;
                     })
                 new_scope_body_expr
                 (Bindlib.bind_var scope_var next))),
          global_vars ))
      ~init:
        ( Fun.id,
          Var.Set.of_list
            (List.map Var.translate [handle_default; handle_default_opt]) )
      p.scopes
  in
  Bindlib.box_apply
    (fun new_scopes -> { p with scopes = new_scopes })
    (new_scopes (Bindlib.box Nil))
