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

open Catala_utils
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
    | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _
    | EArray _ | ELit _ | EAssert _ | EOp _ | EIfThenElse _ | ERaise _
    | ECatch _ ->
      Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union ~f:aux e
    | EVar v ->
      ( (if Var.Set.mem v ctx.globally_bound_vars then Var.Set.empty
        else Var.Set.singleton v),
        (Bindlib.box_var v, m) )
    | EMatch { e; cases; name } ->
      let free_vars, new_e = aux e in
      (* We do not close the clotures inside the arms of the match expression,
         since they get a special treatment at compilation to Scalc. *)
      let free_vars, new_cases =
        EnumConstructor.Map.fold
          (fun cons e1 (free_vars, new_cases) ->
            match Marked.unmark e1 with
            | EAbs { binder; tys } ->
              let vars, body = Bindlib.unmbind binder in
              let new_free_vars, new_body = aux body in
              let new_binder = Expr.bind vars new_body in
              ( Var.Set.union free_vars new_free_vars,
                EnumConstructor.Map.add cons
                  (Expr.eabs new_binder tys (Marked.get_mark e1))
                  new_cases )
            | _ -> failwith "should not happen")
          cases
          (free_vars, EnumConstructor.Map.empty)
      in
      free_vars, Expr.ematch new_e name new_cases m
    | EApp { f = EAbs { binder; tys }, e1_pos; args } ->
      (* let-binding, we should not close these *)
      let vars, body = Bindlib.unmbind binder in
      let free_vars, new_body = aux body in
      let new_binder = Expr.bind vars new_body in
      let free_vars, new_args =
        List.fold_right
          (fun arg (free_vars, new_args) ->
            let new_free_vars, new_arg = aux arg in
            Var.Set.union free_vars new_free_vars, new_arg :: new_args)
          args (free_vars, [])
      in
      free_vars, Expr.eapp (Expr.eabs new_binder tys e1_pos) new_args m
    | EAbs { binder; tys } ->
      (* λ x.t *)
      let binder_mark = m in
      let binder_pos = Expr.mark_pos binder_mark in
      (* Converting the closure. *)
      let vars, body = Bindlib.unmbind binder in
      (* t *)
      let body_vars, new_body = aux body in
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
                 (i + 1)
                 (List.length extra_vars_list)
                 binder_mark)
             extra_vars_list)
          new_body
          (Expr.mark_pos binder_mark)
      in
      let new_closure =
        Expr.make_abs
          (Array.concat [Array.make 1 inner_c_var; vars])
          new_closure_body
          ((TAny, binder_pos) :: tys)
          (Expr.pos e)
      in
      ( extra_vars,
        Expr.make_let_in code_var
          (TAny, Expr.pos e)
          new_closure
          (Expr.etuple
             ((Bindlib.box_var code_var, binder_mark)
             :: List.map
                  (fun extra_var -> Bindlib.box_var extra_var, binder_mark)
                  extra_vars_list)
             m)
          (Expr.pos e) )
    | EApp { f = EOp _, _; _ } ->
      (* This corresponds to an operator call, which we don't want to
         transform*)
      Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union ~f:aux e
    | EApp { f = EVar v, _; _ } when Var.Set.mem v ctx.globally_bound_vars ->
      (* This corresponds to a scope call, which we don't want to transform*)
      Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union ~f:aux e
    | EApp { f = e1; args } ->
      let free_vars, new_e1 = aux e1 in
      let env_var = Var.make "env" in
      let code_var = Var.make "code" in
      let free_vars, new_args =
        List.fold_right
          (fun arg (free_vars, new_args) ->
            let new_free_vars, new_arg = aux arg in
            Var.Set.union free_vars new_free_vars, new_arg :: new_args)
          args (free_vars, [])
      in
      let call_expr =
        let m1 = Marked.get_mark e1 in
        Expr.make_let_in code_var
          (TAny, Expr.pos e)
          (Expr.etupleaccess
             (Bindlib.box_var env_var, m1)
             0
             (List.length new_args + 1)
             m)
          (Expr.eapp
             (Bindlib.box_var code_var, m1)
             ((Bindlib.box_var env_var, m1) :: new_args)
             m)
          (Expr.pos e)
      in
      ( free_vars,
        Expr.make_let_in env_var
          (TAny, Expr.pos e)
          new_e1 call_expr (Expr.pos e) )
  in
  let _vars, e' = aux e in
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
