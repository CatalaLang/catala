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

type 'm ctx = {
  decl_ctx : decl_ctx;
  name_context : string;
  globally_bound_vars : 'm expr Var.Set.t;
}

let tys_as_tanys tys =
  List.map (fun x -> Marked.map_under_mark (fun _ -> TAny) x) tys

type 'm hoisted_closure = { name : 'm expr Var.t; closure : 'm expr }

let rec hoist_context_free_closures :
    type m. m ctx -> m expr -> m hoisted_closure list * m expr boxed =
 fun ctx e ->
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _ | EArray _
  | ELit _ | EAssert _ | EOp _ | EIfThenElse _ | ERaise _ | ECatch _ | EVar _ ->
    Expr.map_gather ~acc:[] ~join:( @ ) ~f:(hoist_context_free_closures ctx) e
  | EMatch { e; cases; name } ->
    let collected_closures, new_e = (hoist_context_free_closures ctx) e in
    (* We do not close the clotures inside the arms of the match expression,
       since they get a special treatment at compilation to Scalc. *)
    let collected_closures, new_cases =
      EnumConstructor.Map.fold
        (fun cons e1 (collected_closures, new_cases) ->
          match Marked.unmark e1 with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_collected_closures, new_body =
              (hoist_context_free_closures ctx) body
            in
            let new_binder = Expr.bind vars new_body in
            ( collected_closures @ new_collected_closures,
              EnumConstructor.Map.add cons
                (Expr.eabs new_binder tys (Marked.get_mark e1))
                new_cases )
          | _ -> failwith "should not happen")
        cases
        (collected_closures, EnumConstructor.Map.empty)
    in
    collected_closures, Expr.ematch new_e name new_cases m
  | EApp { f = EAbs { binder; tys }, e1_pos; args } ->
    (* let-binding, we should not close these *)
    let vars, body = Bindlib.unmbind binder in
    let collected_closures, new_body = (hoist_context_free_closures ctx) body in
    let new_binder = Expr.bind vars new_body in
    let collected_closures, new_args =
      List.fold_right
        (fun arg (collected_closures, new_args) ->
          let new_collected_closures, new_arg =
            (hoist_context_free_closures ctx) arg
          in
          collected_closures @ new_collected_closures, new_arg :: new_args)
        args (collected_closures, [])
    in
    ( collected_closures,
      Expr.eapp (Expr.eabs new_binder (tys_as_tanys tys) e1_pos) new_args m )
  | EAbs _ ->
    (* this is the closure we want to hoist*)
    let closure_var = Var.make ctx.name_context in
    [{ name = closure_var; closure = e }], Expr.make_var closure_var m
  | EApp _ ->
    Expr.map_gather ~acc:[] ~join:( @ ) ~f:(hoist_context_free_closures ctx) e
 [@@warning "-32"]

(** Returns the expression with closed closures and the set of free variables
    inside this new expression. Implementation guided by
    http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=10
    (environment-passing closure conversion). *)
let rec transform_closures_expr :
    type m. m ctx -> m expr -> m expr Var.Set.t * m expr boxed =
 fun ctx e ->
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _ | EArray _
  | ELit _ | EAssert _ | EOp _ | EIfThenElse _ | ERaise _ | ECatch _ ->
    Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union
      ~f:(transform_closures_expr ctx)
      e
  | EVar v ->
    ( (if Var.Set.mem v ctx.globally_bound_vars then Var.Set.empty
      else Var.Set.singleton v),
      (Bindlib.box_var v, m) )
  | EMatch { e; cases; name } ->
    let free_vars, new_e = (transform_closures_expr ctx) e in
    (* We do not close the clotures inside the arms of the match expression,
       since they get a special treatment at compilation to Scalc. *)
    let free_vars, new_cases =
      EnumConstructor.Map.fold
        (fun cons e1 (free_vars, new_cases) ->
          match Marked.unmark e1 with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_free_vars, new_body = (transform_closures_expr ctx) body in
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
    let free_vars, new_body = (transform_closures_expr ctx) body in
    let new_binder = Expr.bind vars new_body in
    let free_vars, new_args =
      List.fold_right
        (fun arg (free_vars, new_args) ->
          let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
          Var.Set.union free_vars new_free_vars, new_arg :: new_args)
        args (free_vars, [])
    in
    ( free_vars,
      Expr.eapp (Expr.eabs new_binder (tys_as_tanys tys) e1_pos) new_args m )
  | EAbs { binder; tys } ->
    (* λ x.t *)
    let binder_mark = m in
    let binder_pos = Expr.mark_pos binder_mark in
    (* Converting the closure. *)
    let vars, body = Bindlib.unmbind binder in
    (* t *)
    let body_vars, new_body = (transform_closures_expr ctx) body in
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
               i
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
           :: [
                Expr.etuple
                  (List.map
                     (fun extra_var -> Bindlib.box_var extra_var, binder_mark)
                     extra_vars_list)
                  m;
              ])
           m)
        (Expr.pos e) )
  | EApp { f = EOp _, _; _ } ->
    (* This corresponds to an operator call, which we don't want to transform*)
    Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union
      ~f:(transform_closures_expr ctx)
      e
  | EApp { f = EVar v, _; _ } when Var.Set.mem v ctx.globally_bound_vars ->
    (* This corresponds to a scope call, which we don't want to transform*)
    Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union
      ~f:(transform_closures_expr ctx)
      e
  | EApp { f = e1; args } ->
    let free_vars, new_e1 = (transform_closures_expr ctx) e1 in
    let code_env_var = Var.make "code_and_env" in
    let env_var = Var.make "env" in
    let code_var = Var.make "code" in
    let free_vars, new_args =
      List.fold_right
        (fun arg (free_vars, new_args) ->
          let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
          Var.Set.union free_vars new_free_vars, new_arg :: new_args)
        args (free_vars, [])
    in
    let call_expr =
      let m1 = Marked.get_mark e1 in
      Expr.make_let_in code_var
        (TAny, Expr.pos e)
        (Expr.etupleaccess (Bindlib.box_var code_env_var, m1) 0 2 m)
        (Expr.make_let_in env_var
           (TAny, Expr.pos e)
           (Expr.etupleaccess (Bindlib.box_var code_env_var, m1) 1 2 m)
           (Expr.eapp
              (Bindlib.box_var code_var, m1)
              ((Bindlib.box_var env_var, m1) :: new_args)
              m)
           (Expr.pos e))
        (Expr.pos e)
    in
    ( free_vars,
      Expr.make_let_in code_env_var
        (TAny, Expr.pos e)
        new_e1 call_expr (Expr.pos e) )

(* Here I have to reimplement Scope.map_exprs_in_lets because I'm changing the
   type *)
let closure_conversion_scope_let ctx scope_body_expr =
  Scope.fold_right_lets
    ~f:(fun scope_let var_next acc ->
      let _free_vars, new_scope_let_expr =
        (transform_closures_expr
           { ctx with name_context = Bindlib.name_of var_next })
          scope_let.scope_let_expr
      in
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet
            {
              scope_let with
              scope_let_next;
              scope_let_expr;
              scope_let_typ = Marked.same_mark_as TAny scope_let.scope_let_typ;
            })
        (Bindlib.bind_var var_next acc)
        (Expr.Box.lift new_scope_let_expr))
    ~init:(fun res ->
      let _free_vars, new_scope_let_expr = (transform_closures_expr ctx) res in
      (* INVARIANT here: the result expr of a scope is simply a struct
         containing all output variables so nothing should be converted here, so
         no need to take into account free variables. *)
      Bindlib.box_apply
        (fun res -> Result res)
        (Expr.Box.lift new_scope_let_expr))
    scope_body_expr

let closure_conversion (p : 'm program) : 'm program Bindlib.box =
  let _, new_code_items =
    Scope.fold_map
      ~f:(fun (toplevel_vars, decl_ctx) var code_item ->
        match code_item with
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr =
            Bindlib.unbind body.scope_body_expr
          in
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = Marked.unmark (ScopeName.get_info name);
              globally_bound_vars = toplevel_vars;
            }
          in
          let new_scope_lets =
            closure_conversion_scope_let ctx scope_body_expr
          in
          let new_scope_body_expr =
            Bindlib.bind_var scope_input_var new_scope_lets
          in
          let new_decl_ctx =
            (* Because closure conversion can change the type of input and
               output scope variables that are structs, their type will change.
               So we replace their type decleration in the structs with TAny so
               that a later re-typing phase can infer them. *)
            let replace_type_with_any s =
              Some
                (StructField.Map.map
                   (fun t -> Marked.same_mark_as TAny t)
                   (Option.get s))
            in
            {
              decl_ctx with
              ctx_structs =
                StructName.Map.update body.scope_body_output_struct
                  replace_type_with_any
                  (StructName.Map.update body.scope_body_input_struct
                     replace_type_with_any decl_ctx.ctx_structs);
            }
          in
          ( (Var.Set.add var toplevel_vars, new_decl_ctx),
            Bindlib.box_apply
              (fun scope_body_expr ->
                ScopeDef (name, { body with scope_body_expr }))
              new_scope_body_expr )
        | Topdef (name, ty, expr) ->
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = Marked.unmark (TopdefName.get_info name);
              globally_bound_vars = toplevel_vars;
            }
          in
          let _free_vars, new_expr = transform_closures_expr ctx expr in
          ( (Var.Set.add var toplevel_vars, decl_ctx),
            Bindlib.box_apply
              (fun e -> Topdef (name, ty, e))
              (Expr.Box.lift new_expr) ))
      ~varf:(fun v -> v)
      ( Var.Set.of_list
          (List.map Var.translate [handle_default; handle_default_opt]),
        p.decl_ctx )
      p.code_items
  in
  Bindlib.box_apply
    (fun new_code_items -> { p with code_items = new_code_items })
    new_code_items
