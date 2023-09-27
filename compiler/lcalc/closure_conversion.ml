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

type 'm ctx = {
  decl_ctx : decl_ctx;
  name_context : string;
  globally_bound_vars : 'm expr Var.Set.t;
}

let tys_as_tanys tys = List.map (fun x -> Mark.map (fun _ -> TAny) x) tys

(** {1 Transforming closures}*)

(** Returns the expression with closed closures and the set of free variables
    inside this new expression. Implementation guided by
    http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=10
    (environment-passing closure conversion). *)
let rec transform_closures_expr :
    type m. m ctx -> m expr -> m expr Var.Set.t * m expr boxed =
 fun ctx e ->
  let m = Mark.get e in
  match Mark.remove e with
  | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _ | EArray _
  | ELit _ | EExternal _ | EAssert _ | EOp _ | EIfThenElse _ | ERaise _
  | ECatch _ ->
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
          match Mark.remove e1 with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_free_vars, new_body = (transform_closures_expr ctx) body in
            let new_binder = Expr.bind vars new_body in
            ( Var.Set.union free_vars
                (Var.Set.diff new_free_vars
                   (Var.Set.of_list (Array.to_list vars))),
              EnumConstructor.Map.add cons
                (Expr.eabs new_binder tys (Mark.get e1))
                new_cases )
          | _ -> failwith "should not happen")
        cases
        (free_vars, EnumConstructor.Map.empty)
    in
    free_vars, Expr.ematch ~e:new_e ~name ~cases:new_cases m
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
    let closure_env_arg_var = Var.make "env" in
    let closure_env_var = Var.make "env" in
    let any_ty = TAny, binder_pos in
    (* let env = from_closure_env env in let arg0 = env.0 in ... *)
    let new_closure_body =
      Expr.make_let_in closure_env_var any_ty
        (Expr.eapp
           (Expr.eop Operator.FromClosureEnv
              [TClosureEnv, binder_pos]
              binder_mark)
           [Expr.evar closure_env_arg_var binder_mark]
           binder_mark)
        (Expr.make_multiple_let_in
           (Array.of_list extra_vars_list)
           (List.map (fun _ -> any_ty) extra_vars_list)
           (List.mapi
              (fun i _ ->
                Expr.etupleaccess
                  (Expr.evar closure_env_var binder_mark)
                  i
                  (List.length extra_vars_list)
                  binder_mark)
              extra_vars_list)
           new_body binder_pos)
        binder_pos
    in
    (* fun env arg0 ... -> new_closure_body *)
    let new_closure =
      Expr.make_abs
        (Array.concat [Array.make 1 closure_env_arg_var; vars])
        new_closure_body
        ((TClosureEnv, binder_pos) :: tys)
        (Expr.pos e)
    in
    ( extra_vars,
      Expr.make_let_in code_var
        (TAny, Expr.pos e)
        new_closure
        (Expr.etuple
           ((Bindlib.box_var code_var, binder_mark)
           :: [
                Expr.eapp
                  (Expr.eop Operator.ToClosureEnv
                     [TAny, Expr.pos e]
                     (Mark.get e))
                  [
                    (if extra_vars_list = [] then Expr.elit LUnit binder_mark
                     else
                       Expr.etuple
                         (List.map
                            (fun extra_var ->
                              Bindlib.box_var extra_var, binder_mark)
                            extra_vars_list)
                         m);
                  ]
                  (Mark.get e);
              ])
           m)
        (Expr.pos e) )
  | EApp
      {
        f =
          (EOp { op = HandleDefaultOpt | Fold | Map | Filter | Reduce; _ }, _)
          as f;
        args;
      } ->
    (* Special case for some operators: its arguments shall remain thunks (which
       are closures) because if you want to extract it as a function you need
       these closures to preserve evaluation order, but backends that don't
       support closures will simply extract these operators in a inlined way and
       skip the thunks. *)
    let free_vars, new_args =
      List.fold_right
        (fun (arg : (lcalc, m) gexpr) (free_vars, new_args) ->
          let m_arg = Mark.get arg in
          match Mark.remove arg with
          | EAbs { binder; tys } ->
            let vars, arg = Bindlib.unmbind binder in
            let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
            let new_arg =
              Expr.make_abs vars new_arg tys (Expr.mark_pos m_arg)
            in
            Var.Set.union free_vars new_free_vars, new_arg :: new_args
          | _ ->
            let new_free_vars, new_arg = transform_closures_expr ctx arg in
            Var.Set.union free_vars new_free_vars, new_arg :: new_args)
        args (Var.Set.empty, [])
    in
    free_vars, Expr.eapp (Expr.box f) new_args (Mark.get e)
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
      let m1 = Mark.get e1 in
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
  | _ -> .

(* Here I have to reimplement Scope.map_exprs_in_lets because I'm changing the
   type *)
let transform_closures_scope_let ctx scope_body_expr =
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
              scope_let_typ = Mark.copy scope_let.scope_let_typ TAny;
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

let transform_closures_program (p : 'm program) : 'm program Bindlib.box =
  let _, new_code_items =
    Scope.fold_map
      ~f:(fun toplevel_vars var code_item ->
        match code_item with
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr =
            Bindlib.unbind body.scope_body_expr
          in
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = Mark.remove (ScopeName.get_info name);
              globally_bound_vars = toplevel_vars;
            }
          in
          let new_scope_lets =
            transform_closures_scope_let ctx scope_body_expr
          in
          let new_scope_body_expr =
            Bindlib.bind_var scope_input_var new_scope_lets
          in

          ( Var.Set.add var toplevel_vars,
            Bindlib.box_apply
              (fun scope_body_expr ->
                ScopeDef (name, { body with scope_body_expr }))
              new_scope_body_expr )
        | Topdef (name, ty, expr) ->
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = Mark.remove (TopdefName.get_info name);
              globally_bound_vars = toplevel_vars;
            }
          in
          let _free_vars, new_expr = transform_closures_expr ctx expr in
          ( Var.Set.add var toplevel_vars,
            Bindlib.box_apply
              (fun e -> Topdef (name, ty, e))
              (Expr.Box.lift new_expr) ))
      ~varf:(fun v -> v)
      Var.Set.empty p.code_items
  in
  (* Now we need to further tweak [decl_ctx] because some of the user-defined
     types can have closures in them and these closured might have changed type.
     So we reset them to [TAny] and leave the typechecker to figure it out. This
     will not yield any type unification conflicts because of the special type
     [TClosureEnv]. Indeed, consider the following closure: [let f = if ... then
     fun v -> x + v else fun v -> v]. To be typed correctly once converted, this
     closure needs an existential type, this is what [TClosureEnv] is for. This
     kind of situation is difficult to produce using the Catala surface
     language: it can only happen if you store a closure which is the output of
     a scope inside a user-defined data structure, and if you do it in two
     different places in the code with two closures that don't have the same
     capture footprint. See
     [tests/tests_func/good/scope_call_func_struct_closure.catala_en]. *)
  let new_decl_ctx =
    let rec type_contains_arrow t =
      match Mark.remove t with
      | TArrow _ -> true
      | TAny -> true
      | TOption t' -> type_contains_arrow t'
      | TClosureEnv | TLit _ -> false
      | TArray ts -> type_contains_arrow ts
      | TTuple ts -> List.exists type_contains_arrow ts
      | TEnum e ->
        EnumConstructor.Map.exists
          (fun _ t' -> type_contains_arrow t')
          (EnumName.Map.find e p.decl_ctx.ctx_enums)
      | TStruct s ->
        StructField.Map.exists
          (fun _ t' -> type_contains_arrow t')
          (StructName.Map.find s p.decl_ctx.ctx_structs)
    in
    let replace_fun_typs t =
      if type_contains_arrow t then Mark.copy t TAny else t
    in
    {
      p.decl_ctx with
      ctx_structs =
        StructName.Map.map
          (StructField.Map.map replace_fun_typs)
          p.decl_ctx.ctx_structs;
      ctx_enums =
        EnumName.Map.map
          (EnumConstructor.Map.map replace_fun_typs)
          p.decl_ctx.ctx_enums;
    }
  in
  Bindlib.box_apply
    (fun new_code_items ->
      { code_items = new_code_items; decl_ctx = new_decl_ctx; lang = p.lang })
    new_code_items

(** {1 Hoisting closures}*)

type 'm hoisted_closure = {
  name : 'm expr Var.t;
  ty : typ;
  closure : (lcalc, 'm) boxed_gexpr (* Starts with [EAbs]. *);
}

let rec hoist_closures_expr :
    type m. string -> m expr -> m hoisted_closure list * m expr boxed =
 fun name_context e ->
  let m = Mark.get e in
  match Mark.remove e with
  | EMatch { e; cases; name } ->
    let collected_closures, new_e = (hoist_closures_expr name_context) e in
    (* We do not close the closures inside the arms of the match expression,
       since they get a special treatment at compilation to Scalc. *)
    let collected_closures, new_cases =
      EnumConstructor.Map.fold
        (fun cons e1 (collected_closures, new_cases) ->
          match Mark.remove e1 with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_collected_closures, new_body =
              (hoist_closures_expr name_context) body
            in
            let new_binder = Expr.bind vars new_body in
            ( collected_closures @ new_collected_closures,
              EnumConstructor.Map.add cons
                (Expr.eabs new_binder tys (Mark.get e1))
                new_cases )
          | _ -> failwith "should not happen")
        cases
        (collected_closures, EnumConstructor.Map.empty)
    in
    collected_closures, Expr.ematch ~e:new_e ~name ~cases:new_cases m
  | EApp { f = EAbs { binder; tys }, e1_pos; args } ->
    (* let-binding, we should not close these *)
    let vars, body = Bindlib.unmbind binder in
    let collected_closures, new_body =
      (hoist_closures_expr name_context) body
    in
    let new_binder = Expr.bind vars new_body in
    let collected_closures, new_args =
      List.fold_right
        (fun arg (collected_closures, new_args) ->
          let new_collected_closures, new_arg =
            (hoist_closures_expr name_context) arg
          in
          collected_closures @ new_collected_closures, new_arg :: new_args)
        args (collected_closures, [])
    in
    ( collected_closures,
      Expr.eapp (Expr.eabs new_binder (tys_as_tanys tys) e1_pos) new_args m )
  | EApp
      {
        f =
          (EOp { op = HandleDefaultOpt | Fold | Map | Filter | Reduce; _ }, _)
          as f;
        args;
      } ->
    (* Special case for some operators: its arguments closures thunks because if
       you want to extract it as a function you need these closures to preserve
       evaluation order, but backends that don't support closures will simply
       extract these operators in a inlined way and skip the thunks. *)
    let collected_closures, new_args =
      List.fold_right
        (fun (arg : (lcalc, m) gexpr) (collected_closures, new_args) ->
          let m_arg = Mark.get arg in
          match Mark.remove arg with
          | EAbs { binder; tys } ->
            let vars, arg = Bindlib.unmbind binder in
            let new_collected_closures, new_arg =
              (hoist_closures_expr name_context) arg
            in
            let new_arg =
              Expr.make_abs vars new_arg tys (Expr.mark_pos m_arg)
            in
            new_collected_closures @ collected_closures, new_arg :: new_args
          | _ ->
            let new_collected_closures, new_arg =
              hoist_closures_expr name_context arg
            in
            new_collected_closures @ collected_closures, new_arg :: new_args)
        args ([], [])
    in
    collected_closures, Expr.eapp (Expr.box f) new_args (Mark.get e)
  | EAbs { tys; _ } ->
    (* this is the closure we want to hoist*)
    let closure_var = Var.make ("closure_" ^ name_context) in
    (* TODO: This will end up as a toplevel name. However for now we assume
       toplevel names are unique, but this breaks this assertions and can lead
       to name wrangling in the backends. We need to have a better system for
       name disambiguation when for instance printing to Dcalc/Lcalc/Scalc but
       also OCaml, Python, etc. *)
    ( [
        {
          name = closure_var;
          ty = TArrow (tys, (TAny, Expr.mark_pos m)), Expr.mark_pos m;
          closure = Expr.rebox e;
        };
      ],
      Expr.make_var closure_var m )
  | EApp _ | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _
  | EArray _ | ELit _ | EAssert _ | EOp _ | EIfThenElse _ | ERaise _ | ECatch _
  | EVar _ ->
    Expr.map_gather ~acc:[] ~join:( @ ) ~f:(hoist_closures_expr name_context) e
  | EExternal _ -> failwith "unimplemented"
  | _ -> .

(* Here I have to reimplement Scope.map_exprs_in_lets because I'm changing the
   type *)
let hoist_closures_scope_let name_context scope_body_expr =
  Scope.fold_right_lets
    ~f:(fun scope_let var_next (hoisted_closures, next_scope_lets) ->
      let new_hoisted_closures, new_scope_let_expr =
        (hoist_closures_expr (Bindlib.name_of var_next))
          scope_let.scope_let_expr
      in
      ( new_hoisted_closures @ hoisted_closures,
        Bindlib.box_apply2
          (fun scope_let_next scope_let_expr ->
            ScopeLet { scope_let with scope_let_next; scope_let_expr })
          (Bindlib.bind_var var_next next_scope_lets)
          (Expr.Box.lift new_scope_let_expr) ))
    ~init:(fun res ->
      let hoisted_closures, new_scope_let_expr =
        (hoist_closures_expr name_context) res
      in
      (* INVARIANT here: the result expr of a scope is simply a struct
         containing all output variables so nothing should be converted here, so
         no need to take into account free variables. *)
      ( hoisted_closures,
        Bindlib.box_apply
          (fun res -> Result res)
          (Expr.Box.lift new_scope_let_expr) ))
    scope_body_expr

let rec hoist_closures_code_item_list
    (code_items : (lcalc, 'm) gexpr code_item_list) :
    (lcalc, 'm) gexpr code_item_list Bindlib.box =
  match code_items with
  | Nil -> Bindlib.box Nil
  | Cons (code_item, next_code_items) ->
    let code_item_var, next_code_items = Bindlib.unbind next_code_items in
    let hoisted_closures, new_code_item =
      match code_item with
      | ScopeDef (name, body) ->
        let scope_input_var, scope_body_expr =
          Bindlib.unbind body.scope_body_expr
        in
        let new_hoisted_closures, new_scope_lets =
          hoist_closures_scope_let
            (fst (ScopeName.get_info name))
            scope_body_expr
        in
        let new_scope_body_expr =
          Bindlib.bind_var scope_input_var new_scope_lets
        in
        ( new_hoisted_closures,
          Bindlib.box_apply
            (fun scope_body_expr ->
              ScopeDef (name, { body with scope_body_expr }))
            new_scope_body_expr )
      | Topdef (name, ty, expr) ->
        let new_hoisted_closures, new_expr =
          hoist_closures_expr (Mark.remove (TopdefName.get_info name)) expr
        in
        ( new_hoisted_closures,
          Bindlib.box_apply
            (fun e -> Topdef (name, ty, e))
            (Expr.Box.lift new_expr) )
    in
    let next_code_items = hoist_closures_code_item_list next_code_items in
    let next_code_items =
      Bindlib.box_apply2
        (fun next_code_items new_code_item ->
          Cons (new_code_item, next_code_items))
        (Bindlib.bind_var code_item_var next_code_items)
        new_code_item
    in
    let next_code_items =
      List.fold_left
        (fun (next_code_items : (lcalc, 'm) gexpr code_item_list Bindlib.box)
             (hoisted_closure : 'm hoisted_closure) ->
          let next_code_items =
            Bindlib.bind_var hoisted_closure.name next_code_items
          in
          let closure, closure_mark = hoisted_closure.closure in
          Bindlib.box_apply2
            (fun next_code_items closure ->
              Cons
                ( Topdef
                    ( TopdefName.fresh []
                        ( Bindlib.name_of hoisted_closure.name,
                          Expr.mark_pos closure_mark ),
                      hoisted_closure.ty,
                      (closure, closure_mark) ),
                  next_code_items ))
            next_code_items closure)
        next_code_items hoisted_closures
    in
    next_code_items

let hoist_closures_program (p : 'm program) : 'm program Bindlib.box =
  let new_code_items = hoist_closures_code_item_list p.code_items in
  (*TODO: we need to insert the hoisted closures just before the scopes they
    belong to, because some of them call sub-scopes and putting them all at the
    beginning breaks dependency ordering. *)
  Bindlib.box_apply
    (fun new_code_items -> { p with code_items = new_code_items })
    new_code_items

(** {1 Closure conversion}*)

let closure_conversion (p : 'm program) : 'm program Bindlib.box =
  let new_p = transform_closures_program p in
  hoist_closures_program (Bindlib.unbox new_p)
