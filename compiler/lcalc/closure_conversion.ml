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

type flags = { keep_special_ops : bool }
type name_context = { prefix : string }

type 'm ctx = {
  decl_ctx : decl_ctx;
  name_context : name_context;
  flags : flags;
  globally_bound_vars : ('m expr, typ) Var.Map.t;
}

let new_context prefix = { prefix }
let new_var ?(pfx = "") name_context = Var.make (pfx ^ name_context.prefix)

(** Function types will be transformed in this way throughout, including in
    [decl_ctx] *)
let translate_type ty =
  let rec aux = function
    | TArrow (t1, t2), pos ->
      let t1 = Bindlib.box_list (List.map aux t1) in
      let t2 = aux t2 in
      Bindlib.box_apply2
        (fun t1 t2 ->
          ( TTuple
              [
                TArrow ((TClosureEnv, Pos.void) :: t1, t2), pos; TClosureEnv, pos;
              ],
            pos ))
        t1 t2
    | ty -> Type.map aux ty
  in
  Bindlib.unbox (aux ty)

let translate_mark e = Mark.map_mark (Expr.map_ty translate_type) e

let join_vars : ('a, 'x) Var.Map.t -> ('a, 'x) Var.Map.t -> ('a, 'x) Var.Map.t =
 fun m1 m2 -> Var.Map.union (fun _ a _ -> Some a) m1 m2

(** {1 Transforming closures}*)

let build_closure :
    type m.
    m ctx ->
    (m expr Var.t * m mark) list ->
    m expr boxed ->
    m expr Var.t array ->
    typ list ->
    m mark ->
    m expr boxed =
 fun ctx free_vars body args tys m ->
  (* λ x.t *)
  let pos = Expr.mark_pos m in
  let mark_ty ty = Expr.with_ty m ty in
  let free_vars_types = List.map (fun (_, m) -> Expr.maybe_ty m) free_vars in
  (* x1, ..., xn *)
  let code_var = new_var ctx.name_context in
  (* code *)
  let closure_env_arg_var = Var.make "env" in
  let closure_env_var = Var.make "env" in
  let env_ty = TTuple free_vars_types, pos in
  (* let env = from_closure_env env in let arg0 = env.0 in ... *)
  let mark_pos (v, (m : m mark)) = Mark.add (Expr.mark_pos m) v in
  let new_closure_body =
    Expr.make_let_in
      (Mark.ghost closure_env_var)
      env_ty
      (Expr.eappop
         ~op:(Operator.FromClosureEnv, pos)
         ~tys:[TClosureEnv, pos]
         ~args:[Expr.evar closure_env_arg_var (mark_ty (TClosureEnv, pos))]
         (mark_ty env_ty))
      (Expr.make_multiple_let_in
         (List.map mark_pos free_vars)
         free_vars_types
         (List.mapi
            (fun i _ ->
              Expr.make_tupleaccess
                (Expr.evar closure_env_var (mark_ty env_ty))
                i (List.length free_vars) pos)
            free_vars)
         body pos)
      pos
  in
  (* fun env arg0 ... -> new_closure_body *)
  let new_closure =
    Expr.make_ghost_abs
      (closure_env_arg_var :: Array.to_list args)
      new_closure_body
      ((TClosureEnv, pos) :: tys)
      pos
  in
  let new_closure_ty = Expr.maybe_ty (Mark.get new_closure) in
  Expr.make_let_in (Mark.ghost code_var) new_closure_ty new_closure
    (Expr.make_tuple
       ((Bindlib.box_var code_var, mark_ty new_closure_ty)
       :: [
            Expr.eappop
              ~op:(Operator.ToClosureEnv, pos)
              ~tys:[TTuple free_vars_types, pos]
              ~args:
                [
                  Expr.etuple
                    (List.map
                       (fun (extra_var, m) ->
                         Bindlib.box_var extra_var, Expr.with_pos pos m)
                       free_vars)
                    (mark_ty (TTuple free_vars_types, pos));
                ]
              (mark_ty (TClosureEnv, pos));
          ])
       m)
    pos

(** Returns the expression with closed closures and the set of free variables
    inside this new expression. Implementation guided by
    http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=10
    (environment-passing closure conversion). *)
let rec transform_closures_expr :
    type m. m ctx -> m expr -> (m expr, m mark) Var.Map.t * m expr boxed =
 fun ctx e ->
  let e = translate_mark e in
  let m = Mark.get e in
  match Mark.remove e with
  | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _ | EArray _
  | ELit _ | EAssert _ | EFatalError _ | EPos _ | EIfThenElse _ ->
    Expr.map_gather ~acc:Var.Map.empty ~join:join_vars
      ~f:(transform_closures_expr ctx)
      e
  | (EVar _ | EExternal _) as e -> (
    let body, (free_vars, fty) =
      match e with
      | EVar v -> (
        ( Bindlib.box_var v,
          match Var.Map.find_opt v ctx.globally_bound_vars with
          | None -> Var.Map.singleton v m, None
          | Some ((TArrow (targs, tret), _) as fty) ->
            Var.Map.empty, Some (targs, tret, fty)
          | Some _ -> Var.Map.empty, None ))
      | EExternal { name = External_value td, _ } as e ->
        ( Bindlib.box e,
          ( Var.Map.empty,
            match TopdefName.Map.find td ctx.decl_ctx.ctx_topdefs with
            | ((TArrow (targs, tret), _) as fty), _vis -> Some (targs, tret, fty)
            | _ -> None ) )
      | EExternal { name = External_scope s, pos } ->
        let fty =
          let si = ScopeName.Map.find s ctx.decl_ctx.ctx_scopes in
          let t_in = TStruct si.in_struct_name, pos in
          let t_out = TStruct si.out_struct_name, pos in
          [t_in], t_out, (TArrow ([t_in], t_out), pos)
        in
        Bindlib.box e, (Var.Map.empty, Some fty)
      | _ -> assert false
    in
    match fty with
    | None -> free_vars, (body, m)
    | Some (targs, tret, fty) ->
      (* Here we eta-expand the argument to make sure function pointers are
         correctly casted as closures *)
      let args =
        Array.init (List.length targs) (fun i ->
            Var.make ("x" ^ string_of_int i))
      in
      let arg_vars =
        List.map2
          (fun v ty -> Expr.evar v (Expr.with_ty m ty))
          (Array.to_list args) targs
      in
      let closure =
        let body =
          Expr.eapp
            ~f:(body, Expr.with_ty m fty)
            ~args:arg_vars ~tys:targs (Expr.with_ty m tret)
        in
        build_closure ctx [] body args targs m
      in
      Var.Map.empty, closure)
  | EMatch { e; cases; name } ->
    let free_vars, new_e = (transform_closures_expr ctx) e in
    (* We do not close the clotures inside the arms of the match expression,
       since they get a special treatment at compilation to Scalc. *)
    let free_vars, new_cases =
      EnumConstructor.Map.fold
        (fun cons e1 (free_vars, new_cases) ->
          match Mark.remove e1 with
          | EAbs { binder; pos; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_free_vars, new_body = (transform_closures_expr ctx) body in
            let new_free_vars =
              Array.fold_left
                (fun acc v -> Var.Map.remove v acc)
                new_free_vars vars
            in
            let new_binder = Expr.bind vars new_body in
            ( join_vars free_vars new_free_vars,
              EnumConstructor.Map.add cons
                (Expr.eabs new_binder pos tys (Mark.get e1))
                new_cases )
          | _ -> assert false)
        cases
        (free_vars, EnumConstructor.Map.empty)
    in
    free_vars, Expr.ematch ~e:new_e ~name ~cases:new_cases m
  | EApp { f = EAbs { binder; pos; tys }, e1_pos; args; _ } ->
    (* let-binding, we should not close these *)
    let vars, body = Bindlib.unmbind binder in
    let free_vars, new_body = (transform_closures_expr ctx) body in
    let free_vars =
      Array.fold_left (fun acc v -> Var.Map.remove v acc) free_vars vars
    in
    let new_binder = Expr.bind vars new_body in
    let free_vars, new_args =
      List.fold_right
        (fun arg (free_vars, new_args) ->
          let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
          join_vars free_vars new_free_vars, new_arg :: new_args)
        args (free_vars, [])
    in
    ( free_vars,
      Expr.eapp
        ~f:(Expr.eabs new_binder pos (List.map translate_type tys) e1_pos)
        ~args:new_args ~tys m )
  | EAbs { binder; pos = _; tys } ->
    (* Converting the closure. *)
    let vars, body = Bindlib.unmbind binder in
    (* t *)
    let free_vars, body = (transform_closures_expr ctx) body in
    (* [[t]] *)
    let free_vars =
      Array.fold_left (fun m v -> Var.Map.remove v m) free_vars vars
    in
    free_vars, build_closure ctx (Var.Map.bindings free_vars) body vars tys m
  | EAppOp
      {
        op = ((HandleExceptions | Fold | Map | Map2 | Filter | Reduce), _) as op;
        tys = tyf :: targs;
        args = f :: args;
      }
    when ctx.flags.keep_special_ops ->
    let free_vars, f = transform_closures_expr ctx f in
    let free_vars, args =
      List.fold_right
        (fun a (free_vars, args) ->
          let free_vars1, a = transform_closures_expr ctx a in
          join_vars free_vars free_vars1, a :: args)
        args (free_vars, [])
    in
    ( free_vars,
      Expr.eappop ~op ~tys:(tyf :: targs) ~args:(f :: args) (Mark.get e) )
  | EAppOp _ ->
    (* This corresponds to an operator call, which we don't want to transform *)
    Expr.map_gather ~acc:Var.Map.empty ~join:join_vars
      ~f:(transform_closures_expr ctx)
      e
  | EApp { f = EVar v, f_m; args; tys }
    when Var.Map.mem v ctx.globally_bound_vars ->
    (* This corresponds to a scope or toplevel function call, which we don't
       want to transform *)
    let free_vars, new_args =
      List.fold_right
        (fun arg (free_vars, new_args) ->
          let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
          join_vars free_vars new_free_vars, new_arg :: new_args)
        args (Var.Map.empty, [])
    in
    free_vars, Expr.eapp ~f:(Expr.evar v f_m) ~args:new_args ~tys m
  | EApp { f = e1; args; tys } ->
    let free_vars, new_e1 = (transform_closures_expr ctx) e1 in
    let tys = List.map translate_type tys in
    let pos = Expr.mark_pos m in
    let env_arg_ty = TClosureEnv, Expr.pos new_e1 in
    let code_env_var = Var.make "code_and_env" in
    let code_env_expr =
      let pos = Expr.pos e1 in
      Expr.evar code_env_var
        (Expr.with_ty (Mark.get e1)
           ( TTuple
               [
                 TArrow ((TClosureEnv, pos) :: tys, Expr.maybe_ty m), Expr.pos e;
                 TClosureEnv, pos;
               ],
             pos ))
    in
    let free_vars, new_args =
      List.fold_right
        (fun arg (free_vars, new_args) ->
          let new_free_vars, new_arg = (transform_closures_expr ctx) arg in
          join_vars free_vars new_free_vars, new_arg :: new_args)
        args (free_vars, [])
    in
    let call_expr =
      Expr.make_app
        (Expr.make_tupleaccess code_env_expr 0 2 pos)
        (Expr.make_tupleaccess code_env_expr 1 2 pos :: new_args)
        (env_arg_ty :: tys) pos
    in

    ( free_vars,
      Expr.make_let_in (Mark.ghost code_env_var) (Type.any pos) new_e1 call_expr
        pos )
  | _ -> .

let transform_closures_scope_let ctx scope_body_expr =
  BoundList.map
    ~f:(fun var_next scope_let ->
      let _free_vars, new_scope_let_expr =
        (transform_closures_expr
           { ctx with name_context = new_context (Bindlib.name_of var_next) })
          scope_let.scope_let_expr
      in
      ( var_next,
        Bindlib.box_apply
          (fun scope_let_expr ->
            {
              scope_let with
              scope_let_expr;
              scope_let_typ = translate_type scope_let.scope_let_typ;
            })
          (Expr.Box.lift new_scope_let_expr) ))
    ~last:(fun res ->
      let _free_vars, new_scope_let_expr = (transform_closures_expr ctx) res in
      (* INVARIANT here: the result expr of a scope is simply a struct
         containing all output variables so nothing should be converted here, so
         no need to take into account free variables. *)
      Expr.Box.lift new_scope_let_expr)
    scope_body_expr

let transform_closures_program ~flags (p : 'm program) : 'm program Bindlib.box
    =
  let (), new_code_items =
    BoundList.fold_map
      ~f:(fun toplevel_vars var code_item ->
        match code_item with
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr =
            Bindlib.unbind body.scope_body_expr
          in
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = new_context (ScopeName.base name);
              flags;
              globally_bound_vars = toplevel_vars;
            }
          in
          let new_scope_lets =
            transform_closures_scope_let ctx scope_body_expr
          in
          let new_scope_body_expr =
            Bindlib.bind_var scope_input_var new_scope_lets
          in
          let ty =
            let pos = Mark.get (ScopeName.get_info name) in
            ( TArrow
                ( [TStruct body.scope_body_input_struct, pos],
                  (TStruct body.scope_body_output_struct, pos) ),
              pos )
          in
          ( Var.Map.add var ty toplevel_vars,
            var,
            Bindlib.box_apply
              (fun scope_body_expr ->
                ScopeDef (name, { body with scope_body_expr }))
              new_scope_body_expr )
        | Topdef (name, ty, vis, (EAbs { binder; pos; tys }, m)) ->
          let v, expr = Bindlib.unmbind binder in
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = new_context (TopdefName.base name);
              flags;
              globally_bound_vars = toplevel_vars;
            }
          in
          let _free_vars, new_expr = transform_closures_expr ctx expr in
          let new_binder = Expr.bind v new_expr in
          ( Var.Map.add var ty toplevel_vars,
            var,
            Bindlib.box_apply
              (fun e -> Topdef (name, ty, vis, e))
              (Expr.Box.lift (Expr.eabs new_binder pos tys m)) )
        | Topdef (name, ty, vis, expr) ->
          let ctx =
            {
              decl_ctx = p.decl_ctx;
              name_context = new_context (TopdefName.base name);
              flags;
              globally_bound_vars = toplevel_vars;
            }
          in
          let _free_vars, new_expr = transform_closures_expr ctx expr in
          ( Var.Map.add var ty toplevel_vars,
            var,
            Bindlib.box_apply
              (fun e -> Topdef (name, ty, vis, e))
              (Expr.Box.lift new_expr) ))
      ~last:(fun toplevel_vars exports ->
        ( (),
          Bindlib.box_list
            (List.map
               (fun (k, e) ->
                 let ctx =
                   {
                     decl_ctx = p.decl_ctx;
                     name_context =
                       new_context
                         (match k with
                         | KScope n | KTest n -> fst (ScopeName.get_info n)
                         | KTopdef n -> TopdefName.base n);
                     flags;
                     globally_bound_vars = toplevel_vars;
                   }
                 in
                 let _free_vars, e = transform_closures_expr ctx e in
                 Bindlib.box_apply (fun e -> k, e) (Expr.Box.lift e))
               exports) ))
      ~init:Var.Map.empty p.code_items
  in
  (* Now we need to further tweak [decl_ctx] because some of the user-defined
     types can have closures in them and these closured might have changed type.
     So we reset them to [TForAll] and leave the typechecker to figure it out.
     This will not yield any type unification conflicts because of the special
     type [TClosureEnv]. Indeed, consider the following closure: [let f = if ...
     then fun v -> x + v else fun v -> v]. To be typed correctly once converted,
     this closure needs an existential type, this is what [TClosureEnv] is for.
     This kind of situation is difficult to produce using the Catala surface
     language: it can only happen if you store a closure which is the output of
     a scope inside a user-defined data structure, and if you do it in two
     different places in the code with two closures that don't have the same
     capture footprint. See
     [tests/tests_func/good/scope_call_func_struct_closure.catala_en]. *)
  let new_decl_ctx =
    {
      p.decl_ctx with
      ctx_structs =
        StructName.Map.map
          (StructField.Map.map translate_type)
          p.decl_ctx.ctx_structs;
      ctx_enums =
        EnumName.Map.map
          (EnumConstructor.Map.map translate_type)
          p.decl_ctx.ctx_enums;
      (* Toplevel definitions may not contain scope calls or take functions as
         arguments at the moment, which ensures that their interfaces aren't
         changed by the conversion *)
    }
  in
  Bindlib.box_apply
    (fun new_code_items ->
      {
        code_items = new_code_items;
        decl_ctx = new_decl_ctx;
        module_name = p.module_name;
        lang = p.lang;
      })
    new_code_items

(** {1 Hoisting closures} *)

type 'm hoisted_closure = {
  name : 'm expr Var.t;
  ty : typ;
  closure : (lcalc, 'm) boxed_gexpr (* Starts with [EAbs]. *);
}

let rec hoist_closures_expr :
    type m.
    flags -> name_context -> m expr -> m hoisted_closure list * m expr boxed =
 fun flags name_context e ->
  let m = Mark.get e in
  match Mark.remove e with
  | EMatch { e; cases; name } ->
    let collected_closures, new_e =
      (hoist_closures_expr flags name_context) e
    in
    (* We do not close the closures inside the arms of the match expression,
       since they get a special treatment at compilation to Scalc. *)
    let collected_closures, new_cases =
      EnumConstructor.Map.fold
        (fun cons e1 (collected_closures, new_cases) ->
          match Mark.remove e1 with
          | EAbs { binder; pos; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let new_collected_closures, new_body =
              (hoist_closures_expr flags name_context) body
            in
            let new_binder = Expr.bind vars new_body in
            ( collected_closures @ new_collected_closures,
              EnumConstructor.Map.add cons
                (Expr.eabs new_binder pos tys (Mark.get e1))
                new_cases )
          | _ -> assert false)
        cases
        (collected_closures, EnumConstructor.Map.empty)
    in
    collected_closures, Expr.ematch ~e:new_e ~name ~cases:new_cases m
  | EApp { f = EAbs { binder; pos; tys }, e1_pos; args; _ } ->
    (* let-binding, we should not close these *)
    let vars, body = Bindlib.unmbind binder in
    let collected_closures, new_body =
      (hoist_closures_expr flags name_context) body
    in
    let new_binder = Expr.bind vars new_body in
    let collected_closures, new_args =
      List.fold_right
        (fun arg (collected_closures, new_args) ->
          let new_collected_closures, new_arg =
            (hoist_closures_expr flags name_context) arg
          in
          collected_closures @ new_collected_closures, new_arg :: new_args)
        args (collected_closures, [])
    in
    ( collected_closures,
      Expr.eapp ~f:(Expr.eabs new_binder pos tys e1_pos) ~args:new_args ~tys m )
  | EAppOp { op = ((Fold | Map | Map2 | Filter | Reduce), _) as op; tys; args }
    when flags.keep_special_ops ->
    (* Special case for some operators: its arguments closures thunks because if
       you want to extract it as a function you need these closures to preserve
       evaluation order, but backends that don't support closures will simply
       extract these operators in a inlined way and skip the thunks. *)
    let collected_closures, new_args =
      List.fold_right
        (fun (arg : (lcalc, m) gexpr) (collected_closures, new_args) ->
          let m_arg = Mark.get arg in
          match Mark.remove arg with
          | EAbs { binder; pos; tys } ->
            let vars, arg = Bindlib.unmbind binder in
            let new_collected_closures, new_arg =
              (hoist_closures_expr flags name_context) arg
            in
            let vars =
              List.map2 (fun v p -> Mark.add p v) (Array.to_list vars) pos
            in
            let new_arg =
              Expr.make_abs vars new_arg tys (Expr.mark_pos m_arg)
            in
            new_collected_closures @ collected_closures, new_arg :: new_args
          | _ ->
            let new_collected_closures, new_arg =
              hoist_closures_expr flags name_context arg
            in
            new_collected_closures @ collected_closures, new_arg :: new_args)
        args ([], [])
    in
    collected_closures, Expr.eappop ~op ~args:new_args ~tys (Mark.get e)
  | EAbs { binder; pos = posl; tys } ->
    (* this is the closure we want to hoist *)
    let closure_var = new_var ~pfx:"closure_" name_context in
    let pos = Expr.mark_pos m in
    let ty = Expr.maybe_ty ~typ:(TArrow (tys, Type.any pos)) m in
    let vars, body = Bindlib.unmbind binder in
    let vars = List.map2 (fun v p -> Mark.add p v) (Array.to_list vars) posl in
    let collected_closures, new_body =
      (hoist_closures_expr flags name_context) body
    in
    let closure = Expr.make_abs vars new_body tys pos in
    ( { name = closure_var; ty; closure } :: collected_closures,
      Expr.make_var closure_var m )
  | EApp _ | EStruct _ | EStructAccess _ | ETuple _ | ETupleAccess _ | EInj _
  | EArray _ | ELit _ | EAssert _ | EFatalError _ | EPos _ | EAppOp _
  | EIfThenElse _ | EVar _ ->
    Expr.map_gather ~acc:[] ~join:( @ )
      ~f:(hoist_closures_expr flags name_context)
      e
  | EExternal { name } -> [], Expr.box (EExternal { name }, m)
  | _ -> .

let hoist_closures_scope_let flags name_context scope_body_expr =
  BoundList.fold_right
    ~f:(fun scope_let var_next (hoisted_closures, next_scope_lets) ->
      let new_hoisted_closures, new_scope_let_expr =
        (hoist_closures_expr flags (new_context (Bindlib.name_of var_next)))
          scope_let.scope_let_expr
      in
      ( new_hoisted_closures @ hoisted_closures,
        Bindlib.box_apply2
          (fun scope_let_next scope_let_expr ->
            Cons ({ scope_let with scope_let_expr }, scope_let_next))
          (Bindlib.bind_var var_next next_scope_lets)
          (Expr.Box.lift new_scope_let_expr) ))
    ~init:(fun res ->
      let hoisted_closures, new_scope_let_expr =
        (hoist_closures_expr flags name_context) res
      in
      (* INVARIANT here: the result expr of a scope is simply a struct
         containing all output variables so nothing should be converted here, so
         no need to take into account free variables. *)
      ( hoisted_closures,
        Bindlib.box_apply
          (fun res -> Last res)
          (Expr.Box.lift new_scope_let_expr) ))
    scope_body_expr

let hoist_closures_items closures items =
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
                  Private,
                  (closure, closure_mark) ),
              next_code_items ))
        next_code_items closure)
    items closures

let rec hoist_closures_code_item_list
    flags
    (code_items : (lcalc, 'm) gexpr code_item_list) :
    (lcalc, 'm) gexpr code_item_list Bindlib.box =
  match code_items with
  | Last exports ->
    let rev_exports =
      List.rev_map
        (fun (k, e) ->
          match k with
          | KTopdef _ | KScope _ ->
            (* These are just holders for variables, they shouldn't require
               conversion *)
            k, Expr.rebox e
          | KTest n ->
            let name, _ = ScopeName.get_info n in
            let new_closures, e =
              hoist_closures_expr flags (new_context name) e
            in
            (* The closures need to remain local to the test, because that may
               be spilled into another file. So we re-embed them using a let-in,
               to be deconstructed at the last stages of compilation *)
            ( k,
              List.fold_left
                (fun e cl ->
                  Expr.make_let_in
                    (cl.name, Expr.pos cl.closure)
                    cl.ty cl.closure e (Expr.pos cl.closure))
                e new_closures ))
        exports
    in
    Bindlib.box_apply
      (fun ex -> Last ex)
      (Bindlib.box_list
         (List.rev_map
            (fun (k, e) -> Bindlib.box_apply (fun e -> k, e) (Expr.Box.lift e))
            rev_exports))
  | Cons (code_item, next_code_items) ->
    let code_item_var, next_code_items = Bindlib.unbind next_code_items in
    let hoisted_closures, new_code_item =
      match code_item with
      | ScopeDef (name, body) ->
        let scope_input_var, scope_body_expr =
          Bindlib.unbind body.scope_body_expr
        in
        let new_hoisted_closures, new_scope_lets =
          hoist_closures_scope_let flags
            (new_context (fst (ScopeName.get_info name)))
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
      | Topdef (name, ty, vis, (EAbs { binder; pos; tys }, m)) ->
        let v, expr = Bindlib.unmbind binder in
        let new_hoisted_closures, new_expr =
          hoist_closures_expr flags (new_context (TopdefName.base name)) expr
        in
        let new_binder = Expr.bind v new_expr in
        ( new_hoisted_closures,
          Bindlib.box_apply
            (fun e -> Topdef (name, ty, vis, e))
            (Expr.Box.lift (Expr.eabs new_binder pos tys m)) )
      | Topdef (name, ty, vis, expr) ->
        let new_hoisted_closures, new_expr =
          hoist_closures_expr flags (new_context (TopdefName.base name)) expr
        in
        ( new_hoisted_closures,
          Bindlib.box_apply
            (fun e -> Topdef (name, ty, vis, e))
            (Expr.Box.lift new_expr) )
    in
    let next_code_items = hoist_closures_code_item_list flags next_code_items in
    let next_code_items =
      Bindlib.box_apply2
        (fun next_code_items new_code_item ->
          Cons (new_code_item, next_code_items))
        (Bindlib.bind_var code_item_var next_code_items)
        new_code_item
    in
    let next_code_items =
      hoist_closures_items hoisted_closures next_code_items
    in
    next_code_items

let hoist_closures_program ~flags (p : 'm program) : 'm program Bindlib.box =
  let new_code_items = hoist_closures_code_item_list flags p.code_items in
  Bindlib.box_apply (fun code_items -> { p with code_items }) new_code_items

(** {1 Closure conversion}*)

let closure_conversion ~keep_special_ops (p : 'm program) : 'm program =
  let new_p = transform_closures_program ~flags:{ keep_special_ops } p in
  let new_p =
    hoist_closures_program ~flags:{ keep_special_ops } (Bindlib.unbox new_p)
  in
  Bindlib.unbox new_p
