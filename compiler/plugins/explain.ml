(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>.

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

type flags = {
  with_conditions : bool;
  with_cleanup : bool;
  merge_level : int;
  format : [ `Dot | `Convert of string ];
  show : string option;
  output : string option;
  base_src_url : string;
}

(* -- Definition of the lazy interpreter -- *)

let log fmt = Format.ifprintf Format.err_formatter (fmt ^^ "@\n")
let error e = Message.raise_spanned_error (Expr.pos e)
let noassert = true

module Env = struct
  type t = Env of (expr, elt) Var.Map.t
  and elt = { base : expr * t; mutable reduced : expr * t }
  and expr = (dcalc, annot custom) gexpr
  and annot = { conditions : (expr * t) list }

  let find v (Env t) = Var.Map.find v t

  (* let get_bas v t = let v, env = find v t in v, !env *)
  let add v e e_env (Env t) =
    Env (Var.Map.add v { base = e, e_env; reduced = e, e_env } t)

  let empty = Env Var.Map.empty

  let join (Env t1) (Env t2) =
    Env
      (Var.Map.union
         (fun _ x1 x2 ->
           (* assert (x1 == x2); *)
           Some x2)
         t1 t2)

  let print ppf (Env t) =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf v -> Print.var_debug ppf v)
      ppf (Var.Map.keys t)
end

type expr = Env.expr
type annot = Env.annot = { conditions : (expr * Env.t) list }

type laziness_level = {
  eval_struct : bool;
      (* if true, evaluate members of structures, tuples, etc. *)
  eval_op : bool;
      (* if false, evaluate the operands but keep e.g. `3 + 4` as is *)
  eval_match : bool;
  eval_default : bool;
  (* if false, stop evaluating as soon as you can discriminate with
     `EEmptyError` *)
  eval_vars : expr Var.t -> bool;
      (* if false, variables are only resolved when they point to another
         unchanged variable *)
}

let value_level =
  {
    eval_struct = false;
    eval_op = true;
    eval_match = true;
    eval_default = true;
    eval_vars = (fun _ -> true);
  }

let add_condition ~condition e =
  Mark.map_mark
    (fun (Custom { pos; custom = { conditions } }) ->
      Custom { pos; custom = { conditions = condition :: conditions } })
    e

let add_conditions ~conditions e =
  Mark.map_mark
    (fun (Custom { pos; custom = { conditions = c } }) ->
      Custom { pos; custom = { conditions = conditions @ c } })
    e

let neg_op = function
  | Op.Xor ->
    Some Op.Eq
    (* Alright, we are cheating here since the type is wider, but the
       transformation preserves the semantics *)
  | Op.Lt_int_int -> Some Op.Gte_int_int
  | Op.Lt_rat_rat -> Some Op.Gte_rat_rat
  | Op.Lt_mon_mon -> Some Op.Gte_mon_mon
  | Op.Lt_dat_dat -> Some Op.Gte_dat_dat
  | Op.Lt_dur_dur -> Some Op.Gte_dur_dur
  | Op.Lte_int_int -> Some Op.Gt_int_int
  | Op.Lte_rat_rat -> Some Op.Gt_rat_rat
  | Op.Lte_mon_mon -> Some Op.Gt_mon_mon
  | Op.Lte_dat_dat -> Some Op.Gt_dat_dat
  | Op.Lte_dur_dur -> Some Op.Gt_dur_dur
  | Op.Gt_int_int -> Some Op.Lte_int_int
  | Op.Gt_rat_rat -> Some Op.Lte_rat_rat
  | Op.Gt_mon_mon -> Some Op.Lte_mon_mon
  | Op.Gt_dat_dat -> Some Op.Lte_dat_dat
  | Op.Gt_dur_dur -> Some Op.Lte_dur_dur
  | Op.Gte_int_int -> Some Op.Lt_int_int
  | Op.Gte_rat_rat -> Some Op.Lt_rat_rat
  | Op.Gte_mon_mon -> Some Op.Lt_mon_mon
  | Op.Gte_dat_dat -> Some Op.Lt_dat_dat
  | Op.Gte_dur_dur -> Some Op.Lt_dur_dur
  | _ -> None

let rec bool_negation e =
  match Expr.skip_wrappers e with
  | ELit (LBool true), m -> ELit (LBool false), m
  | ELit (LBool false), m -> ELit (LBool true), m
  | EApp { f = EOp { op = Op.Not; _ }, _; args = [(e, _)] }, m -> e, m
  | (EApp { f = EOp { op; tys }, mop; args = [e1; e2] }, m) as e -> (
    match op with
    | Op.And ->
      ( EApp
          {
            f = EOp { op = Op.Or; tys }, mop;
            args = [bool_negation e1; bool_negation e2];
          },
        m )
    | Op.Or ->
      ( EApp
          {
            f = EOp { op = Op.And; tys }, mop;
            args = [bool_negation e1; bool_negation e2];
          },
        m )
    | op -> (
      match neg_op op with
      | Some op -> EApp { f = EOp { op; tys }, mop; args = [e1; e2] }, m
      | None ->
        ( EApp
            {
              f = EOp { op = Op.Not; tys = [TLit TBool, Expr.mark_pos m] }, m;
              args = [e];
            },
          m )))
  | (_, m) as e ->
    ( EApp
        {
          f = EOp { op = Op.Not; tys = [TLit TBool, Expr.mark_pos m] }, m;
          args = [e];
        },
      m )

let rec lazy_eval : decl_ctx -> Env.t -> laziness_level -> expr -> expr * Env.t
    =
 fun ctx env llevel e0 ->
  let eval_to_value ?(eval_default = true) env e =
    lazy_eval ctx env { value_level with eval_default } e
  in
  match e0 with
  | EVar v, _ ->
    if (not llevel.eval_default) || not (llevel.eval_vars v) then e0, env
    else
      (* Variables reducing to EEmpty should not propagate to parent EDefault
         (?) *)
      let env_elt =
        try Env.find v env
        with Var.Map.Not_found _ ->
          error e0 "Variable %a undefined [@[<hv>%a@]]" Print.var_debug v
            Env.print env
      in
      let e, env1 = env_elt.reduced in
      let r, env1 = lazy_eval ctx env1 llevel e in
      env_elt.reduced <- r, env1;
      r, Env.join env env1
  | EApp { f; args }, m -> (
    if
      (not llevel.eval_default)
      && not (List.equal Expr.equal args [ELit LUnit, m])
      (* Applications to () encode thunked default terms *)
    then e0, env
    else
      match eval_to_value env f with
      | (EAbs { binder; _ }, _), env ->
        let vars, body = Bindlib.unmbind binder in
        log "@[<v 2>@[<hov 4>{";
        let env =
          Seq.fold_left2
            (fun env1 var e ->
              log "@[<hov 2>LET %a = %a@]@ " Print.var_debug var Expr.format e;
              Env.add var e env env1)
            env (Array.to_seq vars) (List.to_seq args)
        in
        log "@]@[<hov 4>IN [%a]@]" (Print.expr ~debug:true ()) body;
        let e, env = lazy_eval ctx env llevel body in
        log "@]}";
        e, env
      | ( ( EOp
              {
                op = (Op.Map | Op.Filter | Op.Reduce | Op.Fold | Op.Length) as op;
                tys;
              },
            m ),
          env (* when not llevel.eval_op *) ) -> (
        (* Distribute collection operations to the terms rather than use their
           runtime implementations *)
        let arr = List.hd (List.rev args) in
        (* All these ops have the array as last arg *)
        let aty = List.hd (List.rev tys) in
        match eval_to_value env arr with
        | (EArray elts, _), env ->
          let eapp f e = EApp { f; args = [e] }, m in
          let empty_condition () =
            (* Is the expression [length(arr) = 0] *)
            let pos = Expr.mark_pos m in
            ( EApp
                {
                  f =
                    ( EOp
                        {
                          op = Op.Eq_int_int;
                          tys = [TLit TInt, pos; TLit TInt, pos];
                        },
                      m );
                  args =
                    [
                      ( EApp
                          {
                            f = EOp { op = Op.Length; tys = [aty] }, m;
                            args = [arr];
                          },
                        m );
                      ELit (LInt (Runtime.integer_of_int 0)), m;
                    ];
                },
              m )
          in
          let e, env =
            match op, args, elts with
            | (Op.Map | Op.Filter), _, [] ->
              let e = EArray [], m in
              add_condition ~condition:(empty_condition (), env) e, env
            | (Op.Reduce | Op.Fold), [_; dft; _], [] ->
              add_condition ~condition:(empty_condition (), env) dft, env
            | Op.Map, [f; _], elts -> (EArray (List.map (eapp f) elts), m), env
            | Op.Filter, [f; _], elts ->
              let rev_elts, env =
                List.fold_left
                  (fun (elts, env) e ->
                    let cond = eapp f e in
                    match lazy_eval ctx env value_level cond with
                    | (ELit (LBool true), _), _ ->
                      add_condition ~condition:(cond, env) e :: elts, env
                    | (ELit (LBool false), _), _ -> elts, env
                    | _ -> assert false)
                  ([], env) elts
              in
              (EArray (List.rev rev_elts), m), env
            (* Note: no annots for removed terms, even if the result is empty *)
            | Op.Reduce, [f; _; _], elt0 :: elts ->
              let e =
                List.fold_left
                  (fun acc elt -> EApp { f; args = [acc; elt] }, m)
                  elt0 elts
              in
              e, env
            | Op.Fold, [f; base; _], elts ->
              let e =
                List.fold_left
                  (fun acc elt -> EApp { f; args = [acc; elt] }, m)
                  base elts
              in
              e, env
            | Op.Length, [_], elts ->
              (ELit (LInt (Runtime.integer_of_int (List.length elts))), m), env
            | _ -> assert false
          in
          (* We did a transformation (removing the outer operator), but further
             evaluation may be needed to guarantee that [llevel] is reached *)
          lazy_eval ctx env { llevel with eval_match = true } e
        | _ -> (EApp { f; args }, m), env)
      | ((EOp { op; _ }, m) as f), env ->
        let env, args =
          List.fold_left_map
            (fun env e ->
              let e, env = lazy_eval ctx env llevel e in
              env, e)
            env args
        in
        if not llevel.eval_op then (EApp { f; args }, m), env
        else
          let renv = ref env in
          (* Dirty workaround returning env and conds from evaluate_operator *)
          let eval e =
            let e, env = lazy_eval ctx !renv llevel e in
            renv := env;
            e
          in
          let e =
            Interpreter.evaluate_operator eval op m Cli.En
              (* Default language to English but this should not raise any error
                 messages so we don't care. *)
              args
          in
          e, !renv
      (* fixme: this forwards eempty *)
      | e, _ -> error e "Invalid apply on %a" Expr.format e)
  | (EAbs _ | ELit _ | EOp _ | EEmptyError), _ -> e0, env (* these are values *)
  | (EStruct _ | ETuple _ | EInj _ | EArray _), _ ->
    if not llevel.eval_struct then e0, env
    else
      let env, e =
        Expr.map_gather ~acc:env ~join:Env.join
          ~f:(fun e ->
            let e, env = lazy_eval ctx env llevel e in
            env, Expr.box e)
          e0
      in
      Expr.unbox e, env
  | EStructAccess { e; name; field }, _ -> (
    if not llevel.eval_default then e0, env
    else
      match eval_to_value env e with
      | (EStruct { name = n; fields }, _), env when StructName.equal name n ->
        let e, env =
          lazy_eval ctx env llevel (StructField.Map.find field fields)
        in
        e, env
      | _ -> e0, env)
  | ETupleAccess { e; index; size }, _ -> (
    if not llevel.eval_default then e0, env
    else
      match eval_to_value env e with
      | (ETuple es, _), env when List.length es = size ->
        lazy_eval ctx env llevel (List.nth es index)
      | e, _ -> error e "Invalid tuple access on %a" Expr.format e)
  | EMatch { e; name; cases }, _ -> (
    if not llevel.eval_match then e0, env
    else
      match eval_to_value env e with
      | (EInj { name = n; cons; e = e1 }, m), env when EnumName.equal name n ->
        let condition = e, env in
        (* FIXME: condition should be "e TEST_MATCH n" but we don't have a
           concise expression to express that *)
        let e1, env =
          lazy_eval ctx env llevel
            (EApp { f = EnumConstructor.Map.find cons cases; args = [e1] }, m)
        in
        add_condition ~condition e1, env
      | e, _ -> error e "Invalid match argument %a" Expr.format e)
  | EDefault { excepts; just; cons }, m -> (
    let excs =
      List.filter_map
        (fun e ->
          match eval_to_value env e ~eval_default:false with
          | (EEmptyError, _), _ -> None
          | e -> Some e)
        excepts
    in
    match excs with
    | [] -> (
      match eval_to_value env just with
      | (ELit (LBool true), _), _ ->
        let condition = just, env in
        let e, env = lazy_eval ctx env llevel cons in
        add_condition ~condition e, env
      | (ELit (LBool false), _), _ -> (EEmptyError, m), env
      (* Note: conditions for empty are skipped *)
      | e, _ -> error e "Invalid exception justification %a" Expr.format e)
    | [(e, env)] ->
      log "@[<hov 5>EVAL %a@]" Expr.format e;
      lazy_eval ctx env llevel e
    | _ :: _ :: _ ->
      Message.raise_multispanned_error
        ((None, Expr.mark_pos m)
        :: List.map (fun (e, _) -> None, Expr.pos e) excs)
        "Conflicting exceptions")
  | EIfThenElse { cond; etrue; efalse }, _ -> (
    match eval_to_value env cond with
    | (ELit (LBool true), _), _ ->
      let condition = cond, env in
      let e, env = lazy_eval ctx env llevel etrue in
      add_condition ~condition e, env
    | (ELit (LBool false), m), _ -> (
      let condition = bool_negation cond, env in
      let e, env = lazy_eval ctx env llevel efalse in
      match efalse with
      (* The negated condition is not added for nested [else if] to reduce
         verbosity *)
      | EIfThenElse _, _ -> e, env
      | _ -> add_condition ~condition e, env)
    | e, _ -> error e "Invalid condition %a" Expr.format e)
  | EErrorOnEmpty e, _ -> (
    match eval_to_value env e ~eval_default:false with
    | ((EEmptyError, _) as e'), _ ->
      (* This does _not_ match the eager semantics ! *)
      error e' "This value is undefined %a" Expr.format e
    | e, env -> lazy_eval ctx env llevel e)
  | EAssert e, m -> (
    if noassert then (ELit LUnit, m), env
    else
      match eval_to_value env e with
      | (ELit (LBool true), m), env -> (ELit LUnit, m), env
      | (ELit (LBool false), _), _ ->
        error e "Assert failure (%a)" Expr.format e error e
          "Assert failure (%a)" Expr.format e
      | _ -> error e "Invalid assertion condition %a" Expr.format e)
  | EExternal _, _ -> assert false (* todo *)
  | _ -> .

let result_level base_vars =
  {
    value_level with
    eval_struct = true;
    eval_op = false;
    eval_vars = (fun v -> not (Var.Set.mem v base_vars));
  }

let interpret_program (prg : ('dcalc, 'm) gexpr program) (scope : ScopeName.t) :
    ('t, 'm) gexpr * Env.t =
  let ctx = prg.decl_ctx in
  let all_env, scopes =
    Scope.fold_left prg.code_items ~init:(Env.empty, ScopeName.Map.empty)
      ~f:(fun (env, scopes) item v ->
        match item with
        | ScopeDef (name, body) ->
          let e = Scope.to_expr ctx body (Scope.get_body_mark body) in
          let e = Expr.remove_logging_calls (Expr.unbox e) in
          ( Env.add v (Expr.unbox e) env env,
            ScopeName.Map.add name (v, body.scope_body_input_struct) scopes )
        | Topdef (_, _, e) -> Env.add v e env env, scopes)
  in
  let scope_v, _scope_arg_struct = ScopeName.Map.find scope scopes in
  let e, env = (Env.find scope_v all_env).base in
  log "=====================";
  log "%a" (Print.expr ~debug:true ()) e;
  log "=====================";
  (* let m = Mark.get e in *)
  (* let application_arg =
   *   Expr.estruct scope_arg_struct
   *     (StructField.Map.map
   *        (function
   *          | TArrow (ty_in, ty_out), _ ->
   *            Expr.make_abs
   *              [| Var.make "_" |]
   *              (Bindlib.box EEmptyError, Expr.with_ty m ty_out)
   *              ty_in (Expr.mark_pos m)
   *          | ty -> Expr.evar (Var.make "undefined_input") (Expr.with_ty m ty))
   *        (StructName.Map.find scope_arg_struct ctx.ctx_structs))
   *     m
   * in *)
  match e with
  | EAbs { binder; _ }, _ ->
    let _vars, e = Bindlib.unmbind binder in
    let rec get_vars base_vars env = function
      | EApp { f = EAbs { binder; _ }, _; args = [arg] }, _ ->
        let vars, e = Bindlib.unmbind binder in
        let var = vars.(0) in
        let base_vars =
          match Expr.skip_wrappers arg with
          | ELit _, _ -> Var.Set.add var base_vars
          | _ -> base_vars
        in
        let env = Env.add var arg env env in
        get_vars base_vars env e
      | e -> base_vars, env, e
    in
    let base_vars, env, e = get_vars Var.Set.empty env e in
    lazy_eval ctx env (result_level base_vars) e
  | _ -> assert false

let print_value_with_env ctx ppf env expr =
  let already_printed = ref Var.Set.empty in
  let rec aux env ppf expr =
    Print.expr ~debug:true () ppf expr;
    Format.pp_print_cut ppf ();
    let vars = Var.Set.diff (Expr.free_vars expr) !already_printed in
    Var.Set.iter
      (fun v ->
        let e, env = (Env.find v env).reduced in
        let e, env = lazy_eval ctx env (result_level Var.Set.empty) e in
        Format.fprintf ppf "@[<hov 2>%a %a =@ %a =@ %a@]@,@," Print.punctuation
          "»" Print.var_debug v Expr.format
          (fst (lazy_eval ctx env value_level e))
          (aux env) e)
      vars;
    already_printed := Var.Set.union !already_printed vars;
    Format.pp_print_cut ppf ()
  in
  Format.pp_open_vbox ppf 2;
  aux env ppf expr;
  Format.pp_close_box ppf ()

module V = struct
  type t = expr

  let compare a b = Expr.compare a b

  let hash = function
    | EVar v, _ -> Var.hash v
    | EAbs { tys; _ }, _ -> Hashtbl.hash tys
    | e, _ -> Hashtbl.hash e

  let equal a b = Expr.equal a b
  let format = Expr.format
end

module E = struct
  type hand_side = Lhs of string | Rhs of string
  type t = { side : hand_side option; condition : bool }

  let compare x y =
    match Bool.compare x.condition y.condition with
    | 0 ->
      Option.compare
        (fun x y ->
          match x, y with
          | Lhs s, Lhs t | Rhs s, Rhs t -> String.compare s t
          | Lhs _, Rhs _ -> -1
          | Rhs _, Lhs _ -> 1)
        x.side y.side
    | n -> n

  let default = { side = None; condition = false }
end

module G = Graph.Persistent.Digraph.AbstractLabeled (V) (E)

let op_kind = function
  | Op.Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
  | Sub_dur_dur ->
    `Sum
  | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Div_int_int
  | Div_rat_rat | Div_mon_rat | Div_mon_mon | Div_dur_dur ->
    `Product
  | Round_mon | Round_rat -> `Round
  | Map | Filter | Reduce | Fold -> `Fct
  | _ -> `Other

module GTopo = Graph.Topological.Make (G)

let to_graph ctx env expr =
  let rec aux env g e =
    (* lazy_eval ctx env (result_level base_vars) e *)
    match Expr.skip_wrappers e with
    | ( EApp
          {
            f = EOp { op = ToRat_int | ToRat_mon | ToMoney_rat; _ }, _;
            args = [arg];
          },
        _ ) ->
      aux env g arg
    (* we skip conversions *)
    | ELit l, _ ->
      let v = G.V.create e in
      G.add_vertex g v, v
    | (EVar var, _) as e ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let child, env = (Env.find var env).base in
      let g, child_v = aux env g child in
      G.add_edge g v child_v, v
    | EApp { f = EOp { op = _; _ }, _; args }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let g, children = List.fold_left_map (aux env) g args in
      List.fold_left (fun g -> G.add_edge g v) g children, v
    | EInj { e; _ }, _ -> aux env g e
    | EStruct { fields; _ }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let args = StructField.Map.values fields in
      let g, children = List.fold_left_map (aux env) g args in
      List.fold_left (fun g -> G.add_edge g v) g children, v
    | _ ->
      Format.eprintf "%a" Expr.format e;
      assert false
  in
  let base_g, _ = aux env G.empty expr in
  base_g

let rec is_const e =
  match Expr.skip_wrappers e with
  | ELit _, _ -> true
  | EInj { e; _ }, _ -> is_const e
  | EStruct { fields; _ }, _ ->
    StructField.Map.for_all (fun _ e -> is_const e) fields
  | EArray el, _ -> List.for_all is_const el
  | _ -> false

let program_to_graph
    options
    (prg : (dcalc, 'm) gexpr program)
    (scope : ScopeName.t) : G.t * expr Var.Set.t * Env.t =
  let ctx = prg.decl_ctx in
  let customize =
    Expr.map_marks ~f:(fun m ->
        Custom { pos = Expr.mark_pos m; custom = { conditions = [] } })
  in
  let all_env, scopes =
    Scope.fold_left prg.code_items ~init:(Env.empty, ScopeName.Map.empty)
      ~f:(fun (env, scopes) item v ->
        match item with
        | ScopeDef (name, body) ->
          let e = Scope.to_expr ctx body (Scope.get_body_mark body) in
          let e = customize (Expr.unbox e) in
          let e = Expr.remove_logging_calls (Expr.unbox e) in
          let e = Expr.rename_vars (Expr.unbox e) in
          ( Env.add (Var.translate v) (Expr.unbox e) env env,
            ScopeName.Map.add name (v, body.scope_body_input_struct) scopes )
        | Topdef (_, _, e) ->
          Env.add (Var.translate v) (Expr.unbox (customize e)) env env, scopes)
  in
  let scope_v, _scope_arg_struct = ScopeName.Map.find scope scopes in
  let e, env = (Env.find (Var.translate scope_v) all_env).base in
  let e =
    match e with
    | EAbs { binder; _ }, _ ->
      let _vars, e = Bindlib.unmbind binder in
      e
    | _ -> assert false
  in
  let rec get_vars base_vars env = function
    | EApp { f = EAbs { binder; _ }, _; args = [arg] }, _ ->
      let vars, e = Bindlib.unmbind binder in
      let var = vars.(0) in
      let base_vars =
        if is_const arg then Var.Set.add var base_vars else base_vars
      in
      let env = Env.add var arg env env in
      get_vars base_vars env e
    | e -> base_vars, env, e
  in
  let base_vars, env, e = get_vars Var.Set.empty env e in
  let e1, env = lazy_eval ctx env (result_level base_vars) e in
  let level =
    {
      value_level with
      eval_struct = false;
      eval_op = false;
      eval_match = false;
      eval_vars = (fun v -> false);
    }
  in
  let rec aux parent (g, var_vertices, env0) e =
    let e, env0 = lazy_eval ctx env0 level e in
    let m = Mark.get e in
    let (Custom { custom = { conditions; _ }; _ }) = m in
    let g, var_vertices, env0 =
      (* add conditions *)
      if not options.with_conditions then g, var_vertices, env0
      else
        match parent with
        | None -> g, var_vertices, env0
        | Some parent ->
          List.fold_left
            (fun (g, var_vertices, env0) (econd, env) ->
              let (g, var_vertices, env), vcond =
                aux (Some parent) (g, var_vertices, env) econd
              in
              ( G.add_edge_e g
                  (G.E.create parent { side = None; condition = true } vcond),
                var_vertices,
                Env.join env0 env ))
            (g, var_vertices, env0) conditions
    in
    let e = Mark.set m (Expr.skip_wrappers e) in
    match e with
    | ( EApp
          {
            f = EOp { op = ToRat_int | ToRat_mon | ToMoney_rat; _ }, _;
            args = [arg];
          },
        _ ) ->
      aux parent (g, var_vertices, env0) (Mark.set m arg)
    (* we skip conversions *)
    | ELit l, _ ->
      let v = G.V.create e in
      (G.add_vertex g v, var_vertices, env0), v
    | EVar var, _ -> (
      try (g, var_vertices, env0), Var.Map.find var var_vertices
      with Var.Map.Not_found _ -> (
        try
          let child, env = (Env.find var env0).base in
          let m = Mark.get child in
          let v = G.V.create (Mark.set m e) in
          let g = G.add_vertex g v in
          let (g, var_vertices, env), child_v =
            aux (Some v) (g, var_vertices, Env.join env0 env) child
          in
          let var_vertices =
            (* Duplicates non-base constant var nodes *)
            if Var.Set.mem var base_vars then var_vertices
            else
              let rec is_lit v =
                match G.V.label v with
                | ELit _, _ -> true
                | EVar var, _ when not (Var.Set.mem var base_vars) -> (
                  match G.succ g v with [v] -> is_lit v | _ -> false)
                | _ -> false
              in
              if is_lit child_v then var_vertices
                (* This duplicates constant var nodes *)
              else Var.Map.add var v var_vertices
          in
          (G.add_edge g v child_v, var_vertices, env), v
        with Var.Map.Not_found _ ->
          Message.emit_warning "VAR NOT FOUND: %a" Print.var var;
          let v = G.V.create e in
          let g = G.add_vertex g v in
          (g, var_vertices, env), v))
    | ( EApp
          {
            f = EOp { op = Map | Filter | Reduce | Fold; _ }, _;
            args = _ :: args;
          },
        _ ) ->
      (* First argument (which is a function) is ignored *)
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let (g, var_vertices, env), children =
        List.fold_left_map (aux (Some v)) (g, var_vertices, env0) args
      in
      ( (List.fold_left (fun g -> G.add_edge g v) g children, var_vertices, env),
        v )
    | EApp { f = EOp { op; _ }, _; args = [lhs; rhs] }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let (g, var_vertices, env), lhs =
        aux (Some v) (g, var_vertices, env0) lhs
      in
      let (g, var_vertices, env), rhs =
        aux (Some v) (g, var_vertices, env) rhs
      in
      let lhs_label, rhs_label =
        match op with
        | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur
          ->
          Some (E.Lhs "⊕"), Some (E.Rhs "⊕")
        | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
        | Sub_dur_dur ->
          Some (E.Lhs "⊕"), Some (E.Rhs "⊖")
        | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int ->
          Some (E.Lhs "⊗"), Some (E.Rhs "⊗")
        | Div_int_int | Div_rat_rat | Div_mon_rat | Div_mon_mon | Div_dur_dur ->
          Some (E.Lhs "⊗"), Some (E.Rhs "⊘")
        | _ -> None, None
      in
      let g =
        G.add_edge_e g
          (G.E.create v { side = lhs_label; condition = false } lhs)
      in
      let g =
        G.add_edge_e g
          (G.E.create v { side = rhs_label; condition = false } rhs)
      in
      (g, var_vertices, env), v
    | EApp { f = EOp { op = _; _ }, _; args }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let (g, var_vertices, env), children =
        List.fold_left_map (aux (Some v)) (g, var_vertices, env0) args
      in
      ( (List.fold_left (fun g -> G.add_edge g v) g children, var_vertices, env),
        v )
    | EInj { e; _ }, _ -> aux parent (g, var_vertices, env0) e
    | EStruct { fields; _ }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let args = StructField.Map.values fields in
      let (g, var_vertices, env), children =
        List.fold_left_map (aux (Some v)) (g, var_vertices, env0) args
      in
      ( (List.fold_left (fun g -> G.add_edge g v) g children, var_vertices, env),
        v )
    | EArray elts, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let (g, var_vertices, env), children =
        List.fold_left_map (aux (Some v)) (g, var_vertices, env0) elts
      in
      ( (List.fold_left (fun g -> G.add_edge g v) g children, var_vertices, env),
        v )
    | EAbs _, _ ->
      (g, var_vertices, env), G.V.create e (* (testing -> ignored) *)
    | EMatch { name; e; cases }, _ -> aux parent (g, var_vertices, env0) e
    | EStructAccess { e; field; _ }, _ ->
      let v = G.V.create e in
      let g = G.add_vertex g v in
      let (g, var_vertices, env), child =
        aux (Some v) (g, var_vertices, env0) e
      in
      (G.add_edge g v child, var_vertices, env), v
    | _ ->
      Format.eprintf "%a" Expr.format e;
      assert false
  in
  let (g, vmap, env), _ = aux None (G.empty, Var.Map.empty, env) e in
  log "BASE: @[<v>%a@]"
    (Format.pp_print_list Print.var)
    (Var.Set.elements base_vars);
  g, base_vars, env

let reverse_graph g =
  G.fold_edges_e
    (fun e g ->
      G.add_edge_e (G.remove_edge_e g e)
        (G.E.create (G.E.dst e) (G.E.label e) (G.E.src e)))
    g g

let subst_by v1 e2 e =
  let rec f = function
    | EVar v, m when Var.equal v v1 -> Expr.box e2
    | e -> Expr.map ~f e
  in
  Expr.unbox (f e)

let map_vertices f g =
  G.fold_vertex
    (fun v g ->
      let v' = G.V.create (f v) in
      let g =
        G.fold_pred_e
          (fun e g -> G.add_edge_e g (G.E.create (G.E.src e) (G.E.label e) v'))
          g v g
      in
      let g =
        G.fold_succ_e
          (fun e g -> G.add_edge_e g (G.E.create v' (G.E.label e) (G.E.dst e)))
          g v g
      in
      G.remove_vertex g v)
    g g

let rec graph_cleanup options g base_vars =
  (* let _g =
   *   let module GCtr = Graph.Contraction.Make (G) in
   *   GCtr.contract
   *     (fun e ->
   *       G.E.label e = None
   *       &&
   *       match G.V.label (G.E.src e), G.V.label (G.E.dst e) with
   *       | (EVar _, _), (EVar _, _) -> true
   *       | ( (EApp { f = EOp { op = op1; _ }, _; args = [_; _] }, _),
   *           (EApp { f = EOp { op = op2; _ }, _; args = [_; _] }, _) ) -> (
   *         match op_kind op1, op_kind op2 with
   *         | `Sum, `Sum -> true
   *         | `Prod, `Prod -> true
   *         | _ -> false)
   *       | _ -> false)
   *     g
   * in *)
  let module GTop = Graph.Topological.Make (G) in
  let module VMap = Map.Make (struct
    include G.V

    let format ppf v = V.format ppf (G.V.label v)
  end) in
  let g, vmap =
    (* Remove separate nodes for variable literal values *)
    G.fold_vertex
      (fun v (g, vmap) ->
        match G.V.label v with
        (* | (ELit _, _), [EVar _, _] -> G.remove_vertex g v *)
        | ELit _, m ->
          ( G.remove_vertex g v,
            (* Forward position of the deleted literal to its parent *)
            List.fold_left
              (fun vmap v ->
                let out =
                  G.succ_e g v
                  |> List.filter (fun e -> not (G.E.label e).condition)
                in
                match out with [_] -> VMap.add v m vmap | _ -> vmap)
              vmap (G.pred g v) )
        | _, _ -> g, vmap)
      g (g, VMap.empty)
  in
  let g =
    map_vertices
      (fun v ->
        match VMap.find_opt v vmap with
        | Some m -> Mark.set m (G.V.label v)
        | None -> G.V.label v)
      g
  in
  let g =
    (* Merge intermediate operations *)
    let g = reverse_graph g in
    GTop.fold (* Variables -> result order *)
      (fun v g ->
        let succ = G.succ g v in
        match G.V.label v, succ, List.map G.V.label succ with
        | (EApp { f = EOp _, _; _ }, _), [v2], [(EApp { f = EOp _, _; _ }, _)]
          ->
          let g =
            List.fold_left
              (fun g e ->
                G.add_edge_e g (G.E.create (G.E.src e) (G.E.label e) v2))
              g (G.pred_e g v)
          in
          G.remove_vertex g v
        | _ -> g)
      g g
    |> reverse_graph
  in
  let g, substs =
    (* Remove intermediate variables *)
    GTop.fold (* Result -> variables order *)
      (fun v (g, substs) ->
        let succ_e = G.succ_e g v in
        if List.exists (fun ed -> (G.E.label ed).condition) succ_e then
          g, substs
        else
          let succ = List.map G.E.dst succ_e in
          match G.V.label v, succ, List.map G.V.label succ with
          | (EVar var1, m1), [v2], [(EVar var2, m2)]
            when not (Var.Set.mem var1 base_vars) ->
            let g =
              List.fold_left
                (fun g e ->
                  G.add_edge_e g (G.E.create (G.E.src e) (G.E.label e) v2))
                g (G.pred_e g v)
            in
            ( G.remove_vertex g v,
              fun e -> subst_by var1 (EVar var2, m2) (substs e) )
          | (EVar var1, m1), [v2], [((EApp _, _) as e2)]
            when not (Var.Set.mem var1 base_vars) -> (
            let pred_e = G.pred_e g v in
            match pred_e, List.map (fun e -> G.V.label (G.E.src e)) pred_e with
            | [pred_e], [(EApp _, _)]
              when G.E.src pred_e |> G.out_degree g <= options.merge_level ->
              (* Arbitrary heuristics: don't merge if the child node already has
                 > level parents *)
              let g =
                G.add_edge_e g
                  (G.E.create (G.E.src pred_e) (G.E.label pred_e) v2)
              in
              G.remove_vertex g v, fun e -> subst_by var1 e2 (substs e)
            | _ -> g, substs)
          | _ -> g, substs)
      g (g, G.V.label)
  in
  let g = map_vertices substs g in
  let g =
    (* Merge intermediate operations (again) *)
    let g = reverse_graph g in
    GTop.fold (* Variables -> result order *)
      (fun v g ->
        let succ = G.succ g v in
        match G.V.label v, succ, List.map G.V.label succ with
        | (EApp { f = EOp _, _; _ }, _), [v2], [(EApp { f = EOp _, _; _ }, _)]
          ->
          let g =
            List.fold_left
              (fun g e ->
                G.add_edge_e g (G.E.create (G.E.src e) (G.E.label e) v2))
              g (G.pred_e g v)
          in
          G.remove_vertex g v
        | _ -> g)
      g g
    |> reverse_graph
  in
  let g =
    let module EMap = Map.Make (struct
      type t = expr

      let compare = Expr.compare
      let format = Expr.format
    end) in
    (* Merge duplicate nodes *)
    let emap =
      G.fold_vertex
        (fun v expr_map ->
          let e = G.V.label v in
          EMap.update e
            (function None -> Some [v] | Some l -> Some (v :: l))
            expr_map)
        g EMap.empty
    in
    EMap.fold
      (fun expr vs g ->
        match vs with
        | [] | [_] -> g
        | v0 :: vn ->
          let e_in =
            List.map (G.pred_e g) vs
            |> List.flatten
            |> List.map (fun e -> G.E.create (G.E.src e) (G.E.label e) v0)
            |> List.sort_uniq G.E.compare
          in
          let e_out =
            List.map (G.succ_e g) vs
            |> List.flatten
            |> List.map (fun e -> G.E.create v0 (G.E.label e) (G.E.dst e))
            |> List.sort_uniq G.E.compare
          in
          let g = List.fold_left G.remove_vertex g vn in
          let g = List.fold_left G.remove_edge_e g (G.succ_e g v0) in
          let g = List.fold_left G.remove_edge_e g (G.pred_e g v0) in
          let g = List.fold_left G.add_edge_e g e_in in
          let g = List.fold_left G.add_edge_e g e_out in
          g)
      emap g
  in
  g

let expr_to_dot_label0 :
    type a.
    Cli.backend_lang ->
    decl_ctx ->
    Env.t ->
    Format.formatter ->
    (a, 't) gexpr ->
    unit =
 fun lang ctx env ->
  let xlang ~en ?(pl = en) ~fr () =
    match lang with Cli.Fr -> fr | Cli.En -> en | Cli.Pl -> pl
  in
  let rec aux_value : type a t. Format.formatter -> (a, t) gexpr -> unit =
   fun ppf e -> Print.UserFacing.value ~fallback lang ppf e
  and fallback : type a t. Format.formatter -> (a, t) gexpr -> unit =
   fun ppf e ->
    let module E = Print.ExprGen (struct
      let var ppf v = String.format ppf (Bindlib.name_of v)
      let lit = Print.UserFacing.lit lang

      let operator : type x. Format.formatter -> x operator -> unit =
       fun ppf o ->
        let open Op in
        let str =
          match o with
          | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur | Eq_dat_dat | Eq
            ->
            "="
          | Minus_int | Minus_rat | Minus_mon | Minus_dur | Minus -> "-"
          | ToRat_int | ToRat_mon | ToRat -> ""
          | ToMoney_rat | ToMoney -> ""
          | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
          | Add_dur_dur | Add ->
            "+"
          | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
          | Sub_dur_dur | Sub ->
            "-"
          | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Mult ->
            "×"
          | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur
          | Div ->
            "/"
          | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dur_dur | Lt_dat_dat | Lt
            ->
            "<"
          | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dur_dur | Lte_dat_dat
          | Lte ->
            "<="
          | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dur_dur | Gt_dat_dat | Gt
            ->
            ">"
          | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dur_dur | Gte_dat_dat
          | Gte ->
            ">="
          | Concat -> "++"
          | Not -> xlang () ~en:"not" ~fr:"non"
          | Length -> xlang () ~en:"length" ~fr:"nombre"
          | GetDay -> xlang () ~en:"day_of_month" ~fr:"jour_du_mois"
          | GetMonth -> xlang () ~en:"month" ~fr:"mois"
          | GetYear -> xlang () ~en:"year" ~fr:"année"
          | FirstDayOfMonth ->
            xlang () ~en:"first_day_of_month" ~fr:"premier_jour_du_mois"
          | LastDayOfMonth ->
            xlang () ~en:"last_day_of_month" ~fr:"dernier_jour_du_mois"
          | Round_rat | Round_mon | Round -> xlang () ~en:"round" ~fr:"arrondi"
          | Log _ -> xlang () ~en:"Log" ~fr:"Journal"
          | And -> xlang () ~en:"and" ~fr:"et"
          | Or -> xlang () ~en:"or" ~fr:"ou"
          | Xor -> xlang () ~en:"xor" ~fr:"ou bien"
          | Map -> xlang () ~en:"on_every" ~fr:"pour_chaque"
          | Reduce -> xlang () ~en:"reduce" ~fr:"réunion"
          | Filter -> xlang () ~en:"filter" ~fr:"filtre"
          | Fold -> xlang () ~en:"fold" ~fr:"pliage"
          | HandleDefault -> ""
          | HandleDefaultOpt -> ""
          | ToClosureEnv -> ""
          | FromClosureEnv -> ""
        in
        Format.pp_print_string ppf str

      let pre_map = Expr.skip_wrappers

      let bypass : type a t. Format.formatter -> (a, t) gexpr -> bool =
       fun ppf e ->
        match Mark.remove e with
        | ELit _ | EArray _ | ETuple _ | EStruct _ | EInj _ | EEmptyError
        | EAbs _ | EExternal _ ->
          aux_value ppf e;
          true
        | EMatch { e; cases; _ } ->
          let cases =
            List.map
              (function
                | cons, (EAbs { binder; _ }, _) ->
                  cons, snd (Bindlib.unmbind binder)
                | cons, e -> cons, e)
              (EnumConstructor.Map.bindings cases)
          in
          if
            List.for_all
              (function _, (ELit (LBool _), _) -> true | _ -> false)
              cases
          then (
            let cases =
              List.filter_map
                (function c, (ELit (LBool true), _) -> Some c | _ -> None)
                cases
            in
            Format.fprintf ppf "%a @<1>%s @[<hov>%a@]" aux_value e "≅"
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () ->
                   Format.fprintf ppf " %t@ " (fun ppf -> operator ppf Or))
                 EnumConstructor.format)
              cases;
            true)
          else false
        | _ -> false
    end) in
    E.expr ppf e
  in
  aux_value

let rec expr_to_dot_label lang ctx env ppf e =
  let print_expr = expr_to_dot_label lang ctx env in
  let e = Expr.skip_wrappers e in
  match e with
  | EVar v, _ ->
    let e, _ = lazy_eval ctx env value_level e in
    Format.fprintf ppf "%a = %a" String.format (Bindlib.name_of v)
      (expr_to_dot_label0 lang ctx env)
      e
  | EStruct { name; fields }, _ ->
    let pr ppf =
      Format.fprintf ppf "{ %a | { { %a } | { %a }}}" StructName.format name
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " | ")
           (fun ppf fld ->
             StructField.format ppf fld;
             Format.pp_print_string ppf "\\l"))
        (StructField.Map.keys fields)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " | ")
           (fun ppf -> function
             | ((EVar _ | ELit _ | EInj { e = (EVar _ | ELit _), _; _ }), _) as
               e ->
               print_expr ppf e;
               Format.pp_print_string ppf "\\l"
             | _ -> Format.pp_print_string ppf "…\\l"))
        (StructField.Map.values fields)
    in
    Format.pp_print_string ppf (Message.unformat pr)
  | EArray elts, _ ->
    let pr ppf =
      Format.fprintf ppf "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " | ")
           (fun ppf -> function
             | ((EVar _ | ELit _), _) as e -> print_expr ppf e
             | _ -> Format.pp_print_string ppf "…"))
        elts
    in
    Format.pp_print_string ppf (Message.unformat pr)
  | e -> Format.fprintf ppf "%a@," (expr_to_dot_label0 lang ctx env) e

let to_dot lang ppf ctx env base_vars g ~base_src_url =
  let module GPr = Graph.Graphviz.Dot (struct
    include G

    let print_expr env ctx lang ppf e =
      let out_funs = Format.pp_get_formatter_out_functions ppf () in
      Format.pp_set_formatter_out_functions ppf
        {
          out_funs with
          Format.out_newline = (fun () -> out_funs.out_string "\\l" 0 2);
        };
      expr_to_dot_label env ctx lang ppf e;
      Format.pp_print_flush ppf ();
      Format.pp_set_formatter_out_functions ppf out_funs

    let graph_attributes _ = [ (* `Rankdir `LeftToRight *) ]
    let default_vertex_attributes _ = []

    let vertex_label v =
      let print_expr = print_expr lang ctx env in
      match G.V.label v with
      | (EVar v, _) as e ->
        Format.asprintf "%a = %a" String.format (Bindlib.name_of v) print_expr
          (fst (lazy_eval ctx env value_level e))
      | e -> Format.asprintf "%a" print_expr e

    let vertex_name v = Printf.sprintf "x%03d" (G.V.hash v)

    let vertex_attributes v =
      let e = V.label v in
      let pos = Expr.pos e in
      let loc_text =
        Re.replace_string
          Re.(compile (char '\n'))
          ~by:"&#10;"
          (String.concat "\n» " (List.rev (Pos.get_law_info pos)) ^ "\n")
      in
      `Label (vertex_label v (* ^ "\n" ^ loc_text *))
      :: `Comment loc_text
         (* :: `Url
          *      ("http://localhost:8080/fr/examples/housing-benefits#"
          *      ^ Re.(
          *          replace_string
          *            (compile
          *               (seq [char '/'; rep1 (diff any (char '/')); str "/../"]))
          *            ~by:"/" (Pos.get_file pos))
          *      ^ "-"
          *      ^ string_of_int (Pos.get_start_line pos)) *)
      :: `Url
           (base_src_url
           ^ "/"
           ^ Pos.get_file pos
           ^ "#L"
           ^ string_of_int (Pos.get_start_line pos))
      :: `Fontname "DejaVu Sans Mono"
      ::
      (match G.V.label v with
      | EVar var, _ ->
        if Var.Set.mem var base_vars then
          [`Style `Filled; `Fillcolor 0xffaa55; `Shape `Box]
        else if
          List.exists (fun e -> not (G.E.label e).condition) (G.succ_e g v)
        then
          (* non-constants *)
          [`Style `Filled; `Fillcolor 0xffee99; `Shape `Box]
        else (* Constants *)
          [`Style `Filled; `Fillcolor 0x77aaff; `Shape `Note]
      | EStruct _, _ | EArray _, _ -> [`Shape `Record]
      | EApp { f = EOp { op; _ }, _; _ }, _ -> (
        match op_kind op with
        | `Sum | `Product | _ -> [`Shape `Box] (* | _ -> [] *))
      | _ -> [])

    let get_subgraph v =
      match G.V.label v with
      | EVar var, _ -> (
        if Var.Set.mem var base_vars then
          Some
            {
              Graph.Graphviz.DotAttributes.sg_name = "inputs";
              sg_attributes = [];
              sg_parent = None;
            }
        else
          match List.map G.V.label (G.succ g v) with
          (* | [] | [ELit _, _] ->
           *   Some
           *     {
           *       Graph.Graphviz.DotAttributes.sg_name = "constants";
           *       sg_attributes = [`Shape `Box];
           *       sg_parent = None;
           *     } *)
          | _ -> None)
      | _ -> None

    let default_edge_attributes _ = []

    let edge_attributes e =
      match E.label e with
      | { condition = true; _ } ->
        [`Style `Dashed; `Penwidth 5.; `Color 0xff7700; `Arrowhead `Odot]
      | { side = Some (Lhs s | Rhs s); _ } ->
        [ (* `Label s; `Color 0xbb7700 *) ]
      | _ -> []
  end) in
  GPr.fprint_graph ppf (reverse_graph g)

(* -- Plugin registration -- *)

let options =
  let open Cmdliner in
  let conditions =
    Arg.(
      value
      & flag
      & info ["conditions"]
          ~doc:
            "Include boolean conditions used to choose the specific formula \
             nodes (with dashed lines) in the resulting graph. Without this, \
             only the nodes contributing to the actual calculation are shown.")
  in
  let no_cleanup =
    Arg.(
      value
      & flag
      & info ["no-cleanup"]
          ~doc:
            "Disable automatic cleanup of intermediate computation nodes. Very \
             verbose but sometimes useful for debugging.")
  in
  let merge_level =
    Arg.(
      value
      & opt int 2
      & info ["merge-level"]
          ~doc:
            "Determines an internal threshold to the heuristics for merging \
             intermediate nodes with as many parents. Higher means more \
             aggressive merges.")
  in
  let format =
    let mkinfo s =
      ( `Convert s,
        Arg.info [s]
          ~doc:
            (Printf.sprintf
               "Outputs a compiled $(b,.%s) file instead of a $(b,.dot) file \
                (requires $(i,graphviz) to be installed)."
               s) )
    in
    Arg.(
      value
      & vflag `Dot
          [
            ( `Dot,
              info ["dot"]
                ~doc:"Output the graph in dot format (this is the default)" );
            mkinfo "svg";
            mkinfo "png";
            mkinfo "pdf";
          ])
  in
  let show =
    Arg.(
      value
      & opt ~vopt:(Some "xdot") (some string) None
      & info ["show"]
          ~doc:"Opens the resulting graph in the given command immediately.")
  in
  let base_src_url =
    Arg.(
      value
      & opt string "https://github.com/CatalaLang/catala/blob/master"
      & info ["url-base"] ~docv:"URL"
          ~doc:
            "Base URL that can be used to browse the Catala code. Nodes will \
             link to $(i,URL)/relative/filename.catala_xx#LNN where NN is the \
             line number in the file")
  in
  let f with_conditions no_cleanup merge_level format show output base_src_url =
    {
      with_conditions;
      with_cleanup = not no_cleanup;
      merge_level;
      format;
      show;
      output;
      base_src_url;
    }
  in
  Term.(
    const f
    $ conditions
    $ no_cleanup
    $ merge_level
    $ format
    $ show
    $ Cli.Flags.output
    $ base_src_url)

let run includes build_dirs optimize ex_scope explain_options global_options =
  let prg, ctx, _ =
    Driver.Passes.dcalc global_options ~includes ~optimize
      ~check_invariants:false
  in
  Interpreter.load_runtime_modules ~build_dirs prg;
  let scope = Driver.Commands.get_scope_uid ctx ex_scope in
  (* let result_expr, env = interpret_program prg scope in *)
  let g, base_vars, env = program_to_graph explain_options prg scope in
  log "Base variables detected: @[<hov>%a@]"
    (Format.pp_print_list Print.var)
    (Var.Set.elements base_vars);
  let g =
    if explain_options.with_cleanup then
      graph_cleanup explain_options g base_vars
    else g
  in
  let lang = Driver.get_lang global_options global_options.Cli.input_file in
  let dot_content =
    to_dot lang Format.str_formatter prg.decl_ctx env base_vars g
      ~base_src_url:explain_options.base_src_url;
    Format.flush_str_formatter ()
    |> Re.(replace_string (compile (seq [bow; str "comment="])) ~by:"tooltip=")
  in
  let with_dot_file =
    match explain_options with
    | { format = `Convert _; _ } | { show = Some _; output = None; _ } ->
      File.with_temp_file "catala-explain" "dot" ~contents:dot_content
    | { output; _ } ->
      let _, with_out = Driver.Commands.get_output global_options output in
      with_out (fun oc -> output_string oc dot_content);
      fun f -> f (Option.value ~default:"-" output)
  in
  with_dot_file
  @@ fun dotfile ->
  (match explain_options.format with
  | `Convert fmt ->
    let _, with_out =
      Driver.Commands.get_output global_options explain_options.output
    in
    with_out (fun oc ->
        output_string oc (File.process_out "dot" ["-T" ^ fmt; dotfile]))
  | `Dot -> ());
  match explain_options.show with
  | None -> ()
  | Some cmd ->
    raise (Cli.Exit_with (Sys.command (cmd ^ " " ^ Filename.quote dotfile)))

let term =
  let open Cmdliner.Term in
  const run
  $ Driver.Commands.include_flags
  $ Cli.Flags.build_dirs
  $ Cli.Flags.optimize
  $ Cli.Flags.ex_scope
  $ options

let () =
  Driver.Plugin.register "explain" term
    ~doc:
      "Generates a graph of the formulas that are used for a given execution \
       of a scope"
    ~man:
      [
        `P
          "This command requires a given scope with no inputs (i.e. a test \
           scope). A partial/lazy evaluation will recursively take place to \
           explain intermediate formulas that take place in the computation, \
           from the inputs (specified in the test scope) to the final outputs. \
           The output is a graph, in .dot format (graphviz) by default (see \
           $(b,--svg) and $(b,--show) for other options)";
      ]
