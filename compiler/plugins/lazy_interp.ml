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

(* -- Definition of the lazy interpreter -- *)

let log fmt = Format.ifprintf Format.err_formatter (fmt ^^ "@\n")
let error e = Message.raise_spanned_error (Expr.pos e)
let noassert = true

type laziness_level = {
  eval_struct : bool;
      (* if true, evaluate members of structures, tuples, etc. *)
  eval_op : bool;
      (* if false, evaluate the operands but keep e.g. `3 + 4` as is *)
  eval_default : bool;
      (* if false, stop evaluating as soon as you can discriminate with
         `EEmptyError` *)
}

let value_level = { eval_struct = false; eval_op = true; eval_default = true }

module Env = struct
  type 'm t =
    | Env of ((dcalc, 'm) gexpr, ((dcalc, 'm) gexpr * 'm t) ref) Var.Map.t

  let find v (Env t) = Var.Map.find v t
  let add v e e_env (Env t) = Env (Var.Map.add v (ref (e, e_env)) t)
  let empty = Env Var.Map.empty

  let join (Env t1) (Env t2) =
    Env
      (Var.Map.union
         (fun _ x1 x2 ->
           assert (x1 == x2);
           Some x1)
         t1 t2)

  let print ppf (Env t) =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf v -> Print.var_debug ppf v)
      ppf (Var.Map.keys t)
end

let rec lazy_eval :
    decl_ctx ->
    'm Env.t ->
    laziness_level ->
    (dcalc, 'm) gexpr ->
    (dcalc, 'm) gexpr * 'm Env.t =
 fun ctx env llevel e0 ->
  let eval_to_value ?(eval_default = true) env e =
    lazy_eval ctx env { value_level with eval_default } e
  in
  match e0 with
  | EVar v, _ ->
    if not llevel.eval_default then e0, env
    else
      (* Variables reducing to EEmpty should not propagate to parent EDefault
         (?) *)
      let v_env =
        try Env.find v env
        with Not_found ->
          error e0 "Variable %a undefined [@[<hv>%a@]]" Print.var_debug v
            Env.print env
      in
      let e, env1 = !v_env in
      let r, env1 = lazy_eval ctx env1 llevel e in
      if not (Expr.equal e r) then (
        log "@[<hv 2>{{%a =@ [%a]@ ==> [%a]}}@]" Print.var_debug v
          (Print.expr ~debug:true ())
          e
          (Print.expr ~debug:true ())
          r;
        v_env := r, env1);
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
              log "@[<hov 2>LET %a = %a@]@ " Print.var_debug var
                (Print.expr ~debug:true ())
                e;
              Env.add var e env env1)
            env (Array.to_seq vars) (List.to_seq args)
        in
        log "@]@[<hov 4>IN [%a]@]" (Print.expr ~debug:true ()) body;
        let e, env = lazy_eval ctx env llevel body in
        log "@]}";
        e, env
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
          (* Dirty workaround returning env from evaluate_operator *)
          let eval e =
            let e, env = lazy_eval ctx !renv llevel e in
            renv := env;
            e
          in
          Interpreter.evaluate_operator eval op m args, !renv
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
        lazy_eval ctx env llevel (StructField.Map.find field fields)
      | e, _ -> error e "Invalid field access on %a" Expr.format e)
  | ETupleAccess { e; index; size }, _ -> (
    if not llevel.eval_default then e0, env
    else
      match eval_to_value env e with
      | (ETuple es, _), env when List.length es = size ->
        lazy_eval ctx env llevel (List.nth es index)
      | e, _ -> error e "Invalid tuple access on %a" Expr.format e)
  | EMatch { e; name; cases }, _ -> (
    if not llevel.eval_default then e0, env
    else
      match eval_to_value env e with
      | (EInj { name = n; cons; e }, m), env when EnumName.equal name n ->
        lazy_eval ctx env llevel
          (EApp { f = EnumConstructor.Map.find cons cases; args = [e] }, m)
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
      | (ELit (LBool true), _), _ -> lazy_eval ctx env llevel cons
      | (ELit (LBool false), _), _ -> (EEmptyError, m), env
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
    | (ELit (LBool true), _), _ -> lazy_eval ctx env llevel etrue
    | (ELit (LBool false), _), _ -> lazy_eval ctx env llevel efalse
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
        error e "Assert failure (%a)" Expr.format e
      | _ -> error e "Invalid assertion condition %a" Expr.format e)
  | EExternal _, _ -> assert false (* todo *)
  | _ -> .

let interpret_program (prg : ('dcalc, 'm) gexpr program) (scope : ScopeName.t) :
    ('t, 'm) gexpr * 'm Env.t =
  let ctx = prg.decl_ctx in
  let all_env, scopes =
    Scope.fold_left prg.code_items ~init:(Env.empty, ScopeName.Map.empty)
      ~f:(fun (env, scopes) item v ->
        match item with
        | ScopeDef (name, body) ->
          let e = Scope.to_expr ctx body (Scope.get_body_mark body) in
          ( Env.add v (Expr.unbox e) env env,
            ScopeName.Map.add name (v, body.scope_body_input_struct) scopes )
        | Topdef (_, _, e) -> Env.add v e env env, scopes)
  in
  let scope_v, scope_arg_struct = ScopeName.Map.find scope scopes in
  let { contents = e, env } = Env.find scope_v all_env in
  let e = Expr.unbox (Expr.remove_logging_calls e) in
  log "=====================";
  log "%a" (Print.expr ~debug:true ()) e;
  log "=====================";
  let m = Mark.get e in
  let application_arg =
    Expr.estruct ~name:scope_arg_struct
      ~fields:(StructField.Map.map
         (function
           | TArrow (ty_in, ty_out), _ ->
             Expr.make_abs
               [| Var.make "_" |]
               (Bindlib.box EEmptyError, Expr.with_ty m ty_out)
               ty_in (Expr.mark_pos m)
           | ty -> Expr.evar (Var.make "undefined_input") (Expr.with_ty m ty))
         (snd (StructName.Map.find scope_arg_struct ctx.ctx_structs)))
      m
  in
  let e_app = Expr.eapp (Expr.box e) [application_arg] m in
  lazy_eval ctx env
    { value_level with eval_struct = true; eval_op = false }
    (Expr.unbox e_app)

(* -- Plugin registration -- *)

let run link_modules optimize check_invariants ex_scope options =
  Interpreter.load_runtime_modules link_modules;
  let prg, ctx, _ =
    Driver.Passes.dcalc options ~link_modules ~optimize ~check_invariants
  in
  let scope = Driver.Commands.get_scope_uid ctx ex_scope in
  let result_expr, _env = interpret_program prg scope in
  let fmt = Format.std_formatter in
  Expr.format fmt result_expr

let term =
  let open Cmdliner.Term in
  const run
  $ Cli.Flags.link_modules
  $ Cli.Flags.optimize
  $ Cli.Flags.check_invariants
  $ Cli.Flags.ex_scope

let () =
  Driver.Plugin.register "lazy" term
    ~doc:"Experimental lazy evaluation (plugin)"
