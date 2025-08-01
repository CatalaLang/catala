(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

let expr ctx env e =
  (* The typer takes care of disambiguating: this consists in: - ensuring
     [EAbs.tys] doesn't contain any [TForAll] - [EDStructAccess.name_opt] is
     always [Some] *)
  (* Intermediate unboxings are fine since the [check_expr] will rebox in
     depth *)
  Typing.check_expr ctx ~env (Expr.unbox e)

let rule ctx env rule =
  let env =
    match rule.rule_parameter with
    | None -> env
    | Some (vars_and_types, _) ->
      ListLabels.fold_right vars_and_types ~init:env ~f:(fun ((v, _), t) ->
          Typing.Env.add_var v t)
  in
  (* Note: we could use the known rule type here to direct typing. We choose not
     to because it shouldn't be needed for disambiguation, and we prefer to
     focus on local type errors first. *)
  {
    rule with
    rule_just = expr ctx env rule.rule_just;
    rule_cons = expr ctx env rule.rule_cons;
  }

let scope ctx env scope =
  let env = Typing.Env.open_scope scope.scope_uid env in
  let scope_defs =
    ScopeDef.Map.map
      (fun def ->
        let scope_def_rules =
          (* Note: ordering in file order might be better for error reporting ?
             When we gather errors, the ordering could be done afterwards,
             though *)
          RuleName.Map.map (rule ctx env) def.scope_def_rules
        in
        { def with scope_def_rules })
      scope.scope_defs
  in
  let scope_assertions =
    AssertionName.Map.map (expr ctx env) scope.scope_assertions
  in
  { scope with scope_defs; scope_assertions }

let program prg =
  (* Caution: this environment building code is very similar to that in
     scopelang/ast.ml. Any edits should probably be reflected. *)
  let env = Typing.Env.empty prg.program_ctx in
  let env =
    TopdefName.Map.fold
      (fun name (ty, _vis) env -> Typing.Env.add_toplevel_var name ty env)
      prg.program_ctx.ctx_topdefs env
  in
  let env =
    ScopeName.Map.fold
      (fun scope_name _info env ->
        let modul =
          List.fold_left
            (fun _ m -> ModuleName.Map.find m prg.program_modules)
            prg.program_root
            (ScopeName.path scope_name)
        in
        let scope = ScopeName.Map.find scope_name modul.module_scopes in
        let vars =
          ScopeDef.Map.fold
            (fun (v, kind) def vars ->
              match kind with
              | ScopeDef.Var _ ->
                ScopeVar.Map.add (Mark.remove v) def.scope_def_typ vars
              | ScopeDef.SubScopeInput _ -> vars)
            scope.scope_defs ScopeVar.Map.empty
        in
        (* at this stage, rule resolution and the corresponding encapsulation
           into default terms hasn't taken place, so input and output variables
           don't need different typing *)
        Typing.Env.add_scope scope_name ~vars ~in_vars:vars env)
      prg.program_ctx.ctx_scopes env
  in
  let topdef modul =
    TopdefName.Map.map
      (fun def ->
        {
          def with
          topdef_expr =
            Option.map
              (fun e -> Expr.unbox (expr prg.program_ctx env (Expr.box e)))
              def.topdef_expr;
        })
      modul.module_topdefs
  in
  let module_topdefs = topdef prg.program_root in
  let prg =
    if Global.options.whole_program then
      (* Also disambiguate modules' topdefs *)
      let program_modules =
        ModuleName.Map.map
          (fun modul -> { modul with module_topdefs = topdef modul })
          prg.program_modules
      in
      { prg with program_modules }
    else prg
  in
  let module_scopes =
    ScopeName.Map.map (scope prg.program_ctx env) prg.program_root.module_scopes
  in
  let program_modules =
    ModuleName.Map.map
      (fun modul ->
        let module_scopes =
          ScopeName.Map.map (scope prg.program_ctx env) modul.module_scopes
        in
        { modul with module_scopes })
      prg.program_modules
  in
  { prg with program_root = { module_topdefs; module_scopes }; program_modules }

let program prg = Message.with_delayed_errors (fun () -> program prg)
