(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

type location = scopelang glocation

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare = Expr.compare_location
end)

type 'm expr = (scopelang, 'm mark) gexpr

let rec locations_used (e : 'm expr) : LocationSet.t =
  match e with
  | ELocation l, pos -> LocationSet.singleton (l, Expr.mark_pos pos)
  | EAbs { binder; _ }, _ ->
    let _, body = Bindlib.unmbind binder in
    locations_used body
  | e ->
    Expr.shallow_fold
      (fun e -> LocationSet.union (locations_used e))
      e LocationSet.empty

type 'm rule =
  | Definition of location Marked.pos * typ * Desugared.Ast.io * 'm expr
  | Assertion of 'm expr
  | Call of ScopeName.t * SubScopeName.t * 'm mark

type 'm scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ * Desugared.Ast.io) ScopeVar.Map.t;
  scope_decl_rules : 'm rule list;
  scope_mark : 'm mark;
}

type 'm program = {
  program_scopes : 'm scope_decl ScopeName.Map.t;
  program_globals : ('m expr * typ) TopdefName.Map.t;
  program_ctx : decl_ctx;
}

let type_rule decl_ctx env = function
  | Definition (loc, typ, io, expr) ->
    let expr' = Typing.expr decl_ctx ~env ~typ expr in
    Definition (loc, typ, io, Expr.unbox expr')
  | Assertion expr ->
    let typ = Marked.mark (Expr.pos expr) (TLit TBool) in
    let expr' = Typing.expr decl_ctx ~env ~typ expr in
    Assertion (Expr.unbox expr')
  | Call (sc_name, ssc_name, m) ->
    let pos = Expr.mark_pos m in
    Call (sc_name, ssc_name, Typed { pos; ty = Marked.mark pos TAny })

let type_program (prg : 'm program) : typed program =
  let typing_env =
    TopdefName.Map.fold
      (fun name (_, ty) -> Typing.Env.add_global_var name ty)
      prg.program_globals Typing.Env.empty
  in
  let program_globals =
    TopdefName.Map.map
      (fun (expr, typ) ->
        Expr.unbox (Typing.expr prg.program_ctx ~env:typing_env ~typ expr), typ)
      prg.program_globals
  in
  let typing_env =
    ScopeName.Map.fold
      (fun scope_name scope_decl ->
        let vars = ScopeVar.Map.map fst scope_decl.scope_sig in
        Typing.Env.add_scope scope_name ~vars)
      prg.program_scopes typing_env
  in
  let program_scopes =
    ScopeName.Map.map
      (fun scope_decl ->
        let typing_env =
          ScopeVar.Map.fold
            (fun svar (typ, _) env -> Typing.Env.add_scope_var svar typ env)
            scope_decl.scope_sig typing_env
        in
        let scope_decl_rules =
          List.map
            (type_rule prg.program_ctx typing_env)
            scope_decl.scope_decl_rules
        in
        let scope_mark =
          let pos =
            Marked.get_mark (ScopeName.get_info scope_decl.scope_decl_name)
          in
          Typed { pos; ty = Marked.mark pos TAny }
        in
        { scope_decl with scope_decl_rules; scope_mark })
      prg.program_scopes
  in
  { prg with program_globals; program_scopes }
