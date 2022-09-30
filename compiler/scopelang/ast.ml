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

open Utils
open Shared_ast
module StructFieldMapLift = Bindlib.Lift (StructFieldMap)
module EnumConstructorMapLift = Bindlib.Lift (EnumConstructorMap)

type location = scopelang glocation

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare = Expr.compare_location
end)

type 'm expr = (scopelang, 'm mark) gexpr

let rec locations_used (e : 'm expr) : LocationSet.t =
  match Marked.unmark e with
  | ELocation l -> LocationSet.singleton (l, Expr.pos e)
  | EVar _ | ELit _ | EOp _ -> LocationSet.empty
  | EAbs (binder, _) ->
    let _, body = Bindlib.unmbind binder in
    locations_used body
  | EStruct (_, es) ->
    StructFieldMap.fold
      (fun _ e' acc -> LocationSet.union acc (locations_used e'))
      es LocationSet.empty
  | EStructAccess (e1, _, _) -> locations_used e1
  | EEnumInj (e1, _, _) -> locations_used e1
  | EMatchS (e1, _, es) ->
    EnumConstructorMap.fold
      (fun _ e' acc -> LocationSet.union acc (locations_used e'))
      es (locations_used e1)
  | EApp (e1, args) ->
    List.fold_left
      (fun acc arg -> LocationSet.union (locations_used arg) acc)
      (locations_used e1) args
  | EIfThenElse (e1, e2, e3) ->
    LocationSet.union (locations_used e1)
      (LocationSet.union (locations_used e2) (locations_used e3))
  | EDefault (excepts, just, cons) ->
    List.fold_left
      (fun acc except -> LocationSet.union (locations_used except) acc)
      (LocationSet.union (locations_used just) (locations_used cons))
      excepts
  | EArray es ->
    List.fold_left
      (fun acc e' -> LocationSet.union acc (locations_used e'))
      LocationSet.empty es
  | ErrorOnEmpty e' -> locations_used e'

type io_input = NoInput | OnlyInput | Reentrant
type io = { io_output : bool Marked.pos; io_input : io_input Marked.pos }

type 'm rule =
  | Definition of location Marked.pos * typ * io * 'm expr
  | Assertion of 'm expr
  | Call of ScopeName.t * SubScopeName.t * 'm mark

type 'm scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ * io) ScopeVarMap.t;
  scope_decl_rules : 'm rule list;
  scope_mark : 'm mark;
}

type 'm program = {
  program_scopes : 'm scope_decl ScopeMap.t;
  program_ctx : decl_ctx;
}

let type_rule decl_ctx env = function
  | Definition (loc, typ, io, expr) ->
    let expr' = Typing.expr decl_ctx ~env ~typ expr in
    Definition (loc, typ, io, Bindlib.unbox expr')
  | Assertion expr ->
    let typ = Marked.mark (Expr.pos expr) (TLit TBool) in
    let expr' = Typing.expr decl_ctx ~env ~typ expr in
    Assertion (Bindlib.unbox expr')
  | Call (sc_name, ssc_name, m) ->
    let pos = Expr.mark_pos m in
    Call (sc_name, ssc_name, Typed { pos; ty = Marked.mark pos TAny })

let type_program (prg : 'm program) : typed program =
  let typing_env =
    ScopeMap.fold
      (fun scope_name scope_decl ->
        Typing.Env.add_scope scope_name
          (ScopeVarMap.map fst scope_decl.scope_sig))
      prg.program_scopes Typing.Env.empty
  in
  let program_scopes =
    ScopeMap.map
      (fun scope_decl ->
        let typing_env =
          ScopeVarMap.fold
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
  { prg with program_scopes }
