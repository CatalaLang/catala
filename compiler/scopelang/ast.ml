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

module LocationSet : Set.S with type elt = location Mark.pos = Set.Make (struct
  type t = location Mark.pos

  let compare = Expr.compare_location
end)

type 'm expr = (scopelang, 'm) gexpr

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
  | Definition of location Mark.pos * typ * Desugared.Ast.io * 'm expr
  | Assertion of 'm expr
  | Call of ScopeName.t * SubScopeName.t * 'm mark

type scope_var_ty = {
  svar_in_ty : typ;
  svar_out_ty : typ;
  svar_io : Desugared.Ast.io;
}

type 'm scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : scope_var_ty ScopeVar.Map.t;
  scope_decl_rules : 'm rule list;
  scope_options : Desugared.Ast.catala_option Mark.pos list;
}

type 'm program = {
  program_module_name : ModuleName.t option;
  program_ctx : decl_ctx;
  program_modules : nil scope_decl Mark.pos ScopeName.Map.t ModuleName.Map.t;
  program_scopes : 'm scope_decl Mark.pos ScopeName.Map.t;
  program_topdefs : ('m expr * typ) TopdefName.Map.t;
  program_lang : Cli.backend_lang;
}

let type_rule decl_ctx env = function
  | Definition (loc, typ, io, expr) ->
    let expr' = Typing.expr ~leave_unresolved:false decl_ctx ~env ~typ expr in
    Definition (loc, typ, io, Expr.unbox expr')
  | Assertion expr ->
    let typ = Mark.add (Expr.pos expr) (TLit TBool) in
    let expr' = Typing.expr ~leave_unresolved:false decl_ctx ~env ~typ expr in
    Assertion (Expr.unbox expr')
  | Call (sc_name, ssc_name, m) ->
    let pos = Expr.mark_pos m in
    Call (sc_name, ssc_name, Typed { pos; ty = Mark.add pos TAny })

let type_program (type m) (prg : m program) : typed program =
  (* Caution: this environment building code is very similar to that in
     desugared/disambiguate.ml. Any edits should probably be reflected. *)
  let env = Typing.Env.empty prg.program_ctx in
  let env =
    TopdefName.Map.fold
      (fun name ty env -> Typing.Env.add_toplevel_var name ty env)
      prg.program_ctx.ctx_topdefs env
  in
  let env =
    ScopeName.Map.fold
      (fun scope_name _info env ->
        let scope_sig =
          match ScopeName.path scope_name with
          | [] ->
            (Mark.remove (ScopeName.Map.find scope_name prg.program_scopes))
              .scope_sig
          | p ->
            let m = List.hd (List.rev p) in
            let scope =
              ScopeName.Map.find scope_name
                (ModuleName.Map.find m prg.program_modules)
            in
            (Mark.remove scope).scope_sig
        in
        let vars =
          ScopeVar.Map.map (fun { svar_out_ty; _ } -> svar_out_ty) scope_sig
        in
        let in_vars =
          ScopeVar.Map.map (fun { svar_in_ty; _ } -> svar_in_ty) scope_sig
        in
        Typing.Env.add_scope scope_name ~vars ~in_vars env)
      prg.program_ctx.ctx_scopes env
  in
  let program_topdefs =
    TopdefName.Map.map
      (fun (expr, typ) ->
        ( Expr.unbox
            (Typing.expr prg.program_ctx ~leave_unresolved:false ~env ~typ expr),
          typ ))
      prg.program_topdefs
  in
  let program_scopes =
    ScopeName.Map.map
      (Mark.map (fun scope_decl ->
           let env =
             ScopeVar.Map.fold
               (fun svar { svar_out_ty; _ } env ->
                 Typing.Env.add_scope_var svar svar_out_ty env)
               scope_decl.scope_sig env
           in
           let scope_decl_rules =
             List.map
               (type_rule prg.program_ctx env)
               scope_decl.scope_decl_rules
           in
           { scope_decl with scope_decl_rules }))
      prg.program_scopes
  in
  { prg with program_topdefs; program_scopes }
