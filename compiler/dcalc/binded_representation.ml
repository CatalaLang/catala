(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020-2022 Inria, contributor: Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module D = Ast

type scope_lets =
  | Result of D.expr Pos.marked
  | ScopeLet of {
      scope_let_kind : D.scope_let_kind;
      scope_let_typ : D.typ Pos.marked;
      scope_let_expr : D.expr Pos.marked;
      scope_let_next : (D.expr, scope_lets) Bindlib.binder;
      scope_let_pos : Pos.t;
    }

type scope_body = {
  scope_body_input_struct : D.StructName.t;
  scope_body_output_struct : D.StructName.t;
  scope_body_result : (D.expr, scope_lets) Bindlib.binder;
}

type scopes =
  | Nil
  | ScopeDef of {
      scope_name : D.ScopeName.t;
      scope_body : scope_body;
      scope_next : (D.expr, scopes) Bindlib.binder;
    }

let union: unit D.VarMap.t -> unit D.VarMap.t -> unit D.VarMap.t = D.VarMap.union (fun _ _ _ -> Some ())

let rec fv_scope_lets (scope_lets: scope_lets) : unit D.VarMap.t =
  match scope_lets with
  | Result e -> D.fv e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
      let v, body = Bindlib.unbind next in
      union (D.fv e) (D.VarMap.remove v (fv_scope_lets body))

let fv_scope_body (scope_body: scope_body) : unit D.VarMap.t =
  let { scope_body_result = binder; _ } = scope_body in
  let v, body = Bindlib.unbind binder in
  D.VarMap.remove v (fv_scope_lets body)

let rec fv_scopes (scopes: scopes) : unit D.VarMap.t =
  match scopes with
  | Nil -> D.VarMap.empty
  | ScopeDef { scope_body = body; scope_next = next; _ } ->
      let v, next = Bindlib.unbind next in

      union (D.VarMap.remove v (fv_scopes next)) (fv_scope_body body)

let free_vars_scope_lets (scope_lets: scope_lets) : D.Var.t list = fv_scope_lets scope_lets |> D.VarMap.bindings |> List.map fst

let free_vars_scope_body (scope_body: scope_body) : D.Var.t list = fv_scope_body scope_body |> D.VarMap.bindings |> List.map fst

let free_vars_scopes (scopes: scopes): D.Var.t list = fv_scopes scopes |> D.VarMap.bindings |> List.map fst

(** Actual transformation for scopes. *)
let bind_scope_lets (acc : scope_lets Bindlib.box) (scope_let : D.scope_let) :
    scope_lets Bindlib.box =
  let pos = snd scope_let.D.scope_let_var in

  (* Cli.debug_print @@ Format.asprintf "binding let %a. Variable occurs = %b" Print.format_var (fst
     scope_let.D.scope_let_var) (Bindlib.occur (fst scope_let.D.scope_let_var) acc); *)
  let binder = Bindlib.bind_var (fst scope_let.D.scope_let_var) acc in
  Bindlib.box_apply2
    (fun expr binder ->
      (* Cli.debug_print @@ Format.asprintf "free variables in expression: %a" (Format.pp_print_list
         Print.format_var) (D.free_vars expr); *)
      ScopeLet
        {
          scope_let_kind = scope_let.D.scope_let_kind;
          scope_let_typ = scope_let.D.scope_let_typ;
          scope_let_expr = expr;
          scope_let_next = binder;
          scope_let_pos = pos;
        })
    scope_let.D.scope_let_expr binder

let bind_scope_body (body : D.scope_body) : scope_body Bindlib.box =
  (* it is a fold_right and not a fold_left. *)
  let body_result =
    ListLabels.fold_right body.D.scope_body_lets
      ~init:(Bindlib.box_apply (fun e -> Result e) body.D.scope_body_result)
      ~f:(Fun.flip bind_scope_lets)
  in

  (* Cli.debug_print @@ Format.asprintf "binding arg %a" Print.format_var body.D.scope_body_arg; *)
  let scope_body_result = Bindlib.bind_var body.D.scope_body_arg body_result in

  (* Cli.debug_print @@ Format.asprintf "isfinal term is closed: %b" (Bindlib.is_closed
     scope_body_result); *)
  Bindlib.box_apply
    (fun scope_body_result ->
      (* Cli.debug_print @@ Format.asprintf "rank of the final term: %i" (Bindlib.binder_rank
         scope_body_result); *)
      {
        scope_body_output_struct = body.D.scope_body_output_struct;
        scope_body_input_struct = body.D.scope_body_input_struct;
        scope_body_result;
      })
    scope_body_result

let bind_scope
    ((scope_name, scope_var, scope_body) : D.ScopeName.t * D.expr Bindlib.var * D.scope_body)
    (acc : scopes Bindlib.box) : scopes Bindlib.box =
  Bindlib.box_apply2
    (fun scope_body scope_next -> ScopeDef { scope_name; scope_body; scope_next })
    (bind_scope_body scope_body) (Bindlib.bind_var scope_var acc)

let bind_scopes (scopes : (D.ScopeName.t * D.expr Bindlib.var * D.scope_body) list) :
    scopes Bindlib.box =
  let result = ListLabels.fold_right scopes ~init:(Bindlib.box Nil) ~f:bind_scope in
  (* Cli.debug_print @@ Format.asprintf "free variable in the program : [%a]" (Format.pp_print_list
     Print.format_var) (free_vars_scopes (Bindlib.unbox result)); *)
  result
