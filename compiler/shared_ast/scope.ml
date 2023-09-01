(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

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
open Definitions

let rec fold_left_lets ~f ~init scope_body_expr =
  match scope_body_expr with
  | Result _ -> init
  | ScopeLet scope_let ->
    let var, next = Bindlib.unbind scope_let.scope_let_next in
    fold_left_lets ~f ~init:(f init scope_let var) next

let rec fold_right_lets ~f ~init scope_body_expr =
  match scope_body_expr with
  | Result result -> init result
  | ScopeLet scope_let ->
    let var, next = Bindlib.unbind scope_let.scope_let_next in
    let next_result = fold_right_lets ~f ~init next in
    f scope_let var next_result

let map_exprs_in_lets :
    ?reset_types:bool ->
    f:('expr1 -> 'expr2 boxed) ->
    varf:('expr1 Var.t -> 'expr2 Var.t) ->
    'expr1 scope_body_expr ->
    'expr2 scope_body_expr Bindlib.box =
 fun ?(reset_types = false) ~f ~varf scope_body_expr ->
  fold_right_lets
    ~f:(fun scope_let var_next acc ->
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet
            {
              scope_let with
              scope_let_next;
              scope_let_expr;
              scope_let_typ =
                (if reset_types then Mark.copy scope_let.scope_let_typ TAny
                 else scope_let.scope_let_typ);
            })
        (Bindlib.bind_var (varf var_next) acc)
        (Expr.Box.lift (f scope_let.scope_let_expr)))
    ~init:(fun res ->
      Bindlib.box_apply (fun res -> Result res) (Expr.Box.lift (f res)))
    scope_body_expr

let rec fold_left ~f ~init = function
  | Nil -> init
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    fold_left ~f ~init:(f init item var) next

let rec fold_right ~f ~init = function
  | Nil -> init
  | Cons (item, next_bind) ->
    let var_next, next = Bindlib.unbind next_bind in
    let result_next = fold_right ~f ~init next in
    f item var_next result_next

let rec map ~f ~varf = function
  | Nil -> Bindlib.box Nil
  | Cons (item, next_bind) ->
    let item = f item in
    let next_bind =
      let var, next = Bindlib.unbind next_bind in
      Bindlib.bind_var (varf var) (map ~f ~varf next)
    in
    Bindlib.box_apply2
      (fun item next_bind -> Cons (item, next_bind))
      item next_bind

let rec map_ctx ~f ~varf ctx = function
  | Nil -> Bindlib.box Nil
  | Cons (item, next_bind) ->
    let ctx, item = f ctx item in
    let next_bind =
      let var, next = Bindlib.unbind next_bind in
      Bindlib.bind_var (varf var) (map_ctx ~f ~varf ctx next)
    in
    Bindlib.box_apply2
      (fun item next_bind -> Cons (item, next_bind))
      item next_bind

let rec fold_map ~f ~varf ctx = function
  | Nil -> ctx, Bindlib.box Nil
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let ctx, item = f ctx var item in
    let ctx, next = fold_map ~f ~varf ctx next in
    let next_bind = Bindlib.bind_var (varf var) next in
    ( ctx,
      Bindlib.box_apply2
        (fun item next_bind -> Cons (item, next_bind))
        item next_bind )

let map_exprs ~f ~varf scopes =
  let f = function
    | ScopeDef (name, body) ->
      let scope_input_var, scope_lets = Bindlib.unbind body.scope_body_expr in
      let new_body_expr = map_exprs_in_lets ~f ~varf scope_lets in
      let new_body_expr =
        Bindlib.bind_var (varf scope_input_var) new_body_expr
      in
      Bindlib.box_apply
        (fun scope_body_expr -> ScopeDef (name, { body with scope_body_expr }))
        new_body_expr
    | Topdef (name, typ, expr) ->
      Bindlib.box_apply
        (fun e -> Topdef (name, typ, e))
        (Expr.Box.lift (f expr))
  in
  map ~f ~varf scopes

(* TODO: compute the expected body expr arrow type manually instead of [TAny]
   for double-checking types ? *)
let rec get_body_expr_mark = function
  | ScopeLet sl ->
    let _, e = Bindlib.unbind sl.scope_let_next in
    get_body_expr_mark e
  | Result e ->
    let m = Mark.get e in
    Expr.with_ty m (Mark.add (Expr.mark_pos m) TAny)

let get_body_mark scope_body =
  let _, e = Bindlib.unbind scope_body.scope_body_expr in
  get_body_expr_mark e

let rec unfold_body_expr (ctx : decl_ctx) (scope_let : 'e scope_body_expr) =
  match scope_let with
  | Result e -> Expr.rebox e
  | ScopeLet
      {
        scope_let_kind = _;
        scope_let_typ;
        scope_let_expr;
        scope_let_next;
        scope_let_pos;
      } ->
    let var, next = Bindlib.unbind scope_let_next in
    Expr.make_let_in var scope_let_typ
      (Expr.rebox scope_let_expr)
      (unfold_body_expr ctx next)
      scope_let_pos

let build_typ_from_sig
    (_ctx : decl_ctx)
    (scope_input_struct_name : StructName.t)
    (scope_return_struct_name : StructName.t)
    (pos : Pos.t) : typ =
  let input_typ = Mark.add pos (TStruct scope_input_struct_name) in
  let result_typ = Mark.add pos (TStruct scope_return_struct_name) in
  Mark.add pos (TArrow ([input_typ], result_typ))

type 'e scope_name_or_var = ScopeName of ScopeName.t | ScopeVar of 'e Var.t

let to_expr (ctx : decl_ctx) (body : 'e scope_body) (mark_scope : 'm) : 'e boxed
    =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_body_expr ctx body_expr in
  Expr.make_abs [| var |] body_expr
    [TStruct body.scope_body_input_struct, Expr.mark_pos mark_scope]
    (Expr.mark_pos mark_scope)

let rec unfold
    (ctx : decl_ctx)
    (s : 'e code_item_list)
    (mark : 'm mark)
    (main_scope : 'expr scope_name_or_var) : 'e boxed =
  match s with
  | Nil -> (
    match main_scope with
    | ScopeVar v -> Expr.make_var v mark
    | ScopeName _ -> failwith "should not happen")
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let typ, expr, pos, is_main =
      match item with
      | ScopeDef (name, body) ->
        let pos = Mark.get (ScopeName.get_info name) in
        let body_mark = get_body_mark body in
        let is_main =
          match main_scope with
          | ScopeName n -> ScopeName.equal n name
          | ScopeVar _ -> false
        in
        let typ =
          build_typ_from_sig ctx body.scope_body_input_struct
            body.scope_body_output_struct pos
        in
        let expr = to_expr ctx body body_mark in
        typ, expr, pos, is_main
      | Topdef (name, typ, expr) ->
        let pos = Mark.get (TopdefName.get_info name) in
        typ, Expr.rebox expr, pos, false
    in
    let main_scope = if is_main then ScopeVar var else main_scope in
    let next = unfold ctx next mark main_scope in
    Expr.make_let_in var typ expr next pos

let rec free_vars_body_expr scope_lets =
  match scope_lets with
  | Result e -> Expr.free_vars e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
    let v, body = Bindlib.unbind next in
    Var.Set.union (Expr.free_vars e)
      (Var.Set.remove v (free_vars_body_expr body))

let free_vars_item = function
  | ScopeDef (_, { scope_body_expr; _ }) ->
    let v, body = Bindlib.unbind scope_body_expr in
    Var.Set.remove v (free_vars_body_expr body)
  | Topdef (_, _, expr) -> Expr.free_vars expr

let rec free_vars scopes =
  match scopes with
  | Nil -> Var.Set.empty
  | Cons (item, next_bind) ->
    let v, next = Bindlib.unbind next_bind in
    Var.Set.union (Var.Set.remove v (free_vars next)) (free_vars_item item)
