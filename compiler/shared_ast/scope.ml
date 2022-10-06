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

open Utils
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
    f:('expr1 -> 'expr2 boxed) ->
    varf:('expr1 Var.t -> 'expr2 Var.t) ->
    'expr1 scope_body_expr ->
    'expr2 scope_body_expr Bindlib.box =
 fun ~f ~varf scope_body_expr ->
  fold_right_lets
    ~f:(fun scope_let var_next acc ->
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet { scope_let with scope_let_next; scope_let_expr })
        (Bindlib.bind_var (varf var_next) acc)
        (Expr.Box.inj (f scope_let.scope_let_expr)))
    ~init:(fun res ->
      Bindlib.box_apply (fun res -> Result res) (Expr.Box.inj (f res)))
    scope_body_expr

let rec fold_left ~f ~init scopes =
  match scopes with
  | Nil -> init
  | ScopeDef scope_def ->
    let var, next = Bindlib.unbind scope_def.scope_next in
    fold_left ~f ~init:(f init scope_def var) next

let rec fold_right ~f ~init scopes =
  match scopes with
  | Nil -> init
  | ScopeDef scope_def ->
    let var_next, next = Bindlib.unbind scope_def.scope_next in
    let result_next = fold_right ~f ~init next in
    f scope_def var_next result_next

let map ~f scopes =
  fold_right
    ~f:(fun scope_def var_next acc ->
      let new_def = f scope_def in
      let new_next = Bindlib.bind_var var_next acc in
      Bindlib.box_apply2
        (fun new_def new_next ->
          ScopeDef { new_def with scope_next = new_next })
        new_def new_next)
    ~init:(Bindlib.box Nil) scopes

let map_exprs ~f ~varf scopes =
  fold_right
    ~f:(fun scope_def var_next acc ->
      let scope_input_var, scope_lets =
        Bindlib.unbind scope_def.scope_body.scope_body_expr
      in
      let new_body_expr = map_exprs_in_lets ~f ~varf scope_lets in
      let new_body_expr =
        Bindlib.bind_var (varf scope_input_var) new_body_expr
      in
      let new_next = Bindlib.bind_var (varf var_next) acc in
      Bindlib.box_apply2
        (fun scope_body_expr scope_next ->
          ScopeDef
            {
              scope_def with
              scope_body = { scope_def.scope_body with scope_body_expr };
              scope_next;
            })
        new_body_expr new_next)
    ~init:(Bindlib.box Nil) scopes

(* TODO: compute the expected body expr arrow type manually instead of [TAny]
   for double-checking types ? *)
let rec get_body_expr_mark = function
  | ScopeLet sl ->
    let _, e = Bindlib.unbind sl.scope_let_next in
    get_body_expr_mark e
  | Result e ->
    let m = Marked.get_mark e in
    Expr.with_ty m (Utils.Marked.mark (Expr.mark_pos m) TAny)

let get_body_mark scope_body =
  let _, e = Bindlib.unbind scope_body.scope_body_expr in
  get_body_expr_mark e

let rec unfold_body_expr (ctx : decl_ctx) (scope_let : 'e scope_body_expr) =
  match scope_let with
  | Result e -> Expr.box e
  | ScopeLet
      {
        scope_let_kind = _;
        scope_let_typ;
        scope_let_expr;
        scope_let_next;
        scope_let_pos;
      } ->
    let var, next = Bindlib.unbind scope_let_next in
    Expr.make_let_in var scope_let_typ (Expr.box scope_let_expr)
      (unfold_body_expr ctx next)
      scope_let_pos

let build_typ_from_sig
    (_ctx : decl_ctx)
    (scope_input_struct_name : StructName.t)
    (scope_return_struct_name : StructName.t)
    (pos : Pos.t) : typ =
  let input_typ = Marked.mark pos (TStruct scope_input_struct_name) in
  let result_typ = Marked.mark pos (TStruct scope_return_struct_name) in
  Marked.mark pos (TArrow (input_typ, result_typ))

type 'e scope_name_or_var = ScopeName of ScopeName.t | ScopeVar of 'e Var.t

let to_expr (ctx : decl_ctx) (body : 'e scope_body) (mark_scope : 'm mark) :
    'e boxed =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_body_expr ctx body_expr in
  Expr.make_abs [| var |] body_expr
    [TStruct body.scope_body_input_struct, Expr.mark_pos mark_scope]
    (Expr.mark_pos mark_scope)

let format
    ?(debug : bool = false)
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    ((n, s) : ScopeName.t * 'm scope_body) =
  Format.fprintf fmt "@[<hov 2>%a %a =@ %a@]" Print.keyword "let"
    ScopeName.format_t n (Expr.format ctx ~debug)
    (Expr.unbox
       (to_expr ctx s
          (Expr.map_mark
             (fun _ -> Marked.get_mark (ScopeName.get_info n))
             (fun ty -> ty)
             (get_body_mark s))))

let rec unfold
    (ctx : decl_ctx)
    (s : 'e scopes)
    (mark : 'm mark)
    (main_scope : 'expr scope_name_or_var) : 'e boxed =
  match s with
  | Nil -> (
    match main_scope with
    | ScopeVar v -> Expr.make_var v mark
    | ScopeName _ -> failwith "should not happen")
  | ScopeDef { scope_name; scope_body; scope_next } ->
    let scope_var, scope_next = Bindlib.unbind scope_next in
    let scope_pos = Marked.get_mark (ScopeName.get_info scope_name) in
    let scope_body_mark = get_body_mark scope_body in
    let main_scope =
      match main_scope with
      | ScopeVar v -> ScopeVar v
      | ScopeName n ->
        if ScopeName.compare n scope_name = 0 then ScopeVar scope_var
        else ScopeName n
    in
    Expr.make_let_in scope_var
      (build_typ_from_sig ctx scope_body.scope_body_input_struct
         scope_body.scope_body_output_struct scope_pos)
      (to_expr ctx scope_body scope_body_mark)
      (unfold ctx scope_next mark main_scope)
      scope_pos

let rec free_vars_body_expr scope_lets =
  match scope_lets with
  | Result e -> Expr.free_vars e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
    let v, body = Bindlib.unbind next in
    Var.Set.union (Expr.free_vars e)
      (Var.Set.remove v (free_vars_body_expr body))

let free_vars_body scope_body =
  let { scope_body_expr = binder; _ } = scope_body in
  let v, body = Bindlib.unbind binder in
  Var.Set.remove v (free_vars_body_expr body)

let rec free_vars scopes =
  match scopes with
  | Nil -> Var.Set.empty
  | ScopeDef { scope_body = body; scope_next = next; _ } ->
    let v, next = Bindlib.unbind next in
    Var.Set.union (Var.Set.remove v (free_vars next)) (free_vars_body body)
