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

let map_exprs_in_lets :
    ?typ:(typ -> typ) ->
    f:('expr1 -> 'expr2 boxed) ->
    varf:('expr1 Var.t -> 'expr2 Var.t) ->
    'expr1 scope_body_expr ->
    'expr2 scope_body_expr Bindlib.box =
 fun ?(typ = Fun.id) ~f ~varf scope_body_expr ->
  let f e = Expr.Box.lift (f e) in
  BoundList.map ~last:f
    ~f:(fun v scope_let ->
      ( varf v,
        Bindlib.box_apply
          (fun scope_let_expr ->
            {
              scope_let with
              scope_let_expr;
              scope_let_typ = typ scope_let.scope_let_typ;
            })
          (f scope_let.scope_let_expr) ))
    scope_body_expr

let map_last_item ~varf last =
  Bindlib.box_list
  @@ List.map
       (function EVar v -> Bindlib.box_var (varf v) | _ -> assert false)
       last

let map_exprs ?(typ = Fun.id) ~f ~varf scopes =
  let f v = function
    | ScopeDef (name, body) ->
      let scope_input_var, scope_lets = Bindlib.unbind body.scope_body_expr in
      let new_body_expr = map_exprs_in_lets ~typ ~f ~varf scope_lets in
      let new_body_expr =
        Bindlib.bind_var (varf scope_input_var) new_body_expr
      in
      ( varf v,
        Bindlib.box_apply
          (fun scope_body_expr ->
            ScopeDef (name, { body with scope_body_expr }))
          new_body_expr )
    | Topdef (name, ty, vis, expr) ->
      ( varf v,
        Bindlib.box_apply
          (fun e -> Topdef (name, typ ty, vis, e))
          (Expr.Box.lift (f expr)) )
  in
  BoundList.map ~f ~last:(map_last_item ~varf) scopes

let fold_exprs ~f ~init scopes =
  let f acc def _ =
    match def with
    | Topdef (_, typ, _vis, e) -> f acc e typ
    | ScopeDef (_, scope) ->
      let _, body = Bindlib.unbind scope.scope_body_expr in
      let acc, last =
        BoundList.fold_left body ~init:acc ~f:(fun acc sl _ ->
            f acc sl.scope_let_expr sl.scope_let_typ)
      in
      f acc last (TStruct scope.scope_body_output_struct, Expr.pos last)
  in
  fst @@ BoundList.fold_left ~f ~init scopes

let typ body =
  let pos = Mark.get (StructName.get_info body.scope_body_input_struct) in
  let input_typ = Mark.add pos (TStruct body.scope_body_input_struct) in
  let result_typ = Mark.add pos (TStruct body.scope_body_output_struct) in
  Mark.add pos (TArrow ([input_typ], result_typ))

let get_body_mark scope_body =
  let m0 =
    match Bindlib.unbind scope_body.scope_body_expr with
    | _, Last (_, m) | _, Cons ({ scope_let_expr = _, m; _ }, _) -> m
  in
  Expr.with_ty m0 (typ scope_body)

let unfold_body_expr (_ctx : decl_ctx) (scope_let : 'e scope_body_expr) =
  BoundList.fold_right scope_let ~init:Expr.rebox ~f:(fun sl var acc ->
      Expr.make_let_in var sl.scope_let_typ
        (Expr.rebox sl.scope_let_expr)
        acc sl.scope_let_pos)

let input_type ty io =
  match io, ty with
  | (Runtime.Reentrant, iopos), (TArrow (args, ret), tpos) ->
    TArrow (args, (TDefault ret, iopos)), tpos
  | (Runtime.Reentrant, iopos), (ty, tpos) -> TDefault (ty, tpos), iopos
  | _, ty -> ty

let to_expr (ctx : decl_ctx) (body : 'e scope_body) : 'e boxed =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_body_expr ctx body_expr in
  let pos = Expr.pos body_expr in
  Expr.make_abs [| var |] body_expr
    [TStruct body.scope_body_input_struct, pos]
    pos

let unfold (ctx : decl_ctx) (s : 'e code_item_list) (main_scope : ScopeName.t) :
    'e boxed =
  BoundList.fold_lr s ~top:None
    ~down:(fun v item main ->
      match main, item with
      | None, ScopeDef (name, body) when ScopeName.equal name main_scope ->
        Some (Expr.make_var v (get_body_mark body))
      | r, _ -> r)
    ~bottom:(fun _vlist -> function Some v -> v | None -> raise Not_found)
    ~up:(fun var item next ->
      let e, typ =
        match item with
        | ScopeDef (_, body) -> to_expr ctx body, typ body
        | Topdef (_, typ, _vis, expr) -> Expr.rebox expr, typ
      in
      Expr.make_let_in var typ e next (Expr.pos e))

let free_vars_body_expr scope_lets =
  BoundList.fold_right scope_lets ~init:Expr.free_vars ~f:(fun sl v acc ->
      Var.Set.union (Var.Set.remove v acc) (Expr.free_vars sl.scope_let_expr))

let free_vars_item = function
  | ScopeDef (_, { scope_body_expr; _ }) ->
    let v, body = Bindlib.unbind scope_body_expr in
    Var.Set.remove v (free_vars_body_expr body)
  | Topdef (_, _, _, expr) -> Expr.free_vars expr

let free_vars scopes =
  BoundList.fold_right scopes
    ~init:(fun _vlist -> Var.Set.empty)
    ~f:(fun item v acc ->
      Var.Set.union (Var.Set.remove v acc) (free_vars_item item))
