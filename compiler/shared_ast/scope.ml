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

open Types

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

let map_exprs_in_lets ~f ~varf scope_body_expr =
  fold_right_lets
    ~f:(fun scope_let var_next acc ->
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet { scope_let with scope_let_next; scope_let_expr })
        (Bindlib.bind_var (varf var_next) acc)
        (f scope_let.scope_let_expr))
    ~init:(fun res -> Bindlib.box_apply (fun res -> Result res) (f res))
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
