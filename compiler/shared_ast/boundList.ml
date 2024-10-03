(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Definitions

type ('e, 'elt, 'last) t = ('e, 'elt, 'last) bound_list =
  | Last of 'last
  | Cons of 'elt * ('e, ('e, 'elt, 'last) t) binder

let rec to_seq = function
  | Last _ -> Seq.empty
  | Cons (item, next_bind) ->
    fun () ->
      let v, next = Bindlib.unbind next_bind in
      Seq.Cons ((v, item), to_seq next)

let rec of_list list ~last =
  match list with
  | [] -> Bindlib.box_apply (fun l -> Last l) last
  | (var, item) :: list ->
    Bindlib.box_apply2
      (fun item next -> Cons (item, next))
      item
      (Bindlib.bind_var var (of_list list ~last))

let rec last = function
  | Last e -> e
  | Cons (_, bnd) ->
    let _, next = Bindlib.unbind bnd in
    last next

let rec iter ~f = function
  | Last l -> l
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    f var item;
    iter ~f next

let rec find ~f = function
  | Last _ -> raise Not_found
  | Cons (item, next_bind) -> (
    match f item with
    | Some r -> r
    | None ->
      let _, next = Bindlib.unbind next_bind in
      find ~f next)

let rec fold_left ~f ~init = function
  | Last l -> init, l
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    fold_left ~f ~init:(f init item var) next

let rec fold_right ~f ~init = function
  | Last l -> init l
  | Cons (item, next_bind) ->
    let var_next, next = Bindlib.unbind next_bind in
    let result_next = fold_right ~f ~init next in
    f item var_next result_next

let rec fold_lr ~top ~down ~bottom ~up = function
  | Last l -> bottom l top
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let top = down var item top in
    let bottom = fold_lr ~down ~up ~top ~bottom next in
    up var item bottom

let rec map_last ~f ~last = function
  | Last l -> last l
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let var, item = f var item in
    let next_bind = Bindlib.bind_var var (map_last ~f ~last next) in
    Bindlib.box_apply2
      (fun item next_bind -> Cons (item, next_bind))
      item next_bind

let map ~f ~last =
  map_last ~f ~last:(fun l -> Bindlib.box_apply (fun l -> Last l) (last l))

let rec fold_map ~f ~last ~init:ctx = function
  | Last l ->
    let ret, l = last ctx l in
    ret, Bindlib.box_apply (fun l -> Last l) l
  | Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let ctx, var, item = f ctx var item in
    let ctx, next = fold_map ~f ~last ~init:ctx next in
    let next_bind = Bindlib.bind_var var next in
    ( ctx,
      Bindlib.box_apply2
        (fun item next_bind -> Cons (item, next_bind))
        item next_bind )

let rec fold_left2 ~f ~init a b =
  match a, b with
  | Last l1, Last l2 -> init, (l1, l2)
  | Cons (item1, next_bind1), Cons (item2, next_bind2) ->
    let var, next1, next2 = Bindlib.unbind2 next_bind1 next_bind2 in
    fold_left2 ~f ~init:(f init item1 item2 var) next1 next2
  | _ -> invalid_arg "fold_left2"

let rec equal ~f ~last a b =
  match a, b with
  | Last l1, Last l2 -> last l1 l2
  | Cons (item1, next_bind1), Cons (item2, next_bind2) ->
    f item1 item2
    &&
    let _, next1, next2 = Bindlib.unbind2 next_bind1 next_bind2 in
    equal ~f ~last next1 next2
  | _ -> false

let rec compare ~f ~last a b =
  match a, b with
  | Last l1, Last l2 -> last l1 l2
  | Cons (item1, next_bind1), Cons (item2, next_bind2) -> (
    match f item1 item2 with
    | 0 ->
      let _, next1, next2 = Bindlib.unbind2 next_bind1 next_bind2 in
      compare ~f ~last next1 next2
    | n -> n)
  | Last _, Cons _ -> -1
  | Cons _, Last _ -> 1
