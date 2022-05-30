(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type ('a, 'm) t = 'a * 'm
type 'a pos = ('a, Pos.t) t

let mark m e : ('a, 'm) t = e, m
let unmark ((x, _) : ('a, 'm) t) : 'a = x
let get_mark ((_, x) : ('a, 'm) t) : 'm = x
let map_under_mark (f : 'a -> 'b) ((x, y) : ('a, 'm) t) : ('b, 'c) t = f x, y
let same_mark_as (x : 'a) ((_, y) : ('b, 'm) t) : ('a, 'm) t = x, y

let unmark_option (x : ('a, 'm) t option) : 'a option =
  match x with Some x -> Some (unmark x) | None -> None

class ['self] marked_map =
  object (_self : 'self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
            ; .. >

    method visit_marked
        : 'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t =
      fun f env x -> same_mark_as (f env (unmark x)) x
  end

class ['self] marked_iter =
  object (_self : 'self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
            ; .. >

    method visit_marked : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
        =
      fun f env x -> f env (unmark x)
  end

class ['self] pos_map =
  object (_self : 'self)
    constraint
    'self = < visit_pos :
                'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
            ; .. >

    method visit_pos
        : 'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t =
      fun f env x -> same_mark_as (f env (unmark x)) x
  end

class ['self] pos_iter =
  object (_self : 'self)
    constraint
    'self = < visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
            ; .. >

    method visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit =
      fun f env x -> f env (unmark x)
  end
