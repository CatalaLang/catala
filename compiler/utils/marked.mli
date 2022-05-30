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

(** AST node annotations (used for position, type, etc.) *)

type ('a, 'm) t = 'a * 'm
(** Everything related to the source code should keep its position stored, to
    improve error messages *)

type 'a pos = ('a, Pos.t) t

val mark : 'm -> 'a -> ('a, 'm) t
val unmark : ('a, 'm) t -> 'a
val get_mark : ('a, 'm) t -> 'm
val map_under_mark : ('a -> 'b) -> ('a, 'm) t -> ('b, 'm) t
val same_mark_as : 'a -> ('b, 'm) t -> ('a, 'm) t
val unmark_option : ('a, 'm) t option -> 'a option

(** Visitors *)

class ['self] marked_map :
  object ('self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
            ; .. >

    method visit_marked :
      'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
  end

class ['self] marked_iter :
  object ('self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
            ; .. >

    method visit_marked : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
  end

class ['self] pos_map :
  object ('self)
    constraint
    'self = < visit_pos :
                'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
            ; .. >

    method visit_pos :
      'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) t -> ('a, 'm) t
  end

class ['self] pos_iter :
  object ('self)
    constraint
    'self = < visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
            ; .. >

    method visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) t -> unit
  end
