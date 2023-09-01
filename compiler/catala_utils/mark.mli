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

type ('a, 'm) ed = 'a * 'm
(** The type of [Mark.ed] values. Everything related to the source code should
    keep at least its position stored, to improve error messages. Typing, etc.
    also leverage this. *)

type 'a pos = ('a, Pos.t) ed
(** The type of marks containing only position information *)

val add : 'm -> 'a -> ('a, 'm) ed
val remove : ('a, 'm) ed -> 'a
val get : ('a, 'm) ed -> 'm
val set : 'm -> ('a, _) ed -> ('a, 'm) ed
val map : ('a -> 'b) -> ('a, 'm) ed -> ('b, 'm) ed
val map_mark : ('m1 -> 'm2) -> ('a, 'm1) ed -> ('a, 'm2) ed
val copy : ('b, 'm) ed -> 'a -> ('a, 'm) ed
val fold : ('a -> 'b) -> ('a, _) ed -> 'b
val fold2 : ('a -> 'a -> 'b) -> ('a, 'm) ed -> ('a, 'm) ed -> 'b

val compare : ('a -> 'a -> int) -> ('a, 'm) ed -> ('a, 'm) ed -> int
(** Compares two marked values {b ignoring marks} *)

val equal : ('a -> 'a -> bool) -> ('a, 'm) ed -> ('a, 'm) ed -> bool
(** Tests equality of two marked values {b ignoring marks} *)

(** Visitors *)

class ['self] marked_map : object ('self)
  constraint
  'self = < visit_marked :
              'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) ed -> ('a, 'm) ed
          ; .. >

  method visit_marked :
    'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) ed -> ('a, 'm) ed
end

class ['self] marked_iter : object ('self)
  constraint
  'self = < visit_marked :
              'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) ed -> unit
          ; .. >

  method visit_marked : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) ed -> unit
end

class ['self] pos_map : object ('self)
  constraint
  'self = < visit_pos :
              'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) ed -> ('a, 'm) ed
          ; .. >

  method visit_pos :
    'a. ('env -> 'a -> 'a) -> 'env -> ('a, 'm) ed -> ('a, 'm) ed
end

class ['self] pos_iter : object ('self)
  constraint
  'self = < visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) ed -> unit
          ; .. >

  method visit_pos : 'a. ('env -> 'a -> unit) -> 'env -> ('a, 'm) ed -> unit
end
