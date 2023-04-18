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

open Shared_ast

(** Abstract syntax tree for the lambda calculus *)

(** {1 Abstract syntax tree} *)

type 'm naked_expr = (lcalc, 'm mark) naked_gexpr
and 'm expr = (lcalc, 'm mark) gexpr

type 'm program = 'm expr Shared_ast.program

(** {1 Option-related management}*)

(** {2 Names and types}*)

val option_enum : EnumName.t
val none_constr : EnumConstructor.t
val some_constr : EnumConstructor.t
val option_enum_config : typ EnumConstructor.Map.t

(** {2 Term building and management for the [option] monad}*)

val monad_return :
  mark:'m mark ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr

val monad_empty : mark:'m mark -> (([< all ] as 'a), 'm mark) boxed_gexpr

val monad_bind_var :
  mark:'m mark ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) gexpr Var.t ->
  ('a, 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr

val monad_bind :
  mark:'m mark ->
  var_name:string ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr

val monad_bind_cont :
  mark:'m mark ->
  var_name:string ->
  ((([< all ] as 'a), 'm mark) gexpr Var.t -> ('a, 'm mark) boxed_gexpr) ->
  ('a, 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr

val monad_mbind_mvar :
  mark:'m mark ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) gexpr Var.t list ->
  ('a, 'm mark) boxed_gexpr list ->
  ('a, 'm mark) boxed_gexpr

val monad_mbind :
  mark:'m mark ->
  var_name:string ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr list ->
  ('a, 'm mark) boxed_gexpr

val monad_mbind_cont :
  mark:'m mark ->
  var_name:string ->
  ((([< all ] as 'a), 'm mark) gexpr Var.t list -> ('a, 'm mark) boxed_gexpr) ->
  ('a, 'm mark) boxed_gexpr list ->
  ('a, 'm mark) boxed_gexpr

val monad_error_on_empty :
  mark:'a mark ->
  var_name:string ->
  ?toplevel:bool ->
  (([< all > `Exceptions ] as 'b), 'a mark) boxed_gexpr ->
  ('b, 'a mark) boxed_gexpr

val monad_map :
  mark:'m mark ->
  var_name:string ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr

val monad_mmap_mvar :
  mark:'m mark ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) gexpr Var.t list ->
  ('a, 'm mark) boxed_gexpr list ->
  ('a, 'm mark) boxed_gexpr

val monad_mmap :
  mark:'m mark ->
  var_name:string ->
  (([< all ] as 'a), 'm mark) boxed_gexpr ->
  ('a, 'm mark) boxed_gexpr list ->
  ('a, 'm mark) boxed_gexpr
