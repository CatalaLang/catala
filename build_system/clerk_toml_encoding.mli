(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** {1 Overview} *)

(** This module provides constructors and combinators that allow to describe the
    structure of an expected TOML file. It also provide a type-safe
    encoder/decoder. *)

(** {2 Types} *)

type 'a descr
type 'a field
type 'a case
type 'a table_descr
type 'a t = 'a table_descr

(** {2 Encoding functions} *)

val decode : Otoml.t -> 'a t -> 'a
val encode : 'a -> 'a t -> Otoml.t

(** {2 Constructors & Combinators} *)

(** Basic constructors *)

val string : string descr
val pair : 'a descr -> 'b descr -> ('a * 'b) descr
val list : 'a descr -> 'a list descr

(** Object's Field constructors *)

val req_field : name:string -> 'a descr -> 'a field
val opt_field : name:string -> 'a descr -> 'a option field
val dft_field : name:string -> default:'a -> 'a descr -> 'a field

(** Object constructors *)

(** Objects are translated as inner tables in TOML: e.g.,
    [obj2 (req_field ~name:"a" string) (req_field ~name:"b" (list string))]
    described under a table "target" will match the following TOML:
    {v
[target]
a = "some string"
b = [ "hello" ; "world" ]
    v} *)

(** {b Warning}: registering multiple fields in an object under the same name
    will raise an error at runtime when the descriptor gets evaluated. *)

val obj1 : 'a field -> 'a descr
val obj2 : 'a field -> 'b field -> ('a * 'b) descr
val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) descr

val obj4 :
  'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) descr

val obj5 :
  'a field ->
  'b field ->
  'c field ->
  'd field ->
  'e field ->
  ('a * 'b * 'c * 'd * 'e) descr

val merge_objs : 'a descr -> 'b descr -> ('a * 'b) descr

(** Union *)

val case :
  info:string ->
  proj:('a -> 'b option) ->
  inj:('b -> 'a option) ->
  'b descr ->
  'a case
(** Union's case: the [info] is used to describe the case to the user. *)

val union : 'a case list -> 'a descr
(** [union cases] will create a descriptor that matches several patterns, e.g.,
    to describe a sum-type. The first case found for which its [proj] function
    returns [Some _] will be used. *)

val string_cases : (string * 'a) list -> 'a case list

(** Table constructors *)

(** Tables are translated as toplevel tables in TOML: e.g.,
    [table2 (table_opt ~name:"project" (...)) (multi_table ~name:"module" (...))]
    will match the following TOML:
    {v
[project]
...

[[module]]
...

[[module]]
...
    v}

    N.b., the {v [project] v} table could be omitted
*)

(** {b Warning}: describing different toplevel tables using the same name will
    raise an error at runtime when the descriptor gets evaluated. *)

val table_req : name:string -> 'a descr -> 'a t
val table_opt : name:string -> 'a descr -> 'a option t
val multi_table : name:string -> 'a descr -> 'a list t
val table2 : 'a t -> 'b t -> ('a * 'b) t
val table3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val table4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val table5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val merge_tables : 'a t -> 'b t -> ('a * 'b) t

(** Conversion operators *)

val conv : ('a -> 'b) -> ('b -> 'a) -> 'b descr -> 'a descr
val convt : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

(** Utilities *)

val proj_empty_list : 'a list -> 'a list option
val inj_empty_list : 'a list option -> 'a list
