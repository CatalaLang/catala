(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

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

val extern_src :
  filename:string ->
  backend:string ->
  ext:string ->
  missing:string list ->
  string * string list

val check_missing :
  backend:string ->
  module_def:string Mark.pos option ->
  missing:string list ->
  filename:string ->
  unit

val target : ?suffix:string -> ?backend:string -> string -> string

val modfile :
  ?suffix:string ->
  backend:string ->
  (string * string) list ->
  string ->
  string ->
  string

val get_stdlib_module : string -> string option
