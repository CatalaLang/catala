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

open Clerk_utils
open Clerk_lib

module Flags : sig
  val def : variables:(string * 'a) list -> Var.t -> 'a lazy_t -> Var.t * 'a
  val includes : ?backend:string -> string list -> string list
  val include_flags : backend:string -> string list -> string list

  val default :
    code_coverage:bool -> config:Clerk_cli.config -> (Var.t * string list) list
end

module Ninja : sig
  val static_base_rules : Ninja_utils.def list
end
