(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>,
   Vincent Botbol <vincent.botbol@inria.fr>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

val resolve_coord_parts :
  project_name:string -> string -> string * string * string

val format_target_pom_xml :
  config:Clerk_cli.config ->
  project_name:string ->
  Format.formatter ->
  Clerk_config.target ->
  unit

val format_project_pom_xml :
  config:Clerk_cli.config ->
  Format.formatter ->
  Clerk_config.target list ->
  unit
