(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

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

val driver :
  [< Cli.backend_option | `Plugin of Plugin.handler ] ->
  Pos.input_file ->
  Cli.global_options ->
  int
(** Entry function for the executable. Returns a negative number in case of
    error. *)

val main : unit -> unit
(** Main program entry point, including command-line parsing and return code *)

module Plugin : sig
  include module type of Plugin.PluginAPI
  open Cmdliner

  val register_generic : Cmd.info -> Cmd.Exit.code Term.t -> unit

  val register_dcalc :
    Cmd.info ->
    extension:string ->
    Shared_ast.untyped Dcalc.Ast.program plugin_apply_fun_typ ->
    unit

  val register_lcalc :
    Cmd.info ->
    extension:string ->
    Shared_ast.untyped Lcalc.Ast.program plugin_apply_fun_typ ->
    unit

  val register_scalc :
    Cmd.info ->
    extension:string ->
    Scalc.Ast.program plugin_apply_fun_typ ->
    unit
end
