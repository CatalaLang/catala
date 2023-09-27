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

val main : unit -> unit
(** Main program entry point, including command-line parsing and return code *)

(** Compiler passes

    Each pass takes only its cli options, then calls upon its dependent passes
    (forwarding their options as needed) *)
module Passes : sig
  val surface : Cli.options -> includes:Cli.raw_file list -> Surface.Ast.program

  val desugared :
    Cli.options ->
    includes:Cli.raw_file list ->
    Desugared.Ast.program * Desugared.Name_resolution.context

  val scopelang :
    Cli.options ->
    includes:Cli.raw_file list ->
    Shared_ast.untyped Scopelang.Ast.program
    * Desugared.Name_resolution.context
    * Desugared.Dependency.ExceptionsDependencies.t Desugared.Ast.ScopeDef.Map.t

  val dcalc :
    Cli.options ->
    includes:Cli.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    Shared_ast.typed Dcalc.Ast.program
    * Desugared.Name_resolution.context
    * Scopelang.Dependency.TVertex.t list

  val lcalc :
    Cli.options ->
    includes:Cli.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    avoid_exceptions:bool ->
    closure_conversion:bool ->
    Shared_ast.untyped Lcalc.Ast.program
    * Desugared.Name_resolution.context
    * Scopelang.Dependency.TVertex.t list

  val scalc :
    Cli.options ->
    includes:Cli.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    avoid_exceptions:bool ->
    closure_conversion:bool ->
    Scalc.Ast.program
    * Desugared.Name_resolution.context
    * Scopelang.Dependency.TVertex.t list
end

module Commands : sig
  (** Helper functions used by top-level commands *)

  val get_output :
    ?ext:string ->
    Cli.options ->
    Cli.raw_file option ->
    string option * ((out_channel -> 'a) -> 'a)
  (** bounded open of the expected output file *)

  val get_output_format :
    ?ext:string ->
    Cli.options ->
    Cli.raw_file option ->
    string option * ((Format.formatter -> 'a) -> 'a)

  val get_scope_uid :
    Desugared.Name_resolution.context -> string -> Shared_ast.ScopeName.t

  val get_variable_uid :
    Desugared.Name_resolution.context ->
    Shared_ast.ScopeName.t ->
    string ->
    Desugared.Ast.ScopeDef.t

  val commands : unit Cmdliner.Cmd.t list
  (** The list of built-in catala subcommands, as expected by
      [Cmdliner.Cmd.group] *)
end

(** Various helpers *)

val modname_of_file : string -> string

(** API available to plugins for their own registration *)

module Plugin : sig
  val register :
    string ->
    ?man:Cmdliner.Manpage.block list ->
    ?doc:string ->
    (Cli.options -> unit) Cmdliner.Term.t ->
    unit
end
