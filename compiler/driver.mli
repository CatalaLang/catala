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
  val surface : Global.options -> Surface.Ast.program

  val desugared :
    Global.options ->
    includes:Global.raw_file list ->
    Desugared.Ast.program * Desugared.Name_resolution.context

  val scopelang :
    Global.options ->
    includes:Global.raw_file list ->
    Shared_ast.untyped Scopelang.Ast.program

  val dcalc :
    Global.options ->
    includes:Global.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    autotest:bool ->
    typed:'m Shared_ast.mark ->
    'm Dcalc.Ast.program * Shared_ast.TypeIdent.t list

  val lcalc :
    Global.options ->
    includes:Global.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    autotest:bool ->
    typed:'m Shared_ast.mark ->
    closure_conversion:bool ->
    keep_special_ops:bool ->
    monomorphize_types:bool ->
    expand_ops:bool ->
    renaming:Shared_ast.Renaming.t option ->
    Shared_ast.typed Lcalc.Ast.program
    * Shared_ast.TypeIdent.t list
    * Shared_ast.Renaming.context option

  val scalc :
    Global.options ->
    includes:Global.raw_file list ->
    optimize:bool ->
    check_invariants:bool ->
    autotest:bool ->
    closure_conversion:bool ->
    keep_special_ops:bool ->
    dead_value_assignment:bool ->
    no_struct_literals:bool ->
    monomorphize_types:bool ->
    expand_ops:bool ->
    renaming:Shared_ast.Renaming.t option ->
    Scalc.Ast.program
    * Shared_ast.TypeIdent.t list
    * Shared_ast.Renaming.context

  (** Utility functions to move from one pass to its successor *)

  val desugared_from_surface :
    Global.options ->
    includes:Global.raw_file list ->
    Surface.Ast.program ->
    Desugared.Ast.program * Desugared.Name_resolution.context

  val scopelang_from_desugared :
    Desugared.Ast.program -> Shared_ast.untyped Scopelang.Ast.program

  val dcalc_from_scopelang :
    optimize:bool ->
    check_invariants:bool ->
    autotest:bool ->
    typed:'m Shared_ast.mark ->
    Shared_ast.untyped Scopelang.Ast.program ->
    'm Dcalc.Ast.program * Shared_ast.TypeIdent.t list

  val dcalc_from_typed_scopelang :
    optimize:bool ->
    check_invariants:bool ->
    autotest:bool ->
    Shared_ast.typed Scopelang.Ast.program ->
    Shared_ast.typed Dcalc.Ast.program * Shared_ast.TypeIdent.t list

  val lcalc_from_dcalc :
    typed:'m Shared_ast.mark ->
    optimize:bool ->
    closure_conversion:bool ->
    keep_special_ops:bool ->
    monomorphize_types:bool ->
    expand_ops:bool ->
    renaming:Shared_ast.Renaming.t option ->
    'm Dcalc.Ast.program * Shared_ast.TypeIdent.t list ->
    Shared_ast.typed Lcalc.Ast.program
    * Shared_ast.TypeIdent.t list
    * Shared_ast.Renaming.context option

  val scalc_from_lcalc :
    keep_special_ops:bool ->
    dead_value_assignment:bool ->
    no_struct_literals:bool ->
    Shared_ast.typed Lcalc.Ast.program
    * Shared_ast.TypeIdent.t list
    * Shared_ast.Renaming.context option ->
    Scalc.Ast.program
    * Shared_ast.TypeIdent.t list
    * Shared_ast.Renaming.context
end

module Commands : sig
  (** Helper functions used by top-level commands *)

  val get_output :
    ?ext:string ->
    Global.options ->
    Global.raw_file option ->
    string option * ((out_channel -> 'a) -> 'a)
  (** bounded open of the expected output file *)

  val get_output_format :
    ?ext:string ->
    Global.options ->
    Global.raw_file option ->
    string option * ((Format.formatter -> 'a) -> 'a)

  val get_scope_uid : Shared_ast.decl_ctx -> string -> Shared_ast.ScopeName.t

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
    (Global.options -> unit) Cmdliner.Term.t ->
    unit

  val register_subcommands :
    string ->
    ?man:Cmdliner.Manpage.block list ->
    ?doc:string ->
    unit Cmdliner.Cmd.t list ->
    unit
end
