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
open Catala_utils

module type S = sig
  val name : string
  val module_ext : string

  module Flags : sig
    val default :
      variables:(string * string list) list ->
      autotest:bool ->
      use_default_flags:bool ->
      test_flags:string list ->
      include_dirs:string list ->
      (Var.t * string list) list
    (** [defaut ~variables ~autotest ~use_default_flags ~test_flags
         ~include_dirs] returns a list of variables assigned for the Ninja file
        generated. Those variables can be the compiler exec of your backends,
        flags for a specific command, include ...
        - [~variables] is the list of already defined variables, if a variables
          is already defined it will takes this value instead of the one given
        - [~autotest] if the flag is activated, add the autotest flag to the
          catala command, each call to the catala command will come with
          autotest
        - [~use_default_flags] flag to tell if the catala command should only
          use the default flag or could had some more. The default flag being -O
          for optimize
        - [~test_flags] is a list of tests that will be passed to the catala
          interpreter tests if called
        - [~include_dirs] the list of the different direcory to include each
          time, usually this comes from the config in the clerk.toml file. *)
  end

  val modfile :
    is_stdlib:bool -> (string * string) list -> string -> string -> string
  (** [modfile ~is_stdlib same_dir_modules ext modname] is a function that
      return the name of the rule to generates a file in a backend. The
      - [same_dir_modules] is a flag that tells if the rules that makes a call
        to modfile generates a file in the same directory than modname.
      - [ext] is the extension of the file.
      - [modname] is the module name exposed by this file.

      This is more of an internal function but it's needed to be exposed for a
      construction in clerk_rules (so that code is not duplicated in each
      backend) *)

  val static_base_rules : Ninja_utils.def list
  (** [static_base_rules] is a list of rules needed by the backend commonly used
      by any ninja command related to that backend, for example you could have a
      rule for catala command for the backend, a rule to compile a in your
      backend *)

  val runtime_build_statements :
    options:Clerk_lib.Clerk_config.t -> stdbase:string -> Ninja_utils.def list
  (** [runtime_build_statements ~options ~stdbase]
      - [options] the options of the clerk project, the java backends needs it
      - [stdbase] path to the _opam directory where runtime files should be
        installed.

      This function returns a set of rules on how to handle the catala_runtime,
      most of the time it consists in having rules to copy the runtime from the
      _opam directory to the build directory. But as the return of the function
      is generic you can add specific action if needed for that runtime backend.*)

  val external_copy : Scan.item -> Ninja_utils.def Seq.t
  (** [external_copy item] is the set of rules to handle a catala module marked
      as external in [item], most of the time the [external_copy] function
      verifies if the implementation of the external module exists and then
      copies it in the build directory along others modules *)

  val catala :
    ?vars:(Var.t * Ninja_utils.Expr.t) list ->
    is_stdlib:bool ->
    inputs:Ninja_utils.Expr.t ->
    implicit_in:Ninja_utils.Expr.t ->
    bool ->
    Ninja_utils.def Seq.t
  (** [catala ?vars ~is_stdlib ~inputs ~implicit_in has_scope_tests]
      - [?vars] are the vars forwarded to Ninja when calling a rules
      - [is_stdlib] a flag that tells if the item in [inputs] is a standard
        module.
      - [inputs] inputs of the catala rules, the item that will be converted in
        a file.
      - [implicit_in] rules that are implicitly required but as no real link.
      - [has_scope_tests] if the item has scope tests it will generates more
        rules.

      This functions returns the expected to be the rules to create the file in
      your backend language by using the catala command (most of the time using
      what [static_base_rules] set). *)

  val build_object :
    include_dirs:string list ->
    same_dir_modules:(string * string) list ->
    item:Scan.item ->
    bool ->
    Ninja_utils.def Seq.t
  (** [build_object]'s purpose is to return a set of rules that generates the
      compiled form of a target, object is a generic naming for example java's
      [build_object] will generates .class for the item. *)

  val expose_module :
    same_dir_modules:(string * string) list ->
    used_modules:string list ->
    Ninja_utils.def list
  (** [expose_module] generates a set of intermediates rules for backend that
      has more than one file to handle when exposing a module. For example, java
      and python backend only need to handle one file (.py and .class) so the
      function for those backends returns an empty list. For C and OCaml, as
      those backends have interface and implementation file this function
      returns a set of rules to handle those file. *)

  val runtime_dir : File.t Lazy.t
  (** [runtime_dir] the path of the runtime for a backend, most of the time it's
      in the _opam directory that can be queried from Poll library. *)
end
