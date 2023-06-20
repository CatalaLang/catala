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

type backend_lang = En | Fr | Pl

type backend_option =
  [ `Latex
  | `Makefile
  | `Html
  | `Interpret
  | `Interpret_Lcalc
  | `Typecheck
  | `OCaml
  | `Python
  | `Scalc
  | `Lcalc
  | `Dcalc
  | `Scopelang
  | `Exceptions
  | `Proof ]

(** The usual auto/always/never option argument *)
type when_enum = Auto | Always | Never

val languages : (string * backend_lang) list

val language_code : backend_lang -> string
(** Returns the lowercase two-letter language code *)

(** {2 Configuration globals} *)

val source_files : string list ref
(** Source files to be compiled *)

val locale_lang : backend_lang ref
val contents : string ref
val debug_flag : bool ref

val style_flag : when_enum ref
(** Styles the terminal output *)

val optimize_flag : bool ref

val max_prec_digits : int ref
(** Max number of digits to show for decimal results *)

val trace_flag : bool ref
val disable_warnings_flag : bool ref

val check_invariants_flag : bool ref
(** Check structural invariants on the AST. *)

val disable_counterexamples : bool ref
(** Disables model-generated counterexamples for proofs that fail. *)

val avoid_exceptions_flag : bool ref
(** Avoids using [try ... with] exceptions when compiling the default calculus. *)

type message_format_enum =
  | Human
  | GNU  (** Format of error and warning messages output by the compiler. *)

val message_format_flag : message_format_enum ref

(** {2 CLI terms} *)

val file : string Cmdliner.Term.t
val debug : bool Cmdliner.Term.t
val unstyled : bool Cmdliner.Term.t
val trace_opt : bool Cmdliner.Term.t
val check_invariants_opt : bool Cmdliner.Term.t
val wrap_weaved_output : bool Cmdliner.Term.t
val print_only_law : bool Cmdliner.Term.t
val plugins_dirs : string list Cmdliner.Term.t
val language : string option Cmdliner.Term.t
val max_prec_digits_opt : int option Cmdliner.Term.t
val ex_scope : string option Cmdliner.Term.t
val output : string option Cmdliner.Term.t

type global_options = {
  debug : bool;
  color : when_enum;
  message_format : message_format_enum;
  wrap_weaved_output : bool;
  avoid_exceptions : bool;
  plugins_dirs : string list;
  language : string option;
  max_prec_digits : int option;
  trace : bool;
  disable_warnings : bool;
  disable_counterexamples : bool;
  check_invariants : bool;
  optimize : bool;
  ex_scope : string option;
  ex_variable : string option;
  output_file : string option;
  closure_conversion : bool;
  print_only_law : bool;
  link_modules : string list;
}
(** {2 Command-line application} *)

val global_options : global_options Cmdliner.Term.t

val catala_t :
  ?extra:int Cmdliner.Cmd.t list ->
  (backend_option -> string -> global_options -> int) ->
  int Cmdliner.Cmd.t
(** Main entry point: [catala_t file options] *)

val set_option_globals : global_options -> unit
val version : string
val info : Cmdliner.Cmd.info
