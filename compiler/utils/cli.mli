(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

type backend_lang = En | Fr | Pl

(** {2 Configuration globals} *)

val source_files : string list ref
(** Source files to be compiled *)

val locale_lang : backend_lang ref

val contents : string ref

val debug_flag : bool ref

val style_flag : bool ref
(** Styles the terminal output *)

val optimize_flag : bool ref

val max_prec_digits : int ref
(** Max number of digits to show for decimal results *)

val trace_flag : bool ref

(** {2 CLI terms} *)

val file : string Cmdliner.Term.t

val debug : bool Cmdliner.Term.t

val unstyled : bool Cmdliner.Term.t

val trace_opt : bool Cmdliner.Term.t

val wrap_weaved_output : bool Cmdliner.Term.t

val backend : string Cmdliner.Term.t

type backend_option =
  | Latex
  | Makefile
  | Html
  | Run
  | OCaml
  | Python
  | Solidity
  | Dcalc
  | Scopelang

val language : string option Cmdliner.Term.t

val max_prec_digits_opt : int option Cmdliner.Term.t

val ex_scope : string option Cmdliner.Term.t

val output : string option Cmdliner.Term.t

val catala_t :
  (string ->
  bool ->
  bool ->
  bool ->
  string ->
  string option ->
  int option ->
  bool ->
  bool ->
  string option ->
  string option ->
  'a) ->
  'a Cmdliner.Term.t
(** Main entry point:
    [catala_t file debug unstyled wrap_weaved_output backend language max_prec_digits_opt trace_opt optimize
    ex_scope output] *)

val version : string

val info : Cmdliner.Term.info

(**{1 Terminal formatting}*)

(**{2 Markers}*)

val print_with_style : ANSITerminal.style list -> ('a, unit, string) format -> 'a

val debug_marker : unit -> string

val error_marker : unit -> string

val warning_marker : unit -> string

val result_marker : unit -> string

val log_marker : unit -> string

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

val concat_with_line_depending_prefix_and_suffix :
  (int -> string) -> (int -> string) -> string list -> string

val add_prefix_to_each_line : string -> (int -> string) -> string
(** The int argument of the prefix corresponds to the line number, starting at 0 *)

val debug_print : string -> unit

val error_print : string -> unit

val warning_print : string -> unit

val result_print : string -> unit

val log_print : string -> unit
