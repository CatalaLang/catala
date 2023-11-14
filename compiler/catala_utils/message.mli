(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Interface for emitting compiler messages.

    All messages are expected to use the [Format] module. Flush, ["@?"], ["@."],
    ["%!"] etc. are not supposed to be used outside of this module.

    WARNING: this module performs side-effects at load time, adding support for
    ocolor tags (e.g. ["@{<blue>text@}"]) to the standard string formatter used
    by e.g. [Format.sprintf]. (In this case, the tags are ignored, for color
    output you should use the functions of this module that toggle support
    depending on cli flags and terminal support). *)

(** {1 Message content} *)

type content_type = Error | Warning | Debug | Log | Result

module Content : sig
  (** {2 Types}*)

  type message = Format.formatter -> unit
  type t

  (** {2 Content creation}*)

  val of_message : message -> t

  val of_result : message -> t
  (** Similar as [of_message] but tailored for when you want to print the result
      of a value, etc. *)

  val of_string : string -> t
  val prepend_message : t -> (Format.formatter -> unit) -> t

  (** {2 Content manipulation}*)

  val to_internal_error : t -> t
  val add_suggestion : t -> string list -> t

  (** {2 Content emission}*)

  val emit : t -> content_type -> unit
end

(** This functions emits the message according to the emission type defined by
    [Cli.message_format_flag]. *)

(** {1 Error exception} *)

exception CompilerError of Content.t

(** {1 Common error raising} *)

val raise_spanned_error :
  ?span_msg:Content.message ->
  ?suggestion:string list ->
  Pos.t ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

val raise_multispanned_error_full :
  ?suggestion:string list ->
  (Content.message option * Pos.t) list ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

val raise_multispanned_error :
  ?suggestion:string list ->
  (string option * Pos.t) list ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

val raise_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
val raise_internal_error : ('a, Format.formatter, unit, 'b) format4 -> 'a

val assert_internal_error :
  bool -> ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

(** {1 Common warning emission}*)

val emit_multispanned_warning :
  (Content.message option * Pos.t) list ->
  ('a, Format.formatter, unit) format ->
  'a

val emit_spanned_warning :
  ?span_msg:Content.message ->
  Pos.t ->
  ('a, Format.formatter, unit) format ->
  'a

val emit_warning : ('a, Format.formatter, unit) format -> 'a

(** {1 Common log emission}*)

val emit_log : ('a, Format.formatter, unit) format -> 'a

(** {1 Common debug emission}*)

val emit_debug : ('a, Format.formatter, unit) format -> 'a

(** {1 Common result emission}*)

val emit_result : ('a, Format.formatter, unit) format -> 'a

(** {1 Some formatting helpers}*)

val unformat : (Format.formatter -> unit) -> string
(** Converts [f] to a string, discarding formatting and skipping newlines and
    indents *)

val has_color : out_channel -> bool

(* {1 More general color-enabled formatting helpers}*)

val formatter_of_out_channel : out_channel -> Format.formatter
(** Creates a new formatter from the given out channel, with correct handling of
    the ocolor tags. Actual use of escape codes in the output depends on
    [Cli.style_flag] -- and wether the channel is a tty if that is set to auto. *)
