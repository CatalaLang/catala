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

type level = Error | Warning | Debug | Log | Result

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
  val add_position : t -> ?message:message -> Pos.t -> t

  (** {2 Content emission}*)

  val emit : t -> level -> unit
end

(** This functions emits the message according to the emission type defined by
    [Cli.message_format_flag]. *)

(** {1 Error exceptions} *)

exception CompilerError of Content.t
exception CompilerErrors of Content.t list

(** {1 Some formatting helpers}*)

val unformat : (Format.formatter -> unit) -> string
(** Converts [f] to a string, discarding formatting and skipping newlines and
    indents *)

val has_color : out_channel -> bool
val set_terminal_width_function : (unit -> int) -> unit
val terminal_columns : unit -> int

(* {1 More general color-enabled formatting helpers}*)

val formatter_of_out_channel : out_channel -> unit -> Format.formatter
(** Creates a new formatter from the given out channel, with correct handling of
    the ocolor tags. Actual use of escape codes in the output depends on
    [Cli.style_flag] -- and wether the channel is a tty if that is set to auto. *)

(** {1 Simple interface for various message emission} *)

type ('a, 'b) emitter =
  ?header:Content.message ->
  ?internal:bool ->
  ?pos:Pos.t ->
  ?pos_msg:Content.message ->
  ?extra_pos:(string * Pos.t) list ->
  ?fmt_pos:(Content.message * Pos.t) list ->
  ?outcome:Content.message list ->
  ?suggestion:string list ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

val log : ('a, unit) emitter
val debug : ('a, unit) emitter
val result : ('a, unit) emitter
val warning : ('a, unit) emitter
val error : ('a, 'exn) emitter
val results : Content.message list -> unit

(** Multiple errors *)

val with_delayed_errors : ?stop_on_error:bool -> (unit -> 'a) -> 'a
(** [with_delayed_errors ?stop_on_error f] calls [f] and registers each error
    triggered using [delayed_error]. [stop_on_error] defaults to
    [Global.options.stop_on_error].

    @raise CompilerErrors when delayed errors were registered.
    @raise CompilerError
      on the first error encountered when the [stop_on_error] flag is set. *)

val delayed_error : 'b -> ('a, 'b) emitter
