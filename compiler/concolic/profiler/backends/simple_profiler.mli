(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

val time : cpu:bool option -> unit -> Profiler.time

type state

val empty : Profiler.verbosity -> state

(** The [Base] functor helps to define other backends without having to write
    the same functions again and again.

    Given a way to get and set a [state] and an output function, the functor
    will produce a module implementing the [DRIVER] interface.

    Note that the produced module will try to [output] report every time
    [stamp], [inc], [mark], [span] or [stop] is used. *)
module Base (P : sig
  type t

  val get_state : t -> state
  val set_state : t -> state -> unit
  val output_report : (t -> Profiler.report -> unit) option
end) : sig
  val time : cpu:bool option -> P.t -> Profiler.time

  val record :
    cpu:bool option -> P.t -> Profiler.verbosity -> Profiler.id -> unit

  val aggregate :
    cpu:bool option -> P.t -> Profiler.verbosity -> Profiler.id -> unit

  val stop_and_report : cpu:bool option -> P.t -> Profiler.report
  val report : cpu:bool option -> P.t -> Profiler.report option

  val stamp :
    cpu:bool option -> P.t -> Profiler.verbosity -> Profiler.id -> unit

  val inc : P.t -> Profiler.report -> unit
  val mark : P.t -> Profiler.verbosity -> Profiler.ids -> unit

  val span :
    cpu:bool option ->
    P.t ->
    Profiler.verbosity ->
    Profiler.span ->
    Profiler.ids ->
    unit

  val stop : P.t -> unit
end

val headless : Profiler.verbosity Profiler.driver
(** Memory driver allowing building intermediate reports with purpose of being
    included in external reports or manipulated directly. *)

val auto_write_as_txt_to_file : (string * Profiler.verbosity) Profiler.driver
(** Driver printing its report in plain text to a file whenever a toplevel
    section ends. *)

val auto_write_as_json_to_file : (string * Profiler.verbosity) Profiler.driver
(** Driver printing its report in JSON to a file whenever a toplevel section
    ends. *)

val pp_report : ?t0:Profiler.time -> Format.formatter -> Profiler.report -> unit
