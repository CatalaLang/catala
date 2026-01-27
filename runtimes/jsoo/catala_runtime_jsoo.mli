(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Emile Rolley <emile.rolley@tuta.io>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** A {{:http://ocsigen.org/js_of_ocaml/latest/manual/overview} js_of_ocaml}
    wrapper around the {!module: Runtime}. *)

open Js_of_ocaml
open Catala_runtime

type unit_jsoo = unit

val unit_to_jsoo : unit -> unit_jsoo
val unit_of_jsoo : unit_jsoo -> unit

type bool_jsoo = bool Js.t

val bool_to_jsoo : bool -> bool_jsoo
val bool_of_jsoo : bool_jsoo -> bool

class type bigInt = object
  method toLocalString : Js.js_string Js.t -> Js.js_string Js.t Js.meth

  method toLocalString_withopt :
    Js.js_string Js.t -> Js.Unsafe.any -> Js.js_string Js.t Js.meth

  method toString : Js.js_string Js.t Js.meth
  method toString_base : int -> Js.js_string Js.t Js.meth
  method valueOf : bigInt Js.t Js.meth
end

class type decimal_ct = object
  method n : bigInt Js.t Js.prop
  method d : bigInt Js.t Js.prop
end

val bigInt : 'a Js.t -> bigInt Js.t

type money_jsoo = bigInt Js.t

val money_to_jsoo : money -> money_jsoo
val money_of_jsoo : money_jsoo -> money

type integer_jsoo = bigInt Js.t

val integer_to_jsoo : integer -> integer_jsoo
val integer_of_jsoo : integer_jsoo -> integer

type decimal_jsoo = decimal_ct Js.t

val decimal_to_jsoo : decimal -> decimal_jsoo
val decimal_of_jsoo : decimal_jsoo -> decimal

type date_jsoo = Dates_calc_jsoo.date_jsoo

val date_to_jsoo : date -> date_jsoo
val date_of_jsoo : date_jsoo -> date

type date_rounding_jsoo = Dates_calc_jsoo.date_rounding_jsoo

val date_rounding_to_jsoo : date_rounding -> date_rounding_jsoo
val date_rounding_of_jsoo : date_rounding_jsoo -> date_rounding

type duration_jsoo = Dates_calc_jsoo.period_jsoo

val duration_to_jsoo : duration -> duration_jsoo
val duration_of_jsoo : duration_jsoo -> duration

module Optional : sig
  type 'a t = 'a Catala_runtime.Optional.t = Absent | Present of 'a

  class type ['a] ct = object
    method _Absent : unit_jsoo Js.optdef Js.prop
    method _Present : 'a Js.optdef Js.prop
  end

  type 'a jsoo = 'a ct Js.t

  val to_jsoo : ('a -> 'a_jsoo) -> 'a t -> 'a_jsoo jsoo
  val of_jsoo : ('a_jsoo -> 'a) -> 'a_jsoo jsoo -> 'a t
end

type io_input_jsoo = Js.js_string Js.t

val io_input_to_jsoo : io_input -> io_input_jsoo
val io_input_of_jsoo : io_input_jsoo -> io_input

class type io_log_ct = object
  method io_input_ : io_input_jsoo Js.prop
  method io_output_ : bool Js.t Js.prop
end

type io_log_jsoo = io_log_ct Js.t

val io_log_to_jsoo : io_log -> io_log_jsoo
val io_log_of_jsoo : io_log_jsoo -> io_log

class type code_location_ct = object
  method fileName : Js.js_string Js.t Js.prop
  method startLine : int Js.prop
  method endLine : int Js.prop
  method startColumn : int Js.prop
  method endColumn : int Js.prop
  method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
end

type code_location_jsoo = code_location_ct Js.t

val code_location_to_jsoo : code_location -> code_location_jsoo
val code_location_of_jsoo : code_location_jsoo -> code_location

type error_jsoo = Js.js_string Js.t

val error_to_jsoo : error -> error_jsoo
val error_of_jsoo : error_jsoo -> error

(** Wrapper for the {!type: Runtime.raw_event} -- directly collected during the
    program execution.*)
class type raw_event = object
  method eventType : Js.js_string Js.t Js.prop
  (** There is four type of raw log events:

      - 'BeginCall' is emitted when a function or a subscope is called.
      - 'EndCall' is emitted when a function or a subscope is exited.
      - 'VariableDefinition' is emitted when a variable or a function is
        defined.
      - 'DecisionTaken' stores the information about the source position of the
        event. *)

  method information : Js.js_string Js.t Js.js_array Js.t Js.prop
  (** Represents information about a name in the code -- i.e. variable name,
      subscope name, etc...

      It's a list of strings with a length varying from 2 to 3, where:

      - the first string is the name of the current scope -- starting with a
        capitalized letter [Scope_name],
      - the second string is either: the name of a scope variable or, the name
        of a subscope input variable -- [a_subscope_var.input_var]
      - the third string is either: a subscope name (starting with a capitalized
        letter [Subscope_name] or, the [input] (resp. [output]) string -- which
        corresponds to the input (resp. the output) of a function. *)

  method sourcePosition : code_location_jsoo Js.optdef Js.prop

  method loggedIOJson : Js.js_string Js.t Js.prop
  (** Serialzed [Runtime.io_log] corresponding to a `VariableDefinition` raw
      event. *)

  method loggedValueJson : Js.js_string Js.t Js.prop
  (** Serialized [Runtime.runtime_value] corresponding to a 'VariableDefinition'
      raw event. *)
end

(** Wrapper for the {!type: Runtime.event} -- structured log event parsed from
    the {!raw_event} ones. *)
class type event = object
  method data : Js.js_string Js.t Js.prop
  (** Serialized [Runtime.event]. *)
end

class type event_manager = object
  method resetLog : unit Js.meth
  method retrieveEvents : event Js.t Js.js_array Js.t Js.meth
  method retrieveRawEvents : raw_event Js.t Js.js_array Js.t Js.meth
end

val event_manager : event_manager Js.t
(** JS object usable to retrieve and reset log events. *)

val execute_or_throw_error : (unit -> 'a) -> 'a
(** [execute_or_throw_error f] calls [f ()] and propagates the
    {!Catala_runtime.NoValue}, {!Catala_runtime.Conflict}
    {!Catala_runtime.AssertionFailed} exceptions by raising a JS error if
    needed.*)
