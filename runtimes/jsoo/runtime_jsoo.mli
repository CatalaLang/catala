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

type date_jsoo = Js.date Js.t

val date_to_jsoo : date -> date_jsoo
val date_of_jsoo : date_jsoo -> date

type error_jsoo = Js.js_string Js.t

val error_to_jsoo : error -> error_jsoo
val error_of_jsoo : error_jsoo -> error

(** {1 Log events} *)

(** Information about the position of the log inside the Catala source file. *)
class type code_location = object
  method fileName : Js.js_string Js.t Js.prop
  method startLine : int Js.prop
  method endLine : int Js.prop
  method startColumn : int Js.prop
  method endColumn : int Js.prop
  method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
end

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

  method sourcePosition : code_location Js.t Js.optdef Js.prop

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

(** {1 Duration} *)

(** Simple JSOO wrapper around {!type: Runtime.duration}.*)
class type duration = object
  method years : int Js.readonly_prop
  method months : int Js.readonly_prop
  method days : int Js.readonly_prop
end

val duration_of_js : duration Js.t -> Catala_runtime.duration
val duration_to_js : Catala_runtime.duration -> duration Js.t

(** {1 Date conversion} *)

(** Date values are encoded to a string in the
    {{:https://www.iso.org/iso-8601-date-and-time-format.html} ISO8601 format}:
    'YYYY-MM-DD'. *)

val date_of_js : Js.js_string Js.t -> Catala_runtime.date
val date_to_js : Catala_runtime.date -> Js.js_string Js.t

(** {1 Error management} *)

val position_of_js : code_location Js.t -> Catala_runtime.code_location
val position_to_js : Catala_runtime.code_location -> code_location Js.t

val execute_or_throw_error : (unit -> 'a) -> 'a
(** [execute_or_throw_error f] calls [f ()] and propagates the
    {!Catala_runtime.NoValue}, {!Catala_runtime.Conflict}
    {!Catala_runtime.AssertionFailed} exceptions by raising a JS error if
    needed.*)
