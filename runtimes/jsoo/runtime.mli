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
    wrapper around the {!module: Runtime_ocaml.Runtime}. *)

open Js_of_ocaml

(** {1 Log events} *)

(** Information about the position of the log inside the Catala source file. *)
class type source_position = object
  method fileName : Js.js_string Js.t Js.prop
  method startLine : int Js.prop
  method endLine : int Js.prop
  method startColumn : int Js.prop
  method endColumn : int Js.prop
  method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
end

(** Wrapper for the {!type: Runtime_ocaml.Runtime.raw_event} -- directly
    collected during the program execution.*)
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

  method sourcePosition : source_position Js.t Js.optdef Js.prop

  method loggedIOJson : Js.js_string Js.t Js.prop
  (** Serialzed [Runtime_ocaml.Runtime.io_log] corresponding to a
      `VariableDefinition` raw event. *)

  method loggedValueJson : Js.js_string Js.t Js.prop
  (** Serialized [Runtime_ocaml.Runtime.runtime_value] corresponding to a
      'VariableDefinition' raw event. *)
end

(** Wrapper for the {!type: Runtime_ocaml.Runtime.event} -- structured log event
    parsed from the {!raw_event} ones. *)
class type event = object
  method data : Js.js_string Js.t Js.prop
  (** Serialized [Runtime_ocaml.Runtime.event]. *)
end

class type event_manager = object
  method resetLog : (unit, unit) Js.meth_callback Js.meth

  method retrieveEvents :
    (unit, event Js.t Js.js_array Js.t) Js.meth_callback Js.meth

  method retrieveRawEvents :
    (unit, raw_event Js.t Js.js_array Js.t) Js.meth_callback Js.meth
end

val event_manager : event_manager Js.t
(** JS object usable to retrieve and reset log events. *)

(** {1 Duration} *)

(** Simple JSOO wrapper around {!type: Runtime_ocaml.Runtime.duration}.*)
class type duration = object
  method years : int Js.readonly_prop
  method months : int Js.readonly_prop
  method days : int Js.readonly_prop
end

val duration_of_jsoo : duration Js.t -> Runtime_ocaml.Runtime.duration
val duration_to_jsoo : Runtime_ocaml.Runtime.duration -> duration Js.t

(** {1 Date conversion} *)

(** Date values are encoded to a string in the
    {{:https://www.iso.org/iso-8601-date-and-time-format.html} ISO8601 format}:
    'YYYY-MM-DD'. *)

val date_of_jsoo : Js.js_string Js.t -> Runtime_ocaml.Runtime.date
val date_to_jsoo : Runtime_ocaml.Runtime.date -> Js.js_string Js.t

(** {1 Error management} *)

val execute_or_throw_error : (unit -> 'a) -> 'a
(** [execute_or_throw_error f] calls [f ()] and propagates the
    {!Runtime_ocaml.Runtime.NoValueProvided},
    {!Runtime_ocaml.Runtime.ConflictError}
    {!Runtime_ocaml.Runtime.AssertionFailed} exceptions by raising a JS error if
    needed.*)
