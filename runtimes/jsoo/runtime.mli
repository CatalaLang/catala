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

(** Runtime used by generated web API via
    {{:http://ocsigen.org/js_of_ocaml/latest/manual/overview} [js_of_ocaml]}. *)

open Js_of_ocaml

(** {1 Log events} *)

class type source_position =
  object
    method fileName : Js.js_string Js.t Js.prop
    method startLine : int Js.prop
    method endLine : int Js.prop
    method startColumn : int Js.prop
    method endColumn : int Js.prop
    method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
  end

class type raw_event =
  object
    method eventType : Js.js_string Js.t Js.prop
    method information : Js.js_string Js.t Js.js_array Js.t Js.prop
    method sourcePosition : source_position Js.t Js.optdef Js.prop
    method loggedValueJson : Js.js_string Js.t Js.prop
  end

class type event =
  object
    method data : Js.js_string Js.t Js.prop
  end

class type ['a] event_manager =
  object
    method resetLog : ('a, unit -> unit) Js.meth_callback Js.meth

    method retrieveEvents :
      ('a, unit -> event Js.t Js.js_array Js.t) Js.meth_callback Js.meth

    method retrieveRawEvents :
      ('a, unit -> raw_event Js.t Js.js_array Js.t) Js.meth_callback Js.meth
  end

val event_manager : unit event_manager Js.t
(** Composable object to retrieve and reset log events. *)

(** {1 Duration} *)

class type duration =
  object
    method years : int Js.readonly_prop
    method months : int Js.readonly_prop
    method days : int Js.readonly_prop
  end

val duration_of_jsoo : duration Js.t -> Runtime_ocaml.Runtime.duration
val duration_to_jsoo : Runtime_ocaml.Runtime.duration -> duration Js.t

(** {1 Date conversion} *)

val date_of_jsoo : Js.date Js.t -> Runtime_ocaml.Runtime.date
val date_to_jsoo : Runtime_ocaml.Runtime.date -> Js.date Js.t
