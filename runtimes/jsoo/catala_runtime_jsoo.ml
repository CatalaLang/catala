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

open Js_of_ocaml
open Catala_runtime

type unit_jsoo = unit

let unit_to_jsoo = Fun.id
let unit_of_jsoo = Fun.id

type bool_jsoo = bool Js.t

let bool_to_jsoo = Js.bool
let bool_of_jsoo = Js.to_bool

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

let bigInt (x : 'a Js.t) : bigInt Js.t = Js.Unsafe.global##_BigInt x

type money_jsoo = bigInt Js.t

let money_to_jsoo z = bigInt (Js.string (Z.to_string z))
let money_of_jsoo js = Z.of_string (Js.to_string js##toString)

type integer_jsoo = bigInt Js.t

let integer_to_jsoo z = bigInt (Js.string (Z.to_string z))
let integer_of_jsoo js = Z.of_string (Js.to_string js##toString)

type decimal_jsoo = decimal_ct Js.t

let decimal_to_jsoo q =
  object%js
    val mutable n = bigInt (Js.string (Z.to_string (Q.num q)))
    val mutable d = bigInt (Js.string (Z.to_string (Q.den q)))
  end

let decimal_of_jsoo js =
  Q.make
    (Z.of_string (Js.to_string js##.n##toString))
    (Z.of_string (Js.to_string js##.d##toString))

type date_jsoo = Dates_calc_jsoo.date_jsoo

let date_to_jsoo = Dates_calc_jsoo.date_to_jsoo
let date_of_jsoo = Dates_calc_jsoo.date_of_jsoo

type date_rounding_jsoo = Dates_calc_jsoo.date_rounding_jsoo

let date_rounding_to_jsoo = Dates_calc_jsoo.date_rounding_to_jsoo
let date_rounding_of_jsoo = Dates_calc_jsoo.date_rounding_of_jsoo

type duration_jsoo = Dates_calc_jsoo.period_jsoo

let duration_to_jsoo = Dates_calc_jsoo.period_to_jsoo
let duration_of_jsoo = Dates_calc_jsoo.period_of_jsoo

module Optional = struct
  include Optional

  class type ['a] ct = object
    method _Absent : unit_jsoo Js.optdef Js.prop
    method _Present : 'a Js.optdef Js.prop
  end

  type 'a jsoo = 'a ct Js.t

  let to_jsoo a_to_jsoo x =
    match x with
    | Absent ->
      object%js
        val mutable _Absent = Js.def ()
        val mutable _Present = Js.undefined
      end
    | Present a ->
      object%js
        val mutable _Absent = Js.undefined
        val mutable _Present = Js.def (a_to_jsoo a)
      end

  let of_jsoo a_of_jsoo js =
    match
      Js.Optdef.to_option js##._Absent, Js.Optdef.to_option js##._Present
    with
    | Some _, _ -> Absent
    | _, Some a -> Present (a_of_jsoo a)
    | _ -> invalid_arg "unknown case"
end

type io_input_jsoo = Js.js_string Js.t

let io_input_to_jsoo x =
  Js.string
  @@
  match x with
  | NoInput -> "NoInput"
  | OnlyInput -> "OnlyInput"
  | Reentrant -> "Reentrant"

let io_input_of_jsoo js =
  match Js.to_string js with
  | "NoInput" -> NoInput
  | "OnlyInput" -> OnlyInput
  | "Reentrant" -> Reentrant
  | s -> invalid_arg (Format.sprintf "unknown case in enum: %S" s)

class type io_log_ct = object
  method io_input_ : io_input_jsoo Js.prop
  method io_output_ : bool Js.t Js.prop
end

type io_log_jsoo = io_log_ct Js.t

let io_log_to_jsoo x =
  object%js
    val mutable io_input_ = io_input_to_jsoo x.io_input
    val mutable io_output_ = Js.bool x.io_output
  end

let io_log_of_jsoo js =
  {
    io_input = io_input_of_jsoo js##.io_input_;
    io_output = Js.to_bool js##.io_output_;
  }

class type code_location_ct = object
  method fileName : Js.js_string Js.t Js.prop
  method startLine : int Js.prop
  method endLine : int Js.prop
  method startColumn : int Js.prop
  method endColumn : int Js.prop
  method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
end

type code_location_jsoo = code_location_ct Js.t

let code_location_to_jsoo (pos : code_location) : code_location_jsoo =
  object%js
    val mutable fileName = Js.string pos.filename
    val mutable startLine = pos.start_line
    val mutable endLine = pos.end_line
    val mutable startColumn = pos.start_column
    val mutable endColumn = pos.end_column

    val mutable lawHeadings =
      Array.of_list pos.law_headings |> Array.map Js.string |> Js.array
  end

let code_location_of_jsoo (jpos : code_location_jsoo) : code_location =
  {
    filename = Js.to_string jpos##.fileName;
    start_line = jpos##.startLine;
    start_column = jpos##.startColumn;
    end_line = jpos##.endLine;
    end_column = jpos##.endColumn;
    law_headings =
      Js.to_array jpos##.lawHeadings |> Array.map Js.to_string |> Array.to_list;
  }

type error_jsoo = Js.js_string Js.t

let error_to_jsoo e = Js.string (error_to_string e)
let error_of_jsoo js = error_of_string (Js.to_string js)

class type raw_event = object
  method eventType : Js.js_string Js.t Js.prop
  method information : Js.js_string Js.t Js.js_array Js.t Js.prop
  method sourcePosition : code_location_jsoo Js.optdef Js.prop
  method loggedIOJson : Js.js_string Js.t Js.prop
  method loggedValueJson : Js.js_string Js.t Js.prop
end

class type event = object
  method data : Js.js_string Js.t Js.prop
end

class type event_manager = object
  method resetLog : unit Js.meth
  method retrieveEvents : event Js.t Js.js_array Js.t Js.meth
  method retrieveRawEvents : raw_event Js.t Js.js_array Js.t Js.meth
end

let event_manager : event_manager Js.t =
  object%js (_self)
    method resetLog = reset_log ()

    method retrieveEvents =
      retrieve_log ()
      |> EventParser.parse_raw_events
      |> List.map (fun event ->
          object%js
            val mutable data = event |> Json.event |> Js.string
          end)
      |> Array.of_list
      |> Js.array

    method retrieveRawEvents =
      let evt_to_js evt =
        (* FIXME: ideally this could be just a Json.parse (Json.event foo) ? *)
        object%js
          val mutable eventType =
            (match evt with
              | BeginCall _ -> "Begin call"
              | EndCall _ -> "End call"
              | VariableDefinition _ -> "Variable definition"
              | DecisionTaken _ -> "Decision taken")
            |> Js.string

          val mutable information =
            (match evt with
              | BeginCall info | EndCall info | VariableDefinition (info, _, _)
                ->
                List.map Js.string info
              | DecisionTaken _ -> [])
            |> Array.of_list
            |> Js.array

          val mutable loggedIOJson =
            match evt with
            | VariableDefinition (_, io, _) -> io |> Json.io_log |> Js.string
            | EndCall _ | BeginCall _ | DecisionTaken _ ->
              "unavailable" |> Js.string

          val mutable loggedValueJson =
            (match evt with
              | VariableDefinition (_, _, v) -> v
              | EndCall _ | BeginCall _ | DecisionTaken _ -> unembeddable ())
            |> Json.runtime_value
            |> Js.string

          val mutable sourcePosition =
            match evt with
            | DecisionTaken pos ->
              Js.def
                (object%js
                   val mutable fileName = Js.string pos.filename
                   val mutable startLine = pos.start_line
                   val mutable endLine = pos.end_line
                   val mutable startColumn = pos.start_column
                   val mutable endColumn = pos.end_column

                   val mutable lawHeadings =
                     List.map Js.string pos.law_headings
                     |> Array.of_list
                     |> Js.array
                end)
            | _ -> Js.undefined
        end
      in
      retrieve_log () |> List.map evt_to_js |> Array.of_list |> Js.array
  end

let execute_or_throw_error f =
  try f ()
  with Error _ as exc ->
    let msg = Js.string (Printexc.to_string exc) in
    Js.Js_error.raise_
      (Js.Js_error.of_error
         (object%js
            val mutable name = Js.string "CatalaError"
            val mutable message = msg
            val mutable stack = Js.Optdef.empty
            method toString = msg
         end))

let () =
  Js.export_all
    (object%js
       val eventsManager = event_manager
    end)
