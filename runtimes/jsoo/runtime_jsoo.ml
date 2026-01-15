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
module R_ocaml = Catala_runtime

type unit_jsoo = unit

let unit_to_jsoo = Fun.id
let unit_od_jsoo = Fun.id

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
    val n = bigInt (Js.string (Z.to_string (Q.num q)))
    val d = bigInt (Js.string (Z.to_string (Q.den q)))
  end

let decimal_of_jsoo js =
  Q.make
    (Z.of_string (Js.to_string js##.n##toString))
    (Z.of_string (Js.to_string js##.d##toString))

type date_jsoo = Js.date Js.t

let date_to_jsoo d =
  let cs = Js.Unsafe.global##._Date in
  new%js cs (Js.string (Format.asprintf "%a" Dates_calc.format_date d))

let date_of_jsoo js =
  let s = Js.to_string js##toISOString in
  Dates_calc.date_of_string (String.sub s 0 10)

type error_jsoo = Js.js_string Js.t

let error_to_jsoo e = Js.string (R_ocaml.error_to_string e)
let error_of_jsoo js = R_ocaml.error_of_string (Js.to_string js)

class type code_location = object
  method fileName : Js.js_string Js.t Js.prop
  method startLine : int Js.prop
  method endLine : int Js.prop
  method startColumn : int Js.prop
  method endColumn : int Js.prop
  method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
end

class type raw_event = object
  method eventType : Js.js_string Js.t Js.prop
  method information : Js.js_string Js.t Js.js_array Js.t Js.prop
  method sourcePosition : code_location Js.t Js.optdef Js.prop
  method loggedIOJson : Js.js_string Js.t Js.prop
  method loggedValueJson : Js.js_string Js.t Js.prop
end

class type event = object
  method data : Js.js_string Js.t Js.prop
end

class type duration = object
  method years : int Js.readonly_prop
  method months : int Js.readonly_prop
  method days : int Js.readonly_prop
end

let duration_of_js d = R_ocaml.duration_of_numbers d##.years d##.months d##.days

let duration_to_js d =
  let years, months, days = R_ocaml.duration_to_years_months_days d in
  object%js
    val years = years
    val months = months
    val days = days
  end

let date_of_js d =
  let d = Js.to_string d in
  let d =
    if String.contains d 'T' then d |> String.split_on_char 'T' |> List.hd
    else d
  in
  let fail () = failwith "date_of_js: invalid date" in
  match String.split_on_char '-' d with
  | [year; month; day] -> (
    try
      R_ocaml.date_of_numbers (int_of_string year) (int_of_string month)
        (int_of_string day)
    with Failure _ -> fail ())
  | _ -> fail ()

let date_to_js d = Js.string @@ R_ocaml.date_to_string d

let position_of_js (jpos : code_location Js.t) : R_ocaml.code_location =
  {
    R_ocaml.filename = Js.to_string jpos##.fileName;
    start_line = jpos##.startLine;
    start_column = jpos##.startColumn;
    end_line = jpos##.endLine;
    end_column = jpos##.endColumn;
    law_headings =
      Js.to_array jpos##.lawHeadings |> Array.map Js.to_string |> Array.to_list;
  }

let position_to_js (pos : R_ocaml.code_location) : code_location Js.t =
  object%js
    val mutable fileName = Js.string pos.R_ocaml.filename
    val mutable startLine = pos.R_ocaml.start_line
    val mutable endLine = pos.R_ocaml.end_line
    val mutable startColumn = pos.R_ocaml.start_column
    val mutable endColumn = pos.R_ocaml.end_column

    val mutable lawHeadings =
      Array.of_list pos.law_headings |> Array.map Js.string |> Js.array
  end

class type event_manager = object
  method resetLog : unit Js.meth
  method retrieveEvents : event Js.t Js.js_array Js.t Js.meth
  method retrieveRawEvents : raw_event Js.t Js.js_array Js.t Js.meth
end

let event_manager : event_manager Js.t =
  object%js (_self)
    method resetLog = R_ocaml.reset_log ()

    method retrieveEvents =
      R_ocaml.retrieve_log ()
      |> R_ocaml.EventParser.parse_raw_events
      |> List.map (fun event ->
          object%js
            val mutable data = event |> R_ocaml.Json.event |> Js.string
          end)
      |> Array.of_list
      |> Js.array

    method retrieveRawEvents =
      let evt_to_js evt =
        (* FIXME: ideally this could be just a Json.parse (R_ocaml.Json.event
           foo) ? *)
        object%js
          val mutable eventType =
            (match evt with
              | R_ocaml.BeginCall _ -> "Begin call"
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
            | VariableDefinition (_, io, _) ->
              io |> R_ocaml.Json.io_log |> Js.string
            | EndCall _ | BeginCall _ | DecisionTaken _ ->
              "unavailable" |> Js.string

          val mutable loggedValueJson =
            (match evt with
              | VariableDefinition (_, _, v) -> v
              | EndCall _ | BeginCall _ | DecisionTaken _ ->
                R_ocaml.unembeddable ())
            |> R_ocaml.Json.runtime_value
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
      R_ocaml.retrieve_log () |> List.map evt_to_js |> Array.of_list |> Js.array
  end

let execute_or_throw_error f =
  try f ()
  with R_ocaml.Error _ as exc ->
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
