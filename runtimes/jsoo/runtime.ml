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
module R_ocaml = Runtime_ocaml.Runtime

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

class type duration =
  object
    method years : int Js.readonly_prop
    method months : int Js.readonly_prop
    method days : int Js.readonly_prop
  end

let duration_of_jsoo d =
  R_ocaml.duration_of_numbers d##.years d##.months d##.days

let duration_to_jsoo d =
  let years, months, days = R_ocaml.duration_to_years_months_days d in
  object%js
    val years = years
    val months = months
    val days = days
  end

let date_of_jsoo d =
  R_ocaml.date_of_numbers d##getUTCFullYear d##getUTCMonth d##getUTCDate

let date_to_jsoo d =
  let years = R_ocaml.integer_to_int (R_ocaml.year_of_date d) in
  let months = R_ocaml.integer_to_int (R_ocaml.month_number_of_date d) in
  let days = R_ocaml.integer_to_int (R_ocaml.day_of_month_of_date d) in
  new%js Js.date_day years months days

class type ['a] event_manager =
  object
    method resetLog : ('a, unit -> unit) Js.meth_callback Js.meth

    method retrieveEvents :
      ('a, unit -> event Js.t Js.js_array Js.t) Js.meth_callback Js.meth

    method retrieveRawEvents :
      ('a, unit -> raw_event Js.t Js.js_array Js.t) Js.meth_callback Js.meth
  end

let event_manager : unit event_manager Js.t =
  object%js
    method resetLog = Js.wrap_callback R_ocaml.reset_log

    method retrieveEvents =
      Js.wrap_callback (fun () ->
          Js.array
            (Array.of_list
               (R_ocaml.retrieve_log () |> R_ocaml.EventParser.parse_raw_events
               |> List.map (fun event ->
                      object%js
                        val mutable data =
                          event |> R_ocaml.yojson_of_event
                          |> Yojson.Safe.to_string |> Js.string
                      end))))

    method retrieveRawEvents =
      Js.wrap_callback (fun () ->
          Js.array
            (Array.of_list
               (List.map
                  (fun evt ->
                    object%js
                      val mutable eventType =
                        Js.string
                          (match evt with
                          | R_ocaml.BeginCall _ -> "Begin call"
                          | EndCall _ -> "End call"
                          | VariableDefinition _ -> "Variable definition"
                          | DecisionTaken _ -> "Decision taken")

                      val mutable information =
                        Js.array
                          (Array.of_list
                             (match evt with
                             | BeginCall info
                             | EndCall info
                             | VariableDefinition (info, _) ->
                               List.map Js.string info
                             | DecisionTaken _ -> []))

                      val mutable loggedValueJson =
                        (match evt with
                        | VariableDefinition (_, v) -> v
                        | EndCall _ | BeginCall _ | DecisionTaken _ ->
                          R_ocaml.unembeddable ())
                        |> R_ocaml.yojson_of_runtime_value
                        |> Yojson.Safe.to_string |> Js.string

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
                                 Js.array
                                   (Array.of_list
                                      (List.map Js.string pos.law_headings))
                            end)
                        | _ -> Js.undefined
                    end)
                  (R_ocaml.retrieve_log ()))))
  end
