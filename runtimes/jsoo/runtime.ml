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

class type source_position = object
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
  method sourcePosition : source_position Js.t Js.optdef Js.prop
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
  let d = Js.to_string d in
  let d =
    if String.contains d 'T' then d |> String.split_on_char 'T' |> List.hd
    else d
  in
  match String.split_on_char '-' d with
  | [year; month; day] ->
    R_ocaml.date_of_numbers (int_of_string year) (int_of_string month)
      (int_of_string day)
  | _ -> failwith "date_of_jsoo: invalid date"

let date_to_jsoo d = Js.string @@ R_ocaml.date_to_string d

class type event_manager = object
  method resetLog : (unit, unit) Js.meth_callback Js.meth

  method retrieveEvents :
    (unit, event Js.t Js.js_array Js.t) Js.meth_callback Js.meth

  method retrieveRawEvents :
    (unit, raw_event Js.t Js.js_array Js.t) Js.meth_callback Js.meth
end

let event_manager : event_manager Js.t =
  object%js
    method resetLog = Js.wrap_meth_callback R_ocaml.reset_log

    method retrieveEvents =
      Js.wrap_meth_callback (fun () ->
          Js.array
            (Array.of_list
               (R_ocaml.retrieve_log ()
               |> R_ocaml.EventParser.parse_raw_events
               |> List.map (fun event ->
                      object%js
                        val mutable data =
                          event
                          |> R_ocaml.yojson_of_event
                          |> Yojson.Safe.to_string
                          |> Js.string
                      end))))

    method retrieveRawEvents =
      Js.wrap_meth_callback (fun () ->
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
                             | VariableDefinition (info, _, _) ->
                               List.map Js.string info
                             | DecisionTaken _ -> []))

                      val mutable loggedIOJson =
                        match evt with
                        | VariableDefinition (_, io, _) ->
                          io
                          |> R_ocaml.yojson_of_io_log
                          |> Yojson.Safe.to_string
                          |> Js.string
                        | EndCall _ | BeginCall _ | DecisionTaken _ ->
                          "unavailable" |> Js.string

                      val mutable loggedValueJson =
                        (match evt with
                        | VariableDefinition (_, _, v) -> v
                        | EndCall _ | BeginCall _ | DecisionTaken _ ->
                          R_ocaml.unembeddable ())
                        |> R_ocaml.yojson_of_runtime_value
                        |> Yojson.Safe.to_string
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
                                 Js.array
                                   (Array.of_list
                                      (List.map Js.string pos.law_headings))
                            end)
                        | _ -> Js.undefined
                    end)
                  (R_ocaml.retrieve_log ()))))
  end

let execute_or_throw_error f =
  let throw_error (descr : string) (pos : R_ocaml.source_position) =
    let msg =
      Js.string
        (Format.asprintf "%s in file %s, position %d:%d--%d:%d." descr
           pos.filename pos.start_line pos.start_column pos.end_line
           pos.end_column)
    in
    Js.Js_error.raise_
      (Js.Js_error.of_error
         (object%js
            val mutable name = Js.string "CatalaError"
            val mutable message = msg
            val mutable stack = Js.Optdef.empty
            method toString = msg
         end))
  in
  try f () with
  | R_ocaml.NoValueProvided pos ->
    throw_error
      "No rule applies in the given context to give a value to the variable" pos
  | R_ocaml.ConflictError pos ->
    throw_error
      "A conflict happened between two rules giving a value to the variable" pos
  | R_ocaml.AssertionFailed pos ->
    throw_error "A failure happened in the assertion" pos
