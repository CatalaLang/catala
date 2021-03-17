(* This file is part of the French law library, a collection of functions for computing French taxes
   and benefits derived from Catala programs. Copyright (C) 2021 Microsoft,
   Jonathan Protzenko <protz@microsoft.com>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Title17 = Title17_MotorAndOtherVehicles

open Runtime
open Js_of_ocaml

(* TODO: move into a separate library *)
module Helpers = struct
  let no_input () = raise EmptyError
  let catala_date_of_js_date (d: Js.date Js.t) =
    date_of_numbers d##getFullYear d##getMonth d##getDay
end

module H = Helpers

(* Describing input types that the JavaScript API is expected to provide *)
type js_violation = Js.js_string

let catala_violation_of_js (v: js_violation Js.t): Title17.violation_83_135 =
  match Js.to_string v with
  | "Section286_102" -> Section286_102 ()
  | "Section286_122" -> Section286_122 ()
  | "Section286_130" -> Section286_130 ()
  | "Section286_131" -> Section286_131 ()
  | "Section286_132" -> Section286_132 ()
  | "Section286_133" -> Section286_133 ()
  | "Section286_134" -> Section286_134 ()
  | "Other_83_135" -> Other_83_135 ()
  | v -> failwith (v ^ " is not a valid violation")

class type js_offense = object
  method dateOf: Js.date Js.t Js.readonly_prop
  method violation: js_violation Js.t Js.readonly_prop
end

let catala_offense_of_js (o: js_offense Js.t): Title17.offense = {
  date_of = H.catala_date_of_js_date o##.dateOf;
  violation = catala_violation_of_js o##.violation
}

class type js_defendant = object
  method priors: js_offense Js.t Js.js_array Js.t Js.readonly_prop
  method age: int Js.readonly_prop
end

let catala_defendant_of_js (o: js_defendant Js.t): Title17.defendant = {
  priors = Array.map catala_offense_of_js (Js.to_array o##.priors);
  age = integer_of_int o##.age
}

class type js_input = object
  method offense: js_offense Js.t Js.readonly_prop
  method defendant: js_defendant Js.t Js.readonly_prop
end

let catala_input_of_js (i: js_input Js.t): Title17.penalty286_83_135_in = {
  offense_in = (fun () -> catala_offense_of_js i##.offense);
  defendant_in = (fun () -> catala_defendant_of_js i##.defendant);
  max_fine_in = H.no_input;
  min_fine_in = H.no_input;
  max_days_in = H.no_input;
  priors_same_offense_in = H.no_input;
  paragraph_a_applies_in = H.no_input;
  paragraph_b_applies_in = H.no_input;
  paragraph_c_applies_in = H.no_input;
  penalty_in = H.no_input
}

(* What we return to the JavaScript code, using inheritance to encode tagged
   unions. *)
(* class type js_penalty = object *)
(* end *)

(* class type js_time_and_days = object *)
(*   inherit js_penalty *)
(*   method minFine: float Js.t Js.readonly_prop *)
(*   method maxFine: float Js.t Js.readonly_prop *)
(*   method maxDays: int Js.t Js.readonly_prop *)
(* end *)

let js_time_and_days_of_catala (p: Title17.penalty_time_and_days):
  (* js_time_and_days Js.t *)
  _ Js.t
=
  object%js
    method minfine = money_to_float p.Title17.min_fine
    method maxFine = money_to_float p.Title17.max_fine
    method maxDays = p.Title17.max_days
  end

(* class type js_fine500_or_lose_right_to_drive_until18 = object *)
(*   inherit js_penalty *)
(* end *)

(* let js_fine500_or_lose_right_to_drive_until18_of_catala (): *)
(*   js_fine500_or_lose_right_to_drive_until18 Js.t *)
(* = *)
(*   object *)
(*   end *)

let _ =
  Js.export_all (object%js
    method compute_penalties (input: js_input Js.t): _ Js.t =
      match Title17.((penalty286_83_135 (catala_input_of_js input)).penalty_out) with
      | Title17.TimeAndDays td ->
          let o = js_time_and_days_of_catala td in
          let o = Js.Unsafe.coerce o in
          o##.kind := "TimeAndDays";
          o
      | Title17.Fine500OrLoseRightToDriveUntil18 () ->
          let o = object%js end in
          let o = Js.Unsafe.coerce o in
          o##.kind := "Fine500OrLoseRightToDriveUntil18";
          o
  end)
