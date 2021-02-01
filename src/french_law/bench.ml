(* This file is part of the French law library, a collection of functions for computing French taxes
   and benefits derived from Catala programs. Copyright (C) 2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module AF = French_law.Api.Allocations_familiales
open Catala.Runtime

let random_children (id : int) =
  {
    AF.d_identifiant = integer_of_int id;
    d_remuneration_mensuelle = money_of_units_integers (Random.int 2000);
    d_date_de_naissance =
      date_of_numbers (2020 - Random.int 22) (1 + Random.int 12) (1 + Random.int 28);
    d_garde_alternee =
      ( match Random.int 3 with
      | 0 -> AF.NonGardeUnique ()
      | 1 -> AF.OuiPartageAllocations ()
      | _ -> AF.OuiAllocataireUnique () );
    d_pris_en_charge_par_services_sociaux =
      ( match Random.int 3 with
      | 0 -> AF.OuiAllocationVerseeALaFamille ()
      | 1 -> AF.OuiAllocationVerseeAuxServicesSociaux ()
      | _ -> AF.NonPriseEnChargeFamille () );
  }

let format_residence (fmt : Format.formatter) (r : AF.collectivite) : unit =
  Format.fprintf fmt "%s"
    ( match r with
    | AF.Metropole _ -> "Métropole"
    | AF.Guyane _ -> "Guyane"
    | AF.Guadeloupe _ -> "Guadeloupe"
    | AF.Martinique _ -> "Martinique"
    | AF.LaReunion _ -> "La Réunion"
    | AF.SaintBarthelemy _ -> "Saint Barthélemy"
    | AF.SaintPierreEtMiquelon _ -> "Saint Pierre et Miquelon"
    | AF.SaintMartin _ -> "Saint Martin"
    | AF.Mayotte _ -> "Mayotte" )

let format_garde_alternee (fmt : Format.formatter) (g : AF.garde_alternee) : unit =
  Format.fprintf fmt "%s"
    ( match g with
    | AF.NonGardeUnique _ -> "Non"
    | AF.OuiPartageAllocations _ -> "Oui, allocations partagée"
    | AF.OuiAllocataireUnique _ -> "Oui, allocataire unique" )

let format_services_sociaux (fmt : Format.formatter) (g : AF.prise_en_charge_service_sociaux) : unit
    =
  Format.fprintf fmt "%s"
    ( match g with
    | AF.OuiAllocationVerseeALaFamille _ -> "Oui, allocations versée à la famille"
    | AF.OuiAllocationVerseeAuxServicesSociaux _ -> "Oui, allocations versée aux services sociaux"
    | AF.NonPriseEnChargeFamille _ -> "Non" )

let num_successful = ref 0

let total_amount = ref 0.

let run_test () =
  let num_children = Random.int 7 in
  let children = Array.init num_children random_children in
  let income = Random.int 100000 in
  let current_date = CalendarLib.Date.make 2020 05 01 in
  let residence = if Random.bool () then AF.Metropole () else AF.Guadeloupe () in
  try
    let amount =
      French_law.Api.compute_allocations_familiales ~current_date ~income ~residence ~children
    in
    incr num_successful;
    total_amount := Float.add !total_amount amount
  with
  | EmptyError ->
      Format.printf "Empty error reached!\n%a\nincome: %d\ncurrent_date: %s\nresidence: %a\n"
        (Format.pp_print_list (fun fmt child ->
             Format.fprintf fmt
               "Child %d:\n\
               \  income: %.2f\n\
               \  birth date: %s\n\
               \  garde alternée: %a\n\
               \  services sociaux: %a"
               (integer_to_int child.AF.d_identifiant)
               (money_to_float child.AF.d_remuneration_mensuelle)
               (CalendarLib.Printer.Date.to_string
                  (date_to_calendar_date child.AF.d_date_de_naissance))
               format_garde_alternee child.AF.d_garde_alternee format_services_sociaux
               child.AF.d_pris_en_charge_par_services_sociaux))
        (Array.to_list children) income
        (CalendarLib.Printer.Date.to_string current_date)
        format_residence residence;
      exit (-1)
  | AssertionFailed -> ()

let bench =
  Random.init (int_of_float (Unix.time ()));
  let num_iter = 100000 in
  let _ =
    Benchmark.latency1 ~style:Auto ~name:"Allocations familiales" (Int64.of_int num_iter) run_test
      ()
  in
  Printf.printf
    "Successful computations: %d (%.2f%%)\nTotal benefits awarded: %.2f€ (mean %.2f€)\n"
    !num_successful
    (Float.mul (Float.div (float_of_int !num_successful) (float_of_int num_iter)) 100.)
    !total_amount
    (Float.div !total_amount (float_of_int !num_successful))
