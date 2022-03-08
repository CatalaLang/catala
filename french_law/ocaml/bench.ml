(* This file is part of the French law library, a collection of functions for
   computing French taxes and benefits derived from Catala programs. Copyright
   (C) 2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

module AF = Api.Allocations_familiales
open Runtime

let random_children (id : int) =
  {
    AF.d_identifiant = integer_of_int id;
    d_remuneration_mensuelle = money_of_units_int (Random.int 2000);
    d_date_de_naissance =
      date_of_numbers
        (2020 - Random.int 22)
        (1 + Random.int 12)
        (1 + Random.int 28);
    d_prise_en_charge =
      (match Random.int 5 with
      | 0 -> AF.EffectiveEtPermanente ()
      | 1 -> AF.GardeAlterneePartageAllocations ()
      | 2 -> AF.GardeAlterneeAllocataireUnique ()
      | 3 -> AF.ServicesSociauxAllocationVerseeALaFamille ()
      | _ -> AF.ServicesSociauxAllocationVerseeAuxServicesSociaux ());
    d_a_deja_ouvert_droit_aux_allocations_familiales = true;
  }

let format_residence (fmt : Format.formatter) (r : AF.collectivite) : unit =
  Format.fprintf fmt "%s"
    (match r with
    | AF.Metropole _ -> "Métropole"
    | AF.Guyane _ -> "Guyane"
    | AF.Guadeloupe _ -> "Guadeloupe"
    | AF.Martinique _ -> "Martinique"
    | AF.LaReunion _ -> "La Réunion"
    | AF.SaintBarthelemy _ -> "Saint Barthélemy"
    | AF.SaintPierreEtMiquelon _ -> "Saint Pierre et Miquelon"
    | AF.SaintMartin _ -> "Saint Martin"
    | AF.Mayotte _ -> "Mayotte")

let format_prise_en_charge (fmt : Format.formatter) (g : AF.prise_en_charge) :
    unit =
  Format.fprintf fmt "%s"
    (match g with
    | AF.EffectiveEtPermanente _ -> "Effective et permanente"
    | AF.GardeAlterneePartageAllocations _ ->
        "Garde alternée, allocations partagée"
    | AF.GardeAlterneeAllocataireUnique _ ->
        "Garde alternée, allocataire unique"
    | AF.ServicesSociauxAllocationVerseeALaFamille _ ->
        "Oui, allocations versée à la famille"
    | AF.ServicesSociauxAllocationVerseeAuxServicesSociaux _ ->
        "Oui, allocations versée aux services sociaux")

let num_successful = ref 0
let total_amount = ref 0.

let run_test () =
  let num_children = Random.int 7 in
  let children = Array.init num_children random_children in
  let income = Random.int 100000 in
  let current_date = Runtime.date_of_numbers 2020 05 01 in
  let residence =
    let x = Random.int 2 in
    match x with
    | 0 -> AF.Metropole ()
    | 1 -> AF.Guadeloupe ()
    | _ -> AF.Mayotte ()
  in
  try
    let amount =
      Api.compute_allocations_familiales ~current_date ~income ~residence
        ~children ~is_parent:true ~fills_title_I:true
        ~had_rights_open_before_2012:(Random.bool ())
    in
    incr num_successful;
    total_amount := Float.add !total_amount amount
  with
  | (NoValueProvided _ | ConflictError) as err ->
      Format.printf "%s\n%a\nincome: %d\ncurrent_date: %s\nresidence: %a\n"
        (match err with
        | NoValueProvided _ -> "No value provided somewhere!"
        | ConflictError -> "Conflict error!"
        | _ -> failwith "impossible")
        (Format.pp_print_list (fun fmt child ->
             Format.fprintf fmt
               "Child %d:\n\
               \  income: %.2f\n\
               \  birth date: %s\n\
               \  prise en charge: %a"
               (integer_to_int child.AF.d_identifiant)
               (money_to_float child.AF.d_remuneration_mensuelle)
               (Runtime.date_to_string child.AF.d_date_de_naissance)
               format_prise_en_charge child.AF.d_prise_en_charge))
        (Array.to_list children) income
        (Runtime.date_to_string current_date)
        format_residence residence;
      exit (-1)
  | AssertionFailed -> ()

let bench =
  Random.init (int_of_float (Unix.time ()));
  let num_iter = 10000 in
  let _ =
    Benchmark.latency1 ~style:Auto ~name:"Allocations familiales"
      (Int64.of_int num_iter) run_test ()
  in
  Printf.printf
    "Successful computations: %d (%.2f%%)\n\
     Total benefits awarded: %.2f€ (mean %.2f€)\n"
    !num_successful
    (Float.mul
       (Float.div (float_of_int !num_successful) (float_of_int num_iter))
       100.)
    !total_amount
    (Float.div !total_amount (float_of_int !num_successful))
