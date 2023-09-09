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
module Runtime = Runtime_ocaml
open Runtime

let random_children (id : int) =
  Runtime.
    {
      AF.EnfantEntree.d_identifiant = integer_of_int id;
      d_remuneration_mensuelle = money_of_units_int (Random.int 2000);
      d_date_de_naissance =
        date_of_numbers
          (2020 - Random.int 22)
          (1 + Random.int 12)
          (1 + Random.int 28);
      d_prise_en_charge =
        AF.PriseEnCharge.(
          match Random.int 5 with
          | 0 -> EffectiveEtPermanente ()
          | 1 -> GardeAlterneePartageAllocations ()
          | 2 -> GardeAlterneeAllocataireUnique ()
          | 3 -> ServicesSociauxAllocationVerseeALaFamille ()
          | _ -> ServicesSociauxAllocationVerseeAuxServicesSociaux ());
      d_a_deja_ouvert_droit_aux_allocations_familiales = Random.bool ();
      d_beneficie_titre_personnel_aide_personnelle_logement = Random.bool ();
    }

let format_residence (fmt : Format.formatter) (r : AF.Collectivite.t) : unit =
  Format.fprintf fmt "%s"
    AF.Collectivite.(
      match r with
      | Metropole _ -> "Métropole"
      | Guyane _ -> "Guyane"
      | Guadeloupe _ -> "Guadeloupe"
      | Martinique _ -> "Martinique"
      | LaReunion _ -> "La Réunion"
      | SaintBarthelemy _ -> "Saint Barthélemy"
      | SaintPierreEtMiquelon _ -> "Saint Pierre et Miquelon"
      | SaintMartin _ -> "Saint Martin"
      | Mayotte _ -> "Mayotte")

let format_prise_en_charge (fmt : Format.formatter) (g : AF.PriseEnCharge.t) :
    unit =
  Format.fprintf fmt "%s"
    AF.PriseEnCharge.(
      match g with
      | EffectiveEtPermanente _ -> "Effective et permanente"
      | GardeAlterneePartageAllocations _ ->
        "Garde alternée, allocations partagée"
      | GardeAlterneeAllocataireUnique _ -> "Garde alternée, allocataire unique"
      | ServicesSociauxAllocationVerseeALaFamille _ ->
        "Oui, allocations versée à la famille"
      | ServicesSociauxAllocationVerseeAuxServicesSociaux _ ->
        "Oui, allocations versée aux services sociaux")

let num_successful = ref 0
let total_amount = ref 0.

let run_test_allocations_familiales () =
  let num_children = Random.int 7 in
  let children = Array.init num_children random_children in
  let income = Random.int 100000 in
  let current_date = Runtime.date_of_numbers 2020 05 01 in
  let residence =
    let x = Random.int 2 in
    match x with
    | 0 -> AF.Collectivite.Metropole ()
    | 1 -> AF.Collectivite.Guadeloupe ()
    | _ -> AF.Collectivite.Mayotte ()
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
  | (Runtime.NoValueProvided _ | Runtime.ConflictError _) as err ->
    Format.printf "%s\n%a\nincome: %d\ncurrent_date: %s\nresidence: %a\n"
      (match err with
      | Runtime.NoValueProvided _ -> "No value provided somewhere!"
      | Runtime.ConflictError _ -> "Conflict error!"
      | _ -> failwith "impossible")
      (Format.pp_print_list (fun fmt child ->
           Format.fprintf fmt
             "Child %d:\n\
             \  income: %.2f\n\
             \  birth date: %s\n\
             \  prise en charge: %a"
             (Runtime.integer_to_int child.AF.EnfantEntree.d_identifiant)
             (Runtime.money_to_float
                child.AF.EnfantEntree.d_remuneration_mensuelle)
             (Runtime.date_to_string child.AF.EnfantEntree.d_date_de_naissance)
             format_prise_en_charge child.AF.EnfantEntree.d_prise_en_charge))
      (Array.to_list children) income
      (Runtime.date_to_string current_date)
      format_residence residence;
    exit (-1)
  | Runtime.AssertionFailed _ -> ()

let aides_logement_input :
    Law_source.Aides_logement.CalculetteAidesAuLogementGardeAlternee_in.t =
  {
    menage_in =
      {
        personnes_agees_handicapees_foyer_r844_4 = false;
        prestations_recues = Array.of_list [];
        logement =
          {
            residence_principale = true;
            mode_occupation =
              Law_source.Aides_logement.ModeOccupation.Locataire
                {
                  bailleur =
                    Law_source.Aides_logement.TypeBailleur.BailleurPrive ();
                  beneficiaire_aide_adulte_ou_enfant_handicapes = false;
                  logement_est_chambre = false;
                  colocation = false;
                  agees_ou_handicap_adultes_hebergees_onereux_particuliers =
                    false;
                  logement_meuble_d842_2 = false;
                  changement_logement_d842_4 =
                    Law_source.Aides_logement.ChangementLogementD842_4
                    .PasDeChangement
                      ();
                  loyer_principal = Runtime.money_of_units_int 450;
                };
            proprietaire = Law_source.Aides_logement.ParentOuAutre.Autre ();
            loue_ou_sous_loue_a_des_tiers =
              Law_source.Aides_logement.LoueOuSousLoueADesTiers.Non ();
            usufruit = Law_source.Aides_logement.ParentOuAutre.Autre ();
            logement_decent_l89_462 = true;
            surface_m_carres = Runtime.integer_of_int 65;
            zone = Law_source.Aides_logement.ZoneDHabitation.Zone2 ();
          };
        personnes_a_charge =
          Array.of_list
            [
              Law_source.Aides_logement.PersonneACharge.EnfantACharge
                {
                  a_deja_ouvert_droit_aux_allocations_familiales = true;
                  nationalite =
                    Law_source.Aides_logement.Nationalite.Francaise ();
                  etudes_apprentissage_stage_formation_pro_impossibilite_travail =
                    false;
                  remuneration_mensuelle = Runtime.money_of_units_int 0;
                  obligation_scolaire =
                    Law_source.Aides_logement.SituationObligationScolaire
                    .Pendant
                      ();
                  situation_garde_alternee =
                    Law_source.Aides_logement.SituationGardeAlternee
                    .PasDeGardeAlternee
                      ();
                  date_de_naissance = Runtime.date_of_numbers 2015 1 1;
                  identifiant = Runtime.integer_of_int 0;
                };
              Law_source.Aides_logement.PersonneACharge.EnfantACharge
                {
                  a_deja_ouvert_droit_aux_allocations_familiales = true;
                  remuneration_mensuelle = Runtime.money_of_units_int 0;
                  obligation_scolaire =
                    Law_source.Aides_logement.SituationObligationScolaire
                    .Pendant
                      ();
                  situation_garde_alternee =
                    Law_source.Aides_logement.SituationGardeAlternee
                    .PasDeGardeAlternee
                      ();
                  nationalite =
                    Law_source.Aides_logement.Nationalite.Francaise ();
                  etudes_apprentissage_stage_formation_pro_impossibilite_travail =
                    false;
                  date_de_naissance = Runtime.date_of_numbers 2016 1 1;
                  identifiant = Runtime.integer_of_int 1;
                };
            ];
        nombre_autres_occupants_logement = Runtime.integer_of_int 0;
        situation_familiale =
          Law_source.Aides_logement.SituationFamiliale.Concubins ();
        condition_rattache_foyer_fiscal_parent_ifi = false;
        enfant_a_naitre_apres_quatrieme_mois_grossesse = false;
        residence = Metropole ();
      };
    demandeur_in =
      {
        nationalite = Law_source.Aides_logement.Nationalite.Francaise ();
        personne_hebergee_centre_soin_l_L162_22_3_securite_sociale = false;
        date_naissance = Runtime.date_of_numbers 1992 1 1;
        est_non_salarie_agricole_l781_8_l_781_46_code_rural = false;
        magistrat_fonctionnaire_centre_interets_materiels_familiaux_hors_mayotte =
          false;
      };
    date_courante_in = Runtime.date_of_numbers 2022 5 1;
    ressources_menage_prises_en_compte_in = Runtime.money_of_units_int 11500;
  }

let run_test_aides_logement () =
  try
    ignore
      (Law_source.Aides_logement.calculette_aides_au_logement_garde_alternee
         aides_logement_input)
  with
  | (Runtime.NoValueProvided _ | Runtime.ConflictError _) as err ->
    Format.printf "%s"
      (match err with
      | Runtime.NoValueProvided _ -> "No value provided somewhere!"
      | Runtime.ConflictError _ -> "Conflict error!"
      | _ -> failwith "impossible");
    exit (-1)
  | Runtime.AssertionFailed _ -> ()

let _test =
  let () = run_test_aides_logement () in
  let raw_events = Runtime.retrieve_log () in
  Runtime.EventParser.parse_raw_events raw_events

let _test =
  let () = run_test_allocations_familiales () in
  let raw_events = Runtime.retrieve_log () in
  Runtime.EventParser.parse_raw_events raw_events

let _bench =
  Random.init (int_of_float (Unix.time ()));
  let num_iter = 10000 in
  let (_ : Benchmark.samples) =
    Benchmark.latency1 ~style:Auto ~name:"Allocations familiales"
      (Int64.of_int num_iter) run_test_allocations_familiales ()
  in
  Printf.printf
    "Successful\n\
    \   computations: %d (%.2f%%)\n\
    \ Total benefits awarded: %.2f€ (mean %.2f€)\n"
    !num_successful
    (Float.mul
       (Float.div (float_of_int !num_successful) (float_of_int num_iter))
       100.)
    !total_amount
    (Float.div !total_amount (float_of_int !num_successful));
  let (_ : Benchmark.samples) =
    Benchmark.latency1 ~style:Auto ~name:"Aides au logement"
      (Int64.of_int num_iter) run_test_aides_logement ()
  in
  Printf.printf "Successful\n"
