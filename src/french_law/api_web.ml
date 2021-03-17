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

module Allocations_familiales = Law_source.Allocations_familiales
module AF = Allocations_familiales
open Runtime
open Js_of_ocaml

class type enfant_entree =
  object
    method id : int Js.readonly_prop

    method remunerationMensuelle : int Js.readonly_prop

    method dateNaissance : Js.date Js.t Js.readonly_prop

    method gardeAlternee : bool Js.t Js.readonly_prop

    method gardeAlterneePartageAllocation : bool Js.t Js.readonly_prop

    method priseEnChargeServiceSociaux : bool Js.t Js.readonly_prop

    method allocationVerseeServiceSociaux : bool Js.t Js.readonly_prop

    method aDejaOuvertDroitAuxAllocationsFamiliales : bool Js.t Js.readonly_prop
  end

class type allocations_familiales_input =
  object
    method currentDate : Js.date Js.t Js.readonly_prop

    method children : enfant_entree Js.t Js.js_array Js.t Js.readonly_prop

    method income : int Js.readonly_prop

    method residence : Js.js_string Js.t Js.readonly_prop

    method personneQuiAssumeLaChargeEffectivePermanenteEstParent : bool Js.t Js.readonly_prop

    method personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale :
      bool Js.t Js.readonly_prop
  end

let _ =
  Js.export_all
    (object%js
       method computeAllocationsFamiliales (input : allocations_familiales_input Js.t) : float =
         let result =
           AF.interface_allocations_familiales
             {
               AF.personne_charge_effective_permanente_est_parent_in =
                 (fun _ -> Js.to_bool input##.personneQuiAssumeLaChargeEffectivePermanenteEstParent);
               AF.personne_charge_effective_permanente_remplit_titre_I_in =
                 (fun _ ->
                   Js.to_bool
                     input##.personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale);
               AF.date_courante_in =
                 (fun _ ->
                   date_of_numbers
                     input##.currentDate##getFullYear
                     input##.currentDate##getMonth
                     input##.currentDate##getDay);
               AF.enfants_in =
                 (fun _ ->
                   Array.map
                     (fun (child : enfant_entree Js.t) ->
                       {
                         AF.d_a_deja_ouvert_droit_aux_allocations_familiales =
                           Js.to_bool child##.aDejaOuvertDroitAuxAllocationsFamiliales;
                         AF.d_identifiant = integer_of_int child##.id;
                         AF.d_date_de_naissance =
                           date_of_numbers
                             child##.dateNaissance##getFullYear
                             child##.dateNaissance##getMonth
                             child##.dateNaissance##getDay;
                         AF.d_prise_en_charge =
                           ( if Js.to_bool child##.gardeAlternee then
                             if Js.to_bool child##.gardeAlterneePartageAllocation then
                               GardeAlterneePartageAllocations ()
                             else GardeAlterneeAllocataireUnique ()
                           else if Js.to_bool child##.priseEnChargeServiceSociaux then
                             if Js.to_bool child##.allocationVerseeServiceSociaux then
                               ServicesSociauxAllocationVerseeAuxServicesSociaux ()
                             else ServicesSociauxAllocationVerseeALaFamille ()
                           else EffectiveEtPermanente () );
                         AF.d_remuneration_mensuelle =
                           money_of_units_int child##.remunerationMensuelle;
                       })
                     (Js.to_array input##.children));
               AF.enfants_a_charge_in = no_input;
               AF.ressources_menage_in = (fun _ -> money_of_units_int input##.income);
               AF.residence_in =
                 (fun _ ->
                   match Js.to_string input##.residence with
                   | "Métropole" -> AF.Metropole ()
                   | "Guyane" -> AF.Guyane ()
                   | "Guadeloupe" -> AF.Guadeloupe ()
                   | "Martinique" -> AF.Martinique ()
                   | "La Réunion" -> AF.LaReunion ()
                   | "Saint Barthélemy" -> AF.SaintBarthelemy ()
                   | "Saint Pierre et Miquelon" -> AF.SaintPierreEtMiquelon ()
                   | "Saint Martin" -> AF.SaintMartin ()
                   | "Mayotte" -> AF.Mayotte ()
                   | _ -> failwith "unknown collectivite!");
               AF.montant_verse_in = no_input;
             }
         in
         money_to_float result.AF.montant_verse_out
    end)
