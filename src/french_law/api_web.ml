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

    method priseEnCharge : Js.js_string Js.t Js.readonly_prop
    (** Expects one of the five:

        - "Effective et permanente"
        - "Garde alternée, allocataire unique"
        - "Garde alternée, partage des allocations"
        - "Confié aux service sociaux, allocation versée à la famille"
        - "Confié aux service sociaux, allocation versée aux services sociaux" *)

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

class type source_position =
  object
    method fileName : Js.js_string Js.t Js.prop

    method startLine : int Js.prop

    method endLine : int Js.prop

    method startColumn : int Js.prop

    method endColumn : int Js.prop

    method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
  end

class type log_event =
  object
    method eventType : Js.js_string Js.t Js.prop

    method information : Js.js_string Js.t Js.js_array Js.t Js.prop

    method sourcePosition : source_position Js.t Js.optdef Js.prop

    method loggedValue : Js.Unsafe.any Js.prop
  end

let embed_to_js (v : runtime_value) : Js.Unsafe.any =
  match v with
  | Unit -> Js.Unsafe.inject Js.null
  | Bool b -> Js.Unsafe.inject (Js.bool b)
  | Integer i -> Js.Unsafe.inject (integer_to_int i)
  | Decimal d -> Js.Unsafe.inject (decimal_to_float d)
  | Money m -> Js.Unsafe.inject (money_to_float m)
  | Date d ->
      let date = new%js Js.date_now in
      ignore (date##setUTCFullYear (integer_to_int @@ year_of_date d));
      ignore (date##setUTCMonth (integer_to_int @@ month_number_of_date d));
      ignore (date##setUTCDate (integer_to_int @@ day_of_month_of_date d));
      ignore (date##setUTCHours 0);
      ignore (date##setUTCMinutes 0);
      ignore (date##setUTCSeconds 0);
      ignore (date##setUTCMilliseconds 0);
      Js.Unsafe.inject date
  | Duration d ->
      let days, months, years = duration_to_days_months_years d in
      Js.Unsafe.inject (Js.string (Printf.sprintf "%dD%dM%dY" days months years))
  | Struct _ -> Js.Unsafe.inject Js.null
  | Enum _ -> Js.Unsafe.inject Js.null
  | Unembeddable -> Js.Unsafe.inject Js.null

let _ =
  Js.export_all
    (object%js
       method resetLog : (unit -> unit) Js.callback = Js.wrap_callback reset_log

       method retrieveLog : (unit -> log_event Js.t Js.js_array Js.t) Js.callback =
         Js.wrap_callback (fun () ->
             Js.array
               (Array.of_list
                  (List.map
                     (fun evt ->
                       object%js
                         val mutable eventType =
                           Js.string
                             (match evt with
                             | BeginCall _ -> "Begin call"
                             | EndCall _ -> "End call"
                             | VariableDefinition _ -> "Variable definition"
                             | DecisionTaken _ -> "Decision taken")

                         val mutable information =
                           Js.array
                             (Array.of_list
                                (match evt with
                                | BeginCall info | EndCall info | VariableDefinition (info, _) ->
                                    List.map Js.string info
                                | DecisionTaken _ -> []))

                         val mutable loggedValue =
                           match evt with
                           | VariableDefinition (_, v) -> embed_to_js v
                           | EndCall _ | BeginCall _ | DecisionTaken _ ->
                               Js.Unsafe.inject Js.undefined

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
                                      Js.array (Array.of_list (List.map Js.string pos.law_headings))
                                 end)
                           | _ -> Js.undefined
                       end)
                     (retrieve_log ()))))

       method computeAllocationsFamiliales
           : (allocations_familiales_input Js.t -> float) Js.callback =
         Js.wrap_callback (fun input ->
             let result =
               AF.interface_allocations_familiales
                 {
                   AF.personne_charge_effective_permanente_est_parent_in =
                     (fun _ ->
                       Js.to_bool input##.personneQuiAssumeLaChargeEffectivePermanenteEstParent);
                   AF.personne_charge_effective_permanente_remplit_titre_I_in =
                     (fun _ ->
                       Js.to_bool
                         input##.personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale);
                   AF.date_courante_in =
                     (fun _ ->
                       date_of_numbers
                         input##.currentDate##getUTCFullYear
                         input##.currentDate##getUTCMonth
                         input##.currentDate##getUTCDate);
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
                                 child##.dateNaissance##getUTCFullYear
                                 child##.dateNaissance##getUTCMonth
                                 child##.dateNaissance##getUTCDate;
                             AF.d_prise_en_charge =
                               (match Js.to_string child##.priseEnCharge with
                               | "Effective et permanente" -> EffectiveEtPermanente ()
                               | "Garde alternée, allocataire unique" ->
                                   GardeAlterneeAllocataireUnique ()
                               | "Garde alternée, partage des allocations" ->
                                   GardeAlterneePartageAllocations ()
                               | "Confié aux service sociaux, allocation versée à la famille" ->
                                   ServicesSociauxAllocationVerseeALaFamille ()
                               | "Confié aux service sociaux, allocation versée aux services \
                                  sociaux" ->
                                   ServicesSociauxAllocationVerseeAuxServicesSociaux ()
                               | _ -> failwith "Unknown prise en charge");
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
             money_to_float result.AF.montant_verse_out)
    end)
