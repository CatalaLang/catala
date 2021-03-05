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

type garde_alternee =
  | OuiPartageAllocations of unit
  | OuiAllocataireUnique of unit
  | NonGardeUnique of unit

type prise_en_charge_service_sociaux =
  | OuiAllocationVerseeALaFamille of unit
  | OuiAllocationVerseeAuxServicesSociaux of unit
  | NonPriseEnChargeFamille of unit

type collectivite =
  | Guadeloupe of unit
  | Guyane of unit
  | Martinique of unit
  | LaReunion of unit
  | SaintBarthelemy of unit
  | SaintMartin of unit
  | Metropole of unit
  | SaintPierreEtMiquelon of unit
  | Mayotte of unit

type prise_en_compte_evaluation_montant = Complete of unit | Partagee of unit

type versement_allocations = Normal of unit | AllocationVerseeAuxServicesSociaux of unit

type age_alternatif = Absent of unit | Present of Runtime.integer

type element_prestations_familiales =
  | PrestationAccueilJeuneEnfant of unit
  | AllocationsFamiliales of unit
  | ComplementFamilial of unit
  | AllocationLogement of unit
  | AllocationEducationEnfantHandicape of unit
  | AllocationSoutienFamilial of unit
  | AllocationRentreeScolaire of unit
  | AllocationJournalierePresenceParentale of unit

type personne = { numero_securite_sociale : Runtime.integer }

type enfant_entree = {
  d_identifiant : Runtime.integer;
  d_remuneration_mensuelle : Runtime.money;
  d_date_de_naissance : Runtime.date;
  d_garde_alternee : garde_alternee;
  d_prise_en_charge_par_services_sociaux : prise_en_charge_service_sociaux;
}

type enfant = {
  identifiant : Runtime.integer;
  fin_obligation_scolaire : Runtime.date;
  remuneration_mensuelle : Runtime.money;
  date_de_naissance : Runtime.date;
  age : Runtime.integer;
  garde_alternee : garde_alternee;
  prise_en_charge_par_services_sociaux : prise_en_charge_service_sociaux;
}

type stockage_enfant = PasEnfant of unit | UnEnfant of enfant

type interface_allocations_familiales_out = {
  date_courante_out : Runtime.date;
  enfants_out : enfant_entree array;
  enfants_a_charge_out : enfant array;
  ressources_menage_out : Runtime.money;
  residence_out : collectivite;
  montant_verse_out : Runtime.money;
}

type interface_allocations_familiales_in = {
  date_courante_in : unit -> Runtime.date;
  enfants_in : unit -> enfant_entree array;
  enfants_a_charge_in : unit -> enfant array;
  ressources_menage_in : unit -> Runtime.money;
  residence_in : unit -> collectivite;
  montant_verse_in : unit -> Runtime.money;
}

val interface_allocations_familiales :
  interface_allocations_familiales_in -> interface_allocations_familiales_out
