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

open Runtime

type prise_en_charge =
  | GardeAlterneePartageAllocations of unit
  | GardeAlterneeAllocataireUnique of unit
  | EffectiveEtPermanente of unit
  | ServicesSociauxAllocationVerseeALaFamille of unit
  | ServicesSociauxAllocationVerseeAuxServicesSociaux of unit

type situation_obligation_scolaire = Avant of unit | Pendant of unit | Apres of unit

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

type element_prestations_familiales =
  | PrestationAccueilJeuneEnfant of unit
  | AllocationsFamiliales of unit
  | ComplementFamilial of unit
  | AllocationLogement of unit
  | AllocationEducationEnfantHandicape of unit
  | AllocationSoutienFamilial of unit
  | AllocationRentreeScolaire of unit
  | AllocationJournalierePresenceParentale of unit

type enfant_entree = {
  d_identifiant : integer;
  d_remuneration_mensuelle : money;
  d_date_de_naissance : date;
  d_prise_en_charge : prise_en_charge;
  d_a_deja_ouvert_droit_aux_allocations_familiales : bool;
}

type enfant = {
  identifiant : integer;
  obligation_scolaire : situation_obligation_scolaire;
  remuneration_mensuelle : money;
  date_de_naissance : date;
  age : integer;
  prise_en_charge : prise_en_charge;
  a_deja_ouvert_droit_aux_allocations_familiales : bool;
}

type interface_allocations_familiales_out = {
  date_courante_out : date;
  enfants_out : enfant_entree array;
  enfants_a_charge_out : enfant array;
  ressources_menage_out : money;
  residence_out : collectivite;
  montant_verse_out : money;
  personne_charge_effective_permanente_est_parent_out : bool;
  personne_charge_effective_permanente_remplit_titre_I_out : bool;
}

type interface_allocations_familiales_in = {
  date_courante_in : unit -> date;
  enfants_in : unit -> enfant_entree array;
  enfants_a_charge_in : unit -> enfant array;
  ressources_menage_in : unit -> money;
  residence_in : unit -> collectivite;
  montant_verse_in : unit -> money;
  personne_charge_effective_permanente_est_parent_in : unit -> bool;
  personne_charge_effective_permanente_remplit_titre_I_in : unit -> bool;
}

val interface_allocations_familiales :
  interface_allocations_familiales_in -> interface_allocations_familiales_out
