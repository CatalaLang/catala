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

open Runtime

type prise_en_charge =
  | GardeAlterneePartageAllocations of unit
  | GardeAlterneeAllocataireUnique of unit
  | EffectiveEtPermanente of unit
  | ServicesSociauxAllocationVerseeALaFamille of unit
  | ServicesSociauxAllocationVerseeAuxServicesSociaux of unit

type situation_obligation_scolaire =
  | Avant of unit
  | Pendant of unit
  | Apres of unit

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

module EnfantEntree : sig
  type t = {
    d_identifiant : integer;
    d_remuneration_mensuelle : money;
    d_date_de_naissance : date;
    d_prise_en_charge : prise_en_charge;
    d_a_deja_ouvert_droit_aux_allocations_familiales : bool;
    d_beneficie_titre_personnel_aide_personnelle_logement : bool;
  }
end

module Enfant : sig
  type t = {
    identifiant : integer;
    obligation_scolaire : situation_obligation_scolaire;
    remuneration_mensuelle : money;
    date_de_naissance : date;
    age : integer;
    prise_en_charge : prise_en_charge;
    a_deja_ouvert_droit_aux_allocations_familiales : bool;
    beneficie_titre_personnel_aide_personnelle_logement : bool;
  }
end

module InterfaceAllocationsFamilialesOut : sig
  type t = { i_montant_verse_out : money }
end

module InterfaceAllocationsFamilialesIn : sig
  type t = {
    i_date_courante_in : date;
    i_enfants_in : EnfantEntree.t array;
    i_ressources_menage_in : money;
    i_residence_in : collectivite;
    i_personne_charge_effective_permanente_est_parent_in : bool;
    i_personne_charge_effective_permanente_remplit_titre_I_in : bool;
    i_avait_enfant_a_charge_avant_1er_janvier_2012_in : bool;
  }
end

val interface_allocations_familiales :
  InterfaceAllocationsFamilialesIn.t -> InterfaceAllocationsFamilialesOut.t
