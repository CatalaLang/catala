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

let compute_allocations_familiales ~(current_date : Runtime.date)
    ~(children : AF.enfant_entree array) ~(income : int) ~(residence : AF.collectivite)
    ~(is_parent : bool) ~(fills_title_I : bool) ~(had_rights_open_before_2012 : bool) : float =
  let result =
    AF.interface_allocations_familiales
      {
        AF.date_courante_in = (fun _ -> current_date);
        AF.enfants_in = (fun _ -> children);
        AF.enfants_a_charge_in = no_input;
        AF.ressources_menage_in = (fun _ -> money_of_units_int income);
        AF.residence_in = (fun _ -> residence);
        AF.montant_verse_in = no_input;
        AF.personne_charge_effective_permanente_est_parent_in = (fun _ -> is_parent);
        AF.personne_charge_effective_permanente_remplit_titre_I_in = (fun _ -> fills_title_I);
        AF.avait_enfant_a_charge_avant_1er_janvier_2012_in = (fun _ -> had_rights_open_before_2012);
      }
  in
  money_to_float result.AF.montant_verse_out
