(* This file is part of the French law library, a collection of functions for
   computing French taxes and benefits derived from Catala programs. Copyright
   (C) 2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>, Emile
   Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Runtime_ocaml.Runtime
module Allocations_familiales = Law_source.Allocations_familiales
module AF = Allocations_familiales

let compute_allocations_familiales
    ~(current_date : date)
    ~(children : AF.EnfantEntree.t array)
    ~(income : int)
    ~(residence : AF.Collectivite.t)
    ~(is_parent : bool)
    ~(fills_title_I : bool)
    ~(had_rights_open_before_2012 : bool) : float =
  let result =
    AF.interface_allocations_familiales
      {
        AF.InterfaceAllocationsFamiliales_in.i_date_courante_in = current_date;
        AF.InterfaceAllocationsFamiliales_in.i_enfants_in = children;
        AF.InterfaceAllocationsFamiliales_in.i_ressources_menage_in =
          money_of_units_int income;
        AF.InterfaceAllocationsFamiliales_in.i_residence_in = residence;
        AF.InterfaceAllocationsFamiliales_in
        .i_personne_charge_effective_permanente_est_parent_in = is_parent;
        AF.InterfaceAllocationsFamiliales_in
        .i_personne_charge_effective_permanente_remplit_titre_I_in =
          fills_title_I;
        AF.InterfaceAllocationsFamiliales_in
        .i_avait_enfant_a_charge_avant_1er_janvier_2012_in =
          had_rights_open_before_2012;
      }
  in
  money_to_float result.AF.InterfaceAllocationsFamiliales.i_montant_verse
