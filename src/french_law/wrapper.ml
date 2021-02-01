module Allocations_familiales = Law_source.Allocations_familiales
module AF = Allocations_familiales
open Catala.Runtime

let compute_allocations_familiales ~(current_date : CalendarLib.Date.t)
    ~(children : AF.enfant_entree array) ~(income : int) ~(residence : AF.collectivite) : float =
  let result =
    AF.interface_allocations_familiales
      {
        AF.date_courante_in = (fun _ -> date_of_calendar_date current_date);
        AF.enfants_in = (fun _ -> children);
        AF.enfants_a_charge_in = no_input;
        AF.ressources_menage_in = (fun _ -> money_of_units_integers income);
        AF.residence_in = (fun _ -> residence);
        AF.montant_verse_in = no_input;
      }
  in
  money_to_float result.AF.montant_verse_out
