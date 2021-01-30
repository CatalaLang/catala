module Allocations_familiales = Law_source.Allocations_familiales
open Catala.Runtime

let compute_allocations_familiales ~(current_date : CalendarLib.Date.t)
    ~(children : Allocations_familiales.enfant_entree array) ~(income : int)
    ~(residence : Allocations_familiales.collectivite) : float =
  let result =
    Allocations_familiales.interface_allocations_familiales
      (fun _ -> date_of_calendar_date current_date)
      (fun _ -> children)
      no_input
      (fun _ -> money_of_units_integers income)
      (fun _ -> residence)
      no_input
  in
  money_to_float result.Allocations_familiales.montant_verse
