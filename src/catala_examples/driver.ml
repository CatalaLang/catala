module M = Allocations_familiales
open Runtime

let compute_allocations_familiales ~(current_date : Runtime.date)
    ~(children : M.enfant_entree array) ~(income : int) ~(residence : M.collectivite) : float =
  let result =
    M.interface_allocations_familiales
      (fun _ -> current_date)
      (fun _ -> children)
      no_input
      (fun _ -> money_of_units_integers income)
      (fun _ -> residence)
      no_input
  in
  Float.div (Z.to_float result.M.montant_verse) 100.

let run_test (id : int) =
  let children =
    [|
      {
        M.d_identifiant = Z.of_int 1;
        d_remuneration_mensuelle = Z.of_int 0;
        d_date_de_naissance = date_of_numbers 2007 01 01;
        d_garde_alternee = M.NonGardeUnique ();
        d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
      };
      {
        d_identifiant = Z.of_int 2;
        d_remuneration_mensuelle = Z.of_int 0;
        d_date_de_naissance = date_of_numbers 2009 01 01;
        d_garde_alternee = M.NonGardeUnique ();
        d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
      };
      {
        d_identifiant = Z.of_int 3;
        d_remuneration_mensuelle = Z.of_int 40000;
        d_date_de_naissance = date_of_numbers 2003 01 01;
        d_garde_alternee = M.OuiPartageAllocations ();
        d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
      };
      {
        d_identifiant = Z.of_int id;
        d_remuneration_mensuelle = Z.of_int 110000;
        d_date_de_naissance = date_of_numbers 2001 01 01;
        d_garde_alternee = M.NonGardeUnique ();
        d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
      };
    |]
  in
  let current_date = CalendarLib.Date.make 2020 05 01 in
  let amount =
    compute_allocations_familiales ~current_date ~income:30000 ~residence:(M.Metropole ()) ~children
  in
  assert (amount = 351.26)

let _ =
  let _ = Benchmark.latency1 ~style:Auto (Int64.of_int 100000) run_test 4 in
  ()
