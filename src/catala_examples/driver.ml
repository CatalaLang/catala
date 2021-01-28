module M = Allocations_familiales
open Runtime

let run_test (id : int) =
  let _, _, _, _, _, amount =
    M.interfaceallocationsfamiliales
      (fun _ -> date_of_numbers 2020 05 01)
      (fun _ ->
        [|
          {
            d_identifiant = Z.of_int 1;
            d_r_muneration_mensuelle = Z.of_int 0;
            d_date_de_naissance = date_of_numbers 2007 01 01;
            d_garde_altern_e = M.NonGardeUnique ();
            d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
          };
          {
            d_identifiant = Z.of_int 2;
            d_r_muneration_mensuelle = Z.of_int 0;
            d_date_de_naissance = date_of_numbers 2009 01 01;
            d_garde_altern_e = M.NonGardeUnique ();
            d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
          };
          {
            d_identifiant = Z.of_int 3;
            d_r_muneration_mensuelle = Z.of_int 40000;
            d_date_de_naissance = date_of_numbers 2003 01 01;
            d_garde_altern_e = M.OuiPartageAllocations ();
            d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
          };
          {
            d_identifiant = Z.of_int id;
            d_r_muneration_mensuelle = Z.of_int 110000;
            d_date_de_naissance = date_of_numbers 2001 01 01;
            d_garde_altern_e = M.NonGardeUnique ();
            d_pris_en_charge_par_services_sociaux = M.NonPriseEnChargeFamille ();
          };
        |])
      (fun _ -> raise EmptyError)
      (fun _ -> money_of_cent_string "3000000")
      (fun _ -> M.M_tropole ())
      (fun _ -> raise EmptyError)
  in
  assert (Z.to_int amount = 35126)

let _ =
  let _ = Benchmark.latency1 ~style:Auto (Int64.of_int 100000) run_test 4 in
  ()
