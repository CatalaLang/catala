open Dates_calc.Dates

let date = Alcotest.testable format_date (fun x y -> compare_dates x y = 0)

let check_eqs eqs =
  List.iter
    (fun (d1, delta, d2) ->
      (* checks that d1 + delta = d2 under all rounding modes *)
      List.iter
        (fun rmode ->
          Alcotest.(check date)
            (Format.asprintf "%s + %3s = %s" d1 delta d2)
            (date_of_string d2)
            (add_dates ~round:rmode (date_of_string d1) (period_of_string delta)))
        [RoundUp; RoundDown; AbortOnRound])
    eqs

let check_extended_eqs eqs =
  List.iter
    (fun (d1, delta, dru, drd) ->
      (* checks that d1 +up delta = dru, d1+down delta = drd, d1+abort delta
         aborts withou AmbiguousComputation *)
      Alcotest.(check date)
        (Format.asprintf "%s +up %3s = %s" d1 delta dru)
        (date_of_string dru)
        (add_dates ~round:RoundUp (date_of_string d1) (period_of_string delta));
      Alcotest.(check date)
        (Format.asprintf "%s +down %3s = %s" d1 delta drd)
        (date_of_string drd)
        (add_dates ~round:RoundDown (date_of_string d1) (period_of_string delta));
      Alcotest.check_raises (Format.asprintf "%s +abort %3s raises" d1 delta)
        AmbiguousComputation (fun () ->
          let _ =
            add_dates ~round:AbortOnRound (date_of_string d1)
              (period_of_string delta)
          in
          ()))
    eqs

let read_csv ?(drop_header = true) filename =
  let ic = open_in filename in
  if drop_header then ignore(input_line ic);
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line ->
      loop ((String.split_on_char ';' line) :: acc)
  in
  loop []

let test_add_dates_exact () =
  let cases = read_csv "exact_computations.csv" in 
  let eqs = List.map
      (function
        | [d1; p; d2] -> (d1, p, d2)
        | l -> failwith (Format.asprintf "%d" (List.length l)))
      cases in
  check_eqs eqs

let test_add_dates_ambiguous () =
  let cases = read_csv "ambiguous_computations.csv" in
  let eqs = List.map
      (function
        | [d1; p; du; d_o] -> d1, p, du, d_o
        | _ -> assert false)
      cases in
  check_extended_eqs eqs

let test_first_last_day_of_month () =
  let cases = read_csv "first_last_day_of_month.csv" in
  let eqs = List.map
      (function
        | [d; df; dl] -> d, df, dl
        | _ -> assert false)
      cases in
  List.iter
    (fun (d_init, d_first, d_last) ->
       Alcotest.(check date)
         (Format.asprintf "first_day_of_month %s = %s" d_init d_first)
         (date_of_string d_first)
         (first_day_of_month @@ date_of_string d_init);
       Alcotest.(check date)
         (Format.asprintf "last_day_of_month %s = %s" d_init d_last)
         (date_of_string d_last)
         (last_day_of_month @@ date_of_string d_init)
    )
    eqs


(* Run it *)
let () =
  let open Alcotest in
  run "unit"
    [
      ( "add_dates",
        [
          test_case "exact" `Quick test_add_dates_exact;
          test_case "ambig" `Quick test_add_dates_ambiguous;
        ] );
      ( "first_last_day_of_month",
        [ test_case "all" `Quick test_first_last_day_of_month ] )
    ]
