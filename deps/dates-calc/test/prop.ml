open Dates_calc.Dates

let rec date_generator gen =
  let open QCheck.Gen in
  map3 (fun y m d ->
      try make_date ~year:y ~month:m ~day:d
      with InvalidDate ->
        (* let () = Format.printf "%02d-%02d-%04d invalid, sampling again@." d m y in *)
        date_generator gen)
    (int_range 0 10000) (int_range 1 12) (int_range 1 31) gen

let date =
  QCheck.make ~print:(Format.asprintf "%a" format_date) date_generator

let period =
  let r = QCheck.int_range 0 10000 in
  QCheck.map (fun (ys, ms, ds) -> make_period ~years:ys ~months:ms ~days:ds) (QCheck.triple r r r)

let add_dates_days_check (d, x) =
  (* - forall d, forall x, compare_dates (add_dates_days (add_dates_days d x) -x ) d = 0 *)
  let x_days = make_period ~years:0 ~months:0 ~days:x in
  compare_dates (add_dates (add_dates d x_days) (neg_period x_days)) d = 0

let add_sub_dates_days_check (d1, d2) =
  (* - forall d1 d2, compare_dates (add_dates d2 (sub_dates d1 d2)) d1 = 0 *)
  compare_dates (add_dates d2 (sub_dates d1 d2)) d1 = 0

let t1 = QCheck.Test.make ~count:1000 (QCheck.pair date (QCheck.int_range 0 365000)) add_dates_days_check

let t2 = QCheck.Test.make ~count:1000 (QCheck.pair date date) add_sub_dates_days_check

let round_diff_hyp (d, p) =
  let is_in_feb d =
    let _,m,_ = date_to_ymd d in
    m = 2 in
  let dup = add_dates ~round:RoundUp d p in
  let down = add_dates ~round:RoundDown d p in
  period_to_days (sub_dates dup down) <= 1 + (if is_in_feb d then 3 else 0) (* for leap/non-leap discontinuities *)

let h1 = QCheck.Test.make ~name:"h1" ~count:1000000 (QCheck.pair date period) round_diff_hyp

let add_monotonic_hyp (d1, d2, p) =
  let (==>) x y = not x || y in
  ((compare_dates d1 d2 < 0) ==> (compare_dates (add_dates ~round:RoundUp d1 p) (add_dates ~round:RoundUp d2 p) <= 0))
  &&
  ((compare_dates d1 d2 < 0) ==> (compare_dates (add_dates ~round:RoundDown d1 p) (add_dates ~round:RoundDown d2 p) <= 0))

let h2 = QCheck.Test.make ~name:"h2" ~count:1000000 (QCheck.triple date date period) add_monotonic_hyp

let round_monotonic_hyp (d, p) =
  compare_dates (add_dates ~round:RoundDown d p) (add_dates ~round:RoundUp d p) <= 0

let h3 = QCheck.Test.make ~name:"h3" ~count:1000000 (QCheck.pair date period) round_monotonic_hyp

let round_upper_bound_hyp (d, n) =
  let p = make_period ~years:0 ~months:1 ~days:0 in
  let rec add_nth_times count d =
    if count = 0 then d
    else add_nth_times (count-1) (add_dates ~round:RoundDown d p) in
  let r1 = add_nth_times n d in
  let r2 = add_dates ~round:RoundDown d (mul_period p n) in
  abs (period_to_days (sub_dates r1 r2)) <= 3 (*WHY?*)

let h4 = QCheck.Test.make ~name:"h4" ~count:1000000 (QCheck.pair date (QCheck.int_range 0 1000)) round_upper_bound_hyp


let () =
  exit @@ QCheck_base_runner.run_tests ~verbose:true [t1; t2; h1; h2; h3; h4]
