module Dates

(* Needed for using the standard * as multiplication operator *)
open FStar.Mul

(** Basic types *)

type date = {
  year : int;
  month : int;
  day : int
}

type period = {
  years : int;
  months : int;
  days : int
}

(** Functions on periods **)

let make_period (years : int) (months : int) (days : int) : period =
  { years; months; days }

let add_periods (d1 d2:period) : period =
  { years = d1.years + d2.years;
    months = d1.months + d2.months;
    days = d1.days + d2.days
  }

let sub_periods (d1 d2:period) : period =
  { years = d1.years - d2.years;
    months = d1.months - d2.months;
    days = d1.days - d2.days
  }

let mul_period (d:period) (m : int) : period =
  { years = d.years * m; months = d.months * m; days = d.days * m }

(* To remain in F*'s Pure realm to simplify proofs,
   we will use options instead of exceptions *)
let period_to_days (p : period) : option int =
  if p.years <> 0 || p.months <> 0 then None else Some p.days

(** Functions on dates **)

let is_leap_year (year : int) : bool =
  year % 400 = 0 || (year % 4 = 0 && year % 100 <> 0)

let nb_days (month:int) (year : int) : option int =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> Some 31
  | 4 | 6 | 9 | 11 -> Some 30
  | 2 -> if is_leap_year year then Some 29 else Some 28
  | _ -> None

let is_valid_date (d : date) : bool =
  match nb_days d.month d.year with
  | None -> false
  | Some days -> d.day >= 1 && d.day <= days

let make_date (year month day: int) : option date =
  let d = {year; month; day} in
  if is_valid_date d then Some d else None

(* Some measure to express that being in the right year after adding the months
   period is a terminal case for the recursion below *)
let in_same_year (month months:int) : GTot nat =
  if 1 <= month + months && month + months <= 12 then 0
  else 1

let add_dates_years (d: date) (years: int) : date = {d with year = d.year + years}

(** Returns new [year, month] *)
let rec add_dates_months
    (d:date)
    (months : int)
    : Pure date
           (requires True)
           (ensures fun d -> 1 <= d.month /\ d.month <= 12)
           (* The absolute value of the new_month always decreases, except
              maybe in the case where the next iteration will be terminal *)
           (decreases %[in_same_year d.month months; abs (d.month + months)]) =
  let new_month = d.month + months in
  // Add-Month case
  if 1 <= new_month && new_month <= 12 then {d with month = new_month}
  else if new_month > 12 then (
    // Add-Month-Over case
    add_dates_months {d with year = d.year + 1} (months - 12)
  ) else
    // Add-Month-Under case
    (* new_month <= 0 *)
    add_dates_months {d with year = d.year - 1} (months + 12)

(* Some measure to express that being in the right year after adding the months
   period is a terminal case for the recursion below *)
let in_same_month (d:date{is_valid_date d}) (days:int) : GTot nat =
  let days_in_d_month = Some?.v (nb_days d.month d.year) in
  let new_day = d.day + days in
  if 1 <= new_day && new_day <= days_in_d_month then 0
  else 1

(* Day addition function when the date is valid *)
let rec add_dates_days_valid (d:date{is_valid_date d}) (days:int)
  : Tot (d:date{is_valid_date d})
        (decreases %[in_same_month d days; abs (d.day + days); abs (d.day)]) =
  let days_in_d_month = Some?.v (nb_days d.month d.year) in
  let new_day = d.day + days in
  if 1 <= new_day && new_day <= days_in_d_month then
    // Add-Days case
    { d with day = new_day }
  else if new_day >= days_in_d_month then (
    // Add-Days-Over case
    let d' = add_dates_months d 1 in
    add_dates_days_valid
      {d' with day = 1}
      (days - (days_in_d_month - d.day) - 1)
  ) else (
    if 1 < d.day && new_day <= 0 then (
      // Can be deduced from previous clauses, but we add it as an assertion for a sanity check
      assert (d.day <= days_in_d_month);
      // Add-Days-Under1 case
      add_dates_days_valid {d with day = 1} (new_day - 1)
    ) else (
      // Confirming that the Add-Days-Under2 pattern requiring day to be 1 applies
      assert (d.day = 1);
      // Add-Days-Under2 case
      // Computing m', d' in hypothesis
      let d' = add_dates_months d (-1) in
      add_dates_days_valid
        {d' with day = 1}
        (days + Some?.v (nb_days d'.month d'.year))
    )
  )

(* Day addition function. None corresponds to the error (or "bottom") case.
   The case returning None encapsulates Add-Days-Err1 and Add-Days-Err2.
*)
let add_dates_days (d:date) (days: int) : Tot (option date) =
  if d.day < 1 || None? (nb_days d.month d.year) || d.day > Some?.v (nb_days d.month d.year) then None
  else Some (add_dates_days_valid d days)

let compare_dates (d1 d2:date) : int =
  if d1.year - d2.year = 0 then
    if d1.month - d2.month = 0 then d1.day - d2.day
    else d1.month - d2.month
  else d1.year - d2.year

(* A termination measure used below, which decreases when d1 > d2 instead of d1 < d2 *)
let dates_compare_sign (d1 d2:date)
  = if d1.year = d2.year && d1.month = d2.month then 0
    else if compare_dates d1 d2 < 0 then 2
    else 1

(** The returned period is always expressed as a number of days *)
let rec sub_dates (d1:date{is_valid_date d1}) (d2:date{is_valid_date d2})
  : Tot int
  (decreases %[dates_compare_sign d1 d2; abs (d1.year - d2.year); 12 - d2.month]) =
  if d1.year = d2.year && d1.month = d2.month then
    (d1.day - d2.day)
  else begin
    let cmp = compare_dates d1 d2 in
    if cmp < 0 then
      -(sub_dates d2 d1)
    else begin
      let d2' = add_dates_months d2 1 in
      let new_d2 = {d2' with day = 1} in
      (Some?.v (nb_days d2.month d2.year) - d2.day + 1) + (sub_dates d1 new_d2)
    end
  end

(* Rounding down operator *)
let round_down (d:date) : option date =
  let nb = nb_days d.month d.year in
  // Round-Err1 case, and implicit failure is nb_days is not defined
  if None? nb || d.day < 1 then None
  // Round-Down case
  else if d.day > Some?.v nb then Some {d with day = Some?.v nb}
  // Round-noop case
  else Some d


(* Rounding down operator *)
let round_up (d:date) : option date =
  let nb = nb_days d.month d.year in
  // Round-Err1 case, and implicit failure is nb_days is not defined
  if None? nb || d.day < 1 then None
  // Round-Up case
  else if d.day > Some?.v nb then
    let d' = add_dates_months d 1 in
    Some {d' with day = 1}
  // Round-noop case
  else Some d

let round_err (d:date) : option date =
  let nb = nb_days d.month d.year in
  // Round-Err1 case, and implicit failure is nb_days is not defined
  if None? nb || d.day < 1 then None
  // Round-Down case
  else if d.day > Some?.v nb then None
  // Round-noop case
  else Some d

(* Derived semantics: Definition of the three derived forms *)

let add_up (d:date) (p:period) : option date =
   match round_up (add_dates_months (add_dates_years d p.years) p.months) with
   | None -> None
   | Some d -> add_dates_days d p.days

let add_down (d:date) (p:period) : option date =
   match round_down (add_dates_months (add_dates_years d p.years) p.months) with
   | None -> None
   | Some d -> add_dates_days d p.days

let add_err (d:date) (p:period) : option date =
   match round_err (add_dates_months (add_dates_years d p.years) p.months) with
   | None -> None
   | Some d -> add_dates_days d p.days

(*** Lemmas ***)

(* Several of the lemmas and theorems stated in the paper are intrinsically proven above.
   We however restate them below *)

(* Proving Theorem1 for day, month, and year *)
let theorem1_day (d: date) (n: int) : Lemma (exists (v:option date). add_dates_days d n == v) =
  FStar.Classical.exists_intro (fun v -> add_dates_days d n == v) (add_dates_days d n)

let theorem1_month (d: date) (n: int) : Lemma (exists (v:date). add_dates_months d n == v) =
  FStar.Classical.exists_intro (fun v -> add_dates_months d n == v) (add_dates_months d n)

let theorem1_year (d: date) (n: int) : Lemma (exists (v:date). add_dates_years d n == v) =
  FStar.Classical.exists_intro (fun v -> add_dates_years d n == v) (add_dates_years d n)

(* Lemma 1: Well-formedness of day addition. The bottom case is represented by None *)
let lemma1 (d:date{is_valid_date d}) (n:int) (v: option date) : Lemma
  (requires add_dates_days d n == v)
  (ensures Some? v)
  = ()

(* Lemma 2: Well-formedness of year/month addition. We directly have that the result
   is not bottom since none of the functions return an option *)

(* Going through an auxiliary function as recursive calls do not always correspond
   to a valid d:date, although it is never a bottom case *)
let rec lemma2_month' (d:date) (n:int) (v: date) : Lemma
  (requires d.day >= 1 /\ add_dates_months d n == v)
  (ensures v.day >= 1)
  (decreases %[in_same_year d.month n; abs (d.month + n)])
  =
  let new_month = d.month + n in
  // Add-Month case
  if 1 <= new_month && new_month <= 12 then ()
  else if new_month > 12 then (
    lemma2_month' {d with year = d.year + 1} (n - 12) v
  ) else
    lemma2_month' {d with year = d.year - 1} (n + 12) v

(* Actual lemma statement *)
let lemma2_month (d:date) (n:int) (v: date) : Lemma
  (requires is_valid_date d /\ add_dates_months d n == v)
  (ensures v.day >= 1)
  = lemma2_month' d n v

let lemma2_year (d:date) (n:int) (v: date) : Lemma
  (requires is_valid_date d /\ add_dates_years d n == v)
  (ensures v.day >= 1)
  = ()


(* Lemma 3: Well-formedness of rounding. Again, the hypothesis d <> bottom is
   encoded by taking a `date` instead of an `option date`.
 *)
let lemma3_up (d:date) (v:option date) : Lemma
  (requires Some? (nb_days d.month d.year) /\ d.day >= 1 /\ round_up d == v)
  (ensures Some? v /\ is_valid_date (Some?.v v))
  = ()

let lemma3_down (d:date) (v:option date) : Lemma
  (requires Some? (nb_days d.month d.year) /\ d.day >= 1 /\ round_down d == v)
  (ensures Some? v /\ is_valid_date (Some?.v v))
  = ()

(* Theorem 2 : Well-formedness. Proven by combining the lemmas above *)
let theorem2_up (d:date) (p:period) (v: option date) : Lemma
  (requires is_valid_date d /\ add_up d p == v)
  (ensures Some? v /\ is_valid_date (Some?.v v))
  = let v_y = add_dates_years d p.years in
    let v_m = add_dates_months v_y p.months in
    let v_rnd = round_up v_m in
    lemma2_month' v_y p.months v_m;
    lemma3_up v_m v_rnd

let theorem2_down (d:date) (p:period) (v: option date) : Lemma
  (requires is_valid_date d /\ add_down d p == v)
  (ensures Some? v /\ is_valid_date (Some?.v v))
  = let v_y = add_dates_years d p.years in
    let v_m = add_dates_months v_y p.months in
    let v_rnd = round_down v_m in
    lemma2_month' v_y p.months v_m;
    lemma3_down v_m v_rnd

(* Lemma 4: Equivalence of year and month addition *)
let rec lemma4 (d: date) (n: int)
  : Lemma (requires 1 <= d.month /\ d.month <= 12)
          (ensures add_dates_years d n == add_dates_months d (12 * n))
          (decreases %[in_same_year d.month (12 * n); abs (d.month + 12 * n)])
  =
  let months = 12 * n in
  let new_month = d.month + months in
  if 1 <= new_month && new_month <= 12 then ()
  else if new_month > 12 then (
    lemma4 {d with year = d.year + 1} (n - 1)
  ) else (
    lemma4 {d with year = d.year - 1} (n + 1)
  )

(* Lemma 5: Monotonicity of year and month addition *)
let lemma5_year (d1 d2:date) (n: int) : Lemma
  (requires compare_dates d1 d2 < 0) // Encoding d1 < d2
  // Encoding d1 + n < d2 + n
  (ensures compare_dates (add_dates_years d1 n) (add_dates_years d2 n) < 0)
  = ()

let rec lemma5_month (d1 d2:date) (n: int) : Lemma
  (requires compare_dates d1 d2 < 0 /\ // Encoding d1 < d2
            1 <= d1.month /\ d1.month <= 12 /\
            1 <= d2.month /\ d2.month <= 12)
  // Encoding d1 + n < d2 + n
  (ensures compare_dates (add_dates_months d1 n) (add_dates_months d2 n) < 0)
  (decreases %[in_same_year d1.month n; abs (d1.month + n)]) =
  let new1 = d1.month + n in
  let new2 = d2.month + n in
  if 1 <= new1 && new1 <= 12 then ()
  else if new1 > 12 then lemma5_month ({d1 with year = d1.year + 1}) (add_dates_months d2 12) (n - 12)
  else lemma5_month ({d1 with year = d1.year - 1}) (add_dates_months d2 (-12)) (n + 12)

(* Lemma 6: Monotonicity of day addition *)

/// Type abbreviation to avoid repeating the is_valid_date as a precondition
type valid_date = d:date{is_valid_date d}

#push-options "--z3rlimit 50"

// We need the following additional lemma, which states that day addition is associative
val lemma_add_dates_assoc (d:date{is_valid_date d}) (x1 x2:int)
  : Lemma (ensures
    add_dates_days_valid (add_dates_days_valid d x1) x2 == add_dates_days_valid d (x1 + x2))
    (decreases %[in_same_month d x1; abs (d.day + x1); abs (d.day)])

let rec lemma_add_dates_assoc d x1 x2 =
  let nb = Some?.v (nb_days d.month d.year) in
  let new1 = d.day + x1 in
  let new12 = d.day + (x1 + x2) in
  if 1 <= new1 && new1 <= nb then ()
  else if new1 >= nb then (
    let d' = add_dates_months d 1 in
    let x1' = x1 - (nb - d.day) - 1 in
    lemma_add_dates_assoc {d' with day = 1} x1' x2
  ) else (
    if 1 < d.day && new1 <= 0 then (
      lemma_add_dates_assoc {d with day = 1} (new1 - 1) x2
    ) else (
      let d' = add_dates_months d (-1) in
      lemma_add_dates_assoc {d' with day = 1} (x1 + Some?.v (nb_days d'.month d'.year)) x2
    )
  )
#pop-options


// Unclear why F* requires this much larger timeout for the following lemma.
// We split it into several sub-lemmas to help with VC generation
#push-options "--z3rlimit 50"
let lemma6_aux_case1 (d1 d2: valid_date) (n:int) : Lemma
  (requires (
    let v1 = add_dates_days d1 n in
    let nb1 = Some?.v (nb_days d1.month d1.year) in
    let new1 = d1.day + n in
    compare_dates d1 d2 < 0 /\
    (1 <= new1 && new1 <= nb1)
  ))
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  = ()
#pop-options

#push-options "--z3rlimit 500 --fuel 4 --ifuel 0"

let rec lemma6_aux (d1 d2: valid_date) (n:int) : Lemma
  (requires compare_dates d1 d2 < 0)
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  (decreases %[in_same_month d1 n; abs (d1.day + n); abs (d1.day); 1]) =
  let v1 = add_dates_days d1 n in
  let v2 = add_dates_days d2 n in
  let nb1 = Some?.v (nb_days d1.month d1.year) in
  let nb2 = Some?.v (nb_days d2.month d2.year) in
  let new1 = d1.day + n in
  let new2 = d2.day + n in
  if 1 <= new1 && new1 <= nb1 then lemma6_aux_case1 d1 d2 n
  else if new1 >= nb1 then lemma6_aux_case2 d1 d2 n
  else (
    if 1 < d1.day && new1 <= 0 then lemma6_aux_case3 d1 d2 n
    else lemma6_aux_case4 d1 d2 n
  )

and lemma6_aux_case2 (d1 d2: valid_date) (n:int) : Lemma
  (requires (
    let v1 = add_dates_days d1 n in
    let nb1 = Some?.v (nb_days d1.month d1.year) in
    let new1 = d1.day + n in
    compare_dates d1 d2 < 0 /\
    not (1 <= new1 && new1 <= nb1) /\
    (new1 >= nb1)
  ))
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  (decreases %[in_same_month d1 n; abs (d1.day + n); abs (d1.day); 0]) =
   let v1 = add_dates_days d1 n in
   let nb1 = Some?.v (nb_days d1.month d1.year) in
   let new1 = d1.day + n in
   let d1' = add_dates_months d1 1 in
   let n' = n - (nb1 - d1.day) - 1 in
   lemma_add_dates_assoc d2 (n - n') n';
   assert (add_dates_days (Some?.v (add_dates_days d2 (n - n'))) n' == add_dates_days d2 n);
   lemma6_aux {d1' with day = 1} (Some?.v (add_dates_days d2 (n - n'))) n'

and lemma6_aux_case3 (d1 d2: valid_date) (n:int) : Lemma
  (requires (
    let v1 = add_dates_days d1 n in
    let nb1 = Some?.v (nb_days d1.month d1.year) in
    let new1 = d1.day + n in
    compare_dates d1 d2 < 0 /\
    not (1 <= new1 && new1 <= nb1) /\
    not (new1 >= nb1) /\
    (1 < d1.day && new1 <= 0)
  ))
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  (decreases %[in_same_month d1 n; abs (d1.day + n); abs (d1.day); 0]) =
   let v1 = add_dates_days d1 n in
   let nb1 = Some?.v (nb_days d1.month d1.year) in
   let new1 = d1.day + n in
   let n' = new1 - 1 in
   lemma6_aux {d1 with day = 1} (Some?.v (add_dates_days d2 (n - n'))) n'


and lemma6_aux_case4 (d1 d2: valid_date) (n:int) : Lemma
  (requires (
    let v1 = add_dates_days d1 n in
    let nb1 = Some?.v (nb_days d1.month d1.year) in
    let new1 = d1.day + n in
    compare_dates d1 d2 < 0 /\
    not (1 <= new1 && new1 <= nb1) /\
    not (new1 >= nb1) /\
    not (1 < d1.day && new1 <= 0)
  ))
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  (decreases %[in_same_month d1 n; abs (d1.day + n); abs (d1.day); 0]) =
  let d1' = add_dates_months d1 (-1) in
  let nb' = Some?.v (nb_days d1'.month d1'.year) in
  let d1' = {d1' with day = 1} in
  let n' = n + nb' in
  let d2' = Some?.v (add_dates_days d2 (n - n')) in
  lemma6_aux d1' d2' n'


#pop-options

let lemma6 (d1 d2: valid_date) (n:int) : Lemma
  (requires compare_dates d1 d2 < 0)
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) < 0))
  = lemma6_aux d1 d2 n


(* A weaker version that also considers the case where both dates are equal *)
let lemma6_weak (d1 d2:date) (n:int) : Lemma
  (requires is_valid_date d1 /\ is_valid_date d2 /\ compare_dates d1 d2 <= 0)
  (ensures (
    let v1 = add_dates_days d1 n in
    let v2 = add_dates_days d2 n in
    Some? v1 /\ Some? v2 /\
    compare_dates (Some?.v v1) (Some?.v v2) <= 0))
  = if compare_dates d1 d2 = 0 then () // compare_dates = 0 corresponds to equality of dates
    else lemma6 d1 d2 n

(* Lemma 7: Monotonicity of rounding *)
let lemma7_up (d1 d2:date) : Lemma
  (requires Some? (round_up d1) /\ Some? (round_up d2) /\ compare_dates d1 d2 < 0)
  (ensures compare_dates (Some?.v (round_up d1)) (Some?.v (round_up d2)) <= 0)
  = ()

let lemma7_down (d1 d2:date) : Lemma
  (requires Some? (round_down d1) /\ Some? (round_down d2) /\ compare_dates d1 d2 < 0)
  (ensures compare_dates (Some?.v (round_down d1)) (Some?.v (round_down d2)) <= 0)
  = ()

(* Theorem 3: Monotonicity *)
let theorem3_up (d1:date {is_valid_date d1}) (d2: date{is_valid_date d2}) (p: period) : Lemma
  (requires compare_dates d1 d2 < 0) // Encoding d1 < d2
  (ensures Some? (add_up d1 p) /\ Some? (add_up d2 p) /\
           // Encoding d1 + p <= d2 + p
           compare_dates (Some?.v (add_up d1 p)) (Some?.v (add_up d2 p)) <= 0)
  = let v1 = add_up d1 p in
    let v2 = add_up d2 p in
    theorem2_up d1 p v1;
    theorem2_up d2 p v2;
    let v1_y = add_dates_years d1 p.years in
    let v1_m = add_dates_months v1_y p.months in
    let v1_rnd = round_up v1_m in
    let v2_y = add_dates_years d2 p.years in
    let v2_m = add_dates_months v2_y p.months in
    let v2_rnd = round_up v2_m in
    lemma5_year d1 d2 p.years;
    lemma5_month v1_y v2_y p.months;
    lemma7_up v1_m v2_m;
    lemma6_weak (Some?.v v1_rnd) (Some?.v v2_rnd) p.days

let theorem3_down (d1:date {is_valid_date d1}) (d2: date{is_valid_date d2}) (p: period) : Lemma
  (requires compare_dates d1 d2 < 0) // Encoding d1 < d2
  (ensures Some? (add_down d1 p) /\ Some? (add_down d2 p) /\
           // Encoding d1 + p <= d2 + p
           compare_dates (Some?.v (add_down d1 p)) (Some?.v (add_down d2 p)) <= 0)
  = let v1 = add_down d1 p in
    let v2 = add_down d2 p in
    theorem2_down d1 p v1;
    theorem2_down d2 p v2;
    let v1_y = add_dates_years d1 p.years in
    let v1_m = add_dates_months v1_y p.months in
    let v1_rnd = round_down v1_m in
    let v2_y = add_dates_years d2 p.years in
    let v2_m = add_dates_months v2_y p.months in
    let v2_rnd = round_down v2_m in
    lemma5_year d1 d2 p.years;
    lemma5_month v1_y v2_y p.months;
    lemma7_down v1_m v2_m;
    lemma6_weak (Some?.v v1_rnd) (Some?.v v2_rnd) p.days

(* Theorem 4: Rounding *)
let theorem_4_bot (d: date) (p:period) : Lemma
  (Some? (add_up d p) <==> Some? (add_down d p))
  = ()

let theorem_4_1 (d:date) (p:period) : Lemma
  // Having both is redundant, but we keep it for clarity
  (requires Some? (add_up d p) /\ Some? (add_down d p))
  (ensures compare_dates (Some?.v (add_down d p)) (Some?.v (add_up d p)) <= 0)
  = let v_m = add_dates_months (add_dates_years d p.years) p.months in
    let v_rnd_down = Some?.v (round_down v_m) in
    let v_rnd_up = Some?.v (round_up v_m) in
    assert (compare_dates v_rnd_down v_rnd_up <= 0);
    lemma6_weak v_rnd_down v_rnd_up p.days

let theorem_4_2 (d:date) (p:period) : Lemma
  (requires Some? (add_err d p))
  (ensures add_down d p == add_err d p /\ add_err d p == add_up d p)
  = ()

(* Theorem 5: Characterization of ambiguous month additions *)

(* Going through an auxiliary function as recursive calls do not always correspond
   to a valid d:date, although it is never a bottom case *)
let rec theorem5' (d:date) (n:int) (v: date) : Lemma
  (requires d.day >= 1 /\ add_dates_months d n == v)
  (ensures (
    let nb = nb_days v.month v.year in
    Some? nb /\
    (Some?.v nb < v.day <==> None? (round_err v))
  ))
  (decreases %[in_same_year d.month n; abs (d.month + n)])
  =
  let new_month = d.month + n in
  if 1 <= new_month && new_month <= 12 then ()
  else if new_month > 12 then (
    theorem5' {d with year = d.year + 1} (n - 12) v
  ) else
    theorem5' {d with year = d.year - 1} (n + 12) v

let theorem5 (d:date{is_valid_date d}) (n:int) (v: date) : Lemma
  (requires add_dates_months d n == v)
  (ensures (
    let nb = nb_days v.month v.year in
    Some? nb /\
    (Some?.v nb < v.day <==> None? (round_err v))
    )
  )
  = theorem5' d n v
