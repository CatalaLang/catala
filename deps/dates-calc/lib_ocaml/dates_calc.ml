(* This file is part of the Dates_calc library. Copyright (C) 2022 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Aymeric Fromherz
   <aymeric.fromherz@inria.fr>, Raphaël Monat <raphael.monat@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

[@@@warning "-27"]


type date = { year : int; month : int; day : int }
(** A valid date in the standard Gregorian calendar. *)

type period = { years : int; months : int; days : int }
(** A period can be any number and combination of days, months, years. *)

exception InvalidDate
exception AmbiguousComputation

type date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound
      (** When choosing [AbortOnRound], functions may raise
          [AmbiguousComputation]. *)

(** {2 Functions on periods}*)
let make_period ~(years : int) ~(months : int) ~(days : int) : period =
  { years; months; days }

let format_period (fmt : Format.formatter) (p : period) : unit =
  Format.fprintf fmt "[%d years, %d months, %d days]" p.years p.months p.days

let period_of_string str =
  try
    Scanf.sscanf str
      "[%d years, %d months, %d days]"
      (fun years months days -> make_period ~years ~months ~days)
  with Scanf.Scan_failure _ -> invalid_arg "period_of_string"

let add_periods (d1 : period) (d2 : period) : period =
  {
    years = d1.years + d2.years;
    months = d1.months + d2.months;
    days = d1.days + d2.days;
  }

let sub_periods (d1 : period) (d2 : period) : period =
  {
    years = d1.years - d2.years;
    months = d1.months - d2.months;
    days = d1.days - d2.days;
  }

let mul_period (d1 : period) (m : int) : period =
  { years = d1.years * m; months = d1.months * m; days = d1.days * m }

(** @raise [AmbiguousComputation]
      when the period is anything else than a number of days. *)
let period_to_days (p : period) : int =
  if p.years <> 0 || p.months <> 0 then raise AmbiguousComputation else p.days

(** {2 Functions on dates}*)

let is_leap_year (year : int) : bool =
  year mod 400 = 0 || (year mod 4 = 0 && year mod 100 <> 0)

(** @raise [InvalidDate]*)
let days_in_month ~(month : int) ~(is_leap_year : bool) : int =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year then 29 else 28
  | _ -> raise InvalidDate

let is_valid_date (d : date) : bool =
  try
    d.day >= 1
    && d.day <= days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year)
  with InvalidDate -> false

(** @raise [InvalidDate]*)
let make_date ~(year : int) ~(month : int) ~(day : int) : date =
  let d = { year; month; day } in
  if is_valid_date d then d else raise InvalidDate

(** Returns new [year, month]. Precondition: [1 <= month <= 12] *)
let rec add_months_to_first_of_month_date
    ~(year : int)
    ~(month : int)
    ~(months : int) : int * int =
  let new_month = month + months in
  if 1 <= new_month && new_month <= 12 then year, new_month
  else if new_month > 12 then
    add_months_to_first_of_month_date ~year:(year + 1) ~month
      ~months:(months - 12)
  else
    (* new_month <= 0 *)
    add_months_to_first_of_month_date ~year:(year - 1) ~month
      ~months:(months + 12)

(* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the last
   day of the current month. *)
let prev_valid_date (d : date) : date =
  assert (1 <= d.month && d.month <= 12);
  assert (1 <= d.day && d.day <= 31);
  if is_valid_date d then d
  else
    {
      d with
      day = days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year);
    }

(* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the
   first day of the next month. *)
let next_valid_date (d : date) : date =
  assert (1 <= d.month && d.month <= 12);
  assert (1 <= d.day && d.day <= 31);
  if is_valid_date d then d
  else
    let new_year, new_month =
      add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months:1
    in
    { year = new_year; month = new_month; day = 1 }

let round_date ~(round : date_rounding) (new_date : date) =
  if is_valid_date new_date then new_date
  else
    match round with
    | AbortOnRound -> raise AmbiguousComputation
    | RoundDown -> prev_valid_date new_date
    | RoundUp -> next_valid_date new_date

(** This function is only ever called from `add_dates` below.
    Hence, any call to `add_dates_years` will be followed by a call
    to `add_dates_month`. We therefore perform a single rounding
    in `add_dates_month`, to avoid introducing additional imprecision here,
    and to ensure that adding n years + m months is always equivalent to
    adding (12n + m) months *)
let add_dates_years ~(round : date_rounding) (d : date) (years : int) : date =
  { d with year = d.year + years }

let add_dates_month ~(round : date_rounding) (d : date) (months : int) : date =
  let new_year, new_month =
    add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months
  in
  let new_date = { d with year = new_year; month = new_month } in
  round_date ~round new_date


let rec add_dates_days (d : date) (days : int) =
  (* Hello, dear reader! Buckle up because it will be a hard ride. The first
     thing to do here is to retrieve how many days there are in the current
     month of [d]. *)
  let days_in_d_month =
    days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year)
  in
  (* Now, we case analyze of the situation. To do that, we add the current days
     of the month with [days], and see what happens. Beware, [days] is algebraic
     and can be negative! *)
  let new_day = d.day + days in
  if 1 <= new_day && new_day <= days_in_d_month then
    (* The first case is the easy one: when you add [days], the new day keeps
       being a valid day in the current month. All is good, we simply warp to
       that new date without any further changes. *)
    { d with day = new_day }
  else if new_day >= days_in_d_month then
    (* Now, we deal with the case where there is an overflow : you have added
       too many days and the current month cannot handle them any more. The
       strategy here is to fill the current month, and let the next month handle
       the situation via a recursive call. *)
    let new_year, new_month =
      add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months:1
    in
    add_dates_days
      (* We warp to the first day of the next month! *)
      { year = new_year; month = new_month; day = 1 }
      (* Now we compute how many days we still have left to add. Because we have
         warped to the next month, we already have added the rest of the days in
         the current month: [days_in_d_month - d.day]. But then we switch
         months, and that corresponds to adding another day. *)
      (days - (days_in_d_month - d.day) - 1)
  else
    (* The last case is symmetrical, we substracted too many days and the
       current month can't handle it. So we warp to the previous month and let a
       recursive call handle the situation from there. *)
    let new_year, new_month =
      add_months_to_first_of_month_date ~year:d.year ~month:d.month ~months:(-1)
    in
    add_dates_days
      (* We warp to the last day of the previous month. *)
      {
        year = new_year;
        month = new_month;
        day =
          days_in_month ~month:new_month ~is_leap_year:(is_leap_year new_year);
      }
      (* What remains to be substracted (as [days] is negative) has to be
         diminished by the number of days of the date in the current month. *)
      (days + d.day)

(** @raise [AmbiguousComputation] *)
let add_dates ?(round : date_rounding = AbortOnRound) (d : date) (p : period) :
    date =
  let d = add_dates_years ~round d p.years in
  (* NB: after add_dates_years, the date may not be correct.
     Rounding will be performed later, by add_dates_month *)
  let d = add_dates_month ~round d p.months in
  let d = add_dates_days d p.days in
  d

let compare_dates (d1 : date) (d2 : date) : int =
  if Int.compare d1.year d2.year = 0 then
    if Int.compare d1.month d2.month = 0 then Int.compare d1.day d2.day
    else Int.compare d1.month d2.month
  else Int.compare d1.year d2.year

(** Respects ISO8601 format. *)
let format_date (fmt : Format.formatter) (d : date) : unit =
  Format.fprintf fmt "%04d-%02d-%02d" d.year d.month d.day

let date_of_string str =
  try
    Scanf.sscanf str "%04d-%02d-%02d"
      (fun year month day -> make_date ~year ~month ~day)
  with Scanf.Scan_failure _ -> invalid_arg "date_of_string"

let first_day_of_month (d : date) : date =
  assert(is_valid_date d);
  make_date ~year:d.year ~month:d.month ~day:1

let last_day_of_month (d : date) : date =
  assert(is_valid_date d);
  let days_month = days_in_month ~month:d.month ~is_leap_year:(is_leap_year d.year) in
  make_date ~year:d.year ~month:d.month ~day:days_month

let neg_period (p : period) : period =
  { years = -p.years; months = -p.months; days = -p.days }

(** The returned [period] is always expressed as a number of days. *)
let rec sub_dates (d1 : date) (d2 : date) : period =
  if d1.year = d2.year && d1.month = d2.month then
    (* Easy case: the two dates are in the same month. *)
    make_period ~years:0 ~months:0 ~days:(d1.day - d2.day)
  else
    (* Otherwise we'll add a month forward if d2 is after d1.*)
    let cmp = compare_dates d1 d2 in
    if cmp < 0 then
      (* The case were d1 is after d2 is symmetrical so we handle it via a
         recursive call changing the order of the arguments. *)
      neg_period (sub_dates d2 d1)
    else
      (* we know cmp != 0 so cmp > 0*)
      (* We warp d2 to the first day of the next month. *)
      let new_d2_year, new_d2_month =
        add_months_to_first_of_month_date ~year:d2.year ~month:d2.month
          ~months:1
      in
      let new_d2 = { year = new_d2_year; month = new_d2_month; day = 1 } in
      (* Next we divide the result between the number of days we've added to go
         to the end of the month, and the remaining handled by a recursive
         call. *)
      add_periods
        (make_period ~years:0 ~months:0
           ~days:
             (* The number of days is the difference between the last day of the
                month and the current day of d1, plus one day because we go to
                the next month. *)
             (days_in_month ~month:d2.month ~is_leap_year:(is_leap_year d2.year)
             - d2.day + 1))
        (sub_dates d1 new_d2)

let date_to_ymd (d : date) : int * int * int = d.year, d.month, d.day

let period_to_ymds (p : period) : int * int * int = p.years, p.months, p.days
