open Js_of_ocaml
open Dates_calc

class type date_jsoo_ct = object
  method year : int Js.prop
  method month : int Js.prop
  method day : int Js.prop
end

type date_jsoo = date_jsoo_ct Js.t

let date_to_jsoo : date -> date_jsoo =
 fun d ->
  let y, m, d = date_to_ymd d in
  object%js
    val mutable year = y
    val mutable month = m
    val mutable day = d
  end

let date_of_jsoo : date_jsoo -> date =
 fun js -> make_date ~year:js##.year ~month:js##.month ~day:js##.day

class type period_jsoo_ct = object
  method years : int Js.prop
  method months : int Js.prop
  method days : int Js.prop
end

type period_jsoo = period_jsoo_ct Js.t

let period_to_jsoo : period -> period_jsoo =
 fun p ->
  let y, m, d = period_to_ymds p in
  object%js
    val mutable years = y
    val mutable months = m
    val mutable days = d
  end

let period_of_jsoo : period_jsoo -> period =
 fun js -> make_period ~years:js##.years ~months:js##.months ~days:js##.days

type date_rounding_jsoo = Js.js_string Js.t

let date_rounding_to_jsoo x =
  Js.string
  @@
  match x with
  | RoundUp -> "RoundUp"
  | RoundDown -> "RoundDown"
  | AbortOnRound -> "AbortOnRound"

let date_rounding_of_jsoo js =
  match Js.to_string js with
  | "RoundUp" -> RoundUp
  | "RoundDown" -> RoundDown
  | "AbortOnRound" -> AbortOnRound
  | s -> invalid_arg (Format.sprintf "unknown case in enum: %S" s)

let make_date_jsoo year month day = date_to_jsoo (make_date ~year ~month ~day)

let add_dates_jsoo d p round =
  let round = Option.map date_rounding_of_jsoo (Js.Optdef.to_option round) in
  date_to_jsoo (add_dates ?round (date_of_jsoo d) (period_of_jsoo p))

let sub_dates_jsoo d1 d2 =
  period_to_jsoo (sub_dates (date_of_jsoo d1) (date_of_jsoo d2))

let compare_dates_jsoo d1 d2 = compare_dates (date_of_jsoo d1) (date_of_jsoo d2)
let date_to_ymd_jsoo d = Js.array [| d##.year; d##.month; d##.day |]

let date_to_string_jsoo js =
  Js.string (Format.asprintf "%a" format_date (date_of_jsoo js))

let date_of_string_jsoo s = date_to_jsoo (date_of_string (Js.to_string s))

let first_day_of_month_jsoo d =
  date_to_jsoo (first_day_of_month (date_of_jsoo d))

let last_day_of_month_jsoo d = date_to_jsoo (last_day_of_month (date_of_jsoo d))
let is_leap_year_jsoo y = Js.bool (is_leap_year y)

let make_period_jsoo years months days =
  period_to_jsoo (make_period ~years ~months ~days)

let neg_period_jsoo p = period_to_jsoo (neg_period (period_of_jsoo p))

let add_periods_jsoo p1 p2 =
  period_to_jsoo (add_periods (period_of_jsoo p1) (period_of_jsoo p2))

let sub_periods_jsoo p1 p2 =
  period_to_jsoo (sub_periods (period_of_jsoo p1) (period_of_jsoo p2))

let mul_period_jsoo p i = period_to_jsoo (mul_period (period_of_jsoo p) i)
let period_of_string_jsoo s = period_to_jsoo (period_of_string (Js.to_string s))

let period_to_string_jsoo js =
  Js.string (Format.asprintf "%a" format_period (period_of_jsoo js))

let period_to_days_jsoo p = period_to_days (period_of_jsoo p)
let period_to_ymds_jsoo p = Js.array [| p##.years; p##.months; p##.days |]

let () =
  Js.export "Dates_calc"
    (object%js
       method make_date_ y m d = make_date_jsoo y m d
       method add_dates_ d p r = add_dates_jsoo d p r
       method sub_dates_ d1 d2 = sub_dates_jsoo d1 d2
       method compare_dates_ d1 d2 = compare_dates_jsoo d1 d2
       method date_to_ymd_ d = date_to_ymd_jsoo d
       method date_of_string_ s = date_of_string_jsoo s
       method date_to_string_ s = date_to_string_jsoo s
       method first_day_of_month_ d = first_day_of_month_jsoo d
       method last_day_of_month_ d = last_day_of_month_jsoo d
       method is_leap_year_ y = is_leap_year_jsoo y
       method make_period_ y m d = make_period_jsoo y m d
       method neg_period_ p = neg_period_jsoo p
       method add_periods_ p1 p2 = add_periods_jsoo p1 p2
       method sub_periods_ p1 p2 = sub_periods p1 p2
       method mul_period_ p i = mul_period_jsoo p i
       method period_of_string_ s = period_of_string_jsoo s
       method period_to_string_ s = period_to_string_jsoo s
       method period_to_days_ p = period_to_days_jsoo p
       method period_to_ymds_ p = period_to_ymds_jsoo p
    end)
