open Js_of_ocaml
open Dates_calc

class type date_jsoo_ct = object
  method year : int Js.prop
  method month : int Js.prop
  method day : int Js.prop
end

type date_jsoo = Js.Unsafe.any

let date_to_jsoo : date -> date_jsoo =
 fun d -> Js.Unsafe.inject (Js.string (Format.asprintf "%a" format_date d))

let date_of_jsoo : date_jsoo -> date =
 fun js ->
  match Js.to_string (Js.typeof js) with
  | "string" -> date_of_string (Js.to_string (Js.Unsafe.coerce js))
  | "object" ->
    let js = Js.Unsafe.coerce js in
    make_date ~year:js##.year ~month:js##.month ~day:js##.day
  | s -> failwith (Format.sprintf "%S cannot be converted to date" s)

class type period_jsoo_ct = object
  method years : int Js.optdef Js.prop
  method months : int Js.optdef Js.prop
  method days : int Js.optdef Js.prop
end

type period_jsoo = period_jsoo_ct Js.t

let period_to_jsoo : period -> period_jsoo =
 fun p ->
  let y, m, d = period_to_ymds p in
  object%js
    val mutable years = if y = 0 then Js.undefined else Js.def y
    val mutable months = if m = 0 then Js.undefined else Js.def m
    val mutable days = if d = 0 then Js.undefined else Js.def d
  end

let period_of_jsoo : period_jsoo -> period =
 fun js ->
  let years = Option.value ~default:0 @@ Js.Optdef.to_option js##.years in
  let months = Option.value ~default:0 @@ Js.Optdef.to_option js##.months in
  let days = Option.value ~default:0 @@ Js.Optdef.to_option js##.days in
  make_period ~years ~months ~days

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

let date_to_ymd_jsoo d =
  let d = date_of_jsoo d in
  let y, m, d = date_to_ymd d in
  Js.array [| y; m; d |]

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

let period_to_ymds_jsoo p =
  let years = Option.value ~default:0 @@ Js.Optdef.to_option p##.years in
  let months = Option.value ~default:0 @@ Js.Optdef.to_option p##.months in
  let days = Option.value ~default:0 @@ Js.Optdef.to_option p##.days in
  Js.array [| years; months; days |]

class type default_ct = object
  method make_date_ : int -> int -> int -> date_jsoo Js.meth

  method add_dates_ :
    date_jsoo ->
    period_jsoo ->
    date_rounding_jsoo Js.optdef ->
    date_jsoo Js.meth

  method sub_dates_ : date_jsoo -> date_jsoo -> period_jsoo Js.meth
  method compare_dates_ : date_jsoo -> date_jsoo -> int Js.meth
  method date_to_ymd_ : date_jsoo -> int Js.js_array Js.t Js.meth
  method date_of_string_ : Js.js_string Js.t -> date_jsoo Js.meth
  method date_to_string_ : date_jsoo -> Js.js_string Js.t Js.meth
  method first_day_of_month_ : date_jsoo -> date_jsoo Js.meth
  method last_day_of_month_ : date_jsoo -> date_jsoo Js.meth
  method is_leap_year_ : int -> bool Js.t Js.meth
  method make_period_ : int -> int -> int -> period_jsoo Js.meth
  method neg_period_ : period_jsoo -> period_jsoo Js.meth
  method add_periods_ : period_jsoo -> period_jsoo -> period_jsoo Js.meth
  method sub_periods_ : period_jsoo -> period_jsoo -> period_jsoo Js.meth
  method mul_period_ : period_jsoo -> int -> period_jsoo Js.meth
  method period_of_string_ : Js.js_string Js.t -> period_jsoo Js.meth
  method period_to_string_ : period_jsoo -> Js.js_string Js.t Js.meth
  method period_to_days_ : period_jsoo -> int Js.meth
  method period_to_ymds_ : period_jsoo -> int Js.js_array Js.t Js.meth
end

type default = default_ct Js.t

let default : default =
  object%js
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
    method sub_periods_ p1 p2 = sub_periods_jsoo p1 p2
    method mul_period_ p i = mul_period_jsoo p i
    method period_of_string_ s = period_of_string_jsoo s
    method period_to_string_ s = period_to_string_jsoo s
    method period_to_days_ p = period_to_days_jsoo p
    method period_to_ymds_ p = period_to_ymds_jsoo p
  end

let () = Js.export "Dates_calc" default
