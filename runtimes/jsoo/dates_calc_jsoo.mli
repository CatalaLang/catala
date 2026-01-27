open Js_of_ocaml
open Dates_calc

class type date_jsoo_ct = object
  method year : int Js.prop
  method month : int Js.prop
  method day : int Js.prop
end

type date_jsoo = date_jsoo_ct Js.t

val date_to_jsoo : date -> date_jsoo
val date_of_jsoo : date_jsoo -> date

class type period_jsoo_ct = object
  method years : int Js.prop
  method months : int Js.prop
  method days : int Js.prop
end

type period_jsoo = period_jsoo_ct Js.t

val period_to_jsoo : period -> period_jsoo
val period_of_jsoo : period_jsoo -> period

type date_rounding_jsoo = Js.js_string Js.t

val date_rounding_to_jsoo : date_rounding -> date_rounding_jsoo
val date_rounding_of_jsoo : date_rounding_jsoo -> date_rounding
val make_date_jsoo : int -> int -> int -> date_jsoo

val add_dates_jsoo :
  date_jsoo -> period_jsoo -> date_rounding_jsoo Js.optdef -> date_jsoo

val sub_dates_jsoo : date_jsoo -> date_jsoo -> period_jsoo
val compare_dates_jsoo : date_jsoo -> date_jsoo -> int
val date_to_ymd_jsoo : date_jsoo -> int Js.js_array Js.t
val date_of_string_jsoo : Js.js_string Js.t -> date_jsoo
val date_to_string_jsoo : date_jsoo -> Js.js_string Js.t
val first_day_of_month_jsoo : date_jsoo -> date_jsoo
val last_day_of_month_jsoo : date_jsoo -> date_jsoo
val is_leap_year_jsoo : int -> bool Js.t
val make_period_jsoo : int -> int -> int -> period_jsoo
val neg_period_jsoo : period_jsoo -> period_jsoo
val add_periods_jsoo : period_jsoo -> period_jsoo -> period_jsoo
val sub_periods_jsoo : period_jsoo -> period_jsoo -> period_jsoo
val mul_period_jsoo : period_jsoo -> int -> period_jsoo
val period_of_string_jsoo : Js.js_string Js.t -> period_jsoo
val period_to_string_jsoo : period_jsoo -> Js.js_string Js.t
val period_to_days_jsoo : period_jsoo -> int
val period_to_ymds_jsoo : period_jsoo -> int Js.js_array Js.t
