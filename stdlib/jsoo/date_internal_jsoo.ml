open Js_of_ocaml
open Catala_runtime_jsoo
open Date_internal

let of_ymd_jsoo pos y m d =
  date_to_jsoo
  @@ of_ymd
       (code_location_of_jsoo pos)
       (integer_of_jsoo y) (integer_of_jsoo m) (integer_of_jsoo d)

let to_ymd_jsoo d =
  let y, m, d = to_ymd (date_of_jsoo d) in
  Js.array [| integer_to_jsoo y; integer_to_jsoo m; integer_to_jsoo d |]

let last_day_of_month_jsoo = Dates_calc_jsoo.last_day_of_month_jsoo

let add_rounded_down_jsoo d dur =
  date_to_jsoo
  @@ Dates_calc.add_dates ~round:Dates_calc.RoundDown (date_of_jsoo d)
       (duration_of_jsoo dur)

let add_rounded_up_jsoo d dur =
  date_to_jsoo
  @@ Dates_calc.add_dates ~round:Dates_calc.RoundUp (date_of_jsoo d)
       (duration_of_jsoo dur)

class type default_ct = object
  method of_ymd_ :
    code_location_jsoo ->
    integer_jsoo ->
    integer_jsoo ->
    integer_jsoo ->
    date_jsoo Js.meth

  method to_ymd_ : date_jsoo -> integer_jsoo Js.js_array Js.t Js.meth
  method last_day_of_month_ : date_jsoo -> date_jsoo Js.meth
  method add_rounded_down_ : date_jsoo -> duration_jsoo -> date_jsoo Js.meth
  method add_rounded_up_ : date_jsoo -> duration_jsoo -> date_jsoo Js.meth
end

type default = default_ct Js.t

let default : default =
  object%js
    method of_ymd_ pos y m d = of_ymd_jsoo pos y m d
    method to_ymd_ d = to_ymd_jsoo d
    method last_day_of_month_ d = last_day_of_month_jsoo d
    method add_rounded_down_ d dur = add_rounded_down_jsoo d dur
    method add_rounded_up_ d dur = add_rounded_up_jsoo d dur
  end

let () = Js.export "Date_internal" default
