open Js_of_ocaml
open Catala_runtime_jsoo

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

val default : default
