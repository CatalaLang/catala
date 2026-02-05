open Js_of_ocaml
open Catala_runtime_jsoo

class type default_ct = object
  method sort :
    Js.Unsafe.any Js.js_array Js.t Js.js_array Js.t Js.js_array Js.t ->
    Js.Unsafe.any Js.js_array Js.t Js.js_array Js.t Js.js_array Js.t Js.meth

  method split_by_month_ :
    date_jsoo Js.js_array Js.t ->
    date_jsoo Js.js_array Js.t Js.js_array Js.t Js.meth

  method split_by_year_ :
    integer_jsoo ->
    date_jsoo Js.js_array Js.t ->
    date_jsoo Js.js_array Js.t Js.js_array Js.t Js.meth
end

type default = default_ct Js.t

val default : default
