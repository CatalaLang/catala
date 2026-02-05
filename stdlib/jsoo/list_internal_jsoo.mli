open Js_of_ocaml
open Catala_runtime_jsoo

class type default_ct = object
  method sequence :
    integer_jsoo -> integer_jsoo -> integer_jsoo Js.js_array Js.t Js.meth

  method nth_element_ :
    Js.Unsafe.any Js.js_array Js.t ->
    integer_jsoo ->
    Js.Unsafe.any Optional.jsoo Js.meth

  method remove_nth_element_ :
    Js.Unsafe.any Js.js_array Js.t ->
    integer_jsoo ->
    Js.Unsafe.any Js.js_array Js.t Js.meth

  method reverse :
    Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.js_array Js.t Js.meth
end

type default = default_ct Js.t

val default : default
