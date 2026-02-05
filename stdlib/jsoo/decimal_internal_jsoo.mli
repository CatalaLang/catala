open Catala_runtime_jsoo

class type default_ct = object
  method round_to_decimal_ :
    decimal_jsoo -> integer_jsoo -> decimal_jsoo Js_of_ocaml.Js.meth
end

type default = default_ct Js_of_ocaml.Js.t

val default : default
