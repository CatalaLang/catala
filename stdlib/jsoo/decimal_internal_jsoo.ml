open Catala_runtime_jsoo
open Decimal_internal

let round_to_decimal_jsoo m n =
  decimal_to_jsoo @@ round_to_decimal (decimal_of_jsoo m) (integer_of_jsoo n)

class type default_ct = object
  method round_to_decimal_ :
    decimal_jsoo -> integer_jsoo -> decimal_jsoo Js_of_ocaml.Js.meth
end

type default = default_ct Js_of_ocaml.Js.t

let default : default =
  object%js
    method round_to_decimal_ m n = round_to_decimal_jsoo m n
  end

let () = Js_of_ocaml.Js.export "Decimal_internal" default
