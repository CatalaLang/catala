open Catala_runtime_jsoo
open Money_internal

let round_to_decimal_jsoo m n =
  money_to_jsoo (round_to_decimal (money_of_jsoo m) (integer_of_jsoo n))

class type default_ct = object
  method round_to_decimal_ :
    money_jsoo -> integer_jsoo -> money_jsoo Js_of_ocaml.Js.meth
end

type default = default_ct Js_of_ocaml.Js.t

let default : default =
  object%js
    method round_to_decimal_ m n = round_to_decimal_jsoo m n
  end

let () = Js_of_ocaml.Js.export "Money_internal" default
