open Catala_runtime_jsoo
open Money_internal

let round_to_decimal_jsoo m n =
  money_to_jsoo (round_to_decimal (money_of_jsoo m) (integer_of_jsoo n))

let () =
  Js_of_ocaml.Js.export "Money_internal"
    (object%js
       method round_to_decimal_ m n = round_to_decimal_jsoo m n
    end)
