open Catala_runtime

(* Toplevel round_to_decimal *)
let round_to_decimal : decimal -> integer -> decimal =
 fun m n ->
  if n = Z.zero then Q.of_bigint (round m)
  else
    let pow_10 = Z.(pow (of_int 10) (to_int (Z.abs n))) |> Q.of_bigint in
    if Z.(Compare.(n > zero)) then
      let r = round (Q.mul m pow_10) |> Q.of_bigint in
      Q.div r pow_10
    else
      let r = round (Q.div m pow_10) |> Q.of_bigint in
      Q.mul r pow_10

let () =
  Catala_runtime.register_module "Decimal_internal"
    ["round_to_decimal", Obj.repr round_to_decimal]
    "*external*"
