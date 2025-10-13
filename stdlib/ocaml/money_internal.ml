open Catala_runtime

(* Toplevel round_to_decimal *)
let round_to_decimal : money -> integer -> money =
 fun m n ->
  let n_int = Z.to_int n in
  if n_int >= 2 then m
  else
    let ten = Z.of_int 10 in
    if n_int = 1 then Z.(money_round (m * ten) / ten)
    else
      (* let () = assert (n_int <= 0) in *)
      let pow_10 = Z.pow ten (-n_int) in
      Z.(money_round (m / pow_10) * pow_10)

let () =
  Catala_runtime.register_module "Money_internal"
    ["round_to_decimal", Obj.repr round_to_decimal]
    "*external*"
