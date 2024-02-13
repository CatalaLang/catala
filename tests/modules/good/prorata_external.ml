open Runtime_ocaml.Runtime
open Oper

[@@@ocaml.warning "-4-26-27-32-41-42"]

let mzero = money_of_units_int 0

let prorata_ : money -> (money array) -> (money array) =
  fun (amount: money) (weights: money array) ->
  let w_total = Array.fold_left o_add_mon_mon mzero weights in
  let rem, a =
    Array.fold_left_map (fun rem w ->
        let r = o_mult_mon_rat amount (o_div_mon_mon w w_total) in
        o_sub_mon_mon rem r, r)
      amount weights
  in
  a.(Array.length a - 1) <- o_add_mon_mon a.(Array.length a - 1) rem;
  a

let prorata2_ : money -> (money array) -> (money array) =
  fun (amount: money) (weights: money array) ->
  let w_total = Array.fold_left o_add_mon_mon mzero weights in
  let _, a =
    Array.fold_left_map (fun (rem_amount, rem_weights) w ->
        let r =
          o_mult_mon_rat
            rem_amount
            (o_div_mon_mon w rem_weights) in
        (o_sub_mon_mon rem_amount r, o_sub_mon_mon rem_weights w), r)
      (amount, w_total) weights
  in
  a

let () =
  Runtime_ocaml.Runtime.register_module "Prorata_external"
    [ "prorata", Obj.repr prorata_;
      "prorata2", Obj.repr prorata2_ ]
    "todo-module-hash"
