open Z3
open Arithmetic

(** Round Z3 [Real] to Z3 [Integer] using the same strategy as [Runtime.round]:
    round to nearest, half away from zero. *)
let z3_round ctx (q : Expr.expr) : Expr.expr =
  (* The mathematical formula is [round(q) = sgn(q) * floor(abs(q) + 0.5)].
     However, Z3 does not have [sgn] or [abs] functions. Instead, we encode it
     as [round(q) = if q>=0 then floor(q + 1/2) else -floor(-q + 1/2)], where
     [Real.mk_real2int] floors. *)
  (* NOTE why not define Z3 function? =>> they are the same as declaration +
     forall, and the gain in legibility is marginal, so I don't think it is
     necessary *)
  let zero = Real.mk_numeral_i ctx 0 in
  let is_positive = mk_ge ctx q zero in

  let half = Real.mk_numeral_nd ctx 1 2 in

  let shift_pos = mk_add ctx [q; half] in
  let round_pos = Real.mk_real2int ctx shift_pos in

  let shift_neg = mk_add ctx [mk_unary_minus ctx q; half] in
  let round_neg = mk_unary_minus ctx (Real.mk_real2int ctx shift_neg) in

  Boolean.mk_ite ctx is_positive round_pos round_neg

(* Coerce an integer or a real [x] into a real *)
let z3_force_real ctx (x : Expr.expr) : Expr.expr =
  let one = Real.mk_numeral_i ctx 1 in
  let q = mk_mul ctx [one; x] in
  assert (is_real q);
  q
