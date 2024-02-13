open Z3

val z3_round : context -> Expr.expr -> Expr.expr
(** Round Z3 [Real] to Z3 [Integer] using the same strategy as [Runtime.round]:
    round to nearest, half away from zero. *)

val z3_force_real : context -> Expr.expr -> Expr.expr
(** Coerce an [Integer] or a [Real] into a [Real]. An assertion validates the
    type of the result. *)

val z3_int_of_bigint : context -> Z.t -> Expr.expr
(** Translate a Bigint to a Z3 [Integer] while avoiding overflow *)
