open Z3
open Expr
open FuncDecl

val z3_round_expr : context -> expr -> expr
(** Round Z3 [Real] to Z3 [Integer] using the same strategy as [Runtime.round]:
    round to nearest, half away from zero. *)

val z3_round_func : func_decl -> expr -> expr
(** Round Z3 [Real] to Z3 [Integer] using a pre-declared function. Said function
    should be defined using {!val:z3_round_expr} *)

val z3_force_real : context -> expr -> expr
(** Coerce an [Integer] or a [Real] into a [Real]. An assertion validates the
    type of the result. *)

val z3_int_of_bigint : context -> Z.t -> expr
(** Translate a Bigint to a Z3 [Integer] while avoiding overflow *)
