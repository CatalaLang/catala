open Catala_utils
open Shared_ast

module RuntimeError : sig
  type t = {
    error : Catala_runtime.error;
    pos : Pos.t;
    extra_pos : string Mark.pos list;
  }
end

module SymbExpr : sig
  type z3_expr = Z3.Expr.expr
  type z3_symbol = Z3.Symbol.symbol
  type reentrant = { name : StructField.t; symbol : z3_expr }

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_incomplete
    | Symb_abs
    | Symb_error of RuntimeError.t (* only for generic errors *)

  (** {2 Builders} *)

  val mk_z3 : z3_expr -> t
  val mk_reentrant : StructField.t -> z3_expr -> t
  val is_reentrant : t -> bool
  val none : t
  val incomplete : t
  val abs : t
  val mk_emptyerror : Pos.t -> t
  val mk_conflicterror : Pos.t -> string Mark.pos list -> t
  val mk_divisionbyzeroerror : Pos.t -> string Mark.pos list -> t
  val mk_assertionerror : Pos.t -> t

  (** {2 Operations} *)

  val map_z3 : (z3_expr -> z3_expr) -> t -> t
  (** [map_z3 f x] applies [f] to the Z3 expression if [x] is [Symb_z3],
      otherwise does not change [x]. *)

  val app_z3 : (z3_expr -> z3_expr) -> t -> t
  (** [map_z3 f x] applies [f] to the Z3 expression if [x] is [Symb_z3], raises
      [Invalid_arg] otherwise. *)

  val app2_z3 : (z3_expr -> z3_expr -> z3_expr) -> t -> t -> t
  (** same as {!val:app_z3} but with 2 arguments. Fails if either is not
      [Symb_z3]. *)

  val applist_z3 : (z3_expr list -> z3_expr) -> t list -> t
  (** same as {!val:app_z3} but with a list of expressions. Fails if any is not
      [Symb_z3]. *)

  val map_none : none:t -> t -> t
  (** [map_none ~none x] returns [none] if [x] is [Symb_none], [x] otherwise. *)

  val simplify : t -> t
  (** Call Z3 simplification function if possible. *)

  (** {2 Printing} *)

  val to_string : ?typed:bool -> t -> string
  val format_sexpr : Format.formatter -> z3_expr -> unit
  val format : Format.formatter -> t -> unit
  val format_typed : Format.formatter -> t -> unit
end
