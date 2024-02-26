open Catala_utils
open Shared_ast

module RuntimeError : sig
  type span = string option * Pos.t
  type span_list = span list

  type runtime_error =
    | EmptyError
    | ConflictError of { spans : span_list }
    | DivisionByZeroError of { spans : span_list }
    | AssertionError

  type message = string

  type t = {
    except : runtime_error; (* TODO use actual exceptions from [Runtime]? *)
    message : message; (* TODO use formatted stuff instead *)
  }
end

module SymbExpr : sig
  type z3_expr = Z3.Expr.expr
  type reentrant = { name : StructField.t; symbol : z3_expr }

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_error of RuntimeError.t (* only for generic errors *)

  (** {2 Builders} *)

  val mk_z3 : z3_expr -> t
  val mk_reentrant : StructField.t -> z3_expr -> t
  val none : t
  val mk_emptyerror : RuntimeError.message -> t
  val mk_conflicterror : RuntimeError.message -> RuntimeError.span_list -> t

  val mk_divisionbyzeroerror :
    RuntimeError.message -> RuntimeError.span_list -> t

  val mk_assertionerror : RuntimeError.message -> t

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

  (* FIXME remove [to_string] once its not used anymore *)
  val to_string : ?typed:bool -> t -> string
  val formatter : Format.formatter -> t -> unit
  val formatter_typed : Format.formatter -> t -> unit
end
