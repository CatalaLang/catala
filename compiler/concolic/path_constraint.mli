open Catala_utils
open Symb_expr

module PathConstraint : sig
  type s_expr = SymbExpr.z3_expr
  type reentrant = { symb : SymbExpr.reentrant; is_empty : bool }
  type pc_expr = Pc_z3 of s_expr | Pc_reentrant of reentrant

  (* path constraint cannot be empty (this looks like a GADT but it would be
     overkill I think) *)
  type naked_pc = { expr : pc_expr; pos : Pos.t; branch : bool }
  type naked_path = naked_pc list

  type annotated_pc =
    | Negated of naked_pc
        (** the path constraint that has been negated to generate a new input *)
    | Done of naked_pc
        (** a path node that has been explored should, and whose constraint
            should not be negated *)
    | Normal of naked_pc  (** all other constraints *)

  type annotated_path = annotated_pc list

  (** {2 Builders} *)

  val mk_z3 : SymbExpr.t -> Pos.t -> bool -> naked_pc
  val mk_reentrant : SymbExpr.t -> s_expr -> Pos.t -> bool -> naked_pc option

  (** {2 Path operations} *)

  val compare_paths : annotated_path -> naked_path -> annotated_path
  (** Compare the path of the previous evaluation and the path of the current
      evaluation. If a constraint was previously marked as Done or Normal, then
      check that it stayed the same. If it was previously marked as Negated,
      thus if it was negated before the two evaluations, then check that the
      concrete value was indeed negated and mark it Done. If there are new
      constraints after the last one, add them as Normal. Crash in other cases. *)

  val make_expected_path : annotated_path -> annotated_path
  (** Remove Done paths until a Normal (not yet negated) constraint is found,
      then mark this branch as Negated. This function shall be called on an
      output of [compare_paths], and thus no Negated constraint should appear in
      its input. *)

  (** {2 Printing} *)

  val print_path_constraints : naked_path -> unit
  val print_annotated_path_constraints : annotated_path -> unit
end
