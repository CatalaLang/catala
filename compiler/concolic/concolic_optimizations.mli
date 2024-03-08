open Path_constraint

type flag = OTrivial

val optim_list : (string * flag) list
(** Used for command line arguments *)

val remove_trivial_constraints :
  flag list -> PathConstraint.naked_path -> PathConstraint.naked_path
