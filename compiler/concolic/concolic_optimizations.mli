open Path_constraint

type flag = OTrivial | OLazyDefault

val optim_list : (string * flag) list
(** Used for command line arguments *)

val lazy_default : flag list -> bool

val remove_trivial_constraints :
  flag list -> PathConstraint.naked_path -> PathConstraint.naked_path
