open Catala_utils
open Symb_expr

type s_expr = SymbExpr.z3_expr
type soft_id = string
type soft = { symb : s_expr; weight : int; id : soft_id }
type reentrant = { symb : SymbExpr.reentrant; is_empty : bool }

type pc_expr =
  | Pc_z3 of s_expr
  | Pc_soft of soft
  | Pc_reentrant of reentrant
  | Pc_incomplete

module Set : Set.S with type elt = pc_expr

type naked_pc = { expr : pc_expr; pos : Pos.t; branch : bool }
type 'a path
type naked_path = unit path

type annotation =
  | Negated
      (** the path constraint that has been negated to generate a new input *)
  | Done
      (** a path node that has been explored should, and whose constraint should
          not be negated *)
  | Normal

type annotated_path = annotation path
type annotated_pc = annotation * naked_pc
type 'a incremental_action = IncrPush of 'a | IncrPop of 'a
type incremental_annotated_pc = (annotation * naked_pc) incremental_action
type incremental_pc_expr = pc_expr incremental_action

val empty : naked_path
val empty_annotated : annotated_path
val singleton : naked_pc -> naked_path
val constraints : 'a path -> ('a * naked_pc) list
val pop : 'a path -> (('a * naked_pc) * 'a path) option
val is_incomplete : naked_pc -> bool
val add : naked_path -> naked_pc -> naked_path
val add_all : naked_path -> naked_pc list -> naked_path

val append : naked_path -> naked_path -> naked_path
(** [append new_p p] appends [new_p] constraints to the existing [p]
    constraints, filtering existing ones. *)

val rev_append : naked_path -> naked_path -> naked_path
(** same as append but reverse the first path argument *)

val filter : (naked_pc -> bool) -> naked_path -> naked_path
val exists : (naked_pc -> bool) -> naked_path -> bool
val length : 'a path -> int
val is_empty : 'a path -> bool

(** {2 Builders} *)

val mk_z3 : SymbExpr.t -> Pos.t -> bool -> naked_pc
val mk_soft : SymbExpr.t -> int -> soft_id option -> Pos.t -> bool -> naked_pc
val mk_reentrant : SymbExpr.t -> s_expr -> Pos.t -> bool -> naked_pc option

(** {2 Path operations} *)

val compare_paths :
  annotated_path -> naked_path -> annotated_path * incremental_annotated_pc list
(** Compare the path of the previous evaluation and the path of the current
    evaluation. If a constraint was previously marked as Done or Normal, then
    check that it stayed the same. If it was previously marked as Negated, thus
    if it was negated before the two evaluations, then check that the concrete
    value was indeed negated and mark it Done. If there are new constraints
    after the last one, add them as Normal. Crash in other cases. *)

val make_expected_path :
  annotated_path -> annotated_path * incremental_annotated_pc list
(** Remove Done paths until a Normal (not yet negated) constraint is found, then
    mark this branch as Negated. This function shall be called on an output of
    [compare_paths], and thus no Negated constraint should appear in its input.
*)

(** {2 Printing} *)

val format_pc_expr : Format.formatter -> pc_expr -> unit
val format_naked_path : Format.formatter -> naked_path -> unit
val format_annotated_path : Format.formatter -> annotated_path -> unit
