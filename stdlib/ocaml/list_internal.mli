(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

val sequence : integer -> integer -> integer array
(** Toplevel definition sequence *)

val nth_element : 't array -> integer -> 't Optional.t
(** Toplevel definition nth_element *)

val remove_nth_element : 't array -> integer -> 't array
(** Toplevel definition remove_nth_element *)

val reverse : 't array -> 't array
(** Toplevel definition reverse *)
