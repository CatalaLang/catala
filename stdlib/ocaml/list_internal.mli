(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime



(** Toplevel definition sequence *)
val sequence : integer -> integer -> (integer array)

(** Toplevel definition element *)
val element : ('t array) -> integer -> ('t) Optional.t
