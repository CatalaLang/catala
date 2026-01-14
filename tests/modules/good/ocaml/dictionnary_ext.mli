(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module Dictionnary : sig
  type t
end


(** Toplevel definition empty *)
val empty : Dictionnary.t

(** Toplevel definition store *)
val store : Dictionnary.t -> integer -> money -> Dictionnary.t

(** Toplevel definition find *)
val find : Dictionnary.t -> integer -> money Optional.t
