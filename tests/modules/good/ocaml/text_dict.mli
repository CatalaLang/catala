(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module Text_dict : sig
  type t
end


(** Toplevel definition empty *)
val empty : Text_dict.t

(** Toplevel definition store *)
val store : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t -> Text_dict.t

(** Toplevel definition find *)
val find : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t Optional.t
