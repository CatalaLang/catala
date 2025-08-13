(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

open Runtime_ocaml.Runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]


module Str : sig
  type t = {fld: integer}
end

module En : sig type t = Str of Str.t | Empty of unit end


(** Toplevel definition str_with_default *)
val str_with_default : En.t -> Str.t
