[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime


module Text : sig
  type t
  val rtype: t Value.ty
end


(** Toplevel definition loc *)
val loc : code_location array

(** Toplevel definition foo *)
val foo : Text.t

(** Toplevel definition bar *)
val bar : Text.t

val fortytwo : Text.t

(** Toplevel definition of_int *)
val of_int : integer -> Text.t
