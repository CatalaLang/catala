[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime


module String : sig
  type t
  val rtype: t Value.ty
end


(** Toplevel definition loc *)
val loc : code_location array

(** Toplevel definition foo *)
val foo : String.t

(** Toplevel definition bar *)
val bar : String.t

val fortytwo : String.t

(** Toplevel definition of_int *)
val of_int : integer -> String.t
