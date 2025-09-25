open Catala_runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]

val sort : ((date * date) * 'a) array -> ((date * date) * 'a) array
(** Toplevel definition sort *)

val split_by_month : date * date -> (date * date) array
(** Toplevel definition split_by_month *)

val split_by_year : integer -> date * date -> (date * date) array
(** Toplevel definition split_by_year *)
