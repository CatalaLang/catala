open Catala_runtime

val of_ymd : code_location -> integer -> integer -> integer -> date
(** Toplevel definition of_ymd *)

val to_ymd : date -> integer * integer * integer
(** Toplevel definition to_ymd *)

val last_day_of_month : date -> date
(** Toplevel definition last_day_of_month *)
