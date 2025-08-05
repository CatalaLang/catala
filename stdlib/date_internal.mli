open Catala_runtime

(** Toplevel definition of_ymd *)
val of_ymd : integer -> integer -> integer -> date

(** Toplevel definition to_ymd *)
val to_ymd : date -> (integer * integer * integer)

(** Toplevel definition last_day_of_month *)
val last_day_of_month : date -> date
