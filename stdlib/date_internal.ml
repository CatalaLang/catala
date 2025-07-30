open Catala_runtime
open Dates_calc

(* Toplevel def of_ymd *)
let of_ymd : code_location -> integer -> integer -> integer -> date =
 fun pos y m d ->
  try Dates.make_date ~year:(Z.to_int y) ~month:(Z.to_int m) ~day:(Z.to_int d)
  with Dates.InvalidDate ->
    (* FIXME: add specific error *)
    raise (Error (UncomparableDurations, [pos]))

(* Toplevel def to_ymd *)
let to_ymd : date -> integer * integer * integer =
 fun (d : date) ->
  let y, m, d = Dates.date_to_ymd d in
  Z.of_int y, Z.of_int m, Z.of_int d

(* Toplevel def last_day_of_month *)
let last_day_of_month : date -> date =
 fun (d : date) -> Dates.last_day_of_month d

let () =
  Catala_runtime.register_module "Date_internal"
    [
      "of_ymd", Obj.repr of_ymd;
      "to_ymd", Obj.repr to_ymd;
      "last_day_of_month", Obj.repr last_day_of_month;
    ]
    "*external*"
