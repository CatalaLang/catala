(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

open Catala_runtime
module Dates = Dates_calc.Dates

[@@@ocaml.warning "-4-26-27-32-41-42"]

let cmp = Dates.compare_dates

(* Toplevel def sort *)
let sort : ((date * date) * 'a) array -> ((date * date) * 'a) array =
 fun arr ->
  let ret = Array.copy arr in
  Array.sort (fun ((beg1, _), _) ((beg2, _), _) -> cmp beg1 beg2) ret;
  ret

let one_month = Dates.make_period ~years:0 ~months:1 ~days:0

(* Toplevel def split_by_month *)
let split_by_month : date * date -> (date * date) array =
 fun (start, stop) ->
  let rec split start =
    let next = Dates.add_dates (Dates.first_day_of_month start) one_month in
    if cmp stop next < 0 then [start, stop] else (start, next) :: split next
  in
  split start |> Array.of_list

let one_year = Dates.make_period ~years:1 ~months:0 ~days:0

let first_day_of_rolling_year date start_month =
  let year, month, _ = Dates.date_to_ymd date in
  let year = if month < start_month then year - 1 else year in
  Dates.make_date ~year ~month:start_month ~day:1

(* Toplevel def split_by_year *)
let split_by_year : integer -> date * date -> (date * date) array =
 fun start_month (start, stop) ->
  let start_month = integer_to_int start_month in
  assert (1 <= start_month && start_month <= 12);
  let rec split start =
    let next =
      Dates.add_dates (first_day_of_rolling_year start start_month) one_year
    in
    if cmp stop next < 0 then [start, stop] else (start, next) :: split next
  in
  split start |> Array.of_list

let () =
  Catala_runtime.register_module "Period_internal"
    [
      "sort", Obj.repr sort;
      "split_by_month", Obj.repr split_by_month;
      "split_by_year", Obj.repr split_by_year;
    ]
    "*external*"
