open Js_of_ocaml
open Catala_runtime_jsoo
open Stdlib_internals.Period_internal

let sort_jsoo a =
  let a =
    Array.map (fun a ->
        let x = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 0 in
        let y = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 1 in
        let start =
          date_of_jsoo @@ Option.get @@ Js.Optdef.to_option @@ Js.array_get x 0
        in
        let stop =
          date_of_jsoo @@ Option.get @@ Js.Optdef.to_option @@ Js.array_get x 1
        in
        (start, stop), y)
    @@ Js.to_array a
  in
  let a = sort a in
  Js.array
  @@ Array.map
       (fun ((start, stop), b) ->
         Js.array [| Js.array [| date_to_jsoo start; date_to_jsoo stop |]; b |])
       a

let split_by_month_jsoo a =
  let start = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 0 in
  let stop = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 1 in
  Js.array
    (Array.map
       (fun (d1, d2) -> Js.array [| date_to_jsoo d1; date_to_jsoo d2 |])
       (split_by_month (date_of_jsoo start, date_of_jsoo stop)))

let split_by_year_jsoo start_month a =
  let start_month = integer_of_jsoo start_month in
  let start = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 0 in
  let stop = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 1 in
  Js.array
    (Array.map
       (fun (d1, d2) -> Js.array [| date_to_jsoo d1; date_to_jsoo d2 |])
       (split_by_year start_month (date_of_jsoo start, date_of_jsoo stop)))

let () =
  Js.export "Period_internal"
    (object%js
       method sort a = sort_jsoo a
       method split_by_month_ a = split_by_month_jsoo a
       method split_by_year_ s a = split_by_year_jsoo s a
    end)
