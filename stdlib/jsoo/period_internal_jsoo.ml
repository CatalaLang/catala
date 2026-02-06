open Js_of_ocaml
open Catala_runtime_jsoo
open Period_internal

let sort_jsoo a =
  let a =
    Array.map (fun a ->
        let x = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 0 in
        let y = Option.get @@ Js.Optdef.to_option @@ Js.array_get a 1 in
        let start =
          date_of_jsoo
          @@ Js.Unsafe.coerce
          @@ Option.get
          @@ Js.Optdef.to_option
          @@ Js.array_get x 0
        in
        let stop =
          date_of_jsoo
          @@ Js.Unsafe.coerce
          @@ Option.get
          @@ Js.Optdef.to_option
          @@ Js.array_get x 1
        in
        (start, stop), Js.Unsafe.coerce y)
    @@ Js.to_array a
  in
  let a = sort a in
  Js.array
  @@ Array.map
       (fun ((start, stop), b) ->
         Js.array
           [|
             Js.array
               [|
                 Js.Unsafe.inject @@ date_to_jsoo start;
                 Js.Unsafe.inject @@ date_to_jsoo stop;
               |];
             Js.Unsafe.coerce b;
           |])
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

class type default_ct = object
  method sort :
    Js.Unsafe.any Js.js_array Js.t Js.js_array Js.t Js.js_array Js.t ->
    Js.Unsafe.any Js.js_array Js.t Js.js_array Js.t Js.js_array Js.t Js.meth

  method split_by_month_ :
    date_jsoo Js.js_array Js.t ->
    date_jsoo Js.js_array Js.t Js.js_array Js.t Js.meth

  method split_by_year_ :
    integer_jsoo ->
    date_jsoo Js.js_array Js.t ->
    date_jsoo Js.js_array Js.t Js.js_array Js.t Js.meth
end

type default = default_ct Js.t

let default : default =
  object%js
    method sort a = sort_jsoo a
    method split_by_month_ a = split_by_month_jsoo a
    method split_by_year_ s a = split_by_year_jsoo s a
  end

let () = Js.export "Period_internal" default
