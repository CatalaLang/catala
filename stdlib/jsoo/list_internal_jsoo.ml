open Js_of_ocaml
open Catala_runtime_jsoo
open List_internal

let sequence_jsoo start stop =
  Js.array
  @@ Array.map integer_to_jsoo
  @@ sequence (integer_of_jsoo start) (integer_of_jsoo stop)

let nth_element_jsoo arr n =
  Optional.to_jsoo Fun.id @@ nth_element (Js.to_array arr) (integer_of_jsoo n)

let remove_nth_element_jsoo arr n =
  Js.array @@ remove_nth_element (Js.to_array arr) (integer_of_jsoo n)

let reverse_jsoo arr = Js.array (reverse (Js.to_array arr))

let () =
  Js.export "List_internal"
    (object%js
       method sequence start stop = sequence_jsoo start stop
       method nth_element_ arr n = nth_element_jsoo arr n
       method remove_nth_element_ arr n = remove_nth_element_jsoo arr n
       method reverse arr = reverse_jsoo arr
    end)
