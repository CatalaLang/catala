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

class type default_ct = object
  method sequence :
    integer_jsoo -> integer_jsoo -> integer_jsoo Js.js_array Js.t Js.meth

  method nth_element_ :
    Js.Unsafe.any Js.js_array Js.t ->
    integer_jsoo ->
    Js.Unsafe.any Optional.jsoo Js.meth

  method remove_nth_element_ :
    Js.Unsafe.any Js.js_array Js.t ->
    integer_jsoo ->
    Js.Unsafe.any Js.js_array Js.t Js.meth

  method reverse :
    Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.js_array Js.t Js.meth
end

type default = default_ct Js.t

let default : default =
  object%js
    method sequence start stop = sequence_jsoo start stop
    method nth_element_ arr n = nth_element_jsoo arr n
    method remove_nth_element_ arr n = remove_nth_element_jsoo arr n
    method reverse arr = reverse_jsoo arr
  end

let () = Js.export "List_internal" default
