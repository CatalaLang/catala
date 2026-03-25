(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Services
include Resto_directory.Make (Resto_json.Encoding)

let rec repeat i json = if i <= 0 then [] else json :: repeat (i - 1) json

let dir = empty

let dir =
  register1 dir repeat_service (fun i () json ->
      Lwt.return (`Ok (`A (repeat i json))))

let dir = register1 dir add_service (fun i () j -> Lwt.return (`Ok (i + j)))

let dir =
  register2 dir alternate_add_service (fun i j () () ->
      Lwt.return (`Ok (float_of_int i +. j)))

let dir =
  register dir alternate_add_service' (fun (((), i), j) () () ->
      Lwt.return (`Ok (i + int_of_float j)))

let dir =
  register dir alternate_add_service_patch (fun (((), i), j) () _ ->
      Lwt.return (`Ok (int_of_float (float_of_int i +. j))))

let dir =
  register dir alternate_add_service_delete (fun (((), _), _) () () ->
      Lwt.return (`Ok ()))

let dir =
  register
    dir
    dummy_service
    (fun ((((((((), _a), _b), _c), _d), _e), _f), _g) () () ->
      Lwt.return (`Ok ()))

let dir =
  register_dynamic_directory1 dir prefix_dir1 (fun _ ->
      let prefixed_dir =
        register2 empty minus_service (fun i j () () ->
            Lwt.return (`Ok (i -. float_of_int j)))
      in
      Lwt.return prefixed_dir)

let dir = register_describe_directory_service dir describe_service

let dir =
  register1 dir sum_service (fun xs () () ->
      Lwt.return (`Ok (List.fold_left ( + ) 0 xs)))

let add_tail_conflict () =
  ignore (register1 dir sum_service (fun _ () () -> assert false))

let add_type_conflict () =
  ignore
    (register2 dir alternate_add_service_inverted (fun _ _ () () ->
         assert false))
