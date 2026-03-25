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

open Fixtures.Services
open Fixtures.Directory
open Resto_directory
open Lwt.Infix

let () =
  Lwt_main.run
    (allowed_methods dir () ["foo"; "3"; "repeat"] >>= function
     | Ok [`POST] -> Lwt.return_unit
     | _ -> assert false)

let () =
  Lwt_main.run
    (allowed_methods dir () ["bar"; "3"; "4"; "add"] >>= function
     | Ok [`GET; `POST] -> Lwt.return_unit
     | _ -> assert false)

module Test (Request : sig
  val request :
    ('meth, unit, 'params, 'query, 'input, 'output, 'error) Service.t ->
    'params ->
    'query ->
    'input ->
    [> ('output, 'error) Answer.t] Lwt.t
end) =
struct
  let () =
    Lwt_main.run
      (Request.request describe_service ((), ["foo"; "3"]) {recurse = true} ()
       >>= function
       | `Ok dir ->
           Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
           Lwt.return_unit
       | _ -> assert false)

  let () =
    Lwt_main.run
      (Request.request
         describe_service
         ((), ["bar"; "3"; "2."; "add"])
         {recurse = false}
         ()
       >>= function
       | `Ok dir ->
           Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
           Lwt.return_unit
       | _ -> assert false)

  let () =
    Lwt_main.run
      (Request.request
         describe_service
         ((), ["bar"; "3"; "2."])
         {recurse = true}
         ()
       >>= function
       | `Ok dir ->
           Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
           Lwt.return_unit
       | _ -> assert false)

  let () =
    Lwt_main.run
      (Request.request describe_service ((), []) {recurse = true} ()
       >>= function
       | `Ok dir ->
           Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
           Lwt.return_unit
       | _ -> assert false)

  let () =
    let test service args arg expected =
      Lwt_main.run (Request.request service args () arg) = `Ok expected
    in
    assert (test repeat_service ((), 3) (`A []) (`A (repeat 3 (`A [])))) ;
    assert (test add_service ((), 2) 3 5) ;
    assert (test alternate_add_service (((), 1), 2.5) () 3.5) ;
    assert (test real_minus_service1 (((), 2.5), 1) () 1.5) ;
    assert (test alternate_add_service' (((), 1), 2.) () 3) ;
    ()
end

let split_path path =
  let l = String.length path in
  let rec do_slashes acc i =
    if i >= l then List.rev acc
    else if path.[i] = '/' then do_slashes acc (i + 1)
    else do_component acc i i
  and do_component acc i j =
    if j >= l then
      if i = j then List.rev acc else List.rev (String.sub path i (j - i) :: acc)
    else if path.[j] = '/' then do_slashes (String.sub path i (j - i) :: acc) j
    else do_component acc i (j + 1)
  in
  do_slashes [] 0

module Faked = Test (struct
  (** Testing faked client/server communication. *)
  let request (type i) (service : (_, _, _, _, i, _, _) Service.t) params query
      arg =
    let {Service.meth; uri; input} =
      Service.forge_request service params query
    in
    Format.eprintf "\nREQUEST: %a@." Uri.pp_hum uri ;
    let path = split_path (Uri.path uri) in
    let query =
      List.map (fun (n, vs) -> (n, String.concat "," vs)) (Uri.query uri)
    in
    let json =
      match input with
      | Service.No_input -> `O []
      | Service.Input input -> Json_encoding.construct input arg
    in
    lookup dir () meth path >>= function
    | Ok (Service s) -> (
        let query = Resto.Query.parse s.types.query query in
        (match s.types.input with
        | Service.No_input -> s.handler query ()
        | Service.Input input ->
            s.handler query @@ Json_encoding.destruct input json)
        >>= function
        | `Ok res ->
            let json = Json_encoding.construct s.types.output res in
            Lwt.return
              (`Ok
                (Json_encoding.destruct (Service.output_encoding service) json))
        | _ -> failwith "Unexpected lwt result (1)")
    | _ -> failwith "Unexpected lwt result (2)"
end)

module Transparent = Test (struct
  let request x = transparent_lookup dir x
end)

let () = Printf.printf "\n### OK Resto ###\n\n%!"

module Path = struct
  open Resto

  let test_to_segments ?(msg = "") exptd given =
    Alcotest.(check (list string) msg exptd (Path.to_segments given))

  let test_to_string ?(msg = "") exptd given =
    Alcotest.(check string msg exptd (Path.to_string given))

  let tests =
    [
      ( "to_segments should give the good segment list",
        fun () ->
          test_to_segments ~msg:"root" [] Path.root ;
          test_to_segments
            ~msg:"static_and_dynamic"
            ["foo"; "<int>"; "bar"; "<float>"]
            Path.(root / "foo" /: Arg.int / "bar" /: Arg.float) ;
          test_to_segments
            ~msg:"dynamic_tail"
            ["foo"; "<int>*"]
            Path.(root / "foo" /:* Arg.int) );
      ( "to_string should give a well formated string",
        fun () ->
          test_to_string ~msg:"root" "/" Path.root ;
          test_to_string
            ~msg:"static_and_dynamic"
            "/foo/<int>/bar/<float>"
            Path.(root / "foo" /: Arg.int / "bar" /: Arg.float) ;
          test_to_string
            ~msg:"dynamic_tail"
            "/foo/<int>*"
            Path.(root / "foo" /:* Arg.int) );
    ]
end

let () = Alcotest.run "resto" [("Path", Util.do_test Path.tests)]
