(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let () = Printexc.record_backtrace true

open Lwt.Infix
module Encoding = Resto_json.Encoding
module Service = Resto.MakeService (Encoding)
module Directory = Resto_directory.Make (Encoding)
module Media_type = Resto_cohttp.Media_type.Make (Encoding)

(* This tests that the server and client of resto manage to communicate even
   when the output is chunked. *)

let seqing chunk_size s =
  let b = Bytes.unsafe_of_string s in
  let rec aux offset () =
    if offset + chunk_size > Bytes.length b then
      Seq.Cons ((b, offset, Bytes.length b - offset), Seq.empty)
    else Seq.Cons ((b, offset, chunk_size), aux (offset + chunk_size))
  in
  aux 0

let json chunk_size =
  let construct enc v =
    Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
    @@ Json_encoding.construct enc v
  in
  {
    Media_type.name = Cohttp.Accept.MediaType ("application", "json");
    q = Some 1000;
    pp =
      (fun _enc ppf raw ->
        let json = Ezjsonm.from_string raw in
        Format.fprintf ppf "%s" (Ezjsonm.to_string json));
    construct;
    construct_seq =
      (fun enc v ->
        let item = construct enc v in
        seqing chunk_size item);
    destruct =
      (fun enc body ->
        let json = Ezjsonm.from_string body in
        try Ok (Json_encoding.destruct enc json)
        with exc -> Error (Printexc.to_string exc));
  }

let media_types chunk_size = [json chunk_size]

module Logger : Resto_cohttp_server.Server.LOGGING = struct
  let debug fmt = Format.kasprintf (Format.printf "[DEBUG]: %s\n%!") fmt

  let log_info fmt = Format.kasprintf (Printf.printf "[INFO]: %s\n%!") fmt

  let log_notice fmt = Format.kasprintf (Printf.printf "[NOTICE]: %s\n%!") fmt

  let warn fmt = Format.kasprintf (Printf.printf "[WARN]: %s\n%!") fmt

  let log_error fmt = Format.kasprintf (Printf.eprintf "[ERROR]: %s\n%!") fmt

  let lwt_debug fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[DEBUG]: %s\n%!") fmt

  let lwt_log_info fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[INFO]: %s\n%!") fmt

  let lwt_log_notice fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[NOTICE]: %s\n%!") fmt

  let lwt_warn fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[WARN]: %s\n%!") fmt

  let lwt_log_error fmt =
    Format.kasprintf Lwt_fmt.(fprintf stderr "[ERROR]: %s\n%!") fmt
end

let kv_list_encoding =
  let open Json_encoding in
  list (tup2 string int32)

let foo_bar =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "bar"]

let get_foo_bar =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:kv_list_encoding
    ~error:Encoding.unit
    foo_bar

let val_foo_bar = []

let foo_blah =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "blah"]

let get_foo_blah =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:kv_list_encoding
    ~error:Encoding.unit
    foo_blah

let val_foo_blah = List.init 10 (fun i -> (string_of_int i, Int32.of_int i))

let bwraf =
  let open Resto.Path in
  List.fold_left add_suffix root ["blue"; "white"; "red"; "and"; "fuchsia"]

let post_bwraf =
  Service.post_service
    ~query:Resto.Query.empty
    ~input:Encoding.unit
    ~output:kv_list_encoding
    ~error:Encoding.unit
    bwraf

let val_bwraf top = List.init top (fun i -> (string_of_int i, Int32.of_int i))

let directory =
  let open Directory in
  let dir = empty in
  let dir =
    register0 dir get_foo_bar (fun () () -> Lwt.return @@ `OkChunk val_foo_bar)
  in
  let dir =
    register0 dir get_foo_blah (fun () () ->
        Lwt.return @@ `OkChunk val_foo_blah)
  in
  let dir =
    register0 dir post_bwraf (fun () () -> Lwt.return @@ `OkChunk (val_bwraf 7))
  in
  dir

let port = 8001

let uri = "http://localhost:8001"

let is_ok_result r v =
  match r with `Ok (Some w) -> assert (v = w) | _ -> assert false

let child () =
  let module Client =
    Resto_cohttp_client.Client.Make
      (Encoding)
      (Resto_cohttp_client.Client.OfCohttp (Cohttp_lwt_unix.Client))
  in
  let logger = Client.full_logger Format.err_formatter in
  let base = Uri.of_string uri in
  let media_types = media_types 1 in
  let open Lwt.Infix in
  Client.call_service media_types ~base ~logger get_foo_bar () () ()
  >>= fun (_, _, r) ->
  is_ok_result r val_foo_bar ;
  Client.call_service media_types ~base ~logger get_foo_blah () () ()
  >>= fun (_, _, r) ->
  is_ok_result r val_foo_blah ;
  Client.call_service media_types ~base ~logger post_bwraf () () ()
  >>= fun (_, _, r) ->
  is_ok_result r (val_bwraf 7) ;
  Stdlib.exit 0

module Server = Resto_cohttp_server.Server.Make (Encoding) (Logger)

let parent pid chunk_size =
  let media_types = media_types chunk_size in
  Server.init_and_launch ~media_types directory (`TCP (`Port port))
  >>= fun () ->
  Lwt_unix.waitpid [] pid >>= function
  | _, WEXITED 0 -> Lwt.return ()
  | _ -> assert false

let test_one_size chunk_size =
  match Lwt_unix.fork () with
  | 0 -> (
      match Lwt_unix.fork () with
      | 0 ->
          Lwt_unix.sleep 2.0 (* leave time for the server to start *)
          >>= fun () -> child ()
      | pid -> parent pid chunk_size)
  | pid -> (
      Lwt_unix.waitpid [] pid >>= function
      | _, WEXITED 0 -> Lwt.return ()
      | _ -> assert false)

let main () =
  (* test smaller and smaller, more and more numerous chunks *)
  let open Lwt.Infix in
  Printf.printf "Testing chunking with size %d\n%!" (16 * 1024) ;
  test_one_size (16 * 1024) >>= fun () ->
  Printf.printf "Testing chunking with size %d\n%!" 64 ;
  test_one_size 64 >>= fun () ->
  Printf.printf "Testing chunking with size %d\n%!" 1 ;
  test_one_size 1 >>= fun () -> Lwt.return_unit

let () =
  Lwt_main.run @@ main () ;
  Stdlib.exit 0
