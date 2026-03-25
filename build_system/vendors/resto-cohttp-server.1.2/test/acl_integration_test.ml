(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lwt.Infix
module Encoding = Resto_json.Encoding
module Service = Resto.MakeService (Encoding)
module Directory = Resto_directory.Make (Encoding)
module Media_type = Resto_cohttp.Media_type.Make (Encoding)

let json =
  {
    Media_type.name = Cohttp.Accept.MediaType ("application", "json");
    q = Some 1000;
    pp =
      (fun _enc ppf raw ->
        let json = Ezjsonm.from_string raw in
        Format.fprintf ppf "%s" (Ezjsonm.to_string json));
    construct =
      (fun enc v ->
        Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
        @@ Json_encoding.construct enc v);
    construct_seq =
      (fun enc v ->
        let item =
          Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
          @@ Json_encoding.construct enc v
        in
        Seq.return (Bytes.of_string item, 0, String.length item));
    destruct =
      (fun enc body ->
        let json = Ezjsonm.from_string body in
        try Ok (Json_encoding.destruct enc json)
        with exc -> Error (Printexc.to_string exc));
  }

let media_types = [json]

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

let foo_bar =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "bar"]

let get_foo_bar =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:Encoding.unit
    ~error:Encoding.unit
    foo_bar

let foo_blah =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "blah"]

let get_foo_blah =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:Encoding.unit
    ~error:Encoding.unit
    foo_blah

let bwraf =
  let open Resto.Path in
  List.fold_left add_suffix root ["blue"; "white"; "red"; "and"; "fuchsia"]

let post_bwraf =
  Service.post_service
    ~query:Resto.Query.empty
    ~input:Encoding.unit
    ~output:Encoding.unit
    ~error:Encoding.unit
    bwraf

let directory =
  let open Directory in
  let dir = empty in
  let dir = register0 dir get_foo_bar (fun () () -> Lwt.return @@ `Ok ()) in
  let dir = register0 dir get_foo_blah (fun () () -> Lwt.return @@ `Ok ()) in
  let dir = register0 dir post_bwraf (fun () () -> Lwt.return @@ `Ok ()) in
  dir

let port = 8000

let uri = "http://localhost:8000"

let child expect_foo_bar expect_foo_blah expect_bwraf =
  let module Client =
    Resto_cohttp_client.Client.Make
      (Encoding)
      (Resto_cohttp_client.Client.OfCohttp (Cohttp_lwt_unix.Client))
  in
  let logger = Client.full_logger Format.err_formatter in
  let base = Uri.of_string uri in
  let open Lwt.Infix in
  Client.call_service media_types ~base ~logger get_foo_bar () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_foo_bar) ;
  Client.call_service media_types ~base ~logger get_foo_blah () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_foo_blah) ;
  Client.call_service media_types ~base ~logger post_bwraf () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_bwraf) ;
  Stdlib.exit 0

let parent pid =
  Lwt_unix.waitpid [] pid >>= function
  | _, WEXITED 0 -> Lwt.return ()
  | _ -> assert false

module Server = Resto_cohttp_server.Server.Make (Encoding) (Logger)

(* A signal setup with both soft and hard exits to test both behaviours *)
let main () =
  (* set up and start the server *)
  let open Lwt.Infix in
  let server = Server.init_server ~media_types directory in
  Server.launch server (`TCP (`Port port)) >>= fun () ->
  (* first test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Allow_all
       {except = [{meth = Any; path = Exact [Literal "foo"; Wildcard]}]} ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Unauthorized None) (`Unauthorized None) (`Ok (Some ()))
  | pid -> parent pid)
  >>= fun () ->
  (* second test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {except = [{meth = Any; path = FollowedByAnySuffix [Literal "foo"]}]} ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Ok (Some ())) (`Ok (Some ())) (`Unauthorized None)
  | pid -> parent pid)
  >>= fun () ->
  (* third test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {
         except =
           [
             {meth = Exact `GET; path = FollowedByAnySuffix [Literal "foo"]};
             {meth = Exact `DELETE; path = FollowedByAnySuffix []};
           ];
       } ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Ok (Some ())) (`Ok (Some ())) (`Unauthorized None)
  | pid -> parent pid)
  >>= fun () ->
  (* fourth test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {
         except =
           [
             {
               meth = Exact `POST;
               path =
                 FollowedByAnySuffix
                   [
                     Literal "blue";
                     Literal "white";
                     Wildcard;
                     Wildcard;
                     Literal "fuchsia";
                   ];
             };
           ];
       } ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Unauthorized None) (`Unauthorized None) (`Ok (Some ()))
  | pid -> parent pid)
  >>= fun () ->
  (* end of tests *)
  Lwt.return ()

let () =
  Lwt_main.run @@ main () ;
  Stdlib.exit 0
