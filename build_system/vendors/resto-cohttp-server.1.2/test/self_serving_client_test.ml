(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Resto
module Encoding = Resto_json.Encoding
module Media_type = Resto_cohttp.Media_type.Make (Encoding)

module NullLogger : Resto_cohttp_server.Server.LOGGING = struct
  let debug fmt = Format.kasprintf ignore fmt

  let log_info fmt = Format.kasprintf ignore fmt

  let log_notice fmt = Format.kasprintf ignore fmt

  let warn fmt = Format.kasprintf ignore fmt

  let log_error fmt = Format.kasprintf ignore fmt

  let lwt_debug fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_info fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_notice fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_warn fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt

  let lwt_log_error fmt = Format.kasprintf (fun _ -> Lwt.return_unit) fmt
end

module Self_serving_client =
  Resto_cohttp_self_serving_client.Self_serving_client.Make
    (Encoding)
    (NullLogger)

include Resto_directory.Make (Encoding)
module Service = MakeService (Resto_json.Encoding)
open Service

let seqing s =
  let chunk_size = 32 in
  let b = Bytes.unsafe_of_string s in
  let rec aux offset () =
    if offset + chunk_size > Bytes.length b then
      Seq.Cons ((b, offset, Bytes.length b - offset), Seq.empty)
    else Seq.Cons ((b, offset, chunk_size), aux (offset + chunk_size))
  in
  aux 0

let json : Media_type.t =
  let to_string ?(newline = false) ?minify j =
    Format.asprintf
      "%a%s"
      Json_repr.(pp ?compact:minify (module Json_repr.Ezjsonm))
      j
      (if newline then "\n" else "")
  in
  let from_string s =
    match Ezjsonm.from_string ("[" ^ s ^ "]") with
    | exception Ezjsonm.Parse_error (_, msg) -> Error msg
    | `A [json] -> Ok json
    | _ -> Error "Malformed value"
  in
  let construct enc v =
    let x : Json_repr.ezjsonm = Json_encoding.construct enc v in
    to_string ~newline:true ~minify:true x
  in
  {
    name = Cohttp.Accept.MediaType ("application", "json");
    q = Some 1000;
    pp =
      (fun _enc ppf raw ->
        match from_string raw with
        | Error err ->
            Format.fprintf
              ppf
              "@[Invalid JSON:@  - @[<v 2>Error:@ %s@] - @[<v 2>Raw data:@ \
               %s@]@]"
              err
              raw
        | Ok json -> Json_repr.(pp (module Ezjsonm) ppf json));
    construct;
    construct_seq = (fun enc v -> seqing @@ construct enc v);
    destruct =
      (fun enc body ->
        match from_string body with
        | Error _ as err -> err
        | Ok json -> Ok (Json_encoding.destruct enc json));
  }

let media_types = [json]

let repeat_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.any_ezjson_value
    ~output:Json_encoding.any_ezjson_value
    ~error:Json_encoding.empty
    Path.(root / "foo" /: Arg.int / "repeat")

let add_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.int
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "foo" /: Arg.int / "add")

let mk_self_server dir =
  Self_serving_client.launch ?cors:None ?agent:None ~media_types dir

let rec repeat i json = if i <= 0 then [] else json :: repeat (i - 1) json

let dir = empty

let dir =
  register1 dir repeat_service (fun i () json ->
      Lwt.return (`Ok (`A (repeat i json))))

let dir = register1 dir add_service (fun i () j -> Lwt.return (`Ok (i + j)))

let is_ok_some = function `Ok (Some _) -> true | _ -> false

let code_of_response (response : Cohttp.Response.t) =
  Cohttp.Code.code_of_status response.status

let () =
  let (module S) = mk_self_server dir in
  let module C = Resto_cohttp_client.Client.Make (Encoding) (S) in
  let test0 () =
    (* Test a valid call *)
    let call r =
      C.call_service media_types repeat_service ((), r) () (`Bool true)
      |> Lwt_main.run
      |> function
      | _meth, _uri, service_result -> service_result
    in
    List.iter (function r -> assert (is_ok_some (call r))) [0; 1; 2; 3]
  in
  let test1 () =
    (* Test an URI that doesn't exist *)
    let uri, meth = (Uri.of_string "nimporte/quoi", `POST) in
    let response, _body = S.call meth uri |> Lwt_main.run in
    assert (code_of_response response = 404)
  in
  let test2 () =
    (* Test an URI that exists but uses a wrong method (GET whereas POST
       is expected) *)
    let uri, meth = (Uri.of_string "foo/1/repeat", `GET) in
    let response, _body = S.call meth uri |> Lwt_main.run in
    assert (code_of_response response = 405)
  in
  let test3 () =
    (* Test a valid URI but send an unexpectedly empty body *)
    let uri, meth = (Uri.of_string "foo/1/repeat", `POST) in
    let response, _body = S.call meth uri |> Lwt_main.run in
    assert (code_of_response response = 400)
  in
  let test4 () =
    (* Test a valid call, check the returned value *)
    let i, j = (1, 2) in
    let _meth, _uri, service_result =
      C.call_service media_types add_service ((), i) () j |> Lwt_main.run
    in
    assert (service_result <> `Ok (Some 0)) ;
    assert (service_result = `Ok (Some (i + j)))
  in
  let idx = ref 0 in
  List.iter
    (function
      | test ->
          test () ;
          Printf.printf "test%d: ✔️\n" !idx ;
          idx := !idx + 1)
    [test0; test1; test2; test3; test4]
