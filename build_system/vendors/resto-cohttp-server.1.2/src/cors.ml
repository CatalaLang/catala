(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = {allowed_headers : string list; allowed_origins : string list}

let default = {allowed_headers = []; allowed_origins = []}

let check_origin_matches origin allowed_origin =
  String.equal "*" allowed_origin
  || String.equal allowed_origin origin
  ||
  let allowed_w_slash = allowed_origin ^ "/" in
  let len_a_w_s = String.length allowed_w_slash in
  let len_o = String.length origin in
  len_o >= len_a_w_s
  && (String.equal allowed_w_slash @@ String.sub origin 0 len_a_w_s)

let find_matching_origin allowed_origins origin =
  let matching_origins =
    List.filter (check_origin_matches origin) allowed_origins
  in
  let compare_by_length_neg a b =
    ~-(compare (String.length a) (String.length b))
  in
  let matching_origins_sorted =
    List.sort compare_by_length_neg matching_origins
  in
  match matching_origins_sorted with [] -> None | x :: _ -> Some x

let add_allow_origin headers cors origin_header =
  match origin_header with
  | None -> headers
  | Some origin -> (
      match find_matching_origin cors.allowed_origins origin with
      | None -> headers
      | Some allowed_origin ->
          Cohttp.Header.add headers "Access-Control-Allow-Origin" allowed_origin
      )

let add_headers headers cors origin_header =
  let cors_headers =
    Cohttp.Header.add_multi
      headers
      "Access-Control-Allow-Headers"
      cors.allowed_headers
  in
  add_allow_origin cors_headers cors origin_header

let check_host headers cors =
  match Cohttp.Header.get headers "Host" with
  | None -> List.mem "*" cors.allowed_origins
  | Some host -> (
      match find_matching_origin cors.allowed_origins host with
      | None -> false
      | Some _ -> true)
