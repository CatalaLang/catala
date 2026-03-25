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

(** CORS (cross-origin resource sharing) controls the ways in which resources
    from different domains are allowed to be obtained.

    See the specifications at https://fetch.spec.whatwg.org/#http-cors-protocol
*)

type t = {
  allowed_headers : string list;
      (** https://fetch.spec.whatwg.org/#http-access-control-allow-headers *)
  allowed_origins : string list;
      (** https://fetch.spec.whatwg.org/#http-access-control-allow-origin *)
}

(** [default] is a [t] with no allowed headers and no allowed origins. *)
val default : t

val add_allow_origin : Cohttp.Header.t -> t -> string option -> Cohttp.Header.t

val add_headers : Cohttp.Header.t -> t -> string option -> Cohttp.Header.t

(** [check_host header t] is [true] if one of [t]'s members matches the
    [header]'s [Host] field. *)
val check_host : Cohttp.Header.t -> t -> bool
