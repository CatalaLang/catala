(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
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

module Make (Encoding : Resto.ENCODING) : sig
  type t = {
    name : Cohttp.Accept.media_range;
    q : int option;
    pp : 'a. 'a Encoding.t -> Format.formatter -> string -> unit;
    (* [construct] constructs the answer in one go. This is fine for values that
       are small enough: the serialising takes little time, the serialised value
       takes little space, the writing on the network connection takes little
       time. For large values this is problematic: the serialisation is
       non-blocking leading to a Lwt hang-up, the memory footprint of the
       serialisation is at least as big as the serialised value (large), the
       writing on the network takes a long time. In that case, there is
       [construct_seq] below. *)
    construct : 'a. 'a Encoding.t -> 'a -> string;
    (* [construct_seq] constructs the answer lazily as chunks of text to be
       blitted onto whatever buffer/socket/other the server is handling
       internally. This is meant to circumvent the issues mentioned above. Note
       that Resto will yield in between the use of two consecutive chunks of
       text. *)
    construct_seq : 'a. 'a Encoding.t -> 'a -> (Bytes.t * int * int) Seq.t;
    destruct : 'a. 'a Encoding.t -> string -> ('a, string) result;
  }

  val name : t -> string

  val has_complete_media : t list -> bool

  val first_complete_media : t list -> ((string * string) * t) option

  val find_media : string * string -> t list -> t option

  val resolve_accept_header : t list -> string option -> (string * t) option

  val accept_header : t list -> string

  val acceptable_encoding : t list -> string
end
