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

(** A wrapper around [json-data-encoding] that exposes the modules expected by
    [Resto]. *)

(** [Encoding] exposes the minimal part [Json_encoding] that allow to construct
    new encodings as well as pre-made encodings for values useful to [Resto].

    It also makes the type of encodings ['a t] concrete (['a
    Json_encoding.encodings]) which allows for further use -- see [VALUE]. *)
module Encoding :
  Resto.ENCODING
    with type 'a t = 'a Json_encoding.encoding
     and type schema = Json_schema.schema

(** A [VALUE] module allows the actual conversion of values between different
    representations. It is intended as a companion to the [Encoding] module
    above. *)
module type VALUE = sig
  type t

  type 'a encoding

  val construct : 'a encoding -> 'a -> t

  val destruct : 'a encoding -> t -> 'a
end

module Ezjsonm :
  VALUE
    with type t = Json_repr.Ezjsonm.value
     and type 'a encoding := 'a Encoding.t

module Bson :
  VALUE with type t = Json_repr_bson.bson and type 'a encoding := 'a Encoding.t
