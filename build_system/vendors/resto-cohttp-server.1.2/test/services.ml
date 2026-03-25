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

open Resto
module Service = MakeService (Resto_json.Encoding)
open Service

(** Shared part *)

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

let alternate_add_service =
  get_service
    ~query:Query.empty
    ~output:Json_encoding.float
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float / "add")

let alternate_add_service' =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float / "add")

let alternate_add_service_inverted =
  get_service
    ~query:Query.empty
    ~output:Json_encoding.float
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.float /: Arg.int / "add")

let alternate_add_service_patch =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.int
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float / "patch")

let alternate_add_service_delete =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.null
    ~error:Json_encoding.empty
    Path.(root / "bar" /: Arg.int /: Arg.float)

let minus_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.float
    ~error:Json_encoding.empty
    Path.(open_root /: Arg.int / "minus")

let describe_service =
  description_service Json_encoding.empty Path.(root / "describe")

let dummy_service =
  post_service
    ~query:Query.empty
    ~input:Json_encoding.null
    ~output:Json_encoding.null
    ~error:Json_encoding.empty
    Path.(
      root / "a" / "path" / "long" / "enough" / "for" / "<hov>" / "to"
      / "trigger" /: Arg.float /: Arg.float /: Arg.float /: Arg.float
      /: Arg.float /: Arg.float /: Arg.float)

let sum_service =
  get_service
    ~query:Query.empty
    ~output:Json_encoding.int
    ~error:Json_encoding.empty
    Path.(root / "foobar" /:* Arg.int)

let prefix_dir1 = Path.(root / "tartine" /: Arg.float / "chaussure")

(** Client only *)

let real_minus_service1 = Service.prefix prefix_dir1 minus_service
