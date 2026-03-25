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

module Encoding = struct
  include Json_encoding

  type 'a t = 'a encoding

  type schema = Json_schema.schema

  let untyped = obj1 (req "untyped" string)

  let conv f g t = conv ~schema:(schema t) f g t

  module StringMap = Map.Make (String)

  let arg_encoding =
    let open Json_encoding in
    conv
      (fun {Resto.Arg.name; descr} -> (name, descr))
      (fun (name, descr) -> {name; descr})
      (obj2 (req "name" string) (opt "descr" string))

  open Resto.Description

  let meth_encoding =
    Json_encoding.string_enum
      [
        ("GET", `GET);
        ("POST", `POST);
        ("DELETE", `DELETE);
        ("PUT", `PUT);
        ("PATCH", `PATCH);
      ]

  let path_item_encoding =
    let open Json_encoding in
    union
      [
        case
          string
          (function PStatic s -> Some s | _ -> None)
          (fun s -> PStatic s);
        case
          arg_encoding
          (function PDynamic s -> Some s | _ -> None)
          (fun s -> PDynamic s);
      ]

  let query_kind_encoding =
    let open Json_encoding in
    union
      [
        case
          (obj1 (req "single" arg_encoding))
          (function Single s -> Some s | _ -> None)
          (fun s -> Single s);
        case
          (obj1 (req "optional" arg_encoding))
          (function Optional s -> Some s | _ -> None)
          (fun s -> Optional s);
        case
          (obj1 (req "flag" empty))
          (function Flag -> Some () | _ -> None)
          (fun () -> Flag);
        case
          (obj1 (req "multi" arg_encoding))
          (function Multi s -> Some s | _ -> None)
          (fun s -> Multi s);
      ]

  let query_item_encoding =
    let open Json_encoding in
    conv
      (fun {name; description; kind} -> (name, description, kind))
      (fun (name, description, kind) -> {name; description; kind})
      (obj3
         (req "name" string)
         (opt "description" string)
         (req "kind" query_kind_encoding))

  let service_descr_encoding =
    let open Json_encoding in
    conv
      (fun {meth; path; description; query; input; output; error} ->
        ( meth,
          path,
          description,
          query,
          (match input with
          | None -> None
          | Some input -> Some (Lazy.force input)),
          Lazy.force output,
          Lazy.force error ))
      (fun (meth, path, description, query, input, output, error) ->
        {
          meth;
          path;
          description;
          query;
          input =
            (match input with
            | None -> None
            | Some input -> Some (Lazy.from_val input));
          output = Lazy.from_val output;
          error = Lazy.from_val error;
        })
      (obj7
         (req "meth" meth_encoding)
         (req "path" (list path_item_encoding))
         (opt "description" string)
         (req "query" (list query_item_encoding))
         (opt "input" any_schema)
         (req "output" any_schema)
         (req "erro" any_schema))

  let directory_descr_encoding =
    let open Json_encoding in
    mu "service_tree" @@ fun directory_descr_encoding ->
    let static_subdirectories_descr_encoding =
      union
        [
          case
            (obj1
               (req
                  "suffixes"
                  (list
                     (obj2
                        (req "name" string)
                        (req "tree" directory_descr_encoding)))))
            (function
              | Suffixes map -> Some (Resto.StringMap.bindings map) | _ -> None)
            (fun m ->
              let add acc (n, t) = Resto.StringMap.add n t acc in
              Suffixes (List.fold_left add Resto.StringMap.empty m));
          case
            (obj1
               (req
                  "dynamic_dispatch"
                  (obj2
                     (req "arg" arg_encoding)
                     (req "tree" directory_descr_encoding))))
            (function Arg (ty, tree) -> Some (ty, tree) | _ -> None)
            (fun (ty, tree) -> Arg (ty, tree));
        ]
    in
    let static_directory_descr_encoding =
      conv
        (fun {services; subdirs} ->
          let find s =
            try Some (Resto.MethMap.find s services) with Not_found -> None
          in
          (find `GET, find `POST, find `DELETE, find `PUT, find `PATCH, subdirs))
        (fun (get, post, delete, put, patch, subdirs) ->
          let add meth s services =
            match s with
            | None -> services
            | Some s -> Resto.MethMap.add meth s services
          in
          let services =
            Resto.MethMap.empty
            |> add `GET get
            |> add `POST post
            |> add `DELETE delete
            |> add `PUT put
            |> add `PATCH patch
          in
          {services; subdirs})
        (obj6
           (opt "get_service" service_descr_encoding)
           (opt "post_service" service_descr_encoding)
           (opt "delete_service" service_descr_encoding)
           (opt "put_service" service_descr_encoding)
           (opt "patch_service" service_descr_encoding)
           (opt "subdirs" static_subdirectories_descr_encoding))
    in
    union
      [
        case
          (obj1 (req "static" static_directory_descr_encoding))
          (function Static descr -> Some descr | _ -> None)
          (fun descr -> Static descr);
        case
          (obj1 (req "dynamic" (option string)))
          (function Dynamic descr -> Some descr | _ -> None)
          (fun descr -> Dynamic descr);
      ]

  let description_request_encoding =
    conv
      (fun {recurse} -> recurse)
      (function recurse -> {recurse})
      (obj1 (dft "recursive" bool false))

  let description_answer_encoding = directory_descr_encoding
end

module type VALUE = sig
  type t

  type 'a encoding

  val construct : 'a encoding -> 'a -> t

  val destruct : 'a encoding -> t -> 'a
end

module Ezjsonm = struct
  type t = Json_repr.Ezjsonm.value

  let construct enc v = Json_encoding.construct enc v

  let destruct enc v = Json_encoding.destruct enc v
end

module Bson = struct
  open Json_repr_bson

  type t = Repr.value

  let construct enc v = Json_encoding.construct enc v

  let destruct enc v = Json_encoding.destruct enc v
end
