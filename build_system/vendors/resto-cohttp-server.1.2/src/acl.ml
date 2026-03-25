(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic-Labs                                           *)
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

(* Matchers *)

type chunk_matcher = Literal of string | Wildcard

type path_matcher =
  | Exact of chunk_matcher list
  | FollowedByAnySuffix of chunk_matcher list

type meth_matcher = Exact of Resto.meth | Any

type matcher = {meth : meth_matcher; path : path_matcher}

(* matchers parsing *)

let parse_meth_matcher s =
  match s.[0] with
  | '/' | ' ' -> (Any, 0)
  | _ ->
      if String.length s > 3 && String.sub s 0 3 = "GET" then (Exact `GET, 3)
      else if String.length s > 3 && String.sub s 0 3 = "PUT" then
        (Exact `PUT, 3)
      else if String.length s > 4 && String.sub s 0 4 = "POST" then
        (Exact `POST, 4)
      else if String.length s > 5 && String.sub s 0 5 = "PATCH" then
        (Exact `PATCH, 5)
      else if String.length s > 6 && String.sub s 0 6 = "DELETE" then
        (Exact `DELETE, 6)
      else raise (Invalid_argument "Resto.Acl.parse: invalid method matcher")

let ignore_whitespace s offset =
  match String.index_from_opt s offset '/' with
  | None -> raise (Invalid_argument "Resto.Acl.parse: invalid method matcher")
  | Some first_slash ->
      assert (first_slash >= offset) ;
      if first_slash > offset then
        for i = offset to first_slash - 1 do
          if s.[i] <> ' ' then
            raise (Invalid_argument "Resto.Acl.parse: invalid method matcher")
        done ;
      first_slash

let add_chunk (matcher : path_matcher) (chunk : string) =
  match matcher with
  | FollowedByAnySuffix _ ->
      raise
        (Invalid_argument
           "Resto.Acl.parse: double-star can only appear in suffix position")
  | Exact f -> (
      match chunk with
      | "**" -> FollowedByAnySuffix f
      | "*" -> Exact (Wildcard :: f)
      | literal ->
          String.iter
            (function
              | '/' -> assert false
              | ('*' | '?' | '&' | '#' | '=') as c ->
                  Format.kasprintf
                    invalid_arg
                    "Resto.Acl.parse: %c must be percent-encoded"
                    c
              | _ -> ())
            literal ;
          let decoded_literal = Uri.pct_decode literal in
          Exact (Literal decoded_literal :: f))

let parse_path s offset =
  String.sub s offset (String.length s - offset)
  |> Resto.Utils.split_path
  |> List.fold_left add_chunk (Exact [])
  |> function
  | FollowedByAnySuffix m -> FollowedByAnySuffix (List.rev m)
  | Exact m -> Exact (List.rev m)

let parse : string -> matcher =
 fun s ->
  if String.length s = 0 then
    raise (Invalid_argument "Resto.Acl.parse: a filter cannot be empty")
  else
    let meth, offset = parse_meth_matcher s in
    let offset = ignore_whitespace s offset in
    let path = parse_path s offset in
    {meth; path}

(* Serialising *)

let to_string_meth = function
  | Any -> ""
  | Exact `GET -> "GET"
  | Exact `PUT -> "PUT"
  | Exact `POST -> "POST"
  | Exact `PATCH -> "PATCH"
  | Exact `DELETE -> "DELETE"

let escaped_asterisk_seq = String.to_seq "%2A"

let to_string_chunk = function
  | Wildcard -> "*"
  | Literal l ->
      let s = Uri.pct_encode l in
      if String.contains s '*' then
        (* slow path *)
        String.of_seq
          (Seq.flat_map
             (function '*' -> escaped_asterisk_seq | c -> Seq.return c)
             (String.to_seq s))
      else s

let to_string_chunk_list l =
  "/" ^ String.concat "/" (List.map to_string_chunk l)

let to_string_path = function
  | FollowedByAnySuffix l -> to_string_chunk_list l ^ "/**"
  | Exact l -> to_string_chunk_list l

let to_string {meth; path} = to_string_meth meth ^ to_string_path path

(* Matching paths *)

let rec matches_path any_suffix_of matcher path =
  match (matcher, path) with
  | [], [] -> true
  | [], _ :: _ -> any_suffix_of
  | _ :: _, [] -> false
  | Wildcard :: matcher, _ :: path -> matches_path any_suffix_of matcher path
  | Literal lit :: matcher, chunk :: path ->
      String.equal lit chunk && matches_path any_suffix_of matcher path

let matches_path path matcher =
  match matcher with
  | FollowedByAnySuffix matcher -> matches_path true matcher path
  | Exact matcher -> matches_path false matcher path

let matches_meth (meth : [< Resto.meth]) = function
  | Any -> true
  | Exact m -> m = meth

let matches_matcher meth_ path_ {meth; path} =
  matches_meth meth_ meth && matches_path path_ path

let matches_any_matcher meth path matchers =
  List.exists (matches_matcher meth path) matchers

(* ACL policy and implementation *)

type t =
  | Allow_all of {except : matcher list}
  | Deny_all of {except : matcher list}

let allowed policy ~meth ~path =
  match policy with
  | Deny_all {except = []} ->
      (* All paths are blocked, no exceptions *)
      false
  | Allow_all {except = []} ->
      (* All paths are allowed, no exceptions *)
      true
  | Deny_all {except = matchers} ->
      (* default: Deny, matches: Allow *)
      matches_any_matcher meth path matchers
  | Allow_all {except = matchers} ->
      (* default: Allow, matches: Deny *)
      not @@ matches_any_matcher meth path matchers
