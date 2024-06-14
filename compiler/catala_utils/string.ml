(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

include Stdlib.String

let to_ascii : string -> string = Ubase.from_utf8
let is_uppercase_ascii = function 'A' .. 'Z' -> true | _ -> false

let begins_with_uppercase (s : string) : bool =
  "" <> s && is_uppercase_ascii (get (to_ascii s) 0)

let to_snake_case (s : string) : string =
  let out = Buffer.create (2 * length s) in
  s
  |> to_ascii
  |> iteri (fun i c ->
         if is_uppercase_ascii c && 0 <> i then Buffer.add_char out '_';
         Buffer.add_char out (Char.lowercase_ascii c));
  Buffer.contents out

let to_camel_case (s : string) : string =
  let last_was_underscore = ref true in
  let out = Buffer.create (length s) in
  s
  |> to_ascii
  |> iter (function
       | '_' -> last_was_underscore := true
       | c ->
         Buffer.add_char out
           (if !last_was_underscore then Char.uppercase_ascii c else c);
         last_was_underscore := false);
  Buffer.contents out

let remove_prefix ~prefix s =
  if starts_with ~prefix s then
    let plen = length prefix in
    sub s plen (length s - plen)
  else s

let repeat n s =
  let slen = length s in
  let buf = Bytes.create (n * slen) in
  for i = 0 to n - 1 do
    Bytes.blit_string s 0 buf (i * slen) slen
  done;
  Bytes.to_string buf

(* Note: this should do, but remains incorrect for combined unicode characters
   that display as one (e.g. `e` + postfix `'`). We should switch to Uuseg at
   some poing *)
let width s =
  let len = length s in
  let rec aux ncols i =
    if i >= len then ncols
    else if get s i = '\t' then aux (ncols + 8) (i + 1)
    else aux (ncols + 1) (i + Uchar.utf_decode_length (get_utf_8_uchar s i))
  in
  aux 0 0

let format ppf s = Format.pp_print_as ppf (width s) s

module Arg = struct
  include Stdlib.String

  let format = format

  let compare s1 s2 =
    let len1 = length s1 in
    let len2 = length s2 in
    let int c = int_of_char c - int_of_char '0' in
    let rec readnum acc s i =
      if i >= length s then acc, i
      else
        match get s i with
        | '0' .. '9' as c -> readnum ((acc * 10) + int c) s (i + 1)
        | _ -> acc, i
    in
    let rec aux i1 i2 =
      if i1 >= len1 then if i2 >= len2 then 0 else -1
      else if i2 >= len2 then 1
      else
        match get s1 i1, get s2 i2 with
        | ('0' .. '9' as c1), ('0' .. '9' as c2) -> (
          let x1, i1' = readnum (int c1) s1 (i1 + 1) in
          let x2, i2' = readnum (int c2) s2 (i2 + 1) in
          match Int.compare x1 x2 with
          | 0 -> (
            match Int.compare (i1' - i1) (i2' - i2) with
            | 0 -> aux i1' i2'
            | n -> n)
          | n -> n)
        | c1, c2 -> (
          match Char.compare c1 c2 with 0 -> aux (i1 + 1) (i2 + 1) | n -> n)
    in
    aux 0 0
end

let compare = Arg.compare
let hash t = Hash.raw t

module Set = Set.Make (Arg)
module Map = Map.Make (Arg)
