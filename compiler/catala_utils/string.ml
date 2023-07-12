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
  let out = ref "" in
  to_ascii s
  |> iteri (fun i c ->
         out :=
           !out
           ^ (if is_uppercase_ascii c && 0 <> i then "_" else "")
           ^ lowercase_ascii (make 1 c));
  !out

let to_camel_case (s : string) : string =
  let last_was_underscore = ref false in
  let out = ref "" in
  to_ascii s
  |> iteri (fun i c ->
         let is_underscore = c = '_' in
         let c_string = make 1 c in
         out :=
           !out
           ^
           if is_underscore then ""
           else if !last_was_underscore || 0 = i then uppercase_ascii c_string
           else c_string;
         last_was_underscore := is_underscore);
  !out

let remove_prefix ~prefix s =
  if starts_with ~prefix s then
    let plen = length prefix in
    sub s plen (length s - plen)
  else s

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
end

module Set = Set.Make (Arg)
module Map = Map.Make (Arg)
