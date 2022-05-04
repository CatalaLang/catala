(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

let to_ascii (s : string) : string =
  let out = ref "" in
  CamomileLibraryDefault.Camomile.UTF8.iter
    (fun c ->
      let code = CamomileLibraryDefault.Camomile.UChar.uint_code c in
      out :=
        !out
        ^
        match code with
        | 0xc7 -> "C"
        | 0xe7 -> "c"
        | c when c >= 0xc0 && c <= 0xc6 -> "A"
        | c when c >= 0xe0 && c <= 0xe6 -> "a"
        | c when c >= 0xc8 && c <= 0xcb -> "E"
        | c when c >= 0xe8 && c <= 0xeb -> "e"
        | c when c >= 0xcc && c <= 0xcf -> "I"
        | c when c >= 0xec && c <= 0xef -> "i"
        | c when c >= 0xd2 && c <= 0xd6 -> "O"
        | c when c >= 0xf2 && c <= 0xf6 -> "o"
        | c when c >= 0xd9 && c <= 0xdc -> "U"
        | c when c >= 0xf9 && c <= 0xfc -> "u"
        | _ ->
          if code > 128 then "_"
          else String.make 1 (CamomileLibraryDefault.Camomile.UChar.char_of c))
    s;
  !out

let to_lowercase (s : string) : string =
  let is_first = ref true in
  let out = ref "" in
  CamomileLibraryDefault.Camomile.UTF8.iter
    (fun c ->
      let is_uppercase = Dcalc.Print.is_uppercase c in
      out :=
        !out
        ^ (if is_uppercase && not !is_first then "_" else "")
        ^ String.lowercase_ascii
            (String.make 1 (CamomileLibraryDefault.Camomile.UChar.char_of c));
      is_first := false)
    s;
  !out

let to_uppercase (s : string) : string =
  let last_was_underscore = ref false in
  let is_first = ref true in
  let out = ref "" in
  CamomileLibraryDefault.Camomile.UTF8.iter
    (fun c ->
      let is_underscore =
        c = CamomileLibraryDefault.Camomile.UChar.of_char '_'
      in
      let c_string =
        String.make 1 (CamomileLibraryDefault.Camomile.UChar.char_of c)
      in
      out :=
        !out
        ^
        if is_underscore then ""
        else if !last_was_underscore || !is_first then
          String.uppercase_ascii c_string
        else c_string;
      last_was_underscore := is_underscore;
      is_first := false)
    s;
  !out
