(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This module defines module names and path accesses, used to refer to
    separate compilation units *)

open Catala_utils

type modname = string
type ident = string
type path = modname list
type t = path * ident

let compare_path = List.compare String.compare
let equal_path = List.equal String.equal

let compare (p1, i1) (p2, i2) =
  match compare_path p1 p2 with 0 -> String.compare i1 i2 | n -> n

let equal (p1, i1) (p2, i2) = equal_path p1 p2 && String.equal i1 i2
let format_modname ppf m = Format.fprintf ppf "@{<blue>%s@}" m

let format_path ppf p =
  let pp_sep ppf () = Format.fprintf ppf "@{<cyan>.@}" in
  Format.pp_print_list ~pp_sep format_modname ppf p;
  pp_sep ppf ()

let format ppf (p, i) =
  format_path ppf p;
  Format.pp_print_string ppf i

module Ord = struct
  type nonrec t = t

  let compare = compare
  let format = format
end

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)
