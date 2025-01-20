(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
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

type t = int

let mix (h1 : t) (h2 : t) : t = Hashtbl.hash (h1, h2)
let raw = Hashtbl.hash

module Op = struct
  let ( % ) = mix
  let ( ! ) = raw
end

open Op

let option f = function None -> !`None | Some x -> !`Some % f x
let list hf l = List.fold_left (fun acc x -> acc % hf x) !`ListEmpty l

let map fold_fun kh vh map =
  fold_fun (fun k v acc -> acc lxor (kh k % vh v)) map !`HashMapDelim

module Flags : sig
  type nonrec t = private t

  val pass :
    (t -> 'a) -> closure_conversion:bool -> monomorphize_types:bool -> 'a

  val of_t : int -> t
end = struct
  type nonrec t = t

  let pass k ~closure_conversion ~monomorphize_types =
    (* Should not affect the call convention or actual interfaces: include,
       optimize, check_invariants, typed *)
    !(closure_conversion : bool)
    % !(monomorphize_types : bool)
    % (* The following may not affect the call convention, but we want it set in
         an homogeneous way *)
    !(Global.options.trace <> None: bool)
    % !(Global.options.max_prec_digits : int)
    |> k

  let of_t t = t
end

type full = { catala_version : t; flags_hash : Flags.t; contents : t }

let finalise t =
  Flags.pass (fun flags_hash ->
      { catala_version = !(Version.v : string); flags_hash; contents = t })

let to_string full =
  Printf.sprintf "CM0|%08x|%08x|%08x" full.catala_version
    (full.flags_hash :> int)
    full.contents

(* Putting color inside the hash makes them much easier to differentiate at a
   glance *)
let format ppf full =
  let open Ocolor_types in
  let pcolor col f x =
    Format.pp_open_stag ppf Ocolor_format.(Ocolor_style_tag (Fg (C24 col)));
    f x;
    Format.pp_close_stag ppf ()
  in
  let tag = pcolor { r24 = 172; g24 = 172; b24 = 172 } in
  let auto i =
    {
      r24 = 128 + (i mod 128);
      g24 = 128 + ((i lsr 10) mod 128);
      b24 = 128 + ((i lsr 20) mod 128);
    }
  in
  let phash h =
    let col = auto h in
    pcolor col (Format.fprintf ppf "%08x") h
  in
  tag (Format.pp_print_string ppf) "CM0|";
  phash full.catala_version;
  tag (Format.pp_print_string ppf) "|";
  phash (full.flags_hash :> int);
  tag (Format.pp_print_string ppf) "|";
  phash full.contents

let of_string s =
  try
    Scanf.sscanf s "CM0|%08x|%08x|%08x"
      (fun catala_version flags_hash contents ->
        { catala_version; flags_hash = Flags.of_t flags_hash; contents })
  with Scanf.Scan_failure _ -> failwith "Hash.of_string"

let external_placeholder = "*external*"
