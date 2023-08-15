(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

(** Small wrapper on top of the stdlib [Map] module to add some useful functions *)

(* NOTE: only defines typed module, a .mli would be completely redundant *)

include Stdlib.Map

module type OrderedType = sig
  include Stdlib.Map.OrderedType

  val format : Format.formatter -> t -> unit
end

module type S = sig
  include Stdlib.Map.S

  exception Not_found of key
  (* Slightly more informative [Not_found] exception *)

  val find: key -> 'a t -> 'a

  val keys : 'a t -> key list
  val values : 'a t -> 'a list
  val of_list : (key * 'a) list -> 'a t

  val format_keys :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val format_values :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val format_bindings :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> (Format.formatter -> unit) -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
  (** Formats the bindings of [t] in order. The user-supplied format function is
      provided with a formatter for keys (can be used with ["%t"]) and the
      corresponding value. *)

  val format :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?pp_bind:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
  (** Formats all bindings of the map in order using the given separator
      (default ["; "]) and binding indicator (default [" = "]). *)

end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  include Stdlib.Map.Make (Ord)

  exception Not_found of key

  let () =
    Printexc.register_printer @@ function
    | Not_found k ->
      Some (Format.asprintf "key '%a' not found in map" Ord.format k)
    | _ -> None

  let find k t =
    try find k t with Stdlib.Not_found -> raise (Not_found k)

  let keys t = fold (fun k _ acc -> k :: acc) t [] |> List.rev
  let values t = fold (fun _ v acc -> v :: acc) t [] |> List.rev
  let of_list l = List.fold_left (fun m (k, v) -> add k v m) empty l

  let format_keys ?pp_sep ppf t =
    Format.pp_print_list ?pp_sep Ord.format ppf (keys t)

  let format_values ?pp_sep pp_value ppf t =
    Format.pp_print_list ?pp_sep pp_value ppf (values t)

  let format_bindings ?pp_sep pp_bnd ppf t =
    Format.pp_print_list ?pp_sep
      (fun ppf (k, v) -> pp_bnd ppf (fun ppf -> Ord.format ppf k) v)
      ppf (bindings t)

  let format
      ?(pp_sep = fun ppf () -> Format.fprintf ppf ";@ ")
      ?(pp_bind = fun ppf () -> Format.fprintf ppf " =@ ")
      pp_value
      ppf
      t =
    Format.pp_print_list ~pp_sep
      (fun ppf (key, value) ->
        Format.pp_open_hvbox ppf 2;
        Ord.format ppf key;
        pp_bind ppf ();
        pp_value ppf value;
        Format.pp_close_box ppf ())
      ppf (bindings t)
end
