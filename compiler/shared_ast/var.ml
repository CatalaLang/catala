(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Definitions

(** {1 Variables and their collections} *)

(** This module provides types and helpers for Bindlib variables on the [gexpr]
    type *)

type 'e t = ('a, 't) naked_gexpr Bindlib.var constraint 'e = ('a, 't) gexpr
type 'e vars = ('a, 't) naked_gexpr Bindlib.mvar constraint 'e = ('a, 't) gexpr

let make (name : string) : 'e t = Bindlib.new_var (fun x -> EVar x) name
let compare = Bindlib.compare_vars
let equal = Bindlib.eq_vars
let hash = Bindlib.hash_var

let translate (v : 'e1 t) : 'e2 t =
  Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)

type 'e var = 'e t

(* The purpose of this module is just to lift a type parameter outside of
   [Set.S] and [Map.S], so that we can have ['e Var.Set.t] for sets of variables
   bound to the ['e = ('a, 't) gexpr] expression type. This is made possible by
   the fact that [Bindlib.compare_vars] is polymorphic in that parameter; we
   first hide that parameter inside an existential, then re-add a phantom type
   outside of the set to ensure consistency. Extracting the elements is then
   done with [Bindlib.copy_var] but technically it's not much different from an
   [Obj] conversion.

   If anyone has a better solution, besides a copy-paste of Set.Make / Map.Make
   code... *)
module Generic = struct
  (* Existentially quantify the type parameters to allow application of
     Set.Make *)
  type t = Var : 'e var -> t
  (* Note: adding [[@@ocaml.unboxed]] would be OK and make our wrappers live at
     the type-level without affecting the actual data representation. But
     [Bindlib.var] being abstract, we can't convince OCaml it's ok at the moment
     and have to hold it *)

  let t v = Var v
  let get (Var v) = Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)
  let compare (Var x) (Var y) = Bindlib.compare_vars x y
  let eq (Var x) (Var y) = Bindlib.eq_vars x y [@@ocaml.warning "-32"]
  let format ppf v = String.format ppf (Bindlib.name_of (get v))
end

(* Wrapper around Set.Make to re-add type parameters (avoid inconsistent
   sets) *)
module Set = struct
  open Generic
  open Set.Make (Generic)

  type nonrec 'e t = t

  let empty = empty
  let singleton x = singleton (t x)
  let add x s = add (t x) s
  let remove x s = remove (t x) s
  let union s1 s2 = union s1 s2
  let mem x s = mem (t x) s
  let of_list l = of_list (List.map t l)
  let elements s = elements s |> List.map get
  let diff s1 s2 = diff s1 s2
  let iter f s = iter (fun x -> f (get x)) s

  (* Add more as needed *)
end

(* Wrapper around Map.Make to re-add type parameters (avoid inconsistent
   maps) *)
module Map = struct
  open Generic
  module M = Map.Make (Generic)
  open M

  type k0 = M.key

  exception Not_found = M.Not_found

  type nonrec ('e, 'x) t = 'x t

  let empty = empty
  let singleton v x = singleton (t v) x
  let add v x m = add (t v) x m
  let update v f m = update (t v) f m
  let find v m = find (t v) m
  let find_opt v m = find_opt (t v) m
  let bindings m = bindings m |> List.map (fun (v, x) -> get v, x)
  let mem x m = mem (t x) m
  let union f m1 m2 = union (fun v x1 x2 -> f (get v) x1 x2) m1 m2
  let fold f m acc = fold (fun v x acc -> f (get v) x acc) m acc
  let keys m = keys m |> List.map get
  let values m = values m
  let format_keys ?pp_sep m = format_keys ?pp_sep m

  (* Add more as needed *)
end
