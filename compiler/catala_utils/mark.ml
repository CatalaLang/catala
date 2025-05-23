(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type ('a, 'm) ed = 'a * 'm
type 'a pos = ('a, Pos.t) ed

let add m e = e, m
let remove (x, _) = x
let get (_, m) = m
let ghost x = x, Pos.void
let set m (x, _) = x, m
let map f (x, m) = f x, m
let map_mark f (a, m) = a, f m
let copy (_, m) x = x, m
let fold f (x, _) = f x
let fold2 f (x, _) (y, _) = f x y
let compare cmp a b = fold2 cmp a b
let equal eq a b = fold2 eq a b
let hash f (x, _) = f x
