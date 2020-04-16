(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module WithId (X : sig
  type t

  val to_string : t -> string
end) : sig
  type t

  val fresh : X.t -> t

  val to_string : t -> string

  val compare : t -> t -> int
end = struct
  let counter : int ref = ref 0

  type t = { id : int; value : X.t }

  let fresh (x : X.t) =
    counter := !counter + 1;
    { id = !counter; value = x }

  let to_string (x : t) = X.to_string x.value ^ "_" ^ string_of_int x.id

  let compare (x1 : t) (x2 : t) = x1.id - x2.id
end
