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

module type Info = sig
  type info

  val format_info : info -> string
end

module MarkedString : Info with type info = string Pos.marked

module type Id = sig
  type t

  type info

  val fresh : info -> t

  val get_info : t -> info

  val compare : t -> t -> int

  val format_t : Format.formatter -> t -> unit

  val hash : t -> int
end

module Make (X : Info) : Id with type info = X.info
