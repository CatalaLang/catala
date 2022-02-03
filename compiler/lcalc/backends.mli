(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Helper functions common to all Catala compiler backends *)

val to_ascii : string -> string
(** Removes all non-ASCII diacritics from a string by converting them to their base letter in the
    Latin alphabet *)

val to_lowercase : string -> string
(** Converts CamlCase into snake_case *)

val to_uppercase : string -> string
(** Convertes snake_case into CamlCase *)
