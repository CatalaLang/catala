(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Aminata Boiguillé <aminata.boiguille@etu.sorbonne-universite.fr>, Emile
   Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

val suggestion_minimum_levenshtein_distance_association :
  string list -> string -> string list
(**Returns a list of the closest words into {!name:candidates} to the keyword
   {!name:keyword}*)

val format : Format.formatter -> string list -> unit
