(* This file is part of the French law library, a collection of functions for
   computing French taxes and benefits derived from Catala programs. Copyright
   (C) 2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Runtime_ocaml.Runtime
module Allocations_familiales = Law_source.Allocations_familiales

val compute_allocations_familiales :
  current_date:date ->
  children:Allocations_familiales.EnfantEntree.t array ->
  income:int ->
  residence:Allocations_familiales.Collectivite.t ->
  is_parent:bool ->
  fills_title_I:bool ->
  had_rights_open_before_2012:bool ->
  float
(** Usage *)
