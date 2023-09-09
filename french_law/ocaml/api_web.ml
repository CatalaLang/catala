(* This file is part of the French law library, a collection of functions for
   computing French taxes and benefits derived from Catala programs. Copyright
   (C) 2021 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>, Emile
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

open Js_of_ocaml
open Law_source
open Runtime_jsoo.Runtime
module AF = Allocations_familiales
module AF_web = Allocations_familiales_api_web
module AL = Aides_logement
module AL_web = Aides_logement_api_web

let () =
  Js.export_all
    (object%js
       val eventsManager = event_manager

       method computeAllocationsFamiliales
           : (AF_web.interface_allocations_familiales_in -> float) Js.callback =
         Js.wrap_callback (fun interface_allocations_familiales_in ->
             execute_or_throw_error (fun () ->
                 let result =
                   interface_allocations_familiales_in
                   |> AF_web.interface_allocations_familiales
                 in
                 result##.iMontantVerse))

       method computeAidesAuLogement
           : (AL_web.calculette_aides_au_logement_garde_alternee_in -> float)
             Js.callback =
         Js.wrap_callback (fun calculette_aides_au_logement_garde_alternee_in ->
             execute_or_throw_error (fun () ->
                 let result =
                   calculette_aides_au_logement_garde_alternee_in
                   |> AL_web.calculette_aides_au_logement_garde_alternee
                 in
                 result##.aideFinale))
    end)
