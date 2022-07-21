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

module Allocations_familiales = Law_source.Allocations_familiales
module AF = Allocations_familiales
open Runtime
open Js_of_ocaml

class type enfant_entree =
  object
    method id : int Js.readonly_prop
    method remunerationMensuelle : int Js.readonly_prop
    method dateNaissance : Js.date Js.t Js.readonly_prop
    method gardeAlternee : bool Js.t Js.readonly_prop
    method gardeAlterneePartageAllocation : bool Js.t Js.readonly_prop

    method priseEnCharge : Js.js_string Js.t Js.readonly_prop
    (** Expects one of the five:

        - "Effective et permanente"
        - "Garde alternée, allocataire unique"
        - "Garde alternée, partage des allocations"
        - "Confié aux service sociaux, allocation versée à la famille"
        - "Confié aux service sociaux, allocation versée aux services sociaux" *)

    method aDejaOuvertDroitAuxAllocationsFamiliales : bool Js.t Js.readonly_prop

    method beneficieTitrePersonnelAidePersonnelleAuLogement :
      bool Js.t Js.readonly_prop
  end

class type allocations_familiales_input =
  object
    method currentDate : Js.date Js.t Js.readonly_prop
    method children : enfant_entree Js.t Js.js_array Js.t Js.readonly_prop
    method income : int Js.readonly_prop
    method residence : Js.js_string Js.t Js.readonly_prop

    method personneQuiAssumeLaChargeEffectivePermanenteEstParent :
      bool Js.t Js.readonly_prop

    method
        personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale :
      bool Js.t Js.readonly_prop
  end

class type source_position =
  object
    method fileName : Js.js_string Js.t Js.prop
    method startLine : int Js.prop
    method endLine : int Js.prop
    method startColumn : int Js.prop
    method endColumn : int Js.prop
    method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop
  end

class type raw_event =
  object
    method eventType : Js.js_string Js.t Js.prop
    method information : Js.js_string Js.t Js.js_array Js.t Js.prop
    method sourcePosition : source_position Js.t Js.optdef Js.prop
    method loggedValueJson : Js.js_string Js.t Js.prop
  end

class type event =
  object
    method data : Js.js_string Js.t Js.prop
  end

let _ =
  Js.export_all
    (object%js
       method resetLog : (unit -> unit) Js.callback = Js.wrap_callback reset_log

       method retrieveEvents : (unit -> event Js.t Js.js_array Js.t) Js.callback
           =
         Js.wrap_callback (fun () ->
             Js.array
               (Array.of_list
                  (retrieve_log () |> EventParser.parse_raw_events
                  |> List.map (fun event ->
                         object%js
                           val mutable data =
                             event |> Runtime.yojson_of_event
                             |> Yojson.Safe.to_string |> Js.string
                         end))))

       method retrieveRawEvents
           : (unit -> raw_event Js.t Js.js_array Js.t) Js.callback =
         Js.wrap_callback (fun () ->
             Js.array
               (Array.of_list
                  (List.map
                     (fun evt ->
                       object%js
                         val mutable eventType =
                           Js.string
                             (match evt with
                             | BeginCall _ -> "Begin call"
                             | EndCall _ -> "End call"
                             | VariableDefinition _ -> "Variable definition"
                             | DecisionTaken _ -> "Decision taken")

                         val mutable information =
                           Js.array
                             (Array.of_list
                                (match evt with
                                | BeginCall info
                                | EndCall info
                                | VariableDefinition (info, _) ->
                                  List.map Js.string info
                                | DecisionTaken _ -> []))

                         val mutable loggedValueJson =
                           (match evt with
                           | VariableDefinition (_, v) -> v
                           | EndCall _ | BeginCall _ | DecisionTaken _ ->
                             Runtime.unembeddable ())
                           |> Runtime.yojson_of_runtime_value
                           |> Yojson.Safe.to_string |> Js.string

                         val mutable sourcePosition =
                           match evt with
                           | DecisionTaken pos ->
                             Js.def
                               (object%js
                                  val mutable fileName = Js.string pos.filename
                                  val mutable startLine = pos.start_line
                                  val mutable endLine = pos.end_line
                                  val mutable startColumn = pos.start_column
                                  val mutable endColumn = pos.end_column

                                  val mutable lawHeadings =
                                    Js.array
                                      (Array.of_list
                                         (List.map Js.string pos.law_headings))
                               end)
                           | _ -> Js.undefined
                       end)
                     (retrieve_log ()))))

       method computeAllocationsFamiliales
           : (allocations_familiales_input Js.t -> float) Js.callback =
         Js.wrap_callback (fun input ->
             let result =
               AF_web.#interfaceAllocationsFamiliales
                 input
             in
             money_to_float
               result.AF.InterfaceAllocationsFamilialesOut.i_montant_verse_out)
    end)
