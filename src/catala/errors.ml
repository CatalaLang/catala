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

(** Error formatting and helper functions *)

exception StructuredError of (string * (string option * Pos.t) list)

let print_structured_error (msg : string) (pos : (string option * Pos.t) list) : string =
  Printf.sprintf "%s\n\n%s" msg
    (String.concat "\n\n"
       (List.map
          (fun (msg, pos) ->
            Printf.sprintf "%s%s"
              (match msg with None -> "" | Some msg -> msg ^ "\n")
              (Pos.retrieve_loc_text pos))
          pos))

let raise_spanned_error (msg : string) ?(span_msg : string option) (span : Pos.t) : 'a =
  raise (StructuredError (msg, [ (span_msg, span) ]))

let raise_multispanned_error (msg : string) (spans : (string option * Pos.t) list) =
  raise (StructuredError (msg, spans))

let raise_error (msg : string) : 'a = raise (StructuredError (msg, []))
