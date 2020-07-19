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

(** {2 Errors}*)

exception ParsingError of string

exception LexingError of string

exception WeavingError of string

exception ContextError of string

(** {2 Error-raising functions} *)

(** You should use those rather than manually throwing the exceptions above *)

(** Usage: [parser_error error_token message] *)
let parser_error (loc : Pos.t) (token : string) (msg : string) =
  raise
    (ParsingError
       (Printf.sprintf "Syntax error at token \"%s\" %s\n%s\n%s" token (Pos.to_string loc)
          (Pos.retrieve_loc_text loc) msg))

(** Usage: [parser_error error_token] *)
let lexer_error (loc : Pos.t) (msg : string) =
  raise (LexingError (Printf.sprintf "Parsing error %s on token \"%s\"" (Pos.to_string loc) msg))

(** Usage: [weaving_error message] *)
let weaving_error (msg : string) = raise (WeavingError (Printf.sprintf "Weaving error: %s" msg))

(** Usage: [*)
let unknown_identifier (ident : string) (loc : Pos.t) =
  raise (ContextError (Printf.sprintf "Unknown identifier \"%s\"\n%s" ident (Pos.to_string loc)))

let context_error (msg : string) = raise (ContextError (Printf.sprintf "%s" msg))
