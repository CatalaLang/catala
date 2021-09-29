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

(** {1 Error exception and printing} *)

exception StructuredError of (string * (string option * Pos.t) list)
(** The payload of the expression is a main error message, with a list of secondary positions
    related to the error, each carrying an optional secondary message to describe what is pointed by
    the position. *)

val print_structured_error : string -> (string option * Pos.t) list -> string

(** {1 Error exception and printing} *)

val raise_spanned_error : string -> ?span_msg:string -> Pos.t -> 'a

val raise_multispanned_error : string -> (string option * Pos.t) list -> 'a

val raise_error : string -> 'a

(** {1 Warning printing}*)

val print_multispanned_warning : string -> (string option * Pos.t) list -> unit

val print_spanned_warning : string -> ?span_msg:string -> Pos.t -> unit

val print_warning : string -> unit
