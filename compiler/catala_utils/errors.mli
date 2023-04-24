(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Error formatting and helper functions *)

(** {1 Error exception and printing} *)

exception StructuredError of (string * (string option * Pos.t) list)
(** The payload of the expression is a main error message, with a list of
    secondary positions related to the error, each carrying an optional
    secondary message to describe what is pointed by the position. *)

val print_structured_error : string -> (string option * Pos.t) list -> string

(** {1 Error exception and printing} *)

val raise_spanned_error :
  ?span_msg:string -> Pos.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val raise_multispanned_error :
  (string option * Pos.t) list -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val raise_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
val raise_internal_error : ('a, Format.formatter, unit, 'b) format4 -> 'a

val assert_internal_error :
  bool -> ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

(** {1 Warning printing}*)

val format_multispanned_warning :
  (string option * Pos.t) list -> ('a, Format.formatter, unit) format -> 'a

val format_spanned_warning :
  ?span_msg:string -> Pos.t -> ('a, Format.formatter, unit) format -> 'a

val format_warning : ('a, Format.formatter, unit) format -> 'a
