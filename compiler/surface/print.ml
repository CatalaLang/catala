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

open Ast

let format_primitive_typ (fmt : Format.formatter) (t : primitive_typ) : unit =
  match t with
  | Integer -> Format.fprintf fmt "integer"
  | Decimal -> Format.fprintf fmt "decimal"
  | Boolean -> Format.fprintf fmt "boolean"
  | Money -> Format.fprintf fmt "money"
  | Duration -> Format.fprintf fmt "duration"
  | Text -> Format.fprintf fmt "text"
  | Date -> Format.fprintf fmt "date"
  | Named constructor -> Format.fprintf fmt "%s" constructor
