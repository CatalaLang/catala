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

module Field = Id.WithId (struct
  type t = string Pos.marked
  (** The position corresponds to the declaration *)

  let to_string x = Pos.unmark x
end)

module Class = Id.WithId (struct
  type t = string

  let to_string (x : string) = x
end)

module Constructor = Id.WithId (struct
  type t = string Pos.marked
  (** The position corresponds to the declaration *)

  let to_string x = Pos.unmark x
end)

module Enum = Id.WithId (struct
  type t = string Pos.marked
  (** The position corresponds to the declaration *)

  let to_string x = Pos.unmark x
end)

module ClassMap = Map.Make (Class)
module EnumMap = Map.Make (Enum)
module ConstructorMap = Map.Make (Constructor)
module FieldMap = Map.Make (Field)

type base_typ = Integer | Boolean

type typ = BaseTyp of base_typ | EnumTyp of Enum.t | ClassTyp of Class.t

type expression = unit

type setter_method = unit

type function_method = unit

type assert_method = unit

type meta_assert_method = unit

type field_typ = { field_typ_typ : typ }

type method_t =
  | SetterMethod of setter_method Pos.marked
  | FunctionMethod of function_method Pos.marked
  | AssertMethod of assert_method Pos.marked
  | MetaAssertMethod of meta_assert_method Pos.marked

type class_t = {
  class_fields : typ FieldMap.t;
  class_methods : method_t Pos.marked list;
  class_inherits : Class.t list;
}

type enum = typ ConstructorMap.t

type program = { program_classes : class_t ClassMap.t; program_enums : enum EnumMap.t }
