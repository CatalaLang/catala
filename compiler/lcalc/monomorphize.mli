(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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

open Shared_ast
open Ast

val program : typed program -> typed program * TypeIdent.t list
(** This function performs type monomorphization in a Catala program with two
    main actions:
    - transforms tuples into named structs.
    - creates monomorphized instances of [TOption] for every occurence of the
      type.
    - creates monomorphized instances of [TArray] for every occurence of the
      type; each instance is a struct with a integer [length] field and a
      [content] field whose type still is [TArray].

    It also returns the new type ordering for the program. *)
