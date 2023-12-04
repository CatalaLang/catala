(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Definitions

type t = typ

let equal_tlit l1 l2 = l1 = l2
let compare_tlit l1 l2 = Stdlib.compare l1 l2

let rec equal ty1 ty2 =
  match Mark.remove ty1, Mark.remove ty2 with
  | TLit l1, TLit l2 -> equal_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> equal_list tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.equal n1 n2
  | TEnum n1, TEnum n2 -> EnumName.equal n1 n2
  | TOption t1, TOption t2 -> equal t1 t2
  | TArrow (t1, t1'), TArrow (t2, t2') -> equal_list t1 t2 && equal t1' t2'
  | TArray t1, TArray t2 -> equal t1 t2
  | TDefault t1, TDefault t2 -> equal t1 t2
  | TClosureEnv, TClosureEnv | TAny, TAny -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TDefault _ | TAny | TClosureEnv ),
      _ ) ->
    false

and equal_list tys1 tys2 =
  try List.for_all2 equal tys1 tys2 with Invalid_argument _ -> false

(* Similar to [equal], but allows TAny holes *)
let rec unifiable ty1 ty2 =
  match Mark.remove ty1, Mark.remove ty2 with
  | TAny, _ | _, TAny -> true
  | TLit l1, TLit l2 -> equal_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> unifiable_list tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.equal n1 n2
  | TEnum n1, TEnum n2 -> EnumName.equal n1 n2
  | TOption t1, TOption t2 -> unifiable t1 t2
  | TArrow (t1, t1'), TArrow (t2, t2') ->
    unifiable_list t1 t2 && unifiable t1' t2'
  | TArray t1, TArray t2 -> unifiable t1 t2
  | TDefault t1, TDefault t2 -> unifiable t1 t2
  | TClosureEnv, TClosureEnv -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TDefault _ | TClosureEnv ),
      _ ) ->
    false

and unifiable_list tys1 tys2 =
  try List.for_all2 unifiable tys1 tys2 with Invalid_argument _ -> false

let rec compare ty1 ty2 =
  match Mark.remove ty1, Mark.remove ty2 with
  | TLit l1, TLit l2 -> compare_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> List.compare compare tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.compare n1 n2
  | TEnum en1, TEnum en2 -> EnumName.compare en1 en2
  | TOption t1, TOption t2 -> compare t1 t2
  | TArrow (a1, b1), TArrow (a2, b2) -> (
    match List.compare compare a1 a2 with 0 -> compare b1 b2 | n -> n)
  | TArray t1, TArray t2 -> compare t1 t2
  | TAny, TAny | TClosureEnv, TClosureEnv -> 0
  | TLit _, _ -> -1
  | _, TLit _ -> 1
  | TTuple _, _ -> -1
  | _, TTuple _ -> 1
  | TStruct _, _ -> -1
  | _, TStruct _ -> 1
  | TEnum _, _ -> -1
  | _, TEnum _ -> 1
  | TOption _, _ -> -1
  | _, TOption _ -> 1
  | TArrow _, _ -> -1
  | _, TArrow _ -> 1
  | TArray _, _ -> -1
  | _, TArray _ -> 1
  | TDefault _, _ -> -1
  | _, TDefault _ -> 1
  | TClosureEnv, _ -> -1
  | _, TClosureEnv -> 1

let rec arrow_return = function TArrow (_, b), _ -> arrow_return b | t -> t
