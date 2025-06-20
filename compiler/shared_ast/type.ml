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

let map f ty =
  Mark.map
    (function
      | TLit l -> TLit l
      | TTuple tl -> TTuple (List.map f tl)
      | TStruct n -> TStruct n
      | TEnum n -> TEnum n
      | TOption ty -> TOption (f ty)
      | TArrow (tl, ty) -> TArrow (List.map f tl, f ty)
      | TArray ty -> TArray (f ty)
      | TDefault ty -> TDefault (f ty)
      | TAny -> TAny
      | TClosureEnv -> TClosureEnv)
    ty

let shallow_fold f t acc =
  let lfold tl acc = List.fold_left (fun acc x -> f x acc) acc tl in
  match Mark.remove t with
  | TLit _ | TStruct _ | TEnum _ | TAny | TClosureEnv -> acc
  | TTuple tl -> lfold tl acc
  | TOption t -> f t acc
  | TArrow (tl, t) -> lfold tl acc |> f t
  | TArray t -> f t acc
  | TDefault t -> f t acc

let rec hash ~strip ty =
  let open Hash.Op in
  match Mark.remove ty with
  | TLit l -> !`TLit % !(l : typ_lit)
  | TTuple tl -> List.fold_left (fun acc ty -> acc % hash ~strip ty) !`TTuple tl
  | TStruct n -> !`TStruct % StructName.hash ~strip n
  | TEnum n -> !`TEnum % EnumName.hash ~strip n
  | TOption ty -> !`TOption % hash ~strip ty
  | TArrow (tl, ty) ->
    !`TArrow
    % List.fold_left (fun acc ty -> acc % hash ~strip ty) (hash ~strip ty) tl
  | TArray ty -> !`TArray % hash ~strip ty
  | TDefault ty -> !`TDefault % hash ~strip ty
  | TAny -> !`TAny
  | TClosureEnv -> !`TClosureEnv

let rec has_arrow decl_ctx ty =
  match Mark.remove ty with
  | TArrow _ -> true
  | TLit _ -> false
  | TAny | TClosureEnv -> invalid_arg "Type.has_arrow"
  | TTuple tl -> List.exists (has_arrow decl_ctx) tl
  | TStruct n ->
    StructField.Map.exists
      (fun _ -> has_arrow decl_ctx)
      (StructName.Map.find n decl_ctx.ctx_structs)
  | TEnum n ->
    EnumConstructor.Map.exists
      (fun _ -> has_arrow decl_ctx)
      (EnumName.Map.find n decl_ctx.ctx_enums)
  | TOption ty | TArray ty | TDefault ty -> has_arrow decl_ctx ty

let rec arrow_return = function TArrow (_, b), _ -> arrow_return b | t -> t
let format = Print.typ_debug

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
  let format = format
end)
