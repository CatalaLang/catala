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
type var = typ_var

module Var = struct
  module Arg = struct
    type t = var

    let compare = Bindlib.compare_vars
    let format ppf v = String.format ppf (Bindlib.name_of v)
  end

  include Arg

  let fresh pos = Bindlib.new_var (fun v -> TVar v, pos) "ty1"

  module Set = Set.Make (Arg)
  module Map = Map.Make (Arg)
end

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
  | TVar tv1, TVar tv2 -> Bindlib.eq_vars tv1 tv2
  | TAny tb1, TAny tb2 -> Bindlib.eq_binder equal tb1 tb2
  | TClosureEnv, TClosureEnv -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TDefault _ | TVar _ | TAny _ | TClosureEnv ),
      _ ) ->
    false

and equal_list tys1 tys2 =
  try List.for_all2 equal tys1 tys2 with Invalid_argument _ -> false

let shallow_fold f ty acc =
  let lfold x acc = List.fold_left (fun acc x -> f x acc) acc x in
  Mark.fold
    (function
      | TLit _ | TStruct _ | TEnum _ | TClosureEnv | TVar _ -> acc
      | TTuple tl -> lfold tl acc
      | TOption ty | TArray ty | TDefault ty -> f ty acc
      | TArrow (tl, ty) -> lfold tl (f ty acc)
      | TAny tb ->
        let _v, ty = Bindlib.unbind tb in
        f ty acc)
    ty

let rec free_vars ty =
  match Mark.remove ty with
  | TVar v -> Var.Set.singleton v
  | TAny tb ->
    let v, ty = Bindlib.unbind tb in
    Var.Set.remove v (free_vars ty)
  | _ ->
    shallow_fold
      (fun ty acc -> Var.Set.union acc (free_vars ty))
      ty Var.Set.empty

let rec unquantify = function
  | TAny tb, _ ->
    let _v, ty = Bindlib.unbind tb in
    unquantify ty
  | ty -> ty

let any pos =
  let v = Var.fresh pos in
  let tb = Bindlib.bind_var v (Bindlib.box_var v) in
  TAny (Bindlib.unbox tb), pos

let new_var pos = TVar (Var.fresh pos), pos

(* Similar to [equal], but allows TAny holes *)
let rec unifiable ty1 ty2 =
  match ty1, ty2 with
  | (TVar _, _), (TVar _, _) -> true
  | (TVar tv, _), ty | ty, (TVar tv, _) -> not (Var.Set.mem tv (free_vars ty))
  | (TAny tb, _), ty | ty, (TAny tb, _) ->
    let _, ty1 = Bindlib.unbind tb in
    unifiable ty1 ty
  | (TLit l1, _), (TLit l2, _) -> equal_tlit l1 l2
  | (TTuple tys1, _), (TTuple tys2, _) -> unifiable_list tys1 tys2
  | (TStruct n1, _), (TStruct n2, _) -> StructName.equal n1 n2
  | (TEnum n1, _), (TEnum n2, _) -> EnumName.equal n1 n2
  | (TOption t1, _), (TOption t2, _) -> unifiable t1 t2
  | (TArrow (t1, t1'), _), (TArrow (t2, t2'), _) ->
    unifiable_list t1 t2 && unifiable t1' t2'
  | (TArray t1, _), (TArray t2, _) -> unifiable t1 t2
  | (TDefault t1, _), (TDefault t2, _) -> unifiable t1 t2
  | (TClosureEnv, _), (TClosureEnv, _) -> true
  | ( ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
        | TArray _ | TDefault _ | TClosureEnv ),
        _ ),
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
  | TVar tv1, TVar tv2 -> Bindlib.compare_vars tv1 tv2
  | TAny tb1, TAny tb2 ->
    let _, ty1, ty2 = Bindlib.unbind2 tb1 tb2 in
    compare ty1 ty2
  | TClosureEnv, TClosureEnv -> 0
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
  | TVar _, _ -> -1
  | _, TVar _ -> 1
  | TAny _, _ -> -1
  | _, TAny _ -> 1
(* | TClosureEnv, _ -> -1
 * | _, TClosureEnv -> 1 *)

let map f ty =
  let nty, m = ty in
  let ( @& ) f bty = Bindlib.box_apply (fun ty -> f ty, m) bty in
  match nty with
  | TLit l -> (fun l -> TLit l) @& Bindlib.box l
  | TTuple tl -> (fun tl -> TTuple tl) @& Bindlib.box_list (List.map f tl)
  | TStruct n -> (fun n -> TStruct n) @& Bindlib.box n
  | TEnum n -> (fun n -> TEnum n) @& Bindlib.box n
  | TOption ty -> (fun ty -> TOption ty) @& f ty
  | TArrow (tl, ty) ->
    Bindlib.box_apply2
      (fun tl ty -> TArrow (tl, ty), m)
      (Bindlib.box_list (List.map f tl))
      (f ty)
  | TArray ty -> (fun ty -> TArray ty) @& f ty
  | TDefault ty -> (fun ty -> TDefault ty) @& f ty
  | TVar tv -> Bindlib.box_var tv
  | TAny tb ->
    let tv, ty = Bindlib.unbind tb in
    (fun tb -> TAny tb) @& Bindlib.bind_var tv (f ty)
  | TClosureEnv -> Bindlib.box (TClosureEnv, m)

let rec rebox ty = map rebox ty

let quantify vars ty =
  let pos = Mark.get ty in
  Var.Set.fold (fun v ty ->
      Bindlib.box_apply (fun bnd -> TAny bnd, pos)
        (Bindlib.bind_var v ty))
    vars (rebox ty)
  |> Bindlib.unbox

let shallow_fold f t acc =
  let lfold tl acc = List.fold_left (fun acc x -> f x acc) acc tl in
  match Mark.remove t with
  | TLit _ | TStruct _ | TEnum _ | TAny _ | TVar _ | TClosureEnv -> acc
  | TTuple tl -> lfold tl acc
  | TOption t -> f t acc
  | TArrow (tl, t) -> lfold tl acc |> f t
  | TArray t -> f t acc
  | TDefault t -> f t acc

let hash ~strip ty =
  let open Hash.Op in
  let rec aux ctx ty =
    match Mark.remove ty with
    | TLit l -> !`TLit % !(l : typ_lit)
    | TTuple tl -> List.fold_left (fun acc ty -> acc % aux ctx ty) !`TTuple tl
    | TStruct n -> !`TStruct % StructName.hash ~strip n
    | TEnum n -> !`TEnum % EnumName.hash ~strip n
    | TOption ty -> !`TOption % aux ctx ty
    | TArrow (tl, ty) ->
      !`TArrow % List.fold_left (fun acc ty -> acc % aux ctx ty) (aux ctx ty) tl
    | TArray ty -> !`TArray % aux ctx ty
    | TDefault ty -> !`TDefault % aux ctx ty
    | TVar tv ->
      (* Bindlib.hash_var is not stable across executions *)
      !`TVar % !(Bindlib.name_of tv)
    | TAny tb ->
      let _, ty, ctx = Bindlib.unbind_in ctx tb in
      !`TAny % aux ctx ty
    | TClosureEnv -> !`TClosureEnv
  in
  aux Bindlib.empty_ctxt ty

let rec has_arrow decl_ctx ty =
  match Mark.remove ty with
  | TArrow _ -> true
  | TLit _ -> false
  | TVar _ | TAny _ | TClosureEnv -> invalid_arg "Type.has_arrow"
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
let format = Print.typ

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
  let format = format
end)
