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
type var = naked_typ Bindlib.var

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
  | TAny tb1, TAny tb2 -> Bindlib.eq_mbinder equal tb1 tb2
  | TClosureEnv, TClosureEnv -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TDefault _ | TAny _ | TVar _ | TClosureEnv ),
      _ ) ->
    false

and equal_list tys1 tys2 =
  try List.for_all2 equal tys1 tys2 with Invalid_argument _ -> false

let rec compare (ty1: t) (ty2: t) =
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
    let _, ty1, ty2 = Bindlib.unmbind2 tb1 tb2 in
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
   * | _, TClosureEnv -> 1 *\) *)

let format = Print.typ

module Var = struct
  module Arg = struct
    type t = var
    let equal = Bindlib.eq_vars
    let compare = Bindlib.compare_vars
    let hash = Bindlib.hash_var
    let format ppf tv =
      Format.fprintf ppf "@{<bold><%s%s>@}" (Bindlib.name_of tv)
        (if Global.options.debug then "_"^ string_of_int (Bindlib.uid_of tv) else "")
  end
  include Arg
  module Set = Set.Make(Arg)
  module Map = Map.Make(Arg)
  module Hashtbl = Hashtbl.Make (Arg)

  let fresh () = Bindlib.new_var (fun v -> TVar v) "ty1"
end

let shallow_fold f ty acc =
  let lfold x acc = List.fold_left (fun acc x -> f x acc) acc x in
  Mark.fold
    (function
      | TLit _ | TStruct _ | TEnum _ | TClosureEnv | TVar _ -> acc
      | TTuple tl -> lfold tl acc
      | TOption ty | TArray ty | TDefault ty -> f ty acc
      | TArrow (tl, ty) -> lfold tl (f ty acc)
      | TAny tb ->
        let _v, ty = Bindlib.unmbind tb in
        f ty acc)
    ty

let rec free_vars (ty: t) =
  match Mark.remove ty with
  | TVar v -> Var.Set.singleton v
  | TAny tb ->
    let vs, ty = Bindlib.unmbind tb in
    Array.fold_left (fun set v -> Var.Set.remove v set) (free_vars ty) vs
  | _ ->
    shallow_fold
      (fun ty acc -> Var.Set.union acc (free_vars ty))
      ty Var.Set.empty

let rec free_vars_pos = function
  | TVar v, pos -> Var.Map.singleton v pos
  | TAny tb, _ ->
    let vs, ty = Bindlib.unmbind tb in
    Array.fold_left (fun map v -> Var.Map.remove v map) (free_vars_pos ty) vs
  | ty ->
    shallow_fold
      (fun ty acc -> Var.Map.union (fun _ _ x -> Some x) acc (free_vars_pos ty))
      ty Var.Map.empty

let rec unquantify = function
  | TAny tb, _ ->
    let _v, ty = Bindlib.unmbind tb in
    unquantify ty
  | ty -> ty

let fresh_var pos =
  TVar (Var.fresh ()), pos

(* TODO: deprecate and replace with fresh_var *)
let any = fresh_var

let universal pos =
  let v = Var.fresh () in
  let tb =
    Bindlib.bind_mvar [|v|]
      (Bindlib.box_apply (fun v -> v, pos) (Bindlib.box_var v))
  in
  TAny (Bindlib.unbox tb), pos

(* Similar to [equal], but allows TAny holes *)
let rec unifiable (ty1: t) (ty2: t) =
  match ty1, ty2 with
  | (TVar _, _), (TVar _, _) -> true
  | (TVar tv, _), ty | ty, (TVar tv, _) -> not (Var.Set.mem tv (free_vars ty))
  | (TAny tb, _), ty | ty, (TAny tb, _) ->
    let _, ty1 = Bindlib.unmbind tb in
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


let map: (t -> t Bindlib.box) -> t -> t Bindlib.box
  = fun f ty ->
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
  | TVar tv -> Bindlib.box_apply (fun v -> v, m) (Bindlib.box_var tv)
  | TAny tb ->
    let tv, ty = Bindlib.unmbind tb in
    (fun tb -> TAny tb) @& Bindlib.bind_mvar tv (f ty)
  | TClosureEnv -> Bindlib.box (TClosureEnv, m)

let rec rebox ty = map rebox ty

let hash ~strip (ty: t) =
  let open Hash.Op in
  let rec aux ctx (ty: t) =
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
      let _, ty, ctx = Bindlib.unmbind_in ctx tb in
      !`TAny % aux ctx ty
    | TClosureEnv -> !`TClosureEnv
  in
  aux Bindlib.empty_ctxt ty

let rec has_arrow decl_ctx (ty: t) =
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

module Map = Map.Make (struct
    type nonrec t = t
    let compare = compare
    let format = format
  end)
