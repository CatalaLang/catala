(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Definitions

(** Functions handling the types of [shared_ast] *)

(* Basic block constructors *)

let evar v mark = Bindlib.box_apply (Marked.mark mark) (Bindlib.box_var v)

let etuple args s mark =
  Bindlib.box_apply (fun args -> ETuple (args, s), mark) (Bindlib.box_list args)

let etupleaccess e1 i s typs mark =
  Bindlib.box_apply (fun e1 -> ETupleAccess (e1, i, s, typs), mark) e1

let einj e1 i e_name typs mark =
  Bindlib.box_apply (fun e1 -> EInj (e1, i, e_name, typs), mark) e1

let ematch arg arms e_name mark =
  Bindlib.box_apply2
    (fun arg arms -> EMatch (arg, arms, e_name), mark)
    arg (Bindlib.box_list arms)

let earray args mark =
  Bindlib.box_apply (fun args -> EArray args, mark) (Bindlib.box_list args)

let elit l mark = Bindlib.box (ELit l, mark)

let eabs binder typs mark =
  Bindlib.box_apply (fun binder -> EAbs (binder, typs), mark) binder

let eapp e1 args mark =
  Bindlib.box_apply2
    (fun e1 args -> EApp (e1, args), mark)
    e1 (Bindlib.box_list args)

let eassert e1 mark = Bindlib.box_apply (fun e1 -> EAssert e1, mark) e1
let eop op mark = Bindlib.box (EOp op, mark)

let edefault excepts just cons mark =
  Bindlib.box_apply3
    (fun excepts just cons -> EDefault (excepts, just, cons), mark)
    (Bindlib.box_list excepts) just cons

let eifthenelse e1 e2 e3 mark =
  Bindlib.box_apply3 (fun e1 e2 e3 -> EIfThenElse (e1, e2, e3), mark) e1 e2 e3

let eerroronempty e1 mark =
  Bindlib.box_apply (fun e1 -> ErrorOnEmpty e1, mark) e1

let eraise e1 mark = Bindlib.box (ERaise e1, mark)

let ecatch e1 exn e2 mark =
  Bindlib.box_apply2 (fun e1 e2 -> ECatch (e1, exn, e2), mark) e1 e2

let elocation loc mark = Bindlib.box (ELocation loc, mark)

let estruct name fields mark =
  Bindlib.box_apply (fun es -> EStruct (name, es), mark) fields

let estructaccess e1 field struc mark =
  Bindlib.box_apply (fun e1 -> EStructAccess (e1, field, struc), mark) e1

let eenuminj e1 cons enum mark =
  Bindlib.box_apply (fun e1 -> EEnumInj (e1, cons, enum), mark) e1

let ematchs e1 enum cases mark =
  Bindlib.box_apply2 (fun e1 cases -> EMatchS (e1, enum, cases), mark) e1 cases

(* - Manipulation of marks - *)

let no_mark : type m. m mark -> m mark = function
  | Untyped _ -> Untyped { pos = Pos.no_pos }
  | Typed _ -> Typed { pos = Pos.no_pos; ty = Marked.mark Pos.no_pos TAny }

let mark_pos (type m) (m : m mark) : Pos.t =
  match m with Untyped { pos } | Typed { pos; _ } -> pos

let pos (type m) (x : ('a, m mark) Marked.t) : Pos.t =
  mark_pos (Marked.get_mark x)

let ty (_, m) : marked_typ = match m with Typed { ty; _ } -> ty

let with_ty (type m) (ty : marked_typ) (x : ('a, m mark) Marked.t) :
    ('a, typed mark) Marked.t =
  Marked.mark
    (match Marked.get_mark x with
    | Untyped { pos } -> Typed { pos; ty }
    | Typed m -> Typed { m with ty })
    (Marked.unmark x)

let map_mark
    (type m)
    (pos_f : Pos.t -> Pos.t)
    (ty_f : marked_typ -> marked_typ)
    (m : m mark) : m mark =
  match m with
  | Untyped { pos } -> Untyped { pos = pos_f pos }
  | Typed { pos; ty } -> Typed { pos = pos_f pos; ty = ty_f ty }

let map_mark2
    (type m)
    (pos_f : Pos.t -> Pos.t -> Pos.t)
    (ty_f : typed -> typed -> marked_typ)
    (m1 : m mark)
    (m2 : m mark) : m mark =
  match m1, m2 with
  | Untyped m1, Untyped m2 -> Untyped { pos = pos_f m1.pos m2.pos }
  | Typed m1, Typed m2 -> Typed { pos = pos_f m1.pos m2.pos; ty = ty_f m1 m2 }

let fold_marks
    (type m)
    (pos_f : Pos.t list -> Pos.t)
    (ty_f : typed list -> marked_typ)
    (ms : m mark list) : m mark =
  match ms with
  | [] -> invalid_arg "Dcalc.Ast.fold_mark"
  | Untyped _ :: _ as ms ->
    Untyped { pos = pos_f (List.map (function Untyped { pos } -> pos) ms) }
  | Typed _ :: _ ->
    Typed
      {
        pos = pos_f (List.map (function Typed { pos; _ } -> pos) ms);
        ty = ty_f (List.map (function Typed m -> m) ms);
      }

(* - Traversal functions - *)

(* shallow map *)
let map
    (type a)
    (ctx : 'ctx)
    ~(f : 'ctx -> (a, 'm1) marked_gexpr -> (a, 'm2) marked_gexpr Bindlib.box)
    (e : ((a, 'm1) gexpr, 'm2) Marked.t) : (a, 'm2) marked_gexpr Bindlib.box =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | ELit l -> elit l m
  | EApp (e1, args) -> eapp (f ctx e1) (List.map (f ctx) args) m
  | EOp op -> Bindlib.box (EOp op, m)
  | EArray args -> earray (List.map (f ctx) args) m
  | EVar v -> evar (Var.translate v) m
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    eabs (Bindlib.bind_mvar (Array.map Var.translate vars) (f ctx body)) typs m
  | EIfThenElse (e1, e2, e3) ->
    eifthenelse ((f ctx) e1) ((f ctx) e2) ((f ctx) e3) m
  | ETuple (args, s) -> etuple (List.map (f ctx) args) s m
  | ETupleAccess (e1, n, s_name, typs) ->
    etupleaccess ((f ctx) e1) n s_name typs m
  | EInj (e1, i, e_name, typs) -> einj ((f ctx) e1) i e_name typs m
  | EMatch (arg, arms, e_name) ->
    ematch ((f ctx) arg) (List.map (f ctx) arms) e_name m
  | EAssert e1 -> eassert ((f ctx) e1) m
  | EDefault (excepts, just, cons) ->
    edefault (List.map (f ctx) excepts) ((f ctx) just) ((f ctx) cons) m
  | ErrorOnEmpty e1 -> eerroronempty ((f ctx) e1) m
  | ECatch (e1, exn, e2) -> ecatch (f ctx e1) exn (f ctx e2) m
  | ERaise exn -> eraise exn m
  | ELocation loc -> elocation loc m
  | EStruct (name, fields) ->
    let fields =
      StructFieldMap.fold
        (fun fld e -> Bindlib.box_apply2 (StructFieldMap.add fld) (f ctx e))
        fields
        (Bindlib.box StructFieldMap.empty)
    in
    estruct name fields m
  | EStructAccess (e1, field, struc) -> estructaccess (f ctx e1) field struc m
  | EEnumInj (e1, cons, enum) -> eenuminj (f ctx e1) cons enum m
  | EMatchS (e1, enum, cases) ->
    let cases =
      EnumConstructorMap.fold
        (fun cstr e ->
          Bindlib.box_apply2 (EnumConstructorMap.add cstr) (f ctx e))
        cases
        (Bindlib.box EnumConstructorMap.empty)
    in
    ematchs (f ctx e1) enum cases m

let rec map_top_down ~f e = map () ~f:(fun () -> map_top_down ~f) (f e)

let map_marks ~f e =
  map_top_down ~f:(fun e -> Marked.(mark (f (get_mark e)) (unmark e))) e

(* - *)

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let box e =
  let rec id_t () e = map () ~f:id_t e in
  id_t () e

let untype e = map_marks ~f:(fun m -> Untyped { pos = mark_pos m }) e

(* - Expression building helpers - *)

let make_var (x, mark) =
  Bindlib.box_apply (fun x -> x, mark) (Bindlib.box_var x)

let make_abs xs e taus mark =
  Bindlib.box_apply (fun b -> EAbs (b, taus), mark) (Bindlib.bind_mvar xs e)

let make_app e u mark =
  Bindlib.box_apply2 (fun e u -> EApp (e, u), mark) e (Bindlib.box_list u)

let empty_thunked_term mark =
  let silent = Var.make "_" in
  let pos = mark_pos mark in
  Bindlib.unbox
    (make_abs [| silent |]
       (Bindlib.box (ELit LEmptyError, mark))
       [TLit TUnit, pos]
       (map_mark
          (fun pos -> pos)
          (fun ty ->
            Marked.mark pos (TArrow (Marked.mark pos (TLit TUnit), ty)))
          mark))

let make_let_in x tau e1 e2 pos =
  let m_e1 = Marked.get_mark (Bindlib.unbox e1) in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> Marked.mark pos (TArrow (m1.ty, m2.ty)))
      m_e1 m_e2
  in
  make_app (make_abs [| x |] e2 [tau] m_abs) [e1] m_e2

let make_multiple_let_in xs taus e1s e2 pos =
  (* let m_e1s = List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s in *)
  let m_e1s =
    fold_marks List.hd
      (fun tys -> TTuple (List.map (fun t -> t.ty) tys), (List.hd tys).pos)
      (List.map (fun e -> Marked.get_mark (Bindlib.unbox e)) e1s)
  in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> Marked.mark pos (TArrow (m1.ty, m2.ty)))
      m_e1s m_e2
  in
  make_app (make_abs xs e2 taus m_abs) e1s m_e2

(* Tests *)

let is_value (type a) (e : (a, 'm mark) gexpr marked) =
  match Marked.unmark e with
  | ELit _ | EAbs _ | EOp _ | ERaise _ -> true
  | _ -> false

let rec equal_typs ty1 ty2 =
  match Marked.unmark ty1, Marked.unmark ty2 with
  | TLit l1, TLit l2 -> l1 = l2
  | TTuple tys1, TTuple tys2 -> equal_typs_list tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.equal n1 n2
  | TEnum n1, TEnum n2 -> EnumName.equal n1 n2
  | TOption t1, TOption t2 -> equal_typs t1 t2
  | TArrow (t1, t1'), TArrow (t2, t2') -> equal_typs t1 t2 && equal_typs t1' t2'
  | TArray t1, TArray t2 -> equal_typs t1 t2
  | TAny, TAny -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TAny ),
      _ ) ->
    false

and equal_typs_list tys1 tys2 =
  try List.for_all2 equal_typs tys1 tys2 with Invalid_argument _ -> false

let equal_log_entries l1 l2 =
  match l1, l2 with
  | VarDef t1, VarDef t2 -> equal_typs (t1, Pos.no_pos) (t2, Pos.no_pos)
  | x, y -> x = y

let equal_unops op1 op2 =
  match op1, op2 with
  (* Log entries contain a typ which contain position information, we thus need
     to descend into them *)
  | Log (l1, info1), Log (l2, info2) -> equal_log_entries l1 l2 && info1 = info2
  | Log _, _ | _, Log _ -> false
  (* All the other cases can be discharged through equality *)
  | ( ( Not | Minus _ | Length | IntToRat | MoneyToRat | RatToMoney | GetDay
      | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth | RoundMoney
      | RoundDecimal ),
      _ ) ->
    op1 = op2

let equal_ops op1 op2 =
  match op1, op2 with
  | Ternop op1, Ternop op2 -> op1 = op2
  | Binop op1, Binop op2 -> op1 = op2
  | Unop op1, Unop op2 -> equal_unops op1 op2
  | _, _ -> false

let equal_except ex1 ex2 = ex1 = ex2

(* weird indentation; see
   https://github.com/ocaml-ppx/ocamlformat/issues/2143 *)
let rec equal_list :
          'a. ('a, 't) gexpr marked list -> ('a, 't) gexpr marked list -> bool =
 fun es1 es2 ->
  try List.for_all2 equal es1 es2 with Invalid_argument _ -> false

and equal : type a. (a, 't) gexpr marked -> (a, 't) gexpr marked -> bool =
 fun e1 e2 ->
  match Marked.unmark e1, Marked.unmark e2 with
  | EVar v1, EVar v2 -> Bindlib.eq_vars v1 v2
  | ETuple (es1, n1), ETuple (es2, n2) -> n1 = n2 && equal_list es1 es2
  | ETupleAccess (e1, id1, n1, tys1), ETupleAccess (e2, id2, n2, tys2) ->
    equal e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EInj (e1, id1, n1, tys1), EInj (e2, id2, n2, tys2) ->
    equal e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EMatch (e1, cases1, n1), EMatch (e2, cases2, n2) ->
    n1 = n2 && equal e1 e2 && equal_list cases1 cases2
  | EArray es1, EArray es2 -> equal_list es1 es2
  | ELit l1, ELit l2 -> l1 = l2
  | EAbs (b1, tys1), EAbs (b2, tys2) ->
    equal_typs_list tys1 tys2
    &&
    let vars1, body1 = Bindlib.unmbind b1 in
    let body2 = Bindlib.msubst b2 (Array.map (fun x -> EVar x) vars1) in
    equal body1 body2
  | EApp (e1, args1), EApp (e2, args2) -> equal e1 e2 && equal_list args1 args2
  | EAssert e1, EAssert e2 -> equal e1 e2
  | EOp op1, EOp op2 -> equal_ops op1 op2
  | EDefault (exc1, def1, cons1), EDefault (exc2, def2, cons2) ->
    equal def1 def2 && equal cons1 cons2 && equal_list exc1 exc2
  | EIfThenElse (if1, then1, else1), EIfThenElse (if2, then2, else2) ->
    equal if1 if2 && equal then1 then2 && equal else1 else2
  | ErrorOnEmpty e1, ErrorOnEmpty e2 -> equal e1 e2
  | ERaise ex1, ERaise ex2 -> equal_except ex1 ex2
  | ECatch (etry1, ex1, ewith1), ECatch (etry2, ex2, ewith2) ->
    equal etry1 etry2 && equal_except ex1 ex2 && equal ewith1 ewith2
  | ELocation _, ELocation _ -> true
  | EStruct (s1, fields1), EStruct (s2, fields2) ->
    StructName.equal s1 s2 && StructFieldMap.equal equal fields1 fields2
  | EStructAccess (e1, f1, s1), EStructAccess (e2, f2, s2) ->
    StructName.equal s1 s2 && StructFieldName.equal f1 f2 && equal e1 e2
  | EEnumInj (e1, c1, n1), EEnumInj (e2, c2, n2) ->
    EnumName.equal n1 n2 && EnumConstructor.equal c1 c2 && equal e1 e2
  | EMatchS (e1, n1, cases1), EMatchS (e2, n2, cases2) ->
    EnumName.equal n1 n2
    && equal e1 e2
    && EnumConstructorMap.equal equal cases1 cases2
  | ( ( EVar _ | ETuple _ | ETupleAccess _ | EInj _ | EMatch _ | EArray _
      | ELit _ | EAbs _ | EApp _ | EAssert _ | EOp _ | EDefault _
      | EIfThenElse _ | ErrorOnEmpty _ | ERaise _ | ECatch _ | ELocation _
      | EStruct _ | EStructAccess _ | EEnumInj _ | EMatchS _ ),
      _ ) ->
    false

let rec free_vars : type a. (a, 't) gexpr marked -> (a, 't) gexpr Var.Set.t =
 fun e ->
  match Marked.unmark e with
  | EOp _ | ELit _ | ERaise _ -> Var.Set.empty
  | EVar v -> Var.Set.singleton v
  | ETuple (es, _) ->
    es |> List.map free_vars |> List.fold_left Var.Set.union Var.Set.empty
  | EArray es ->
    es |> List.map free_vars |> List.fold_left Var.Set.union Var.Set.empty
  | ETupleAccess (e1, _, _, _) -> free_vars e1
  | EAssert e1 -> free_vars e1
  | EInj (e1, _, _, _) -> free_vars e1
  | ErrorOnEmpty e1 -> free_vars e1
  | ECatch (etry, _, ewith) -> Var.Set.union (free_vars etry) (free_vars ewith)
  | EApp (e1, es) ->
    e1 :: es |> List.map free_vars |> List.fold_left Var.Set.union Var.Set.empty
  | EMatch (e1, es, _) ->
    e1 :: es |> List.map free_vars |> List.fold_left Var.Set.union Var.Set.empty
  | EDefault (es, ejust, econs) ->
    ejust :: econs :: es
    |> List.map free_vars
    |> List.fold_left Var.Set.union Var.Set.empty
  | EIfThenElse (e1, e2, e3) ->
    [e1; e2; e3]
    |> List.map free_vars
    |> List.fold_left Var.Set.union Var.Set.empty
  | EAbs (binder, _) ->
    let vs, body = Bindlib.unmbind binder in
    Array.fold_right Var.Set.remove vs (free_vars body)
  | ELocation _ -> Var.Set.empty
  | EStruct (_, fields) ->
    StructFieldMap.fold
      (fun _ e -> Var.Set.union (free_vars e))
      fields Var.Set.empty
  | EStructAccess (e1, _, _) -> free_vars e1
  | EEnumInj (e1, _, _) -> free_vars e1
  | EMatchS (e1, _, cases) ->
    free_vars e1
    |> EnumConstructorMap.fold (fun _ e -> Var.Set.union (free_vars e)) cases

let remove_logging_calls e =
  let rec f () e =
    match Marked.unmark e with
    | EApp ((EOp (Unop (Log _)), _), [arg]) -> map () ~f arg
    | _ -> map () ~f e
  in
  f () e

let format ?debug decl_ctx ppf e = Print.expr ?debug decl_ctx ppf e

let rec size : type a. (a, 't) gexpr marked -> int =
 fun e ->
  match Marked.unmark e with
  | EVar _ | ELit _ | EOp _ -> 1
  | ETuple (args, _) -> List.fold_left (fun acc arg -> acc + size arg) 1 args
  | EArray args -> List.fold_left (fun acc arg -> acc + size arg) 1 args
  | ETupleAccess (e1, _, _, _) -> size e1 + 1
  | EInj (e1, _, _, _) -> size e1 + 1
  | EAssert e1 -> size e1 + 1
  | ErrorOnEmpty e1 -> size e1 + 1
  | EMatch (arg, args, _) ->
    List.fold_left (fun acc arg -> acc + size arg) (1 + size arg) args
  | EApp (arg, args) ->
    List.fold_left (fun acc arg -> acc + size arg) (1 + size arg) args
  | EAbs (binder, _) ->
    let _, body = Bindlib.unmbind binder in
    1 + size body
  | EIfThenElse (e1, e2, e3) -> 1 + size e1 + size e2 + size e3
  | EDefault (exceptions, just, cons) ->
    List.fold_left
      (fun acc except -> acc + size except)
      (1 + size just + size cons)
      exceptions
  | ERaise _ -> 1
  | ECatch (etry, _, ewith) -> 1 + size etry + size ewith
  | ELocation _ -> 1
  | EStruct (_, fields) ->
    StructFieldMap.fold (fun _ e acc -> acc + 1 + size e) fields 0
  | EStructAccess (e1, _, _) -> 1 + size e1
  | EEnumInj (e1, _, _) -> 1 + size e1
  | EMatchS (e1, _, cases) ->
    EnumConstructorMap.fold (fun _ e acc -> acc + 1 + size e) cases (size e1)
