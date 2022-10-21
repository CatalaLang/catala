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

module Box = struct
  module B = Bindlib

  let app0 x mark = B.box x, mark
  let app1 (xb, m) f mark = B.box_apply (fun x -> f (x, m)) xb, mark

  let app2 (xb1, m1) (xb2, m2) f mark =
    B.box_apply2 (fun x1 x2 -> f (x1, m1) (x2, m2)) xb1 xb2, mark

  let app3 (xb1, m1) (xb2, m2) (xb3, m3) f mark =
    ( B.box_apply3 (fun x1 x2 x3 -> f (x1, m1) (x2, m2) (x3, m3)) xb1 xb2 xb3,
      mark )

  let appn xmbl f mark =
    let xbl, ml = List.split xmbl in
    B.box_apply (fun xl -> f (List.combine xl ml)) (B.box_list xbl), mark

  let app1n (xb0, m0) xmbl f mark =
    let xbl, ml = List.split xmbl in
    ( B.box_apply2
        (fun x0 xl -> f (x0, m0) (List.combine xl ml))
        xb0 (B.box_list xbl),
      mark )

  let app2n (xb0, m0) (xb1, m1) xmbl f mark =
    let xbl, ml = List.split xmbl in
    ( B.box_apply3
        (fun x0 x1 xl -> f (x0, m0) (x1, m1) (List.combine xl ml))
        xb0 xb1 (B.box_list xbl),
      mark )

  let lift : ('a, 't) boxed_gexpr -> ('a, 't) gexpr B.box =
   fun em ->
    B.box_apply (fun e -> Marked.mark (Marked.get_mark em) e) (Marked.unmark em)

  module LiftStruct = Bindlib.Lift (StructFieldMap)

  let lift_struct = LiftStruct.lift_box

  module LiftEnum = Bindlib.Lift (EnumConstructorMap)

  let lift_enum = LiftEnum.lift_box

  module LiftScopeVars = Bindlib.Lift (ScopeVarMap)

  let lift_scope_vars = LiftScopeVars.lift_box
end

let bind vars e = Bindlib.bind_mvar vars (Box.lift e)

let subst binder vars =
  Bindlib.msubst binder (Array.of_list (List.map Marked.unmark vars))

let evar v mark = Marked.mark mark (Bindlib.box_var v)
let etuple args s = Box.appn args @@ fun args -> ETuple (args, s)

let etupleaccess e1 i s typs =
  Box.app1 e1 @@ fun e1 -> ETupleAccess (e1, i, s, typs)

let einj e1 i e_name typs = Box.app1 e1 @@ fun e1 -> EInj (e1, i, e_name, typs)

let ematch arg arms e_name =
  Box.app1n arg arms @@ fun arg arms -> EMatch (arg, arms, e_name)

let earray args = Box.appn args @@ fun args -> EArray args
let elit l mark = Marked.mark mark (Bindlib.box (ELit l))

let eabs binder typs mark =
  Bindlib.box_apply (fun binder -> EAbs (binder, typs)) binder, mark

let eapp e1 args = Box.app1n e1 args @@ fun e1 args -> EApp (e1, args)
let eassert e1 = Box.app1 e1 @@ fun e1 -> EAssert e1
let eop op = Box.app0 @@ EOp op

let edefault excepts just cons =
  Box.app2n just cons excepts
  @@ fun just cons excepts -> EDefault (excepts, just, cons)

let eifthenelse e1 e2 e3 =
  Box.app3 e1 e2 e3 @@ fun e1 e2 e3 -> EIfThenElse (e1, e2, e3)

let eerroronempty e1 = Box.app1 e1 @@ fun e1 -> ErrorOnEmpty e1
let eraise e1 = Box.app0 @@ ERaise e1
let ecatch e1 exn e2 = Box.app2 e1 e2 @@ fun e1 e2 -> ECatch (e1, exn, e2)
let elocation loc = Box.app0 @@ ELocation loc

let estruct name (fields : ('a, 't) boxed_gexpr StructFieldMap.t) mark =
  Marked.mark mark
  @@ Bindlib.box_apply
       (fun fields -> EStruct (name, fields))
       (Box.lift_struct (StructFieldMap.map Box.lift fields))

let estructaccess e1 field struc =
  Box.app1 e1 @@ fun e1 -> EStructAccess (e1, field, struc)

let eenuminj e1 cons enum = Box.app1 e1 @@ fun e1 -> EEnumInj (e1, cons, enum)

let ematchs e1 enum cases mark =
  Marked.mark mark
  @@ Bindlib.box_apply2
       (fun e1 cases -> EMatchS (e1, enum, cases))
       (Box.lift e1)
       (Box.lift_enum (EnumConstructorMap.map Box.lift cases))

(* - Manipulation of marks - *)

let no_mark : type m. m mark -> m mark = function
  | Untyped _ -> Untyped { pos = Pos.no_pos }
  | Typed _ -> Typed { pos = Pos.no_pos; ty = Marked.mark Pos.no_pos TAny }

let mark_pos (type m) (m : m mark) : Pos.t =
  match m with Untyped { pos } | Typed { pos; _ } -> pos

let pos (type m) (x : ('a, m mark) Marked.t) : Pos.t =
  mark_pos (Marked.get_mark x)

let ty (_, m) : typ = match m with Typed { ty; _ } -> ty

let set_ty (type m) (ty : typ) (x : ('a, m mark) Marked.t) :
    ('a, typed mark) Marked.t =
  Marked.mark
    (match Marked.get_mark x with
    | Untyped { pos } -> Typed { pos; ty }
    | Typed m -> Typed { m with ty })
    (Marked.unmark x)

let map_mark (type m) (pos_f : Pos.t -> Pos.t) (ty_f : typ -> typ) (m : m mark)
    : m mark =
  match m with
  | Untyped { pos } -> Untyped { pos = pos_f pos }
  | Typed { pos; ty } -> Typed { pos = pos_f pos; ty = ty_f ty }

let map_mark2
    (type m)
    (pos_f : Pos.t -> Pos.t -> Pos.t)
    (ty_f : typed -> typed -> typ)
    (m1 : m mark)
    (m2 : m mark) : m mark =
  match m1, m2 with
  | Untyped m1, Untyped m2 -> Untyped { pos = pos_f m1.pos m2.pos }
  | Typed m1, Typed m2 -> Typed { pos = pos_f m1.pos m2.pos; ty = ty_f m1 m2 }

let fold_marks
    (type m)
    (pos_f : Pos.t list -> Pos.t)
    (ty_f : typed list -> typ)
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

let with_pos (type m) (pos : Pos.t) (m : m mark) : m mark =
  map_mark (fun _ -> pos) (fun ty -> ty) m

let map_ty (type m) (ty_f : typ -> typ) (m : m mark) : m mark =
  map_mark (fun pos -> pos) ty_f m

let with_ty (type m) (m : m mark) ?pos (ty : typ) : m mark =
  map_mark (fun default -> Option.value pos ~default) (fun _ -> ty) m

let maybe_ty (type m) ?(typ = TAny) (m : m mark) : typ =
  match m with Untyped { pos } -> Marked.mark pos typ | Typed { ty; _ } -> ty

(* - Traversal functions - *)

(* shallow map *)
let map
    (type a)
    (ctx : 'ctx)
    ~(f : 'ctx -> (a, 'm1) gexpr -> (a, 'm2) boxed_gexpr)
    (e : ((a, 'm1) naked_gexpr, 'm2) Marked.t) : (a, 'm2) boxed_gexpr =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | ELit l -> elit l m
  | EApp (e1, args) -> eapp (f ctx e1) (List.map (f ctx) args) m
  | EOp op -> eop op m
  | EArray args -> earray (List.map (f ctx) args) m
  | EVar v -> evar (Var.translate v) m
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    let body = f ctx body in
    let binder = bind (Array.map Var.translate vars) body in
    eabs binder typs m
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
    let fields = StructFieldMap.map (f ctx) fields in
    estruct name fields m
  | EStructAccess (e1, field, struc) -> estructaccess (f ctx e1) field struc m
  | EEnumInj (e1, cons, enum) -> eenuminj (f ctx e1) cons enum m
  | EMatchS (e1, enum, cases) ->
    let cases = EnumConstructorMap.map (f ctx) cases in
    ematchs (f ctx e1) enum cases m

let rec map_top_down ~f e = map () ~f:(fun () -> map_top_down ~f) (f e)

let map_marks ~f e =
  map_top_down ~f:(fun e -> Marked.(mark (f (get_mark e)) (unmark e))) e

(* Folds the given function on the direct children of the given expression. Does
   not open binders. *)
let shallow_fold
    (type a)
    (f : (a, 'm) gexpr -> 'acc -> 'acc)
    (e : (a, 'm) gexpr)
    (acc : 'acc) : 'acc =
  let lfold x acc = List.fold_left (fun acc x -> f x acc) acc x in
  match Marked.unmark e with
  | ELit _ | EOp _ | EVar _ | ERaise _ | ELocation _ -> acc
  | EApp (e1, args) -> acc |> f e1 |> lfold args
  | EArray args -> acc |> lfold args
  | EAbs _ -> acc
  | EIfThenElse (e1, e2, e3) -> acc |> f e1 |> f e2 |> f e3
  | ETuple (args, _) -> acc |> lfold args
  | ETupleAccess (e1, _, _, _) -> acc |> f e1
  | EInj (e1, _, _, _) -> acc |> f e1
  | EMatch (arg, arms, _) -> acc |> f arg |> lfold arms
  | EAssert e1 -> acc |> f e1
  | EDefault (excepts, just, cons) -> acc |> lfold excepts |> f just |> f cons
  | ErrorOnEmpty e1 -> acc |> f e1
  | ECatch (e1, _, e2) -> acc |> f e1 |> f e2
  | EStruct (_, fields) -> acc |> StructFieldMap.fold (fun _ -> f) fields
  | EStructAccess (e1, _, _) -> acc |> f e1
  | EEnumInj (e1, _, _) -> acc |> f e1
  | EMatchS (e1, _, cases) ->
    acc |> f e1 |> EnumConstructorMap.fold (fun _ -> f) cases

(* - *)

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let rebox e =
  let rec id_t () e = map () ~f:id_t e in
  id_t () e

let box e = Marked.same_mark_as (Bindlib.box (Marked.unmark e)) e
let unbox (e, m) = Bindlib.unbox e, m
let untype e = map_marks ~f:(fun m -> Untyped { pos = mark_pos m }) e

(* Tests *)

let is_value (type a) (e : (a, _) gexpr) =
  match Marked.unmark e with
  | ELit _ | EAbs _ | EOp _ | ERaise _ -> true
  | _ -> false

let equal_tlit l1 l2 = l1 = l2
let compare_tlit l1 l2 = Stdlib.compare l1 l2

let rec equal_typ ty1 ty2 =
  match Marked.unmark ty1, Marked.unmark ty2 with
  | TLit l1, TLit l2 -> equal_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> equal_typ_list tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.equal n1 n2
  | TEnum n1, TEnum n2 -> EnumName.equal n1 n2
  | TOption t1, TOption t2 -> equal_typ t1 t2
  | TArrow (t1, t1'), TArrow (t2, t2') -> equal_typ t1 t2 && equal_typ t1' t2'
  | TArray t1, TArray t2 -> equal_typ t1 t2
  | TAny, TAny -> true
  | ( ( TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _
      | TArray _ | TAny ),
      _ ) ->
    false

and equal_typ_list tys1 tys2 =
  try List.for_all2 equal_typ tys1 tys2 with Invalid_argument _ -> false

(* Similar to [equal_typ], but allows TAny holes *)
let rec unifiable ty1 ty2 =
  match Marked.unmark ty1, Marked.unmark ty2 with
  | TAny, _ | _, TAny -> true
  | TLit l1, TLit l2 -> equal_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> unifiable_list tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.equal n1 n2
  | TEnum n1, TEnum n2 -> EnumName.equal n1 n2
  | TOption t1, TOption t2 -> unifiable t1 t2
  | TArrow (t1, t1'), TArrow (t2, t2') -> unifiable t1 t2 && unifiable t1' t2'
  | TArray t1, TArray t2 -> unifiable t1 t2
  | ( (TLit _ | TTuple _ | TStruct _ | TEnum _ | TOption _ | TArrow _ | TArray _),
      _ ) ->
    false

and unifiable_list tys1 tys2 =
  try List.for_all2 unifiable tys1 tys2 with Invalid_argument _ -> false

let rec compare_typ ty1 ty2 =
  match Marked.unmark ty1, Marked.unmark ty2 with
  | TLit l1, TLit l2 -> compare_tlit l1 l2
  | TTuple tys1, TTuple tys2 -> List.compare compare_typ tys1 tys2
  | TStruct n1, TStruct n2 -> StructName.compare n1 n2
  | TEnum en1, TEnum en2 -> EnumName.compare en1 en2
  | TOption t1, TOption t2 -> compare_typ t1 t2
  | TArrow (a1, b1), TArrow (a2, b2) -> (
    match compare_typ a1 a2 with 0 -> compare_typ b1 b2 | n -> n)
  | TArray t1, TArray t2 -> compare_typ t1 t2
  | TAny, TAny -> 0
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

let equal_lit (type a) (l1 : a glit) (l2 : a glit) =
  match l1, l2 with
  | LBool b1, LBool b2 -> Bool.equal b1 b2
  | LEmptyError, LEmptyError -> true
  | LInt n1, LInt n2 -> Runtime.( =! ) n1 n2
  | LRat r1, LRat r2 -> Runtime.( =& ) r1 r2
  | LMoney m1, LMoney m2 -> Runtime.( =$ ) m1 m2
  | LUnit, LUnit -> true
  | LDate d1, LDate d2 -> Runtime.( =@ ) d1 d2
  | LDuration d1, LDuration d2 -> Runtime.( =^ ) d1 d2
  | ( ( LBool _ | LEmptyError | LInt _ | LRat _ | LMoney _ | LUnit | LDate _
      | LDuration _ ),
      _ ) ->
    false

let compare_lit (type a) (l1 : a glit) (l2 : a glit) =
  match l1, l2 with
  | LBool b1, LBool b2 -> Bool.compare b1 b2
  | LEmptyError, LEmptyError -> 0
  | LInt n1, LInt n2 ->
    if Runtime.( <! ) n1 n2 then -1 else if Runtime.( =! ) n1 n2 then 0 else 1
  | LRat r1, LRat r2 ->
    if Runtime.( <& ) r1 r2 then -1 else if Runtime.( =& ) r1 r2 then 0 else 1
  | LMoney m1, LMoney m2 ->
    if Runtime.( <$ ) m1 m2 then -1 else if Runtime.( =$ ) m1 m2 then 0 else 1
  | LUnit, LUnit -> 0
  | LDate d1, LDate d2 ->
    if Runtime.( <@ ) d1 d2 then -1 else if Runtime.( =@ ) d1 d2 then 0 else 1
  | LDuration d1, LDuration d2 -> (
    (* Duration comparison in the runtime may fail, so rely on a basic
       lexicographic comparison instead *)
    let y1, m1, d1 = Runtime.duration_to_years_months_days d1 in
    let y2, m2, d2 = Runtime.duration_to_years_months_days d2 in
    match compare y1 y2 with
    | 0 -> ( match compare m1 m2 with 0 -> compare d1 d2 | n -> n)
    | n -> n)
  | LBool _, _ -> -1
  | _, LBool _ -> 1
  | LEmptyError, _ -> -1
  | _, LEmptyError -> 1
  | LInt _, _ -> -1
  | _, LInt _ -> 1
  | LRat _, _ -> -1
  | _, LRat _ -> 1
  | LMoney _, _ -> -1
  | _, LMoney _ -> 1
  | LUnit, _ -> -1
  | _, LUnit -> 1
  | LDate _, _ -> -1
  | _, LDate _ -> 1
  | LDuration _, _ -> .
  | _, LDuration _ -> .

let compare_location
    (type a)
    (x : a glocation Marked.pos)
    (y : a glocation Marked.pos) =
  match Marked.unmark x, Marked.unmark y with
  | DesugaredScopeVar (vx, None), DesugaredScopeVar (vy, None)
  | DesugaredScopeVar (vx, Some _), DesugaredScopeVar (vy, None)
  | DesugaredScopeVar (vx, None), DesugaredScopeVar (vy, Some _) ->
    ScopeVar.compare (Marked.unmark vx) (Marked.unmark vy)
  | DesugaredScopeVar ((x, _), Some sx), DesugaredScopeVar ((y, _), Some sy) ->
    let cmp = ScopeVar.compare x y in
    if cmp = 0 then StateName.compare sx sy else cmp
  | ScopelangScopeVar (vx, _), ScopelangScopeVar (vy, _) ->
    ScopeVar.compare vx vy
  | ( SubScopeVar (_, (xsubindex, _), (xsubvar, _)),
      SubScopeVar (_, (ysubindex, _), (ysubvar, _)) ) ->
    let c = SubScopeName.compare xsubindex ysubindex in
    if c = 0 then ScopeVar.compare xsubvar ysubvar else c
  | DesugaredScopeVar _, _ -> -1
  | _, DesugaredScopeVar _ -> 1
  | ScopelangScopeVar _, _ -> -1
  | _, ScopelangScopeVar _ -> 1
  | SubScopeVar _, _ -> .
  | _, SubScopeVar _ -> .

let equal_location a b = compare_location a b = 0

let equal_log_entries l1 l2 =
  match l1, l2 with
  | VarDef t1, VarDef t2 -> equal_typ (t1, Pos.no_pos) (t2, Pos.no_pos)
  | x, y -> x = y

let compare_log_entries l1 l2 =
  match l1, l2 with
  | VarDef t1, VarDef t2 -> compare_typ (t1, Pos.no_pos) (t2, Pos.no_pos)
  | BeginCall, BeginCall
  | EndCall, EndCall
  | PosRecordIfTrueBool, PosRecordIfTrueBool ->
    0
  | VarDef _, _ -> -1
  | _, VarDef _ -> 1
  | BeginCall, _ -> -1
  | _, BeginCall -> 1
  | EndCall, _ -> -1
  | _, EndCall -> 1
  | PosRecordIfTrueBool, _ -> .
  | _, PosRecordIfTrueBool -> .

(* let equal_op_kind = Stdlib.(=) *)

let compare_op_kind = Stdlib.compare

let equal_unops op1 op2 =
  match op1, op2 with
  (* Log entries contain a typ which contain position information, we thus need
     to descend into them *)
  | Log (l1, info1), Log (l2, info2) ->
    equal_log_entries l1 l2 && List.equal Uid.MarkedString.equal info1 info2
  | Log _, _ | _, Log _ -> false
  (* All the other cases can be discharged through equality *)
  | ( ( Not | Minus _ | Length | IntToRat | MoneyToRat | RatToMoney | GetDay
      | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth | RoundMoney
      | RoundDecimal ),
      _ ) ->
    op1 = op2

let compare_unops op1 op2 =
  match op1, op2 with
  | Not, Not -> 0
  | Minus k1, Minus k2 -> compare_op_kind k1 k2
  | Log (l1, info1), Log (l2, info2) -> (
    match compare_log_entries l1 l2 with
    | 0 -> List.compare Uid.MarkedString.compare info1 info2
    | n -> n)
  | Length, Length
  | IntToRat, IntToRat
  | MoneyToRat, MoneyToRat
  | RatToMoney, RatToMoney
  | GetDay, GetDay
  | GetMonth, GetMonth
  | GetYear, GetYear
  | FirstDayOfMonth, FirstDayOfMonth
  | LastDayOfMonth, LastDayOfMonth
  | RoundMoney, RoundMoney
  | RoundDecimal, RoundDecimal ->
    0
  | Not, _ -> -1
  | _, Not -> 1
  | Minus _, _ -> -1
  | _, Minus _ -> 1
  | Log _, _ -> -1
  | _, Log _ -> 1
  | Length, _ -> -1
  | _, Length -> 1
  | IntToRat, _ -> -1
  | _, IntToRat -> 1
  | MoneyToRat, _ -> -1
  | _, MoneyToRat -> 1
  | RatToMoney, _ -> -1
  | _, RatToMoney -> 1
  | GetDay, _ -> -1
  | _, GetDay -> 1
  | GetMonth, _ -> -1
  | _, GetMonth -> 1
  | GetYear, _ -> -1
  | _, GetYear -> 1
  | FirstDayOfMonth, _ -> -1
  | _, FirstDayOfMonth -> 1
  | LastDayOfMonth, _ -> -1
  | _, LastDayOfMonth -> 1
  | RoundMoney, _ -> -1
  | _, RoundMoney -> 1
  | RoundDecimal, _ -> .
  | _, RoundDecimal -> .

let equal_binop = Stdlib.( = )
let compare_binop = Stdlib.compare
let equal_ternop = Stdlib.( = )
let compare_ternop = Stdlib.compare

let equal_ops op1 op2 =
  match op1, op2 with
  | Ternop op1, Ternop op2 -> equal_ternop op1 op2
  | Binop op1, Binop op2 -> equal_binop op1 op2
  | Unop op1, Unop op2 -> equal_unops op1 op2
  | _, _ -> false

let compare_op op1 op2 =
  match op1, op2 with
  | Ternop op1, Ternop op2 -> compare_ternop op1 op2
  | Binop op1, Binop op2 -> compare_binop op1 op2
  | Unop op1, Unop op2 -> compare_unops op1 op2
  | Ternop _, _ -> -1
  | _, Ternop _ -> 1
  | Binop _, _ -> -1
  | _, Binop _ -> 1
  | Unop _, _ -> .
  | _, Unop _ -> .

let equal_except ex1 ex2 = ex1 = ex2
let compare_except ex1 ex2 = Stdlib.compare ex1 ex2

(* weird indentation; see
   https://github.com/ocaml-ppx/ocamlformat/issues/2143 *)
let rec equal_list : 'a. ('a, 't) gexpr list -> ('a, 't) gexpr list -> bool =
 fun es1 es2 ->
  try List.for_all2 equal es1 es2 with Invalid_argument _ -> false

and equal : type a. (a, 't) gexpr -> (a, 't) gexpr -> bool =
 fun e1 e2 ->
  match Marked.unmark e1, Marked.unmark e2 with
  | EVar v1, EVar v2 -> Bindlib.eq_vars v1 v2
  | ETuple (es1, n1), ETuple (es2, n2) -> n1 = n2 && equal_list es1 es2
  | ETupleAccess (e1, id1, n1, tys1), ETupleAccess (e2, id2, n2, tys2) ->
    equal e1 e2 && id1 = id2 && n1 = n2 && equal_typ_list tys1 tys2
  | EInj (e1, id1, n1, tys1), EInj (e2, id2, n2, tys2) ->
    equal e1 e2 && id1 = id2 && n1 = n2 && equal_typ_list tys1 tys2
  | EMatch (e1, cases1, n1), EMatch (e2, cases2, n2) ->
    n1 = n2 && equal e1 e2 && equal_list cases1 cases2
  | EArray es1, EArray es2 -> equal_list es1 es2
  | ELit l1, ELit l2 -> l1 = l2
  | EAbs (b1, tys1), EAbs (b2, tys2) ->
    equal_typ_list tys1 tys2
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
  | ELocation l1, ELocation l2 ->
    equal_location (Marked.mark Pos.no_pos l1) (Marked.mark Pos.no_pos l2)
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

let rec compare : type a. (a, _) gexpr -> (a, _) gexpr -> int =
 fun e1 e2 ->
  (* Infix operator to chain comparisons lexicographically. *)
  let ( @@< ) cmp1 cmpf = match cmp1 with 0 -> cmpf () | n -> n in
  (* OCamlformat doesn't know to keep consistency in match cases so disabled
     locally for readability *)
  match[@ocamlformat "disable"] Marked.unmark e1, Marked.unmark e2 with
  | ELit l1, ELit l2 ->
    compare_lit l1 l2
  | EApp (f1, args1), EApp (f2, args2) ->
    compare f1 f2 @@< fun () ->
    List.compare compare args1 args2
  | EOp op1, EOp op2 ->
    compare_op op1 op2
  | EArray a1, EArray a2 ->
    List.compare compare a1 a2
  | EVar v1, EVar v2 ->
    Bindlib.compare_vars v1 v2
  | EAbs (binder1, typs1), EAbs (binder2, typs2) ->
    List.compare compare_typ typs1 typs2 @@< fun () ->
    let _, e1, e2 = Bindlib.unmbind2 binder1 binder2 in
    compare e1 e2
  | EIfThenElse (i1, t1, e1), EIfThenElse (i2, t2, e2) ->
    compare i1 i2 @@< fun () ->
    compare t1 t2 @@< fun () ->
    compare e1 e2
  | ELocation l1, ELocation l2 ->
    compare_location (Marked.mark Pos.no_pos l1) (Marked.mark Pos.no_pos l2)
  | EStruct (name1, field_map1), EStruct (name2, field_map2) ->
    StructName.compare name1 name2 @@< fun () ->
    StructFieldMap.compare compare field_map1 field_map2
  | EStructAccess (e1, field_name1, struct_name1),
    EStructAccess (e2, field_name2, struct_name2) ->
    compare e1 e2 @@< fun () ->
    StructFieldName.compare field_name1 field_name2 @@< fun () ->
    StructName.compare struct_name1 struct_name2
  | EEnumInj (e1, cstr1, name1), EEnumInj (e2, cstr2, name2) ->
    compare e1 e2 @@< fun () ->
    EnumName.compare name1 name2 @@< fun () ->
    EnumConstructor.compare cstr1 cstr2
  | EMatchS (e1, name1, emap1), EMatchS (e2, name2, emap2) ->
    compare e1 e2 @@< fun () ->
    EnumName.compare name1 name2 @@< fun () ->
    EnumConstructorMap.compare compare emap1 emap2
  | ETuple (es1, s1), ETuple (es2, s2) ->
    Option.compare StructName.compare s1 s2 @@< fun () ->
    List.compare compare es1 es2
  | ETupleAccess (e1, n1, s1, tys1), ETupleAccess (e2, n2, s2, tys2) ->
    Option.compare StructName.compare s1 s2 @@< fun () ->
    Int.compare n1 n2 @@< fun () ->
    List.compare compare_typ tys1 tys2 @@< fun () ->
    compare e1 e2
  | EInj (e1, n1, name1, ts1), EInj (e2, n2, name2, ts2) ->
    EnumName.compare name1 name2 @@< fun () ->
    Int.compare n1 n2 @@< fun () ->
    List.compare compare_typ ts1 ts2 @@< fun () ->
    compare e1 e2
  | EMatch (e1, cases1, n1), EMatch (e2, cases2, n2) ->
    EnumName.compare n1 n2 @@< fun () ->
    compare e1 e2 @@< fun () ->
    List.compare compare cases1 cases2
  | EAssert e1, EAssert e2 ->
    compare e1 e2
  | EDefault (exs1, just1, cons1), EDefault (exs2, just2, cons2) ->
    compare just1 just2 @@< fun () ->
    compare cons1 cons2 @@< fun () ->
    List.compare compare exs1 exs2
  | ErrorOnEmpty e1, ErrorOnEmpty e2 ->
    compare e1 e2
  | ERaise ex1, ERaise ex2 ->
    compare_except ex1 ex2
  | ECatch (etry1, ex1, ewith1), ECatch (etry2, ex2, ewith2) ->
    compare_except ex1 ex2 @@< fun () ->
    compare etry1 etry2 @@< fun () ->
    compare ewith1 ewith2
  | ELit _, _ -> -1 | _, ELit _ -> 1
  | EApp _, _ -> -1 | _, EApp _ -> 1
  | EOp _, _ -> -1 | _, EOp _ -> 1
  | EArray _, _ -> -1 | _, EArray _ -> 1
  | EVar _, _ -> -1 | _, EVar _ -> 1
  | EAbs _, _ -> -1 | _, EAbs _ -> 1
  | EIfThenElse _, _ -> -1 | _, EIfThenElse _ -> 1
  | ELocation _, _ -> -1 | _, ELocation _ -> 1
  | EStruct _, _ -> -1 | _, EStruct _ -> 1
  | EStructAccess _, _ -> -1 | _, EStructAccess _ -> 1
  | EEnumInj _, _ -> -1 | _, EEnumInj _ -> 1
  | EMatchS _, _ -> -1 | _, EMatchS _ -> 1
  | ETuple _, _ -> -1 | _, ETuple _ -> 1
  | ETupleAccess _, _ -> -1 | _, ETupleAccess _ -> 1
  | EInj _, _ -> -1 | _, EInj _ -> 1
  | EMatch _, _ -> -1 | _, EMatch _ -> 1
  | EAssert _, _ -> -1 | _, EAssert _ -> 1
  | EDefault _, _ -> -1 | _, EDefault _ -> 1
  | ErrorOnEmpty _, _ -> . | _, ErrorOnEmpty _ -> .
  | ERaise _, _ -> -1 | _, ERaise _ -> 1
  | ECatch _, _ -> . | _, ECatch _ -> .

let rec free_vars : type a. (a, 't) gexpr -> (a, 't) gexpr Var.Set.t = function
  | EVar v, _ -> Var.Set.singleton v
  | EAbs (binder, _), _ ->
    let vs, body = Bindlib.unmbind binder in
    Array.fold_right Var.Set.remove vs (free_vars body)
  | e -> shallow_fold (fun e -> Var.Set.union (free_vars e)) e Var.Set.empty

let remove_logging_calls e =
  let rec f () e =
    match Marked.unmark e with
    | EApp ((EOp (Unop (Log _)), _), [arg]) -> map () ~f arg
    | _ -> map () ~f e
  in
  f () e

let format ?debug decl_ctx ppf e = Print.expr ?debug decl_ctx ppf e

let rec size : type a. (a, 't) gexpr -> int =
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

(* - Expression building helpers - *)

let make_var v mark = evar v mark

let make_abs xs e taus pos =
  let mark =
    map_mark
      (fun _ -> pos)
      (fun ety ->
        List.fold_right
          (fun tx acc -> Marked.mark pos (TArrow (tx, acc)))
          taus ety)
      (Marked.get_mark e)
  in
  eabs (bind xs e) taus mark

let make_app e u pos =
  let mark =
    fold_marks
      (fun _ -> pos)
      (function
        | [] -> assert false
        | fty :: argtys ->
          List.fold_left
            (fun tf tx ->
              match Marked.unmark tf with
              | TArrow (tx', tr) ->
                assert (unifiable tx.ty tx');
                (* wrong arg type *)
                tr
              | TAny -> tf
              | _ -> assert false)
            fty.ty argtys)
      (List.map Marked.get_mark (e :: u))
  in
  eapp e u mark

let empty_thunked_term mark =
  let silent = Var.make "_" in
  let pos = mark_pos mark in
  make_abs [| silent |]
    (Bindlib.box (ELit LEmptyError), mark)
    [TLit TUnit, pos]
    pos

let make_let_in x tau e1 e2 mpos =
  make_app (make_abs [| x |] e2 [tau] mpos) [e1] (pos e2)

let make_multiple_let_in xs taus e1s e2 mpos =
  make_app (make_abs xs e2 taus mpos) e1s (pos e2)

let make_default_unboxed exceptions just cons =
  let rec bool_value = function
    | ELit (LBool b), _ -> Some b
    | EApp ((EOp (Unop (Log (l, _))), _), [e]), _
      when l <> PosRecordIfTrueBool
           (* we don't remove the log calls corresponding to source code
              definitions !*) ->
      bool_value e
    | _ -> None
  in
  match exceptions, bool_value just, cons with
  | [], Some true, cons -> Marked.unmark cons
  | exceptions, Some true, (EDefault ([], just, cons), _) ->
    EDefault (exceptions, just, cons)
  | [except], Some false, _ -> Marked.unmark except
  | exceptions, _, cons -> EDefault (exceptions, just, cons)

let make_default exceptions just cons =
  Box.app2n just cons exceptions
  @@ fun just cons exceptions -> make_default_unboxed exceptions just cons

let make_tuple el structname m0 =
  match el with
  | [] ->
    etuple [] structname
      (with_ty m0
         (match structname with
         | Some n -> TStruct n, mark_pos m0
         | None -> TTuple [], mark_pos m0))
  | el ->
    let m =
      fold_marks
        (fun posl -> List.hd posl)
        (fun ml -> TTuple (List.map (fun t -> t.ty) ml), (List.hd ml).pos)
        (List.map (fun e -> Marked.get_mark e) el)
    in
    etuple el structname m
