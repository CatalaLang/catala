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
include Definitions.Op

let name : type a. a t -> string = function
  | Not -> "o_not"
  | Length -> "o_length"
  | GetDay -> "o_getDay"
  | GetMonth -> "o_getMonth"
  | GetYear -> "o_getYear"
  | FirstDayOfMonth -> "o_firstDayOfMonth"
  | LastDayOfMonth -> "o_lastDayOfMonth"
  | Log _ -> "o_log"
  | Minus -> "o_minus"
  | Minus_int -> "o_minus_int"
  | Minus_rat -> "o_minus_rat"
  | Minus_mon -> "o_minus_mon"
  | Minus_dur -> "o_minus_dur"
  | ToRat -> "o_torat"
  | ToRat_int -> "o_torat_int"
  | ToRat_mon -> "o_torat_mon"
  | ToMoney -> "o_tomoney"
  | ToMoney_rat -> "o_tomoney_rat"
  | Round -> "o_round"
  | Round_rat -> "o_round_rat"
  | Round_mon -> "o_round_mon"
  | And -> "o_and"
  | Or -> "o_or"
  | Xor -> "o_xor"
  | Eq -> "o_eq"
  | Map -> "o_map"
  | Concat -> "o_concat"
  | Filter -> "o_filter"
  | Reduce -> "o_reduce"
  | Add -> "o_add"
  | Add_int_int -> "o_add_int_int"
  | Add_rat_rat -> "o_add_rat_rat"
  | Add_mon_mon -> "o_add_mon_mon"
  | Add_dat_dur rm -> begin
    match rm with
    | RoundUp -> "o_add_dat_dur RoundUp"
    | RoundDown -> "o_add_dat_dur RoundDown"
    | AbortOnRound -> "o_add_dat_dur AbortOnRound"
  end
  | Add_dur_dur -> "o_add_dur_dur"
  | Sub -> "o_sub"
  | Sub_int_int -> "o_sub_int_int"
  | Sub_rat_rat -> "o_sub_rat_rat"
  | Sub_mon_mon -> "o_sub_mon_mon"
  | Sub_dat_dat -> "o_sub_dat_dat"
  | Sub_dat_dur -> "o_sub_dat_dur"
  | Sub_dur_dur -> "o_sub_dur_dur"
  | Mult -> "o_mult"
  | Mult_int_int -> "o_mult_int_int"
  | Mult_rat_rat -> "o_mult_rat_rat"
  | Mult_mon_rat -> "o_mult_mon_rat"
  | Mult_dur_int -> "o_mult_dur_int"
  | Div -> "o_div"
  | Div_int_int -> "o_div_int_int"
  | Div_rat_rat -> "o_div_rat_rat"
  | Div_mon_mon -> "o_div_mon_mon"
  | Div_mon_rat -> "o_div_mon_rat"
  | Div_dur_dur -> "o_div_dur_dur"
  | Lt -> "o_lt"
  | Lt_int_int -> "o_lt_int_int"
  | Lt_rat_rat -> "o_lt_rat_rat"
  | Lt_mon_mon -> "o_lt_mon_mon"
  | Lt_dur_dur -> "o_lt_dur_dur"
  | Lt_dat_dat -> "o_lt_dat_dat"
  | Lte -> "o_lte"
  | Lte_int_int -> "o_lte_int_int"
  | Lte_rat_rat -> "o_lte_rat_rat"
  | Lte_mon_mon -> "o_lte_mon_mon"
  | Lte_dur_dur -> "o_lte_dur_dur"
  | Lte_dat_dat -> "o_lte_dat_dat"
  | Gt -> "o_gt"
  | Gt_int_int -> "o_gt_int_int"
  | Gt_rat_rat -> "o_gt_rat_rat"
  | Gt_mon_mon -> "o_gt_mon_mon"
  | Gt_dur_dur -> "o_gt_dur_dur"
  | Gt_dat_dat -> "o_gt_dat_dat"
  | Gte -> "o_gte"
  | Gte_int_int -> "o_gte_int_int"
  | Gte_rat_rat -> "o_gte_rat_rat"
  | Gte_mon_mon -> "o_gte_mon_mon"
  | Gte_dur_dur -> "o_gte_dur_dur"
  | Gte_dat_dat -> "o_gte_dat_dat"
  | Eq_int_int -> "o_eq_int_int"
  | Eq_rat_rat -> "o_eq_rat_rat"
  | Eq_mon_mon -> "o_eq_mon_mon"
  | Eq_dur_dur -> "o_eq_dur_dur"
  | Eq_dat_dat -> "o_eq_dat_dat"
  | Fold -> "o_fold"
  | HandleDefault -> "o_handledefault"
  | HandleDefaultOpt -> "o_handledefaultopt"
  | ToClosureEnv -> "o_toclosureenv"
  | FromClosureEnv -> "o_fromclosureenv"

let compare_log_entries l1 l2 =
  match l1, l2 with
  | VarDef t1, VarDef t2 ->
    let tcompare =
      Type.compare (t1.log_typ, Pos.no_pos) (t2.log_typ, Pos.no_pos)
    in
    if tcompare = 0 then
      let ocompare = Bool.compare t1.log_io_output t2.log_io_output in
      if ocompare = 0 then
        match t1.log_io_input, t2.log_io_input with
        | NoInput, NoInput | OnlyInput, OnlyInput | Reentrant, Reentrant -> 0
        | NoInput, _ -> 1
        | _, NoInput -> -1
        | OnlyInput, _ -> 1
        | _, OnlyInput -> -1
      else ocompare
    else tcompare
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

let compare (type a1 a2) (t1 : a1 t) (t2 : a2 t) =
  match[@ocamlformat "disable"] t1, t2 with
  | Log (l1, info1), Log (l2, info2) -> (
    match compare_log_entries l1 l2 with
    | 0 -> List.compare Uid.MarkedString.compare info1 info2
    | n -> n)
  | Add_dat_dur l, Add_dat_dur r -> Stdlib.compare l r
  | Not, Not
  | Length, Length
  | GetDay, GetDay
  | GetMonth, GetMonth
  | GetYear, GetYear
  | FirstDayOfMonth, FirstDayOfMonth
  | LastDayOfMonth, LastDayOfMonth
  | Minus, Minus
  | Minus_int, Minus_int
  | Minus_rat, Minus_rat
  | Minus_mon, Minus_mon
  | Minus_dur, Minus_dur
  | ToRat, ToRat
  | ToRat_int, ToRat_int
  | ToRat_mon, ToRat_mon
  | ToMoney, ToMoney
  | ToMoney_rat, ToMoney_rat
  | Round, Round
  | Round_rat, Round_rat
  | Round_mon, Round_mon
  | And, And
  | Or, Or
  | Xor, Xor
  | Eq, Eq
  | Map, Map
  | Concat, Concat
  | Filter, Filter
  | Reduce, Reduce
  | Add, Add
  | Add_int_int, Add_int_int
  | Add_rat_rat, Add_rat_rat
  | Add_mon_mon, Add_mon_mon
  | Add_dur_dur, Add_dur_dur
  | Sub, Sub
  | Sub_int_int, Sub_int_int
  | Sub_rat_rat, Sub_rat_rat
  | Sub_mon_mon, Sub_mon_mon
  | Sub_dat_dat, Sub_dat_dat
  | Sub_dat_dur, Sub_dat_dur
  | Sub_dur_dur, Sub_dur_dur
  | Mult, Mult
  | Mult_int_int, Mult_int_int
  | Mult_rat_rat, Mult_rat_rat
  | Mult_mon_rat, Mult_mon_rat
  | Mult_dur_int, Mult_dur_int
  | Div, Div
  | Div_int_int, Div_int_int
  | Div_rat_rat, Div_rat_rat
  | Div_mon_mon, Div_mon_mon
  | Div_mon_rat, Div_mon_rat
  | Div_dur_dur, Div_dur_dur
  | Lt, Lt
  | Lt_int_int, Lt_int_int
  | Lt_rat_rat, Lt_rat_rat
  | Lt_mon_mon, Lt_mon_mon
  | Lt_dat_dat, Lt_dat_dat
  | Lt_dur_dur, Lt_dur_dur
  | Lte, Lte
  | Lte_int_int, Lte_int_int
  | Lte_rat_rat, Lte_rat_rat
  | Lte_mon_mon, Lte_mon_mon
  | Lte_dat_dat, Lte_dat_dat
  | Lte_dur_dur, Lte_dur_dur
  | Gt, Gt
  | Gt_int_int, Gt_int_int
  | Gt_rat_rat, Gt_rat_rat
  | Gt_mon_mon, Gt_mon_mon
  | Gt_dat_dat, Gt_dat_dat
  | Gt_dur_dur, Gt_dur_dur
  | Gte, Gte
  | Gte_int_int, Gte_int_int
  | Gte_rat_rat, Gte_rat_rat
  | Gte_mon_mon, Gte_mon_mon
  | Gte_dat_dat, Gte_dat_dat
  | Gte_dur_dur, Gte_dur_dur
  | Eq_int_int, Eq_int_int
  | Eq_rat_rat, Eq_rat_rat
  | Eq_mon_mon, Eq_mon_mon
  | Eq_dat_dat, Eq_dat_dat
  | Eq_dur_dur, Eq_dur_dur
  | Fold, Fold
  | HandleDefault, HandleDefault
  | HandleDefaultOpt, HandleDefaultOpt
  | FromClosureEnv, FromClosureEnv | ToClosureEnv, ToClosureEnv -> 0
  | Not, _ -> -1 | _, Not -> 1
  | Length, _ -> -1 | _, Length -> 1
  | GetDay, _ -> -1 | _, GetDay -> 1
  | GetMonth, _ -> -1 | _, GetMonth -> 1
  | GetYear, _ -> -1 | _, GetYear -> 1
  | FirstDayOfMonth, _ -> -1 | _, FirstDayOfMonth -> 1
  | LastDayOfMonth, _ -> -1 | _, LastDayOfMonth -> 1
  | Log _, _ -> -1 | _, Log _ -> 1
  | Minus, _ -> -1 | _, Minus -> 1
  | Minus_int, _ -> -1 | _, Minus_int -> 1
  | Minus_rat, _ -> -1 | _, Minus_rat -> 1
  | Minus_mon, _ -> -1 | _, Minus_mon -> 1
  | Minus_dur, _ -> -1 | _, Minus_dur -> 1
  | ToRat, _ -> -1 | _, ToRat -> 1
  | ToRat_int, _ -> -1 | _, ToRat_int -> 1
  | ToRat_mon, _ -> -1 | _, ToRat_mon -> 1
  | ToMoney, _ -> -1 | _, ToMoney -> 1
  | ToMoney_rat, _ -> -1 | _, ToMoney_rat -> 1
  | Round, _ -> -1 | _, Round -> 1
  | Round_rat, _ -> -1 | _, Round_rat -> 1
  | Round_mon, _ -> -1 | _, Round_mon -> 1
  | And, _ -> -1 | _, And -> 1
  | Or, _ -> -1 | _, Or -> 1
  | Xor, _ -> -1 | _, Xor -> 1
  | Eq, _ -> -1 | _, Eq -> 1
  | Map, _ -> -1 | _, Map -> 1
  | Concat, _ -> -1 | _, Concat -> 1
  | Filter, _ -> -1 | _, Filter -> 1
  | Reduce, _ -> -1 | _, Reduce -> 1
  | Add, _ -> -1 | _, Add -> 1
  | Add_int_int, _ -> -1 | _, Add_int_int -> 1
  | Add_rat_rat, _ -> -1 | _, Add_rat_rat -> 1
  | Add_mon_mon, _ -> -1 | _, Add_mon_mon -> 1
  | Add_dat_dur _, _ -> -1 | _, Add_dat_dur _ -> 1
  | Add_dur_dur, _ -> -1 | _, Add_dur_dur -> 1
  | Sub, _ -> -1 | _, Sub -> 1
  | Sub_int_int, _ -> -1 | _, Sub_int_int -> 1
  | Sub_rat_rat, _ -> -1 | _, Sub_rat_rat -> 1
  | Sub_mon_mon, _ -> -1 | _, Sub_mon_mon -> 1
  | Sub_dat_dat, _ -> -1 | _, Sub_dat_dat -> 1
  | Sub_dat_dur, _ -> -1 | _, Sub_dat_dur -> 1
  | Sub_dur_dur, _ -> -1 | _, Sub_dur_dur -> 1
  | Mult, _ -> -1 | _, Mult -> 1
  | Mult_int_int, _ -> -1 | _, Mult_int_int -> 1
  | Mult_rat_rat, _ -> -1 | _, Mult_rat_rat -> 1
  | Mult_mon_rat, _ -> -1 | _, Mult_mon_rat -> 1
  | Mult_dur_int, _ -> -1 | _, Mult_dur_int -> 1
  | Div, _ -> -1 | _, Div -> 1
  | Div_int_int, _ -> -1 | _, Div_int_int -> 1
  | Div_rat_rat, _ -> -1 | _, Div_rat_rat -> 1
  | Div_mon_mon, _ -> -1 | _, Div_mon_mon -> 1
  | Div_mon_rat, _ -> -1 | _, Div_mon_rat -> 1
  | Div_dur_dur, _ -> -1 | _, Div_dur_dur -> 1
  | Lt, _ -> -1 | _, Lt -> 1
  | Lt_int_int, _ -> -1 | _, Lt_int_int -> 1
  | Lt_rat_rat, _ -> -1 | _, Lt_rat_rat -> 1
  | Lt_mon_mon, _ -> -1 | _, Lt_mon_mon -> 1
  | Lt_dat_dat, _ -> -1 | _, Lt_dat_dat -> 1
  | Lt_dur_dur, _ -> -1 | _, Lt_dur_dur -> 1
  | Lte, _ -> -1 | _, Lte -> 1
  | Lte_int_int, _ -> -1 | _, Lte_int_int -> 1
  | Lte_rat_rat, _ -> -1 | _, Lte_rat_rat -> 1
  | Lte_mon_mon, _ -> -1 | _, Lte_mon_mon -> 1
  | Lte_dat_dat, _ -> -1 | _, Lte_dat_dat -> 1
  | Lte_dur_dur, _ -> -1 | _, Lte_dur_dur -> 1
  | Gt, _ -> -1 | _, Gt -> 1
  | Gt_int_int, _ -> -1 | _, Gt_int_int -> 1
  | Gt_rat_rat, _ -> -1 | _, Gt_rat_rat -> 1
  | Gt_mon_mon, _ -> -1 | _, Gt_mon_mon -> 1
  | Gt_dat_dat, _ -> -1 | _, Gt_dat_dat -> 1
  | Gt_dur_dur, _ -> -1 | _, Gt_dur_dur -> 1
  | Gte, _ -> -1 | _, Gte -> 1
  | Gte_int_int, _ -> -1 | _, Gte_int_int -> 1
  | Gte_rat_rat, _ -> -1 | _, Gte_rat_rat -> 1
  | Gte_mon_mon, _ -> -1 | _, Gte_mon_mon -> 1
  | Gte_dat_dat, _ -> -1 | _, Gte_dat_dat -> 1
  | Gte_dur_dur, _ -> -1 | _, Gte_dur_dur -> 1
  | Eq_int_int, _ -> -1 | _, Eq_int_int -> 1
  | Eq_rat_rat, _ -> -1 | _, Eq_rat_rat -> 1
  | Eq_mon_mon, _ -> -1 | _, Eq_mon_mon -> 1
  | Eq_dat_dat, _ -> -1 | _, Eq_dat_dat -> 1
  | Eq_dur_dur, _ -> -1 | _, Eq_dur_dur -> 1
  | HandleDefault, _ -> -1 | _, HandleDefault -> 1
  | HandleDefaultOpt, _ -> -1 | _, HandleDefaultOpt -> 1
  | FromClosureEnv, _ -> -1 | _, FromClosureEnv -> 1
  | ToClosureEnv, _ -> -1 | _, ToClosureEnv -> 1
  | Fold, _  | _, Fold -> .

let equal t1 t2 = compare t1 t2 = 0

(* Classification of operators *)

let kind_dispatch :
    type a.
    polymorphic:(< polymorphic : yes ; .. > t -> 'b) ->
    monomorphic:(< monomorphic : yes ; .. > t -> 'b) ->
    ?overloaded:(< overloaded : yes ; .. > t -> 'b) ->
    ?resolved:(< resolved : yes ; .. > t -> 'b) ->
    a t ->
    'b =
 fun ~polymorphic ~monomorphic ?(overloaded = fun _ -> assert false)
     ?(resolved = fun _ -> assert false) op ->
  match op with
  | ( Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth | And
    | Or | Xor ) as op ->
    monomorphic op
  | ( Log _ | Length | Eq | Map | Concat | Filter | Reduce | Fold
    | HandleDefault | HandleDefaultOpt | FromClosureEnv | ToClosureEnv ) as op
    ->
    polymorphic op
  | ( Minus | ToRat | ToMoney | Round | Add | Sub | Mult | Div | Lt | Lte | Gt
    | Gte ) as op ->
    overloaded op
  | ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToRat_int | ToRat_mon
    | ToMoney_rat | Round_rat | Round_mon | Add_int_int | Add_rat_rat
    | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Sub_int_int | Sub_rat_rat
    | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur | Sub_dur_dur | Mult_int_int
    | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Div_int_int | Div_rat_rat
    | Div_mon_mon | Div_mon_rat | Div_dur_dur | Lt_int_int | Lt_rat_rat
    | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur | Lte_int_int | Lte_rat_rat
    | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur | Gt_int_int | Gt_rat_rat
    | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur | Gte_int_int | Gte_rat_rat
    | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur | Eq_int_int | Eq_rat_rat
    | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur ) as op ->
    resolved op

type 'a no_overloads =
  < overloaded : no
  ; monomorphic : yes
  ; polymorphic : yes
  ; resolved : yes
  ; .. >
  as
  'a

let translate (t : 'a no_overloads t) : 'b no_overloads t =
  match t with
  | ( Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth | And
    | Or | Xor | HandleDefault | HandleDefaultOpt | Log _ | Length | Eq | Map
    | Concat | Filter | Reduce | Fold | Minus_int | Minus_rat | Minus_mon
    | Minus_dur | ToRat_int | ToRat_mon | ToMoney_rat | Round_rat | Round_mon
    | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur
    | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
    | Sub_dur_dur | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int
    | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur
    | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur
    | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur
    | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur
    | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur
    | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur
    | FromClosureEnv | ToClosureEnv ) as op ->
    op

let monomorphic_type ((op : monomorphic t), pos) =
  let args, ret =
    match op with
    | Not -> [TBool], TBool
    | GetDay -> [TDate], TInt
    | GetMonth -> [TDate], TInt
    | GetYear -> [TDate], TInt
    | FirstDayOfMonth -> [TDate], TDate
    | LastDayOfMonth -> [TDate], TDate
    | And -> [TBool; TBool], TBool
    | Or -> [TBool; TBool], TBool
    | Xor -> [TBool; TBool], TBool
  in
  TArrow (List.map (fun tau -> TLit tau, pos) args, (TLit ret, pos)), pos

(** Rules for overloads definitions:

    - the concrete operator, including its return type, is uniquely determined
      by the type of the operands

    - no resolved version of an operator should be the redefinition of another
      one with an added conversion. For example, [int + rat -> rat] is not
      acceptable (that would amount to implicit casts).

    These two points can be generalised for binary operators as: when
    considering an operator with type ['a -> 'b -> 'c], for any given two among
    ['a], ['b] and ['c], there should be a unique solution for the third. *)

let resolved_type ((op : resolved t), pos) =
  let args, ret =
    match op with
    | Minus_int -> [TInt], TInt
    | Minus_rat -> [TRat], TRat
    | Minus_mon -> [TMoney], TMoney
    | Minus_dur -> [TDuration], TDuration
    | ToRat_int -> [TInt], TRat
    | ToRat_mon -> [TMoney], TRat
    | ToMoney_rat -> [TRat], TMoney
    | Round_rat -> [TRat], TRat
    | Round_mon -> [TMoney], TMoney
    | Add_int_int -> [TInt; TInt], TInt
    | Add_rat_rat -> [TRat; TRat], TRat
    | Add_mon_mon -> [TMoney; TMoney], TMoney
    | Add_dat_dur _ -> [TDate; TDuration], TDate
    | Add_dur_dur -> [TDuration; TDuration], TDuration
    | Sub_int_int -> [TInt; TInt], TInt
    | Sub_rat_rat -> [TRat; TRat], TRat
    | Sub_mon_mon -> [TMoney; TMoney], TMoney
    | Sub_dat_dat -> [TDate; TDate], TDuration
    | Sub_dat_dur -> [TDate; TDuration], TDuration
    | Sub_dur_dur -> [TDuration; TDuration], TDuration
    | Mult_int_int -> [TInt; TInt], TInt
    | Mult_rat_rat -> [TRat; TRat], TRat
    | Mult_mon_rat -> [TMoney; TRat], TMoney
    | Mult_dur_int -> [TDuration; TInt], TDuration
    | Div_int_int -> [TInt; TInt], TRat
    | Div_rat_rat -> [TRat; TRat], TRat
    | Div_mon_mon -> [TMoney; TMoney], TRat
    | Div_mon_rat -> [TMoney; TRat], TMoney
    | Div_dur_dur -> [TDuration; TDuration], TRat
    | Lt_int_int -> [TInt; TInt], TBool
    | Lt_rat_rat -> [TRat; TRat], TBool
    | Lt_mon_mon -> [TMoney; TMoney], TBool
    | Lt_dat_dat -> [TDate; TDate], TBool
    | Lt_dur_dur -> [TDuration; TDuration], TBool
    | Lte_int_int -> [TInt; TInt], TBool
    | Lte_rat_rat -> [TRat; TRat], TBool
    | Lte_mon_mon -> [TMoney; TMoney], TBool
    | Lte_dat_dat -> [TDate; TDate], TBool
    | Lte_dur_dur -> [TDuration; TDuration], TBool
    | Gt_int_int -> [TInt; TInt], TBool
    | Gt_rat_rat -> [TRat; TRat], TBool
    | Gt_mon_mon -> [TMoney; TMoney], TBool
    | Gt_dat_dat -> [TDate; TDate], TBool
    | Gt_dur_dur -> [TDuration; TDuration], TBool
    | Gte_int_int -> [TInt; TInt], TBool
    | Gte_rat_rat -> [TRat; TRat], TBool
    | Gte_mon_mon -> [TMoney; TMoney], TBool
    | Gte_dat_dat -> [TDate; TDate], TBool
    | Gte_dur_dur -> [TDuration; TDuration], TBool
    | Eq_int_int -> [TInt; TInt], TBool
    | Eq_rat_rat -> [TRat; TRat], TBool
    | Eq_mon_mon -> [TMoney; TMoney], TBool
    | Eq_dat_dat -> [TDate; TDate], TBool
    | Eq_dur_dur -> [TDuration; TDuration], TBool
  in
  TArrow (List.map (fun tau -> TLit tau, pos) args, (TLit ret, pos)), pos

let resolve_overload_aux (op : overloaded t) (operands : typ_lit list) :
    < resolved : yes ; .. > t * [ `Straight | `Reversed ] =
  match op, operands with
  | Minus, [TInt] -> Minus_int, `Straight
  | Minus, [TRat] -> Minus_rat, `Straight
  | Minus, [TMoney] -> Minus_mon, `Straight
  | Minus, [TDuration] -> Minus_dur, `Straight
  | ToRat, [TInt] -> ToRat_int, `Straight
  | ToRat, [TMoney] -> ToRat_mon, `Straight
  | ToMoney, [TRat] -> ToMoney_rat, `Straight
  | Round, [TRat] -> Round_rat, `Straight
  | Round, [TMoney] -> Round_mon, `Straight
  | Add, [TInt; TInt] -> Add_int_int, `Straight
  | Add, [TRat; TRat] -> Add_rat_rat, `Straight
  | Add, [TMoney; TMoney] -> Add_mon_mon, `Straight
  | Add, [TDuration; TDuration] -> Add_dur_dur, `Straight
  | Add, [TDate; TDuration] -> Add_dat_dur AbortOnRound, `Straight
  | Add, [TDuration; TDate] -> Add_dat_dur AbortOnRound, `Reversed
  | Sub, [TInt; TInt] -> Sub_int_int, `Straight
  | Sub, [TRat; TRat] -> Sub_rat_rat, `Straight
  | Sub, [TMoney; TMoney] -> Sub_mon_mon, `Straight
  | Sub, [TDuration; TDuration] -> Sub_dur_dur, `Straight
  | Sub, [TDate; TDate] -> Sub_dat_dat, `Straight
  | Sub, [TDate; TDuration] -> Sub_dat_dur, `Straight
  | Mult, [TInt; TInt] -> Mult_int_int, `Straight
  | Mult, [TRat; TRat] -> Mult_rat_rat, `Straight
  | Mult, [TMoney; TRat] -> Mult_mon_rat, `Straight
  | Mult, [TRat; TMoney] -> Mult_mon_rat, `Reversed
  | Mult, [TDuration; TInt] -> Mult_dur_int, `Straight
  | Mult, [TInt; TDuration] -> Mult_dur_int, `Reversed
  | Div, [TInt; TInt] -> Div_int_int, `Straight
  | Div, [TRat; TRat] -> Div_rat_rat, `Straight
  | Div, [TMoney; TMoney] -> Div_mon_mon, `Straight
  | Div, [TMoney; TRat] -> Div_mon_rat, `Straight
  | Div, [TDuration; TDuration] -> Div_dur_dur, `Straight
  | Lt, [TInt; TInt] -> Lt_int_int, `Straight
  | Lt, [TRat; TRat] -> Lt_rat_rat, `Straight
  | Lt, [TMoney; TMoney] -> Lt_mon_mon, `Straight
  | Lt, [TDuration; TDuration] -> Lt_dur_dur, `Straight
  | Lt, [TDate; TDate] -> Lt_dat_dat, `Straight
  | Lte, [TInt; TInt] -> Lte_int_int, `Straight
  | Lte, [TRat; TRat] -> Lte_rat_rat, `Straight
  | Lte, [TMoney; TMoney] -> Lte_mon_mon, `Straight
  | Lte, [TDuration; TDuration] -> Lte_dur_dur, `Straight
  | Lte, [TDate; TDate] -> Lte_dat_dat, `Straight
  | Gt, [TInt; TInt] -> Gt_int_int, `Straight
  | Gt, [TRat; TRat] -> Gt_rat_rat, `Straight
  | Gt, [TMoney; TMoney] -> Gt_mon_mon, `Straight
  | Gt, [TDuration; TDuration] -> Gt_dur_dur, `Straight
  | Gt, [TDate; TDate] -> Gt_dat_dat, `Straight
  | Gte, [TInt; TInt] -> Gte_int_int, `Straight
  | Gte, [TRat; TRat] -> Gte_rat_rat, `Straight
  | Gte, [TMoney; TMoney] -> Gte_mon_mon, `Straight
  | Gte, [TDuration; TDuration] -> Gte_dur_dur, `Straight
  | Gte, [TDate; TDate] -> Gte_dat_dat, `Straight
  | ( ( Minus | ToRat | ToMoney | Round | Add | Sub | Mult | Div | Lt | Lte | Gt
      | Gte ),
      _ ) ->
    raise Not_found

let resolve_overload ctx (op : overloaded t Mark.pos) (operands : typ list) :
    < resolved : yes ; .. > t * [ `Straight | `Reversed ] =
  try
    let operands =
      List.map
        (fun t ->
          match Mark.remove t with TLit tl -> tl | _ -> raise Not_found)
        operands
    in
    resolve_overload_aux (Mark.remove op) operands
  with Not_found ->
    Message.raise_multispanned_error
      ((None, Mark.get op)
      :: List.map
           (fun ty ->
             ( Some
                 (Format.asprintf "Type %a coming from expression:"
                    (Print.typ ctx) ty),
               Mark.get ty ))
           operands)
      "I don't know how to apply operator %a on types %a"
      (Print.operator ~debug:true)
      (Mark.remove op)
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and@ ")
         (Print.typ ctx))
      operands

let overload_type ctx (op : overloaded t Mark.pos) (operands : typ list) : typ =
  let rop = fst (resolve_overload ctx op operands) in
  resolved_type (Mark.copy op rop)
