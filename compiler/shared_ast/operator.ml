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
  | Log _ -> "o_log"
  | Minus -> "o_minus"
  | Minus_int -> "o_minus_int"
  | Minus_rat -> "o_minus_rat"
  | Minus_mon -> "o_minus_mon"
  | Minus_dur -> "o_minus_dur"
  | ToInt -> "o_toint"
  | ToInt_rat -> "o_toint_rat"
  | ToInt_mon -> "o_toint_mon"
  | ToRat -> "o_torat"
  | ToRat_int -> "o_torat_int"
  | ToRat_mon -> "o_torat_mon"
  | ToMoney -> "o_tomoney"
  | ToMoney_int -> "o_tomoney_int"
  | ToMoney_rat -> "o_tomoney_rat"
  | Round -> "o_round"
  | Round_rat -> "o_round_rat"
  | Round_mon -> "o_round_mon"
  | And -> "o_and"
  | Or -> "o_or"
  | Xor -> "o_xor"
  | Eq -> "o_eq"
  | Concat -> "o_concat"
  | Map -> "o_map"
  | Filter -> "o_filter"
  | Find -> "o_find"
  | Reduce -> "o_reduce"
  | Sort `Asc -> "o_sort_asc"
  | Sort `Desc -> "o_sort_desc"
  | Map2 -> "o_map2"
  | Fold -> "o_fold"
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
  | Sub_dat_dur rm -> begin
    match rm with
    | RoundUp -> "o_sub_dat_dur RoundUp"
    | RoundDown -> "o_sub_dat_dur RoundDown"
    | AbortOnRound -> "o_sub_dat_dur AbortOnRound"
  end
  | Sub_dur_dur -> "o_sub_dur_dur"
  | Mult -> "o_mult"
  | Mult_int_int -> "o_mult_int_int"
  | Mult_rat_rat -> "o_mult_rat_rat"
  | Mult_mon_int -> "o_mult_mon_int"
  | Mult_mon_rat -> "o_mult_mon_rat"
  | Mult_dur_int -> "o_mult_dur_int"
  | Div -> "o_div"
  | Div_int_int -> "o_div_int_int"
  | Div_rat_rat -> "o_div_rat_rat"
  | Div_mon_mon -> "o_div_mon_mon"
  | Div_mon_int -> "o_div_mon_int"
  | Div_mon_rat -> "o_div_mon_rat"
  | Div_dur_dur -> "o_div_dur_dur"
  | Lt -> "o_lt"
  | Lte -> "o_lte"
  | Gt -> "o_gt"
  | Gte -> "o_gte"
  | HandleExceptions -> "handle_exceptions"
  | ToClosureEnv -> "o_toclosureenv"
  | FromClosureEnv -> "o_fromclosureenv"
  | ArrayAccess n -> Printf.sprintf "o_array_nth(%d)" n
  | ConstructorCheck (e, c) ->
    Printf.sprintf "o_is(%s.%s)" (EnumName.to_string e)
      (EnumConstructor.to_string c)
  | ValueFromJson (_ty, str) -> Printf.sprintf "from_json(%s)" str

let compare_log_entries l1 l2 =
  match l1, l2 with
  | VarDef t1, VarDef t2 ->
    let tcompare = Type.compare (t1.log_typ, Pos.void) (t2.log_typ, Pos.void) in
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
  | Sub_dat_dur l, Sub_dat_dur r -> Stdlib.compare l r
  | Not, Not
  | Length, Length
  | Minus, Minus
  | Minus_int, Minus_int
  | Minus_rat, Minus_rat
  | Minus_mon, Minus_mon
  | Minus_dur, Minus_dur
  | ToInt, ToInt
  | ToInt_rat, ToInt_rat
  | ToInt_mon, ToInt_mon
  | ToRat, ToRat
  | ToRat_int, ToRat_int
  | ToRat_mon, ToRat_mon
  | ToMoney, ToMoney
  | ToMoney_int, ToMoney_int
  | ToMoney_rat, ToMoney_rat
  | Round, Round
  | Round_rat, Round_rat
  | Round_mon, Round_mon
  | And, And
  | Or, Or
  | Xor, Xor
  | Eq, Eq
  | Concat, Concat
  | Map, Map
  | Filter, Filter
  | Find, Find
  | Reduce, Reduce
  | Map2, Map2
  | Fold, Fold
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
  | Sub_dur_dur, Sub_dur_dur
  | Mult, Mult
  | Mult_int_int, Mult_int_int
  | Mult_rat_rat, Mult_rat_rat
  | Mult_mon_int, Mult_mon_int
  | Mult_mon_rat, Mult_mon_rat
  | Mult_dur_int, Mult_dur_int
  | Div, Div
  | Div_int_int, Div_int_int
  | Div_rat_rat, Div_rat_rat
  | Div_mon_mon, Div_mon_mon
  | Div_mon_int, Div_mon_int
  | Div_mon_rat, Div_mon_rat
  | Div_dur_dur, Div_dur_dur
  | Lt, Lt
  | Lte, Lte
  | Gt, Gt
  | Gte, Gte
  | HandleExceptions, HandleExceptions
  | FromClosureEnv, FromClosureEnv | ToClosureEnv, ToClosureEnv -> 0
  | ArrayAccess n1, ArrayAccess n2 -> compare n1 n2
  | ConstructorCheck (_, c1), ConstructorCheck (_, c2) -> EnumConstructor.compare c1 c2
  | ValueFromJson (ty1, j1), ValueFromJson (ty2, j2) ->
    (match Type.compare ty1 ty2 with
     | 0 -> String.compare j1 j2
     | n -> n)
  | Sort o1, Sort o2 -> (match o1, o2 with `Asc, `Asc | `Desc, `Desc -> 0 | `Desc, `Asc -> -1 | `Asc, `Desc -> 1)
  | Not, _ -> -1 | _, Not -> 1
  | Length, _ -> -1 | _, Length -> 1
  | Log _, _ -> -1 | _, Log _ -> 1
  | Minus, _ -> -1 | _, Minus -> 1
  | Minus_int, _ -> -1 | _, Minus_int -> 1
  | Minus_rat, _ -> -1 | _, Minus_rat -> 1
  | Minus_mon, _ -> -1 | _, Minus_mon -> 1
  | Minus_dur, _ -> -1 | _, Minus_dur -> 1
  | ToInt, _ -> -1 | _, ToInt -> 1
  | ToInt_rat, _ -> -1 | _, ToInt_rat -> 1
  | ToInt_mon,_ -> -1 | _, ToInt_mon -> 1
  | ToRat, _ -> -1 | _, ToRat -> 1
  | ToRat_int, _ -> -1 | _, ToRat_int -> 1
  | ToRat_mon, _ -> -1 | _, ToRat_mon -> 1
  | ToMoney, _ -> -1 | _, ToMoney -> 1
  | ToMoney_int, _ -> -1 | _, ToMoney_int -> 1
  | ToMoney_rat, _ -> -1 | _, ToMoney_rat -> 1
  | Round, _ -> -1 | _, Round -> 1
  | Round_rat, _ -> -1 | _, Round_rat -> 1
  | Round_mon, _ -> -1 | _, Round_mon -> 1
  | And, _ -> -1 | _, And -> 1
  | Or, _ -> -1 | _, Or -> 1
  | Xor, _ -> -1 | _, Xor -> 1
  | Eq, _ -> -1 | _, Eq -> 1
  | Concat, _ -> -1 | _, Concat -> 1
  | Map, _ -> -1 | _, Map -> 1
  | Filter, _ -> -1 | _, Filter -> 1
  | Find, _ -> -1 | _, Find -> 1
  | Reduce, _ -> -1 | _, Reduce -> 1
  | Sort _, _ -> -1 | _, Sort _ -> 1
  | Map2, _ -> -1 | _, Map2 -> 1
  | Fold, _ -> -1 | _, Fold -> 1
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
  | Sub_dat_dur _, _ -> -1 | _, Sub_dat_dur _ -> 1
  | Sub_dur_dur, _ -> -1 | _, Sub_dur_dur -> 1
  | Mult, _ -> -1 | _, Mult -> 1
  | Mult_int_int, _ -> -1 | _, Mult_int_int -> 1
  | Mult_rat_rat, _ -> -1 | _, Mult_rat_rat -> 1
  | Mult_mon_int, _ -> -1 | _, Mult_mon_int -> 1
  | Mult_mon_rat, _ -> -1 | _, Mult_mon_rat -> 1
  | Mult_dur_int, _ -> -1 | _, Mult_dur_int -> 1
  | Div, _ -> -1 | _, Div -> 1
  | Div_int_int, _ -> -1 | _, Div_int_int -> 1
  | Div_rat_rat, _ -> -1 | _, Div_rat_rat -> 1
  | Div_mon_mon, _ -> -1 | _, Div_mon_mon -> 1
  | Div_mon_int, _ -> -1 | _, Div_mon_int -> 1
  | Div_mon_rat, _ -> -1 | _, Div_mon_rat -> 1
  | Div_dur_dur, _ -> -1 | _, Div_dur_dur -> 1
  | Lt, _ -> -1 | _, Lt -> 1
  | Lte, _ -> -1 | _, Lte -> 1
  | Gt, _ -> -1 | _, Gt -> 1
  | Gte, _ -> -1 | _, Gte -> 1
  | HandleExceptions, _ -> -1 | _, HandleExceptions -> 1
  | FromClosureEnv, _ -> -1 | _, FromClosureEnv -> 1
  | ToClosureEnv, _ -> -1 | _, ToClosureEnv -> 1
  | ArrayAccess _, _ -> -1 | _, ArrayAccess _ -> 1
  | ConstructorCheck _, _ -> -1 | _, ConstructorCheck _  -> 1
  | ValueFromJson _, _ | _, ValueFromJson _ -> .

let equal t1 t2 = compare t1 t2 = 0

(* Classification of operators *)

let kind_dispatch : type a.
    polymorphic:(< polymorphic : yes ; .. > t Mark.pos -> 'b) ->
    monomorphic:(< monomorphic : yes ; .. > t Mark.pos -> 'b) ->
    ?overloaded:(< overloaded : yes ; .. > t Mark.pos -> 'b) ->
    ?resolved:(< resolved : yes ; .. > t Mark.pos -> 'b) ->
    a t Mark.pos ->
    'b =
 fun ~polymorphic ~monomorphic ?(overloaded = fun _ -> assert false)
     ?(resolved = fun _ -> assert false) op ->
  match op with
  | ((Not | And | Or | Xor | ValueFromJson _), _) as op -> monomorphic op
  | ( ( Log _ | Length | Eq | Concat | Map | Filter | Find | Reduce | Sort _
      | Map2 | Fold | Lt | Lte | Gt | Gte | HandleExceptions | FromClosureEnv
      | ToClosureEnv | ArrayAccess _ | ConstructorCheck _ ),
      _ ) as op ->
    polymorphic op
  | ((Minus | ToInt | ToRat | ToMoney | Round | Add | Sub | Mult | Div), _) as
    op ->
    overloaded op
  | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToInt_rat | ToInt_mon
      | ToRat_int | ToRat_mon | ToMoney_rat | ToMoney_int | Round_rat
      | Round_mon | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
      | Add_dur_dur | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat
      | Sub_dat_dur _ | Sub_dur_dur | Mult_int_int | Mult_rat_rat | Mult_mon_int
      | Mult_mon_rat | Mult_dur_int | Div_int_int | Div_rat_rat | Div_mon_mon
      | Div_mon_int | Div_mon_rat | Div_dur_dur ),
      _ ) as op ->
    resolved op

type 'a no_overloads =
  < overloaded : no
  ; monomorphic : yes
  ; polymorphic : yes
  ; resolved : yes
  ; .. >
  as
  'a

let translate (t : 'a no_overloads t Mark.pos) : 'b no_overloads t Mark.pos =
  match t with
  | ( ( Not | And | Or | Xor | HandleExceptions | Log _ | Length | Eq | Lt | Gt
      | Lte | Gte | Concat | Map | Filter | Find | Reduce | Sort _ | Map2 | Fold
      | Minus_int | Minus_rat | Minus_mon | Minus_dur | ToInt_rat | ToInt_mon
      | ToRat_int | ToRat_mon | ToMoney_rat | ToMoney_int | Round_rat
      | Round_mon | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
      | Add_dur_dur | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat
      | Sub_dat_dur _ | Sub_dur_dur | Mult_int_int | Mult_rat_rat | Mult_mon_int
      | Mult_mon_rat | Mult_dur_int | Div_int_int | Div_rat_rat | Div_mon_mon
      | Div_mon_int | Div_mon_rat | Div_dur_dur | FromClosureEnv | ToClosureEnv
      | ArrayAccess _ | ConstructorCheck _ | ValueFromJson _ ),
      _ ) as op ->
    op

let monomorphic_type ((op : monomorphic t), pos) =
  let args, ret =
    match op with
    | Not -> [TBool], TLit TBool
    | And -> [TBool; TBool], TLit TBool
    | Or -> [TBool; TBool], TLit TBool
    | Xor -> [TBool; TBool], TLit TBool
    | ValueFromJson (ty, _) -> [TUnit], Mark.remove ty
  in
  TArrow (List.map (fun tau -> TLit tau, pos) args, (ret, pos)), pos

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
    | ToInt_rat -> [TRat], TInt
    | ToInt_mon -> [TMoney], TInt
    | ToRat_int -> [TInt], TRat
    | ToRat_mon -> [TMoney], TRat
    | ToMoney_rat -> [TRat], TMoney
    | ToMoney_int -> [TInt], TMoney
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
    | Sub_dat_dur _ -> [TDate; TDuration], TDate
    | Sub_dur_dur -> [TDuration; TDuration], TDuration
    | Mult_int_int -> [TInt; TInt], TInt
    | Mult_rat_rat -> [TRat; TRat], TRat
    | Mult_mon_int -> [TMoney; TInt], TMoney
    | Mult_mon_rat -> [TMoney; TRat], TMoney
    | Mult_dur_int -> [TDuration; TInt], TDuration
    | Div_int_int -> [TInt; TInt], TRat
    | Div_rat_rat -> [TRat; TRat], TRat
    | Div_mon_mon -> [TMoney; TMoney], TRat
    | Div_mon_int -> [TMoney; TInt], TMoney
    | Div_mon_rat -> [TMoney; TRat], TMoney
    | Div_dur_dur -> [TDuration; TDuration], TRat
  in
  TArrow (List.map (fun tau -> TLit tau, pos) args, (TLit ret, pos)), pos

let resolve_overload_aux (op : overloaded t) (operands : typ_lit list) :
    < resolved : yes ; .. > t * [ `Straight | `Reversed ] =
  match op, operands with
  | Minus, [TInt] -> Minus_int, `Straight
  | Minus, [TRat] -> Minus_rat, `Straight
  | Minus, [TMoney] -> Minus_mon, `Straight
  | Minus, [TDuration] -> Minus_dur, `Straight
  | ToInt, [TRat] -> ToInt_rat, `Straight
  | ToInt, [TMoney] -> ToInt_mon, `Straight
  | ToRat, [TInt] -> ToRat_int, `Straight
  | ToRat, [TMoney] -> ToRat_mon, `Straight
  | ToMoney, [TRat] -> ToMoney_rat, `Straight
  | ToMoney, [TInt] -> ToMoney_int, `Straight
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
  | Sub, [TDate; TDuration] -> Sub_dat_dur AbortOnRound, `Straight
  | Mult, [TInt; TInt] -> Mult_int_int, `Straight
  | Mult, [TRat; TRat] -> Mult_rat_rat, `Straight
  | Mult, [TMoney; TInt] -> Mult_mon_int, `Straight
  | Mult, [TInt; TMoney] -> Mult_mon_int, `Reversed
  | Mult, [TMoney; TRat] -> Mult_mon_rat, `Straight
  | Mult, [TRat; TMoney] -> Mult_mon_rat, `Reversed
  | Mult, [TDuration; TInt] -> Mult_dur_int, `Straight
  | Mult, [TInt; TDuration] -> Mult_dur_int, `Reversed
  | Div, [TInt; TInt] -> Div_int_int, `Straight
  | Div, [TRat; TRat] -> Div_rat_rat, `Straight
  | Div, [TMoney; TMoney] -> Div_mon_mon, `Straight
  | Div, [TMoney; TInt] -> Div_mon_int, `Straight
  | Div, [TMoney; TRat] -> Div_mon_rat, `Straight
  | Div, [TDuration; TDuration] -> Div_dur_dur, `Straight
  | (Minus | ToInt | ToRat | ToMoney | Round | Add | Sub | Mult | Div), _ ->
    raise Not_found

let resolve_overload ((op, pos) : overloaded t Mark.pos) (operands : typ list) :
    < resolved : yes ; .. > t Mark.pos * [ `Straight | `Reversed ] =
  try
    let operands =
      List.map
        (fun t ->
          match Mark.remove t with TLit tl -> tl | _ -> raise Not_found)
        operands
    in
    let op, direction = resolve_overload_aux op operands in
    (op, pos), direction
  with Not_found -> (
    let poly_ops =
      List.filter (function TVar _, _ -> true | _ -> false) operands
    in
    match poly_ops with
    | poly_op :: _ ->
      Message.error ~pos
        ~fmt_pos:
          [
            ( (fun ppf ->
                Format.fprintf ppf
                  "Undetermined type@ %a@ coming@ from@ expression:" Print.typ
                  poly_op),
              Mark.get poly_op );
          ]
        "In this application of operator %a,@ the@ type@ of@ an@ operand@ is@ \
         unknown"
        (Print.operator ~debug:true)
        op
    | [] ->
      Message.error ~main_pos:pos ~pos
        ~fmt_pos:
          (List.map
             (fun ty ->
               ( (fun ppf ->
                   Format.fprintf ppf "Type %a@ coming@ from@ expression:"
                     Print.typ ty),
                 Mark.get ty ))
             operands)
        "I don't know how to apply operator %a on types@ %a"
        (Print.operator ~debug:true)
        op
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " and@ ")
           Print.typ)
        operands)

let overload_type (op : overloaded t Mark.pos) (operands : typ list) : typ =
  let rop = fst (resolve_overload op operands) in
  resolved_type rop

let is_pure : type a. a t -> bool = function
  | Map2 | Add_dat_dur _ | Add | Sub_dat_dur _ | Sub | Div_int_int | Div_rat_rat
  | Div_mon_int | Div_mon_rat | Div_mon_mon | Div_dur_dur | Div | Eq | Lte | Gte
  | Gt | Lt ->
    (* basically, operators that take a position in the backends, and their
       overloaded counterparts: those are the ones that can raise *)
    false
  | Log _ -> false
  | Not | Length | ToClosureEnv | FromClosureEnv | ArrayAccess _
  | ConstructorCheck _ | Minus | Minus_int | Minus_rat | Minus_mon | Minus_dur
  | ToInt | ToInt_mon | ToInt_rat | ToRat | ToRat_int | ToRat_mon | ToMoney
  | ToMoney_rat | ToMoney_int | Round | Round_rat | Round_mon | And | Or | Xor
  | Concat | Map | Filter | Find | Reduce | Sort _ | Fold | Add_int_int
  | Add_rat_rat | Add_mon_mon | Add_dur_dur | Sub_int_int | Sub_rat_rat
  | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur | Mult | Mult_int_int | Mult_rat_rat
  | Mult_mon_int | Mult_mon_rat | Mult_dur_int | HandleExceptions
  | ValueFromJson _ ->
    true
