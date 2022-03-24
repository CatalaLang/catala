(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type money = Z.t
type integer = Z.t
type decimal = Q.t
type date = CalendarLib.Date.t
type duration = CalendarLib.Date.Period.t

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

type 'a eoption = ENone of unit | ESome of 'a

exception EmptyError
exception AssertionFailed
exception ConflictError
exception UncomparableDurations
exception IndivisableDurations
exception ImpossibleDate
exception NoValueProvided of source_position

type runtime_value =
  | Unit
  | Bool of bool
  | Money of money
  | Integer of integer
  | Decimal of decimal
  | Date of date
  | Duration of duration
  | Enum of string list * (string * runtime_value)
  | Struct of string list * (string * runtime_value) list
  | Array of runtime_value Array.t
  | Unembeddable

let unembeddable _ = Unembeddable
let embed_unit () = Unit
let embed_bool x = Bool x
let embed_money x = Money x
let embed_integer x = Integer x
let embed_decimal x = Decimal x
let embed_date x = Date x
let embed_duration x = Duration x
let embed_array f x = Array (Array.map f x)

type event =
  | BeginCall of string list
  | EndCall of string list
  | VariableDefinition of string list * runtime_value
  | DecisionTaken of source_position

let log_ref : event list ref = ref []
let reset_log () = log_ref := []
let retrieve_log () = List.rev !log_ref

let log_begin_call info f x =
  log_ref := BeginCall info :: !log_ref;
  f x

let log_end_call info x =
  log_ref := EndCall info :: !log_ref;
  x

let log_variable_definition (info : string list) embed (x : 'a) =
  log_ref := VariableDefinition (info, embed x) :: !log_ref;
  x

let log_decision_taken pos x =
  if x then log_ref := DecisionTaken pos :: !log_ref;
  x

let money_of_cents_string (cents : string) : money = Z.of_string cents
let money_of_units_int (units : int) : money = Z.(of_int units * of_int 100)
let money_of_cents_integer (cents : integer) : money = cents
let money_to_float (m : money) : float = Z.to_float m /. 100.

let money_to_string (m : money) : string =
  Format.asprintf "%.2f" Q.(to_float (of_bigint m / of_int 100))

let money_to_cents m = m

let money_round (m : money) : money =
  let units, cents = Z.div_rem m (Z.of_int 100) in
  (* If [m] is negative, [cents] will also be negative. *)
  if Z.(abs cents < of_int 50) then Z.(units * of_int 100)
  else Z.((units + of_int (sign units)) * of_int 100)

let decimal_of_string (d : string) : decimal = Q.of_string d
let decimal_to_float (d : decimal) : float = Q.to_float d
let decimal_of_float (d : float) : decimal = Q.of_float d
let decimal_of_integer (d : integer) : decimal = Q.of_bigint d

let decimal_to_string ~(max_prec_digits : int) (i : decimal) : string =
  let sign = Q.sign i in
  let n = Z.abs (Q.num i) in
  let d = Z.abs (Q.den i) in
  let int_part = Z.ediv n d in
  let n = ref (Z.erem n d) in
  let digits = ref [] in
  let leading_zeroes (digits : Z.t list) : int =
    match
      List.fold_right
        (fun digit num_leading_zeroes ->
          match num_leading_zeroes with
          | `End _ -> num_leading_zeroes
          | `Begin i -> if Z.(digit = zero) then `Begin (i + 1) else `End i)
        digits (`Begin 0)
    with
    | `End i -> i
    | `Begin i -> i
  in
  while
    !n <> Z.zero
    && List.length !digits - leading_zeroes !digits < max_prec_digits
  do
    n := Z.mul !n (Z.of_int 10);
    digits := Z.ediv !n d :: !digits;
    n := Z.erem !n d
  done;
  Format.asprintf "%s%a.%a%s"
    (if sign < 0 then "-" else "")
    Z.pp_print int_part
    (Format.pp_print_list
       ~pp_sep:(fun _fmt () -> ())
       (fun fmt digit -> Format.fprintf fmt "%a" Z.pp_print digit))
    (List.rev !digits)
    (if List.length !digits - leading_zeroes !digits = max_prec_digits then "…"
    else "")

let integer_of_string (s : string) : integer = Z.of_string s
let integer_to_string (i : integer) : string = Z.to_string i
let integer_to_int (i : integer) : int = Z.to_int i
let integer_of_int (i : int) : integer = Z.of_int i
let integer_exponentiation (i : integer) (e : int) : integer = Z.pow i e
let integer_log2 = Z.log2
let year_of_date (d : date) : integer = Z.of_int (CalendarLib.Date.year d)

let month_number_of_date (d : date) : integer =
  Z.of_int (CalendarLib.Date.int_of_month (CalendarLib.Date.month d))

let day_of_month_of_date (d : date) : integer =
  Z.of_int (CalendarLib.Date.day_of_month d)

let date_of_numbers (year : int) (month : int) (day : int) : date =
  try CalendarLib.Date.make year month day with _ -> raise ImpossibleDate

let date_to_string (d : date) : string = CalendarLib.Printer.Date.to_string d

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  CalendarLib.Date.Period.make year month day

let duration_to_string (d : duration) : string =
  let x, y, z = CalendarLib.Date.Period.ymd d in
  let to_print =
    List.filter
      (fun (a, _) -> a <> 0)
      [ (x, "years"); (y, "months"); (z, "days") ]
  in
  match to_print with
  | [] -> "empty duration"
  | _ ->
      Format.asprintf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (d, l) -> Format.fprintf fmt "%d %s" d l))
        to_print

let duration_to_years_months_days (d : duration) : int * int * int =
  CalendarLib.Date.Period.ymd d

let duration_to_nb_days (d : duration) : int = CalendarLib.Date.Period.nb_days d

let handle_default :
      'a. (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a =
 fun exceptions just cons ->
  let except =
    Array.fold_left
      (fun acc except ->
        let new_val = try Some (except ()) with EmptyError -> None in
        match (acc, new_val) with
        | None, _ -> new_val
        | Some _, None -> acc
        | Some _, Some _ -> raise ConflictError)
      None exceptions
  in
  match except with
  | Some x -> x
  | None -> if just () then cons () else raise EmptyError

let handle_default_opt
    (exceptions : 'a eoption array) (just : bool eoption) (cons : 'a eoption) :
    'a eoption =
  let except =
    Array.fold_left
      (fun acc except ->
        match (acc, except) with
        | ENone _, _ -> except
        | ESome _, ENone _ -> acc
        | ESome _, ESome _ -> raise ConflictError)
      (ENone ()) exceptions
  in
  match except with
  | ESome _ -> except
  | ENone _ -> (
      match just with
      | ESome b -> if b then cons else ENone ()
      | ENone _ -> ENone ())

let no_input : unit -> 'a = fun _ -> raise EmptyError

let ( *$ ) (i1 : money) (i2 : decimal) : money =
  let rat_result = Q.mul (Q.of_bigint i1) i2 in
  let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
  (* we perform nearest rounding when multiplying an amount of money by a
     decimal !*)
  if Z.(of_int 2 * remainder >= Q.den rat_result) then Z.add res (Z.of_int 1)
  else res

let ( /$ ) (m1 : money) (m2 : money) : decimal =
  if Z.zero = m2 then raise Division_by_zero
  else Q.div (Q.of_bigint m1) (Q.of_bigint m2)

let ( +$ ) (m1 : money) (m2 : money) : money = Z.add m1 m2
let ( -$ ) (m1 : money) (m2 : money) : money = Z.sub m1 m2
let ( ~-$ ) (m1 : money) : money = Z.sub Z.zero m1
let ( +! ) (i1 : integer) (i2 : integer) : integer = Z.add i1 i2
let ( -! ) (i1 : integer) (i2 : integer) : integer = Z.sub i1 i2
let ( ~-! ) (i1 : integer) : integer = Z.sub Z.zero i1
let ( *! ) (i1 : integer) (i2 : integer) : integer = Z.mul i1 i2

let ( /! ) (i1 : integer) (i2 : integer) : integer =
  if Z.zero = i2 then raise Division_by_zero else Z.div i1 i2

let ( +& ) (i1 : decimal) (i2 : decimal) : decimal = Q.add i1 i2
let ( -& ) (i1 : decimal) (i2 : decimal) : decimal = Q.sub i1 i2
let ( ~-& ) (i1 : decimal) : decimal = Q.sub Q.zero i1
let ( *& ) (i1 : decimal) (i2 : decimal) : decimal = Q.mul i1 i2

let ( /& ) (i1 : decimal) (i2 : decimal) : decimal =
  if Q.zero = i2 then raise Division_by_zero else Q.div i1 i2

let ( +@ ) (d1 : date) (d2 : duration) : date = CalendarLib.Date.add d1 d2
let ( -@ ) (d1 : date) (d2 : date) : duration = CalendarLib.Date.sub d1 d2

let ( +^ ) (d1 : duration) (d2 : duration) : duration =
  CalendarLib.Date.Period.add d1 d2

let ( -^ ) (d1 : duration) (d2 : duration) : duration =
  CalendarLib.Date.Period.sub d1 d2

(* (EmileRolley) NOTE: {!CalendarLib.Date.Period.nb_days} is deprecated,
   {!CalendarLib.Date.Period.safe_nb_days} should be used. But the current
   {!duration} is greater that the supported polymorphic variants.*)
let ( /^ ) (d1 : duration) (d2 : duration) : decimal =
  try
    let nb_day1 = CalendarLib.Date.Period.nb_days d1 in
    let nb_day2 = CalendarLib.Date.Period.nb_days d2 in
    if 0 = nb_day2 then raise Division_by_zero else Q.(nb_day1 // nb_day2)
  with CalendarLib.Date.Period.Not_computable -> raise IndivisableDurations

let ( <=$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 <= 0
let ( >=$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 >= 0
let ( <$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 < 0
let ( >$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 > 0
let ( =$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 = 0
let ( >=! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 >= 0
let ( <=! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 <= 0
let ( >! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 > 0
let ( <! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 < 0
let ( =! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 = 0
let ( >=& ) (i1 : decimal) (i2 : decimal) : bool = Q.compare i1 i2 >= 0
let ( <=& ) (i1 : decimal) (i2 : decimal) : bool = Q.compare i1 i2 <= 0
let ( >& ) (i1 : decimal) (i2 : decimal) : bool = Q.compare i1 i2 > 0
let ( <& ) (i1 : decimal) (i2 : decimal) : bool = Q.compare i1 i2 < 0
let ( =& ) (i1 : decimal) (i2 : decimal) : bool = Q.compare i1 i2 = 0
let ( >=@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 >= 0
let ( <=@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 <= 0
let ( >@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 > 0
let ( <@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 < 0
let ( =@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 = 0

let compare_periods
    (p1 : CalendarLib.Date.Period.t) (p2 : CalendarLib.Date.Period.t) : int =
  try
    let p1_days = CalendarLib.Date.Period.nb_days p1 in
    let p2_days = CalendarLib.Date.Period.nb_days p2 in
    compare p1_days p2_days
  with CalendarLib.Date.Period.Not_computable -> raise UncomparableDurations

let ( >=^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 >= 0
let ( <=^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 <= 0
let ( >^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 > 0
let ( <^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 < 0
let ( =^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 = 0
let ( ~-^ ) (d1 : duration) : duration = CalendarLib.Date.Period.opp d1

let array_filter (f : 'a -> bool) (a : 'a array) : 'a array =
  Array.of_list (List.filter f (Array.to_list a))

let array_length (a : 'a array) : integer = Z.of_int (Array.length a)
