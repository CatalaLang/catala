(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils

(** {1 Types} *)

type money = Z.t

type integer = Z.t

type decimal = Q.t

type date = CalendarLib.Date.t

type duration = CalendarLib.Date.Period.t

(**{1 Constructors} *)

let money_of_cent_string (cents : string) : money = Z.of_string cents

let money_of_units_integers (units : int) : money = Z.(of_int units * of_int 100)

let decimal_of_string (d : string) : decimal = Q.of_string d

let integer_of_string (i : string) : integer = Z.of_string i

let date_of_numbers (year : int) (month : int) (day : int) : date =
  CalendarLib.Date.make year month day

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  CalendarLib.Date.Period.make year month day

let int_to_rat (i : integer) : decimal = Q.of_bigint i

(**{1 Exceptions and defaults} *)

exception EmptyError

let error_empty : 'a. 'a -> 'a =
 fun x -> try x with EmptyError -> Errors.raise_error "empty value found!"

let handle_default : 'a. (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a =
 fun exceptions just cons ->
  let except =
    Array.fold_left
      (fun acc except ->
        let new_val = try Some (except ()) with EmptyError -> None in
        match (acc, new_val) with
        | None, _ -> new_val
        | Some _, None -> acc
        | Some _, Some _ -> Errors.raise_error "conflict!")
      None exceptions
  in
  match except with Some x -> x | None -> if just () then cons () else raise EmptyError

let no_input : unit -> 'a = fun _ -> raise EmptyError

(**{1 Operators} *)

let ( *$ ) (i1 : money) (i2 : decimal) : money =
  let rat_result = Q.mul (Q.of_bigint i1) i2 in
  let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
  (* we perform nearest rounding when multiplying an amount of money by a decimal !*)
  if Z.(of_int 2 * remainder >= Q.den rat_result) then Z.add res (Z.of_int 1) else res

let ( /$ ) (i1 : money) (i2 : money) : decimal =
  if i2 <> Z.zero then Q.div (Q.of_bigint i1) (Q.of_bigint i2)
  else Errors.raise_error "division by zero at runtime"

let ( +$ ) (i1 : money) (i2 : money) : money = Z.add i1 i2

let ( -$ ) (i1 : money) (i2 : money) : money = Z.sub i1 i2

let ( ~-$ ) (i1 : money) : money = Z.sub Z.zero i1

let ( +! ) (i1 : integer) (i2 : integer) : integer = Z.add i1 i2

let ( -! ) (i1 : integer) (i2 : integer) : integer = Z.sub i1 i2

let ( ~-! ) (i1 : integer) : integer = Z.sub Z.zero i1

let ( *! ) (i1 : integer) (i2 : integer) : integer = Z.mul i1 i2

let ( /! ) (i1 : integer) (i2 : integer) : integer =
  if i2 <> Z.zero then Z.div i1 i2 else Errors.raise_error "division by zero at runtime"

let ( +. ) (i1 : decimal) (i2 : decimal) : decimal = Q.add i1 i2

let ( -. ) (i1 : decimal) (i2 : decimal) : decimal = Q.sub i1 i2

let ( ~-. ) (i1 : decimal) : decimal = Q.sub Q.zero i1

let ( *. ) (i1 : decimal) (i2 : decimal) : decimal = Q.mul i1 i2

let ( /. ) (i1 : decimal) (i2 : decimal) : decimal =
  if i2 <> Q.zero then Q.div i1 i2 else Errors.raise_error "division by zero at runtime"

let ( +@ ) (d1 : date) (d2 : duration) : date = CalendarLib.Date.add d1 d2

let ( -@ ) (d1 : date) (d2 : date) : duration = CalendarLib.Date.sub d1 d2

let ( +^ ) (d1 : duration) (d2 : duration) : duration = CalendarLib.Date.Period.add d1 d2

let ( -^ ) (d1 : duration) (d2 : duration) : duration = CalendarLib.Date.Period.sub d1 d2

let ( <=$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 <= 0

let ( >=$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 >= 0

let ( <$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 < 0

let ( >$ ) (m1 : money) (m2 : money) : bool = Z.compare m1 m2 > 0

let ( >=! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 >= 0

let ( <=! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 <= 0

let ( >! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 > 0

let ( <! ) (i1 : integer) (i2 : integer) : bool = Z.compare i1 i2 < 0

let ( >=@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 >= 0

let ( <=@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 <= 0

let ( >@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 > 0

let ( <@ ) (d1 : date) (d2 : date) : bool = CalendarLib.Date.compare d1 d2 < 0

let compare_periods (p1 : CalendarLib.Date.Period.t) (p2 : CalendarLib.Date.Period.t) : int =
  try
    let p1_days = CalendarLib.Date.Period.nb_days p1 in
    let p2_days = CalendarLib.Date.Period.nb_days p2 in
    compare p1_days p2_days
  with CalendarLib.Date.Period.Not_computable ->
    Errors.raise_error
      "Cannot compare together durations that cannot be converted to a precise number of days"

let ( >=^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 >= 0

let ( <=^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 <= 0

let ( >^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 > 0

let ( <^ ) (d1 : duration) (d2 : duration) : bool = compare_periods d1 d2 < 0

let array_filter (f : 'a -> bool) (a : 'a array) : 'a array =
  Array.of_list (List.filter f (Array.to_list a))

let array_length (a : 'a array) : integer = Z.of_int (Array.length a)

let get_year (d : date) : integer = Z.of_int (CalendarLib.Date.year d)

let get_month (d : date) = Z.of_int (CalendarLib.Date.int_of_month (CalendarLib.Date.month d))

let get_day (d : date) = Z.of_int (CalendarLib.Date.day_of_month d)
