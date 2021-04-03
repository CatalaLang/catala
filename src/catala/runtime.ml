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

type money = Z.t

type integer = Z.t

type decimal = Q.t

type date = CalendarLib.Date.t

type duration = CalendarLib.Date.Period.t

exception EmptyError

exception NoValueProvided

exception AssertionFailed

exception ConflictError

exception UncomparableDurations

exception ImpossibleDate

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

type store_key = Hmap.Key.t

type event =
  | BeginCall of string list * store_key
  | EndCall of string list * store_key
  | VariableDefinition of string list * store_key
  | DecisionTaken of source_position

let log_ref : event list ref = ref []

let store_ref : Hmap.t ref = ref Hmap.empty

let reset_log () =
  log_ref := [];
  store_ref := Hmap.empty

let retrieve_log () = List.rev !log_ref

(* This function is where we have to punch trough the OCaml type system. Indeed, this value store is
   really a cheap version of an embedding and de-embedding system where values are annotated by
   their types. However, since this logging is meant to be accessed through Javascript where we have
   access to type tagging, this is fine? *)
let retrieve_value : 'a. store_key -> 'a =
 fun key ->
  let unique =
    Hmap.filter
      (fun binding ->
        match binding with Hmap.B (key', _) -> Hmap.Key.equal key (Hmap.Key.hide_type key'))
      !store_ref
  in
  match Hmap.get_any_binding unique with Hmap.B (_, v) -> Obj.magic v

let log_begin_call info f x =
  let x_key = Hmap.Key.create () in
  store_ref := Hmap.add x_key x !store_ref;
  log_ref := BeginCall (info, Hmap.Key.hide_type x_key) :: !log_ref;
  f x

let log_end_call info x =
  let x_key = Hmap.Key.create () in
  store_ref := Hmap.add x_key x !store_ref;
  log_ref := EndCall (info, Hmap.Key.hide_type x_key) :: !log_ref;
  x

let log_variable_definition (info : string list) (x : 'a) =
  let x_key = Hmap.Key.create () in
  store_ref := Hmap.add x_key x !store_ref;
  log_ref := VariableDefinition (info, Hmap.Key.hide_type x_key) :: !log_ref;
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
  while !n <> Z.zero && List.length !digits - leading_zeroes !digits < max_prec_digits do
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
    (if List.length !digits - leading_zeroes !digits = max_prec_digits then "…" else "")

let integer_of_string (i : string) : integer = Z.of_string i

let integer_to_string (i : integer) : string = Z.to_string i

let integer_to_int (i : integer) : int = Z.to_int i

let integer_of_int (i : int) : integer = Z.of_int i

let integer_exponentiation (i : integer) (e : int) : integer = Z.pow i e

let integer_log2 = Z.log2

let year_of_date (d : date) : integer = Z.of_int (CalendarLib.Date.year d)

let month_number_of_date (d : date) : integer =
  Z.of_int (CalendarLib.Date.int_of_month (CalendarLib.Date.month d))

let day_of_month_of_date (d : date) : integer = Z.of_int (CalendarLib.Date.day_of_month d)

let date_of_numbers (year : int) (month : int) (day : int) : date =
  try CalendarLib.Date.make year month day with _ -> raise ImpossibleDate

let date_to_string (d : date) : string = CalendarLib.Printer.Date.to_string d

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  CalendarLib.Date.Period.make year month day

let duration_to_string (d : duration) : string =
  let x, y, z = CalendarLib.Date.Period.ymd d in
  let to_print = List.filter (fun (a, _) -> a <> 0) [ (x, "years"); (y, "months"); (z, "days") ] in
  match to_print with
  | [] -> "empty duration"
  | _ ->
      Format.asprintf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (d, l) -> Format.fprintf fmt "%d %s" d l))
        to_print

let duration_to_days_months_years (d : duration) : int * int * int = CalendarLib.Date.Period.ymd d

let handle_default : 'a. (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a =
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
  match except with Some x -> x | None -> if just () then cons () else raise EmptyError

let no_input : unit -> 'a = fun _ -> raise EmptyError

let ( *$ ) (i1 : money) (i2 : decimal) : money =
  let rat_result = Q.mul (Q.of_bigint i1) i2 in
  let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
  (* we perform nearest rounding when multiplying an amount of money by a decimal !*)
  if Z.(of_int 2 * remainder >= Q.den rat_result) then Z.add res (Z.of_int 1) else res

let ( /$ ) (i1 : money) (i2 : money) : decimal =
  if i2 <> Z.zero then Q.div (Q.of_bigint i1) (Q.of_bigint i2) else raise Division_by_zero

let ( +$ ) (i1 : money) (i2 : money) : money = Z.add i1 i2

let ( -$ ) (i1 : money) (i2 : money) : money = Z.sub i1 i2

let ( ~-$ ) (i1 : money) : money = Z.sub Z.zero i1

let ( +! ) (i1 : integer) (i2 : integer) : integer = Z.add i1 i2

let ( -! ) (i1 : integer) (i2 : integer) : integer = Z.sub i1 i2

let ( ~-! ) (i1 : integer) : integer = Z.sub Z.zero i1

let ( *! ) (i1 : integer) (i2 : integer) : integer = Z.mul i1 i2

let ( /! ) (i1 : integer) (i2 : integer) : integer =
  if i2 <> Z.zero then Z.div i1 i2 else raise Division_by_zero

let ( +& ) (i1 : decimal) (i2 : decimal) : decimal = Q.add i1 i2

let ( -& ) (i1 : decimal) (i2 : decimal) : decimal = Q.sub i1 i2

let ( ~-& ) (i1 : decimal) : decimal = Q.sub Q.zero i1

let ( *& ) (i1 : decimal) (i2 : decimal) : decimal = Q.mul i1 i2

let ( /& ) (i1 : decimal) (i2 : decimal) : decimal =
  if i2 <> Q.zero then Q.div i1 i2 else raise Division_by_zero

let ( +@ ) (d1 : date) (d2 : duration) : date = CalendarLib.Date.add d1 d2

let ( -@ ) (d1 : date) (d2 : date) : duration = CalendarLib.Date.sub d1 d2

let ( +^ ) (d1 : duration) (d2 : duration) : duration = CalendarLib.Date.Period.add d1 d2

let ( -^ ) (d1 : duration) (d2 : duration) : duration = CalendarLib.Date.Period.sub d1 d2

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

let compare_periods (p1 : CalendarLib.Date.Period.t) (p2 : CalendarLib.Date.Period.t) : int =
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
