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

(** {1 Types} *)

type money

type integer

type decimal

type date

type duration

(** {1 Exceptions} *)

exception EmptyError

exception AssertionFailed

exception ConflictError

exception UncomparableDurations

exception ImpossibleDate

(**{1 Constructors and conversions} *)

(**{2 Money}*)

val money_of_cents_string : string -> money

val money_of_units_int : int -> money

val money_of_cents_integer : integer -> money

val money_to_float : money -> float

val money_to_string : money -> string

val money_to_cents : money -> integer

(** {2 Decimals} *)

val decimal_of_string : string -> decimal

val decimal_to_string : max_prec_digits:int -> decimal -> string

val decimal_of_integer : integer -> decimal

val decimal_of_float : float -> decimal

val decimal_to_float : decimal -> float

(**{2 Integers} *)

val integer_of_string : string -> integer

val integer_to_string : integer -> string

val integer_to_int : integer -> int

val integer_of_int : int -> integer

val integer_log2 : integer -> int

val integer_exponentiation : integer -> int -> integer

(**{2 Dates} *)

val day_of_month_of_date : date -> integer

val month_number_of_date : date -> integer

val year_of_date : date -> integer

val date_to_string : date -> string

val date_of_numbers : int -> int -> int -> date
(** Usage: [date_of_numbers year month day]

    @raise ImpossibleDate *)

(**{2 Durations} *)

val duration_of_numbers : int -> int -> int -> duration

val duration_to_days_months_years : duration -> int * int * int

val duration_to_string : duration -> string

(**{1 Defaults} *)

val error_empty : 'a -> 'a

val handle_default : (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a
(** @raise EmptyError
    @raise ConflictError *)

val no_input : unit -> 'a

(**{1 Operators} *)

(**{ 2 Money} *)

val ( *$ ) : money -> decimal -> money

val ( /$ ) : money -> money -> decimal
(** @raise Division_by_zero *)

val ( +$ ) : money -> money -> money

val ( -$ ) : money -> money -> money

val ( ~-$ ) : money -> money

val ( =$ ) : money -> money -> bool

val ( <=$ ) : money -> money -> bool

val ( >=$ ) : money -> money -> bool

val ( <$ ) : money -> money -> bool

val ( >$ ) : money -> money -> bool

(**{2 Integers} *)

val ( +! ) : integer -> integer -> integer

val ( -! ) : integer -> integer -> integer

val ( ~-! ) : integer -> integer

val ( *! ) : integer -> integer -> integer

val ( /! ) : integer -> integer -> integer
(** @raise Division_by_zero *)

val ( =! ) : integer -> integer -> bool

val ( >=! ) : integer -> integer -> bool

val ( <=! ) : integer -> integer -> bool

val ( >! ) : integer -> integer -> bool

val ( <! ) : integer -> integer -> bool

(** {2 Decimals} *)

val ( +& ) : decimal -> decimal -> decimal

val ( -& ) : decimal -> decimal -> decimal

val ( ~-& ) : decimal -> decimal

val ( *& ) : decimal -> decimal -> decimal

val ( /& ) : decimal -> decimal -> decimal
(** @raise Division_by_zero *)

val ( =& ) : decimal -> decimal -> bool

val ( >=& ) : decimal -> decimal -> bool

val ( <=& ) : decimal -> decimal -> bool

val ( >& ) : decimal -> decimal -> bool

val ( <& ) : decimal -> decimal -> bool

(** {2 Dates} *)

val ( +@ ) : date -> duration -> date

val ( -@ ) : date -> date -> duration

val ( =@ ) : date -> date -> bool

val ( >=@ ) : date -> date -> bool

val ( <=@ ) : date -> date -> bool

val ( >@ ) : date -> date -> bool

val ( <@ ) : date -> date -> bool

(** {2 Durations} *)

val ( +^ ) : duration -> duration -> duration

val ( -^ ) : duration -> duration -> duration

val ( ~-^ ) : duration -> duration

val ( =^ ) : duration -> duration -> bool

val ( >=^ ) : duration -> duration -> bool
(** @raise UncomparableDurations *)

val ( <=^ ) : duration -> duration -> bool
(** @raise UncomparableDurations *)

val ( >^ ) : duration -> duration -> bool
(** @raise UncomparableDurations *)

val ( <^ ) : duration -> duration -> bool
(** @raise UncomparableDurations *)

(** {2 Arrays} *)

val array_filter : ('a -> bool) -> 'a array -> 'a array

val array_length : 'a array -> integer
