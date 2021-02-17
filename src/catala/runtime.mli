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

(**{1 Constructors and conversions} *)

val money_of_cent_string : string -> money

val money_of_units_integers : int -> money

val money_to_float : money -> float

val decimal_of_string : string -> decimal

val integer_of_string : string -> integer

val integer_to_int : integer -> int

val integer_of_int : int -> integer

val date_of_calendar_date : CalendarLib.Date.t -> date

val date_to_calendar_date : date -> CalendarLib.Date.t

val duration_of_calendar_period : CalendarLib.Date.Period.t -> duration

val duration_to_calendar_period : duration -> CalendarLib.Date.Period.t

val date_of_numbers : int -> int -> int -> date
(** Usage: [date_of_numbers year month day] *)

val duration_of_numbers : int -> int -> int -> duration

val int_to_rat : integer -> decimal

(**{1 Exceptions and defaults} *)

exception EmptyError

exception AssertionFailed

exception EmptyValue

exception ConflictError

val error_empty : 'a -> 'a

val handle_default : (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a

val no_input : unit -> 'a

(**{1 Operators} *)

val ( *$ ) : money -> decimal -> money

val ( /$ ) : money -> money -> decimal

val ( +$ ) : money -> money -> money

val ( -$ ) : money -> money -> money

val ( ~-$ ) : money -> money

val ( +! ) : integer -> integer -> integer

val ( -! ) : integer -> integer -> integer

val ( ~-! ) : integer -> integer

val ( *! ) : integer -> integer -> integer

val ( /! ) : integer -> integer -> integer

val ( +& ) : decimal -> decimal -> decimal

val ( -& ) : decimal -> decimal -> decimal

val ( ~-& ) : decimal -> decimal

val ( *& ) : decimal -> decimal -> decimal

val ( /& ) : decimal -> decimal -> decimal

val ( +@ ) : date -> duration -> date

val ( -@ ) : date -> date -> duration

val ( +^ ) : duration -> duration -> duration

val ( -^ ) : duration -> duration -> duration

val ( <=$ ) : money -> money -> bool

val ( >=$ ) : money -> money -> bool

val ( <$ ) : money -> money -> bool

val ( >$ ) : money -> money -> bool

val ( >=! ) : integer -> integer -> bool

val ( <=! ) : integer -> integer -> bool

val ( >! ) : integer -> integer -> bool

val ( <! ) : integer -> integer -> bool

val ( >=@ ) : date -> date -> bool

val ( <=@ ) : date -> date -> bool

val ( >@ ) : date -> date -> bool

val ( <@ ) : date -> date -> bool

val ( >=^ ) : duration -> duration -> bool

val ( <=^ ) : duration -> duration -> bool

val ( >^ ) : duration -> duration -> bool

val ( <^ ) : duration -> duration -> bool

val array_filter : ('a -> bool) -> 'a array -> 'a array

val array_length : 'a array -> integer

val get_year : date -> integer

val get_month : date -> integer

val get_day : date -> integer
