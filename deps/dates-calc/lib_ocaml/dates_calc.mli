(* This file is part of the Dates_calc library. Copyright (C) 2022 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Aymeric Fromherz
   <aymeric.fromherz@inria.fr>, Raphaël Monat <raphael.monat@lip6.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type date
(** A valid date in the standard Gregorian calendar. *)

type period
(** A period can be any number and combination of days, months, years. *)

exception InvalidDate
exception AmbiguousComputation

type date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound
      (** When choosing [AbortOnRound], functions may raise
          [AmbiguousComputation]. *)

(** {2 Functions on dates}*)

val make_date : year:int -> month:int -> day:int -> date
(** @raise [InvalidDate]*)

val add_dates : ?round:date_rounding -> date -> period -> date
(** @raise [AmbiguousComputation] *)

val sub_dates : date -> date -> period
(** The returned [period] is always expressed as a number of days. *)

val compare_dates : date -> date -> int
val date_to_ymd : date -> int * int * int

val format_date : Format.formatter -> date -> unit
(** Respects ISO8601 format. *)

val date_of_string : string -> date

val first_day_of_month : date -> date
val last_day_of_month : date -> date

val is_leap_year : int -> bool

(** {2 Functions on periods}*)

val make_period : years:int -> months:int -> days:int -> period
val neg_period : period -> period
val add_periods : period -> period -> period
val sub_periods : period -> period -> period
val mul_period : period -> int -> period

val format_period : Format.formatter -> period -> unit
val period_of_string : string -> period

val period_to_days : period -> int
(** @raise [AmbiguousComputation]
      when the period is anything else than a number of days. *)

val period_to_ymds : period -> int * int * int
