(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** The OCaml runtime. *)

(** {1 Types} *)

type nonrec unit = unit
type nonrec bool = bool

type money = Z.t
(** Number of cents *)

type integer = Z.t
type decimal = Q.t
type date = Dates_calc.Dates.date
type duration = Dates_calc.Dates.period
type date_rounding = Dates_calc.Dates.date_rounding

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

module Eoption : sig
  type 'a t = ENone of unit | ESome of 'a
end

(** This type characterizes the three levels of visibility for a given scope
    variable with regards to the scope's input and possible redefinitions inside
    the scope. *)
type io_input =
  | NoInput
      (** For an internal variable defined only in the scope, and does not
          appear in the input. *)
  | OnlyInput
      (** For variables that should not be redefined in the scope, because they
          appear in the input. *)
  | Reentrant
      (** For variables defined in the scope that can also be redefined by the
          caller as they appear in the input. *)
[@@deriving yojson_of]

type io_log = {
  io_input : io_input;
  io_output : bool;  (** [true] if the variable is an output *)
}
[@@deriving yojson_of]

(** {1 Exceptions} *)

exception EmptyError
exception AssertionFailed of source_position
exception ConflictError of source_position
exception UncomparableDurations
exception IndivisibleDurations
exception ImpossibleDate
exception NoValueProvided of source_position

(** {1 Value Embedding} *)

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
[@@deriving yojson_of]

val unembeddable : 'a -> runtime_value
val embed_unit : unit -> runtime_value
val embed_bool : bool -> runtime_value
val embed_money : money -> runtime_value
val embed_integer : integer -> runtime_value
val embed_decimal : decimal -> runtime_value
val embed_date : date -> runtime_value
val embed_duration : duration -> runtime_value
val embed_array : ('a -> runtime_value) -> 'a Array.t -> runtime_value

(** {1 Logging} *)

(** {2 Global process} *)

(** The logging is constituted of two phases:

    - The first one consists of collecting {i raw} events (see
      {!type:raw_event}) during the program execution (see {!val:retrieve_log})
      throught {!instruments}.
    - The second one consists in parsing the collected raw events into
      {i structured} ones (see {!type: event}). *)

(** {2 Data structures} *)

type information = string list [@@deriving yojson_of]
(** Represents information about a name in the code -- i.e. variable name,
    subscope name, etc...

    It's a list of strings with a length varying from 2 to 3, where:

    - the first string is the name of the current scope -- starting with a
      capitalized letter [Scope_name],
    - the second string is either: the name of a scope variable or, the name of
      a subscope input variable -- [a_subscope_var.input_var]
    - the third string is either: a subscope name (starting with a capitalized
      letter [Subscope_name] or, the [input] (resp. [output]) string -- which
      corresponds to the input (resp. the output) of a function. *)

(** {3 The raw events} *)

type raw_event =
  | BeginCall of information  (** Subscope or function call. *)
  | EndCall of information  (** End of a subscope or a function call. *)
  | VariableDefinition of information * io_log * runtime_value
      (** Definition of a variable or a function argument. *)
  | DecisionTaken of source_position  (** Source code position of an event. *)

(** {3 The structured events} *)

(** The corresponding grammar of the {!type: event} type, is the following:

    {v
<event> := <fun_call>
         | <subscope_call>
         | <var_def>
         | <var_def_with_fun>
         | VariableDefinition

<fun_call> :=
    VariableDefinition                      (function input)
    <fun_call_beg>
        <event>*
        (<var_def> | <var_def_with_fun>)    (function output)
    EndCall

<var_def_with_fun> :=
       /-- DecisionTaken
pos of |   <fun_call>+                      (function calls needed to compute the variable value)
       \-> VariableDefinition

<subscope_call> :=
    <sub_var_def>*          (sub-scope attributes def)
    <sub_call_beg>
        <event>+
    EndCall

<var_def> := DecisionTaken VariableDefinition(info, _)
  (when info.length = 2 && info[1] == "id")

<sub_var_def> := DecisionTaken VariableDefinition(info, _)
  (when info.length = 3)

<fun_call_beg> := BeginCall(info)
  (when info.length = 2)

<sub_call_beg> := BeginCall(info)
  (when info.length = 2 and '.' in info[1])
    v} *)

type event =
  | VarComputation of var_def
  | FunCall of fun_call
  | SubScopeCall of {
      name : information;
      inputs : var_def list;
      body : event list;
    }
[@@deriving yojson_of]

and var_def = {
  pos : source_position option;
  name : information;
  io : io_log;
  value : runtime_value;
  fun_calls : fun_call list option;
}

and fun_call = {
  fun_name : information;
  fun_inputs : var_def list;
  body : event list;
  output : var_def;
}

(** {2 Parsing} *)

val retrieve_log : unit -> raw_event list
(** [retrieve_log ()] returns the current list of collected [raw_event].*)

module EventParser : sig
  val parse_raw_events : raw_event list -> event list
  (** [parse_raw_events raw_events] parses raw events into {i structured} ones. *)
end

(** {2 Helping functions} *)

(** {3:instruments Logging instruments} *)

val reset_log : unit -> unit
val log_begin_call : string list -> 'a -> 'a
val log_end_call : string list -> 'a -> 'a

val log_variable_definition :
  string list -> io_log -> ('a -> runtime_value) -> 'a -> 'a

val log_decision_taken : source_position -> bool -> bool

(** {3 Pretty printers} *)

val pp_events : ?is_first_call:bool -> Format.formatter -> event list -> unit
(** [pp_events ~is_first_call ppf events] pretty prints in [ppf] the string
    representation of [events].

    If [is_first_call] is set to true, the formatter will be flush at the end.
    By default, [is_first_call] is set to false. *)

(**{1 Constructors and conversions} *)

(**{2 Money}*)

val money_of_cents_string : string -> money
val money_of_units_int : int -> money

val money_of_decimal : decimal -> money
(** Warning: rounds to nearest cent. *)

val money_of_cents_integer : integer -> money
val money_to_float : money -> float
val money_to_string : money -> string
val money_to_cents : money -> integer
val money_round : money -> money

(** {2 Decimals} *)

val decimal_of_string : string -> decimal
val decimal_to_string : max_prec_digits:int -> decimal -> string
val decimal_of_integer : integer -> decimal
val decimal_of_float : float -> decimal
val decimal_to_float : decimal -> float
val decimal_round : decimal -> decimal
val decimal_of_money : money -> decimal

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
val is_leap_year : integer -> bool
val year_of_date : date -> integer
val date_to_string : date -> string

val date_of_numbers : int -> int -> int -> date
(** Usage: [date_of_numbers year month day]

    @raise ImpossibleDate *)

val first_day_of_month : date -> date
val last_day_of_month : date -> date

(**{2 Durations} *)

val duration_of_numbers : int -> int -> int -> duration
(** Usage : [duration_of_numbers year mounth day]. *)

val duration_to_years_months_days : duration -> int * int * int
(**{2 Times} *)

val duration_to_string : duration -> string

(**{1 Defaults} *)

val handle_default :
  source_position -> (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a
(** @raise EmptyError
    @raise ConflictError *)

val handle_default_opt :
  source_position ->
  'a Eoption.t array ->
  (unit -> bool) ->
  (unit -> 'a Eoption.t) ->
  'a Eoption.t
(** @raise ConflictError *)

val no_input : unit -> 'a

(**{1 Operators} *)

module Oper : sig
  (* The types **must** match with Shared_ast.Operator.*_type *)
  val o_not : bool -> bool
  val o_length : 'a array -> integer
  val o_torat_int : integer -> decimal
  val o_torat_mon : money -> decimal
  val o_tomoney_rat : decimal -> money
  val o_getDay : date -> integer
  val o_getMonth : date -> integer
  val o_getYear : date -> integer
  val o_firstDayOfMonth : date -> date
  val o_lastDayOfMonth : date -> date
  val o_round_mon : money -> money
  val o_round_rat : decimal -> decimal
  val o_minus_int : integer -> integer
  val o_minus_rat : decimal -> decimal
  val o_minus_mon : money -> money
  val o_minus_dur : duration -> duration
  val o_and : bool -> bool -> bool
  val o_or : bool -> bool -> bool
  val o_xor : bool -> bool -> bool
  val o_eq : 'a -> 'a -> bool
  val o_map : ('a -> 'b) -> 'a array -> 'b array
  val o_reduce : ('a -> 'a -> 'a) -> 'a -> 'a array -> 'a
  val o_concat : 'a array -> 'a array -> 'a array
  val o_filter : ('a -> bool) -> 'a array -> 'a array
  val o_add_int_int : integer -> integer -> integer
  val o_add_rat_rat : decimal -> decimal -> decimal
  val o_add_mon_mon : money -> money -> money
  val o_add_dat_dur : date_rounding -> date -> duration -> date
  val o_add_dur_dur : duration -> duration -> duration
  val o_sub_int_int : integer -> integer -> integer
  val o_sub_rat_rat : decimal -> decimal -> decimal
  val o_sub_mon_mon : money -> money -> money
  val o_sub_dat_dat : date -> date -> duration
  val o_sub_dat_dur : date -> duration -> date
  val o_sub_dur_dur : duration -> duration -> duration
  val o_mult_int_int : integer -> integer -> integer
  val o_mult_rat_rat : decimal -> decimal -> decimal
  val o_mult_mon_rat : money -> decimal -> money
  val o_mult_dur_int : duration -> integer -> duration
  val o_div_int_int : integer -> integer -> decimal
  val o_div_rat_rat : decimal -> decimal -> decimal
  val o_div_mon_mon : money -> money -> decimal
  val o_div_mon_rat : money -> decimal -> money
  val o_div_dur_dur : duration -> duration -> decimal
  val o_lt_int_int : integer -> integer -> bool
  val o_lt_rat_rat : decimal -> decimal -> bool
  val o_lt_mon_mon : money -> money -> bool
  val o_lt_dur_dur : duration -> duration -> bool
  val o_lt_dat_dat : date -> date -> bool
  val o_lte_int_int : integer -> integer -> bool
  val o_lte_rat_rat : decimal -> decimal -> bool
  val o_lte_mon_mon : money -> money -> bool
  val o_lte_dur_dur : duration -> duration -> bool
  val o_lte_dat_dat : date -> date -> bool
  val o_gt_int_int : integer -> integer -> bool
  val o_gt_rat_rat : decimal -> decimal -> bool
  val o_gt_mon_mon : money -> money -> bool
  val o_gt_dur_dur : duration -> duration -> bool
  val o_gt_dat_dat : date -> date -> bool
  val o_gte_int_int : integer -> integer -> bool
  val o_gte_rat_rat : decimal -> decimal -> bool
  val o_gte_mon_mon : money -> money -> bool
  val o_gte_dur_dur : duration -> duration -> bool
  val o_gte_dat_dat : date -> date -> bool
  val o_eq_int_int : integer -> integer -> bool
  val o_eq_rat_rat : decimal -> decimal -> bool
  val o_eq_mon_mon : money -> money -> bool
  val o_eq_dur_dur : duration -> duration -> bool
  val o_eq_dat_dat : date -> date -> bool
  val o_fold : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
end

include module type of Oper

(** Modules API *)

type hash = string

val register_module : string -> (string * Obj.t) list -> hash -> unit
(** Registers a module by the given name defining the given bindings. Required
    for evaluation to be able to access the given values. The last argument is
    expected to be a hash of the source file and the Catala version, and will in
    time be used to ensure that the module and the interface are in sync *)

val check_module : string -> hash -> bool
(** Returns [true] if it has been registered with the correct hash, [false] if
    there is a hash mismatch.

    @raise Not_found if the module does not exist at all *)

val lookup_value : string list * string -> Obj.t
