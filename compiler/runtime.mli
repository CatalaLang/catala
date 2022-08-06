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

(** {1 Types} *)

type money
type integer
type decimal
type date
type duration

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}
[@@deriving yojson_of]

type 'a eoption = ENone of unit | ESome of 'a

(** {1 Exceptions} *)

exception EmptyError
exception AssertionFailed
exception ConflictError
exception UncomparableDurations
exception IndivisableDurations
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
  | VariableDefinition of information * runtime_value
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
  value : runtime_value;
  fun_calls : fun_call list option;
}

and fun_call = {
  fun_name : information;
  input : var_def;
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
val log_variable_definition : string list -> ('a -> runtime_value) -> 'a -> 'a
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
val duration_to_years_months_days : duration -> int * int * int
val duration_to_string : duration -> string

(**{1 Defaults} *)

val handle_default : (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a
(** @raise EmptyError
    @raise ConflictError *)

val handle_default_opt :
  'a eoption array -> bool eoption -> 'a eoption -> 'a eoption
(** @raise ConflictError *)

val no_input : unit -> 'a

(**{1 Operators} *)

(**{2 Money} *)

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

(* val ( /^ ) : duration -> duration -> decimal *)
(** @raise Division_by_zero
    @raise IndivisableDurations *)

val ( *^ ) : duration -> integer -> duration
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
