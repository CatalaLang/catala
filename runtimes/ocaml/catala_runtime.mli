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
type date = Dates_calc.date
type duration = Dates_calc.period

type date_rounding = Dates_calc.date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound

type code_location = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

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

type io_log = {
  io_input : io_input;
  io_output : bool;  (** [true] if the variable is an output *)
}

(** {1 Exceptions} *)

type error =
  | AssertionFailed  (** An assertion in the program doesn't hold *)
  | NoValue  (** No computation with valid conditions found *)
  | Conflict  (** Two different valid computations at that point *)
  | DivisionByZero  (** The denominator happened to be 0 here *)
  | ListEmpty  (** Element access on an empty list *)
  | NotSameLength  (** Traversing multiple lists of different lengths *)
  | UncomparableValues  (** Equality check or comparison on functions *)
  | DateError of string  (** Errors related to date and duration computations *)
  | Impossible  (** The "impossible" keyword was reached *)

val error_to_string : error -> string
(** Returns the capitalized tag of the error as a string *)

val error_message : error -> string
(** Returns a short explanation message about the error *)

exception Error of error * code_location list * string option
exception Empty

(** {1 Value Embedding} *)

(** {2 Runtime type encoding} *)

module Value : sig
  type _ external_tag = ..

  module type External = sig
    type t
    type _ external_tag += T : t external_tag

    val name : string
    val equal : code_location -> t -> t -> bool
    val compare : code_location -> t -> t -> int
    val print : t -> string
    val to_json : t -> string
    val from_json : code_location -> string -> t
  end

  (** 'a ty provides runtime information about the structure of values of OCaml
      type 'a *)
  type _ ty =
    | Unit : unit ty
    | Bool : bool ty
    | Integer : integer ty
    | Money : money ty
    | Decimal : decimal ty
    | Date : date ty
    | Duration : duration ty
    | Position : code_location ty
    | Array : ('a -> t) -> 'a array ty
    | Tuple : ('a -> t list) -> 'a ty
    | Struct : {
        name : string;
        fields : 'a -> (string * t) list;
            (* list order must be consistent with the representation *)
      }
        -> 'a ty
    | Enum : {
        name : string;
        constr : 'a -> int * string * t option;
            (* destr: string * t option -> 'a; ? *)
      }
        -> 'a ty
    | External : (module External with type t = 'a) -> 'a ty
    | Function : 'a ty
    | Polymorphic : 'a ty
  (* | Function : (('args -> 'ret) -> 'args -> t ) -> ('args -> 'ret) ty *)

  (** [Runtime.Value.t] is an embedded runtime value that comes with type
      information, allowing for introspection *)
  and t = V : 'a ty * 'a -> t

  val embed : 'a ty -> 'a -> t
  val equal : code_location -> t -> t -> bool
  val compare : code_location -> t -> t -> int
  val format : Format.formatter -> t -> unit
  val from_json : 'a ty -> code_location -> string -> 'a
end

val equal : 'a Value.ty -> code_location -> 'a -> 'a -> bool
(** Polymorphic, structural equality using runtime type information *)

val compare : 'a Value.ty -> code_location -> 'a -> 'a -> int
(** Polymorphic, structural comparison using runtime type information *)

(* val unembed: runvalue -> 'a runtype * 'a *)

(** {1 Catala types utils} *)

module type CatalaType = sig
  type t

  val rtype : t Value.ty
end

module Optional : sig
  type 'a t = Absent | Present of 'a

  val rtype : 'a Value.ty -> 'a t Value.ty
  val of_option : 'a option -> 'a t
end

(** This interface must be supplied to extend the Catala runtime with abstract
    types *)
module type ExternalTypeSpec = sig
  type t
  (** The embedded type *)

  val name : string
  (** Catala name of the type (capitalised) *)

  val equal : code_location -> t -> t -> bool

  val compare : code_location -> t -> t -> int
  (** Standard [compare] function: must return -1, 0 or 1 depending on whether
      the left-hand side is respectively smaller, equal or greater than the
      right-hand side *)

  val print : t -> string
  (** User-directed printing of the value *)

  val to_json : t -> string
  val from_json : code_location -> string -> t
end

module ExternalType (Spec : ExternalTypeSpec) : CatalaType with type t = Spec.t

(** {1 Execution traces} *)

(** The trace construction mechanism is a stateful process (i.e., non-reentrant)
    that collect traces emitted by the generated trace constructors defined
    below.

    The built trace is organized as a tree where each node represent a trace
    element (e.g., a variable definition, a branching, etc.) which may have
    sub-traces. For instance, a function that defines local variable definitions
    will be represented as a [FunCall] node with a [sub_trace] containing these
    [LocalVarDef] sub-nodes.

    Conceptually, whenever we reach a trace event, we open a scope, evaluate the
    sub-expression potentially yielding new traces that will be its sub-nodes,
    close the scope and finalize this trace node with the computed
    sub-expression value.

    Whenever a runtime error is triggered, we insert an [Error] node as a
    sub-trace of the currently opened scope and re-raise this error. This is
    automatically performed by the [with_trace] wrapper.

    The global trace state can be retrieved using the [retrieve_trace] accessor
    and cleared using [reset_trace]. *)

(** {2 Traces types} *)

(** This type describes all the different trace kind generated throughout
    execution *)
type trace_kind =
  | ScopeCall of trace_ident_decl
      (** Scope call with the scope declaration information *)
  | ScopeVarDef of { var : trace_ident_decl; io : io_log }
      (** Scope variable definition with its declaration information and its i/o
          kind *)
  | LocalVarDef of string  (** Let-binding of a local variable with its name *)
  | LocalTupDef of string list
      (** Let-binding of a local tuple with the name of each binded variable *)
  | FunCall of trace_ident_decl
      (** Function call with its declaration information *)
  | BranchingCondition
      (** Branching condition for an if-then-else or a pattern matching *)
  | IfBranching
      (** Branch taken of the consequence or alternative of an if-then-else *)
  | MatchBranching of { constructor_name : string }
      (** Pattern taken of a pattern-matching *)
  | Assertion  (** Beginning of an assertion *)
  | Exception of {
      label : (string * code_location) option;
          (** Exception label and its position in case of named exception *)
      cons_pos : code_location;
          (** Position of the consequence that would be evaluated if this
              exception gets fulfilled *)
    }  (** Scope variable definition exception *)
  | Error of {
      error : error;  (** Runtime error itself *)
      locs : code_location list;  (** Extra-relevant locations *)
      message : string option;  (** User-faced error message *)
    }  (** Runtime error description *)

and trace_ident_decl = { name : string; decl_pos : code_location }
(** Variable-kind declaration info, i.e., name and declaration position *)

type trace = trace_element list

and trace_element = {
  kind : trace_kind;  (** The [kind] of trace *)
  pos : code_location;  (** The expression [pos] responsible for this trace *)
  value : Value.t option;
      (** The resulting evaluated value. No value present on error. *)
  sub_trace : trace;
      (** Sub elements of the trace. E.g., a [ScopeCall] trace will contain
          [ScopeVarDef] traces in its sub-trace. *)
}
(** Trace node *)

(** {2 Trace constructors} *)

val begin_trace : trace_kind -> code_location -> unit
(** Begins a trace scope given a [trace_kind] and its [code_location]. *)

val end_trace : ?value:Value.t -> unit -> unit
(** Ends the currently opened trace scope and register the [value]. *)

val single_trace : trace_kind -> code_location -> unit
(** Equivalent to a sequence of [begin_trace] then [end_trace]. Used for error
    reporting. *)

val with_trace :
  embed:('a -> Value.t) -> trace_kind -> code_location -> (unit -> 'a) -> 'a
(** Wrapper that takes a function [f] and insert a [begin_trace] before [f ()]
    and a [end_trace] afterwards. It also retrieves and embeds the computed
    value.

    It also catches runtime's [error]s and insert a [single_trace] before
    re-raising it. When it occurs, a flag is also set so that callers do not
    catch it again. *)

(** {2 Trace accessors} *)

val retrieve_trace : unit -> trace
(** Finalize the global trace state and returns the immutable [trace] value. *)

val reset_trace : unit -> unit
(** Resets the global trace state. *)

(** {1 Pretty printers} *)

(** This module is for setting options and internals, use [Value.format] to
    print values *)
module Print : sig
  type lang = [ `En | `Fr | `Pl ]

  val set_lang : [ `En | `Fr | `Pl ] -> unit
  (** Sets the language to be used for the output of values *)

  val get_lang : unit -> [ `En | `Fr | `Pl ]

  val set_precision : int -> unit
  (** Sets the maximum number of decimal numbers to print *)

  val get_precision : unit -> int
end

(** {1 JSON printers} *)

module Json : sig
  val runtime_value : Value.t -> string
  val trace : trace -> string
end

(**{1 Constructors and conversions} *)

(**{2 Rounding}*)

val round : Q.t -> Z.t
(** This helper function rounds a rational to the nearest integer. Tie-breaker
    is the "half away from zero" rule: [0.5] is rounded to [1.0] and [-0.5] is
    rounded to [-1.0]. This function shall be used anytime rounding is
    necessary. *)

(**{2 Money}*)

val money_of_cents_string : string -> money
val money_of_units_int : int -> money

val money_of_decimal : decimal -> money
(** Warning: rounds to nearest cent, using {!val:round}. *)

val money_of_cents_integer : integer -> money
val money_to_float : money -> float
val money_to_string : money -> string
val money_to_cents : money -> integer

val money_round : money -> money
(** Rounds to the nearest currency unit using {!val:round}. *)

(** {2 Decimals} *)

val decimal_of_string : string -> decimal
val decimal_to_string : max_prec_digits:int -> decimal -> string
val decimal_of_integer : integer -> decimal
val decimal_of_float : float -> decimal
val decimal_to_float : decimal -> float

val decimal_round : decimal -> decimal
(** Rounds to the nearest integer using {!val:round}. *)

val decimal_of_money : money -> decimal

(**{2 Integers} *)

val integer_of_string : string -> integer
val integer_to_string : integer -> string
val integer_to_int : integer -> int
val integer_of_int : int -> integer
val integer_of_decimal : decimal -> integer
val integer_log2 : integer -> int
val integer_exponentiation : integer -> int -> integer

(**{2 Dates} *)

val day_of_month_of_date : date -> integer
val month_number_of_date : date -> integer
val is_leap_year : integer -> bool
val year_of_date : date -> integer
val date_to_string : date -> string

val date_of_numbers : int -> int -> int -> date
(** Usage: [date_of_numbers year month day].

    Raises Failure on invalid inputs *)

val first_day_of_month : date -> date
val last_day_of_month : date -> date
val date_to_years_months_days : date -> int * int * int

(**{2 Durations} *)

val duration_of_numbers : int -> int -> int -> duration
(** Usage : [duration_of_numbers year mounth day]. *)

val duration_to_years_months_days : duration -> int * int * int

(**{2 Times} *)

val duration_to_string : duration -> string

(**{1 Defaults} *)

val handle_exceptions :
  ('a * code_location) Optional.t array -> ('a * code_location) Optional.t
(** @raise Error Conflict *)

(**{1 Operators} *)

module Oper : sig
  (* The types **must** match with Shared_ast.Operator.*_type ; but for the
     added first argument [pos] for any operator that might trigger an error. *)
  val o_not : bool -> bool
  val o_length : 'a array -> integer
  val o_toint_rat : decimal -> integer
  val o_toint_mon : money -> integer
  val o_torat_int : integer -> decimal
  val o_torat_mon : money -> decimal
  val o_tomoney_rat : decimal -> money
  val o_tomoney_int : integer -> money
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
  val o_eq : 'a Value.ty -> code_location -> 'a -> 'a -> bool
  val o_lt : 'a Value.ty -> code_location -> 'a -> 'a -> bool
  val o_lte : 'a Value.ty -> code_location -> 'a -> 'a -> bool
  val o_gt : 'a Value.ty -> code_location -> 'a -> 'a -> bool
  val o_gte : 'a Value.ty -> code_location -> 'a -> 'a -> bool
  val o_map : ('a -> 'b) -> 'a array -> 'b array

  val o_map2 :
    code_location -> ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  (** @raise Runtime.NotSameLength *)

  val o_reduce : ('a -> 'a -> 'a) -> 'a array -> 'a Optional.t
  val o_concat : 'a array -> 'a array -> 'a array
  val o_filter : ('a -> bool) -> 'a array -> 'a array
  val o_add_int_int : integer -> integer -> integer
  val o_add_rat_rat : decimal -> decimal -> decimal
  val o_add_mon_mon : money -> money -> money
  val o_add_dat_dur : date_rounding -> code_location -> date -> duration -> date
  val o_add_dur_dur : duration -> duration -> duration
  val o_sub_int_int : integer -> integer -> integer
  val o_sub_rat_rat : decimal -> decimal -> decimal
  val o_sub_mon_mon : money -> money -> money
  val o_sub_dat_dat : date -> date -> duration
  val o_sub_dat_dur : date_rounding -> code_location -> date -> duration -> date
  val o_sub_dur_dur : duration -> duration -> duration
  val o_mult_int_int : integer -> integer -> integer
  val o_mult_rat_rat : decimal -> decimal -> decimal
  val o_mult_mon_int : money -> integer -> money
  val o_mult_mon_rat : money -> decimal -> money
  val o_mult_dur_int : duration -> integer -> duration
  val o_div_int_int : code_location -> integer -> integer -> decimal
  val o_div_rat_rat : code_location -> decimal -> decimal -> decimal
  val o_div_mon_mon : code_location -> money -> money -> decimal
  val o_div_mon_int : code_location -> money -> integer -> money
  val o_div_mon_rat : code_location -> money -> decimal -> money
  val o_div_dur_dur : code_location -> duration -> duration -> decimal
  val o_fold : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
  val o_find : ('a -> bool) -> 'a array -> 'a Optional.t

  val o_sort_asc :
    'b Value.ty -> code_location -> ('a -> 'b) -> 'a array -> 'a array

  val o_sort_desc :
    'b Value.ty -> code_location -> ('a -> 'b) -> 'a array -> 'a array

  val o_toclosureenv : 'a -> Obj.t
  val o_fromclosureenv : Obj.t -> 'a
end

include module type of Oper

(** Modules API *)

type hash = string

val register_module :
  string ->
  (string * Obj.t) list ->
  ?types:(string * (module CatalaType)) list ->
  hash ->
  unit
(** Registers a module by the given name defining the given bindings. Required
    for evaluation to be able to access the given values. The last argument is
    expected to be a hash of the source file and the Catala version, and will in
    time be used to ensure that the module and the interface are in sync *)

val check_module : string -> hash -> (unit, hash) result
(** Returns [Ok] if it has been registered with the correct hash, [Error h] if
    there is a hash mismatch.

    Raises Not_found if the module does not exist at all *)

val lookup_value : string * string -> Obj.t
val lookup_type : string * string -> (module CatalaType)
