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
type 'a eoption = ENone of unit | ESome of 'a

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

exception EmptyError
exception AssertionFailed
exception ConflictError
exception UncomparableDurations
exception IndivisableDurations
exception ImpossibleDate
exception NoValueProvided of source_position

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

let decimal_round (q : decimal) : decimal =
  (* Implements the workaround by
     https://gmplib.org/list-archives/gmp-discuss/2009-May/003767.html *)
  let n = Q.num q in
  let d = Q.den q in
  Q.of_bigint Z.(fdiv ((of_int 2 * n) + d) (of_int 2 * d))

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
    List.filter (fun (a, _) -> a <> 0) [x, "years"; y, "months"; z, "days"]
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

let yojson_of_money (m : money) = `Float (money_to_float m)
let yojson_of_integer (i : integer) = `Int (integer_to_int i)
let yojson_of_decimal (d : decimal) = `Float (decimal_to_float d)
let yojson_of_date (d : date) = `String (date_to_string d)
let yojson_of_duration (d : duration) = `String (duration_to_string d)

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
  | Array of runtime_value array
  | Unembeddable
[@@deriving yojson_of]

let unembeddable _ = Unembeddable
let embed_unit () = Unit
let embed_bool x = Bool x
let embed_money x = Money x
let embed_integer x = Integer x
let embed_decimal x = Decimal x
let embed_date x = Date x
let embed_duration x = Duration x
let embed_array f x = Array (Array.map f x)

type raw_event =
  | BeginCall of string list
  | EndCall of string list
  | VariableDefinition of string list * runtime_value
  | DecisionTaken of source_position

type event =
  | VarDef of var_def
  | VarDefWithFunCalls of var_def_with_fun_calls
  | FunCall of fun_call
  | SubScopeCall of {
      name : string list;
      inputs : var_def list;
      body : event list;
    }

and var_def = {
  pos : source_position option;
  name : string list;
  value : runtime_value;
}

and var_def_with_fun_calls = { var : var_def; fun_calls : fun_call list }

and fun_call = {
  fun_name : string list;
  input : var_def;
  body : event list;
  output : var_def_with_fun_calls;
}

let log_ref : raw_event list ref = ref []
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

module EventParser = struct
  module VarDefMap = struct
    module StringMap = Map.Make (String)

    type t = var_def list StringMap.t

    let add (name : string) (v : var_def) (map : t) : t =
      match StringMap.find_opt name map with
      | Some ls -> StringMap.add name (v :: ls) map
      | None -> StringMap.add name [v] map

    (** [get name map] return the list of definitions if there is a
        corresponding entry, otherwise, returns an empty array. *)
    let get (name : string) (map : t) : var_def list =
      match StringMap.find_opt name map with Some ls -> ls | None -> []

    let empty : t = StringMap.empty
  end

  type context = {
    (* Keeps tracks of the subscope input variable definitions. *)
    vars : VarDefMap.t;
    (* Current parsed events. *)
    events : event list;
  }

  (** TODO:

      - add error handling*)
  let parse_log (raw_events : raw_event list) : event list =
    let nb_raw_events = List.length raw_events in
    Printf.printf "Start parsing %d events\n" nb_raw_events;
    let is_function_call infos = 2 = List.length infos in
    let is_subscope_call infos = 3 = List.length infos in
    let is_var_def name = 2 = List.length name in
    let is_output_var_def name =
      3 = List.length name && "output" = List.nth name 2
    in
    let is_input_var_def name =
      3 = List.length name && "input" = List.nth name 2
    in
    let is_subscope_input_var_def name =
      3 = List.length name
      && not (is_output_var_def name || is_input_var_def name)
    in

    let rec parse_events ctx raw_events : raw_event list * event list =
      (try
         Printf.printf "[%d/%d] Parsing events: %s\n"
           (nb_raw_events - List.length raw_events + 1)
           nb_raw_events
           (List.hd raw_events |> raw_event_to_string)
       with Failure _ -> ());
      match raw_events with
      | [] -> [], ctx.events |> List.rev
      | VariableDefinition (name, value) :: rest when is_var_def name ->
        rest
        |> parse_events
             {
               ctx with
               events = VarDef { pos = None; name; value } :: ctx.events;
             }
      | DecisionTaken pos :: VariableDefinition (name, value) :: rest
        when is_var_def name || is_output_var_def name ->
        rest
        |> parse_events
             {
               ctx with
               events = VarDef { pos = Some pos; name; value } :: ctx.events;
             }
      | DecisionTaken pos :: VariableDefinition (name, value) :: rest
        when is_subscope_input_var_def name -> (
        match name with
        | [_; var_name; _] ->
          rest
          |> parse_events
               {
                 ctx with
                 vars =
                   ctx.vars
                   |> VarDefMap.add var_name { pos = Some pos; name; value };
               }
        | _ ->
          failwith
            ("Invalid subscope input variable definition: [ "
           ^ String.concat ", " name ^ " ]"))
      | DecisionTaken pos
        :: (VariableDefinition _ as fun_input_var_def) (* fun input *)
        :: BeginCall infos
        :: rest
        when is_function_call infos ->
        (* Variable definition with fun calls. *)
        let rec parse_fun_calls fun_calls raw_events =
          match raw_events with
          | VariableDefinition _ :: BeginCall infos :: _
            when is_function_call infos ->
            let rest, fun_call = parse_fun_call raw_events in
            rest |> parse_fun_calls (fun_call :: fun_calls)
          | rest -> rest, fun_calls |> List.rev
        in
        let rest, fun_calls, var =
          let rest, fun_calls =
            parse_fun_calls [] (fun_input_var_def :: BeginCall infos :: rest)
          in
          match rest with
          | VariableDefinition (name, value) :: rest ->
            rest, fun_calls, { pos = Some pos; name; value }
          | event :: _ ->
            failwith
              ("Invalid function call ([ " ^ String.concat ", " infos
             ^ " ]): expected variable definition (function ouput), found: "
             ^ raw_event_to_string event ^ "["
              ^ (nb_raw_events - List.length rest + 1 |> string_of_int)
              ^ "]")
          | [] -> failwith "empty log"
        in

        rest
        |> parse_events
             {
               ctx with
               events = VarDefWithFunCalls { var; fun_calls } :: ctx.events;
             }
      | VariableDefinition _ :: BeginCall infos :: _ when is_function_call infos
        ->
        let rest, fun_call = parse_fun_call raw_events in

        rest
        |> parse_events { ctx with events = FunCall fun_call :: ctx.events }
      | BeginCall infos :: rest when is_subscope_call infos -> (
        match infos with
        | [_; var_name; _] ->
          (* FIXME: should retrieve inputs from ctx.vars*)
          let inputs = VarDefMap.get var_name ctx.vars in
          (* NOTE: should use an empty context here? *)
          let rest, body = parse_events { ctx with events = [] } rest in
          rest
          |> parse_events
               {
                 ctx with
                 events =
                   SubScopeCall { name = infos; inputs; body } :: ctx.events;
               }
        | _ ->
          failwith
            ("Invalid subscope call name: [ " ^ String.concat ", " infos ^ " ]")
        )
      | EndCall infos :: rest ->
        Printf.printf "Find the endcall token of: %s\n"
          (String.concat ", " infos);
        rest, ctx.events |> List.rev
      | event :: event' :: _ ->
        failwith
          ("[EventParser error] invalid event: " ^ raw_event_to_string event
         ^ ", followed by: " ^ raw_event_to_string event')
      | _ -> failwith "empty log"
    and _parse_call infos ctx events =
      Printf.printf "[%d/%d] In parse_call of '%s', parsing events: %s\n"
        (nb_raw_events - List.length events)
        nb_raw_events (String.concat ", " infos)
        (List.hd events |> raw_event_to_string);
      match events with
      | EndCall infos' :: rest when infos = infos' ->
        (* NOTE: unreached case. *)
        rest, ctx.events |> List.rev
      | rest ->
        (* Printf.printf "Parsing body of the function call: %s\n" *)
        (*   (String.concat ", " infos); *)
        (* let rest, events = parse_events ctx rest in *)
        (* rest |> parse_call infos { ctx with events = events @ ctx.events } *)
        parse_events ctx rest
    and parse_fun_call events =
      (try
         Printf.printf "[%d/%d] In parse_fun_call, parsing events: %s\n"
           (nb_raw_events - List.length events)
           nb_raw_events
           (List.hd events |> raw_event_to_string)
       with Failure _ -> Printf.printf "Error in parse_fun_call");
      match events with
      | VariableDefinition (name, value) :: BeginCall infos :: rest
        when is_function_call infos && is_input_var_def name ->
        Printf.printf "Parsing function call of: %s\n"
          (String.concat ", " infos);
        let rest, body, output =
          let rest, body =
            parse_events { vars = VarDefMap.empty; events = [] } rest
          in
          let body_rev = List.rev body in
          rest, body_rev |> List.tl |> List.rev, body_rev |> List.hd
        in
        let output =
          match output with
          | VarDef var -> { var; fun_calls = [] }
          | VarDefWithFunCalls def -> def
          | _ -> failwith "[EventParser error]: invalid function call output"
        in

        ( rest,
          {
            fun_name = infos;
            input = { pos = None; name; value };
            body;
            output;
          } )
      | _ -> failwith "[EventParser error]: invalid function call"
    in
    List.iteri
      (fun i event ->
        Printf.printf "* [%d/%d] Parsing event: %s\n" (i + 1) nb_raw_events
          (raw_event_to_string event))
      raw_events;
    let _, events =
      parse_events { vars = VarDefMap.empty; events = [] } raw_events
    in
    events
end

let handle_default :
      'a. (unit -> 'a) array -> (unit -> bool) -> (unit -> 'a) -> 'a =
 fun exceptions just cons ->
  let except =
    Array.fold_left
      (fun acc except ->
        let new_val = try Some (except ()) with EmptyError -> None in
        match acc, new_val with
        | None, _ -> new_val
        | Some _, None -> acc
        | Some _, Some _ -> raise ConflictError)
      None exceptions
  in
  match except with
  | Some x -> x
  | None -> if just () then cons () else raise EmptyError

let handle_default_opt
    (exceptions : 'a eoption array)
    (just : bool eoption)
    (cons : 'a eoption) : 'a eoption =
  let except =
    Array.fold_left
      (fun acc except ->
        match acc, except with
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
  let i1_abs = Z.abs i1 in
  let i2_abs = Q.abs i2 in
  let sign_int = Z.sign i1 * Q.sign i2 in
  let rat_result = Q.mul (Q.of_bigint i1_abs) i2_abs in
  let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
  (* we perform nearest rounding when multiplying an amount of money by a
     decimal !*)
  if Z.(of_int 2 * remainder >= Q.den rat_result) then
    Z.(add res (of_int 1) * of_int sign_int)
  else Z.(res * of_int sign_int)

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

let ( *^ ) (d1 : duration) (i1 : integer) : duration =
  let y, m, d = CalendarLib.Date.Period.ymd d1 in
  CalendarLib.Date.Period.make
    (y * integer_to_int i1)
    (m * integer_to_int i1)
    (d * integer_to_int i1)

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
    (p1 : CalendarLib.Date.Period.t)
    (p2 : CalendarLib.Date.Period.t) : int =
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
