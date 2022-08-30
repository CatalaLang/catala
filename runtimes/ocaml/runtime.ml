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

(* An integer number of cents *)
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
[@@deriving yojson_of]

exception EmptyError
exception AssertionFailed of source_position
exception ConflictError of source_position
exception UncomparableDurations
exception IndivisableDurations
exception ImpossibleDate
exception NoValueProvided of source_position

let money_of_cents_string (cents : string) : money = Z.of_string cents
let money_of_units_int (units : int) : money = Z.(of_int units * of_int 100)
let money_of_cents_integer (cents : integer) : money = cents
let money_to_float (m : money) : float = Z.to_float m /. 100.

let money_of_decimal (d : decimal) : money =
  Q.to_bigint (Q.mul d (Q.of_int 100))

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

let decimal_of_money (m : money) : decimal =
  Q.div (Q.of_bigint m) (Q.of_int 100)

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

let first_day_of_month (d : date) : date =
  date_of_numbers (CalendarLib.Date.year d)
    (CalendarLib.Date.int_of_month (CalendarLib.Date.month d))
    1

let last_day_of_month (d : date) : date =
  date_of_numbers (CalendarLib.Date.year d)
    (CalendarLib.Date.int_of_month (CalendarLib.Date.month d))
    (CalendarLib.Date.days_in_month d)

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

type information = string list [@@deriving yojson_of]

type raw_event =
  | BeginCall of information
  | EndCall of information
  | VariableDefinition of information * runtime_value
  | DecisionTaken of source_position

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

let log_ref : raw_event list ref = ref []
let reset_log () = log_ref := []
let retrieve_log () = List.rev !log_ref

let log_begin_call info f =
  log_ref := BeginCall info :: !log_ref;
  f

let log_end_call info x =
  log_ref := EndCall info :: !log_ref;
  x

let log_variable_definition (info : string list) embed (x : 'a) =
  log_ref := VariableDefinition (info, embed x) :: !log_ref;
  x

let log_decision_taken pos x =
  if x then log_ref := DecisionTaken pos :: !log_ref;
  x

let rec pp_events ?(is_first_call = true) ppf events =
  let rec format_var_def ppf var =
    Format.fprintf ppf "@[<hov 2><var_def at %a>@ %s:@ %a@]" format_pos_opt
      var.pos
      (String.concat "." var.name)
      format_value var.value
  and format_pos_opt ppf = function
    | None -> Format.fprintf ppf "no_pos"
    | Some pos ->
      Format.fprintf ppf "%s line %d to %d" pos.filename pos.start_line
        pos.end_line
  and format_var_defs ppf =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
      format_var_def ppf
  and format_var_def_with_fun_calls ppf var_with_fun =
    match var_with_fun.fun_calls with
    | None | Some [] -> format_var_def ppf var_with_fun
    | Some fun_calls ->
      Format.fprintf ppf
        "@[<hov 2><var_def_with_fun>@ %s: %a@ computed from@ :@ @[<hv 2>[@ %a@;\
         <1 -2>]@] @]"
        (String.concat "." var_with_fun.name)
        format_value var_with_fun.value
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           (fun ppf fun_call -> format_event ppf (FunCall fun_call)))
        fun_calls
  and format_value ppf = function
    | Unembeddable -> Format.fprintf ppf "fun"
    | Unit -> Format.fprintf ppf "()"
    | Bool x -> Format.fprintf ppf "%b" x
    | Money x -> Format.fprintf ppf "%s€" (money_to_string x)
    | Integer x -> Format.fprintf ppf "%d" (integer_to_int x)
    | Decimal x ->
      Format.fprintf ppf "%s" (decimal_to_string ~max_prec_digits:10 x)
    | Date x -> Format.fprintf ppf "%s" (date_to_string x)
    | Duration x -> Format.fprintf ppf "%s" (duration_to_string x)
    | Enum (_, (name, _)) -> Format.fprintf ppf "%s" name
    | Struct (name, attrs) ->
      Format.fprintf ppf "@[<hv 2>%s = {@ %a@;<1 -2>}@]"
        (String.concat "." name)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,")
           (fun fmt (name, value) ->
             Format.fprintf fmt "%s: %a" name format_value value))
        attrs
    | Array elts ->
      Format.fprintf ppf "@[<hv 2>[@ %a@;<1 -2>]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           format_value)
        (elts |> Array.to_list)
  and format_event ppf = function
    | VarComputation var_def_with_fun
      when Option.is_some var_def_with_fun.fun_calls ->
      Format.fprintf ppf "%a" format_var_def_with_fun_calls var_def_with_fun
    | VarComputation var_def -> Format.fprintf ppf "%a" format_var_def var_def
    | FunCall { fun_name; input; body; output } ->
      Format.fprintf ppf
        "@[<hov 1><function_call>@ %s :=@ {@[<hv 1>@ input:@ %a,@ output:@ \
         %a,@ body:@ [@,\
         %a]@]@,\
         @]@,\
         }"
        (String.concat "." fun_name)
        format_var_def input format_var_def_with_fun_calls output
        (pp_events ~is_first_call:false)
        body
    | SubScopeCall { name; inputs; body } ->
      Format.fprintf ppf
        "@[<hv 2><subscope_call>@ %s :=@ {@[<hv 1>@,\
         inputs:@ @[<hv 2>[@,\
         %a@]],@,\
         body:@ @[<hv 2>[@ %a@ ]@]@]@]@,\
         }"
        (String.concat "." name) format_var_defs inputs
        (pp_events ~is_first_call:false)
        body
  in
  Format.fprintf ppf
    ("@[<hv 1>%a@]" ^^ if is_first_call then "@." else "")
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
       format_event)
    events

module EventParser = struct
  module VarDefMap = struct
    module StringMap = Map.Make (String)

    type t = var_def list StringMap.t

    let add (name : string) (v : var_def) (map : t) : t =
      match StringMap.find_opt name map with
      | Some ls -> StringMap.add name (v :: ls) map
      | None -> StringMap.add name [v] map

    (** [get name map] returns the list of definitions if there is a
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
    rest : raw_event list;
  }

  let empty_ctx = { vars = VarDefMap.empty; events = []; rest = [] }

  let raw_event_to_string = function
    | BeginCall name ->
      Printf.sprintf "BeginCall([ " ^ String.concat ", " name ^ " ])"
    | EndCall name ->
      Printf.sprintf "EndCall([ " ^ String.concat ", " name ^ " ])"
    | VariableDefinition (name, value) ->
      Printf.sprintf "VariableDefinition([ %s ], %s)" (String.concat ", " name)
        (yojson_of_runtime_value value |> Yojson.Safe.to_string)
    | DecisionTaken _ -> Printf.sprintf "DecisionTaken(_)"

  let parse_raw_events raw_events =
    let nb_raw_events = List.length raw_events
    and is_function_call infos = 2 = List.length infos
    and is_subscope_call infos = 3 = List.length infos
    and is_var_def name = 2 = List.length name
    and is_output_var_def name =
      3 = List.length name && "output" = List.nth name 2
    and is_input_var_def name =
      3 = List.length name && "input" = List.nth name 2
    and is_subscope_input_var_def name =
      2 = List.length name && String.contains (List.nth name 1) '.'
    in

    let rec parse_events (ctx : context) : context =
      match ctx.rest with
      | [] -> { ctx with events = ctx.events |> List.rev }
      | VariableDefinition (name, _) :: rest when is_var_def name ->
        (* VariableDefinition without position corresponds to a function
           definition which are ignored for now in structured events. *)
        parse_events { ctx with rest }
      | DecisionTaken pos :: VariableDefinition (name, value) :: rest
        when is_subscope_input_var_def name -> (
        match name with
        | [_; var_dot_subscope_var_name] ->
          let var_name =
            List.nth (String.split_on_char '.' var_dot_subscope_var_name) 0
          in
          parse_events
            {
              ctx with
              vars =
                ctx.vars
                |> VarDefMap.add var_name
                     { pos = Some pos; name; value; fun_calls = None };
              rest;
            }
        | _ ->
          failwith "unreachable due to the [is_subscope_input_var_def] test")
      | DecisionTaken pos :: VariableDefinition (name, value) :: rest
        when is_var_def name || is_output_var_def name ->
        parse_events
          {
            ctx with
            events =
              VarComputation { pos = Some pos; name; value; fun_calls = None }
              :: ctx.events;
            rest;
          }
      | DecisionTaken pos :: VariableDefinition _ :: BeginCall infos :: _
        when is_function_call infos ->
        (* Variable definition with function calls. *)
        let rec parse_fun_calls fun_calls raw_events =
          match raw_events with
          | VariableDefinition _ :: BeginCall infos :: _
            when is_function_call infos ->
            let rest, fun_call = parse_fun_call raw_events in
            parse_fun_calls (fun_call :: fun_calls) rest
          | rest -> rest, fun_calls |> List.rev
        in
        let rest, var_comp =
          let rest, fun_calls = parse_fun_calls [] (List.tl ctx.rest) in
          match rest with
          | VariableDefinition (name, value) :: rest ->
            ( rest,
              VarComputation
                { pos = Some pos; name; value; fun_calls = Some fun_calls } )
          | event :: _ ->
            failwith
              ("Invalid function call ([ "
              ^ String.concat ", " infos
              ^ " ]): expected variable definition (function output), found: "
              ^ raw_event_to_string event
              ^ "["
              ^ (nb_raw_events - List.length rest + 1 |> string_of_int)
              ^ "]")
          | [] ->
            failwith
              ("Invalid function call ([ "
              ^ String.concat ", " infos
              ^ " ]): expected variable definition (function output), found: \
                 end of tokens")
        in

        parse_events { ctx with events = var_comp :: ctx.events; rest }
      | VariableDefinition _ :: BeginCall infos :: _ when is_function_call infos
        ->
        let rest, fun_call = parse_fun_call ctx.rest in

        parse_events { ctx with events = FunCall fun_call :: ctx.events; rest }
      | BeginCall infos :: rest when is_subscope_call infos -> (
        match infos with
        | [_; var_name; _] ->
          let body_ctx = parse_events { empty_ctx with rest } in
          let inputs = VarDefMap.get var_name ctx.vars in
          parse_events
            {
              ctx with
              events =
                SubScopeCall { name = infos; inputs; body = body_ctx.events }
                :: ctx.events;
              rest = body_ctx.rest;
            }
        | _ -> failwith "unreachable due to the [is_subscope_call] test")
      | EndCall _ :: rest -> { ctx with events = ctx.events |> List.rev; rest }
      | event :: _ -> failwith ("Unexpected event: " ^ raw_event_to_string event)
    and parse_fun_call events =
      match events with
      | VariableDefinition (name, value) :: BeginCall infos :: rest
        when is_function_call infos && is_input_var_def name ->
        let rest, body, output =
          let body_ctx =
            parse_events { vars = VarDefMap.empty; events = []; rest }
          in
          let body_rev = List.rev body_ctx.events in
          body_ctx.rest, body_rev |> List.tl |> List.rev, body_rev |> List.hd
        in
        let output =
          match output with
          | VarComputation var_def -> var_def
          | _ -> failwith "Missing function output variable definition."
        in

        ( rest,
          {
            fun_name = infos;
            input = { pos = None; name; value; fun_calls = None };
            body;
            output;
          } )
      | _ -> failwith "Invalid start of function call."
    in

    let ctx =
      try parse_events { empty_ctx with rest = raw_events }
      with Failure msg ->
        (* TODO: discuss what should be done. *)
        Printf.eprintf "An error occurred while parsing raw events: %s\n" msg;
        empty_ctx
    in
    ctx.events
end

let handle_default :
      'a.
      source_position ->
      (unit -> 'a) array ->
      (unit -> bool) ->
      (unit -> 'a) ->
      'a =
 fun pos exceptions just cons ->
  let except =
    Array.fold_left
      (fun acc except ->
        let new_val = try Some (except ()) with EmptyError -> None in
        match acc, new_val with
        | None, _ -> new_val
        | Some _, None -> acc
        | Some _, Some _ -> raise (ConflictError pos))
      None exceptions
  in
  match except with
  | Some x -> x
  | None -> if just () then cons () else raise EmptyError

let handle_default_opt
    (pos : source_position)
    (exceptions : 'a eoption array)
    (just : bool eoption)
    (cons : 'a eoption) : 'a eoption =
  let except =
    Array.fold_left
      (fun acc except ->
        match acc, except with
        | ENone _, _ -> except
        | ESome _, ENone _ -> acc
        | ESome _, ESome _ -> raise (ConflictError pos))
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
