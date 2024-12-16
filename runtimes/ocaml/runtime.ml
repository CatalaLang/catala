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
type nonrec unit = unit
type nonrec bool = bool

(* An integer number of cents *)
type money = Z.t
type integer = Z.t
type decimal = Q.t
type date = Dates_calc.Dates.date

type date_rounding = Dates_calc.Dates.date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound

type duration = Dates_calc.Dates.period

module Eoption = struct
  type 'a t = ENone of unit | ESome of 'a
end

type io_input = NoInput | OnlyInput | Reentrant
type io_log = { io_input : io_input; io_output : bool }

type source_position = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

type error =
  | AssertionFailed
  | NoValue
  | Conflict
  | DivisionByZero
  | NotSameLength
  | UncomparableDurations
  | AmbiguousDateRounding
  | IndivisibleDurations

let error_to_string = function
  | AssertionFailed -> "AssertionFailed"
  | NoValue -> "NoValue"
  | Conflict -> "Conflict"
  | DivisionByZero -> "DivisionByZero"
  | NotSameLength -> "NotSameLength"
  | UncomparableDurations -> "UncomparableDurations"
  | AmbiguousDateRounding -> "AmbiguousDateRounding"
  | IndivisibleDurations -> "IndivisibleDurations"

let error_message = function
  | AssertionFailed -> "an assertion doesn't hold"
  | NoValue -> "no applicable rule to define this variable in this situation"
  | Conflict ->
    "conflict between multiple valid consequences for assigning the same \
     variable"
  | DivisionByZero ->
    "a value is being used as denominator in a division and it computed to zero"
  | NotSameLength -> "traversing multiple lists of different lengths"
  | UncomparableDurations ->
    "ambiguous comparison between durations in different units (e.g. months \
     vs. days)"
  | AmbiguousDateRounding ->
    "ambiguous date computation, and rounding mode was not specified"
  | IndivisibleDurations -> "dividing durations that are not in days"

exception Error of error * source_position list
exception Empty

let error err pos = raise (Error (err, pos))

(* Register (fallback) exception printers *)
let () =
  let ppos () p =
    Printf.sprintf "%s:%d.%d-%d.%d" p.filename p.start_line p.start_column
      p.end_line p.end_column
  in
  let pposl () pl = String.concat ", " (List.map (ppos ()) pl) in
  Printexc.register_printer
  @@ function
  | Error (err, pos) ->
    Some (Printf.sprintf "At %a: %s" pposl pos (error_message err))
  | _ -> None

let () =
  Printexc.set_uncaught_exception_handler
  @@ fun exc bt ->
  Printf.eprintf "[ERROR] %s\n%!" (Printexc.to_string exc);
  if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt
(* TODO: the backtrace will point to the OCaml code; but we could make it point
   to the Catala code if we add #line directives everywhere in the generated
   code. *)

let round (q : Q.t) : Z.t =
  (* The mathematical formula is [round(q) = sgn(q) * floor(abs(q) + 0.5)].
     However, Zarith's [Q.to_bigint] does not floor. Instead, it rounds towards
     0 (that is, [-0.1] is rounded to [0]). We work around this by using
     [Z.fdiv], integer division with rounding towards [-inf], and implementing
     the trick from
     https://gmplib.org/list-archives/gmp-discuss/2009-May/003767.html *)
  let sgn = Q.sign q in
  let abs = Q.abs q in
  let n = Q.num abs in
  let d = Q.den abs in
  let abs_round = Z.(fdiv ((of_int 2 * n) + d) (of_int 2 * d)) in
  Z.(of_int sgn * abs_round)

let money_of_cents_string (cents : string) : money = Z.of_string cents
let money_of_units_int (units : int) : money = Z.(of_int units * of_int 100)
let money_of_cents_integer (cents : integer) : money = cents
let money_to_float (m : money) : float = Z.to_float m /. 100.

let money_of_decimal (d : decimal) : money =
  (* Turn units to cents then round to nearest cent *)
  round Q.(d * of_int 100)

let money_to_string (m : money) : string =
  Format.asprintf "%.2f" Q.(to_float (of_bigint m / of_int 100))

let money_to_cents m = m

let money_round (m : money) : money =
  (* Turn cents to units then round to nearest unit, and convert back *)
  let units = Q.(of_bigint m / of_int 100) in
  Z.(round units * of_int 100)

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

let decimal_round (q : decimal) : decimal = Q.of_bigint (round q)

let decimal_of_money (m : money) : decimal =
  Q.div (Q.of_bigint m) (Q.of_int 100)

let integer_of_string (s : string) : integer = Z.of_string s
let integer_to_string (i : integer) : string = Z.to_string i
let integer_to_int (i : integer) : int = Z.to_int i
let integer_of_int (i : int) : integer = Z.of_int i
let integer_of_decimal (d : decimal) : integer = Q.to_bigint d
let integer_exponentiation (i : integer) (e : int) : integer = Z.pow i e
let integer_log2 = Z.log2

let year_of_date (d : date) : integer =
  let y, _, _ = Dates_calc.Dates.date_to_ymd d in
  Z.of_int y

let month_number_of_date (d : date) : integer =
  let _, m, _ = Dates_calc.Dates.date_to_ymd d in
  Z.of_int m

let is_leap_year (y : integer) =
  let y = Z.to_int y in
  Dates_calc.Dates.is_leap_year y

let day_of_month_of_date (d : date) : integer =
  let _, _, d = Dates_calc.Dates.date_to_ymd d in
  Z.of_int d

(* This could fail, but is expected to only be called with known, already
   validated arguments by the generated code *)
let date_of_numbers (year : int) (month : int) (day : int) : date =
  try Dates_calc.Dates.make_date ~year ~month ~day
  with Dates_calc.Dates.InvalidDate ->
    failwith "date_of_numbers: invalid date"

let date_to_string (d : date) : string =
  Format.asprintf "%a" Dates_calc.Dates.format_date d

let date_to_years_months_days (d : date) : int * int * int =
  Dates_calc.Dates.date_to_ymd d

let first_day_of_month = Dates_calc.Dates.first_day_of_month
let last_day_of_month = Dates_calc.Dates.last_day_of_month

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  Dates_calc.Dates.make_period ~years:year ~months:month ~days:day

let duration_to_string (d : duration) : string =
  Format.asprintf "%a" Dates_calc.Dates.format_period d

let duration_to_years_months_days (d : duration) : int * int * int =
  Dates_calc.Dates.period_to_ymds d

type runtime_value =
  | Unit
  | Bool of bool
  | Money of money
  | Integer of integer
  | Decimal of decimal
  | Date of date
  | Duration of duration
  | Enum of string * (string * runtime_value)
  | Struct of string * (string * runtime_value) list
  | Array of runtime_value array
  | Tuple of runtime_value array
  | Unembeddable

let unembeddable _ = Unembeddable
let embed_unit () = Unit
let embed_bool x = Bool x
let embed_money x = Money x
let embed_integer x = Integer x
let embed_decimal x = Decimal x
let embed_date x = Date x
let embed_duration x = Duration x
let embed_array f x = Array (Array.map f x)

type information = string list

type raw_event =
  | BeginCall of information
  | EndCall of information
  | VariableDefinition of information * io_log * runtime_value
  | DecisionTaken of source_position

type event =
  | VarComputation of var_def
  | FunCall of fun_call
  | SubScopeCall of {
      name : information;
      inputs : var_def list;
      body : event list;
    }

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

module BufferedJson = struct
  let rec list f buf = function
    | [] -> ()
    | [x] -> f buf x
    | x :: r ->
      f buf x;
      Buffer.add_char buf ',';
      list f buf r

  let quote buf str =
    Buffer.add_char buf '"';
    String.iter
      (function
        | ('"' | '\\') as c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\x00' .. '\x1F' as c -> Printf.bprintf buf "\\u%04x" (int_of_char c)
        | c -> Buffer.add_char buf c)
      str;
    Buffer.add_char buf '"'

  (* Note: the output format is made for transition with what Yojson gave us,
     but we could change it to something nicer (e.g. objects for structures) *)
  let rec runtime_value buf = function
    | Unit -> Buffer.add_string buf {|"Unit"|}
    | Bool b -> Buffer.add_string buf (string_of_bool b)
    | Money m -> Buffer.add_string buf (money_to_string m)
    | Integer i -> Buffer.add_string buf (integer_to_string i)
    | Decimal d ->
      Buffer.add_string buf (decimal_to_string ~max_prec_digits:10 d)
    | Date d -> quote buf (date_to_string d)
    | Duration d -> quote buf (duration_to_string d)
    | Enum (name, (constr, v)) ->
      Printf.bprintf buf {|[["%s"],["%s",%a]]|} name constr runtime_value v
    | Struct (name, elts) ->
      Printf.bprintf buf {|["%s",[%a]]|} name
        (list (fun buf (cstr, v) ->
             Printf.bprintf buf {|"%s":%a|} cstr runtime_value v))
        elts
    | Array elts | Tuple elts ->
      Printf.bprintf buf "[%a]" (list runtime_value) (Array.to_list elts)
    | Unembeddable -> Buffer.add_string buf {|"unembeddable"|}

  let information buf info = Printf.bprintf buf "[%a]" (list quote) info

  let source_position buf pos =
    Printf.bprintf buf {|{"filename":%a|} quote pos.filename;
    Printf.bprintf buf {|,"start_line":%d|} pos.start_line;
    Printf.bprintf buf {|,"start_column":%d|} pos.start_column;
    Printf.bprintf buf {|,"end_line":%d|} pos.end_line;
    Printf.bprintf buf {|,"end_column":%d|} pos.end_column;
    Printf.bprintf buf {|,"law_headings":[%a]}|} (list quote) pos.law_headings

  let io_input buf = function
    | NoInput -> quote buf "NoInput"
    | OnlyInput -> quote buf "OnlyInput"
    | Reentrant -> quote buf "Reentrant"

  let io_log buf iol =
    Printf.bprintf buf {|{"io_input":%a|} io_input iol.io_input;
    Printf.bprintf buf {|,"io_output":%b}|} iol.io_output

  let rec event buf = function
    | VarComputation vd ->
      Printf.bprintf buf {|"VarComputation",%a]|} var_def vd
    | FunCall fc -> Printf.bprintf buf {|"FunCall",%a]|} fun_call fc
    | SubScopeCall { name; inputs; body } ->
      Printf.bprintf buf {|{"name":%a,"inputs":[%a],"body":[%a]}|} information
        name (list var_def) inputs (list event) body

  and var_def buf def =
    Option.iter (Printf.bprintf buf {|{"pos":%a|} source_position) def.pos;
    Printf.bprintf buf {|,"name":%a|} information def.name;
    Printf.bprintf buf {|,"io":%a|} io_log def.io;
    Printf.bprintf buf {|,"value":%a|} runtime_value def.value;
    Option.iter
      (Printf.bprintf buf {|,"fun_calls":[%a]}|} (list fun_call))
      def.fun_calls

  and fun_call buf fc =
    Printf.bprintf buf {|{"fun_name":%a|} information fc.fun_name;
    Printf.bprintf buf {|,"fun_inputs":[%a]|} (list var_def) fc.fun_inputs;
    Printf.bprintf buf {|,"body":[%a]|} (list event) fc.body;
    Printf.bprintf buf {|,"output":%a}|} var_def fc.output

  and raw_event buf = function
    | BeginCall name ->
      Printf.bprintf buf {|{"event": "BeginCall", "name": "%s"}|}
        (String.concat "." name)
    | EndCall name ->
      Printf.bprintf buf {|{"event": "EndCall", "name": "%s"}|}
        (String.concat "." name)
    | VariableDefinition (name, io, value) ->
      Printf.bprintf buf
        {|{
         "event": "VariableDefinition",
         "name": "%s",
         "io": %a,
         "value": "%a"
         }|}
        (String.concat "." name) io_log io runtime_value value
    | DecisionTaken _dectaken -> Printf.bprintf buf {|DecisionTaken|}
end

module Json = struct
  let str f x =
    let buf = Buffer.create 800 in
    f buf x;
    Buffer.contents buf

  open BufferedJson

  let runtime_value = str runtime_value
  let io_log = str io_log
  let event = str event
  let raw_event = str raw_event
end

let log_ref : raw_event list ref = ref []
let reset_log () = log_ref := []
let retrieve_log () = List.rev !log_ref

let log_begin_call info f =
  log_ref := BeginCall info :: !log_ref;
  f

let log_end_call info x =
  log_ref := EndCall info :: !log_ref;
  x

let log_variable_definition (info : string list) (io : io_log) embed (x : 'a) =
  log_ref := VariableDefinition (info, io, embed x) :: !log_ref;
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
      Format.fprintf ppf "@[<hv 2>%s = {@ %a@;<1 -2>}@]" name
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
    | Tuple elts ->
      Format.fprintf ppf "@[<hv 2>(@ %a@;<1 -2>)@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           format_value)
        (elts |> Array.to_list)
  and format_event ppf = function
    | VarComputation var_def_with_fun
      when Option.is_some var_def_with_fun.fun_calls ->
      Format.fprintf ppf "%a" format_var_def_with_fun_calls var_def_with_fun
    | VarComputation var_def -> Format.fprintf ppf "%a" format_var_def var_def
    | FunCall { fun_name; fun_inputs; body; output } ->
      Format.fprintf ppf
        "@[<hov 1><function_call>@ %s :=@ {@[<hv 1>@ input:@ %a,@ output:@ \
         %a,@ body:@ [@,\
         %a]@]@,\
         @]@,\
         }"
        (String.concat "." fun_name)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           format_var_def)
        fun_inputs format_var_def_with_fun_calls output
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

  let io_log_to_string (io : io_log) : string =
    match io.io_input, io.io_output with
    | NoInput, false -> "internal"
    | _ ->
      Printf.sprintf "%s%s%s"
        (match io.io_input with
        | NoInput -> ""
        | OnlyInput -> "input"
        | Reentrant -> "reentrant")
        (match io.io_input, io.io_output with
        | (OnlyInput | Reentrant), true -> "/"
        | _ -> "")
        (if io.io_output then "output" else "")

  let raw_event_to_string = function
    | BeginCall name ->
      Printf.sprintf "BeginCall([ " ^ String.concat ", " name ^ " ])"
    | EndCall name ->
      Printf.sprintf "EndCall([ " ^ String.concat ", " name ^ " ])"
    | VariableDefinition (name, io, value) ->
      Printf.sprintf "VariableDefinition([ %s ], %s, %s)"
        (String.concat ", " name) (io_log_to_string io)
        (Json.runtime_value value)
    | DecisionTaken pos ->
      Printf.sprintf "DecisionTaken(%s:%d.%d-%d.%d)" pos.filename pos.start_line
        pos.start_column pos.end_line pos.end_column

  (** [takewhile p xs] split the list [xs] as the longest prefix of the list
      [xs] where every element [x] satisfies [p x] and the rest. *)
  let rec take_while (p : 'a -> bool) (l : 'a list) : 'a list * 'a list =
    match l with
    | [] -> [], []
    | h :: t when p h ->
      let t, rest = take_while p t in
      h :: t, rest
    | _ -> [], l

  let parse_raw_events raw_events =
    let nb_raw_events = List.length raw_events
    and is_function_call infos = 2 = List.length infos
    and is_subscope_call infos = 3 = List.length infos
    and is_var_def name = 2 = List.length name
    and is_output_var_def name =
      3 = List.length name && "output" = List.nth name 2
    and is_input_var_def name =
      3 = List.length name
      && String.starts_with ~prefix:"input" (List.nth name 2)
    and is_subscope_input_var_def name =
      2 = List.length name && String.contains (List.nth name 1) '.'
    in
    let rec parse_events (ctx : context) : context =
      match ctx.rest with
      | [] -> { ctx with events = ctx.events |> List.rev }
      | VariableDefinition (name, _, _) :: rest when is_var_def name ->
        (* VariableDefinition without position corresponds to a function
           definition which are ignored for now in structured events. *)
        parse_events { ctx with rest }
      | DecisionTaken pos :: VariableDefinition (name, io, value) :: rest
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
                     { pos = Some pos; name; value; fun_calls = None; io };
              rest;
            }
        | _ ->
          failwith "unreachable due to the [is_subscope_input_var_def] test")
      | DecisionTaken pos :: VariableDefinition (name, io, value) :: rest
        when is_var_def name || is_output_var_def name ->
        parse_events
          {
            ctx with
            events =
              VarComputation
                { pos = Some pos; name; value; fun_calls = None; io }
              :: ctx.events;
            rest;
          }
      | DecisionTaken pos :: VariableDefinition _ :: BeginCall infos :: _
        when is_function_call infos ->
        (* Variable definition with function calls. *)
        let rec parse_fun_calls fun_calls raw_events =
          match
            take_while
              (function VariableDefinition _ -> true | _ -> false)
              raw_events
          with
          | _, BeginCall infos :: _ when is_function_call infos ->
            let rest, fun_call = parse_fun_call raw_events in
            parse_fun_calls (fun_call :: fun_calls) rest
          | _ -> raw_events, fun_calls |> List.rev
        in
        let rest, var_comp =
          let rest, fun_calls = parse_fun_calls [] (List.tl ctx.rest) in
          match rest with
          | VariableDefinition (name, io, value) :: rest ->
            ( rest,
              VarComputation
                { pos = Some pos; name; value; fun_calls = Some fun_calls; io }
            )
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
      match
        take_while
          (function
            | VariableDefinition (name, _, _) -> is_input_var_def name
            | _ -> false)
          events
      with
      | inputs, BeginCall infos :: rest when is_function_call infos ->
        let fun_inputs =
          ListLabels.map inputs ~f:(function
            | VariableDefinition (name, io, value) ->
              { pos = None; name; value; fun_calls = None; io }
            | _ -> assert false)
        in
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

        rest, { fun_name = infos; fun_inputs; body; output }
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

let handle_exceptions
    (pos : source_position array)
    (exceptions : 'a Eoption.t array) : 'a Eoption.t =
  let len = Array.length exceptions in
  let rec filt_except i =
    if i < len then
      match exceptions.(i) with
      | Eoption.ESome _ as new_val -> (new_val, i) :: filt_except (i + 1)
      | Eoption.ENone () -> filt_except (i + 1)
    else []
  in
  match filt_except 0 with
  | [] -> Eoption.ENone ()
  | [(res, _)] -> res
  | res -> error Conflict (List.map (fun (_, i) -> pos.(i)) res)

(* TODO: add a compare built-in to dates_calc. At the moment this fails on e.g.
   [3 months, 4 months] *)
let compare_periods pos (p1 : duration) (p2 : duration) : int =
  try
    let p1_days = Dates_calc.Dates.period_to_days p1 in
    let p2_days = Dates_calc.Dates.period_to_days p2 in
    compare p1_days p2_days
  with Dates_calc.Dates.AmbiguousComputation ->
    error UncomparableDurations [pos]

(* TODO: same here, although it was tweaked to never fail on equal dates.
   Comparing the difference to duration_0 is not a good idea because we still
   want to fail on [1 month, 30 days] rather than return [false] *)
let equal_periods pos (p1 : duration) (p2 : duration) : bool =
  Dates_calc.Dates.period_to_ymds p1 = Dates_calc.Dates.period_to_ymds p2
  ||
  try Dates_calc.Dates.period_to_days (Dates_calc.Dates.sub_periods p1 p2) = 0
  with Dates_calc.Dates.AmbiguousComputation ->
    error UncomparableDurations [pos]

module Oper = struct
  let o_not = Stdlib.not
  let o_length a = Z.of_int (Array.length a)
  let o_toint_rat = integer_of_decimal
  let o_torat_int = decimal_of_integer
  let o_torat_mon = decimal_of_money
  let o_tomoney_rat = money_of_decimal
  let o_getDay = day_of_month_of_date
  let o_getMonth = month_number_of_date
  let o_getYear = year_of_date
  let o_firstDayOfMonth = first_day_of_month
  let o_lastDayOfMonth = last_day_of_month
  let o_round_mon = money_round
  let o_round_rat = decimal_round
  let o_minus_int i1 = Z.sub Z.zero i1
  let o_minus_rat i1 = Q.sub Q.zero i1
  let o_minus_mon m1 = Z.sub Z.zero m1
  let o_minus_dur = Dates_calc.Dates.neg_period
  let o_and = ( && )
  let o_or = ( || )
  let o_xor : bool -> bool -> bool = ( <> )
  let o_eq = ( = )
  let o_map = Array.map

  let o_map2 pos f a b =
    try Array.map2 f a b with Invalid_argument _ -> error NotSameLength [pos]

  let o_reduce f dft a =
    let len = Array.length a in
    if len = 0 then dft
    else
      let r = ref a.(0) in
      for i = 1 to len - 1 do
        r := f !r a.(i)
      done;
      !r

  let o_concat = Array.append
  let o_filter f a = Array.of_list (List.filter f (Array.to_list a))
  let o_add_int_int i1 i2 = Z.add i1 i2
  let o_add_rat_rat i1 i2 = Q.add i1 i2
  let o_add_mon_mon m1 m2 = Z.add m1 m2

  let o_add_dat_dur r pos da du =
    try Dates_calc.Dates.add_dates ~round:r da du
    with Dates_calc.Dates.AmbiguousComputation ->
      error AmbiguousDateRounding [pos]

  let o_add_dur_dur = Dates_calc.Dates.add_periods
  let o_sub_int_int i1 i2 = Z.sub i1 i2
  let o_sub_rat_rat i1 i2 = Q.sub i1 i2
  let o_sub_mon_mon m1 m2 = Z.sub m1 m2
  let o_sub_dat_dat = Dates_calc.Dates.sub_dates

  let o_sub_dat_dur r pos dat dur =
    o_add_dat_dur r pos dat (Dates_calc.Dates.neg_period dur)

  let o_sub_dur_dur = Dates_calc.Dates.sub_periods
  let o_mult_int_int i1 i2 = Z.mul i1 i2
  let o_mult_rat_rat i1 i2 = Q.mul i1 i2

  let o_mult_mon_rat i1 i2 =
    (* Multiply then round to nearest cent *)
    let rat_result = Q.mul (Q.of_bigint i1) i2 in
    round rat_result

  let o_mult_dur_int d m = Dates_calc.Dates.mul_period d (Z.to_int m)

  let o_div_int_int pos i1 i2 =
    (* It's not on the ocamldoc, but Q.div likely already raises this ? *)
    if Z.zero = i2 then error DivisionByZero [pos]
    else Q.div (Q.of_bigint i1) (Q.of_bigint i2)

  let o_div_rat_rat pos i1 i2 =
    if Q.zero = i2 then error DivisionByZero [pos] else Q.div i1 i2

  let o_div_mon_mon pos m1 m2 =
    if Z.zero = m2 then error DivisionByZero [pos]
    else Q.div (Q.of_bigint m1) (Q.of_bigint m2)

  let o_div_mon_rat pos m1 r1 =
    if Q.zero = r1 then error DivisionByZero [pos]
    else o_mult_mon_rat m1 (Q.inv r1)

  let o_div_dur_dur pos d1 d2 =
    let i1, i2 =
      try
        ( integer_of_int (Dates_calc.Dates.period_to_days d1),
          integer_of_int (Dates_calc.Dates.period_to_days d2) )
      with Dates_calc.Dates.AmbiguousComputation ->
        error IndivisibleDurations [pos]
    in
    o_div_int_int pos i1 i2

  let o_lt_int_int i1 i2 = Z.compare i1 i2 < 0
  let o_lt_rat_rat i1 i2 = Q.compare i1 i2 < 0
  let o_lt_mon_mon m1 m2 = Z.compare m1 m2 < 0
  let o_lt_dur_dur pos d1 d2 = compare_periods pos d1 d2 < 0
  let o_lt_dat_dat d1 d2 = Dates_calc.Dates.compare_dates d1 d2 < 0
  let o_lte_int_int i1 i2 = Z.compare i1 i2 <= 0
  let o_lte_rat_rat i1 i2 = Q.compare i1 i2 <= 0
  let o_lte_mon_mon m1 m2 = Z.compare m1 m2 <= 0
  let o_lte_dur_dur pos d1 d2 = compare_periods pos d1 d2 <= 0
  let o_lte_dat_dat d1 d2 = Dates_calc.Dates.compare_dates d1 d2 <= 0
  let o_gt_int_int i1 i2 = Z.compare i1 i2 > 0
  let o_gt_rat_rat i1 i2 = Q.compare i1 i2 > 0
  let o_gt_mon_mon m1 m2 = Z.compare m1 m2 > 0
  let o_gt_dur_dur pos d1 d2 = compare_periods pos d1 d2 > 0
  let o_gt_dat_dat d1 d2 = Dates_calc.Dates.compare_dates d1 d2 > 0
  let o_gte_int_int i1 i2 = Z.compare i1 i2 >= 0
  let o_gte_rat_rat i1 i2 = Q.compare i1 i2 >= 0
  let o_gte_mon_mon m1 m2 = Z.compare m1 m2 >= 0
  let o_gte_dur_dur pos d1 d2 = compare_periods pos d1 d2 >= 0
  let o_gte_dat_dat d1 d2 = Dates_calc.Dates.compare_dates d1 d2 >= 0
  let o_eq_boo_boo b1 b2 = b1 = b2
  let o_eq_int_int i1 i2 = Z.equal i1 i2
  let o_eq_rat_rat i1 i2 = Q.equal i1 i2
  let o_eq_mon_mon m1 m2 = Z.equal m1 m2
  let o_eq_dur_dur pos d1 d2 = equal_periods pos d1 d2
  let o_eq_dat_dat d1 d2 = Dates_calc.Dates.compare_dates d1 d2 = 0
  let o_fold = Array.fold_left
  let o_toclosureenv = Obj.repr
  let o_fromclosureenv = Obj.obj
end

include Oper

type hash = string

let modules_table : (string, hash) Hashtbl.t = Hashtbl.create 13
let values_table : (string list * string, Obj.t) Hashtbl.t = Hashtbl.create 13

let register_module modname values hash =
  Hashtbl.add modules_table modname hash;
  List.iter (fun (id, v) -> Hashtbl.add values_table ([modname], id) v) values

let check_module m h =
  let h1 = Hashtbl.find modules_table m in
  if String.equal h h1 then Ok () else Error h1

let lookup_value qid =
  try Hashtbl.find values_table qid
  with Not_found ->
    failwith
      ("Could not resolve reference to "
      ^ String.concat "." (fst qid)
      ^ "."
      ^ snd qid)
