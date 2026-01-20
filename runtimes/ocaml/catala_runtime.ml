(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2026 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

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
type date = Dates_calc.date

type date_rounding = Dates_calc.date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound

type duration = Dates_calc.period
type io_input = NoInput | OnlyInput | Reentrant
type io_log = { io_input : io_input; io_output : bool }

type code_location = {
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
  | ListEmpty
  | NotSameLength
  | InvalidDate
  | UncomparableDurations
  | AmbiguousDateRounding
  | IndivisibleDurations
  | Impossible

let error_to_string = function
  | AssertionFailed -> "AssertionFailed"
  | NoValue -> "NoValue"
  | Conflict -> "Conflict"
  | DivisionByZero -> "DivisionByZero"
  | ListEmpty -> "ListEmpty"
  | NotSameLength -> "NotSameLength"
  | InvalidDate -> "InvalidDate"
  | UncomparableDurations -> "UncomparableDurations"
  | AmbiguousDateRounding -> "AmbiguousDateRounding"
  | IndivisibleDurations -> "IndivisibleDurations"
  | Impossible -> "Impossible"

let error_message = function
  | AssertionFailed -> "an assertion doesn't hold"
  | NoValue -> "no applicable rule to define this variable in this situation"
  | Conflict ->
    "conflict between multiple valid consequences for assigning the same \
     variable"
  | DivisionByZero ->
    "a value is being used as denominator in a division and it computed to zero"
  | ListEmpty -> "the list was empty"
  | NotSameLength -> "traversing multiple lists of different lengths"
  | InvalidDate -> "the provided numbers do not correspond to a valid date"
  | UncomparableDurations ->
    "ambiguous comparison between durations in different units (e.g. months \
     vs. days)"
  | AmbiguousDateRounding ->
    "ambiguous date computation, and rounding mode was not specified"
  | IndivisibleDurations -> "dividing durations that are not in days"
  | Impossible -> "\"impossible\" computation reached"

exception Error of error * code_location list * string option
exception Empty

let error ?note err pos = raise (Error (err, pos, note))

(* Register (fallback) exception printers *)
let () =
  let ppos () p =
    Printf.sprintf "%s:%d.%d-%d.%d" p.filename p.start_line p.start_column
      p.end_line p.end_column
  in
  let pposl () pl = String.concat ", " (List.map (ppos ()) pl) in
  Printexc.register_printer
  @@ function
  | Error (err, pos, note) ->
    Some
      (Printf.sprintf "At %a: %s%s" pposl pos (error_message err)
         (Option.fold ~none:"" ~some:(( ^ ) ". ") note))
  | _ -> None

let () =
  Printexc.set_uncaught_exception_handler
  @@ fun exc bt ->
  Printf.eprintf "\x1b[1;31m[ERROR]\x1b[m %s\n%!" (Printexc.to_string exc);
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

let money_of_integer (i : integer) : money = Z.(i * of_int 100)

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
let integer_of_money (m : money) : integer = round (decimal_of_money m)
let integer_exponentiation (i : integer) (e : int) : integer = Z.pow i e
let integer_log2 = Z.log2

let year_of_date (d : date) : integer =
  let y, _, _ = Dates_calc.date_to_ymd d in
  Z.of_int y

let month_number_of_date (d : date) : integer =
  let _, m, _ = Dates_calc.date_to_ymd d in
  Z.of_int m

let is_leap_year (y : integer) =
  let y = Z.to_int y in
  Dates_calc.is_leap_year y

let day_of_month_of_date (d : date) : integer =
  let _, _, d = Dates_calc.date_to_ymd d in
  Z.of_int d

(* This could fail, but is expected to only be called with known, already
   validated arguments by the generated code *)
let date_of_numbers (year : int) (month : int) (day : int) : date =
  try Dates_calc.make_date ~year ~month ~day
  with Dates_calc.InvalidDate -> failwith "date_of_numbers: invalid date"

let date_to_string (d : date) : string =
  Format.asprintf "%a" Dates_calc.format_date d

let date_to_years_months_days (d : date) : int * int * int =
  Dates_calc.date_to_ymd d

let first_day_of_month = Dates_calc.first_day_of_month
let last_day_of_month = Dates_calc.last_day_of_month

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  Dates_calc.make_period ~years:year ~months:month ~days:day

let duration_to_string (d : duration) : string =
  Format.asprintf "%a" Dates_calc.format_period d

let duration_to_years_months_days (d : duration) : int * int * int =
  Dates_calc.period_to_ymds d

(* Maybe should be integrated into dates_calc ? *)
let compare_periods pos p1 p2 =
  let y1, m1, d1 = Dates_calc.period_to_ymds p1 in
  let y2, m2, d2 = Dates_calc.period_to_ymds p2 in
  match y1, y2, m1, m2, d1, d2 with
  | _, _, _, _, 0, 0 -> Int.compare ((12 * y1) + m1) ((12 * y2) + m2)
  | 0, 0, 0, 0, d1, d2 -> Int.compare d1 d2
  | _ -> error UncomparableDurations [pos]

let equal_periods pos p1 p2 =
  Dates_calc.period_to_ymds p1 = Dates_calc.period_to_ymds p2
  || compare_periods pos p1 p2 = 0

(* -- Runtime types and embedding -- *)

module Value = struct
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
    | External : {
        name : string;
        equal : code_location -> 'a -> t -> bool;
        compare : code_location -> 'a -> t -> int;
        to_json : ('a -> string) option;
        to_string : 'a -> string;
      }
        -> 'a ty
    | Function : 'a ty

  and t = V : 'a ty * 'a -> t

  let embed t v = V (t, v)

  (* let unembed (type a) (V { t; v }): a ty * a =
   *   Obj.magic t, Obj.magic v *)

  let rec equal : code_location -> t -> t -> bool =
   fun pos rv1 rv2 ->
    match rv1, rv2 with
    | V (Unit, ()), V (Unit, ()) -> true
    | V (Bool, v1), V (Bool, v2) -> equal_values Bool pos v1 v2
    | V (Integer, v1), V (Integer, v2) -> equal_values Integer pos v1 v2
    | V (Money, v1), V (Money, v2) -> equal_values Money pos v1 v2
    | V (Decimal, v1), V (Decimal, v2) -> equal_values Decimal pos v1 v2
    | V (Date, v1), V (Date, v2) -> equal_values Date pos v1 v2
    | V (Duration, v1), V (Duration, v2) -> equal_values Duration pos v1 v2
    | V (Position, v1), V (Position, v2) -> equal_values Position pos v1 v2
    | V (Array t1, v1), V (Array t2, v2) ->
      Array.length v1 = Array.length v2
      && Array.for_all2 (equal pos) (Array.map t1 v1) (Array.map t2 v2)
    | V (Tuple t1, v1), V (Tuple t2, v2) ->
      List.for_all2 (equal pos) (t1 v1) (t2 v2)
    | V (Struct str1, v1), V (Struct str2, v2) ->
      str1.name = str2.name
      &&
      (* could be an assert if well-typed ? *)
      List.for_all2
        (fun (fld1, rv1) (fld2, rv2) -> fld1 = fld2 && equal pos rv1 rv2)
        (str1.fields v1) (str2.fields v2)
    | V (Enum en1, v1), V (Enum en2, v2) ->
      en1.name = en2.name
      &&
      (* could be an assert if well-typed ? *)
      let n1, _, x1 = en1.constr v1 in
      let n2, _, x2 = en2.constr v2 in
      n1 = n2 && Option.equal (equal pos) x1 x2
    | V (External ex, v), rv2 -> ex.equal pos v rv2
    | V (Function, _), V (Function, _) -> failwith "Uncomparable"
    (* The follwing shouldn't happen on well-typed terms *)
    | ( V
          ( ( Unit | Bool | Integer | Money | Decimal | Date | Duration
            | Position | Array _ | Tuple _ | Struct _ | Enum _ | Function ),
            _ ),
        _ ) ->
      false

  and equal_values : type a. a ty -> code_location -> a -> a -> bool =
   fun ty pos x1 x2 ->
    match ty with
    | Unit -> true
    | Bool -> Bool.equal x1 x2
    | Integer -> Z.equal x1 x2
    | Money -> Z.equal x1 x2
    | Decimal -> Q.equal x1 x2
    | Date -> Dates_calc.compare_dates x1 x2 = 0
    | Duration -> equal_periods pos x1 x2
    | Position -> x1 = x2
    | t -> equal pos (V (t, x1)) (V (t, x2))

  let rec compare : code_location -> t -> t -> int =
   fun pos rv1 rv2 ->
    let rec compare_lists l1 l2 =
      match l1, l2 with
      | x1 :: l1, x2 :: l2 -> (
        match compare pos x1 x2 with 0 -> compare_lists l1 l2 | n -> n)
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
    in
    match rv1, rv2 with
    | V (Unit, ()), V (Unit, ()) -> 0
    | V (Bool, v1), V (Bool, v2) -> compare_values Bool pos v1 v2
    | V (Integer, v1), V (Integer, v2) -> compare_values Integer pos v1 v2
    | V (Money, v1), V (Money, v2) -> compare_values Money pos v1 v2
    | V (Decimal, v1), V (Decimal, v2) -> compare_values Decimal pos v1 v2
    | V (Date, v1), V (Date, v2) -> compare_values Date pos v1 v2
    | V (Duration, v1), V (Duration, v2) -> compare_values Duration pos v1 v2
    | V (Array t1, v1), V (Array t2, v2) ->
      let rec aux i =
        if i >= Array.length v1 then if i >= Array.length v2 then 0 else -1
        else if i >= Array.length v2 then 1
        else
          match compare pos (t1 v1.(i)) (t2 v2.(i)) with
          | 0 -> aux (i + 1)
          | n -> n
      in
      aux 0
    | V (Tuple to_list1, v1), V (Tuple to_list2, v2) ->
      compare_lists (to_list1 v1) (to_list2 v2)
    | V (Struct str1, v1), V (Struct str2, v2) -> (
      match String.compare str1.name str2.name with
      | 0 ->
        compare_lists
          (List.map snd (str1.fields v1))
          (List.map snd (str2.fields v2))
      | n -> n (* could be assert false if well-typed ? *))
    | V (Enum en1, v1), V (Enum en2, v2) -> (
      match String.compare en1.name en2.name with
      | 0 -> (
        let n1, _, x1 = en1.constr v1 in
        let n2, _, x2 = en2.constr v2 in
        match Stdlib.compare n1 n2 with
        | 0 -> Option.compare (compare pos) x1 x2
        | n -> n)
      | n -> n (* could be assert false if well-typed ? *))
    | V (External ext, v1), rv2 -> ext.compare pos v1 rv2
    | V (Function, _), _ | _, V (Function, _) -> failwith "Uncomparable"
    (* The follwing shouldn't happen on well-typed terms *)
    | V (Unit, _), _ -> -1
    | _, V (Unit, _) -> 1
    | V (Bool, _), _ -> -1
    | _, V (Bool, _) -> 1
    | V (Integer, _), _ -> -1
    | _, V (Integer, _) -> 1
    | V (Money, _), _ -> -1
    | _, V (Money, _) -> 1
    | V (Decimal, _), _ -> -1
    | _, V (Decimal, _) -> 1
    | V (Position, _), _ -> -1
    | _, V (Position, _) -> 1
    | V (Date, _), _ -> -1
    | _, V (Date, _) -> 1
    | V (Duration, _), _ -> -1
    | _, V (Duration, _) -> 1
    | V (Array _, _), _ -> -1
    | _, V (Array _, _) -> 1
    | V (Tuple _, _), _ -> -1
    | _, V (Tuple _, _) -> 1
    | V (Struct _, _), _ -> -1
    | _, V (Struct _, _) -> 1
    | V (Enum _, _), _ -> -1
    | _, V (Enum _, _) -> .
    | V (External _, _), _ -> .
    | _, V (External _, _) -> .

  and compare_values : type a. a ty -> code_location -> a -> a -> int =
   fun ty pos x1 x2 ->
    match ty with
    | Unit -> 0
    | Bool -> Bool.compare x1 x2
    | Money -> Z.compare x1 x2
    | Integer -> Z.compare x1 x2
    | Decimal -> Q.compare x1 x2
    | Date -> Dates_calc.compare_dates x1 x2
    | Duration -> compare_periods pos x1 x2
    | Position -> Stdlib.compare x1 x2
    | t -> compare pos (V (t, x1)) (V (t, x2))

  let rec format ppf = function
    | V (Unit, ()) -> Format.fprintf ppf "()"
    | V (Bool, x) -> Format.fprintf ppf "%b" x
    | V (Money, x) -> Format.fprintf ppf "%s€" (money_to_string x)
    | V (Integer, x) -> Format.fprintf ppf "%s" (Z.to_string x)
    | V (Decimal, x) ->
      Format.fprintf ppf "%s" (decimal_to_string ~max_prec_digits:10 x)
    | V (Date, x) -> Format.fprintf ppf "%s" (date_to_string x)
    | V (Duration, x) -> Format.fprintf ppf "%s" (duration_to_string x)
    | V (Enum en, v) -> (
      match en.constr v with
      | _, name, None -> Format.fprintf ppf "%s" name
      | _, name, Some v -> Format.fprintf ppf "%s(%a)" name format v)
    | V (Struct str, v) ->
      Format.fprintf ppf "@[<hv 2>%s = {@ %a@;<1 -2>}@]" str.name
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           (fun fmt (name, value) ->
             Format.fprintf fmt "%s: %a" name format value))
        (str.fields v)
    | V (Array t, v) ->
      Format.fprintf ppf "@[<hv 2>[@ %a@;<1 -2>]@]"
        (Format.pp_print_seq
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           (fun ppf v -> format ppf (t v)))
        (Array.to_seq v)
    | V (Tuple destr, v) ->
      Format.fprintf ppf "@[<hv 2>(@ %a@;<1 -2>)@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           format)
        (destr v)
    | V (Position, pos) ->
      Format.fprintf ppf "@[<h><%s:%d.%d-%d-%d@]" pos.filename pos.start_line
        pos.start_column pos.end_line pos.end_column
    | V (Function, _) -> Format.fprintf ppf "fun"
    | V (External ex, v) -> Format.pp_print_string ppf (ex.to_string v)
end

let equal = Value.equal_values
let compare = Value.compare_values

(* let get_ty : type a. any_ty -> a ty =
 *   let open struct external cast : _ ty -> a ty = "%identity" end in
 *   function TAny t -> cast t
 * 
 * let unembed (type a) (V (t, v)): a ty * a =
 *   get_ty (TAny t), Obj.magic v *)

(* Catala types utils *)

module type CatalaType = sig
  type t

  val rtype : t Value.ty
end

(* EX PROTO module Foo : CatalaType = struct type t = { foo: integer; bar: date
   }

   let rtype = Struct { name = "Foo"; fields = fun t -> [ "foo", embed
   Integer.rtype t.foo; "bar", embed Date.rtype t.bar; ] } end

   module Bar : CatalaType = struct type t = Foo of integer | Bar of date | Baz
   of (bool * integer)

   let rtype = Enum { name = "Bar"; constr = fun t -> match t with | Foo x -> 0,
   "Foo", Some (embed Integer.rtype x) | Bar x -> 1, "Bar", Some (embed
   Date.rtype x) | Baz x -> 2, "Baz", Some (embed (Tuple (fun (x1, x2) -> [embed
   Bool.rtype x1; embed Integer.rtype x2])) x); } end *)

(* module List : (T: CatalaType) -> CatalaType with type t = T.t array = struct
 *   type t = T.t array
 * end *)

module Optional = struct
  type 'a t = Absent | Present of 'a

  let rtype t =
    Value.Enum
      {
        name = "Optional";
        constr =
          (function
          | Absent -> 0, "Absent", None
          | Present v -> 1, "Present", Some (Value.embed t v));
      }
end

(* -- *)

type information = string list

type raw_event =
  | BeginCall of information
  | EndCall of information
  | VariableDefinition of information * io_log * Value.t
  | DecisionTaken of code_location

type event =
  | VarComputation of var_def
  | FunCall of fun_call
  | SubScopeCall of {
      name : information;
      inputs : var_def list;
      body : event list;
    }

and var_def = {
  pos : code_location option;
  name : information;
  io : io_log;
  value : Value.t;
  fun_calls : fun_call list option;
}

and fun_call = {
  fun_name : information;
  fun_inputs : var_def list;
  body : event list;
  output : var_def;
}

module BufferedJson = struct
  let seq f buf sq =
    match Seq.uncons sq with
    | None -> ()
    | Some (x, r) ->
      f buf x;
      let rec aux sq =
        match Seq.uncons sq with
        | None -> ()
        | Some (x, r) ->
          Buffer.add_char buf ',';
          f buf x;
          aux r
      in
      aux r

  let list f buf l = seq f buf (List.to_seq l)

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

  let decimal buf d =
    let max_decimals = 6 in
    let dec_str =
      let open Z in
      let sign = Q.sign d in
      let n = abs (Q.num d) in
      let d = abs (Q.den d) in
      let int_part, dec_part = div_rem n d in
      bprint buf (~$sign * int_part);
      Buffer.add_char buf '.';
      let dec_part = (((~$10 ** max_decimals) * dec_part) + (d / ~$2)) / d in
      format ("%0" ^ string_of_int max_decimals ^ "d") dec_part
    in
    let rec last_non0 n =
      if n <= 1 || dec_str.[n - 1] <> '0' then n else last_non0 (n - 1)
    in
    Buffer.add_substring buf dec_str 0 (last_non0 max_decimals)

  (* Note: the output format is made for transition with what Yojson gave us,
     but we could change it to something nicer (e.g. objects for structures) *)
  let rec runtime_value buf = function
    | Value.V (Unit, ()) -> Buffer.add_string buf "{}"
    | V (Bool, b) -> Buffer.add_string buf (string_of_bool b)
    | V (Money, m) -> Buffer.add_string buf (money_to_string m)
    | V (Integer, i) -> Buffer.add_string buf (integer_to_string i)
    | V (Decimal, d) -> decimal buf d
    | V (Date, d) -> quote buf (date_to_string d)
    | V (Duration, d) -> quote buf (duration_to_string d)
    | V (Enum en, e) ->
      let _, constr, value = en.constr e in
      Printf.bprintf buf
        {|{"kind": "enum", "name": "%s", "constructor": "%s"%a}|} en.name constr
        (fun buf -> function
          | None -> ()
          | Some v -> Printf.bprintf buf {|, "value": %a|} runtime_value v)
        value
    | V (Struct str, s) ->
      let fields = str.fields s in
      Printf.bprintf buf {|{"kind": "struct", "name": "%s", "fields": {%a}}|}
        str.name
        (fun buf ->
          List.iter (fun (name, v) ->
              Printf.bprintf buf {|"%a": %a|} quote name runtime_value v))
        fields
    | V (Array t, a) ->
      Printf.bprintf buf {|{"kind": "array", "value":[%a]}|}
        (seq (fun buf v -> runtime_value buf (t v)))
        (Stdlib.Array.to_seq a)
    | V (Tuple destr, a) ->
      Printf.bprintf buf {|{"kind": "tuple", "value":[%a]}|}
        (list runtime_value) (destr a)
    | V (Position, pos) ->
      Printf.bprintf buf {|{"kind": "position", "value":[%s, %d, %d, %d, %d]}|}
        pos.filename pos.start_line pos.start_column pos.end_line pos.end_column
    | V (Function, _) -> Buffer.add_string buf {|"unembeddable"|}
    | V (External _ex, _v) ->
      Buffer.add_string buf {|"unembeddable"|} (* ex.to_json v ?? *)

  let information buf info = Printf.bprintf buf "[%a]" (list quote) info

  let code_location buf pos =
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
    Option.iter (Printf.bprintf buf {|{"pos":%a|} code_location) def.pos;
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
         "value": %a
         }|}
        (String.concat "." name) io_log io runtime_value value
    | DecisionTaken source_pos ->
      Printf.bprintf buf {|{"event": "DecisionTaken", "pos": %a}|} code_location
        source_pos
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
      Value.format var.value
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
        Value.format var_with_fun.value
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           (fun ppf fun_call -> format_event ppf (FunCall fun_call)))
        fun_calls
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

let handle_exceptions (exceptions : ('a * code_location) Optional.t array) :
    ('a * code_location) Optional.t =
  let len = Array.length exceptions in
  let rec filt_except i =
    if i < len then
      match exceptions.(i) with
      | Optional.Present _ as new_val -> new_val :: filt_except (i + 1)
      | Optional.Absent -> filt_except (i + 1)
    else []
  in
  match filt_except 0 with
  | [] -> Optional.Absent
  | [res] -> res
  | res ->
    error Conflict
      (List.map
         (function Optional.Present (_, pos) -> pos | _ -> assert false)
         res)

module Oper = struct
  let o_not = Stdlib.not
  let o_length a = Z.of_int (Array.length a)
  let o_toint_rat = integer_of_decimal
  let o_toint_mon = integer_of_money
  let o_torat_int = decimal_of_integer
  let o_torat_mon = decimal_of_money
  let o_tomoney_rat = money_of_decimal
  let o_tomoney_int = money_of_integer
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
  let o_minus_dur = Dates_calc.neg_period
  let o_and = ( && )
  let o_or = ( || )
  let o_xor : bool -> bool -> bool = ( <> )
  let o_eq t pos x1 x2 = equal t pos x1 x2
  let o_lt t pos x1 x2 = compare t pos x1 x2 < 0
  let o_lte t pos x1 x2 = compare t pos x1 x2 <= 0
  let o_gt t pos x1 x2 = compare t pos x1 x2 > 0
  let o_gte t pos x1 x2 = compare t pos x1 x2 >= 0
  let o_map = Array.map

  let o_map2 pos f a b =
    try Array.map2 f a b with Invalid_argument _ -> error NotSameLength [pos]

  let o_reduce f dft a =
    let len = Array.length a in
    if len = 0 then dft ()
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
    try Dates_calc.add_dates ~round:r da du
    with Dates_calc.AmbiguousComputation -> error AmbiguousDateRounding [pos]

  let o_add_dur_dur = Dates_calc.add_periods
  let o_sub_int_int i1 i2 = Z.sub i1 i2
  let o_sub_rat_rat i1 i2 = Q.sub i1 i2
  let o_sub_mon_mon m1 m2 = Z.sub m1 m2
  let o_sub_dat_dat = Dates_calc.sub_dates

  let o_sub_dat_dur r pos dat dur =
    o_add_dat_dur r pos dat (Dates_calc.neg_period dur)

  let o_sub_dur_dur = Dates_calc.sub_periods
  let o_mult_int_int i1 i2 = Z.mul i1 i2
  let o_mult_rat_rat i1 i2 = Q.mul i1 i2

  let o_mult_mon_rat i1 i2 =
    (* Multiply then round to nearest cent *)
    let rat_result = Q.mul (Q.of_bigint i1) i2 in
    round rat_result

  let o_mult_mon_int i1 i2 = o_mult_mon_rat i1 (decimal_of_integer i2)
  let o_mult_dur_int d m = Dates_calc.mul_period d (Z.to_int m)

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

  let o_div_mon_int pos m1 i1 = o_div_mon_rat pos m1 (decimal_of_integer i1)

  let o_div_dur_dur pos d1 d2 =
    let i1, i2 =
      try
        ( integer_of_int (Dates_calc.period_to_days d1),
          integer_of_int (Dates_calc.period_to_days d2) )
      with Dates_calc.AmbiguousComputation -> error IndivisibleDurations [pos]
    in
    o_div_int_int pos i1 i2

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
