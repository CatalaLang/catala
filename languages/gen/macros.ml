(* This file is part of the Catala compiler, a specification language
   for tax and social benefits computation rules. Copyright (C) 2026
   Inria, contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License. You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. *)

(** This file generates preprocessor directives that will populate the tokens of
    'compiler/surface/lexer.cppo.ml' *)

open Utils
open Language_t

type re_kind = Str of string | Space | Unicode of Uchar.t

let string_to_regexp s =
  let s = utf8_seq s in
  let is_space = ( = ) 0x20 in
  let is_ascii = ( > ) 128 in
  let rec loop (acc, cl) s =
    let wrap_cl () =
      if cl = [] then acc
      else Str (String.of_seq (List.rev cl |> List.to_seq)) :: acc
    in
    match s () with
    | Seq.Nil -> List.rev (wrap_cl ())
    | Cons (c, r) ->
      let ic = Uchar.to_int c in
      if is_space ic then loop (Space :: wrap_cl (), []) r
      else if is_ascii ic then loop (acc, Uchar.to_char c :: cl) r
      else loop (Unicode c :: wrap_cl (), []) r
  in
  loop ([], []) s

let format_regexp ppf re =
  let open Format in
  let format_re_kind ppf = function
    | Str s ->
      if String.length s = 1 then fprintf ppf "'%s'" s
      else fprintf ppf "\"%s\"" s
    | Space -> fprintf ppf "space_plus"
    | Unicode c -> fprintf ppf "0x%x" (Uchar.to_int c)
  in
  (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") format_re_kind) ppf re

let format_regexp_macro ppf (tok_name, re) =
  Format.fprintf ppf "@[<h>#define MR_%s %a@]" tok_name format_regexp re

let format_macro ppf (tok_name, s) =
  let r = string_to_regexp s in
  Format.fprintf ppf "@[<h>#define MS_%s \"%s\"@]" tok_name s;
  match r with
  | [Str _] -> ()
  | re ->
    Format.pp_print_newline ppf ();
    format_regexp_macro ppf (tok_name, re)

let format_lang_macro_file ppf (language : language) =
  (* Expanding all records for exhaustivity check *)
  let { name; code = _; keywords; specific_delimiters; builtins; directives } =
    language
  in
  (* keywords *)
  let {
    all;
    among;
    and_;
    and_then;
    assertion;
    but_replace;
    combine;
    condition;
    consequence;
    contains;
    content;
    context;
    data;
    day;
    declaration;
    decreasing;
    defined_as;
    definition;
    depends;
    else_;
    enum;
    exception_;
    exists;
    false_;
    filled;
    for_;
    if_;
    in_;
    increasing;
    initially;
    input;
    internal;
    is;
    label;
    let_;
    list;
    map_each;
    match_;
    maximum;
    minimum;
    month;
    not_;
    of_;
    option;
    or_;
    or_if_list_empty;
    order_increasing;
    order_decreasing;
    output;
    rule;
    scope;
    sort;
    state;
    struct_;
    such;
    sum;
    that;
    then_;
    to_;
    true_;
    type_;
    under_condition;
    we_have;
    wildcard;
    with_;
    with_v;
    xor;
    year;
  } =
    keywords
  in
  (* builtins *)
  let { types; optionals; functions } = builtins in
  let { boolean; date; decimal; duration; integer; money; position } = types in
  let { present; absent } = optionals in
  let { round; impossible; cardinal } = functions in
  (* specific_delimiters *)
  let { decimal_separator; money_unit; money_unit_position; money_delim } =
    specific_delimiters
  in
  (* directives *)
  let { external_; law_include; module_alias; module_def; module_use } =
    directives
  in
  let open Format in
  let dl () =
    pp_print_newline ppf ();
    pp_print_newline ppf ()
  in
  fprintf ppf {|(* This file has been generated, do not edit manually. *)|};
  dl ();
  fprintf ppf {|(* Defining the lexer macros for %s *)|} name;
  dl ();
  fprintf ppf {|#define LEXER_MODULE_NAME %s |} (String.capitalize_ascii name);
  dl ();
  fprintf ppf {|(* Keywords *)|};
  dl ();
  let lp =
    [
      "ALL", all;
      "AMONG", among;
      "AND", and_;
      "AND_THEN", and_then;
      "ASSERTION", assertion;
      "BUT_REPLACE", but_replace;
      "COMBINE", combine;
      "CONDITION", condition;
      "CONSEQUENCE", consequence;
      "CONTAINS", contains;
      "CONTENT", content;
      "CONTEXT", context;
      "DATA", data;
      "DAY", day;
      "DECLARATION", declaration;
      "DECREASING", decreasing;
      "DEFINED_AS", defined_as;
      "DEFINITION", definition;
      "DEPENDS", depends;
      "ELSE", else_;
      "ENUM", enum;
      "EXCEPTION", exception_;
      "EXISTS", exists;
      "FALSE", false_;
      "FILLED", filled;
      "FOR", for_;
      "IF", if_;
      "IN", in_;
      "INCREASING", increasing;
      "INITIALLY", initially;
      "INPUT", input;
      "INTERNAL", internal;
      "IS", is;
      "LABEL", label;
      "LET", let_;
      "LIST", list;
      "MAP_EACH", map_each;
      "MATCH", match_;
      "MAXIMUM", maximum;
      "MINIMUM", minimum;
      "MONTH", month;
      "NOT", not_;
      "OF", of_;
      "OPTION", option;
      "OR", or_;
      "OR_IF_LIST_EMPTY", or_if_list_empty;
      "ORDER_INCREASING", order_increasing;
      "ORDER_DECREASING", order_decreasing;
      "OUTPUT", output;
      "RULE", rule;
      "SCOPE", scope;
      "SORT", sort;
      "STATE", state;
      "STRUCT", struct_;
      "SUCH", such;
      "SUM", sum;
      "THAT", that;
      "THEN", then_;
      "TO", to_;
      "TRUE", true_;
      "TYPE", type_;
      "UNDER_CONDITION", under_condition;
      "WE_HAVE", we_have;
      "WILDCARD", wildcard;
      "WITH", with_;
      "WITH_V", with_v;
      "XOR", xor;
      "YEAR", year;
    ]
  in
  (pp_print_list ~pp_sep:pp_print_newline format_macro) ppf lp;
  dl ();
  fprintf ppf {|(* Specific delimiters *)|};
  dl ();
  fprintf ppf "#define MC_DECIMAL_SEPARATOR '%c'" decimal_separator.[0];
  pp_print_newline ppf ();
  fprintf ppf "#define MR_MONEY_DELIM '%c'" money_delim.[0];
  pp_print_newline ppf ();
  format_macro ppf ("MONEY_OP_SUFFIX", money_unit);
  pp_print_newline ppf ();
  if money_unit_position = `Right then
    fprintf ppf "#define MR_MONEY_PREFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_PREFIX %a, Star hspace" format_regexp
      (string_to_regexp money_unit);
  pp_print_newline ppf ();
  if money_unit_position = `Left then fprintf ppf "#define MR_MONEY_SUFFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_SUFFIX Star hspace, %a" format_regexp
      (string_to_regexp money_unit);
  dl ();
  fprintf ppf {|(* Builtins *)|};
  dl ();
  let bt =
    [
      "BOOLEAN", boolean;
      "DATE", date;
      "DECIMAL", decimal;
      "DURATION", duration;
      "INTEGER", integer;
      "MONEY", money;
      "POSITION", position;
      "PRESENT", present;
      "ABSENT", absent;
      "ROUND", round;
      "IMPOSSIBLE", impossible;
      "CARDINAL", cardinal;
    ]
  in
  (pp_print_list ~pp_sep:pp_print_newline format_macro) ppf bt;
  dl ();
  fprintf ppf {|(* Directives *)|};
  dl ();
  let dt =
    [
      "EXTERNAL", external_;
      "LAW_INCLUDE", law_include;
      "MODULE_ALIAS", module_alias;
      "MODULE_DEF", module_def;
      "MODULE_USE", module_use;
    ]
    |> List.map (fun (n, s) -> n, string_to_regexp s)
  in
  (pp_print_list ~pp_sep:pp_print_newline format_regexp_macro) ppf dt;
  ()

let () =
  let c = contents Sys.argv.(1) in
  let lang =
    Lexing.from_string c |> Language_j.read_language (Yojson.init_lexer ())
  in
  format_lang_macro_file Format.std_formatter lang
