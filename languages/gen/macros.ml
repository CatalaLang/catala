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

open Utils

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

let format_lang_macro_file ppf (language : Language_t.language) =
  let open Format in
  let dl () =
    pp_print_newline ppf ();
    pp_print_newline ppf ()
  in
  fprintf ppf {|(* This file has been generated, do not edit manually. *)|};
  dl ();
  fprintf ppf {|(* Defining the lexer macros for %s *)|} language.name;
  dl ();
  fprintf ppf {|(* Keywords *)|};
  dl ();
  let lp =
    let kwds = language.keywords in
    [
      "ALL", kwds.all;
      "AMONG", kwds.among;
      "AND", kwds.and_;
      "AND_THEN", kwds.and_then;
      "ASSERTION", kwds.assertion;
      "BUT_REPLACE", kwds.but_replace;
      "COMBINE", kwds.combine;
      "CONDITION", kwds.condition;
      "CONSEQUENCE", kwds.consequence;
      "CONTAINS", kwds.contains;
      "CONTENT", kwds.content;
      "CONTEXT", kwds.context;
      "DATA", kwds.data;
      "DAY", kwds.day;
      "DECLARATION", kwds.declaration;
      "DECREASING", kwds.decreasing;
      "DEFINED_AS", kwds.defined_as;
      "DEFINITION", kwds.definition;
      "DEPENDS", kwds.depends;
      "ELSE", kwds.else_;
      "ENUM", kwds.enum;
      "EXCEPTION", kwds.exception_;
      "EXISTS", kwds.exists;
      "FALSE", kwds.false_;
      "FILLED", kwds.filled;
      "FOR", kwds.for_;
      "IF", kwds.if_;
      "IN", kwds.in_;
      "INCREASING", kwds.increasing;
      "INITIALLY", kwds.initially;
      "INPUT", kwds.input;
      "INTERNAL", kwds.internal;
      "IS", kwds.is;
      "LABEL", kwds.label;
      "LET", kwds.let_;
      "LIST", kwds.list;
      "MAP_EACH", kwds.map_each;
      "MATCH", kwds.match_;
      "MAXIMUM", kwds.maximum;
      "MINIMUM", kwds.minimum;
      "MONTH", kwds.month;
      "NOT", kwds.not_;
      "OF", kwds.of_;
      "OPTION", kwds.option;
      "OR", kwds.or_;
      "OR_IF_LIST_EMPTY", kwds.or_if_list_empty;
      "ORDER_ASCENDING", kwds.order_ascending;
      "ORDER_DESCENDING", kwds.order_descending;
      "OUTPUT", kwds.output;
      "RULE", kwds.rule;
      "SCOPE", kwds.scope;
      "SORT", kwds.sort;
      "STATE", kwds.state;
      "STRUCT", kwds.struct_;
      "SUCH", kwds.such;
      "SUM", kwds.sum;
      "THAT", kwds.that;
      "THEN", kwds.then_;
      "TO", kwds.to_;
      "TRUE", kwds.true_;
      "TYPE", kwds.type_;
      "UNDER_CONDITION", kwds.under_condition;
      "WE_HAVE", kwds.we_have;
      "WILDCARD", kwds.wildcard;
      "WITH", kwds.with_;
      "WITH_V", kwds.with_v;
      "XOR", kwds.xor;
      "YEAR", kwds.year;
    ]
  in
  (pp_print_list ~pp_sep:pp_print_newline format_macro) ppf lp;
  dl ();
  fprintf ppf {|(* Specific delimiters *)|};
  dl ();
  let sd = language.specific_delimiters in
  fprintf ppf "#define MC_DECIMAL_SEPARATOR '%c'" sd.decimal_separator.[0];
  pp_print_newline ppf ();
  fprintf ppf "#define MR_MONEY_DELIM '%c'" sd.money_delim.[0];
  pp_print_newline ppf ();
  format_macro ppf ("MONEY_OP_SUFFIX", sd.money_unit);
  pp_print_newline ppf ();
  if sd.money_unit_position = `Right then
    fprintf ppf "#define MR_MONEY_PREFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_PREFIX %a, Star hspace" format_regexp
      (string_to_regexp sd.money_unit);
  pp_print_newline ppf ();
  if sd.money_unit_position = `Left then
    fprintf ppf "#define MR_MONEY_SUFFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_SUFFIX Star hspace, %a" format_regexp
      (string_to_regexp sd.money_unit);
  dl ();
  fprintf ppf {|(* Builtins *)|};
  dl ();
  let bt =
    let t = language.builtins.types in
    let o = language.builtins.optionals in
    let f = language.builtins.functions in
    [
      "BOOLEAN", t.boolean;
      "DATE", t.date;
      "DECIMAL", t.decimal;
      "DURATION", t.duration;
      "INTEGER", t.integer;
      "MONEY", t.money;
      "POSITION", t.position;
      "PRESENT", o.present;
      "ABSENT", o.absent;
      "ROUND", f.round;
      "IMPOSSIBLE", f.impossible;
      "CARDINAL", f.cardinal;
    ]
  in
  (pp_print_list ~pp_sep:pp_print_newline format_macro) ppf bt;
  dl ();
  fprintf ppf {|(* Directives *)|};
  dl ();
  let dt =
    let d = language.directives in
    [
      "EXTERNAL", d.external_;
      "LAW_INCLUDE", d.law_include;
      "MODULE_ALIAS", d.module_alias;
      "MODULE_DEF", d.module_def;
      "MODULE_USE", d.module_use;
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
