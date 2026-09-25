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

open Catala_utils

type re_kind = Str of string | Space | Unicode of Uchar.t

let string_to_regexp s =
  let s = String.utf8_seq s in
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

let format_token_macro ppf (tok_name, s) =
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
  fprintf ppf {|(* Defining the lexer macros for %s *)|} language.lang_name;
  dl ();
  fprintf ppf {|(* Tokens *)|};
  dl ();
  let lp =
    let toks = language.tokens in
    [
      "ALL", toks.all;
      "AMONG", toks.among;
      "AND", toks.and_;
      "AND_THEN", toks.and_then;
      "ASSERTION", toks.assertion;
      "BUT_REPLACE", toks.but_replace;
      "COMBINE", toks.combine;
      "CONDITION", toks.condition;
      "CONSEQUENCE", toks.consequence;
      "CONTAINS", toks.contains;
      "CONTENT", toks.content;
      "CONTEXT", toks.context;
      "DATA", toks.data;
      "DAY", toks.day;
      "DECLARATION", toks.declaration;
      "DECREASING", toks.decreasing;
      "DEFINED_AS", toks.defined_as;
      "DEFINITION", toks.definition;
      "DEPENDS", toks.depends;
      "ELSE", toks.else_;
      "ENUM", toks.enum;
      "EXCEPTION", toks.exception_;
      "EXISTS", toks.exists;
      "FALSE", toks.false_;
      "FILLED", toks.filled;
      "FOR", toks.for_;
      "IF", toks.if_;
      "IN", toks.in_;
      "INCREASING", toks.increasing;
      "INITIALLY", toks.initially;
      "INPUT", toks.input;
      "INTERNAL", toks.internal;
      "IS", toks.is;
      "LABEL", toks.label;
      "LET", toks.let_;
      "LIST", toks.list;
      "MAP_EACH", toks.map_each;
      "MATCH", toks.match_;
      "MAXIMUM", toks.maximum;
      "MINIMUM", toks.minimum;
      "MONTH", toks.month;
      "NOT", toks.not_;
      "OF", toks.of_;
      "OPTION", toks.option;
      "OR", toks.or_;
      "OR_IF_LIST_EMPTY", toks.or_if_list_empty;
      "ORDER_ASCENDING", toks.order_ascending;
      "ORDER_DESCENDING", toks.order_descending;
      "OUTPUT", toks.output;
      "RULE", toks.rule;
      "SCOPE", toks.scope;
      "SORT", toks.sort;
      "STATE", toks.state;
      "STRUCT", toks.struct_;
      "SUCH", toks.such;
      "SUM", toks.sum;
      "THAT", toks.that;
      "THEN", toks.then_;
      "TO", toks.to_;
      "TRUE", toks.true_;
      "TYPE", toks.type_;
      "UNDER_CONDITION", toks.under_condition;
      "WE_HAVE", toks.we_have;
      "WILDCARD", toks.wildcard;
      "WITH", toks.with_;
      "WITH_V", toks.with_v;
      "XOR", toks.xor;
      "YEAR", toks.year;
    ]
  in
  (pp_print_list ~pp_sep:pp_print_newline format_token_macro) ppf lp;
  dl ();
  fprintf ppf {|(* Specific delimiters *)|};
  dl ();
  let sd = language.specific_delimiters in
  fprintf ppf "#define MC_DECIMAL_SEPARATOR '%c'" sd.decimal_separator.[0];
  pp_print_newline ppf ();
  fprintf ppf "#define MR_MONEY_DELIM '%c'" sd.money_delim.[0];
  pp_print_newline ppf ();
  format_token_macro ppf ("MONEY_OP_SUFFIX", sd.money_op_suffix);
  pp_print_newline ppf ();
  if sd.money_prefix = "" then fprintf ppf "#define MR_MONEY_PREFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_PREFIX %a, Star hspace" format_regexp
      (string_to_regexp sd.money_prefix);
  pp_print_newline ppf ();
  if sd.money_suffix = "" then fprintf ppf "#define MR_MONEY_SUFFIX \"\""
  else
    fprintf ppf "#define MR_MONEY_SUFFIX Star hspace, %a" format_regexp
      (string_to_regexp sd.money_suffix);
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
  (pp_print_list ~pp_sep:pp_print_newline format_token_macro) ppf bt;
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
  let c = File.contents Sys.argv.(1) in
  let lang =
    Lexing.from_string c |> Language_j.read_language (Yojson.init_lexer ())
  in
  Format.printf "%a" format_lang_macro_file lang
