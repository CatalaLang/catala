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

let () =
  let c = contents Sys.argv.(1) in
  let lang =
    Lexing.from_string c |> Language_j.read_language (Yojson.init_lexer ())
  in
  let open Format in
  let ppf = Format.std_formatter in
  let dl () =
    pp_print_newline ppf ();
    pp_print_newline ppf ()
  in
  fprintf ppf "open Language_t";
  dl ();
  let format_field_def ppf (name, s) = fprintf ppf "%s = %S" name s in
  let format_keywords ppf =
    let kwds = lang.keywords in
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") format_field_def)
      ppf
      [
        "all", kwds.all;
        "among", kwds.among;
        "and_", kwds.and_;
        "and_then", kwds.and_then;
        "assertion", kwds.assertion;
        "but_replace", kwds.but_replace;
        "combine", kwds.combine;
        "condition", kwds.condition;
        "consequence", kwds.consequence;
        "contains", kwds.contains;
        "content", kwds.content;
        "context", kwds.context;
        "data", kwds.data;
        "day", kwds.day;
        "declaration", kwds.declaration;
        "decreasing", kwds.decreasing;
        "defined_as", kwds.defined_as;
        "definition", kwds.definition;
        "depends", kwds.depends;
        "else_", kwds.else_;
        "enum", kwds.enum;
        "exception_", kwds.exception_;
        "exists", kwds.exists;
        "false_", kwds.false_;
        "filled", kwds.filled;
        "for_", kwds.for_;
        "if_", kwds.if_;
        "in_", kwds.in_;
        "increasing", kwds.increasing;
        "initially", kwds.initially;
        "input", kwds.input;
        "internal", kwds.internal;
        "is", kwds.is;
        "label", kwds.label;
        "let_", kwds.let_;
        "list", kwds.list;
        "map_each", kwds.map_each;
        "match_", kwds.match_;
        "maximum", kwds.maximum;
        "minimum", kwds.minimum;
        "month", kwds.month;
        "not_", kwds.not_;
        "of_", kwds.of_;
        "option", kwds.option;
        "or_", kwds.or_;
        "or_if_list_empty", kwds.or_if_list_empty;
        "order_ascending", kwds.order_ascending;
        "order_descending", kwds.order_descending;
        "output", kwds.output;
        "rule", kwds.rule;
        "scope", kwds.scope;
        "sort", kwds.sort;
        "state", kwds.state;
        "struct_", kwds.struct_;
        "such", kwds.such;
        "sum", kwds.sum;
        "that", kwds.that;
        "then_", kwds.then_;
        "to_", kwds.to_;
        "true_", kwds.true_;
        "type_", kwds.type_;
        "under_condition", kwds.under_condition;
        "we_have", kwds.we_have;
        "wildcard", kwds.wildcard;
        "with_", kwds.with_;
        "with_v", kwds.with_v;
        "xor", kwds.xor;
        "year", kwds.year;
      ]
  in
  fprintf ppf "@[<v 2>let keywords =@ @[<v 2>{@ %t@;<0 -2>}@]@]" format_keywords;
  let format_specifics ppf =
    let d = lang.specific_delimiters in
    fprintf ppf
      {|let specific_delimiters = {
  decimal_separator = %S;
  money_unit = %S;
  money_unit_position = %s;
  money_delim = %S;
}
|}
      d.decimal_separator d.money_unit
      (if d.money_unit_position = `Left then "`Left" else "`Right")
      d.money_delim
  in
  dl ();
  format_specifics ppf;
  let format_builtins ppf =
    let bt = lang.builtins in
    let format_types ppf =
      let t = bt.types in
      fprintf ppf "let types = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        [
          "boolean", t.boolean;
          "date", t.date;
          "decimal", t.decimal;
          "duration", t.duration;
          "integer", t.integer;
          "money", t.money;
          "position", t.position;
        ]
    in
    format_types ppf;
    let format_optionals ppf =
      let o = bt.optionals in
      fprintf ppf "let optionals = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        ["present", o.present; "absent", o.absent]
    in
    format_optionals ppf;
    let format_functions ppf =
      let f = bt.functions in
      fprintf ppf "let functions = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        ["round", f.round; "impossible", f.impossible; "cardinal", f.cardinal]
    in
    format_functions ppf;
    fprintf ppf "{ types ; optionals ; functions }"
  in
  dl ();
  fprintf ppf "@[<v 2>let builtins =@ %t@]" format_builtins;
  dl ();
  let format_directives ppf =
    let d = lang.directives in
    fprintf ppf
      {|let directives = {
  external_  = %S;
  law_include = %S;
  module_alias = %S;
  module_def = %S;
  module_use = %S;
}
|}
      d.external_ d.law_include d.module_alias d.module_def d.module_use
  in
  format_directives ppf;
  fprintf ppf
    {|let language = {
  name = %S;
  code = %S;
  keywords;
  specific_delimiters;
  builtins;
  directives;
}
|}
    lang.name lang.code;
  fprintf ppf "@\n";
  dl ();
  fprintf ppf "type Languages.t += T";
  fprintf ppf "@\n";
  fprintf ppf "let runtime_lang : Catala_runtime.Print.lang = `%s"
    (String.capitalize_ascii lang.code);
  fprintf ppf "@\n";
  fprintf ppf "let () = Languages.register_language (T, language)";
  fprintf ppf "@\n"
