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
open Language_t

(** This file generates an OCaml module per syntax file. *)

let () =
  let c = contents Sys.argv.(1) in
  (* Expanding all records for exhaustivity check *)
  let { name; code; keywords; specific_delimiters; builtins; directives } =
    Lexing.from_string c |> Language_j.read_language (Yojson.init_lexer ())
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
  let ppf = Format.std_formatter in
  let dl () =
    pp_print_newline ppf ();
    pp_print_newline ppf ()
  in
  fprintf ppf
    "(* This file was automatically generated from the corresponding JSON \
     file: do not edit manually. *)";
  dl ();
  fprintf ppf "open Locale";
  dl ();
  let format_field_def ppf (name, s) = fprintf ppf "%s = %S" name s in
  let format_keywords ppf =
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") format_field_def)
      ppf
      [
        "all", all;
        "among", among;
        "and_", and_;
        "and_then", and_then;
        "assertion", assertion;
        "but_replace", but_replace;
        "combine", combine;
        "condition", condition;
        "consequence", consequence;
        "contains", contains;
        "content", content;
        "context", context;
        "data", data;
        "day", day;
        "declaration", declaration;
        "decreasing", decreasing;
        "defined_as", defined_as;
        "definition", definition;
        "depends", depends;
        "else_", else_;
        "enum", enum;
        "exception_", exception_;
        "exists", exists;
        "false_", false_;
        "filled", filled;
        "for_", for_;
        "if_", if_;
        "in_", in_;
        "increasing", increasing;
        "initially", initially;
        "input", input;
        "internal", internal;
        "is", is;
        "label", label;
        "let_", let_;
        "list", list;
        "map_each", map_each;
        "match_", match_;
        "maximum", maximum;
        "minimum", minimum;
        "month", month;
        "not_", not_;
        "of_", of_;
        "option", option;
        "or_", or_;
        "or_if_list_empty", or_if_list_empty;
        "order_increasing", order_increasing;
        "order_decreasing", order_decreasing;
        "output", output;
        "rule", rule;
        "scope", scope;
        "sort", sort;
        "state", state;
        "struct_", struct_;
        "such", such;
        "sum", sum;
        "that", that;
        "then_", then_;
        "to_", to_;
        "true_", true_;
        "type_", type_;
        "under_condition", under_condition;
        "we_have", we_have;
        "wildcard", wildcard;
        "with_", with_;
        "with_v", with_v;
        "xor", xor;
        "year", year;
      ]
  in
  fprintf ppf "@[<v 2>let keywords =@ @[<v 2>{@ %t@;<0 -2>}@]@]" format_keywords;
  let format_specifics ppf =
    fprintf ppf
      {|let specific_delimiters = {
  decimal_separator = %S;
  money_unit = %S;
  money_unit_position = %s;
  money_delim = %S;
}
|}
      decimal_separator money_unit
      (if money_unit_position = `Left then "`Left" else "`Right")
      money_delim
  in
  dl ();
  format_specifics ppf;
  let format_builtins ppf =
    let format_types ppf =
      fprintf ppf "let types = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        [
          "boolean", boolean;
          "date", date;
          "decimal", decimal;
          "duration", duration;
          "integer", integer;
          "money", money;
          "position", position;
        ]
    in
    format_types ppf;
    let format_optionals ppf =
      fprintf ppf "let optionals = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        ["present", present; "absent", absent]
    in
    format_optionals ppf;
    let format_functions ppf =
      fprintf ppf "let functions = @[<v 2>{@ %a@;<0 -2>} in@]@ "
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           format_field_def)
        ["round", round; "impossible", impossible; "cardinal", cardinal]
    in
    format_functions ppf;
    fprintf ppf "{ types ; optionals ; functions }"
  in
  dl ();
  fprintf ppf "@[<v 2>let builtins =@ %t@]" format_builtins;
  dl ();
  let format_directives ppf =
    fprintf ppf
      {|let directives = {
  external_  = %S;
  law_include = %S;
  module_alias = %S;
  module_def = %S;
  module_use = %S;
}
|}
      external_ law_include module_alias module_def module_use
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
    name code;
  fprintf ppf "@\n";
  dl ();
  fprintf ppf "type Languages.t += T";
  fprintf ppf "@\n";
  let runtime_variant_cstr =
    (* FIXME: this is used for the runtime value printer. Currently,
       the runtime doesn't support the rest. *)
    match String.capitalize_ascii code with
    | ("En" | "Fr" | "Pl") as x -> x
    | _ -> "En"
  in
  fprintf ppf "let runtime_lang : Catala_runtime.Print.lang = `%s"
    runtime_variant_cstr;
  fprintf ppf "@\n";
  fprintf ppf "let () = Languages.register_language (T, language)";
  fprintf ppf "@\n"
