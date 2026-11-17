(* Auto-generated from "language.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type types = {
  boolean: string;
  date: string;
  decimal: string;
  duration: string;
  integer: string;
  money: string;
  position: string
}

type money_unit_position = [ `Left | `Right ]

type specific_delimiters = {
  decimal_separator: string;
  money_unit: string;
  money_unit_position: money_unit_position;
  money_delim: string
}

type optionals = { present: string; absent: string }

type keywords = {
  all: string;
  among: string;
  and_: string;
  and_then: string;
  assertion: string;
  but_replace: string;
  combine: string;
  condition: string;
  consequence: string;
  contains: string;
  content: string;
  context: string;
  data: string;
  day: string;
  declaration: string;
  decreasing: string;
  defined_as: string;
  definition: string;
  depends: string;
  else_: string;
  enum: string;
  exception_: string;
  exists: string;
  false_: string;
  filled: string;
  for_: string;
  if_: string;
  in_: string;
  increasing: string;
  initially: string;
  input: string;
  internal: string;
  is: string;
  label: string;
  let_: string;
  list: string;
  map_each: string;
  match_: string;
  maximum: string;
  minimum: string;
  month: string;
  not_: string;
  of_: string;
  option: string;
  or_: string;
  or_if_list_empty: string;
  order_increasing: string;
  order_decreasing: string;
  output: string;
  rule: string;
  scope: string;
  sort: string;
  state: string;
  struct_: string;
  such: string;
  sum: string;
  that: string;
  then_: string;
  to_: string;
  true_: string;
  type_: string;
  under_condition: string;
  we_have: string;
  wildcard: string;
  with_: string;
  with_v: string;
  xor: string;
  year: string
}

type functions = { round: string; impossible: string; cardinal: string }

type directives = {
  external_: string;
  law_include: string;
  module_alias: string;
  module_def: string;
  module_use: string
}

type builtins = { types: types; optionals: optionals; functions: functions }

type language = {
  name: string;
  code: string;
  keywords: keywords;
  specific_delimiters: specific_delimiters;
  builtins: builtins;
  directives: directives
}
