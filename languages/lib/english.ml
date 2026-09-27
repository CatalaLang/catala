open Language_t

module L = struct
  let keywords =
    {
      all = "all";
      among = "among";
      and_ = "and";
      and_then = "and then";
      assertion = "assertion";
      but_replace = "but replace";
      combine = "combine";
      condition = "condition";
      consequence = "consequence";
      contains = "contains";
      content = "content";
      context = "context";
      data = "data";
      day = "day";
      declaration = "declaration";
      decreasing = "down";
      defined_as = "equals";
      definition = "definition";
      depends = "depends on";
      else_ = "else";
      enum = "enumeration";
      exception_ = "exception";
      exists = "exists";
      false_ = "false";
      filled = "fulfilled";
      for_ = "for";
      if_ = "if";
      in_ = "in";
      increasing = "up";
      initially = "initially";
      input = "input";
      internal = "internal";
      is = "is";
      label = "label";
      let_ = "let";
      list = "list of";
      map_each = "map each";
      match_ = "match";
      maximum = "maximum";
      minimum = "minimum";
      month = "month";
      not_ = "not";
      of_ = "of";
      option = "optional of";
      or_ = "or";
      or_if_list_empty = "or if list empty";
      order_ascending = "in increasing order";
      order_descending = "in decreasing order";
      output = "output";
      rule = "rule";
      scope = "scope";
      sort = "sort";
      state = "state";
      struct_ = "structure";
      such = "such";
      sum = "sum";
      that = "that";
      then_ = "then";
      to_ = "to";
      true_ = "true";
      type_ = "type";
      under_condition = "under condition";
      we_have = "we have";
      wildcard = "anything";
      with_ = "with pattern";
      with_v = "with";
      xor = "xor";
      year = "year";
    }

  let specific_delimiters =
    {
      decimal_separator = ".";
      money_unit = "$";
      money_unit_position = `Left;
      money_delim = ",";
    }

  let builtins =
    let types =
      {
        boolean = "boolean";
        date = "date";
        decimal = "decimal";
        duration = "duration";
        integer = "integer";
        money = "money";
        position = "code_location";
      }
    in
    let optionals = { present = "Present"; absent = "Absent" } in
    let functions =
      { round = "round"; impossible = "impossible"; cardinal = "number" }
    in
    { types; optionals; functions }

  let directives =
    {
      external_ = "external";
      law_include = "Include";
      module_alias = "as";
      module_def = "Module";
      module_use = "Using";
    }

  let language =
    {
      lang_name = "english";
      file_ext_suffix = "en";
      keywords;
      specific_delimiters;
      builtins;
      directives;
    }

  module Lexer = Surface.Lexer_en
end

include L

let () = Langs.register_language (module L)
