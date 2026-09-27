open Language_t

module L = struct
  let keywords =
    {
      all = "tout";
      among = "parmi";
      and_ = "et";
      and_then = "puis";
      assertion = "assertion";
      but_replace = "mais en rempla\195\167ant";
      combine = "combine";
      condition = "condition";
      consequence = "cons\195\169quence";
      contains = "contient";
      content = "contenu";
      context = "contexte";
      data = "donn\195\169e";
      day = "jour";
      declaration = "d\195\169claration";
      decreasing = "inf\195\169rieur";
      defined_as = "\195\169gal \195\160";
      definition = "d\195\169finition";
      depends = "d\195\169pend de";
      else_ = "sinon";
      enum = "\195\169num\195\169ration";
      exception_ = "exception";
      exists = "existe";
      false_ = "faux";
      filled = "rempli";
      for_ = "pour";
      if_ = "si";
      in_ = "dans";
      increasing = "sup\195\169rieur";
      initially = "initialement";
      input = "entr\195\169e";
      internal = "interne";
      is = "est";
      label = "\195\169tiquette";
      let_ = "soit";
      list = "liste de";
      map_each = "transforme chaque";
      match_ = "selon";
      maximum = "maximum";
      minimum = "minimum";
      month = "mois";
      not_ = "non";
      of_ = "de";
      option = "optionnel de";
      or_ = "ou";
      or_if_list_empty = "ou si liste vide";
      order_ascending = "par ordre croissant";
      order_descending = "par ordre d\195\169croissant";
      output = "r\195\169sultat";
      rule = "r\195\168gle";
      scope = "champ d'application";
      sort = "trie";
      state = "\195\169tat";
      struct_ = "structure";
      such = "tel";
      sum = "somme";
      that = "que";
      then_ = "alors";
      to_ = "en";
      true_ = "vrai";
      type_ = "type";
      under_condition = "sous condition";
      we_have = "on a";
      wildcard = "n'importe quel";
      with_ = "sous forme";
      with_v = "avec";
      xor = "ou bien";
      year = "an";
    }

  let specific_delimiters =
    {
      decimal_separator = ",";
      money_unit = "\226\130\172";
      money_unit_position = `Right;
      money_delim = " ";
    }

  let builtins =
    let types =
      {
        boolean = "bool\195\169en";
        date = "date";
        decimal = "d\195\169cimal";
        duration = "dur\195\169e";
        integer = "entier";
        money = "argent";
        position = "position_source";
      }
    in
    let optionals = { present = "Pr\195\169sent"; absent = "Absent" } in
    let functions =
      { round = "arrondi"; impossible = "impossible"; cardinal = "nombre" }
    in
    { types; optionals; functions }

  let directives =
    {
      external_ = "externe";
      law_include = "Inclusion";
      module_alias = "en tant que";
      module_def = "Module";
      module_use = "Usage de";
    }

  let language =
    {
      lang_name = "french";
      file_ext_suffix = "fr";
      keywords;
      specific_delimiters;
      builtins;
      directives;
    }

  module Lexer = Surface.Lexer_fr
end

include L

let () = Langs.register_language (module L)
