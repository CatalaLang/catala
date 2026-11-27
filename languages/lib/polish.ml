(* This file was automatically generated from the corresponding JSON file: do not edit manually. *)

open Locale

let keywords =
  {
    all = "wszystkie";
    among = "w\197\155r\195\179d";
    and_ = "i";
    and_then = "and then";
    assertion = "asercja";
    but_replace = "ale zast\196\133pi\196\135";
    combine = "po\197\130\196\133czy\196\135";
    condition = "warunek";
    consequence = "konsekwencja";
    contains = "zawiera";
    content = "typu";
    context = "kontekst";
    data = "dane";
    day = "dzie\197\132";
    declaration = "deklaracja";
    decreasing = "d\195\179\197\130";
    defined_as = "wynosi";
    definition = "definicja";
    depends = "zale\197\188y od";
    else_ = "inaczej";
    enum = "enumeracja";
    exception_ = "wyj\196\133tek";
    exists = "istnieje";
    false_ = "fa\197\130sz";
    filled = "spe\197\130nione";
    for_ = "dla";
    if_ = "je\197\188eli";
    in_ = "w";
    increasing = "g\195\179r\196\153";
    initially = "pocz\196\133tkowo";
    input = "wej\197\155cie";
    internal = "wewn\196\153trzny";
    is = "jest";
    label = "etykieta";
    let_ = "niech";
    list = "lista";
    map_each = "plan ka\197\188dy";
    match_ = "pasuje";
    maximum = "maksimum";
    minimum = "minimum";
    month = "miesi\196\133c";
    not_ = "nie";
    of_ = "z";
    option = "opcjonalny";
    or_ = "lub";
    or_if_list_empty = "lub je\197\188eli lista pusta";
    order_increasing = "by increasing order";
    order_decreasing = "by decreasing order";
    output = "wyj\197\155cie";
    rule = "zasada";
    scope = "zakres";
    sort = "sort";
    state = "stan";
    struct_ = "struktura";
    such = "takie \197\188e";
    sum = "suma";
    that = "to";
    then_ = "wtedy";
    to_ = "do";
    true_ = "prawda";
    type_ = "rodzaj";
    under_condition = "pod warunkiem";
    we_have = "mamy";
    wildcard = "cokolwiek";
    with_ = "ze wzorem";
    with_v = "wraz z";
    xor = "albo";
    year = "rok";
  }

let specific_delimiters =
  {
    decimal_separator = ".";
    money_unit = "PLN";
    money_unit_position = `Right;
    money_delim = ",";
  }

let builtins =
  let types =
    {
      boolean = "zerojedynkowy";
      date = "czas";
      decimal = "dziesi\196\153tny";
      duration = "czastrwania";
      integer = "ca\197\130kowita";
      money = "pieni\196\133dze";
      position = "code_location";
    }
  in
  let optionals = { present = "Obecny"; absent = "Nieobecny" } in
  let functions =
    {
      round = "zaokr\196\133glony";
      impossible = "niemo\197\188liwe";
      cardinal = "liczba";
    }
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
    name = "polish";
    code = "pl";
    keywords;
    specific_delimiters;
    builtins;
    directives;
  }

type Languages.t += T

let runtime_lang : Catala_runtime.Print.lang = `Pl
let () = Languages.register_language (T, language)
