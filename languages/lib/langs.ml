type lang = (module Sig.Language)

let langs : lang list ref = ref []

let register_language (module Lang : Sig.Language) =
  langs := (module Lang) :: !langs

let all_languages () = !langs
