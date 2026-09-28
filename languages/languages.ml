type t = ..
type code = t
type lang = Language_t.language

let langs : (code * lang) list ref = ref []
let register_language (kind, lang) = langs := (kind, lang) :: !langs
let languages () = !langs

let file_lang path =
  let filename = Filename.basename path in
  let comps = List.rev (String.split_on_char '.' filename) in
  match comps with
  | "md" :: ext :: _ | ext :: _ -> begin
    match String.split_on_char '_' ext with
    | ["catala"; lang_ext] ->
      List.find_opt (fun (_, (l : lang)) -> l.code = lang_ext) (languages ())
    | _ -> None
  end
  | [] -> None

let code_lang c = List.assoc_opt c (languages ())
