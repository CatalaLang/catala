(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type t = ..
type code = t
type lang = Locale.language

let langs : (code * lang) list ref = ref []
let register_language (kind, lang) = langs := (kind, lang) :: !langs
let languages () = !langs

let file_lang path =
  let filename = Filename.basename path in
  let comps = List.rev (String.split_on_char '.' filename) in
  match comps with
  | "md" :: ext :: _ | ext :: _ ->
    begin match String.split_on_char '_' ext with
    | ["catala"; lang_ext] ->
      List.find_opt (fun (_, (l : lang)) -> l.code = lang_ext) (languages ())
    | _ -> None
    end
  | [] -> None

let code_lang c = List.assoc_opt c (languages ())
