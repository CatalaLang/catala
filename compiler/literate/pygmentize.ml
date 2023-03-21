(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Literate_common

let lang_of_ext s =
  if String.starts_with ~prefix:"catala_" s then
    match s with
    | "catala_en" -> Some Cli.En
    | "catala_fr" -> Some Cli.Fr
    | "catala_pl" -> Some Cli.Pl
    | _ -> failwith "Unknown Catala dialect"
  else None

let exec () =
  let args = List.tl (Array.to_list Sys.argv) in
  let rec find_lang acc = function
    | "-l" :: lang :: r -> Some lang, List.rev_append acc r
    | x :: r -> find_lang (x :: acc) r
    | [] -> None, List.rev acc
  in
  let lang, args = find_lang [] args in
  let catala_lang =
    match lang with
    | Some l -> lang_of_ext l
    | None ->
      List.find_map
        (fun s ->
          match Filename.extension s with
          | "" -> None
          | e -> lang_of_ext (String.sub e 1 (String.length e - 1)))
        args
  in
  match catala_lang with
  | None -> Unix.execvp "pygmentize" (Array.of_list args)
  | Some lang ->
    with_pygmentize_lexer lang
    @@ fun lex_args ->
    Unix.execvp "pygmentize"
      (Array.of_list (("pygmentize" :: lex_args) @ List.tl args))
