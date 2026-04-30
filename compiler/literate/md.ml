(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This modules weaves the source code and the legislative text together into a
    document that law professionals can understand. *)

open Catala_utils
open Literate_common
module A = Surface.Ast
module P = Printf
module R = Re.Pcre
module C = Global

(** {1 Weaving} *)

let wrap_markdown
    (source_files : string list)
    (language : Global.backend_lang)
    (fmt : Format.formatter)
    (wrapped : Format.formatter -> unit) : unit =
  Format.fprintf fmt "%s@.@.%s@.@.%s:@.@.%a@.@." (literal_title language)
    (literal_generated_by language)
    (literal_source_files language)
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@.@.")
       (fun ppf filename ->
         let mtime = (Unix.stat filename).Unix.st_mtime in
         let ltime = Unix.localtime mtime in
         let ftime =
           Printf.sprintf "%d-%02d-%02d, %d:%02d"
             (1900 + ltime.Unix.tm_year)
             (ltime.Unix.tm_mon + 1) ltime.Unix.tm_mday ltime.Unix.tm_hour
             ltime.Unix.tm_min
         in
         Format.fprintf ppf "* %s, %s %s"
           (Filename.basename filename)
           (literal_last_modification language)
           ftime))
    source_files;
  wrapped fmt

let sanitize_md_href str =
  str
  |> String.to_ascii
  |> R.substitute
       ~rex:(R.regexp "[' '°\"!%(),/]")
       ~subst:
         (String.fold_left
            (fun acc c -> Format.sprintf "%s%d" acc (Char.code c))
            "")

let rec separate_law_and_code
    (language : C.backend_lang)
    (print_only_law : bool)
    (parents_headings : string list)
    (fmt : Format.formatter)
    (i : A.law_structure) : unit =
  match i with
  | A.LawText t -> if t = "" then () else Format.fprintf fmt "%s\n" t
  | A.CodeBlock (_, c, _metadata) when not print_only_law ->
    let start_line = Pos.get_start_line (Mark.get c) + 1 in
    let filename = Pos.get_file (Mark.get c) in
    let block_content = Mark.remove c in
    check_exceeding_lines start_line filename block_content;
    Format.fprintf fmt "[%s]{.filename}\n\n%s@\n"
      (Pos.get_file (Mark.get c))
      ("~~~~~~~catala\n" ^ Mark.remove c ^ "~~~~~~~\n")
  | A.CodeBlock _ -> ()
  | A.LawHeading (heading, children) ->
    let h_number = heading.law_heading_precedence + 1 in
    let is_a_section_to_collapse =
      (* Only 2 depth sections are collasped in a <details> tag. Indeed, this
         allow to significantly reduce rendering time (~= 100x for the
         [aides_logement] example in the catala-website), while remaining
         practicable. *)
      h_number = 2
    in
    let h_name = Mark.remove heading.law_heading_name in
    let complete_headings = parents_headings @ [h_name] in
    let id = complete_headings |> String.concat "-" |> sanitize_md_href in
    let fmt_details_open fmt () =
      if is_a_section_to_collapse then
        Format.fprintf fmt "<details><summary>%s</summary>" h_name
      else Format.fprintf fmt "%s" h_name
    in
    let fmt_details_close fmt () =
      if is_a_section_to_collapse then Format.fprintf fmt "</details>"
    in
    Format.fprintf fmt "%s [%s](#%s){#%s .law-heading}@.%a%s@\n%a\n%a@."
      (String.make h_number '#') h_name id id fmt_details_open ()
      (match heading.law_heading_id, language with
      | Some id, Fr -> (
        try
          P.sprintf
            "[Voir le texte sur \
             Légifrance.gouv.fr](https://legifrance.gouv.fr/%s/id/%s){.link-article}"
            (if String.starts_with ~prefix:"LEGIARTI" id then "codes"
             else if String.starts_with ~prefix:"JORFARTI" id then "jorf"
             else if String.starts_with ~prefix:"CETATEXT" id then "ceta"
             else raise Not_found)
            id
        with Not_found -> "")
      | _ -> "")
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
         (separate_law_and_code language print_only_law complete_headings))
      children fmt_details_close ()
  | A.LawInclude _ -> ()
  | A.ModuleDef _ | A.ModuleUse _ -> () (* TODO: show somehow ?*)

let rec fmt_toc
    (parents_headings : string list)
    fmt
    (items : A.law_structure list) =
  let items =
    items
    |> List.filter (function A.LawHeading (_, _) -> true | _ -> false)
    |> List.mapi (fun i item -> i + 1, item)
  in
  match items with
  | [] -> ()
  | items ->
    Format.fprintf fmt "@,%a"
      (Format.pp_print_list (fun fmt (i, item) ->
           match item with
           | A.LawHeading (heading, childs) ->
             let h_name = Mark.remove heading.law_heading_name in
             let complete_headings = parents_headings @ [h_name] in
             let id =
               complete_headings |> String.concat "-" |> sanitize_md_href
             in
             Format.fprintf fmt "@[<v 3>%d. [%s](#%s){.toc-item}%a@]" i h_name
               id
               (fmt_toc complete_headings)
               childs
           | _ -> ()))
      items

(** {1 API} *)

let ast_to_markdown
    (language : C.backend_lang)
    ~(print_only_law : bool)
    (fmt : Format.formatter)
    (program : A.program) : unit =
  let toc =
    match language with
    | C.Fr -> "Sommaire"
    | C.En -> "Table of contents"
    | C.Pl -> "Spis treści."
  in

  Format.fprintf fmt
    "<details><summary>%s</summary>@.@[<v 2>%a@]@,</details>@]@.@.%a" toc
    (fmt_toc []) program.program_items
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       (fun fmt ->
         Format.fprintf fmt "%a"
           (separate_law_and_code language print_only_law [])))
    program.program_items
