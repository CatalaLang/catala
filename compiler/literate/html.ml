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
module C = Cli

(** {1 Helpers} *)

(** Converts double lines into HTML newlines. *)
let pre_html (s : string) = String.trim (run_pandoc s `Html)

(** Partial application allowing to remove first code lines of
    [<td class="code">] and [<td class="linenos">] generated HTML. Basically,
    remove all code block first lines. *)
let remove_cb_first_lines : string -> string =
  R.substitute ~rex:(R.regexp "<pre>.*\n") ~subst:(function _ -> "<pre>\n")

(** Partial application allowing to remove last code lines of
    [<td class="code">] and [<td class="linenos">] generated HTML. Basically,
    remove all code block last lines. *)
let remove_cb_last_lines : string -> string =
  R.substitute ~rex:(R.regexp "<.*\n*</pre>") ~subst:(function _ -> "</pre>")

(** Usage: [wrap_html source_files custom_pygments language fmt wrapped]

    Prints an HTML complete page structure around the [wrapped] content. *)
let wrap_html
    (source_files : string list)
    (language : Cli.backend_lang)
    (fmt : Format.formatter)
    (wrapped : Format.formatter -> unit) : unit =
  let css_as_string =
    call_pygmentize ["-f"; "html"; "-S"; "default"; "-a"; ".catala-code"]
  in
  Format.fprintf fmt
    "<!DOCTYPE html>\n\
     <html>\n\
     <head>\n\
     <style>\n\
     %s\n\
     </style>\n\
     <meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>\n\
     </head>\n\
     <body>\n\
     <h1>%s<br />\n\
     <small>%s Catala version %s</small>\n\
     </h1>\n\
     %s\n\
     <p>\n\
     %s:\n\
     </p>\n\
     <ul>\n\
     %s\n\
     </ul>\n"
    css_as_string (literal_title language)
    (literal_generated_by language)
    Cli.version
    (pre_html (literal_disclaimer_and_link language))
    (literal_source_files language)
    (String.concat "\n"
       (List.map
          (fun filename ->
            let mtime = (Unix.stat filename).Unix.st_mtime in
            let ltime = Unix.localtime mtime in
            let ftime =
              Printf.sprintf "%d-%02d-%02d, %d:%02d"
                (1900 + ltime.Unix.tm_year)
                (ltime.Unix.tm_mon + 1) ltime.Unix.tm_mday ltime.Unix.tm_hour
                ltime.Unix.tm_min
            in
            Printf.sprintf "<li><tt>%s</tt>, %s %s</li>"
              (Filename.basename filename)
              (literal_last_modification language)
              ftime)
          source_files));
  wrapped fmt;
  Format.fprintf fmt "</body>\n</html>\n"

(** Performs syntax highlighting on a piece of code by using Pygments and the
    special Catala lexer. *)
let pygmentize_code (c : string Mark.pos) (lang : C.backend_lang) : string =
  Message.emit_debug "Pygmenting the code chunk %s" (Pos.to_string (Mark.get c));
  let output =
    File.with_temp_file "catala_html_pygments" "in" ~contents:(Mark.remove c)
    @@ fun temp_file_in ->
    call_pygmentize ~lang
      [
        "-f";
        "html";
        "-O";
        "anchorlinenos=True,lineanchors="
        ^ String.to_ascii (Pos.get_file (Mark.get c))
        ^ ",linenos=table,linenostart="
        ^ string_of_int (Pos.get_start_line (Mark.get c));
        temp_file_in;
      ]
  in
  (* Remove code blocks delimiters needed by [Pygments]. *)
  let trimmed_output =
    output |> remove_cb_first_lines |> remove_cb_last_lines
  in
  trimmed_output

(** {1 Weaving} *)

let sanitize_html_href str =
  str
  |> String.to_ascii
  |> R.substitute ~rex:(R.regexp "[' '°\"]") ~subst:(function _ -> "%20")

let rec law_structure_to_html
    (language : C.backend_lang)
    (print_only_law : bool)
    (parents_headings : string list)
    (fmt : Format.formatter)
    (i : A.law_structure) : unit =
  match i with
  | A.LawText t ->
    let t = pre_html t in
    if t = "" then () else Format.fprintf fmt "<div class='law-text'>%s</div>" t
  | A.CodeBlock (_, c, metadata) when not print_only_law ->
    let start_line = Pos.get_start_line (Mark.get c) - 1 in
    let filename = Pos.get_file (Mark.get c) in
    let block_content = Mark.remove c in
    check_exceeding_lines start_line filename block_content;
    Format.fprintf fmt
      "<div class='code-wrapper%s catala-code'>\n\
       <div class='filename'>%s</div>\n\
       %s\n\
       </div>"
      (if metadata then " code-metadata" else "")
      (Pos.get_file (Mark.get c))
      (pygmentize_code
         (Mark.copy c ("```catala\n" ^ Mark.remove c ^ "```"))
         language)
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
    let id = complete_headings |> String.concat "-" |> sanitize_html_href in
    let fmt_details_open fmt () =
      if is_a_section_to_collapse then
        Format.fprintf fmt "<details><summary>%s</summary>" h_name
    in
    let fmt_details_close fmt () =
      if is_a_section_to_collapse then Format.fprintf fmt "</details>"
    in
    Format.fprintf fmt
      "<h%d class='law-heading' id=\"%s\"><a href=\"#%s\">%s</a>%s</h%d>@\n\
       %a\n\
       %a\n\
       %a"
      h_number id id h_name
      (match heading.law_heading_id, language with
      | Some id, Fr -> (
        try
          P.sprintf
            "<a class=\"link-article\" \
             href=\"https://legifrance.gouv.fr/%s/id/%s\" \
             target=\"_blank\">Voir le texte sur Légifrance.gouv.fr</a>"
            (if String.starts_with ~prefix:"LEGIARTI" id then "codes"
             else if String.starts_with ~prefix:"JORFARTI" id then "jorf"
             else if String.starts_with ~prefix:"CETATEXT" id then "ceta"
             else raise Not_found)
            id
        with Not_found -> "")
      | _ -> "")
      h_number fmt_details_open ()
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
         (law_structure_to_html language print_only_law complete_headings))
      children fmt_details_close ()
  | A.LawInclude _ -> ()

let rec fmt_toc
    (parents_headings : string list)
    fmt
    (items : A.law_structure list) =
  Format.fprintf fmt "@[<v 2><ol class=\"toc-%d\">@\n%a@\n@]</ol>"
    (List.length parents_headings)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt item ->
         match item with
         | A.LawHeading (heading, childs) ->
           let h_name = Mark.remove heading.law_heading_name in
           let complete_headings = parents_headings @ [h_name] in
           let id =
             complete_headings |> String.concat "-" |> sanitize_html_href
           in
           Format.fprintf fmt
             "@[<hov 2><li class=\"toc-item\">@\n\
              @[<hov 2><div>@\n\
              <a href=\"#%s\">%s</a>@\n\
              %a@\n\
              @]</div>@\n\
              @]</li>"
             id h_name
             (fmt_toc complete_headings)
             childs
         | _ -> ()))
    (items |> List.filter (function A.LawHeading (_, _) -> true | _ -> false))

(** {1 API} *)

let ast_to_html
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
    "@[<hov 2><details class=\"toc\">@\n\
     <summary>%s</summary>@\n\
     %a@\n\
     @]</details>\n\
     %a@\n"
    toc (fmt_toc []) program.program_items
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       (fun fmt ->
         Format.fprintf fmt "%a"
           (law_structure_to_html language print_only_law [])))
    program.program_items
