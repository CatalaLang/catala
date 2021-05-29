(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** This modules weaves the source code and the legislative text together into a document that law
    professionals can understand. *)

open Utils
open Literate_common
module A = Surface.Ast
module P = Printf
module R = Re.Pcre
module C = Cli

(** {1 Helpers} *)

(** Converts double lines into HTML newlines. *)
let pre_html (s : string) =
  let s = String.trim s in
  let doublenewline = R.regexp "\n\n" in
  let s = R.substitute ~rex:doublenewline ~subst:(fun _ -> "<br/>\n") s in
  s

(** Raise an error if pygments cannot be found *)
let raise_failed_pygments (command : string) (error_code : int) : 'a =
  Errors.raise_error
    (Printf.sprintf "Weaving to HTML failed: pygmentize command \"%s\" returned with error code %d"
       command error_code)

(** Usage: [wrap_html source_files custom_pygments language fmt wrapped]

    Prints an HTML complete page structure around the [wrapped] content. *)
let wrap_html (source_files : string list) (language : Cli.backend_lang) (fmt : Format.formatter)
    (wrapped : Format.formatter -> unit) : unit =
  let pygments = "pygmentize" in
  let css_file = Filename.temp_file "catala_css_pygments" "" in
  let pygments_args = [| "-f"; "html"; "-S"; "colorful"; "-a"; ".catala-code" |] in
  let cmd =
    Format.sprintf "%s %s > %s" pygments (String.concat " " (Array.to_list pygments_args)) css_file
  in
  let return_code = Sys.command cmd in
  if return_code <> 0 then raise_failed_pygments cmd return_code;
  let oc = open_in css_file in
  let css_as_string = really_input_string oc (in_channel_length oc) in
  close_in oc;
  Format.fprintf fmt
    "<head>\n\
     <style>\n\
     %s\n\
     </style>\n\
     <meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>\n\
     </head>\n\
     <h1>%s<br />\n\
     <small>%s Catala version %s</small>\n\
     </h1>\n\
     <p>\n\
     %s\n\
     </p>\n\
     <ul>\n\
     %s\n\
     </ul>\n"
    css_as_string (literal_title language) (literal_generated_by language) Utils.Cli.version
    (literal_source_files language)
    (String.concat "\n"
       (List.map
          (fun filename ->
            let mtime = (Unix.stat filename).Unix.st_mtime in
            let ltime = Unix.localtime mtime in
            let ftime =
              Printf.sprintf "%d-%02d-%02d, %d:%02d" (1900 + ltime.Unix.tm_year)
                (ltime.Unix.tm_mon + 1) ltime.Unix.tm_mday ltime.Unix.tm_hour ltime.Unix.tm_min
            in
            Printf.sprintf "<li><tt>%s</tt>, %s %s</li>"
              (pre_html (Filename.basename filename))
              (literal_last_modification language)
              ftime)
          source_files));
  wrapped fmt

(** Performs syntax highlighting on a piece of code by using Pygments and the special Catala lexer. *)
let pygmentize_code (c : string Pos.marked) (language : C.backend_lang) : string =
  C.debug_print (Printf.sprintf "Pygmenting the code chunk %s" (Pos.to_string (Pos.get_position c)));
  let temp_file_in = Filename.temp_file "catala_html_pygments" "in" in
  let temp_file_out = Filename.temp_file "catala_html_pygments" "out" in
  let oc = open_out temp_file_in in
  Printf.fprintf oc "%s" (Pos.unmark c);
  close_out oc;
  let pygments = "pygmentize" in
  let pygments_lexer = get_language_extension language in
  let pygments_args =
    [|
      "-l";
      pygments_lexer;
      "-f";
      "html";
      "-O";
      "style=colorful,anchorlinenos=True,lineanchors=\""
      ^ Pos.get_file (Pos.get_position c)
      ^ "\",linenos=table,linenostart="
      ^ string_of_int (Pos.get_start_line (Pos.get_position c));
      "-o";
      temp_file_out;
      temp_file_in;
    |]
  in
  let cmd = Format.asprintf "%s %s" pygments (String.concat " " (Array.to_list pygments_args)) in
  let return_code = Sys.command cmd in
  if return_code <> 0 then raise_failed_pygments cmd return_code;
  let oc = open_in temp_file_out in
  let output = really_input_string oc (in_channel_length oc) in
  close_in oc;
  output

(** {1 Weaving} *)

let rec law_structure_to_html (language : C.backend_lang) (fmt : Format.formatter)
    (i : A.law_structure) : unit =
  match i with
  | A.LawText t ->
      let t = pre_html t in
      if t = "" then () else Format.fprintf fmt "<p class='law-text'>%s</p>" t
  | A.CodeBlock (_, c, metadata) ->
      let date = "\\d\\d/\\d\\d/\\d\\d\\d\\d" in
      let syms = R.regexp (date ^ "|!=|<=|>=|--|->|\\*|\\/") in
      let syms_subst = function
        | "!=" -> "≠"
        | "<=" -> "≤"
        | ">=" -> "≥"
        | "--" -> "—"
        | "->" -> "→"
        | "*" -> "×"
        | "/" -> "÷"
        | s -> s
      in
      let pprinted_c = R.substitute ~rex:syms ~subst:syms_subst (Pos.unmark c) in
      Format.fprintf fmt "<div class='code-wrapper%s'>\n<div class='filename'>%s</div>\n%s\n</div>"
        (if metadata then " code-metadata" else "")
        (Pos.get_file (Pos.get_position c))
        (pygmentize_code (Pos.same_pos_as ("```catala" ^ pprinted_c ^ "```") c) language)
  | A.LawHeading (heading, children) ->
      let h_number = heading.law_heading_precedence + 1 in
      Format.fprintf fmt "<h%d class='law-heading'><a href='%s'>%s</a></h%d>\n" h_number
        (match (heading.law_heading_id, language) with
        | Some id, Fr ->
            let ltime = Unix.localtime (Unix.time ()) in
            P.sprintf "https://legifrance.gouv.fr/codes/id/%s/%d-%02d-%02d" id
              (1900 + ltime.Unix.tm_year) (ltime.Unix.tm_mon + 1) ltime.Unix.tm_mday
        | _ -> "#")
        (pre_html (Pos.unmark heading.law_heading_name))
        h_number;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
        (law_structure_to_html language) fmt children
  | A.LawInclude _ -> ()

(** {1 API} *)

let ast_to_html (language : C.backend_lang) (fmt : Format.formatter) (program : A.program) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
    (law_structure_to_html language) fmt program.program_items
