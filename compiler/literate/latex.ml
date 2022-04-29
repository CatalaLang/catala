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

open Utils
open Literate_common
module A = Surface.Ast
module R = Re.Pcre
module C = Cli

(** {1 Helpers} *)

(** Espaces various LaTeX-sensitive characters *)
let pre_latexify (s : string) : string =
  let substitute s (old_s, new_s) =
    R.substitute ~rex:(R.regexp old_s) ~subst:(fun _ -> new_s) s
  in
  [
    ("\\$", "\\$");
    ("%", "\\%");
    ("\\_", "\\_");
    ("\\#", "\\#");
    ("1er", "1\\textsuperscript{er}");
    ("\\^", "\\textasciicircum");
  ]
  |> List.fold_left substitute s

(** Usage: [wrap_latex source_files custom_pygments language fmt wrapped]

    Prints an LaTeX complete documùent structure around the [wrapped] content. *)
let wrap_latex
    (source_files : string list)
    (language : C.backend_lang)
    (fmt : Format.formatter)
    (wrapped : Format.formatter -> unit) =
  Format.fprintf fmt
    "\\documentclass[%s, 11pt, a4paper]{article}\n\n\
     \\usepackage[T1]{fontenc}\n\
     \\usepackage[utf8]{inputenc}\n\
     \\usepackage{amssymb}\n\
     \\usepackage{babel}\n\
     \\usepackage{lmodern}\n\
     \\usepackage{minted}\n\
     \\usepackage{newunicodechar}\n\
     \\usepackage{textcomp}\n\
     \\usepackage[hidelinks]{hyperref}\n\
     \\usepackage[dvipsnames]{xcolor}\n\
     \\usepackage[left=2cm,right=2cm,top=3cm,bottom=3cm]{geometry}\n\
     \\usepackage[many]{tcolorbox}\n\n\
     \\usepackage{fancyhdr}\n\
     \\pagestyle{fancy}\n\
     \\fancyhf{}\n\
     \\fancyhead[C]{\\leftmark}\n\
     \\fancyfoot[C]{\\thepage}\n\
     \\renewcommand{\\headrulewidth}{0.5pt}\n\
     \\renewcommand{\\footrulewidth}{0.5pt}\n\
     \\usepackage{titlesec}\n\
     \\titleclass{\\subsubsubsection}{straight}[\\subsection]\n\
     \\newcounter{subsubsubsection}[subsubsection]\n\
     \\renewcommand\\thesubsubsubsection{\\thesubsubsection.\\arabic{subsubsubsection}}\n\
     \\titleformat{\\subsubsubsection}{\\normalfont\\normalsize\\bfseries}{\\thesubsubsubsection}{1em}{}\n\
     \\titlespacing*{\\subsubsubsection}{0pt}{3.25ex plus 1ex minus \
     .2ex}{1.5ex plus .2ex}\n\
     \\titleclass{\\subsubsubsubsection}{straight}[\\subsubsection]\n\
     \\newcounter{subsubsubsubsection}[subsubsubsection]\n\
     \\renewcommand\\thesubsubsubsubsection{\\thesubsubsubsection.\\arabic{subsubsubsubsection}}\n\
     \\titleformat{\\subsubsubsubsection}{\\normalfont\\normalsize\\bfseries}{\\thesubsubsubsubsection}{0.75em}{}\n\
     \\titlespacing*{\\subsubsubsubsection}{0pt}{2.75ex plus 1ex minus \
     .2ex}{1.25ex plus .2ex}\n\
     \\titleclass{\\subsubsubsubsubsection}{straight}[\\subsubsubsection]\n\
     \\newcounter{subsubsubsubsubsection}[subsubsubsubsection]\n\
     \\renewcommand\\thesubsubsubsubsubsection{\\thesubsubsubsubsection.\\arabic{subsubsubsubsubsection}}\n\
     \\titleformat{\\subsubsubsubsubsection}{\\normalfont\\normalsize\\bfseries}{\\thesubsubsubsubsubsection}{0.7em}{}\n\
     \\titlespacing*{\\subsubsubsubsubsection}{0pt}{2.5ex plus 1ex minus \
     .2ex}{1.1ex plus .2ex}\n\
     \\titleclass{\\subsubsubsubsubsubsection}{straight}[\\subsubsubsubsection]\n\
     \\newcounter{subsubsubsubsubsubsection}[subsubsubsubsubsection]\n\
     \\renewcommand\\thesubsubsubsubsubsubsection{\\thesubsubsubsubsubsection.\\arabic{subsubsubsubsubsubsection}}\n\
     \\titleformat{\\subsubsubsubsubsubsection}{\\normalfont\\normalsize\\bfseries}{\\thesubsubsubsubsubsubsection}{0.6em}{}\n\
     \\titlespacing*{\\subsubsubsubsubsubsection}{0pt}{2.25ex plus 1ex minus \
     .2ex}{1ex plus .2ex}\n\
     \\makeatletter\n\
     \\def\\toclevel@subsubsubsection{4}\n\
     \\def\\toclevel@subsubsubsubsection{5}\n\
     \\def\\toclevel@subsubsubsubsubsection{6}\n\
     \\def\\toclevel@subsubsubsubsubsubsection{7}\n\
     \\def\\toclevel@paragraph{8}\n\
     \\def\\toclevel@subparagraph{9}\n\
     \\def\\l@subsection{\\@dottedtocline{1}{1em}{0.5em}}\n\
     \\def\\l@subsubsection{\\@dottedtocline{2}{2em}{1em}}\n\
     \\def\\l@subsubsubsection{\\@dottedtocline{3}{3em}{1.5em}}\n\
     \\def\\l@subsubsubsubsection{\\@dottedtocline{5}{4em}{2em}}\n\
     \\def\\l@subsubsubsubsubsection{\\@dottedtocline{6}{5em}{2.5em}}\n\
     \\def\\l@subsubsubsubsubsubsection{\\@dottedtocline{7}{6em}{3em}}\n\
     \\def\\l@paragraph{\\@dottedtocline{8}{7em}{3.5em}}\n\
     \\def\\l@subparagraph{\\@dottedtocline{9}{8em}{4em}}\n\
     \\makeatother\n\
     \\setcounter{secnumdepth}{0}\n\
     \\setcounter{tocdepth}{9}\n\
     \\newunicodechar{÷}{$\\div$}\n\
     \\newunicodechar{×}{$\\times$}\n\
     \\newunicodechar{≤}{$\\leqslant$}\n\
     \\newunicodechar{≥}{$\\geqslant$}\n\
     \\newunicodechar{→}{$\\rightarrow$}\n\
     \\newunicodechar{≠}{$\\neq$}\n\n\
     \\newcommand*\\FancyVerbStartString{```catala}\n\
     \\newcommand*\\FancyVerbStopString{```}\n\n\
     \\fvset{\n\
     numbers=left,\n\
     frame=lines,\n\
     framesep=3mm,\n\
     rulecolor=\\color{gray!70},\n\
     firstnumber=last,\n\
     codes={\\catcode`\\$=3\\catcode`\\^=7}\n\
     }\n\n\
     \\title{\n\
     %s\n\
     }\n\
     \\author{\n\
     %s Catala version %s\n\
     }\n\
     \\begin{document}\n\
     \\maketitle\n\n\
     %s : \n\
     \\begin{itemize}%s\\end{itemize}\n\n\
     \\clearpage\n\
     \\tableofcontents\n\n\
     \\[\\star\\star\\star\\]\n\
     \\clearpage"
    (match language with Fr -> "french" | En -> "english" | Pl -> "polish")
    (literal_title language)
    (literal_generated_by language)
    Utils.Cli.version
    (literal_source_files language)
    (String.concat ","
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
            Printf.sprintf "\\item\\texttt{%s}, %s %s"
              (pre_latexify (Filename.basename filename))
              (literal_last_modification language)
              ftime)
          source_files));
  wrapped fmt;
  Format.fprintf fmt "\n\n\\end{document}"

(** {1 Weaving} *)

let rec law_structure_to_latex
    (language : C.backend_lang) (fmt : Format.formatter) (i : A.law_structure) :
    unit =
  match i with
  | A.LawHeading (heading, children) ->
      Format.fprintf fmt "\\%s{%s}\n\n"
        (match heading.law_heading_precedence with
        | 0 -> "section"
        | 1 -> "subsection"
        | 2 -> "subsubsection"
        | 3 -> "subsubsubsection"
        | 4 -> "subsubsubsubsection"
        | 5 -> "subsubsubsubsubsection"
        | 6 -> "subsubsubsubsubsubsection"
        | 7 -> "paragraph"
        | _ -> "subparagraph")
        (pre_latexify (Pos.unmark heading.law_heading_name));
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
        (law_structure_to_latex language)
        fmt children
  | A.LawInclude (A.PdfFile ((file, _), page)) ->
      let label =
        file
        ^ match page with None -> "" | Some p -> Format.sprintf "_page_%d," p
      in
      Format.fprintf fmt
        "\\begin{center}\\textit{Annexe incluse, retranscrite page \
         \\pageref{%s}}\\end{center} \
         \\begin{figure}[p]\\begin{center}\\includegraphics[%swidth=\\textwidth]{%s}\\label{%s}\\end{center}\\end{figure}"
        label
        (match page with None -> "" | Some p -> Format.sprintf "page=%d," p)
        file label
  | A.LawInclude (A.CatalaFile _ | A.LegislativeText _) -> ()
  | A.LawText t -> Format.fprintf fmt "%s" (pre_latexify t)
  | A.CodeBlock (_, c, false) ->
      Format.fprintf fmt
        "\\begin{minted}[label={\\hspace*{\\fill}\\texttt{%s}},firstnumber=%d]{%s}\n\
         ```catala\n\
         %s```\n\
         \\end{minted}"
        (pre_latexify (Filename.basename (Pos.get_file (Pos.get_position c))))
        (Pos.get_start_line (Pos.get_position c) - 1)
        (get_language_extension language)
        (Pos.unmark c)
  | A.CodeBlock (_, c, true) ->
      let metadata_title =
        match language with
        | Fr -> "Métadonnées"
        | En -> "Metadata"
        | Pl -> "Metadane"
      in
      Format.fprintf fmt
        "\\begin{tcolorbox}[colframe=OliveGreen, breakable, \
         title=\\textcolor{black}{\\texttt{%s}},title after \
         break=\\textcolor{black}{\\texttt{%s}},before skip=1em, after \
         skip=1em]\n\
         \\begin{minted}[numbersep=9mm, firstnumber=%d, \
         label={\\hspace*{\\fill}\\texttt{%s}}]{%s}\n\
         ```catala\n\
         %s```\n\
         \\end{minted}\n\
         \\end{tcolorbox}"
        metadata_title metadata_title
        (Pos.get_start_line (Pos.get_position c) - 1)
        (pre_latexify (Filename.basename (Pos.get_file (Pos.get_position c))))
        (get_language_extension language)
        (Pos.unmark c)

(** {1 API} *)

let ast_to_latex
    (language : C.backend_lang) (fmt : Format.formatter) (program : A.program) :
    unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
    (law_structure_to_latex language)
    fmt program.program_items
