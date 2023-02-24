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
module R = Re.Pcre
module C = Cli

(** {1 Helpers} *)

let lines_of_code = ref 0

let update_lines_of_code c =
  lines_of_code :=
    !lines_of_code
    + Pos.get_end_line (Marked.get_mark c)
    - Pos.get_start_line (Marked.get_mark c)
    - 1

(** Espaces various LaTeX-sensitive characters *)
let pre_latexify (s : string) : string =
  (* Then we send to pandoc, to ensure the markdown features used in the
     original document are correctly printed! *)
  String.trim (run_pandoc s `Latex)

(** Usage: [wrap_latex source_files custom_pygments language fmt wrapped]

    Prints an LaTeX complete documùent structure around the [wrapped] content. *)
let wrap_latex
    (source_files : string list)
    (language : C.backend_lang)
    (fmt : Format.formatter)
    (wrapped : Format.formatter -> unit) =
  Format.fprintf fmt
    {latex|\documentclass[%s, 11pt, a4paper]{article}

\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage{amssymb}
\usepackage{babel}
\usepackage{fontspec}
\usepackage[hidelinks]{hyperref}
%s
\usepackage{minted}
\usepackage{longtable}
\usepackage{booktabs,tabularx}
\usepackage{newunicodechar}
\usepackage{textcomp}
\usepackage[hidelinks]{hyperref}
\usepackage[dvipsnames]{xcolor}
\usepackage[left=2cm,right=2cm,top=3cm,bottom=3cm,headheight=2cm]{geometry}
\usepackage[many]{tcolorbox}

\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[C]{\leftmark}
\fancyfoot[C]{\thepage}
\renewcommand{\headrulewidth}{0.5pt}
\renewcommand{\footrulewidth}{0.5pt}
\usepackage{titlesec}
\titleclass{\subsubsubsection}{straight}[\subsection]
\newcounter{subsubsubsection}[subsubsection]
\renewcommand\thesubsubsubsection{\thesubsubsection.\arabic{subsubsubsection}}
\titleformat{\subsubsubsection}{\normalfont\normalsize\bfseries}{\thesubsubsubsection}{1em}{}
\titlespacing*{\subsubsubsection}{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}
\titleclass{\subsubsubsubsection}{straight}[\subsubsection]
\newcounter{subsubsubsubsection}[subsubsubsection]
\renewcommand\thesubsubsubsubsection{\thesubsubsubsection.\arabic{subsubsubsubsection}}
\titleformat{\subsubsubsubsection}{\normalfont\normalsize\bfseries}{\thesubsubsubsubsection}{0.75em}{}
\titlespacing*{\subsubsubsubsection}{0pt}{2.75ex plus 1ex minus .2ex}{1.25ex plus .2ex}
\titleclass{\subsubsubsubsubsection}{straight}[\subsubsubsection]
\newcounter{subsubsubsubsubsection}[subsubsubsubsection]
\renewcommand\thesubsubsubsubsubsection{\thesubsubsubsubsection.\arabic{subsubsubsubsubsection}}
\titleformat{\subsubsubsubsubsection}{\normalfont\normalsize\bfseries}{\thesubsubsubsubsubsection}{0.7em}{}
\titlespacing*{\subsubsubsubsubsection}{0pt}{2.5ex plus 1ex minus .2ex}{1.1ex plus .2ex}
\titleclass{\subsubsubsubsubsubsection}{straight}[\subsubsubsubsection]
\newcounter{subsubsubsubsubsubsection}[subsubsubsubsubsection]
\renewcommand\thesubsubsubsubsubsubsection{\thesubsubsubsubsubsection.\arabic{subsubsubsubsubsubsection}}
\titleformat{\subsubsubsubsubsubsection}{\normalfont\normalsize\bfseries}{\thesubsubsubsubsubsubsection}{0.6em}{}
\titlespacing*{\subsubsubsubsubsubsection}{0pt}{2.25ex plus 1ex minus .2ex}{1ex plus .2ex}
\makeatletter
\def\toclevel@subsubsubsection{4}
\def\toclevel@subsubsubsubsection{5}
\def\toclevel@subsubsubsubsubsection{6}
\def\toclevel@subsubsubsubsubsubsection{7}
\def\toclevel@paragraph{8}
\def\toclevel@subparagraph{9}
\def\l@subsection{\@dottedtocline{1}{1em}{0.5em}}
\def\l@subsubsection{\@dottedtocline{2}{2em}{1em}}
\def\l@subsubsubsection{\@dottedtocline{3}{3em}{1.5em}}
\def\l@subsubsubsubsection{\@dottedtocline{5}{4em}{2em}}
\def\l@subsubsubsubsubsection{\@dottedtocline{6}{5em}{2.5em}}
\def\l@subsubsubsubsubsubsection{\@dottedtocline{7}{6em}{3em}}
\def\l@paragraph{\@dottedtocline{8}{7em}{3.5em}}
\def\l@subparagraph{\@dottedtocline{9}{8em}{4em}}
\makeatother
\setcounter{secnumdepth}{0}
\setcounter{tocdepth}{9}
\newunicodechar{÷}{$\div$}
\newunicodechar{×}{$\times$}
\newunicodechar{≤}{$\leqslant$}
\newunicodechar{≥}{$\geqslant$}
\newunicodechar{→}{$\rightarrow$}
\newunicodechar{≠}{$\neq$}

\newcommand*\FancyVerbStartString{\PYG{l+s}{```catala}}
\newcommand*\FancyVerbStopString{\PYG{l+s}{```}}

\fvset{
numbers=left,
frame=lines,
framesep=3mm,
rulecolor=\color{gray!70},
firstnumber=last,
codes={\catcode`\$=3\catcode`\^=7}
}
\newcommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}

\title{
%s\\
%s Catala version %s
}
\begin{document}
\maketitle

%s

%s :
\begin{itemize}%s\end{itemize}

\clearpage
\tableofcontents

\[\star\star\star\]
\clearpage
|latex}
    (match language with Fr -> "french" | En -> "english" | Pl -> "polish")
    (match language with Fr -> "\\setmainfont{Marianne}" | _ -> "")
    (* for France, we use the official font of the French state design system
       https://gouvfr.atlassian.net/wiki/spaces/DB/pages/223019527/Typographie+-+Typography *)
    (literal_title language)
    (literal_generated_by language)
    Cli.version
    (pre_latexify (literal_disclaimer_and_link language))
    (literal_source_files language)
    (String.concat
       ((match language with Fr -> " ;" | En -> ";" | Pl -> ";") ^ "\n")
       (List.map
          (fun filename ->
            let mtime = (Unix.stat filename).Unix.st_mtime in
            let ltime = Unix.localtime mtime in
            let ftime =
              Printf.sprintf "%d-%02d-%02d %d:%02d"
                (1900 + ltime.Unix.tm_year)
                (ltime.Unix.tm_mon + 1) ltime.Unix.tm_mday ltime.Unix.tm_hour
                ltime.Unix.tm_min
            in
            Printf.sprintf "\\item\\texttt{%s}, %s %s"
              (pre_latexify (Filename.basename filename))
              (literal_last_modification language)
              ftime)
          source_files)
    ^ ".");
  wrapped fmt;
  Format.fprintf fmt "\n\n\\end{document}"

(** {1 Weaving} *)

let rec law_structure_to_latex
    (language : C.backend_lang)
    (print_only_law : bool)
    (fmt : Format.formatter)
    (i : A.law_structure) : unit =
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
      (pre_latexify (Marked.unmark heading.law_heading_name));
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
      (law_structure_to_latex language print_only_law)
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
  | A.CodeBlock (_, c, false) when not print_only_law ->
    let start_line = Pos.get_start_line (Marked.get_mark c) - 1 in
    let filename = Filename.basename (Pos.get_file (Marked.get_mark c)) in
    let block_content = Marked.unmark c in
    check_exceeding_lines start_line filename block_content;
    update_lines_of_code c;
    Format.fprintf fmt
      "\\begin{minted}[label={\\hspace*{\\fill}\\texttt{%s}},firstnumber=%d]{%s}\n\
       ```catala\n\
       %s```\n\
       \\end{minted}"
      (pre_latexify (Filename.basename (Pos.get_file (Marked.get_mark c))))
      (Pos.get_start_line (Marked.get_mark c) + 1)
      (get_language_extension language)
      (Marked.unmark c)
  | A.CodeBlock (_, c, true) when not print_only_law ->
    let metadata_title =
      match language with
      | Fr -> "Métadonnées"
      | En -> "Metadata"
      | Pl -> "Metadane"
    in
    let start_line = Pos.get_start_line (Marked.get_mark c) + 1 in
    let filename = Filename.basename (Pos.get_file (Marked.get_mark c)) in
    let block_content = Marked.unmark c in
    check_exceeding_lines start_line filename block_content;
    update_lines_of_code c;
    Format.fprintf fmt
      "\\begin{tcolorbox}[colframe=OliveGreen, breakable, \
       title=\\textcolor{black}{\\texttt{%s}},title after \
       break=\\textcolor{black}{\\texttt{%s}},before skip=1em, after skip=1em]\n\
       \\begin{minted}[numbersep=9mm, firstnumber=%d, \
       label={\\hspace*{\\fill}\\texttt{%s}}]{%s}\n\
       ```catala\n\
       %s```\n\
       \\end{minted}\n\
       \\end{tcolorbox}"
      metadata_title metadata_title start_line (pre_latexify filename)
      (get_language_extension language)
      block_content
  | A.CodeBlock _ -> ()

(** {1 API} *)

let ast_to_latex
    (language : C.backend_lang)
    ~(print_only_law : bool)
    (fmt : Format.formatter)
    (program : A.program) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
    (law_structure_to_latex language print_only_law)
    fmt program.program_items;
  Cli.debug_print "Lines of Catala inside literate source code: %d"
    !lines_of_code
