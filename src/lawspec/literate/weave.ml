(* This file is part of the Lawspec compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** This modules weaves the source code and the legislative text together into a document that law
    professionals can understand. *)

module A = Ast
module P = Printf

let source_file_item_to_latex (i : A.source_file_item) : string =
  match i with
  | A.LawCode c -> P.sprintf "\\subsection{%s}" c
  | A.LawText t -> t
  | A.LawArticle a -> P.sprintf "\\paragraph{%s}" a
  | A.CodeBlock c -> P.sprintf "\\begin{lstlisting}%s\\end{lstlisting}" c

let ast_to_latex (file : A.source_file) : string =
  String.concat "\n\n" (List.map (fun i -> source_file_item_to_latex i) file)
