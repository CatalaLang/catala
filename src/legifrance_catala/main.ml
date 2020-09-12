(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Main logic for interacting with LegiFrance when traversing Catala source files *)

type new_article_version = NotAvailable | Available of string

(** Returns the ID of the future version of the article if any *)
let check_article_expiration (article_catala : Catala.Catala_ast.law_article)
    (access_token : Api.access_token) : new_article_version option =
  match article_catala.Catala.Catala_ast.law_article_id with
  | None -> None
  | Some article_id ->
      let article = Api.retrieve_article access_token article_id in
      let api_article_expiration_date = Api.get_article_expiration_date article in
      let msg =
        Printf.sprintf "%s %s expires on %s according to LegiFrance%s"
          (Catala.Pos.unmark article_catala.Catala.Catala_ast.law_article_name)
          (Catala.Pos.to_string
             (Catala.Pos.get_position article_catala.Catala.Catala_ast.law_article_name))
          (Date.print_tm api_article_expiration_date)
          ( match article_catala.Catala.Catala_ast.law_article_expiration_date with
          | None -> ""
          | Some source_exp_date -> ", " ^ source_exp_date ^ " according to source code" )
      in
      let new_version_available = not (Date.is_infinity api_article_expiration_date) in
      let source_code_expiration =
        match article_catala.Catala.Catala_ast.law_article_expiration_date with
        | None -> false
        | Some source_exp_date ->
            let source_exp_date = Date.parse_expiration_date source_exp_date in
            not (Date.is_infinity source_exp_date)
      in
      if new_version_available || source_code_expiration then begin
        Catala.Cli.warning_print msg;
        if new_version_available then begin
          let new_version = Api.get_article_new_version article in
          Catala.Cli.debug_print (Printf.sprintf "New version of the article: %s" new_version);
          Some (Available new_version)
        end
        else Some NotAvailable
      end
      else begin
        Catala.Cli.debug_print msg;
        None
      end

type article_text_acc = {
  article_title : string Catala.Pos.marked;
  text : string;
  new_version : string option;
  current_version : string option;
}
(** Accumulator type when traversing the Catala source files *)

module Diff = Diff.Make (String)
(** Diff algorithm for a list of words *)

(** [compare_article_to_version token text version] retrieves the text of the article whose
    LegiFrance ID is [version] and produces a diff with the expected [text]*)
let compare_article_to_version (access_token : Api.access_token) (text : string) (version : string)
    : Diff.t option =
  let new_article = Api.retrieve_article access_token version in
  let new_article_text = Api.get_article_text new_article in
  let text_to_list text = List.filter (fun word -> word <> "") (String.split_on_char ' ' text) in
  let old_list = text_to_list text in
  let new_list = text_to_list new_article_text in
  let diff = Diff.get_diff (Array.of_list old_list) (Array.of_list new_list) in
  let all_equal =
    List.for_all (fun chunk -> match chunk with Diff.Equal _ -> true | _ -> false) diff
  in
  if not all_equal then Some diff else None

(** Compares [article_text_acc.current_version] and [article_text_acc.new_version] by accessing
    LegiFrance and display differences if any *)
let compare_to_versions (article_text_acc : article_text_acc) (access_token : Api.access_token) :
    unit =
  let print_diff msg diff =
    Catala.Cli.warning_print
      (Printf.sprintf "%s\n%s" msg
         (String.concat "\n"
            (List.map
               (fun chunk ->
                 match chunk with
                 | Diff.Equal words -> ANSITerminal.sprintf [] "%s" (String.concat " " words)
                 | Diff.Added words ->
                     ANSITerminal.sprintf [ ANSITerminal.green ] "(+) %s" (String.concat " " words)
                 | Diff.Deleted words ->
                     ANSITerminal.sprintf [ ANSITerminal.red ] "(-) %s" (String.concat " " words))
               diff)))
  in
  begin
    match article_text_acc.current_version with
    | Some version -> (
        match compare_article_to_version access_token article_text_acc.text version with
        | None -> ()
        | Some diff ->
            print_diff
              (Printf.sprintf
                 "There is a diff between the source code version of %s %s and the text stored on \
                  LegiFrance:"
                 (Catala.Pos.unmark article_text_acc.article_title)
                 (Catala.Pos.to_string (Catala.Pos.get_position article_text_acc.article_title)))
              diff )
    | None -> ()
  end;
  match article_text_acc.new_version with
  | Some version -> (
      match compare_article_to_version access_token article_text_acc.text version with
      | None -> ()
      | Some diff ->
          print_diff
            (Printf.sprintf
               "Here is the diff between the current version of %s %s and what it will become in \
                the future:"
               (Catala.Pos.unmark article_text_acc.article_title)
               (Catala.Pos.to_string (Catala.Pos.get_position article_text_acc.article_title)))
            diff )
  | None -> ()

(** Fill an [@@Include ...@@] tag inside the Catala source file with the legislative contents
    retrieved from LegiFrance *)
let include_legislative_text (id : string Catala.Pos.marked) (access_token : Api.access_token) :
    unit =
  let excerpt = Api.retrieve_law_excerpt access_token (Catala.Pos.unmark id) in
  let title = "@@" ^ Api.get_law_excerpt_title excerpt ^ "@@" in
  let articles =
    List.map
      (fun article ->
        Printf.sprintf "@Article %s|%s@\n%s" article.Api.num article.Api.id article.Api.content)
      (Api.get_law_excerpt_articles excerpt)
  in
  let to_insert = title ^ "\n\n" ^ String.concat "\n\n" articles in
  let pos = Catala.Pos.get_position id in
  Catala.Cli.debug_print (Printf.sprintf "Position: %s" (Catala.Pos.to_string pos));
  let file = Catala.Pos.get_file pos in
  let include_line = Catala.Pos.get_end_line pos in
  let ic = open_in file in
  let new_file = file ^ ".new" in
  Catala.Cli.warning_print
    (Printf.sprintf "LegiFrance inclusion detected, writing new contents to %s" new_file);
  let oc = open_out new_file in
  (* Pos.t lines start at 1 *)
  let counter = ref 1 in
  try
    while true do
      let line = input_line ic in
      if include_line = !counter then Printf.fprintf oc "%s\n" to_insert
      else Printf.fprintf oc "%s\n" line;
      counter := !counter + 1
    done
  with End_of_file ->
    close_in ic;
    close_out oc

(** Parses the Catala master source file and checks each article:

    - if the article has a LegiFrance ID, checks the text of the article in the source code vs the
      text from LegiFrance;
    - if the article has an expiration date, display the difference between the current version of
      the article and the next one on LegiFrance;
    - fill each [@@Include ...@@] tag with the contents retrieved from LegiFrance *)
let driver (file : string) (debug : bool) (client_id : string) (client_secret : string) =
  if debug then Catala.Cli.debug_flag := true;
  let access_token = Api.get_token client_id client_secret in
  (* LegiFrance is only supported for French texts *)
  let program = Catala.Parser_driver.parse_source_files [ file ] `Fr in
  let article_text_acc =
    List.fold_left
      (fun article_text_acc item ->
        match item with
        | Catala.Catala_ast.LawArticle article_catala -> (
            compare_to_versions article_text_acc access_token;
            let new_version = check_article_expiration article_catala access_token in
            match new_version with
            | Some (Available version) ->
                {
                  article_title = article_catala.law_article_name;
                  text = "";
                  new_version = Some version;
                  current_version = article_catala.Catala.Catala_ast.law_article_id;
                }
            | _ ->
                {
                  article_title = article_catala.law_article_name;
                  text = "";
                  new_version = None;
                  current_version = article_catala.Catala.Catala_ast.law_article_id;
                } )
        | Catala.Catala_ast.LawText art_text ->
            { article_text_acc with text = article_text_acc.text ^ " " ^ art_text }
        | Catala.Catala_ast.LawInclude (Catala.Catala_ast.LegislativeText id) ->
            include_legislative_text id access_token;
            article_text_acc
        | _ -> article_text_acc)
      {
        article_title = ("", Catala.Pos.no_pos);
        text = "";
        new_version = None;
        current_version = None;
      }
      program.program_items
  in
  compare_to_versions article_text_acc access_token;
  exit 0

(** Hook for the executable *)
let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.catala_legifrance_t driver, Cli.info)
