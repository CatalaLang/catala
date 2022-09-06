(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Main logic for interacting with LegiFrance when traversing Catala source
    files *)

type new_article_version = NotAvailable | Available of string

(** Returns the ID of the future version of the article if any *)
let check_article_expiration
    (law_heading : Surface.Ast.law_heading)
    (access_token : Api.access_token) : new_article_version option =
  match law_heading.Surface.Ast.law_heading_id with
  | None -> None
  | Some article_id ->
    let article = Api.retrieve_article access_token article_id in
    let api_article_expiration_date = Api.get_article_expiration_date article in
    let msg =
      Printf.sprintf "%s %s expires on %s according to LegiFrance%s"
        (Utils.Marked.unmark law_heading.Surface.Ast.law_heading_name)
        (Utils.Pos.to_string
           (Utils.Marked.get_mark law_heading.Surface.Ast.law_heading_name))
        (Date.print_tm api_article_expiration_date)
        (match law_heading.Surface.Ast.law_heading_expiration_date with
        | None -> ""
        | Some source_exp_date ->
          ", " ^ source_exp_date ^ " according to source code")
    in
    let new_version_available =
      not (Date.is_infinity api_article_expiration_date)
    in
    let source_code_expiration =
      match law_heading.Surface.Ast.law_heading_expiration_date with
      | None -> false
      | Some source_exp_date ->
        let source_exp_date = Date.parse_expiration_date source_exp_date in
        not (Date.is_infinity source_exp_date)
    in
    if new_version_available || source_code_expiration then begin
      Utils.Cli.warning_print "%s" msg;
      if new_version_available then begin
        let new_version = Api.get_article_new_version article in
        Utils.Cli.debug_print "New version of the article: %s" new_version;
        Some (Available new_version)
      end
      else Some NotAvailable
    end
    else begin
      Utils.Cli.debug_print "%s" msg;
      None
    end

type law_article_text = {
  article_title : string * Utils.Pos.t;
  text : string;
  new_version : string option;
  current_version : string option;
}
(** Representation of the text of an article of law *)

module Diff = Diff.Make (String)
(** Diff algorithm for a list of words *)

(** [compare_article_to_version token text version] retrieves the text of the
    article whose LegiFrance ID is [version] and produces a diff with the
    expected [text]*)
let compare_article_to_version
    (access_token : Api.access_token)
    (text : string)
    (version : string) : Diff.t option =
  let new_article = Api.retrieve_article access_token version in
  let new_article_text = Api.get_article_text new_article in
  let text_to_list text =
    List.filter (fun word -> word <> "") (String.split_on_char ' ' text)
  in
  let old_list = text_to_list text in
  let new_list = text_to_list new_article_text in
  let diff = Diff.get_diff (Array.of_list old_list) (Array.of_list new_list) in
  let all_equal =
    List.for_all
      (fun chunk -> match chunk with Diff.Equal _ -> true | _ -> false)
      diff
  in
  if not all_equal then Some diff else None

(** Compares [article_text_acc.current_version] and
    [article_text_acc.new_version] by accessing LegiFrance and display
    differences if any *)
let compare_to_versions
    (law_article_text : law_article_text)
    (access_token : Api.access_token) : unit =
  let print_diff msg diff =
    Utils.Cli.warning_print "%s\n%s" msg
      (String.concat "\n"
         (List.map
            (fun chunk ->
              match chunk with
              | Diff.Equal words ->
                ANSITerminal.sprintf [] "%s" (String.concat " " words)
              | Diff.Added words ->
                ANSITerminal.sprintf [ANSITerminal.green] "(+) %s"
                  (String.concat " " words)
              | Diff.Deleted words ->
                ANSITerminal.sprintf [ANSITerminal.red] "(-) %s"
                  (String.concat " " words))
            diff))
  in
  begin
    match law_article_text.current_version with
    | Some version -> (
      match
        compare_article_to_version access_token law_article_text.text version
      with
      | None -> ()
      | Some diff ->
        print_diff
          (Printf.sprintf
             "There is a diff between the source code version of %s %s and the \
              text stored on LegiFrance:"
             (fst law_article_text.article_title)
             (Utils.Pos.to_string (snd law_article_text.article_title)))
          diff)
    | None -> ()
  end;
  match law_article_text.new_version with
  | Some version -> (
    match
      compare_article_to_version access_token law_article_text.text version
    with
    | None -> ()
    | Some diff ->
      print_diff
        (Printf.sprintf
           "Here is the diff between the current version of %s %s and what it \
            will become in the future:"
           (fst law_article_text.article_title)
           (Utils.Pos.to_string (snd law_article_text.article_title)))
        diff)
  | None -> ()

(** Fill an [@@Include ...@@] tag inside the Catala source file with the
    legislative contents retrieved from LegiFrance *)
let include_legislative_text
    (id : string * Utils.Pos.t)
    (access_token : Api.access_token) : string =
  let excerpt = Api.retrieve_law_excerpt access_token (fst id) in
  let title = "#" ^ Api.get_law_excerpt_title excerpt in
  let excerpts = Api.get_law_excerpt_articles excerpt in
  let text_to_return =
    String.concat "\n\n"
      (List.map (fun article -> article.Api.content) excerpts)
  in
  let articles =
    List.map
      (fun article ->
        Printf.sprintf "## Article %s|%s@\n%s" article.Api.num article.Api.id
          article.Api.content)
      excerpts
  in
  let to_insert = title ^ "\n\n" ^ String.concat "\n\n" articles in
  let pos = snd id in
  Utils.Cli.debug_format "Position: %s" (Utils.Pos.to_string_short pos);
  let file = Utils.Pos.get_file pos in
  let include_line = Utils.Pos.get_end_line pos in
  let ic = open_in file in
  let new_file = file ^ ".new" in
  Utils.Cli.warning_print
    "LegiFrance inclusion detected, writing new contents to %s" new_file;
  let oc = open_out new_file in
  (* Pos.t lines start at 1 *)
  let counter = ref 1 in
  (try
     while true do
       let line = input_line ic in
       if include_line = !counter then Printf.fprintf oc "%s\n" to_insert
       else Printf.fprintf oc "%s\n" line;
       counter := !counter + 1
     done
   with End_of_file ->
     close_in ic;
     close_out oc);
  text_to_return

let rec traverse_source_code
    (access_token : Api.access_token)
    (item : Surface.Ast.law_structure) : string =
  match item with
  | Surface.Ast.LawHeading (law_heading, children) ->
    let children_text =
      List.fold_left
        (fun acc child ->
          acc ^ "\n\n" ^ traverse_source_code access_token child)
        "" children
    in
    let new_version = check_article_expiration law_heading access_token in
    let law_article_text =
      {
        article_title = law_heading.law_heading_name;
        text = children_text;
        new_version =
          (match new_version with
          | Some (Available version) -> Some version
          | _ -> None);
        current_version = law_heading.law_heading_id;
      }
    in
    compare_to_versions law_article_text access_token;
    children_text
  | Surface.Ast.LawText art_text -> art_text
  | Surface.Ast.LawInclude (Surface.Ast.LegislativeText id) ->
    include_legislative_text id access_token
  | _ -> ""

(** Parses the Catala master source file and checks each article:

    - if the article has a LegiFrance ID, checks the text of the article in the
      source code vs the text from LegiFrance;
    - if the article has an expiration date, display the difference between the
      current version of the article and the next one on LegiFrance;
    - fill each [@@Include ...@@] tag with the contents retrieved from
      LegiFrance *)
let driver
    (file : string)
    (debug : bool)
    (client_id : string)
    (client_secret : string) =
  if debug then Utils.Cli.debug_flag := true;
  let access_token = Api.get_token client_id client_secret in
  (* LegiFrance is only supported for French texts *)
  let program = Surface.Parser_driver.parse_top_level_file (FileName file) Fr in
  List.iter
    (fun item -> ignore (traverse_source_code access_token item))
    program.program_items;
  exit 0

(** Hook for the executable *)
let _ =
  Stdlib.exit
  @@ Cmdliner.Cmd.eval'
       (Cmdliner.Cmd.v Cli.info (Cli.catala_legifrance_t driver))
