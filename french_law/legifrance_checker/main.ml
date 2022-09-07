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

(** Main logic for interacting with LégiFrance when traversing Catala source
    files *)

(** Returns the ID of the future version of the article if any *)
let check_article_expiration
    (current_date : Unix.tm)
    (law_heading : Surface.Ast.law_heading)
    (access_token : Api.access_token) : string option =
  match law_heading.Surface.Ast.law_heading_id with
  | None -> None
  | Some heading_id ->
    let article_id = Api.parse_id heading_id in
    let article = Api.retrieve_article access_token article_id in
    let legifrance_expiration_date = Api.get_article_expiration_date article in
    let source_expiration_date =
      Option.map Date.parse_expiration_date
        law_heading.Surface.Ast.law_heading_expiration_date
    in
    (* At this point we have three dates. [C] the current date, [L] the
       expiration date from LégiFrance, and [S] (optionnal) the expiration date
       according to the source code.

       First, if [S < L], we raise an error: the source code is wrong. Indeed
       the [S] expiration date is only meant as an override that extends
       LégiFrance expiration date, not shorten it.

       Now, we take [D = max(S,L)] and if [C > D] then we throw an error saying
       it is expired. *)
    (match source_expiration_date with
    | None -> ()
    | Some source_expiration_date ->
      if Date.date_compare source_expiration_date legifrance_expiration_date < 0
      then
        Utils.Cli.warning_print "%s %s expires on %s according to LégiFrance%s"
          (Utils.Marked.unmark law_heading.Surface.Ast.law_heading_name)
          (Utils.Pos.to_string
             (Utils.Marked.get_mark law_heading.Surface.Ast.law_heading_name))
          (Date.print_tm legifrance_expiration_date)
          (match law_heading.Surface.Ast.law_heading_expiration_date with
          | None -> assert false
          | Some source_exp_date ->
            ", but"
            ^ source_exp_date
            ^ " according to source code, which is more restrictive."));
    let max_date =
      match source_expiration_date with
      | None -> legifrance_expiration_date
      | Some source_expiration_date ->
        if
          Date.date_compare source_expiration_date legifrance_expiration_date
          < 0
        then legifrance_expiration_date
        else source_expiration_date
    in
    if Date.date_compare current_date max_date > 0 then (
      let new_version_available =
        not (Date.is_infinity legifrance_expiration_date)
      in
      let new_version =
        if new_version_available then
          let new_version = Api.get_article_new_version article in
          Some new_version
        else None
      in
      Utils.Cli.warning_print
        "%s %s has expired! Its expiration date is %s according to \
         LégiFrance%s.%s"
        (Utils.Marked.unmark law_heading.Surface.Ast.law_heading_name)
        (Utils.Pos.to_string
           (Utils.Marked.get_mark law_heading.Surface.Ast.law_heading_name))
        (Date.print_tm legifrance_expiration_date)
        (match law_heading.Surface.Ast.law_heading_expiration_date with
        | None -> ""
        | Some source_exp_date ->
          "and " ^ source_exp_date ^ " according to source code")
        (match new_version with
        | None -> ""
        | Some new_version ->
          Format.asprintf " New version of the article: %s." new_version);
      new_version)
    else None

type law_article_text = {
  article_title : string * Utils.Pos.t;
  text : string;
  new_version : Api.article_id option;
  current_version : Api.article_id option;
}
(** Representation of the text of an article of law *)

module Diff = Diff.Make (String)
(** Diff algorithm for a list of words *)

(** [compare_article_to_version token text version] retrieves the text of the
    article whose LégiFrance ID is [version] and produces a diff with the
    expected [text]*)
let compare_article_to_version
    (access_token : Api.access_token)
    (text : string)
    (version : Api.article_id) : Diff.t option =
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
    [article_text_acc.new_version] by accessing LégiFrance and display
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
              text stored on LégiFrance:"
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
    legislative contents retrieved from LégiFrance *)
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
    "LégiFrance inclusion detected, writing new contents to %s" new_file;
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
    ~(current_date : Unix.tm)
    ~(diff : bool)
    ~(expiration : bool)
    (access_token : Api.access_token)
    (item : Surface.Ast.law_structure) : string =
  match item with
  | Surface.Ast.LawHeading (law_heading, children) ->
    let children_text =
      List.fold_left
        (fun acc child ->
          acc
          ^ "\n\n"
          ^ traverse_source_code ~current_date ~diff ~expiration access_token
              child)
        "" children
    in
    let new_version =
      if expiration then
        check_article_expiration current_date law_heading access_token
      else None
    in
    let law_article_text =
      {
        article_title = law_heading.law_heading_name;
        text = children_text;
        new_version =
          (match new_version with
          | Some version -> Some (Api.parse_id version)
          | _ -> None);
        current_version = Option.map Api.parse_id law_heading.law_heading_id;
      }
    in
    if diff then compare_to_versions law_article_text access_token;
    children_text
  | Surface.Ast.LawText art_text -> art_text
  | Surface.Ast.LawInclude (Surface.Ast.LegislativeText id) ->
    include_legislative_text id access_token
  | _ -> ""

(** Parses the Catala master source file and checks each article:

    - if the article has a LégiFrance ID, checks the text of the article in the
      source code vs the text from LégiFrance;
    - if the article has an expiration date, display the difference between the
      current version of the article and the next one on LégiFrance;
    - fill each [@@Include ...@@] tag with the contents retrieved from
      LégiFrance *)
let driver
    (file : string)
    (debug : bool)
    (diff : bool)
    (expiration : bool)
    (custom_date : string option)
    (client_id : string)
    (client_secret : string) =
  try
    if debug then Utils.Cli.debug_flag := true;
    let access_token = Api.get_token client_id client_secret in
    (* LégiFrance is only supported for French texts *)
    let program =
      Surface.Parser_driver.parse_top_level_file (FileName file) Fr
    in
    let current_date =
      match custom_date with
      | Some custom_date -> Date.parse_expiration_date custom_date
      | None -> Unix.localtime (Unix.time ())
    in
    List.iter
      (fun item ->
        ignore
          (traverse_source_code ~current_date ~diff ~expiration access_token
             item))
      program.program_items;
    0
  with Utils.Errors.StructuredError (msg, pos) ->
    let bt = Printexc.get_raw_backtrace () in
    Utils.Cli.error_print "%s" (Utils.Errors.print_structured_error msg pos);
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    -1

(** Hook for the executable *)
let _ =
  Stdlib.exit
  @@ Cmdliner.Cmd.eval'
       (Cmdliner.Cmd.v Cli.info (Cli.catala_legifrance_t driver))
