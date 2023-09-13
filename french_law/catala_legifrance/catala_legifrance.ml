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

open Catala_utils

let ( let* ) = Lwt.bind

(** Main logic for interacting with LégiFrance when traversing Catala source
    files *)

(** Returns the ID of the future version of the article if any *)
let check_article_expiration
    (current_date : Unix.tm)
    (law_heading : Surface.Ast.law_heading)
    (access_token : Api.access_token) : string option Lwt.t =
  match law_heading.Surface.Ast.law_heading_id with
  | None -> Lwt.return None
  | Some heading_id ->
    let article_id = Api.parse_id heading_id in
    let* article = Api.retrieve_article access_token article_id in
    let legifrance_expiration_date = Api.get_article_expiration_date article in
    let is_archive = law_heading.Surface.Ast.law_heading_is_archive in
    (* At this point we have two dates. [C] the current date, [L] the expiration
       date from LégiFrance. Plus we have flag [A] that tells us if [A] is an
       archive, which should not be checked for expiration. Now, if [C > L] then
       we throw an error saying it is expired, except if [A] is true *)
    if
      (not is_archive)
      && Date.date_compare current_date legifrance_expiration_date > 0
    then (
      let new_version_available =
        not (Date.is_infinity legifrance_expiration_date)
      in
      let new_version =
        if new_version_available then
          let new_version = Api.get_article_new_version article in
          Some new_version
        else None
      in
      Message.emit_warning
        "%s %s has expired! Its expiration date is %s according to \
         LégiFrance.%s"
        (Mark.remove law_heading.Surface.Ast.law_heading_name)
        (Pos.to_string (Mark.get law_heading.Surface.Ast.law_heading_name))
        (Date.print_tm legifrance_expiration_date)
        (match new_version with
        | None -> ""
        | Some new_version ->
          Format.asprintf " New version of the article: \"%s\"." new_version);
      Lwt.return new_version)
    else Lwt.return None

type law_article_text = {
  article_title : string * Pos.t;
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
    (version : Api.article_id) : Diff.t option Lwt.t =
  let* new_article = Api.retrieve_article access_token version in
  let new_article_text = Api.get_article_text new_article in
  let text_to_list text =
    List.map String.trim
      (List.flatten
         (List.map
            (fun line ->
              List.filter
                (fun word -> word <> "")
                (String.split_on_char ' ' line))
            (List.filter
               (fun word -> word <> "")
               (String.split_on_char '\n'
                  (Re.replace_string (Re.compile (Re.char '\t')) ~by:" " text)))))
  in
  let old_list = text_to_list text in
  let new_list = text_to_list new_article_text in
  let diff = Diff.get_diff (Array.of_list old_list) (Array.of_list new_list) in
  let all_equal =
    List.for_all
      (fun chunk -> match chunk with Diff.Equal _ -> true | _ -> false)
      diff
  in
  Lwt.return (if not all_equal then Some diff else None)

(** Compares [article_text_acc.current_version] and
    [article_text_acc.new_version] by accessing LégiFrance and display
    differences if any *)
let compare_to_versions
    (law_article_text : law_article_text)
    (access_token : Api.access_token) : unit Lwt.t =
  let print_diff msg diff =
    Message.emit_warning "@[<v>%s@,%a@]" msg
      (Format.pp_print_list (fun ppf chunk ->
           match chunk with
           | Diff.Equal words ->
             Format.fprintf ppf "    %s" (String.concat " " words)
           | Diff.Added words ->
             Format.fprintf ppf "@{<green>(+) %s@}" (String.concat " " words)
           | Diff.Deleted words ->
             Format.fprintf ppf "@{<red>(-) %s@}" (String.concat " " words)))
      diff
  in
  let* _checl =
    match law_article_text.current_version with
    | Some version -> (
      let* comparison =
        compare_article_to_version access_token law_article_text.text version
      in
      match comparison with
      | None -> Lwt.return_unit
      | Some diff ->
        print_diff
          (Printf.sprintf
             "There is a diff between the source code version of %s %s and the \
              text stored on LégiFrance:\n"
             (fst law_article_text.article_title)
             (Pos.to_string (snd law_article_text.article_title)))
          diff;
        Lwt.return_unit)
    | None -> Lwt.return_unit
  in
  match law_article_text.new_version with
  | Some version -> (
    let* comparison =
      compare_article_to_version access_token law_article_text.text version
    in
    match comparison with
    | None -> Lwt.return_unit
    | Some diff ->
      print_diff
        (Printf.sprintf
           "Here is the diff between the current version of %s %s and what it \
            will become in the future:\n"
           (fst law_article_text.article_title)
           (Pos.to_string (snd law_article_text.article_title)))
        diff;
      Lwt.return_unit)
  | None -> Lwt.return_unit

(** Fill an [@@Include ...@@] tag inside the Catala source file with the
    legislative contents retrieved from LégiFrance *)
let include_legislative_text
    (id : string * Pos.t)
    (access_token : Api.access_token) : string Lwt.t =
  let pos = snd id in
  let id = Api.parse_id (fst id) in
  let* article = Api.retrieve_article access_token id in
  let text_to_return = Api.get_article_text article in
  let to_insert = text_to_return in
  Message.emit_debug "Position: %s" (Pos.to_string_short pos);
  let file = Pos.get_file pos in
  let include_line = Pos.get_start_line pos in
  let ic = open_in file in
  let new_file = file ^ ".new" in
  Message.emit_warning
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
  Lwt.return text_to_return

let rec traverse_source_code
    ~(current_date : Unix.tm)
    ~(diff : bool)
    ~(expiration : bool)
    (access_token : Api.access_token)
    (item : Surface.Ast.law_structure) : string Lwt.t =
  match item with
  | Surface.Ast.LawHeading (law_heading, children) ->
    let* children_text =
      Lwt_list.fold_left_s
        (fun acc child ->
          let* traversal =
            traverse_source_code ~current_date ~diff ~expiration access_token
              child
          in
          Lwt.return (acc ^ "\n\n" ^ traversal))
        "" children
    in
    let* new_version =
      if expiration then
        check_article_expiration current_date law_heading access_token
      else Lwt.return None
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
    let* _cmp =
      if diff then compare_to_versions law_article_text access_token
      else Lwt.return_unit
    in
    Lwt.return children_text
  | Surface.Ast.LawText art_text -> Lwt.return art_text
  | Surface.Ast.LawInclude (Surface.Ast.LegislativeText id) ->
    include_legislative_text id access_token
  | _ -> Lwt.return ""

(** Parses the Catala master source file and checks each article:

    - if the article has a LégiFrance ID, checks the text of the article in the
      source code vs the text from LégiFrance;
    - if the article has an expiration date, display the difference between the
      current version of the article and the next one on LégiFrance;
    - fill each [@@Include ...@@] tag with the contents retrieved from
      LégiFrance *)
let driver_lwt
    (file : string)
    (debug : bool)
    (diff : bool)
    (expiration : bool)
    (custom_date : string option)
    (client_id : string)
    (client_secret : string) =
  try
    let _options = Cli.enforce_globals ~debug () in
    if not (expiration || diff) then
      Message.raise_error
        "You have to check at least something, see the list of options with \
         --help";
    let* access_token = Api.get_token client_id client_secret in
    (* LégiFrance is only supported for French texts *)
    let program =
      Surface.Parser_driver.parse_top_level_file (FileName file) Fr
    in
    let current_date =
      match custom_date with
      | Some custom_date -> Date.parse_expiration_date ISO custom_date
      | None -> Unix.localtime (Unix.time ())
    in
    let* () =
      Lwt_list.iter_s
        (fun item ->
          let* _r =
            traverse_source_code ~current_date ~diff ~expiration access_token
              item
          in
          Lwt.return_unit)
        program.program_items
    in
    prerr_endline "0";
    Lwt.return 0
  with Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    Lwt.return (-1)

let driver file debug diff expiration custom_date client_id client_secret =
  try
    Lwt_main.run
      (driver_lwt file debug diff expiration custom_date client_id client_secret)
  with Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    -1

(** Hook for the executable *)
let () =
  Stdlib.exit
  @@ Cmdliner.Cmd.eval' ~catch:false
       (Cmdliner.Cmd.v Legifrance_cli.info
          (Legifrance_cli.catala_legifrance_t driver))
