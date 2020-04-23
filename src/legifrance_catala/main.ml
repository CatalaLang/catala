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

open Cmdliner

let file =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"FILE"
        ~doc:"Name of the Catala master file you want to get LegiFrance information on")

let client_id =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"CLIENT_ID" ~doc:"LegiFrance Oauth cliend id")

let client_secret =
  Arg.(
    required
    & pos 2 (some string) None
    & info [] ~docv:"CLIENT_SECRET" ~doc:"LegiFrance Oauth cliend secret")

let debug = Arg.(value & flag & info [ "d"; "debug" ] ~doc:"Prints debug information")

let catala_legifrance_t f = Term.(const f $ file $ debug $ client_id $ client_secret)

let info =
  let doc = "LegiFrance interaction tool for Catala" in
  let man =
    [
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://gitlab.inria.fr/verifisc/catala/issues";
    ]
  in
  let exits = Term.default_exits @ [ Term.exit_info ~doc:"on error" 1 ] in
  Term.info "legifrance_catala"
    ~version:
      ( match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v )
    ~doc ~exits ~man

let parse_expiration_date (expiration_date : string) : Unix.tm =
  try
    let extract_article_title = Re.Pcre.regexp "([0-9]{2})\\/([0-9]{2})\\/([0-9]{4})" in
    let get_substring =
      Re.Pcre.get_substring (Re.Pcre.exec ~rex:extract_article_title expiration_date)
    in
    snd
      (Unix.mktime
         {
           Unix.tm_mday = int_of_string (get_substring 1);
           Unix.tm_mon = int_of_string (get_substring 2);
           Unix.tm_year = int_of_string (get_substring 3) - 1900;
           Unix.tm_sec = 0;
           Unix.tm_min = 0;
           Unix.tm_hour = 0;
           Unix.tm_wday = 0;
           Unix.tm_yday = 0;
           Unix.tm_isdst = false;
         })
  with _ ->
    Catala.Cli.error_print
      (Printf.sprintf "Error while parsing expiration date argument (%s)" expiration_date);
    exit 0

let print_tm (d : Unix.tm) : string =
  if d.Unix.tm_year + 1900 = 2999 then "undefined date"
  else Printf.sprintf "%02d/%02d/%d " d.Unix.tm_mday (1 + d.Unix.tm_mon) (1900 + d.Unix.tm_year)

let is_infinity (d : Unix.tm) : bool = d.Unix.tm_year + 1900 = 2999

type new_article_version = NotAvailable | Available of string

(* Returns the ID of the future version of the article if any *)
let check_article_expiration (article_catala : Catala.Ast.law_article) (access_token : string) :
    new_article_version option =
  match article_catala.Catala.Ast.law_article_id with
  | None -> None
  | Some article_id ->
      let article = Api.get_article_json access_token article_id in
      let api_article_expiration_date = Api.get_article_expiration_date article in
      let msg =
        Printf.sprintf "%s %s expires on %s according to LegiFrance%s"
          (Catala.Pos.unmark article_catala.Catala.Ast.law_article_name)
          (Catala.Pos.to_string
             (Catala.Pos.get_position article_catala.Catala.Ast.law_article_name))
          (print_tm api_article_expiration_date)
          ( match article_catala.Catala.Ast.law_article_expiration_date with
          | None -> ""
          | Some source_exp_date -> ", " ^ source_exp_date ^ " according to source code" )
      in
      let new_version_available = not (is_infinity api_article_expiration_date) in
      let source_code_expiration =
        match article_catala.Catala.Ast.law_article_expiration_date with
        | None -> false
        | Some source_exp_date ->
            let source_exp_date = parse_expiration_date source_exp_date in
            not (is_infinity source_exp_date)
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

module Diff = Simple_diff.Make (String)

let compare_article_to_version (access_token : string) (text : string) (version : string) :
    Diff.t option =
  let new_article = Api.get_article_json access_token version in
  let new_article_text = Api.get_article_text new_article in
  let text_to_list text = List.filter (fun word -> word <> "") (String.split_on_char ' ' text) in
  let old_list = text_to_list text in
  let new_list = text_to_list new_article_text in
  let diff = Diff.get_diff (Array.of_list old_list) (Array.of_list new_list) in
  let all_equal =
    List.for_all (fun chunk -> match chunk with Diff.Equal _ -> true | _ -> false) diff
  in
  if not all_equal then Some diff else None

let compare_to_versions (article_text_acc : article_text_acc) (access_token : string) : unit =
  let print_diff msg diff =
    Catala.Cli.warning_print
      (Printf.sprintf "%s\n%s" msg
         (String.concat "\n"
            (List.map
               (fun chunk ->
                 match chunk with
                 | Diff.Equal words ->
                     ANSITerminal.sprintf [] "= %s" (String.concat " " (Array.to_list words))
                 | Diff.Added words ->
                     ANSITerminal.sprintf [ ANSITerminal.green ] "+ %s"
                       (String.concat " " (Array.to_list words))
                 | Diff.Deleted words ->
                     ANSITerminal.sprintf [ ANSITerminal.red ] "- %s"
                       (String.concat " " (Array.to_list words)))
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

let driver (file : string) (debug : bool) (client_id : string) (client_secret : string) =
  if debug then Catala.Cli.debug_flag := true;
  let access_token = Api.get_token client_id client_secret in
  Catala.Cli.debug_print (Printf.sprintf "The LegiFrance API access token is %s" access_token);
  (* LegiFrance is only supported for French texts *)
  let program = Catala.Parser_driver.parse_source_files [ file ] Catala.Cli.Fr in
  let article_text_acc =
    List.fold_left
      (fun article_text_acc item ->
        match item with
        | Catala.Ast.LawArticle article_catala -> (
            compare_to_versions article_text_acc access_token;
            let new_version = check_article_expiration article_catala access_token in
            match new_version with
            | Some (Available version) ->
                {
                  article_title = article_catala.law_article_name;
                  text = "";
                  new_version = Some version;
                  current_version = article_catala.Catala.Ast.law_article_id;
                }
            | _ ->
                {
                  article_title = article_catala.law_article_name;
                  text = "";
                  new_version = None;
                  current_version = article_catala.Catala.Ast.law_article_id;
                } )
        | Catala.Ast.LawText art_text ->
            { article_text_acc with text = article_text_acc.text ^ " " ^ art_text }
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

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (catala_legifrance_t driver, info)
