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

let expiration_date =
  Arg.(
    required
    & pos 3 (some string) None
    & info [] ~docv:"EXPIRATION_DATE"
        ~doc:"Articles that expire before this date will yield a warning (format DD/MM/YYYY)")

let debug = Arg.(value & flag & info [ "d"; "debug" ] ~doc:"Prints debug information")

let catala_legifrance_t f =
  Term.(const f $ file $ debug $ client_id $ client_secret $ expiration_date)

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

let date_before (d1 : Unix.tm) (d2 : Unix.tm) : bool = fst (Unix.mktime d1) <= fst (Unix.mktime d2)

let process_article (article_catala : Catala.Ast.law_article) (access_token : string)
    (expiration_date : Unix.tm) : unit =
  match article_catala.Catala.Ast.law_article_id with
  | None -> ()
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
      if
        date_before api_article_expiration_date expiration_date
        ||
        match article_catala.Catala.Ast.law_article_expiration_date with
        | None -> false
        | Some source_exp_date ->
            let source_exp_date = parse_expiration_date source_exp_date in
            date_before source_exp_date expiration_date
      then Catala.Cli.warning_print msg
      else Catala.Cli.debug_print msg

let driver (file : string) (debug : bool) (client_id : string) (client_secret : string)
    (expiration_date : string) =
  if debug then Catala.Cli.debug_flag := true;
  let expiration_date = parse_expiration_date expiration_date in
  let access_token = Api.get_token client_id client_secret in
  Catala.Cli.debug_print (Printf.sprintf "The LegiFrance API access token is %s" access_token);
  (* LegiFrance is only supported for French texts *)
  let program = Catala.Parser_driver.parse_source_files [ file ] Catala.Cli.Fr in
  List.iter
    (fun item ->
      match item with
      | Catala.Ast.LawArticle article_catala ->
          process_article article_catala access_token expiration_date
      | _ -> ())
    program.program_items;
  exit 0

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (catala_legifrance_t driver, info)
