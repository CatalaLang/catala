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

let driver (file : string) (debug : bool) (client_id : string) (client_secret : string) =
  if debug then Catala.Cli.debug_flag := true;
  let access_token = Api.get_token client_id client_secret in
  Catala.Cli.debug_print (Printf.sprintf "The LegiFrance API access token is %s" access_token);
  let article = Api.get_article_json access_token "LEGIARTI000038889038" in
  let article_text = Api.get_article_text article in
  let article_expiration_date = Api.get_article_expiration_date article in
  Catala.Cli.debug_print
    (Printf.sprintf "The content of the article (that expires on %02d/%02d/%d) is\n%s"
       article_expiration_date.Unix.tm_mday article_expiration_date.Unix.tm_mon
       (1900 + article_expiration_date.Unix.tm_year)
       article_text);
  (* LegiFrance is only supported for French texts *)
  let _program = Catala.Parser_driver.parse_source_files [ file ] Catala.Cli.Fr in
  (*TODO: introduce content id on Catala articles, and then retrive the text through the API *)
  exit 0

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (catala_legifrance_t driver, info)
