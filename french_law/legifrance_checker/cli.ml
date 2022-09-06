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

(** Command line arguments specification of [legifrance_catala] *)

open Cmdliner

let file =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"FILE"
        ~doc:
          "Name of the Catala master file you want to get LegiFrance \
           information on")

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

let debug =
  Arg.(value & flag & info ["d"; "debug"] ~doc:"Prints debug information")

(** Arguments : [file debug cliend_id client_secret] *)
let catala_legifrance_t f =
  Term.(const f $ file $ debug $ client_id $ client_secret)

let info =
  let doc = "LegiFrance interaction tool for Catala" in
  let man =
    [
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `S Manpage.s_bugs;
      `P
        "Please file bug reports at \
         https://gitlab.inria.fr/verifisc/catala/issues";
    ]
  in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error" 1] in
  Cmd.info "legifrance_catala" ~version:Utils.Cli.version ~doc ~exits ~man
