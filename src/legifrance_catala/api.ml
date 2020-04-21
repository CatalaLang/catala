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

open Lwt

let get_token_aux (client_id : string) (client_secret : string) : (string * string t) t =
  let site = "https://oauth.aife.economie.gouv.fr" in
  let token_url = "/api/oauth/token" in
  let uri = Uri.of_string (site ^ token_url) in
  let headers = Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded" in
  let body_string =
    [
      ("grant_type", "client_credentials");
      ("client_id", client_id);
      ("client_secret", client_secret);
      ("scope", "openid");
    ]
    |> List.map (fun (k, v) -> Printf.sprintf {|%s=%s|} k v)
    |> String.concat "&" |> Printf.sprintf "%s"
  in
  let body = body_string |> Cohttp_lwt.Body.of_string in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
  ( resp |> Cohttp_lwt.Response.status |> Cohttp.Code.string_of_status,
    body |> Cohttp_lwt.Body.to_string )
  |> return

let get_token (client_id : string) (client_secret : string) : string =
  let resp, body = Lwt_main.run (get_token_aux client_id client_secret) in
  let body = Lwt_main.run body in
  if resp = "200 OK" then
    let open Yojson.Basic.Util in
    body |> Yojson.Basic.from_string |> member "access_token" |> to_string
  else begin
    Catala.Cli.debug_print
      (Printf.sprintf "The API access token request went wrong ; status is %s and the body is\n%s"
         resp body);
    exit 1
  end

let get_article_aux (access_token : string) : (string * string t) t =
  let site = "https://api.aife.economie.gouv.fr" in
  let token_url = "/dila/legifrance-beta/lf-engine-app/consult/legiPart" in
  let uri = Uri.of_string (site ^ token_url) in
  let headers = Cohttp.Header.init_with "Authorization" (Printf.sprintf "Bearer %s" access_token) in
  let body_string =
    [ ("textId", "LEGITEXT000006073189"); ("date", "1587488994320") ]
    |> List.map (fun (k, v) -> Printf.sprintf {|"%s":"%s"|} k v)
    |> String.concat "," |> Printf.sprintf "{%s}"
  in
  let body = body_string |> Cohttp_lwt.Body.of_string in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
  ( resp |> Cohttp_lwt.Response.status |> Cohttp.Code.string_of_status,
    body |> Cohttp_lwt.Body.to_string )
  |> return

let get_article (access_token : string) : string =
  let resp, body = Lwt_main.run (get_article_aux access_token) in
  let body = Lwt_main.run body in
  if resp = "200 OK" then
    let open Yojson.Basic.Util in
    body |> Yojson.Basic.from_string |> member "access_token" |> to_string
  else begin
    Catala.Cli.debug_print
      (Printf.sprintf "The API access token request went wrong ; status is %s and the body is\n%s"
         resp body);
    exit 1
  end
