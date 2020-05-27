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

type access_token = string

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
  if resp = "200 OK" then begin
    let token =
      body |> Yojson.Basic.from_string
      |> Yojson.Basic.Util.member "access_token"
      |> Yojson.Basic.Util.to_string
    in
    Catala.Cli.debug_print (Printf.sprintf "The LegiFrance API access token is %s" token);
    token
  end
  else begin
    Catala.Cli.debug_print
      (Printf.sprintf "The API access token request went wrong ; status is %s and the body is\n%s"
         resp body);
    exit 1
  end

let site = "https://api.aife.economie.gouv.fr"

let base_token_url = "/dila/legifrance-beta/lf-engine-app/"

let api_timestamp_to_localtime (timestamp : int) : Unix.tm =
  Unix.localtime (float_of_int (timestamp / 1000))

let make_request (access_token : string) (token_url : string) (body_json : (string * string) list) :
    (string * string t) t =
  let uri = Uri.of_string (site ^ base_token_url ^ token_url) in
  let headers = Cohttp.Header.init_with "Authorization" (Printf.sprintf "Bearer %s" access_token) in
  let headers = Cohttp.Header.add headers "Content-Type" "application/json" in
  let headers = Cohttp.Header.add headers "Accept" "application/json" in
  let body_string =
    body_json
    |> List.map (fun (k, v) -> Printf.sprintf {|"%s":"%s"|} k v)
    |> String.concat "," |> Printf.sprintf "{%s}"
  in
  let body = body_string |> Cohttp_lwt.Body.of_string in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
  ( resp |> Cohttp_lwt.Response.status |> Cohttp.Code.string_of_status,
    body |> Cohttp_lwt.Body.to_string )
  |> return

type article = Yojson.Basic.t

let run_request (request : (string * string t) t) : Yojson.Basic.t =
  let resp, body = Lwt_main.run request in
  let body = Lwt_main.run body in
  if resp = "200 OK" then (
    try body |> Yojson.Basic.from_string
    with Yojson.Basic.Util.Type_error (msg, obj) ->
      Catala.Cli.error_print
        (Printf.sprintf
           "Error while parsing JSON answer from API: %s\nSpecific JSON:\n%s\nFull answer:\n%s" msg
           (Yojson.Basic.to_string obj) body);
      exit (-1) )
  else begin
    Catala.Cli.error_print
      (Printf.sprintf "The API request went wrong ; status is %s and the body is\n%s" resp body);
    exit (-1)
  end

let retrieve_article (access_token : string) (article_id : string) : Yojson.Basic.t =
  run_request (make_request access_token "consult/getArticle" [ ("id", article_id) ])

let raise_article_parsing_error (json : Yojson.Basic.t) (msg : string) (obj : Yojson.Basic.t) =
  Catala.Cli.error_print
    (Printf.sprintf
       "Error while manipulating JSON answer from API: %s\nSpecific JSON:\n%s\nFull answer:\n%s" msg
       (Yojson.Basic.to_string obj) (Yojson.Basic.to_string json));
  exit 1

type law_excerpt = Yojson.Basic.t

let retrieve_law_excerpt (access_token : string) (text_id : string) : law_excerpt =
  run_request (make_request access_token "consult/jorfPart" [ ("textCid", text_id) ])

let get_article_id (json : article) : string =
  try
    json
    |> Yojson.Basic.Util.member "article"
    |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
  with Yojson.Basic.Util.Type_error (msg, obj) -> raise_article_parsing_error json msg obj

let get_article_text (json : article) : string =
  try
    let text =
      json
      |> Yojson.Basic.Util.member "article"
      |> Yojson.Basic.Util.member "texte" |> Yojson.Basic.Util.to_string
    in
    (* there might be a nota *)
    let nota =
      try
        json
        |> Yojson.Basic.Util.member "article"
        |> Yojson.Basic.Util.member "nota" |> Yojson.Basic.Util.to_string
      with Yojson.Basic.Util.Type_error _ -> ""
    in
    text ^ " " ^ if nota <> "" then "NOTA : " ^ nota else ""
  with Yojson.Basic.Util.Type_error (msg, obj) -> raise_article_parsing_error json msg obj

let get_article_expiration_date (json : article) : Unix.tm =
  try
    let article_id = get_article_id json in
    json
    |> Yojson.Basic.Util.member "article"
    |> Yojson.Basic.Util.member "articleVersions"
    |> Yojson.Basic.Util.to_list
    |> List.find (fun version ->
           Yojson.Basic.to_string (Yojson.Basic.Util.member "id" version) = "\"" ^ article_id ^ "\"")
    |> Yojson.Basic.Util.member "dateFin"
    |> Yojson.Basic.Util.to_int |> api_timestamp_to_localtime
  with Yojson.Basic.Util.Type_error (msg, obj) -> raise_article_parsing_error json msg obj

let get_article_new_version (json : article) : string =
  let expiration_date = get_article_expiration_date json in
  let get_version_date_debut (version : Yojson.Basic.t) : Unix.tm =
    version
    |> Yojson.Basic.Util.member "dateDebut"
    |> Yojson.Basic.Util.to_int |> api_timestamp_to_localtime
  in
  try
    json
    |> Yojson.Basic.Util.member "article"
    |> Yojson.Basic.Util.member "articleVersions"
    |> Yojson.Basic.Util.to_list
    |> List.filter (fun version ->
           Date.date_compare expiration_date (get_version_date_debut version) <= 0)
    |> List.sort (fun version1 version2 ->
           Date.date_compare (get_version_date_debut version1) (get_version_date_debut version2))
    |> List.hd |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
  with Yojson.Basic.Util.Type_error (msg, obj) -> raise_article_parsing_error json msg obj

let get_law_excerpt_title (json : law_excerpt) : string =
  json |> Yojson.Basic.Util.member "title" |> Yojson.Basic.Util.to_string

type law_excerpt_article = { id : string; num : string; content : string }

let clean_html (s : string) : string =
  let new_line = Re.Pcre.regexp "\\s*\\<br\\s*\\/\\>\\s*" in
  let s = Re.Pcre.substitute ~rex:new_line ~subst:(fun _ -> "\n") s in
  let tag = Re.Pcre.regexp "\\<[^\\>]+\\>" in
  let s = Re.Pcre.substitute ~rex:tag ~subst:(fun _ -> "") s in
  String.trim s

let get_law_excerpt_articles (json : law_excerpt) : law_excerpt_article list =
  let articles = json |> Yojson.Basic.Util.member "articles" |> Yojson.Basic.Util.to_list in
  let articles =
    List.sort
      (fun a1 a2 ->
        let a1_num =
          int_of_string (a1 |> Yojson.Basic.Util.member "num" |> Yojson.Basic.Util.to_string)
        in
        let a2_num =
          int_of_string (a2 |> Yojson.Basic.Util.member "num" |> Yojson.Basic.Util.to_string)
        in
        compare a1_num a2_num)
      articles
  in
  List.map
    (fun article ->
      let article_id = article |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
      let article_num = article |> Yojson.Basic.Util.member "num" |> Yojson.Basic.Util.to_string in
      let article_content =
        article |> Yojson.Basic.Util.member "content" |> Yojson.Basic.Util.to_string |> clean_html
      in
      { id = article_id; num = article_num; content = article_content })
    articles
