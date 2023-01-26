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

type access_token = string

let ( let* ) = Lwt.bind

let get_token_aux (client_id : string) (client_secret : string) :
    (Cohttp.Code.status_code * string) Lwt.t =
  let site = "https://oauth.aife.economie.gouv.fr" in
  let token_url = "/api/oauth/token" in
  let uri = Uri.of_string (site ^ token_url) in
  let headers =
    Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded"
  in
  let body_string =
    [
      "grant_type", "client_credentials";
      "client_id", client_id;
      "client_secret", client_secret;
      "scope", "openid";
    ]
    |> List.map (fun (k, v) -> Printf.sprintf {|%s=%s|} k v)
    |> String.concat "&"
    |> Printf.sprintf "%s"
  in
  let body = body_string |> Cohttp_lwt.Body.of_string in
  let* resp, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let status = resp |> Cohttp_lwt.Response.status in
  let* body = body |> Cohttp_lwt.Body.to_string in
  Lwt.return (status, body)

let get_token (client_id : string) (client_secret : string) : string Lwt.t =
  let rec retry count =
    if count = 0 then (
      Cli.debug_format "Too many retries, giving up\n";
      exit 1)
    else
      let* resp, body = get_token_aux client_id client_secret in
      if Cohttp.Code.code_of_status resp = 200 then begin
        let token =
          body
          |> Yojson.Basic.from_string
          |> Yojson.Basic.Util.member "access_token"
          |> Yojson.Basic.Util.to_string
        in
        Cli.debug_format "The LegiFrance API access token is %s" token;
        Lwt.return token
      end
      else if Cohttp.Code.code_of_status resp = 400 then begin
        Cli.debug_format "The API access request returned code 400%s\n"
          (if count > 1 then ", retrying..." else "");
        retry (count - 1)
      end
      else begin
        Cli.debug_format
          "The API access token request went wrong ; status is %s and the body \
           is\n\
           %s"
          (Cohttp.Code.string_of_status resp)
          body;
        exit 1
      end
  in
  retry 10

let site = "https://api.aife.economie.gouv.fr"
let base_token_url = "/dila/legifrance-beta/lf-engine-app/"

let api_timestamp_to_localtime (timestamp : int) : Unix.tm =
  Unix.localtime (float_of_int (timestamp / 1000))

let make_request
    (access_token : string)
    (token_url : string)
    (body_json : (string * string) list)
    () : (string * string) Lwt.t =
  let uri = Uri.of_string (site ^ base_token_url ^ token_url) in
  let headers =
    Cohttp.Header.init_with "Authorization"
      (Printf.sprintf "Bearer %s" access_token)
  in
  let headers = Cohttp.Header.add headers "Content-Type" "application/json" in
  let headers = Cohttp.Header.add headers "Accept" "application/json" in
  let body_string =
    body_json
    |> List.map (fun (k, v) -> Printf.sprintf {|"%s":"%s"|} k v)
    |> String.concat ","
    |> Printf.sprintf "{%s}"
  in
  let body = body_string |> Cohttp_lwt.Body.of_string in
  let* resp, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let resp =
    resp |> Cohttp_lwt.Response.status |> Cohttp.Code.string_of_status
  in
  let* body = body |> Cohttp_lwt.Body.to_string in
  Lwt.return (resp, body)

type article_type = LEGIARTI | CETATEXT | JORFARTI
type article_id = { id : string; typ : article_type }
type article = { content : Yojson.Basic.t; typ : article_type }

let run_request (request : unit -> (string * string) Lwt.t) :
    Yojson.Basic.t Lwt.t =
  let handle_once resp body =
    if resp = "200 OK" then
      try body |> Yojson.Basic.from_string with
      | Yojson.Basic.Util.Type_error (msg, obj) ->
        Cli.error_print
          "Error while parsing JSON answer from API: %s\n\
           Specific JSON:\n\
           %s\n\
           Full answer:\n\
           %s"
          msg
          (Yojson.Basic.to_string obj)
          body;
        exit (-1)
      | _ -> raise (Failure "")
    else raise (Failure "")
  in
  let rec try_n_times (n : int) =
    let* resp, body = request () in
    try Lwt.return (handle_once resp body)
    with Failure _ ->
      if n > 0 then (
        Unix.sleep 2;
        Cli.debug_format "Retrying request...";
        try_n_times (n - 1))
      else (
        Cli.error_print
          "The API request went wrong ; status is %s and the body is\n%s" resp
          body;
        exit (-1))
  in
  try_n_times 5

let parse_id (id : string) : article_id =
  let legi_rex =
    Re.(compile @@ whole_string @@ seq [str "LEGIARTI"; repn digit 12 None])
  in
  let ceta_tex =
    Re.(compile @@ whole_string @@ seq [str "CETATEXT"; repn digit 12 None])
  in
  let jorf_rex =
    Re.(compile @@ whole_string @@ seq [str "JORFARTI"; repn digit 12 None])
  in
  let typ =
    if Re.execp legi_rex id then LEGIARTI
    else if Re.execp ceta_tex id then CETATEXT
    else if Re.execp jorf_rex id then JORFARTI
    else
      Errors.raise_error
        "LégiFrance ID \"%s\" does not correspond to an ID format recognized \
         by the LégiFrance API"
        id
  in
  { id; typ }

let retrieve_article (access_token : string) (obj : article_id) : article Lwt.t
    =
  Cli.debug_format "Accessing article %s" obj.id;
  let* content =
    run_request
      (make_request access_token
         (match obj.typ with
         | CETATEXT -> "consult/juri"
         | LEGIARTI | JORFARTI -> "consult/getArticle")
         (match obj.typ with
         | CETATEXT -> ["textId", obj.id]
         | LEGIARTI | JORFARTI -> ["id", obj.id]))
  in
  Lwt.return { content; typ = obj.typ }

let raise_article_parsing_error
    (json : Yojson.Basic.t)
    (msg : string)
    (obj : Yojson.Basic.t) =
  Cli.error_print
    "Error while manipulating JSON answer from API: %s\n\
     Specific JSON:\n\
     %s\n\
     Full answer:\n\
     %s"
    msg
    (Yojson.Basic.to_string obj)
    (Yojson.Basic.to_string json);
  exit 1

let get_article_id (article : article) : string =
  try
    article.content
    |> Yojson.Basic.Util.member
         (match article.typ with
         | CETATEXT -> "text"
         | LEGIARTI | JORFARTI -> "article")
    |> Yojson.Basic.Util.member "id"
    |> Yojson.Basic.Util.to_string
  with Yojson.Basic.Util.Type_error (msg, obj) ->
    raise_article_parsing_error article.content msg obj

let get_article_text (article : article) : string =
  try
    let text =
      article.content
      |> Yojson.Basic.Util.member
           (match article.typ with
           | CETATEXT -> "text"
           | LEGIARTI | JORFARTI -> "article")
      |> Yojson.Basic.Util.member "texte"
      |> Yojson.Basic.Util.to_string
    in
    (* there might be a nota *)
    let nota =
      try
        article.content
        |> Yojson.Basic.Util.member
             (match article.typ with
             | CETATEXT -> "text"
             | LEGIARTI | JORFARTI -> "article")
        |> Yojson.Basic.Util.member "nota"
        |> Yojson.Basic.Util.to_string
      with Yojson.Basic.Util.Type_error _ -> ""
    in
    text ^ " " ^ if nota <> "" then "NOTA : " ^ nota else ""
  with Yojson.Basic.Util.Type_error (msg, obj) ->
    raise_article_parsing_error article.content msg obj

let get_article_title (article : article) : string =
  try
    article.content
    |> Yojson.Basic.Util.member
         (match article.typ with
         | CETATEXT -> "text"
         | LEGIARTI | JORFARTI -> "article")
    |> Yojson.Basic.Util.member "titre"
    |> Yojson.Basic.Util.to_string
  with Yojson.Basic.Util.Type_error (msg, obj) ->
    raise_article_parsing_error article.content msg obj

let get_article_expiration_date (article : article) : Unix.tm =
  try
    let article_id = get_article_id article in
    match article.typ with
    | CETATEXT -> Date.parse_expiration_date DDMMYYYY "01/01/2999"
    | LEGIARTI | JORFARTI ->
      article.content
      |> Yojson.Basic.Util.member "article"
      |> Yojson.Basic.Util.member "articleVersions"
      |> Yojson.Basic.Util.to_list
      |> List.find (fun version ->
             Yojson.Basic.to_string (Yojson.Basic.Util.member "id" version)
             = "\"" ^ article_id ^ "\"")
      |> Yojson.Basic.Util.member "dateFin"
      |> Yojson.Basic.Util.to_int
      |> api_timestamp_to_localtime
  with Yojson.Basic.Util.Type_error (msg, obj) ->
    raise_article_parsing_error article.content msg obj

let get_article_new_version (article : article) : string =
  match article.typ with
  | CETATEXT -> get_article_id article
  | LEGIARTI | JORFARTI -> (
    let expiration_date = get_article_expiration_date article in
    let get_version_date_debut (version : Yojson.Basic.t) : Unix.tm =
      version
      |> Yojson.Basic.Util.member "dateDebut"
      |> Yojson.Basic.Util.to_int
      |> api_timestamp_to_localtime
    in
    try
      article.content
      |> Yojson.Basic.Util.member "article"
      |> Yojson.Basic.Util.member "articleVersions"
      |> Yojson.Basic.Util.to_list
      |> List.filter (fun version ->
             Date.date_compare expiration_date (get_version_date_debut version)
             <= 0)
      |> List.sort (fun version1 version2 ->
             Date.date_compare
               (get_version_date_debut version1)
               (get_version_date_debut version2))
      |> List.hd
      |> Yojson.Basic.Util.member "id"
      |> Yojson.Basic.Util.to_string
    with Yojson.Basic.Util.Type_error (msg, obj) ->
      raise_article_parsing_error article.content msg obj)
