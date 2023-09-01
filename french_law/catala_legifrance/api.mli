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

(** Performs API requests and manipulates API data. Needs a working Internet
    connection to work *)

(** {2 Requests}*)

type access_token
(** The [access_token] is the OAuth token used in every API request for
    authentication *)

val get_token : string -> string -> access_token Lwt.t
(** [get_token cliend_id client_secret] retrieves the access token from the
    LegiFrance API. You have to register on the
    {{:https://developer.aife.economie.gouv.fr/} the official website of the
      French government} to get your OAuth client ID and Secret for the
    LegiFrance API *)

type article
type article_id

val parse_id : string -> article_id
(** [parse_id id] parses the string representing the LégiFrance object to be
    fetched from the API, checks its validity (for instance
    ["LEGIARTI000006307920"]) and returns an [object_id]*)

val retrieve_article : access_token -> article_id -> article Lwt.t
(** [retrieve_article token article_id] returns the article from the LegiFrance
    API.*)

(**{2 Manipulating API objects}*)

(**{3 Articles}*)

val get_article_id : article -> string
val get_article_text : article -> string
val get_article_title : article -> string
val get_article_expiration_date : article -> Unix.tm
val get_article_new_version : article -> string
