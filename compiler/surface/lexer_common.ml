(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Tokens
open Sedlexing
open Utils
module R = Re.Pcre

(* Calculates the precedence according a {!val: matched_regex} of the form : '[#]+'.

   @note -2 because [LAW_HEADING] start with at least "#" and the number of '#' remaining
   corresponds to the precedence. *)
let calc_precedence (matched_regex : string) : int = String.length matched_regex - 1

(* Gets the [LAW_HEADING] token from the current {!val: lexbuf} *)
let get_law_heading (lexbuf : lexbuf) : token =
  let extract_article_title =
    R.regexp "([#]+)\\s*([^\\|]+)(\\|([^\\|]+)|)(\\|\\s*([0-9]{4}\\-[0-9]{2}\\-[0-9]{2})|)"
  in
  let get_substring = R.get_substring (R.exec ~rex:extract_article_title (Utf8.lexeme lexbuf)) in
  let title = String.trim (get_substring 2) in
  let article_id = try Some (String.trim (get_substring 4)) with Not_found -> None in
  let article_expiration_date = try Some (String.trim (get_substring 6)) with Not_found -> None in
  let precedence = calc_precedence (String.trim (get_substring 1)) in
  LAW_HEADING (title, article_id, article_expiration_date, precedence)

type lexing_context = Law | Code | Directive | Directive_args

(** Boolean reference, used by the lexer as the mutable state to distinguish whether it is lexing
    code or law. *)
let context : lexing_context ref = ref Law

(** Mutable string reference that accumulates the string representation of the body of code being
    lexed. This string representation is used in the literate programming backends to faithfully
    capture the spacing pattern of the original program *)
let code_buffer : Buffer.t = Buffer.create 4000

(** Updates {!val:code_buffer} with the current lexeme *)
let update_acc (lexbuf : lexbuf) : unit = Buffer.add_string code_buffer (Utf8.lexeme lexbuf)

(** Error-generating helper *)
let raise_lexer_error (loc : Pos.t) (token : string) =
  Errors.raise_spanned_error
    (Printf.sprintf "Parsing error after token \"%s\": what comes after is unknown" token)
    loc

(** Associative list matching each punctuation string part of the Catala syntax with its {!module:
    Surface.Parser} token. Same for all the input languages (English, French, etc.) *)
let token_list_language_agnostic : (string * token) list =
  [
    (".", DOT);
    ("<=", LESSER_EQUAL);
    (">=", GREATER_EQUAL);
    (">", GREATER);
    ("!=", NOT_EQUAL);
    ("=", EQUAL);
    ("(", LPAREN);
    (")", RPAREN);
    ("{", LBRACKET);
    ("}", RBRACKET);
    ("{", LSQUARE);
    ("}", RSQUARE);
    ("+", PLUS);
    ("-", MINUS);
    ("*", MULT);
    ("/", DIV);
    ("|", VERTICAL);
    (":", COLON);
    (";", SEMICOLON);
    ("--", ALT);
    ("++", PLUSPLUS);
  ]

module type LocalisedLexer = sig
  val token_list : (string * Tokens.token) list
  (** Same as {!val: token_list_language_agnostic}, but with tokens specialized to a given language. *)

  val lex_builtin : string -> Ast.builtin_expression option
  (** Simple lexer for builtins *)

  val lex_code : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used in code blocks *)

  val lex_law : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used outside code blocks *)

  val lexer : Sedlexing.lexbuf -> Tokens.token
  (** Entry point of the lexer, distributes to {!val: lex_code} or {!val:lex_law} depending of the
      current {!val: Surface.Lexer_common.context}. *)
end
