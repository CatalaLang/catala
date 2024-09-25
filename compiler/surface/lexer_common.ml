(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Tokens
open Sedlexing
open Catala_utils
module R = Re.Pcre

(* Calculates the precedence according a {!val: matched_regex} of the form :
   '[#]+'.

   @note -2 because [LAW_HEADING] start with at least "#" and the number of '#'
   remaining corresponds to the precedence. *)
let calc_precedence (matched_regex : string) : int =
  String.length matched_regex - 1

(* Gets the [LAW_HEADING] token from the current {!val: lexbuf} *)
let get_law_heading (lexbuf : lexbuf) : token =
  let extract_article_title =
    R.regexp "([#]+)\\s*([^\\|]+)(\\|\\s*([^\\s]+)|)(\\s*(\\[archive\\])|)"
  in
  let rex = R.exec ~rex:extract_article_title (Utf8.lexeme lexbuf) in
  let title = String.trim (R.get_substring rex 2) in
  let article_id =
    try Some (String.trim (R.get_substring rex 4)) with Not_found -> None
  in
  let is_archive = Option.is_some (Re.Group.get_opt rex 6) in
  let precedence = calc_precedence (String.trim (R.get_substring rex 1)) in
  LAW_HEADING (title, article_id, is_archive, precedence)

type lexing_context = Law | Raw | Code | Directive | Directive_args | Inactive

(** Boolean reference, used by the lexer as the mutable state to distinguish
    whether it is lexing code or law. *)
let context : lexing_context ref = ref Inactive

(** Mutable string reference that accumulates the string representation of the
    body of code being lexed. This string representation is used in the literate
    programming backends to faithfully capture the spacing pattern of the
    original program *)
let code_buffer : Buffer.t option ref = ref None

let with_lexing_context filename f =
  let saved_context = !context in
  let saved_buffer = !code_buffer in
  context := Law;
  code_buffer := Some (Buffer.create 4000);
  Fun.protect f ~finally:(fun () ->
      if
        !context <> Law
        || match !code_buffer with Some b -> Buffer.length b > 0 | _ -> false
      then
        Message.warning
          "Unclosed block or missing newline at the end of file %a.@ Did you \
           forget a @{<yellow>```@} ?"
          File.format filename;
      context := saved_context;
      code_buffer := saved_buffer)

(** Updates {!val:code_buffer} with the current lexeme *)
let update_acc (lexbuf : lexbuf) : unit =
  match !code_buffer with
  | None ->
    Message.error ~internal:true "Lexer update outside of a lexing context"
  | Some buf -> Buffer.add_string buf (Utf8.lexeme lexbuf)

let flush_acc () =
  match !code_buffer with
  | None ->
    Message.error ~internal:true "Lexer update outside of a lexing context"
  | Some buf ->
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s

exception Lexing_error of (Pos.t * string)

(** Error-generating helper *)
let raise_lexer_error (loc : Pos.t) (token : string) =
  raise (Lexing_error (loc, token))

(** Associative list matching each punctuation string part of the Catala syntax
    with its {!module: Surface.Parser} token. Same for all the input languages
    (English, French, etc.) *)
let token_list_language_agnostic : (string * token) list =
  [
    ".", DOT;
    "<=", LESSER_EQUAL KPoly;
    ">=", GREATER_EQUAL KPoly;
    ">", GREATER KPoly;
    "!=", NOT_EQUAL;
    "=", EQUAL;
    "(", LPAREN;
    ")", RPAREN;
    "{", LBRACE;
    "}", RBRACE;
    "{", LBRACKET;
    "}", RBRACKET;
    "+", PLUS KPoly;
    "-", MINUS KPoly;
    "*", MULT KPoly;
    "/", DIV KPoly;
    ":", COLON;
    ";", SEMICOLON;
    "--", ALT;
    "++", PLUSPLUS;
  ]

type line_token =
  | LINE_TEST of string (* ```catala-test { id = xx } *)
  | LINE_INLINE_TEST (* ```catala-test-inline *)
  | LINE_BLOCK_END (* ``` *)
  | LINE_INCLUDE of string (* > Include foo.catala_en *)
  | LINE_MODULE_DEF of string * bool (* > Module Xxx [external] *)
  | LINE_MODULE_USE of string (* > Using Xxx [as Yyy] *)
  | LINE_ANY (* anything else *)

module type LocalisedLexer = sig
  val token_list : (string * Tokens.token) list
  (** Same as {!val: token_list_language_agnostic}, but with tokens specialized
      to a given language. *)

  val lex_builtin : string -> Ast.builtin_expression option
  (** Simple lexer for builtins *)

  val lex_code : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used in code blocks *)

  val lex_law : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used outside code blocks *)

  val lexer : Sedlexing.lexbuf -> Tokens.token
  (** Entry point of the lexer, distributes to {!val: lex_code} or
      {!val:lex_law} depending of the current
      {!val:Surface.Lexer_common.context}. *)

  val lex_line : Sedlexing.lexbuf -> (string * line_token) option
  (** Low-level lexer intended for dependency extraction *)
end
