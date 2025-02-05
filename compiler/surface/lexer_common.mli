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

open Catala_utils

(** Auxiliary functions used by all lexers. *)

type lexing_context = Law | Raw | Code | Directive | Directive_args | Inactive

val with_lexing_context : File.t -> (unit -> 'a) -> 'a
(** Initialises the lexing context during the call of the supplied function,
    which is required for using the lexer. Calls can be nested. Upon
    termination, emits a warning if the lexer is not in a consistent state
    ([Law] context, no pending code content) *)

val context : lexing_context ref
(** Reference, used by the lexer as the mutable state to distinguish whether it
    is lexing code or law. *)

val context_start_pos : (Lexing.position * Lexing.position) ref
(** The position of the opening token of the last opened [context] *)

val update_acc : Sedlexing.lexbuf -> unit
(** Updates the current code buffer with the current lexeme. The underlying
    buffer is used to accumulate the string representation of the body of code
    being lexed. This string representation is used in the literate programming
    backends to faithfully capture the spacing pattern of the original program *)

val flush_acc : unit -> string
(** Flushes the code buffer and returns its contents (see [update_acc]) *)

exception Lexing_error of (Catala_utils.Pos.t * string)

val raise_lexer_error : Catala_utils.Pos.t -> string -> 'a
(** Error-generating helper *)

val token_list_language_agnostic : (string * Tokens.token) list
(** Associative list matching each punctuation string part of the Catala syntax
    with its {!Surface.Parser} token. Same for all the input languages (English,
    French, etc.) *)

val calc_precedence : string -> int
(** Calculates the precedence according a matched regex of the form : '[#]+' *)

val get_law_heading : Sedlexing.lexbuf -> Tokens.token
(** Gets the [LAW_HEADING] token from the current [lexbuf] *)

(** Simplified tokens for dependency extraction *)
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
  (** Same as {!val:Surface.Lexer_common.token_list_language_agnostic}, but with
      tokens whose string varies with the input language. *)

  val lex_builtin : string -> Ast.builtin_expression option
  (** Simple lexer for builtins *)

  val lex_code : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used in a code block *)

  val lex_law : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used outside code blocks *)

  val lexer : Sedlexing.lexbuf -> Tokens.token
  (** Entry point of the lexer, distributes to {!val:lex_code} or {!val:lex_law}
      depending of the current {!val:Surface.Lexer_common.context}. *)

  val lex_line : Sedlexing.lexbuf -> (string * line_token) option
  (** Low-level lexer intended for dependency extraction. The whole line
      (including ["\n"] is always returned together with the token. [None] for
      EOF. *)
end
