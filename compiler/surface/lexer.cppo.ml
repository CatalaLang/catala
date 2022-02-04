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
module L = Lexer_common
module R = Re.Pcre

(* The localised strings and regexps for the tokens and specific parsing rules
   are defined as CPPO macros in the `lexer_XX.cppo.ml` files.

   - `MS_*` macros define token strings
   - `MR_*` are sedlex regexps matching the token (inferred from the strings if absent,
     but should be present for any token containing spacing, and for any non-latin1
     character)
   - `MX_*` are full matching rules of the form `sedlex regexp -> ocaml expression`
*)

(* Avoid the need for defining the regexps when they are simple strings *)
#ifndef MR_SCOPE
  #define MR_SCOPE MS_SCOPE
#endif
#ifndef MR_CONSEQUENCE
  #define MR_CONSEQUENCE MS_CONSEQUENCE
#endif
#ifndef MR_DATA
  #define MR_DATA MS_DATA
#endif
#ifndef MR_DEPENDS
  #define MR_DEPENDS MS_DEPENDS
#endif
#ifndef MR_DECLARATION
  #define MR_DECLARATION MS_DECLARATION
#endif
#ifndef MR_CONTEXT
  #define MR_CONTEXT MS_CONTEXT
#endif
#ifndef MR_DECREASING
  #define MR_DECREASING MS_DECREASING
#endif
#ifndef MR_INCREASING
  #define MR_INCREASING MS_INCREASING
#endif
#ifndef MR_OF
  #define MR_OF MS_OF
#endif
#ifndef MR_COLLECTION
  #define MR_COLLECTION MS_COLLECTION
#endif
#ifndef MR_ENUM
  #define MR_ENUM MS_ENUM
#endif
#ifndef MR_INTEGER
  #define MR_INTEGER MS_INTEGER
#endif
#ifndef MR_MONEY
  #define MR_MONEY MS_MONEY
#endif
#ifndef MR_TEXT
  #define MR_TEXT MS_TEXT
#endif
#ifndef MR_DECIMAL
  #define MR_DECIMAL MS_DECIMAL
#endif
#ifndef MR_DATE
  #define MR_DATE MS_DATE
#endif
#ifndef MR_DURATION
  #define MR_DURATION MS_DURATION
#endif
#ifndef MR_BOOLEAN
  #define MR_BOOLEAN MS_BOOLEAN
#endif
#ifndef MR_SUM
  #define MR_SUM MS_SUM
#endif
#ifndef MR_FILLED
  #define MR_FILLED MS_FILLED
#endif
#ifndef MR_DEFINITION
  #define MR_DEFINITION MS_DEFINITION
#endif
#ifndef MR_LABEL
  #define MR_LABEL MS_LABEL
#endif
#ifndef MR_EXCEPTION
  #define MR_EXCEPTION MS_EXCEPTION
#endif
#ifndef MR_DEFINED_AS
  #define MR_DEFINED_AS MS_DEFINED_AS
#endif
#ifndef MR_MATCH
  #define MR_MATCH MS_MATCH
#endif
#ifndef MR_WILDCARD
  #define MR_WILDCARD MS_WILDCARD
#endif
#ifndef MR_WITH
  #define MR_WITH MS_WITH
#endif
#ifndef MR_UNDER_CONDITION
  #define MR_UNDER_CONDITION MS_UNDER_CONDITION
#endif
#ifndef MR_IF
  #define MR_IF MS_IF
#endif
#ifndef MR_THEN
  #define MR_THEN MS_THEN
#endif
#ifndef MR_ELSE
  #define MR_ELSE MS_ELSE
#endif
#ifndef MR_CONDITION
  #define MR_CONDITION MS_CONDITION
#endif
#ifndef MR_CONTENT
  #define MR_CONTENT MS_CONTENT
#endif
#ifndef MR_STRUCT
  #define MR_STRUCT MS_STRUCT
#endif
#ifndef MR_ASSERTION
  #define MR_ASSERTION MS_ASSERTION
#endif
#ifndef MR_VARIES
  #define MR_VARIES MS_VARIES
#endif
#ifndef MR_WITH_V
  #define MR_WITH_V MS_WITH_V
#endif
#ifndef MR_FOR
  #define MR_FOR MS_FOR
#endif
#ifndef MR_ALL
  #define MR_ALL MS_ALL
#endif
#ifndef MR_WE_HAVE
  #define MR_WE_HAVE MS_WE_HAVE
#endif
#ifndef MR_FIXED
  #define MR_FIXED MS_FIXED
#endif
#ifndef MR_BY
  #define MR_BY MS_BY
#endif
#ifndef MR_RULE
  #define MR_RULE MS_RULE
#endif
#ifndef MR_EXISTS
  #define MR_EXISTS MS_EXISTS
#endif
#ifndef MR_IN
  #define MR_IN MS_IN
#endif
#ifndef MR_SUCH
  #define MR_SUCH MS_SUCH
#endif
#ifndef MR_THAT
  #define MR_THAT MS_THAT
#endif
#ifndef MR_AND
  #define MR_AND MS_AND
#endif
#ifndef MR_OR
  #define MR_OR MS_OR
#endif
#ifndef MR_XOR
  #define MR_XOR MS_XOR
#endif
#ifndef MR_NOT
  #define MR_NOT MS_NOT
#endif
#ifndef MR_MAXIMUM
  #define MR_MAXIMUM MS_MAXIMUM
#endif
#ifndef MR_MINIMUM
  #define MR_MINIMUM MS_MINIMUM
#endif
#ifndef MR_FILTER
  #define MR_FILTER MS_FILTER
#endif
#ifndef MR_MAP
  #define MR_MAP MS_MAP
#endif
#ifndef MR_INIT
  #define MR_INIT MS_INIT
#endif
#ifndef MR_CARDINAL
  #define MR_CARDINAL MS_CARDINAL
#endif
#ifndef MR_YEAR
  #define MR_YEAR MS_YEAR
#endif
#ifndef MR_MONTH
  #define MR_MONTH MS_MONTH
#endif
#ifndef MR_DAY
  #define MR_DAY MS_DAY
#endif
#ifndef MR_TRUE
  #define MR_TRUE MS_TRUE
#endif
#ifndef MR_FALSE
  #define MR_FALSE MS_FALSE
#endif
#ifndef MR_IntToDec
  #define MR_IntToDec MS_IntToDec
#endif
#ifndef MR_GetDay
  #define MR_GetDay MS_GetDay
#endif
#ifndef MR_GetMonth
  #define MR_GetMonth MS_GetMonth
#endif
#ifndef MR_GetYear
  #define MR_GetYear MS_GetYear
#endif
#ifndef MR_INPUT
  #define MR_INPUT MS_INPUT
#endif
#ifndef MR_OUTPUT
  #define MR_OUTPUT MS_OUTPUT
#endif
#ifndef MR_INTERNAL
  #define MR_INTERNAL MS_INTERNAL
#endif

let token_list : (string * token) list =
  [
    (MS_SCOPE, SCOPE);
    (MS_CONSEQUENCE, CONSEQUENCE);
    (MS_DATA, DATA);
    (MS_DEPENDS, DEPENDS);
    (MS_DECLARATION, DECLARATION);
    (MS_CONTEXT, CONTEXT);
    (MS_DECREASING, DECREASING);
    (MS_INCREASING, INCREASING);
    (MS_OF, OF);
    (MS_COLLECTION, COLLECTION);
    (MS_ENUM, ENUM);
    (MS_INTEGER, INTEGER);
    (MS_MONEY, MONEY);
    (MS_TEXT, TEXT);
    (MS_DECIMAL, DECIMAL);
    (MS_DATE, DATE);
    (MS_DURATION, DURATION);
    (MS_BOOLEAN, BOOLEAN);
    (MS_SUM, SUM);
    (MS_FILLED, FILLED);
    (MS_DEFINITION, DEFINITION);
    (MS_LABEL, LABEL);
    (MS_EXCEPTION, EXCEPTION);
    (MS_DEFINED_AS, DEFINED_AS);
    (MS_MATCH, MATCH);
    (MS_WILDCARD, WILDCARD);
    (MS_WITH, WITH);
    (MS_UNDER_CONDITION, UNDER_CONDITION);
    (MS_IF, IF);
    (MS_THEN, THEN);
    (MS_ELSE, ELSE);
    (MS_CONDITION, CONDITION);
    (MS_CONTENT, CONTENT);
    (MS_STRUCT, STRUCT);
    (MS_ASSERTION, ASSERTION);
    (MS_VARIES, VARIES);
    (MS_WITH_V, WITH_V);
    (MS_FOR, FOR);
    (MS_ALL, ALL);
    (MS_WE_HAVE, WE_HAVE);
    (MS_FIXED, FIXED);
    (MS_BY, BY);
    (MS_RULE, RULE);
    (MS_EXISTS, EXISTS);
    (MS_IN, IN);
    (MS_SUCH, SUCH);
    (MS_THAT, THAT);
    (MS_AND, AND);
    (MS_OR, OR);
    (MS_XOR, XOR);
    (MS_NOT, NOT);
    (MS_MAXIMUM, MAXIMUM);
    (MS_MINIMUM, MINIMUM);
    (MS_FILTER, FILTER);
    (MS_MAP, MAP);
    (MS_INIT, INIT);
    (MS_CARDINAL, CARDINAL);
    (MS_YEAR, YEAR);
    (MS_MONTH, MONTH);
    (MS_DAY, DAY);
    (MS_TRUE, TRUE);
    (MS_FALSE, FALSE);
    (MS_INPUT, INPUT);
    (MS_OUTPUT, OUTPUT);
    (MS_INTERNAL, INTERNAL)
  ]
  @ L.token_list_language_agnostic

(** Localised builtin functions *)
let lex_builtin (s : string) : Ast.builtin_expression option =
  let lexbuf = Utf8.from_string s in
  match%sedlex lexbuf with
  | MR_IntToDec, eof -> Some IntToDec
  | MR_GetDay, eof -> Some GetDay
  | MR_GetMonth, eof -> Some GetMonth
  | MR_GetYear, eof -> Some GetYear
  | _ -> None

(** Regexp matching any digit character.

    @note can not be used outside the current module (@see <
    https://github.com/ocaml-community/sedlex#lexer-specifications >). *)
let digit = [%sedlex.regexp? '0' .. '9']

(** Regexp matching at least one space. *)
let space_plus = [%sedlex.regexp? Plus white_space]

(** Regexp matching white space but not newlines *)
let hspace = [%sedlex.regexp? Sub (white_space, Chars "\n\r")]

(** Main lexing function used in code blocks *)
let rec lex_code (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | white_space ->
      (* Whitespaces *)
      L.update_acc lexbuf;
      lex_code lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      L.update_acc lexbuf;
      lex_code lexbuf
  | "```" ->
      (* End of code section *)
      L.context := Law;
      END_CODE (Buffer.contents L.code_buffer)
  | MR_SCOPE ->
      L.update_acc lexbuf;
      SCOPE
  | MR_DATA ->
      L.update_acc lexbuf;
      DATA
  | MR_DEPENDS ->
      L.update_acc lexbuf;
      DEPENDS
  | MR_DECLARATION ->
      L.update_acc lexbuf;
      DECLARATION
  | MR_CONTEXT ->
      L.update_acc lexbuf;
      CONTEXT
  | MR_DECREASING ->
      L.update_acc lexbuf;
      DECREASING
  | MR_INCREASING ->
      L.update_acc lexbuf;
      INCREASING
  | MR_OF ->
      L.update_acc lexbuf;
      OF
  | MR_COLLECTION ->
      L.update_acc lexbuf;
      COLLECTION
  | MR_ENUM ->
      L.update_acc lexbuf;
      ENUM
  | MR_INTEGER ->
      L.update_acc lexbuf;
      INTEGER
  | MR_MONEY ->
      L.update_acc lexbuf;
      MONEY
  | MR_TEXT ->
      L.update_acc lexbuf;
      TEXT
  | MR_DECIMAL ->
      L.update_acc lexbuf;
      DECIMAL
  | MR_DATE ->
      L.update_acc lexbuf;
      DATE
  | MR_DURATION ->
      L.update_acc lexbuf;
      DURATION
  | MR_BOOLEAN ->
      L.update_acc lexbuf;
      BOOLEAN
  | MR_SUM ->
      L.update_acc lexbuf;
      SUM
  | MR_FILLED ->
      L.update_acc lexbuf;
      FILLED
  | MR_DEFINITION ->
      L.update_acc lexbuf;
      DEFINITION
  | MR_LABEL ->
      L.update_acc lexbuf;
      LABEL
  | MR_EXCEPTION ->
      L.update_acc lexbuf;
      EXCEPTION
  | MR_DEFINED_AS ->
      L.update_acc lexbuf;
      DEFINED_AS
  | MR_MATCH ->
      L.update_acc lexbuf;
      MATCH
  | MR_WITH ->
      L.update_acc lexbuf;
      WITH
  | MR_WILDCARD ->
      L.update_acc lexbuf;
      WILDCARD
  | MR_UNDER_CONDITION ->
      L.update_acc lexbuf;
      UNDER_CONDITION
  | MR_IF ->
      L.update_acc lexbuf;
      IF
  | MR_CONSEQUENCE ->
      L.update_acc lexbuf;
      CONSEQUENCE
  | MR_THEN ->
      L.update_acc lexbuf;
      THEN
  | MR_ELSE ->
      L.update_acc lexbuf;
      ELSE
  | MR_CONDITION ->
      L.update_acc lexbuf;
      CONDITION
  | MR_CONTENT ->
      L.update_acc lexbuf;
      CONTENT
  | MR_STRUCT ->
      L.update_acc lexbuf;
      STRUCT
  | MR_ASSERTION ->
      L.update_acc lexbuf;
      ASSERTION
  | MR_VARIES ->
      L.update_acc lexbuf;
      VARIES
  | MR_WITH_V ->
      L.update_acc lexbuf;
      WITH_V
  | MR_FOR ->
      L.update_acc lexbuf;
      FOR
  | MR_ALL ->
      L.update_acc lexbuf;
      ALL
  | MR_WE_HAVE ->
      L.update_acc lexbuf;
      WE_HAVE
  | MR_FIXED ->
      L.update_acc lexbuf;
      FIXED
  | MR_BY ->
      L.update_acc lexbuf;
      BY
  | MR_RULE ->
      L.update_acc lexbuf;
      RULE
  | MR_EXISTS ->
      L.update_acc lexbuf;
      EXISTS
  | MR_IN ->
      L.update_acc lexbuf;
      IN
  | MR_SUCH ->
      L.update_acc lexbuf;
      SUCH
  | MR_THAT ->
      L.update_acc lexbuf;
      THAT
  | MR_AND ->
      L.update_acc lexbuf;
      AND
  | MR_OR ->
      L.update_acc lexbuf;
      OR
  | MR_XOR ->
      L.update_acc lexbuf;
      XOR
  | MR_NOT ->
      L.update_acc lexbuf;
      NOT
  | MR_MAXIMUM ->
      L.update_acc lexbuf;
      MAXIMUM
  | MR_MINIMUM ->
      L.update_acc lexbuf;
      MINIMUM
  | MR_FILTER ->
      L.update_acc lexbuf;
      FILTER
  | MR_MAP ->
      L.update_acc lexbuf;
      MAP
  | MR_INIT ->
      L.update_acc lexbuf;
      INIT
  | MR_CARDINAL ->
      L.update_acc lexbuf;
      CARDINAL
  | MR_TRUE ->
      L.update_acc lexbuf;
      TRUE
  | MR_FALSE ->
      L.update_acc lexbuf;
      FALSE
  | MR_YEAR ->
      L.update_acc lexbuf;
      YEAR
  | MR_MONTH ->
      L.update_acc lexbuf;
      MONTH
  | MR_DAY ->
      L.update_acc lexbuf;
      DAY
  | MR_MONEY_PREFIX, digit, Opt (Star (digit | MR_MONEY_DELIM), digit), Opt (MC_DECIMAL_SEPARATOR, Rep (digit, 0 .. 2)), MR_MONEY_SUFFIX ->
      let s = Utf8.lexeme lexbuf in
      let units = Buffer.create (String.length s) in
      let cents = Buffer.create 2 in
      let buf = ref units in
      for i = 0 to String.length s - 1 do
        match s.[i] with
        | '0'..'9' as c -> Buffer.add_char !buf c
        | MC_DECIMAL_SEPARATOR -> buf := cents
        | _ -> ()
      done;
      L.update_acc lexbuf;
      MONEY_AMOUNT (Runtime.integer_of_string (Buffer.contents units), Runtime.integer_of_string (Buffer.contents cents))
  | Plus digit, MC_DECIMAL_SEPARATOR, Star digit ->
    let rex =
      Re.(compile @@ whole_string @@ seq [
          group (rep1 digit);
          char MC_DECIMAL_SEPARATOR;
          group (rep digit)
        ]) in
    let dec_parts = R.get_substring (R.exec ~rex (Utf8.lexeme lexbuf)) in
    L.update_acc lexbuf;
    DECIMAL_LITERAL
      (Runtime.integer_of_string (dec_parts 1), Runtime.integer_of_string (dec_parts 2))
  | "<=@" ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DATE
  | "<@" ->
      L.update_acc lexbuf;
      LESSER_DATE
  | ">=@" ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DATE
  | ">@" ->
      L.update_acc lexbuf;
      GREATER_DATE
  | "-@" ->
      L.update_acc lexbuf;
      MINUSDATE
  | "+@" ->
      L.update_acc lexbuf;
      PLUSDATE
  | "<=^" ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DURATION
  | "<^" ->
      L.update_acc lexbuf;
      LESSER_DURATION
  | ">=^" ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DURATION
  | ">^" ->
      L.update_acc lexbuf;
      GREATER_DURATION
  | "+^" ->
      L.update_acc lexbuf;
      PLUSDURATION
  | "-^" ->
      L.update_acc lexbuf;
      MINUSDURATION
  | "/^" ->
      L.update_acc lexbuf;
      DIVDURATION
  | "<=", MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      LESSER_EQUAL_MONEY
  | '<', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      LESSER_MONEY
  | ">=", MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      GREATER_EQUAL_MONEY
  | '>', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      GREATER_MONEY
  | '+', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      PLUSMONEY
  | '-', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      MINUSMONEY
  | '*', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      MULTMONEY
  | '/', MR_MONEY_OP_SUFFIX ->
      L.update_acc lexbuf;
      DIVMONEY
  | "<=." ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DEC
  | "<." ->
      L.update_acc lexbuf;
      LESSER_DEC
  | ">=." ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DEC
  | ">." ->
      L.update_acc lexbuf;
      GREATER_DEC
  | "+." ->
      L.update_acc lexbuf;
      PLUSDEC
  | "-." ->
      L.update_acc lexbuf;
      MINUSDEC
  | "*." ->
      L.update_acc lexbuf;
      MULTDEC
  | "/." ->
      L.update_acc lexbuf;
      DIVDEC
  | "<=" ->
      L.update_acc lexbuf;
      LESSER_EQUAL
  | '<' ->
      L.update_acc lexbuf;
      LESSER
  | ">=" ->
      L.update_acc lexbuf;
      GREATER_EQUAL
  | '>' ->
      L.update_acc lexbuf;
      GREATER
  | '+' ->
      L.update_acc lexbuf;
      PLUS
  | '-' ->
      L.update_acc lexbuf;
      MINUS
  | '*' ->
      L.update_acc lexbuf;
      MULT
  | '/' ->
      L.update_acc lexbuf;
      DIV
  | "!=" ->
      L.update_acc lexbuf;
      NOT_EQUAL
  | '=' ->
      L.update_acc lexbuf;
      EQUAL
  | '%' ->
      L.update_acc lexbuf;
      PERCENT
  | '(' ->
      L.update_acc lexbuf;
      LPAREN
  | ')' ->
      L.update_acc lexbuf;
      RPAREN
  | '{' ->
      L.update_acc lexbuf;
      LBRACKET
  | '}' ->
      L.update_acc lexbuf;
      RBRACKET
  | '[' ->
      L.update_acc lexbuf;
      LSQUARE
  | ']' ->
      L.update_acc lexbuf;
      RSQUARE
  | '|' ->
      L.update_acc lexbuf;
      VERTICAL
  | ':' ->
      L.update_acc lexbuf;
      COLON
  | ';' ->
      L.update_acc lexbuf;
      SEMICOLON
  | "--" ->
      L.update_acc lexbuf;
      ALT
  | "++" ->
      L.update_acc lexbuf;
      PLUSPLUS
  | '.' ->
      L.update_acc lexbuf;
      DOT
  | uppercase, Star (uppercase | lowercase | digit | '_' | '\'') ->
      (* Name of constructor *)
      L.update_acc lexbuf;
      CONSTRUCTOR (Utf8.lexeme lexbuf)
  | lowercase, Star (lowercase | uppercase | digit | '_' | '\'') ->
      (* Name of variable *)
      L.update_acc lexbuf;
      IDENT (Utf8.lexeme lexbuf)
  | Plus digit ->
      (* Integer literal*)
      L.update_acc lexbuf;
      INT_LITERAL (Runtime.integer_of_string (Utf8.lexeme lexbuf))
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

let rec lex_directive_args (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | '@', Star hspace, "p.", Star hspace, Plus digit ->
      let s = Utf8.lexeme lexbuf in
      let i = String.index s '.' in
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
  | Plus (Compl white_space) -> DIRECTIVE_ARG (Utf8.lexeme lexbuf)
  | Plus hspace -> lex_directive_args lexbuf
  | '\n' | eof ->
      L.context := Law;
      END_DIRECTIVE
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

let rec lex_directive (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | Plus hspace -> lex_directive lexbuf
  | MR_LAW_INCLUDE -> LAW_INCLUDE
  | ":" ->
      L.context := Directive_args;
      COLON
  | '\n' | eof ->
      L.context := Law;
      END_DIRECTIVE
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Main lexing function used outside code blocks *)
let lex_law (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let ((_, start_pos) as prev_pos) = lexing_positions lexbuf in
  let at_bol = Lexing.(start_pos.pos_bol = start_pos.pos_cnum) in
  if at_bol then
    match%sedlex lexbuf with
    | eof -> EOF
    | "```catala", Star white_space, ('\n' | eof) ->
        L.context := Code;
        Buffer.clear L.code_buffer;
        BEGIN_CODE
    | "```catala-metadata", Star white_space, ('\n' | eof) ->
        L.context := Code;
        Buffer.clear L.code_buffer;
        BEGIN_METADATA
    | '>' ->
        L.context := Directive;
        BEGIN_DIRECTIVE
    | Plus '#', Star hspace, Plus (Compl '\n'), Star hspace, ('\n' | eof) ->
        L.get_law_heading lexbuf
    | _ -> (
        (* Nested match for lower priority; `_` matches length 0 so we effectively retry the
           sub-match at the same point *)
        let lexbuf = lexbuf in
        (* workaround sedlex bug, see https://github.com/ocaml-community/sedlex/issues/12 *)
        match%sedlex lexbuf with
        | Star (Compl '\n'), ('\n' | eof) -> LAW_TEXT (Utf8.lexeme lexbuf)
        | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme)
  else
    match%sedlex lexbuf with
    | eof -> EOF
    | Star (Compl '\n'), ('\n' | eof) -> LAW_TEXT (Utf8.lexeme lexbuf)
    | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Entry point of the lexer, distributes to {!val: lex_code} or {!val:lex_law}
    depending of the current {!val: Surface.Lexer_common.context}. *)
let lexer (lexbuf : lexbuf) : token =
  match !L.context with
  | Law -> lex_law lexbuf
  | Code -> lex_code lexbuf
  | Directive -> lex_directive lexbuf
  | Directive_args -> lex_directive_args lexbuf
