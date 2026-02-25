(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

[@@@ocaml.warning "-27-32-33"]
(* TODO: remove warnings suppression *)

open Catala_utils
open Parser_utils
open Sedlexing
open Ast
open Tokens

type parsing_error

type parsing_result =
  | Ok of Ast.source_file
  | Partial of Ast.source_file * parsing_error list

(* Ring buffer *)
type t = {
  curr_idx : int;
  start : int ref;
  stop : int ref;
  max_size : int;
  feed : unit -> Tokens.token * Lexing.position * Lexing.position;
  data : (Tokens.token * Lexing.position * Lexing.position) array;
}

let next ({ curr_idx; start; stop; max_size; feed; data } as buff) =
  let next_idx = succ curr_idx mod max_size in
  if curr_idx = !stop then (
    let new_elt = feed () in
    data.(curr_idx) <- new_elt;
    let size = ((!stop - !start + max_size) mod max_size) + 1 in
    stop := succ !stop mod max_size;
    let is_full = size = max_size in
    if is_full then
      (* buffer will get full: start is also moved *)
      start := succ !start mod max_size;
    { buff with curr_idx = next_idx }, new_elt)
  else
    let elt = data.(curr_idx) in
    { buff with curr_idx = next_idx }, elt

let create ?(max_size = 20) feed =
  {
    curr_idx = 0;
    start = ref 0;
    stop = ref 0;
    feed;
    data = Array.make max_size Lexing.(Tokens.EOF, dummy_pos, dummy_pos);
    max_size;
  }

let skip_just_until p (buff : t) =
  (* FIXME but oh well.. *)
  let rec loop b =
    let new_b, (tok, _, _) = next b in
    if p tok then (* prev buffer *) Some b
    else if tok = Tokens.EOF then None
    else loop new_b
  in
  loop buff

module ParserAux (LocalisedLexer : Lexer_common.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)
  module I = MenhirInterpreter

  (** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match I.top env with None -> 0 | Some (Element (s, _, _, _)) -> I.number s

  let register_parsing_error
      (lexbuf : lexbuf)
      (env : 'semantic_value I.env)
      (acceptable_tokens : (string * Tokens.token) list)
      (similar_candidate_tokens : string list) : 'a =
    (* The parser has suspended itself because of a syntax error. *)
    let custom_menhir_message ppf =
      (match Parser_errors.message (state env) with
      | exception Not_found ->
        Format.fprintf ppf "@{<yellow>unexpected token.@}"
      | msg ->
        Format.fprintf ppf "@{<yellow>@<1>%s@} @[<hov>%a.@]" "»"
          Format.pp_print_text
          (String.trim (String.uncapitalize_ascii msg)));
      if acceptable_tokens <> [] then
        Format.fprintf ppf "@\n@[<hov>Those are valid at this point:@ %a.@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string))
          (List.map (fun (s, _) -> s) acceptable_tokens)
    in
    let suggestion =
      if similar_candidate_tokens = [] then None
      else Some similar_candidate_tokens
    in
    let error_loc = Pos.from_lpos (lexing_positions lexbuf) in
    let wrong_token = Utf8.lexeme lexbuf in
    if String.trim wrong_token = "```" then
      (* If the token is an ending code fence, override the message for an
         appropriate one. *)
      Message.delayed_error ~kind:Parsing () ?suggestion ~pos:error_loc
        "@[<hov>Syntax error in preceding code block@]"
    else
      Message.delayed_error ~kind:Parsing () ?suggestion ~pos:error_loc
        "@[<hov>Syntax error at %a:@ %t@]"
        (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
        wrong_token custom_menhir_message

  let sorted_candidate_tokens lexbuf token_list env =
    let acceptable_tokens =
      List.filter_map
        (fun ((_, t) as elt) ->
          if I.acceptable (I.input_needed env) t (fst (lexing_positions lexbuf))
          then Some elt
          else None)
        token_list
    in
    let lexeme = Utf8.lexeme lexbuf in
    let similar_acceptable_tokens =
      Suggestions.best_candidates (List.map fst acceptable_tokens) lexeme
    in
    let module S = Set.Make (String) in
    let s_toks = S.of_list similar_acceptable_tokens in
    let sorted_acceptable_tokens =
      List.sort
        (fun (s, _) _ -> if S.mem s s_toks then -1 else 1)
        acceptable_tokens
    in
    similar_acceptable_tokens, sorted_acceptable_tokens

  let progress ?(max_step = 10) lexer_buffer env checkpoint : int =
    let rec loop nth_step lexer_buffer env checkpoint =
      if nth_step >= max_step then nth_step
      else
        match checkpoint with
        | I.InputNeeded env ->
          let new_lexer_buffer, token = next lexer_buffer in
          let checkpoint = I.offer checkpoint token in
          loop (succ nth_step) new_lexer_buffer env checkpoint
        | I.Shifting _ | I.AboutToReduce _ ->
          let checkpoint = I.resume checkpoint in
          loop nth_step lexer_buffer env checkpoint
        | I.HandlingError (_ : _ I.env) | I.Accepted _ | I.Rejected -> nth_step
    in
    loop 0 lexer_buffer env checkpoint

  let recover_parsing_error lexer_buffer env acceptable_tokens =
    let candidates_checkpoints =
      let without_token = I.input_needed env in
      let make_with_token tok =
        let l, r = I.positions env in
        let checkpoint = I.input_needed env in
        I.offer checkpoint (tok, l, r)
      in
      without_token :: List.map make_with_token acceptable_tokens
    in
    let threshold = min 10 lexer_buffer.max_size in
    let rec iterate ((curr_max_progress, _) as acc) = function
      | [] -> acc
      | cp :: t ->
        if curr_max_progress >= 10 then acc
        else
          let cp_progress = progress ~max_step:threshold lexer_buffer env cp in
          if cp_progress > curr_max_progress then iterate (cp_progress, cp) t
          else iterate acc t
    in
    let best_progress, best_cp =
      let dummy_cp = I.input_needed env in
      iterate (-1, dummy_cp) candidates_checkpoints
    in
    (* We do not consider paths where progress isn't significant *)
    if best_progress < 2 then None else Some best_cp

  (** Main parsing loop *)
  let loop
      (lexer_buffer : t)
      (token_list : (string * Tokens.token) list)
      (lexbuf : lexbuf)
      (last_input_needed : 'semantic_value I.env option)
      (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
    let before_def = ref None in
    let rec loop
        (lexer_buffer : t)
        (token_list : (string * Tokens.token) list)
        (lexbuf : lexbuf)
        (last_input_needed : 'semantic_value I.env option)
        (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
      match checkpoint with
      | I.InputNeeded env ->
        let new_lexer_buffer, ((tok, p, p') as token) = next lexer_buffer in
        (* Format.eprintf "%s@." (Pos.to_string_shorter (Pos.from_lpos (p,
           p'))); *)
        (match tok with
        | Tokens.DEFINITION ->
          before_def := Some (new_lexer_buffer, checkpoint, last_input_needed)
        | _ -> ());
        let checkpoint = I.offer checkpoint token in
        loop new_lexer_buffer token_list lexbuf (Some env) checkpoint
      | I.AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        loop lexer_buffer token_list lexbuf last_input_needed checkpoint
      | I.Shifting _ ->
        let checkpoint = I.resume checkpoint in
        loop lexer_buffer token_list lexbuf last_input_needed checkpoint
      | I.HandlingError (env : 'semantic_value I.env) -> (
        match !before_def with
        | Some (prev_buff, prev_cp, prev_last_input_needed) -> (
          let p = function
            | Tokens.DEFINITION | Tokens.END_CODE _ -> true
            | _ -> false
          in
          match skip_just_until p prev_buff with
          | None ->
            ignore (Lexer_common.flush_acc ());
            Lexer_common.context := Law;
            []
          | Some skipped_buf ->
            before_def := None;
            loop skipped_buf token_list lexbuf prev_last_input_needed prev_cp)
        | None -> (
          let similar_candidate_tokens, sorted_acceptable_tokens =
            sorted_candidate_tokens lexbuf token_list env
          in
          register_parsing_error lexbuf env sorted_acceptable_tokens
            similar_candidate_tokens;
          let best_effort_checkpoint =
            recover_parsing_error lexer_buffer env
              (List.map snd sorted_acceptable_tokens)
          in
          match best_effort_checkpoint with
          | None ->
            (* No reasonable solution, aborting *)
            (* Let's reset the lexer buffer in order to not trigger the unclosed
             block finalizer: we have at least one error to report *)
            ignore (Lexer_common.flush_acc ());
            Lexer_common.context := Law;
            []
          | Some best_effort_checkpoint ->
            loop lexer_buffer token_list lexbuf last_input_needed
              best_effort_checkpoint))
      | I.Accepted v -> v
      | I.Rejected -> []
    in
    loop lexer_buffer token_list lexbuf last_input_needed checkpoint

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
      difference for [lexbuf]. *)
  let sedlex_with_menhir
      (lexer' : lexbuf -> Tokens.token)
      (token_list : (string * Tokens.token) list)
      (target_rule : Lexing.position -> 'semantic_value I.checkpoint)
      (lexbuf : lexbuf) : Ast.source_file =
    let lexer_buffer : t =
      let feed =
        let f = with_tokenizer lexer' lexbuf in
        fun () ->
          let ((tok, _, _) as x) :
              Tokens.token * Lexing.position * Lexing.position =
            f ()
          in
          (* Format.eprintf "%s@\n" (Parser_utils.token_to_string tok); *)
          x
      in
      create feed
    in
    try
      let target_rule =
        target_rule (fst @@ Sedlexing.lexing_positions lexbuf)
      in
      loop lexer_buffer token_list lexbuf None target_rule
    with Lexer_common.Lexing_error (pos, token) ->
      (* The encapsulating [Message.with_delayed_errors] will raise an
         exception: we are safe returning a dummy value. *)
      Message.delayed_error ~kind:Lexing [] ~pos
        "Parsing error after token @{<yellow>%S@}: what comes after could not \
         be recognised"
        token

  let commands_or_includes (lexbuf : lexbuf) : Ast.source_file =
    Lexer_common.with_lexing_context
      (fst (Sedlexing.lexing_positions lexbuf)).pos_fname
    @@ fun () ->
    sedlex_with_menhir LocalisedLexer.lexer LocalisedLexer.token_list
      Incremental.source_file lexbuf
end

module Parser_En = ParserAux (Lexer_en)
module Parser_Fr = ParserAux (Lexer_fr)
module Parser_Pl = ParserAux (Lexer_pl)

let parse (lang : Global.backend_lang) (lexbuf : Sedlexing.lexbuf) =
  let r =
    match lang with
    | En -> Parser_En.commands_or_includes lexbuf
    | Fr -> Parser_Fr.commands_or_includes lexbuf
    | Pl -> Parser_Pl.commands_or_includes lexbuf
  in
  Ok r
