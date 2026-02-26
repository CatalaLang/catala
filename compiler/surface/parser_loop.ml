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

[@@@ocaml.warning "-27-32-33-69"]
(* TODO: remove warnings suppression *)

open Catala_utils
open Parser_utils
open Sedlexing
open Ast
open Tokens

type pattern =
  | One of (token -> bool)
  | Two of (token * token -> bool)
  | Three of (token * token * token -> bool)
  | Four of (token * token * token * token -> bool)

type scope = {
  id : string;
  start_pat : pattern;
  stop_token : (token -> bool) option;
  recoverable : bool;
}

let source_scope =
  {
    id = " source";
    start_pat = One (fun _ -> false);
    stop_token = Some (function EOF -> true | _ -> false);
    recoverable = false;
  }

let code_block_scope =
  {
    id = "code_block";
    start_pat =
      One (function BEGIN_CODE | BEGIN_METADATA -> true | _ -> false);
    stop_token = Some (function END_CODE _ -> true | _ -> false);
    recoverable = false;
  }

let scope_decl_scope =
  {
    id = "scope_decl";
    start_pat = Two (function DECLARATION, SCOPE -> true | _ -> false);
    stop_token = None;
    recoverable = true;
  }

let struct_decl_scope =
  {
    id = "struct_decl";
    start_pat =
      Four
        (function DECLARATION, STRUCT, UIDENT _, COLON -> true | _ -> false);
    stop_token = None;
    recoverable = false;
    (* recoverable = true; *)
  }

let enum_decl_scope =
  {
    id = "enum_decl";
    start_pat =
      Four (function DECLARATION, ENUM, UIDENT _, COLON -> true | _ -> false);
    stop_token = None;
    recoverable = false;
    (* recoverable = true; *)
  }

let topdef_scope =
  {
    id = "topdef";
    start_pat = Two (function DECLARATION, LIDENT _ -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let scope_def_scope =
  {
    id = "scope_def";
    start_pat =
      Three
        (function
        | SCOPE, UIDENT _, UNDER_CONDITION | SCOPE, UIDENT _, COLON -> true
        | _ -> false);
    stop_token = None;
    recoverable = true;
  }

let scope_decl_item_scope =
  {
    id = "scope_decl_item";
    start_pat =
      One (function INPUT | CONTEXT | INTERNAL | OUTPUT -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let subscope_decl_item_scope =
  {
    id = "subscope_decl_item";
    start_pat =
      Three
        (function
        (* with & w/o output attribute versions *)
        | OUTPUT, LIDENT _, SCOPE | LIDENT _, SCOPE, UIDENT _ -> true
        | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let label_var_def_scope =
  {
    id = "label_var_def";
    start_pat = Two (function LABEL, LIDENT _ -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let except_var_def_scope =
  {
    id = "except_var_def";
    start_pat = One (function EXCEPTION -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let var_def_scope =
  {
    id = "var_def";
    start_pat = One (function DEFINITION -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let rule_def_scope =
  {
    id = "rule_def";
    start_pat = One (function RULE -> true | _ -> false);
    stop_token = Some (function FILLED -> true | _ -> false);
    recoverable = false;
  }

let assert_scope =
  {
    id = "assert";
    start_pat = One (function ASSERTION -> true | _ -> false);
    stop_token = None;
    recoverable = false;
  }

let let_in_scope =
  {
    id = "let_in";
    start_pat = One (function LET -> true | _ -> false);
    stop_token = Some (function IN -> true | _ -> false);
    recoverable = false;
  }

let assert_in_scope =
  {
    id = "assert_in";
    start_pat = One (function ASSERTION -> true | _ -> false);
    stop_token = Some (function IN -> true | _ -> false);
    recoverable = false;
  }

module ScopeT = struct
  type t = scope

  let compare a b = String.compare a.id b.id
  let format ppf a = Format.pp_print_string ppf a.id
end

module ScopeMap = Map.Make (ScopeT)
module ScopeSet = Set.Make (ScopeT)

let scope_map =
  let scopes_transitions =
    [
      source_scope, [code_block_scope];
      ( code_block_scope,
        [
          scope_decl_scope;
          struct_decl_scope;
          enum_decl_scope;
          topdef_scope;
          scope_def_scope;
        ] );
      scope_decl_scope, [scope_decl_item_scope; subscope_decl_item_scope];
      ( scope_def_scope,
        [
          label_var_def_scope;
          except_var_def_scope;
          var_def_scope;
          rule_def_scope;
          assert_scope;
        ] );
      label_var_def_scope, [except_var_def_scope; var_def_scope; rule_def_scope];
      except_var_def_scope, [var_def_scope; rule_def_scope];
      rule_def_scope, [let_in_scope; assert_in_scope];
      var_def_scope, [let_in_scope; assert_in_scope];
      topdef_scope, [let_in_scope; assert_in_scope];
      let_in_scope, [let_in_scope; assert_in_scope];
    ]
  in
  let map = ScopeMap.of_list scopes_transitions in
  (* Add implicit empty transitions to non-toplevel subscopes *)
  List.fold_left
    (fun m (_, rl) ->
      List.fold_left
        (fun m s -> if ScopeMap.mem s m then m else ScopeMap.add s [] m)
        m rl)
    map scopes_transitions

type parsing_error

type parsing_result =
  | Ok of Ast.source_file
  | Partial of Ast.source_file * parsing_error list

type token_state = {
  offset : int;
  tokens : ((Tokens.token * Lexing.position * Lexing.position) * string) array;
}

module ParserAux (LocalisedLexer : Lexer_common.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)
  module I = MenhirInterpreter

  type 'a stack_scope_elt = {
    last_valid_cp : 'a I.checkpoint;
    last_valid_offset : int;
    scope : scope;
  }

  type 'a parsing_state = {
    scope_stack : 'a stack_scope_elt list;
    tokens_to_consume : int;
    token_state : token_state;
  }

  let format_parsing_state fmt { scope_stack; token_state; tokens_to_consume } =
    let open Format in
    Format.fprintf fmt
      "Last valid token offset: %d => '%s'@\nTokens to consume: %d@\n"
      (List.hd scope_stack).last_valid_offset
      (snd token_state.tokens.((List.hd scope_stack).last_valid_offset))
      tokens_to_consume;
    let rec loop = function
      | [] -> ()
      | { scope; _ } :: t ->
        Format.pp_open_vbox fmt 2;
        Format.fprintf fmt "In scope: %s@\n" scope.id;
        loop t;
        pp_close_box fmt ()
    in
    loop (List.rev scope_stack);
    Format.fprintf fmt "@\n"

  let init_parsing_state lexer lexbuf init_cp =
    let f = with_tokenizer lexer lexbuf in
    let rec loop acc =
      match f () with
      | (Tokens.EOF, _, _) as tok ->
        Array.of_list (List.rev ((tok, Utf8.lexeme lexbuf) :: acc))
      | tok -> loop ((tok, Utf8.lexeme lexbuf) :: acc)
    in
    let tokens = loop [] in
    Format.eprintf "all tokens:@\n@[<hov>%s@]@\n@."
      (String.concat " "
         (List.map
            (fun ((tok, _, _), _) -> token_to_string tok)
            (Array.to_list tokens)));
    let toplevel_scope_elt =
      { scope = source_scope; last_valid_offset = 0; last_valid_cp = init_cp }
    in
    let token_state = { offset = 0; tokens } in
    { scope_stack = [toplevel_scope_elt]; tokens_to_consume = 0; token_state }

  let current_token { token_state = { offset; tokens }; _ } = tokens.(offset)

  let check_pattern (pat : pattern) { offset; tokens } =
    let tok i =
      let (tok, _, _), _ = tokens.(offset + i) in
      tok
    in
    try
      match pat with
      | One f -> f (tok 0)
      | Two f -> f (tok 0, tok 1)
      | Three f -> f (tok 0, tok 1, tok 2)
      | Four f -> f (tok 0, tok 1, tok 2, tok 3)
    with Invalid_argument _ -> false

  let pat_len = function One f -> 1 | Two f -> 2 | Three f -> 3 | Four f -> 4

  let get_all_succs_scopes stack =
    let rec loop acc = function
      | (i, h) :: t ->
        let succs =
          ScopeMap.find h scope_map
          |> List.map (fun s -> s, i)
          |> ScopeMap.of_list
        in
        (* We keep track of the closest pattern that matches *)
        loop ScopeMap.(union (fun _ i j -> Some (min i j)) acc succs) t
      | [] -> ScopeMap.bindings acc
    in
    loop ScopeMap.empty (List.mapi (fun i x -> i, x.scope) stack)

  let get_all_stop_pat stack =
    let rec loop acc = function
      | (i, { stop_token = Some p; _ }) :: t -> loop ((i, p) :: acc) t
      | _ :: t -> loop acc t
      | [] -> acc
    in
    loop [] (List.mapi (fun i x -> i, x.scope) stack)

  let rec pop_stack stack i =
    match stack, i with
    | _, 0 -> stack
    | [], _ -> assert false
    | _ :: t, _ -> pop_stack t (pred i)

  let rec pop_stack_or_until_recoverable (stack : 'a stack_scope_elt list) i =
    match stack, i with
    | _, 0 -> stack
    | [], _ -> assert false
    | { scope = { recoverable = false; _ }; _ } :: t, _ ->
      pop_stack_or_until_recoverable t (pred i)
    | { scope = { recoverable = true; _ }; _ } :: t, _ -> stack

  let find_next_transition { scope_stack; token_state = ts; _ } =
    let valid_succs_scopes = get_all_succs_scopes scope_stack in
    match
      List.find_map
        (fun (scope, i) ->
          if check_pattern scope.start_pat ts then Some (i, scope) else None)
        valid_succs_scopes
    with
    | Some (i, scope) -> Some (`Succ (i, scope))
    | None -> (
      let all_stop_pat = get_all_stop_pat scope_stack in
      let (tok, _, _), _ = ts.tokens.(ts.offset) in
      match
        List.find_map
          (fun (i, p) -> if p tok then Some i else None)
          all_stop_pat
      with
      | None -> None
      | Some i -> Some (`Stop i))

  let next
      ({ scope_stack; tokens_to_consume; token_state = { offset; tokens }; _ }
       as ps)
      cp =
    let ((tok, l, r) as new_token), _lexem = tokens.(offset) in
    let pos = Pos.from_lpos (l, r) in
    let new_token_state = { offset = succ offset; tokens } in
    let new_ps = { ps with token_state = new_token_state } in
    if tokens_to_consume > 0 then
      { new_ps with tokens_to_consume = pred tokens_to_consume }, new_token
    else
      match scope_stack with
      | [] -> assert false
      | { scope = { id; stop_token = Some p; _ }; _ } :: r when p tok ->
        (* FIXME?: can be factorized *)
        (* If the current scope has an end pattern and it matches, then we pop
           it from the scope stack *)
        Format.eprintf "tok: %s:%s, exiting current scope %s@."
          (token_to_string tok)
          (Pos.to_string_shorter pos)
          id;
        ( {
            scope_stack = r;
            tokens_to_consume = 0;
            token_state = new_token_state;
          },
          new_token )
      | { scope; _ } :: _ -> (
        match find_next_transition ps with
        | None -> new_ps, new_token
        | Some (`Stop i) ->
          let new_stack = pop_stack scope_stack i in
          let new_parsing_state = { new_ps with scope_stack = new_stack } in
          Format.eprintf "tok: %s:%s, exiting scope %s & popping %d times@."
            (token_to_string tok)
            (Pos.to_string_shorter pos)
            scope.id i;
          new_parsing_state, new_token
        | Some (`Succ (i, scope)) ->
          let new_scope =
            { scope; last_valid_offset = pred offset; last_valid_cp = cp }
          in
          let new_stack : 'a stack_scope_elt list =
            match pop_stack scope_stack i with
            | [] -> [new_scope]
            | parent :: t ->
              (* also update cp of the new parent's top stack *)
              new_scope :: { parent with last_valid_cp = cp } :: t
          in
          Format.eprintf "tok: %s:%s, popping %d times, entering scope %s@."
            (token_to_string tok)
            (Pos.to_string_shorter pos)
            i scope.id;
          ( {
              new_ps with
              tokens_to_consume = pred (pat_len scope.start_pat);
              scope_stack = new_stack;
            },
            new_token ))

  (** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match I.top env with None -> 0 | Some (Element (s, _, _, _)) -> I.number s

  let register_parsing_error
      (env : 'semantic_value I.env)
      ((_tok, l, r), wrong_lexeme)
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
    let error_loc = Pos.from_lpos (l, r) in
    if String.trim wrong_lexeme = "```" then
      (* If the token is an ending code fence, override the message for an
         appropriate one. *)
      Message.delayed_error ~kind:Parsing () ?suggestion ~pos:error_loc
        "@[<hov>Syntax error in preceding code block@]"
    else
      Message.delayed_error ~kind:Parsing () ?suggestion ~pos:error_loc
        "@[<hov>Syntax error at %a:@ %t@]"
        (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
        wrong_lexeme custom_menhir_message

  let sorted_candidate_tokens ((_tok, l, r), lexeme) env =
    let acceptable_tokens =
      List.filter_map
        (fun ((_, t) as elt) ->
          if I.acceptable (I.input_needed env) t l then Some elt else None)
        LocalisedLexer.token_list
    in
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

  let progress ?(max_step = 10) parsing_state env checkpoint : int =
    let rec loop nth_step lexer_buffer env checkpoint =
      if nth_step >= max_step then nth_step
      else
        match checkpoint with
        | I.InputNeeded env ->
          let new_lexer_buffer, token = next lexer_buffer checkpoint in
          let checkpoint = I.offer checkpoint token in
          loop (succ nth_step) new_lexer_buffer env checkpoint
        | I.Shifting _ | I.AboutToReduce _ ->
          let checkpoint = I.resume checkpoint in
          loop nth_step lexer_buffer env checkpoint
        | I.HandlingError (_ : _ I.env) | I.Accepted _ | I.Rejected -> nth_step
    in
    loop 0 parsing_state env checkpoint

  let recover_parsing_error parsing_state env acceptable_tokens =
    let candidates_checkpoints =
      let without_token = I.input_needed env in
      let make_with_token tok =
        let l, r = I.positions env in
        let checkpoint = I.input_needed env in
        I.offer checkpoint (tok, l, r)
      in
      without_token :: List.map make_with_token acceptable_tokens
    in
    let threshold = min 10 (Array.length parsing_state.token_state.tokens) in
    let rec iterate ((curr_max_progress, _) as acc) = function
      | [] -> acc
      | cp :: t ->
        if curr_max_progress >= 10 then acc
        else
          let cp_progress = progress ~max_step:threshold parsing_state env cp in
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
  let rec loop
      (parsing_state : 'semantic_value parsing_state)
      (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
    match checkpoint with
    | I.InputNeeded env ->
      let new_parsing_state, ((tok, p, p') as token) =
        next parsing_state checkpoint
      in
      let checkpoint = I.offer checkpoint token in
      loop new_parsing_state checkpoint
    | I.AboutToReduce _ | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      loop parsing_state checkpoint
    | I.HandlingError (env : 'semantic_value I.env) ->
      Format.eprintf "Recovering@\n%a@." format_parsing_state parsing_state;
      let curr_token = current_token parsing_state in
      let similar_candidate_tokens, sorted_acceptable_tokens =
        sorted_candidate_tokens curr_token env
      in
      register_parsing_error env curr_token sorted_acceptable_tokens
        similar_candidate_tokens;
      recover parsing_state checkpoint
      (* ignore (Lexer_common.flush_acc ()); *)
      (* Lexer_common.context := Law; *)
      (* [] *)
    | I.Accepted v -> v
    | I.Rejected -> []

  and recover parsing_state checkpoint =
    (* We should only match valid grammar, hence there shouldn't be any leftover
       tokens. *)
    assert (parsing_state.tokens_to_consume = 0);
    let step (ps : 'a parsing_state) :
        'a parsing_state * (token * Lexing.position * Lexing.position) =
      (* Advance the lexer by one *)
      let ts = ps.token_state in
      let new_token_state = { ts with offset = succ ts.offset } in
      let tok, _lexem = ts.tokens.(succ ts.offset) in
      Format.eprintf "advancing recovery parser to %s@." _lexem;
      { ps with token_state = new_token_state }, tok
    in
    (* lookahead and take the first possible transition *)
    let rec recovery_loop (ps, (tok, _, _)) =
      if tok = EOF then
        (* Pop everything until source level, it should accept the EOF Note:
           this should already be handled by the general case, below. *)
        let new_stack =
          pop_stack ps.scope_stack (List.length ps.scope_stack - 1)
        in
        ( { parsing_state with scope_stack = new_stack },
          pred ps.token_state.offset )
      else
        find_next_transition ps
        |> function
        | None ->
          Format.eprintf "No transition with %s@." (token_to_string tok);
          (* No possible continuation, continue... *)
          recovery_loop (step ps)
        (* We found a valid successor, let's pop the stack, move past the faulty
           tokens and continue *)
        | Some (`Succ (i, _s)) ->
          Format.eprintf
            "Successor transition with %s => %s (popping %d times)@."
            (token_to_string tok) _s.id i;
          let new_stack = pop_stack_or_until_recoverable ps.scope_stack i in
          { parsing_state with scope_stack = new_stack }, ps.token_state.offset
        | Some (`Stop i) ->
          Format.eprintf "Stop transition with %s (popping %d times)@."
            (token_to_string tok) i;
          (* let new_stack = pop_stack ps.scope_stack i in *)
          let new_stack = pop_stack ps.scope_stack 1 in
          ( { parsing_state with scope_stack = new_stack },
            ps.token_state.offset + 1 )
    in
    let recovered_state, next_non_invalid_offset =
      recovery_loop (step parsing_state)
    in
    match recovered_state.scope_stack with
    | [] -> assert false
    | ({ last_valid_cp; last_valid_offset; _ } as s) :: t ->
      let new_stack =
        { s with last_valid_offset = next_non_invalid_offset } :: t
      in
      let recovered_state =
        {
          recovered_state with
          scope_stack = new_stack;
          token_state =
            { parsing_state.token_state with offset = next_non_invalid_offset };
        }
      in
      (match last_valid_cp with
      | I.InputNeeded env -> (
        match I.top env with
        | Some (Element (_state, _, l, r)) ->
          let p = Pos.from_lpos (l, r) in
          Format.eprintf "last valid cp: %a@." Pos.format_loc_text p
        | None -> assert false)
      | _ -> assert false);
      loop recovered_state last_valid_cp

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
      difference for [lexbuf]. *)
  let sedlex_with_menhir
      (lexer : lexbuf -> Tokens.token)
      (target_rule : Lexing.position -> 'semantic_value I.checkpoint)
      (lexbuf : lexbuf) : Ast.source_file =
    try
      let init_cp = target_rule (fst @@ Sedlexing.lexing_positions lexbuf) in
      let parsing_state = init_parsing_state lexer lexbuf init_cp in
      loop parsing_state init_cp
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
    sedlex_with_menhir LocalisedLexer.lexer Incremental.source_file lexbuf
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
  Format.eprintf "@[<v 2>result:@ %a@]@." Print.format_source_file r;
  Ok r
