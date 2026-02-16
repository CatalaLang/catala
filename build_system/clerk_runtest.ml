(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2022-2023 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

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
open Shared_ast

type output_buf = { oc : out_channel; mutable pos : Lexing.position }

let pos0 pos_fname =
  { Lexing.pos_fname; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 }

let with_output file_opt f =
  match file_opt with
  | Some file ->
    File.with_out_channel ~bin:false file @@ fun oc -> f { oc; pos = pos0 file }
  | None -> f { oc = stdout; pos = pos0 "<stdout>" }

let out_line output_buf str =
  let len = String.length str in
  let has_nl = str <> "" && str.[len - 1] = '\n' in
  output_string output_buf.oc str;
  if not has_nl then output_char output_buf.oc '\n';
  let pos_cnum = output_buf.pos.pos_cnum + len + if has_nl then 0 else 1 in
  output_buf.pos <-
    {
      output_buf.pos with
      Lexing.pos_cnum;
      pos_lnum = output_buf.pos.pos_lnum + 1;
      pos_bol = pos_cnum;
    }

let sanitize =
  let re_endtest = Re.(compile @@ seq [bol; str "```"]) in
  let re_modhash =
    Re.(
      compile
      @@ seq
           [
             str "\"CM0|";
             repn xdigit 8 (Some 8);
             char '|';
             repn xdigit 8 (Some 8);
             char '|';
             repn xdigit 8 (Some 8);
             char '"';
           ])
  in
  let re_endline = Re.(compile @@ seq [rep (set "\r\n")]) in
  let re_backslash = Re.(compile (repn (char '\\') 1 (Some 2))) in
  fun str ->
    str
    |> Re.replace_string re_endtest ~by:"\\```"
    |> Re.replace_string re_modhash ~by:"\"CMX|XXXXXXXX|XXXXXXXX|XXXXXXXX\""
    |> Re.replace_string re_backslash ~by:"/"
    |> Re.replace_string re_endline ~by:""
    |> fun s -> s ^ "\n"

let catala_test_command test_flags catala_exe catala_opts args out =
  let catala_exe =
    (* If the exe name contains directories, make it absolute. Otherwise don't
       modify it so that it can be looked up in PATH. *)
    if String.contains catala_exe Filename.dir_sep.[0] then
      Unix.realpath catala_exe
    else catala_exe
  in
  match args with
  | "latex" :: flags ->
    Some
      (Array.of_list
         ((catala_exe :: "latex" :: flags)
         @ List.filter
             (fun s -> not (String.starts_with ~prefix:"--stdlib=" s))
             catala_opts))
  | cmd0 :: flags -> (
    try
      let cmd0, flags =
        match String.lowercase_ascii cmd0, flags, test_flags with
        | "test-scope", scope_name :: flags, test_flags ->
          "interpret", flags @ test_flags @ ["--scope=" ^ scope_name]
        | "test-scope", [], _ ->
          out_line out
            "[INVALID TEST] Invalid test command syntax, the 'test-scope' \
             pseudo-command takes a scope name as first argument\n";
          "interpret", test_flags
        | cmd0, flags, [] -> cmd0, flags
        | _, _, _ :: _ ->
          raise Exit (* Skip other tests when test-flags is specified *)
      in
      Some (Array.of_list ((catala_exe :: cmd0 :: catala_opts) @ flags))
    with Exit -> None)
  | [] -> Some (Array.of_list (catala_exe :: catala_opts))

let catala_test_env () =
  Unix.environment ()
  |> Array.to_seq
  |> Seq.filter (fun s ->
      not
        (String.starts_with ~prefix:"OCAMLRUNPARAM=" s
        || String.starts_with ~prefix:"CATALA_" s))
  |> Seq.cons "CATALA_OUT=-"
  |> Seq.cons "CATALA_COLOR=never"
  |> Seq.cons "CATALA_PLUGINS="
  |> Array.of_seq

let run_catala_test filename cmd program expected out_line =
  let cmd_in_rd, cmd_in_wr = Unix.pipe ~cloexec:true () in
  let cmd_out_rd, cmd_out_wr = Unix.pipe ~cloexec:true () in
  let command_oc = Unix.out_channel_of_descr cmd_in_wr in
  let command_ic = Unix.in_channel_of_descr cmd_out_rd in
  let env = catala_test_env () in
  let cmd = Array.append cmd [| "--name=" ^ filename; "-" |] in
  let pid =
    Unix.create_process_env cmd.(0) cmd env cmd_in_rd cmd_out_wr cmd_out_wr
  in
  Unix.close cmd_in_rd;
  Unix.close cmd_out_wr;
  Seq.iter (output_string command_oc) program;
  close_out command_oc;
  let out_lines =
    Seq.of_dispenser (fun () -> In_channel.input_line command_ic)
  in
  let success, expected =
    Seq.fold_left
      (fun (success, expected) result_line ->
        let result_line = sanitize result_line in
        out_line result_line;
        match Seq.uncons expected with
        | Some (l, expected) -> success && String.equal result_line l, expected
        | None -> false, Seq.empty)
      (true, expected) out_lines
  in
  let return_code =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED n -> n
    | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
  in
  let success, expected =
    if return_code = 0 then success, expected
    else
      let line = Printf.sprintf "#return code %d#\n" return_code in
      out_line line;
      match Seq.uncons expected with
      | Some (l, expected) when String.equal l line -> success, expected
      | Some (_, expected) -> false, expected
      | None -> false, Seq.empty
  in
  success && Seq.is_empty expected

(* Note: the line number is enough for now, we ignore the rest (recomputing the
   cnum would require reading the file) *)
let get_pos pos_fname pos_lnum col =
  let pos_bol = -1 in
  { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = pos_bol + col }

let run_catala_test_scopes
    ~code_coverage
    test_flags
    catala_exe
    catala_opts
    filename =
  let cmd_out_rd, cmd_out_wr = Unix.pipe ~cloexec:true () in
  let command_ic = Unix.in_channel_of_descr cmd_out_rd in
  let env = catala_test_env () in
  let whole_program_opt = "--whole-program" in
  let code_coverage_opt = "--code-coverage" in
  let catala_opts =
    catala_opts
    @
    if code_coverage then
      (if List.mem code_coverage_opt catala_opts then []
       else [code_coverage_opt])
      @
      if List.mem whole_program_opt catala_opts then [] else [whole_program_opt]
    else []
  in
  let test_flags =
    if code_coverage then List.filter (( <> ) whole_program_opt) test_flags
    else test_flags
  in
  let cmd =
    Array.of_list
      (catala_exe
       :: "interpret"
       :: "--quiet"
       :: "--message-format=gnu"
       :: filename
       :: catala_opts
      @ test_flags)
  in
  let start_time = Sys.time () in
  let current_time = ref start_time in
  let pid =
    Unix.create_process_env catala_exe cmd env Unix.stdin cmd_out_wr cmd_out_wr
  in
  Unix.close cmd_out_wr;
  let out_lines =
    Seq.of_dispenser (fun () -> In_channel.input_line command_ic)
  in
  let parse_error line =
    let re_error =
      let open Re in
      compile
      @@ whole_string
      @@ seq
           [
             bos;
             group ~name:"file" @@ rep1 (diff any (char ':'));
             char ':';
             group ~name:"line0" @@ rep1 digit;
             char '.';
             group ~name:"col0" @@ rep1 digit;
             char '-';
             group ~name:"line1" @@ rep1 digit;
             char '.';
             group ~name:"col1" @@ rep1 digit;
             str ": [ERROR]";
             rep (alt [set " :/"; digit]);
             group ~name:"message" @@ rep1 any;
           ]
    in
    match Re.exec_opt re_error line with
    | Some g ->
      let gets label =
        Re.Group.get g (List.assoc label (Re.group_names re_error))
      in
      let file = gets "file" in
      let pos = get_pos file in
      let geti label = int_of_string (gets label) in
      Some
        ( (pos (geti "line0") (geti "col0"), pos (geti "line1") (geti "col1")),
          gets "message" )
    | None -> None
  in
  let re_line =
    let open Re in
    compile
    @@ whole_string
    @@ seq
         [
           group (rep1 (diff any (char ':')));
           str ": ";
           group (alt [str "passed"; str "failed"]);
           opt (seq [char '|'; group (rep notnl)]);
         ]
  in
  let errs, scopes_results =
    Seq.fold_left
      (fun (errs, acc) line ->
        match Re.exec_opt re_line line with
        | Some g ->
          let scope = Re.Group.get g 1 in
          let result =
            match Re.Group.get g 2 with
            | "passed" -> true
            | "failed" -> false
            | _ -> assert false
          in
          let line_time = Sys.time () in
          let delta = line_time -. !current_time in
          current_time := line_time;
          ( [],
            {
              Clerk_report.s_name = scope;
              s_success = result;
              s_command_line =
                (catala_exe :: "interpret" :: filename :: catala_opts)
                @ test_flags
                @ ["--scope=" ^ scope];
              s_errors = List.rev errs;
              s_time = delta;
              s_coverage =
                (if code_coverage && result then
                   let hex_coverage_string = Re.Group.get g 3 in
                   Some (Coverage.of_hex hex_coverage_string)
                 else None);
            }
            :: acc )
        | None -> (
          match parse_error line with
          | Some (pos, err) -> (pos, err) :: errs, acc
          | None ->
            Message.debug
              "Ignored unrecognised output line from 'catala interpret':@ %S"
              line;
            errs, acc))
      ([], []) out_lines
  in
  let return_code =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED n -> n
    | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
  in
  let scopes_results =
    (* Add a fake result in case some scopes failed to run *)
    if
      return_code <> 0
      && List.filter (fun t -> not t.Clerk_report.s_success) scopes_results = []
    then
      {
        Clerk_report.s_name = "compilation";
        s_success = false;
        s_command_line =
          (catala_exe :: "interpret" :: filename :: catala_opts) @ test_flags;
        s_errors = errs;
        s_time = Sys.time () -. start_time;
        s_coverage = None;
      }
      :: scopes_results
    else scopes_results
  in
  List.rev scopes_results

(** Directly runs the test (not using ninja, this will be called by ninja rules
    through the "clerk runtest" command) *)
let run_tests
    ~catala_exe
    ~catala_opts
    ~code_coverage
    ~test_flags
    ~report
    ~out
    filename =
  let module L = Surface.Lexer_common in
  let lang =
    match Clerk_scan.get_lang filename with
    | Some l -> l
    | None ->
      Message.error "Can't infer catala dialect from file extension of %a"
        File.format filename
  in
  let lines = Surface.Parser_driver.lines filename lang in
  with_output out
  @@ fun out ->
  let lines_until_now = Queue.create () in
  let push_line str =
    out_line out str;
    Queue.add str lines_until_now
  in
  let rtests : Clerk_report.inline_test list ref = ref [] in
  let rec skip_block lines =
    match Seq.uncons lines with
    | Some ((l, tok, _), lines) ->
      push_line l;
      if tok = L.LINE_BLOCK_END then lines else skip_block lines
    | None -> Seq.empty
  in
  let rec get_block acc lines =
    let return lines acc =
      let endpos =
        match acc with
        | (_, _, (_, epos)) :: _ -> epos
        | [] -> { Lexing.dummy_pos with pos_fname = filename }
      in
      let block = List.rev acc in
      let startpos =
        match block with
        | (_, _, (spos, _)) :: _ -> spos
        | [] -> { Lexing.dummy_pos with pos_fname = filename }
      in
      lines, block, (startpos, endpos)
    in
    match Seq.uncons lines with
    | None -> return Seq.empty acc
    | Some ((_, L.LINE_BLOCK_END, _), lines) -> return lines acc
    | Some (li, lines) -> get_block (li :: acc) lines
  in
  let broken_test msg =
    let opos_start = out.pos in
    push_line msg;
    {
      Clerk_report.i_success = false;
      i_command_line = [];
      i_expected =
        ( { Lexing.dummy_pos with pos_fname = filename },
          { Lexing.dummy_pos with pos_fname = filename } );
      i_result = opos_start, out.pos;
    }
  in
  let get_test_command lines =
    match Seq.uncons lines with
    | None ->
      let t =
        broken_test
          "[INVALID TEST] Missing test command, use '$ catala <args>'\n"
      in
      rtests := t :: !rtests;
      None, Seq.empty
    | Some ((str, L.LINE_BLOCK_END, _), lines) ->
      let t =
        broken_test
          "[INVALID TEST] Missing test command, use '$ catala <args>'\n"
      in
      rtests := t :: !rtests;
      push_line str;
      None, lines
    | Some ((str, _, _), lines) -> (
      push_line str;
      match Clerk_scan.test_command_args str with
      | None ->
        let t =
          broken_test
            "[INVALID TEST] Invalid test command syntax, must match '$ catala \
             <args>'\n"
        in
        let lines, _, ipos = get_block [] lines in
        push_line "```\n";
        rtests := { t with Clerk_report.i_expected = ipos } :: !rtests;
        None, lines
      | Some args -> (
        let args = String.split_on_char ' ' args in
        let program =
          let rec drop_last seq () =
            match seq () with
            | Seq.Nil -> assert false
            | Seq.Cons (x, next) -> (
              match next () with
              | Seq.Nil -> Seq.Nil
              | Seq.Cons _ as s -> Seq.Cons (x, drop_last (fun () -> s)))
          in
          Queue.to_seq lines_until_now |> drop_last |> drop_last
        in
        let opos_start = out.pos in
        match
          catala_test_command test_flags catala_exe catala_opts args out
        with
        | Some cmd -> Some (cmd, program, opos_start), lines
        | None -> None, skip_block lines))
  in
  let run_inline_test lines =
    match get_test_command lines with
    | None, lines -> lines
    | Some (cmd, program, opos_start), lines ->
      let lines, expected, ipos = get_block [] lines in
      let expected = Seq.map (fun (s, _, _) -> s) (List.to_seq expected) in
      let i_success = run_catala_test filename cmd program expected push_line in
      let opos_end = out.pos in
      push_line "```\n";
      rtests :=
        {
          Clerk_report.i_success;
          i_command_line = Array.to_list cmd @ [filename];
          i_result = opos_start, opos_end;
          i_expected = ipos;
        }
        :: !rtests;
      lines
  in
  let rec process ~has_test_scopes ~includes lines =
    match Seq.uncons lines with
    | Some ((str, L.LINE_INLINE_TEST, _), lines) ->
      push_line str;
      let lines = run_inline_test lines in
      process ~has_test_scopes ~includes lines
    | Some ((str, L.LINE_TEST_ATTRIBUTE, _), lines) ->
      push_line str;
      process ~has_test_scopes:true ~includes lines
    | Some ((str, L.LINE_INCLUDE f, _), lines) ->
      push_line str;
      let f = if Filename.is_relative f then File.(filename /../ f) else f in
      process ~has_test_scopes ~includes:(f :: includes) lines
    | Some ((str, _, _), lines) ->
      push_line str;
      process ~has_test_scopes ~includes lines
    | None -> has_test_scopes, includes
  in
  let has_test_scopes, includes =
    process ~has_test_scopes:false ~includes:[] lines
  in
  let has_test_scopes =
    has_test_scopes || List.exists (Clerk_scan.find_test_scope ~lang) includes
  in
  let scopes_results =
    if has_test_scopes then
      run_catala_test_scopes ~code_coverage test_flags catala_exe catala_opts
        filename
    else []
  in
  let successful_test_scopes, failed_test_scopes, coverage =
    List.fold_left
      (fun (nsucc, nfail, code_coverage) t ->
        let x, y =
          if t.Clerk_report.s_success then nsucc + 1, nfail
          else nsucc, nfail + 1
        in
        let code_coverage =
          match code_coverage, t.s_coverage with
          | None, None -> None
          | None, Some cov | Some cov, None -> Some cov
          | Some cov, Some cov' -> Some (Coverage.union cov cov')
        in
        x, y, code_coverage)
      (0, 0, None) scopes_results
  in
  let num_test_scopes = successful_test_scopes + failed_test_scopes in
  let tests_report =
    let open Clerk_report in
    List.fold_left
      (fun tests t ->
        {
          tests with
          total = tests.total + 1;
          successful = (tests.successful + if t.i_success then 1 else 0);
          tests = t :: tests.tests;
        })
      {
        Clerk_report.name = filename;
        successful = successful_test_scopes;
        total = num_test_scopes;
        tests = [];
        scopes = scopes_results;
        coverage;
      }
      !rtests
  in
  match report with
  | Some file -> Clerk_report.write_to file tests_report
  | None -> ()
