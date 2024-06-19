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

type output_buf = { oc : out_channel; mutable pos : Lexing.position }

let pos0 pos_fname =
  { Lexing.pos_fname; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 }

let with_output file_opt f =
  match file_opt with
  | Some file ->
    File.with_out_channel file @@ fun oc -> f { oc; pos = pos0 file }
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
  fun str ->
    str
    |> Re.replace_string re_endtest ~by:"\\```"
    |> Re.replace_string re_modhash ~by:"\"CMX|XXXXXXXX|XXXXXXXX|XXXXXXXX\""

let catala_test_command test_flags catala_exe catala_opts args out =
  let catala_exe =
    (* If the exe name contains directories, make it absolute. Otherwise don't
       modify it so that it can be looked up in PATH. *)
    if String.contains catala_exe Filename.dir_sep.[0] then
      Unix.realpath catala_exe
    else catala_exe
  in
  match args with
  | cmd0 :: flags -> (
    try
      let cmd0, flags =
        match String.lowercase_ascii cmd0, flags, test_flags with
        | "test-scope", scope_name :: flags, test_flags ->
          "interpret", (("--scope=" ^ scope_name) :: flags) @ test_flags
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
        let result_line = sanitize result_line ^ "\n" in
        out_line result_line;
        match Seq.uncons expected with
        | Some (l, expected) -> success && String.equal result_line l, expected
        | None -> false, expected)
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
      | None -> false, expected
  in
  success && Seq.is_empty expected

(** Directly runs the test (not using ninja, this will be called by ninja rules
    through the "clerk runtest" command) *)
let run_tests ~catala_exe ~catala_opts ~test_flags ~report ~out filename =
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
  let rtests : Clerk_report.test list ref = ref [] in
  let rec skip_block lines =
    match Seq.uncons lines with
    | Some ((l, tok, _), lines) ->
      push_line l;
      if tok = L.LINE_BLOCK_END then lines else skip_block lines
    | None -> lines
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
    | None -> return lines acc
    | Some ((_, L.LINE_BLOCK_END, _), lines) -> return lines acc
    | Some (li, lines) -> get_block (li :: acc) lines
  in
  let broken_test msg =
    let opos_start = out.pos in
    push_line msg;
    {
      Clerk_report.success = false;
      command_line = [];
      expected =
        ( { Lexing.dummy_pos with pos_fname = filename },
          { Lexing.dummy_pos with pos_fname = filename } );
      result = opos_start, out.pos;
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
      None, lines
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
        rtests := { t with Clerk_report.expected = ipos } :: !rtests;
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
  let rec run_inline_test lines =
    match get_test_command lines with
    | None, lines -> process lines
    | Some (cmd, program, opos_start), lines ->
      let lines, expected, ipos = get_block [] lines in
      let expected = Seq.map (fun (s, _, _) -> s) (List.to_seq expected) in
      let success = run_catala_test filename cmd program expected push_line in
      let opos_end = out.pos in
      push_line "```\n";
      rtests :=
        {
          Clerk_report.success;
          command_line = Array.to_list cmd @ [filename];
          result = opos_start, opos_end;
          expected = ipos;
        }
        :: !rtests;
      process lines
  and run_output_test id lines =
    match get_test_command lines with
    | None, lines -> process lines
    | Some (cmd, program, _), lines ->
      let lines = skip_block lines in
      let ref_file =
        File.((filename /../ "output" / Filename.basename filename) -.- id)
      in
      if not (Sys.file_exists ref_file) then
        (* Create the file if it doesn't exist *)
        File.with_out_channel ref_file ignore;
      let output = ref_file ^ "@out" in
      let ipos_start = pos0 ref_file in
      let ipos_end = ref ipos_start in
      let report =
        File.with_in_channel ref_file
        @@ fun ic ->
        let expected =
          Seq.of_dispenser (fun () ->
              match In_channel.input_line ic with
              | None -> None
              | Some s ->
                let s = s ^ "\n" in
                let pos_cnum = !ipos_end.pos_cnum + String.length s in
                ipos_end :=
                  {
                    !ipos_end with
                    Lexing.pos_cnum;
                    pos_lnum = !ipos_end.pos_lnum + 1;
                    pos_bol = pos_cnum;
                  };
                Some s)
        in
        with_output (Some output)
        @@ fun test_out ->
        let opos_start = test_out.pos in
        let success =
          run_catala_test filename cmd program expected (out_line test_out)
        in
        Seq.iter ignore expected;
        {
          Clerk_report.success;
          command_line = Array.to_list cmd @ [filename];
          result = opos_start, test_out.pos;
          expected = ipos_start, !ipos_end;
        }
      in
      rtests := report :: !rtests;
      process lines
  and process lines =
    match Seq.uncons lines with
    | Some ((str, L.LINE_INLINE_TEST, _), lines) ->
      push_line str;
      run_inline_test lines
    | Some ((str, L.LINE_TEST id, _), lines) ->
      push_line str;
      run_output_test id lines
    | Some ((str, _, _), lines) ->
      push_line str;
      process lines
    | None -> ()
  in
  process lines;
  let tests_report =
    List.fold_left
      Clerk_report.(
        fun tests t ->
          {
            tests with
            total = tests.total + 1;
            successful = (tests.successful + if t.success then 1 else 0);
            tests = t :: tests.tests;
          })
      { Clerk_report.name = filename; successful = 0; total = 0; tests = [] }
      !rtests
  in
  match report with
  | Some file -> Clerk_report.write_to file tests_report
  | None -> ()
