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

let run_catala_test test_flags catala_exe catala_opts file program args oc =
  let cmd_in_rd, cmd_in_wr = Unix.pipe () in
  Unix.set_close_on_exec cmd_in_wr;
  let command_oc = Unix.out_channel_of_descr cmd_in_wr in
  let catala_exe =
    (* If the exe name contains directories, make it absolute. Otherwise don't
       modify it so that it can be looked up in PATH. *)
    if String.contains catala_exe Filename.dir_sep.[0] then
      Unix.realpath catala_exe
    else catala_exe
  in
  let cmd =
    match args with
    | cmd0 :: flags ->
      let cmd0, flags =
        match String.lowercase_ascii cmd0, flags, test_flags with
        | "test-scope", scope_name :: flags, test_flags ->
          "interpret", (("--scope=" ^ scope_name) :: flags) @ test_flags
        | "test-scope", [], _ ->
          output_string oc
            "[INVALID TEST] Invalid test command syntax, the 'test-scope' \
             pseudo-command takes a scope name as first argument\n";
          "interpret", test_flags
        | cmd0, flags, [] -> cmd0, flags
        | _, _, _ :: _ ->
          raise Exit (* Skip other tests when test-flags is specified *)
      in
      Array.of_list
        ((catala_exe :: cmd0 :: catala_opts) @ flags @ ["--name=" ^ file; "-"])
    | [] -> Array.of_list ((catala_exe :: catala_opts) @ [file])
  in
  let env =
    Unix.environment ()
    |> Array.to_seq
    |> Seq.filter (fun s -> not (String.starts_with ~prefix:"OCAMLRUNPARAM=" s))
    |> Seq.cons "CATALA_OUT=-"
    (* |> Seq.cons "CATALA_COLOR=never" *)
    |> Seq.cons "CATALA_PLUGINS="
    |> Array.of_seq
  in
  flush oc;
  let ocfd = Unix.descr_of_out_channel oc in
  let pid = Unix.create_process_env catala_exe cmd env cmd_in_rd ocfd ocfd in
  Unix.close cmd_in_rd;
  Seq.iter (output_string command_oc) program;
  close_out command_oc;
  let return_code =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED n -> n
    | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
  in
  if return_code <> 0 then Printf.fprintf oc "#return code %d#\n" return_code

(** Directly runs the test (not using ninja, this will be called by ninja rules
    through the "clerk runtest" command) *)
let run_inline_tests catala_exe catala_opts test_flags filename =
  let module L = Surface.Lexer_common in
  let lang =
    match Clerk_scan.get_lang filename with
    | Some l -> l
    | None ->
      Message.raise_error "Can't infer catala dialect from file extension of %a"
        File.format filename
  in
  let lines = Surface.Parser_driver.lines filename lang in
  let oc = stdout in
  let lines_until_now = Queue.create () in
  let push str =
    output_string oc str;
    Queue.add str lines_until_now
  in
  let rec run_test lines =
    match Seq.uncons lines with
    | None ->
      output_string oc
        "[INVALID TEST] Missing test command, use '$ catala <args>'\n"
    | Some ((str, L.LINE_BLOCK_END), lines) ->
      output_string oc
        "[INVALID TEST] Missing test command, use '$ catala <args>'\n";
      push str;
      process lines
    | Some ((str, _), lines) -> (
      push str;
      match Clerk_scan.test_command_args str with
      | None ->
        output_string oc
          "[INVALID TEST] Invalid test command syntax, must match '$ catala \
           <args>'\n";
        skip_block lines
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
        match
          run_catala_test test_flags catala_exe catala_opts filename program
            args oc
        with
        | () -> skip_block lines
        | exception Exit -> process lines))
  and skip_block lines =
    match Seq.uncons lines with
    | None -> ()
    | Some ((str, L.LINE_BLOCK_END), lines) ->
      push str;
      process lines
    | Some ((str, _), lines) ->
      Queue.add str lines_until_now;
      skip_block lines
  and process lines =
    match Seq.uncons lines with
    | Some ((str, L.LINE_INLINE_TEST), lines) ->
      push str;
      run_test lines
    | Some ((str, _), lines) ->
      push str;
      process lines
    | None -> ()
  in
  process lines
