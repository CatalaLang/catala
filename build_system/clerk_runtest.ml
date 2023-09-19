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

type test = {
  text_before : string;
      (** Verbatim of everything from the last test end or beginning of file up
          to the test output start *)
  params : string list;
      (** Catala command-line arguments for the test *)
      (* Also contains test_output and return_code, but they are not relevant
         for just running the test *)
}

type file_tests = {
  filename : string;
  tests : test list;
  text_after : string;  (** Verbatim of everything following the last test *)
}

let checkfile parents file =
  let file = try Unix.realpath file with Unix.Unix_error _ -> file in
  if List.mem file parents then
    Message.raise_error "@[<hv 2>Cyclic file inclusion:@ %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " %a@ " String.format "→")
         Format.pp_print_string)
      (List.rev (file :: parents));
  file :: parents, file

let with_in_channel_safe parents file f =
  try File.with_in_channel file f
  with Sys_error err ->
    Message.raise_error "Could not open file %S:@ %s@ %a" file err
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf f ->
           Format.fprintf ppf "included from %S" f))
      parents

(* Matches both test starts and includes; discriminate by checking [Group.get g
   1], which will be defined only for includes (and equal to the included
   file) *)
let test_scan_rex =
  let open Re in
  let inline_test_start_key = str "```catala-test-inline" in
  let include_regexp =
    (* TODO: we match on "Inclu*" which will work for now but may not scale to
       new languages. The reasonable alternative would be to run the appropriate
       lexer on all files, but it might not yet be worth the added complexity
       (?) *)
    seq
      [
        char '>';
        rep1 blank;
        str "Inclu";
        rep1 alpha;
        rep blank;
        char ':';
        rep blank;
        group (rep1 notnl);
      ]
  in
  compile
    (seq [bol; alt [inline_test_start_key; include_regexp]; rep blank; eol])

let rec has_inline_tests ?(parents = []) (file : string) : bool =
  let parents, file = checkfile parents file in
  let rec aux ic =
    match input_line ic with
    | exception End_of_file -> false
    | li -> (
      match Re.exec_opt test_scan_rex li with
      | None -> aux ic
      | Some gr -> (
        match Re.Group.get_opt gr 1 with
        | None -> true
        | Some incl ->
          let incl_file = File.(Filename.dirname file / incl) in
          aux ic
          ||
          (close_in ic;
           has_inline_tests ~parents incl_file)))
  in
  with_in_channel_safe parents file aux

let has_inline_tests file = has_inline_tests file (* hide optional parameter *)

let [@ocamlformat "disable"] rec scan_for_inline_tests
    ?(parents=[]) (file : string)
  : file_tests list =
  let parents, file = checkfile parents file in
  let read_file ic =
    (* Matches something of the form: {v
     ```catala-test-inline
     $ catala Interpret -s A
     ... output from catala ...
     #return code 10#
     ```
     v} *)
    let test_content_rex =
      Re.(compile @@
          seq
            [
              seq [char '$'; rep space; str "catala"; rep space; group (rep1 notnl);
                   char '\n'];
              group (non_greedy (rep any));
              seq [bol; str "```\n"];
            ])
    in
    let file_str = really_input_string ic (in_channel_length ic) in
    let rec scan incls acc pos_scan pos_block =
      try
        let scan_grp = Re.exec ~pos:pos_scan test_scan_rex file_str in
        let pos = Re.Group.stop scan_grp 0 in
        match Re.Group.get_opt scan_grp 1 with
        | Some incl ->
          let incl_file = File.(Filename.dirname file / incl) in
          scan (incl_file::incls) acc (Re.Group.stop scan_grp 0) pos_block
        | None ->
          let test_contents =
            try Re.exec ~pos test_content_rex file_str
            with Not_found ->
              let line =
                String.fold_left
                  (fun n -> function '\n' -> n + 1 | _ -> n)
                  1
                  (String.sub file_str 0 pos)
              in
              Message.raise_error "Bad inline-test format at %s line %d"
                file line
          in
          let params =
            List.filter (( <> ) "")
              (String.split_on_char ' ' (Re.Group.get test_contents 1))
          in
          let out_start = Re.Group.start test_contents 2 in
          let test =
            { text_before = String.sub file_str pos_block (out_start - pos_block);
              params }
          in
          let pos_next = Re.Group.stop test_contents 2 in
          scan incls (test :: acc) pos_next pos_next
      with Not_found -> (
          match acc with
          | [] -> List.rev incls, []
          | tests ->
            List.rev incls,
            [{
              filename = file;
              tests = List.rev tests;
              text_after =
                String.sub file_str pos_block
                  (String.length file_str - pos_block);
            }])
    in
    scan [] [] 0 0
  in
  let incls, tests = with_in_channel_safe parents file read_file in
  List.fold_left (fun tests incfile ->
      List.rev_append (scan_for_inline_tests ~parents incfile) tests)
    (List.rev tests) incls
  |> List.rev

(** Directly runs the test (not using ninja, this will be called by ninja rules
    through the "clerk runtest" command) *)
let run_inline_tests
    ~(reset : bool)
    (file : string)
    (catala_exe : string)
    (catala_opts : string list) =
  let _, file = checkfile [] file in
  match scan_for_inline_tests file with
  | [] -> Message.emit_warning "No inline tests found in %s" file
  | file_tests ->
    Message.emit_debug "@[<v 2>Running tests:@ %a@]"
      (Format.pp_print_list (fun ppf t ->
           Format.fprintf ppf "- @[<hov>%s:@ %d tests@]" t.filename
             (List.length t.tests)))
      file_tests;
    let run test oc =
      List.iter
        (fun test ->
          output_string oc test.text_before;
          let cmd_out_rd, cmd_out_wr = Unix.pipe () in
          let ic = Unix.in_channel_of_descr cmd_out_rd in
          (* let file_dir, file = Filename.dirname file, Filename.basename file
             in *)
          let catala_exe =
            (* If the exe name contains directories, make it absolute. Otherwise
               don't modify it so that it can be looked up in PATH. *)
            if String.contains catala_exe Filename.dir_sep.[0] then
              Unix.realpath catala_exe
            else catala_exe
          in
          let cmd =
            match test.params with
            | cmd0 :: flags ->
              Array.of_list
                ((catala_exe :: cmd0 :: catala_opts) @ flags @ [file])
            | [] -> Array.of_list ((catala_exe :: catala_opts) @ [file])
          in
          let env =
            Unix.environment ()
            |> Array.to_seq
            |> Seq.filter (fun s ->
                   not (String.starts_with ~prefix:"OCAMLRUNPARAM=" s))
            |> Seq.cons "CATALA_OUT=-"
            |> Seq.cons "CATALA_COLOR=never"
            |> Seq.cons "CATALA_PLUGINS="
            |> Array.of_seq
          in
          let pid =
            (* let cwd = Unix.getcwd () in
             * (\* Catala depends on the CWD when printing relative file locations
             *    in error messages. Here we are dealing with inline tests, and it
             *    would be inconvenient for the file to contain its own location
             *    relative to where the test was run from ; to avoid that, we
             *    ensure to always run the catala exec from the directory where the
             *    test file was found. *\)
             * Unix.chdir file_dir;
             * Fun.protect ~finally:(fun () -> Unix.chdir cwd)
             * @@ fun () -> *)
            Unix.create_process_env catala_exe cmd env Unix.stdin cmd_out_wr
              cmd_out_wr
          in
          Unix.close cmd_out_wr;
          let rec process_cmd_out () =
            let s = input_line ic in
            let s =
              Re.(replace_string (compile (str File.(file /../ ""))) ~by:"" s)
            in
            if s = "```" || String.starts_with ~prefix:"#return code" s then
              output_char oc '\\';
            let rec trail s i =
              if i < 1 then String.length s
              else if s.[i - 1] = ' ' then trail s (i - 1)
              else i
            in
            output_substring oc s 0 (trail s (String.length s));
            output_char oc '\n';
            process_cmd_out ()
          in
          let () = try process_cmd_out () with End_of_file -> close_in ic in
          let return_code =
            match Unix.waitpid [] pid with
            | _, Unix.WEXITED n -> n
            | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
          in
          if return_code <> 0 then
            Printf.fprintf oc "#return code %d#\n" return_code)
        test.tests;
      output_string oc test.text_after;
      flush oc
    in
    List.iter
      (fun test ->
        if test.filename <> file then ()
        else if reset then (
          let out = test.filename ^ ".out" in
          (try File.with_out_channel out (run test)
           with e ->
             Sys.remove out;
             raise e);
          Sys.rename out test.filename)
        else run test stdout)
      file_tests
