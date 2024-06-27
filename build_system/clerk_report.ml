(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2024 Inria,
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

(** This module defines and manipulates Clerk test reports, which can be written
    by `clerk runtest` and read to provide test result summaries. This only
    concerns inline tests (```catala-test-inline blocks). *)

open Catala_utils

type test = {
  success : bool;
  command_line : string list;
  expected : Lexing.position * Lexing.position;
  result : Lexing.position * Lexing.position;
}

type file = { name : File.t; successful : int; total : int; tests : test list }

type disp_flags = {
  mutable files : [ `All | `Failed | `None ];
  mutable tests : [ `All | `FailedFile | `Failed | `None ];
  mutable diffs : bool;
  mutable diff_command : string option option;
}

let disp_flags =
  { files = `Failed; tests = `FailedFile; diffs = true; diff_command = None }

let set_display_flags
    ?(files = disp_flags.files)
    ?(tests = disp_flags.tests)
    ?(diffs = disp_flags.diffs)
    ?(diff_command = disp_flags.diff_command)
    () =
  disp_flags.files <- files;
  disp_flags.tests <- tests;
  disp_flags.diffs <- diffs;
  disp_flags.diff_command <- diff_command

let write_to f file =
  File.with_out_channel f (fun oc -> Marshal.to_channel oc (file : file) [])

let read_from f = File.with_in_channel f Marshal.from_channel

let read_many f =
  File.with_in_channel f
  @@ fun ic ->
  let rec results () =
    match Marshal.from_channel ic with
    | file -> file :: results ()
    | exception End_of_file -> []
  in
  results ()

let has_command cmd =
  let check_cmd = Printf.sprintf "type %s >/dev/null 2>&1" cmd in
  Sys.command check_cmd = 0

let longuest_common_prefix_length s1 s2 =
  let len = min (String.length s1) (String.length s2) in
  let rec aux i =
    if i >= len then i
    else
      let c1 = String.get_utf_8_uchar s1 i in
      let c2 = String.get_utf_8_uchar s2 i in
      if c1 = c2 then aux (i + Uchar.utf_decode_length c1) else i
  in
  aux 0

let diff_command =
  let has_gnu_diff () =
    File.process_out ~check_exit:ignore "diff" ["--version"]
    |> Re.(execp (compile (str "GNU")))
  in
  lazy
    begin
      match disp_flags.diff_command with
      | None when Message.has_color stdout && has_gnu_diff () ->
        let width = Message.terminal_columns () - 5 in
        ( [
            "diff";
            "-y";
            "-t";
            "-W";
            string_of_int (Message.terminal_columns () - 5);
          ],
          fun ppf s ->
            let mid = (width - 1) / 2 in
            Format.fprintf ppf "@{<blue;ul>%*sReference%*s│%*sResult%*s@}@,"
              ((mid - 9) / 2)
              ""
              (mid - 9 - ((mid - 9) / 2))
              ""
              ((width - mid - 7) / 2)
              ""
              (width - mid - 7 - ((width - mid - 7) / 2))
              "";
            s
            |> String.trim_end
            |> String.split_on_char '\n'
            |> Format.pp_print_list
                 (fun ppf li ->
                   let rec find_cut col index =
                     if index >= String.length li then None
                     else if col = mid then Some index
                     else
                       let c = String.get_utf_8_uchar li index in
                       find_cut (col + 1) (index + Uchar.utf_decode_length c)
                   in
                   match find_cut 0 0 with
                   | None ->
                     if li = "" then Format.fprintf ppf "%*s@{<blue>│@}" mid ""
                     else Format.pp_print_string ppf li
                   | Some i -> (
                     let l, c, r =
                       ( String.sub li 0 i,
                         li.[i],
                         String.sub li (i + 1) (String.length li - i - 1) )
                     in
                     match c with
                     | ' ' -> Format.fprintf ppf "%s@{<blue>│@}%s" l r
                     | '>' ->
                       if String.for_all (( = ) ' ') l then
                         Format.fprintf ppf
                           "%*s@{<red>-@}@{<blue>│@}@{<red>%s@}" (mid - 1) "" r
                       else Format.fprintf ppf "%s@{<blue>│@}@{<red>%s@}" l r
                     | '<' -> Format.fprintf ppf "%s@{<blue>│@}@{<red>-@}" l
                     | '|' ->
                       let w = longuest_common_prefix_length (" " ^ l) r in
                       Format.fprintf ppf "%s@{<blue>│@}%s@{<red>%s@}" l
                         (String.sub r 0 w)
                         (String.sub r w (String.length r - w))
                     | _ -> Format.pp_print_string ppf li))
                 ppf )
      | Some cmd_opt | (None as cmd_opt) ->
        let command =
          match cmd_opt with
          | Some str -> String.split_on_char ' ' str
          | None ->
            if Message.has_color stdout && has_command "patdiff" then
              ["patdiff"; "-alt-old"; "Reference"; "-alt-new"; "Result"]
            else ["diff"; "-u"; "-L"; "Reference"; "-L"; "Result"]
        in
        ( command,
          fun ppf s ->
            s
            |> String.trim_end
            |> String.split_on_char '\n'
            |> Format.pp_print_list Format.pp_print_string ppf )
    end

let print_diff ppf p1 p2 =
  let get_str (pstart, pend) =
    assert (pstart.Lexing.pos_fname = pend.Lexing.pos_fname);
    File.with_in_channel pstart.Lexing.pos_fname
    @@ fun ic ->
    seek_in ic pstart.Lexing.pos_cnum;
    really_input_string ic (pend.Lexing.pos_cnum - pstart.Lexing.pos_cnum)
  in
  File.with_temp_file "clerk-diff" "a" ~contents:(get_str p1)
  @@ fun f1 ->
  File.with_temp_file "clerk_diff" "b" ~contents:(get_str p2)
  @@ fun f2 ->
  match Lazy.force diff_command with
  | [], _ -> assert false
  | cmd :: args, printer ->
    File.process_out ~check_exit:(fun _ -> ()) cmd (args @ [f1; f2])
    |> printer ppf

let catala_commands_with_output_flag =
  ["makefile"; "html"; "latex"; "ocaml"; "python"; "r"; "c"]

let pfile ~build_dir f =
  f
  |> String.remove_prefix ~prefix:(build_dir ^ Filename.dir_sep)
  |> String.remove_prefix ~prefix:(Sys.getcwd () ^ Filename.dir_sep)

let clean_command_line ~build_dir file cl =
  cl
  |> List.filter_map (fun s ->
         if s = "--directory=" ^ build_dir then None
         else Some (pfile ~build_dir s))
  |> (function
       | catala :: cmd :: args ->
         catala :: cmd :: "-I" :: Filename.dirname file :: args
       | cl -> cl)
  |> function
  | catala :: cmd :: args
    when List.mem (String.lowercase_ascii cmd) catala_commands_with_output_flag
    ->
    (catala :: cmd :: args) @ ["-o -"]
  | cl -> cl

let display ~build_dir file ppf t =
  let pp_pos ppf (start, stop) =
    assert (start.Lexing.pos_fname = stop.Lexing.pos_fname);
    Format.fprintf ppf "@{<cyan>%s:%d-%d@}"
      (pfile ~build_dir start.Lexing.pos_fname)
      start.Lexing.pos_lnum stop.Lexing.pos_lnum
  in
  let print_command () =
    Format.fprintf ppf "@,@[<h>$ @{<yellow>%a@}@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
      (clean_command_line ~build_dir file t.command_line)
  in
  Format.pp_open_vbox ppf 2;
  if t.success then (
    Format.fprintf ppf "@{<green>■@} %a passed" pp_pos t.expected;
    if Global.options.debug then print_command ())
  else (
    Format.fprintf ppf "@{<red>■@} %a failed" pp_pos t.expected;
    print_command ();
    if disp_flags.diffs then (
      Format.pp_print_cut ppf ();
      print_diff ppf t.expected t.result));
  Format.pp_close_box ppf ()

let display_file ~build_dir ppf t =
  let pfile f = String.remove_prefix ~prefix:(build_dir ^ Filename.dir_sep) f in
  let print_tests tests =
    let tests =
      match disp_flags.tests with
      | `All | `FailedFile -> tests
      | `Failed -> List.filter (fun t -> not t.success) tests
      | `None -> assert false
    in
    Format.pp_print_break ppf 0 3;
    Format.pp_open_vbox ppf 0;
    Format.pp_print_list (display ~build_dir t.name) ppf tests;
    Format.pp_close_box ppf ()
  in
  if t.successful = t.total then (
    if disp_flags.files = `All then (
      Format.fprintf ppf
        "@{<green;reverse;ul>  @} @{<cyan>%s@}: @{<green;bold>%d@} / %d tests \
         passed"
        (pfile t.name) t.successful t.total;
      if disp_flags.tests = `All then print_tests t.tests;
      Format.pp_print_cut ppf ()))
  else
    let () =
      match t.successful with
      | 0 -> Format.fprintf ppf "@{<red;reverse;ul>  @}"
      | _ -> Format.fprintf ppf "@{<yellow;reverse;ul>  @}"
    in
    Format.fprintf ppf " @{<cyan>%s@}: " (pfile t.name);
    (function
      | 0 -> Format.fprintf ppf "@{<red;bold>0@}"
      | n -> Format.fprintf ppf "@{<yellow;bold>%d@}" n)
      t.successful;
    Format.fprintf ppf " / %d tests passed" t.total;
    if disp_flags.tests <> `None then print_tests t.tests;
    Format.pp_print_cut ppf ()

type box = { print_line : 'a. ('a, Format.formatter, unit) format -> 'a }
[@@ocaml.unboxed]

let print_box tcolor ppf title (pcontents : box -> unit) =
  let columns = Message.terminal_columns () in
  let tpad = columns - String.width title - 6 in
  Format.fprintf ppf "@,%t┏%t @{<bold;reverse> %s @} %t┓@}@," tcolor
    (Message.pad (tpad / 2) "━")
    title
    (Message.pad (tpad - (tpad / 2)) "━");
  Format.pp_open_tbox ppf ();
  Format.fprintf ppf "%t@<1>%s@}%*s" tcolor "┃" (columns - 2) "";
  Format.pp_set_tab ppf ();
  Format.fprintf ppf "%t┃@}@," tcolor;
  let box =
    {
      print_line =
        (fun fmt ->
          Format.kfprintf
            (fun ppf ->
              Format.pp_print_tab ppf ();
              Format.fprintf ppf "%t┃@}@," tcolor)
            ppf ("%t@<1>%s@}   " ^^ fmt) tcolor "┃");
    }
  in
  pcontents box;
  box.print_line "";
  Format.pp_close_tbox ppf ();
  Format.fprintf ppf "%t┗%t┛@}@," tcolor (Message.pad (columns - 2) "━")

let summary ~build_dir tests =
  let ppf = Message.formatter_of_out_channel stdout () in
  Format.pp_open_vbox ppf 0;
  let tests = List.filter (fun f -> f.total > 0) tests in
  let files, success_files, success, total =
    List.fold_left
      (fun (files, success_files, success, total) file ->
        ( files + 1,
          (if file.successful < file.total then success_files
           else success_files + 1),
          success + file.successful,
          total + file.total ))
      (0, 0, 0, 0) tests
  in
  if disp_flags.files <> `None then
    List.iter (fun f -> display_file ~build_dir ppf f) tests;
  let result_box =
    if success < total then
      print_box (fun ppf -> Format.fprintf ppf "@{<red>") ppf "TESTS FAILED"
    else
      print_box
        (fun ppf -> Format.fprintf ppf "@{<green>")
        ppf "ALL TESTS PASSED"
  in
  result_box (fun box ->
      box.print_line "@{<ul>%-5s %10s %10s %10s@}" "" "FAILED" "PASSED" "TOTAL";
      if files > 1 then
        box.print_line "%-5s @{<red;bold>%a@} @{<green;bold>%a@} @{<bold>%10d@}"
          "files"
          (fun ppf -> function
            | 0 -> Format.fprintf ppf "@{<green>%10d@}" 0
            | n -> Format.fprintf ppf "%10d" n)
          (files - success_files)
          (fun ppf -> function
            | 0 -> Format.fprintf ppf "@{<red>%10d@}" 0
            | n -> Format.fprintf ppf "%10d" n)
          success_files files;
      box.print_line "%-5s @{<red;bold>%a@} @{<green;bold>%a@} @{<bold>%10d@}"
        "tests"
        (fun ppf -> function
          | 0 -> Format.fprintf ppf "@{<green>%10d@}" 0
          | n -> Format.fprintf ppf "%10d" n)
        (total - success)
        (fun ppf -> function
          | 0 -> Format.fprintf ppf "@{<red>%10d@}" 0
          | n -> Format.fprintf ppf "%10d" n)
        success total);
  Format.pp_close_box ppf ();
  Format.pp_print_flush ppf ();
  success = total

let print_xml ~build_dir tests =
  let ffile ppf f = Format.pp_print_string ppf (pfile ~build_dir f) in
  let ppf = Message.formatter_of_out_channel stdout () in
  let tests = List.filter (fun f -> f.total > 0) tests in
  let success, total =
    List.fold_left
      (fun (success, total) file ->
        success + file.successful, total + file.total)
      (0, 0) tests
  in
  Format.fprintf ppf "@[<v><?xml version=\"1.0\" encoding=\"UTF-8\"?>@,";
  Format.fprintf ppf "@[<v 2><testsuites tests=\"%d\" failures=\"%d\">@,"
    success (total - success);
  Format.pp_print_list
    (fun ppf f ->
      Format.fprintf ppf
        "@[<v 2>@[<hov 1><testsuite@ name=\"%a\"@ tests=\"%d\"@ \
         failures=\"%d\">@]@,"
        ffile f.name f.total (f.total - f.successful);
      Format.pp_print_list
        (fun ppf t ->
          Format.fprintf ppf "@[<v 2><testcase line=\"%d\">"
            (fst t.expected).Lexing.pos_lnum;
          Format.fprintf ppf
            "@,\
             @[<hv 2><property name=\"description\">@,\
             @[<hov 2>%a@]@;\
             <0 -2></property>@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Format.pp_print_string)
            (clean_command_line ~build_dir f.name t.command_line);
          if not t.success then (
            Format.fprintf ppf
              "@,@[<v 2><failure message=\"Output differs from reference\">@,";
            print_diff ppf t.expected t.result;
            Format.fprintf ppf "@]@,</failure>");
          Format.fprintf ppf "@]@,</testcase>")
        ppf f.tests;
      Format.fprintf ppf "@]@,</testsuite>")
    ppf tests;
  Format.fprintf ppf "@]@,</testsuites>@,@]@.";
  success = total
