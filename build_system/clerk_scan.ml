(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
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

type expected_output_descr = {
  tested_filename : string;
  output_dir : string;
  id : string;
  cmd : string list;
}

type item = {
  file_name : File.t;
  module_def : string option;
  used_modules : string list;
  included_files : File.t list;
  legacy_tests : expected_output_descr list;
  has_inline_tests : bool;
}

let catala_suffix_regex =
  Re.(compile (seq [str ".catala_"; group (seq [alpha; alpha]); eos]))

let test_command_args =
  let open Re in
  let re =
    compile
    @@ seq
      [
        bos;
        char '$';
        rep space;
        str "catala";
        rep space;
        group (rep1 notnl);
        char '\n';
      ]
  in
  fun str ->
    exec_opt re str |>
    Option.map (fun g -> String.trim (Re.Group.get g 1))

let catala_file (file : File.t) (lang : Catala_utils.Cli.backend_lang) :
    item =
  let module L = Surface.Lexer_common in
  let rec parse lines n acc =
    match Seq.uncons lines with
    | None -> acc
    | Some ((_, L.LINE_TEST id), lines) ->
      let test, lines, n = parse_test id lines (n + 1) in
      parse lines n { acc with legacy_tests = test :: acc.legacy_tests }
    | Some ((_, line), lines) -> (
      parse lines (n + 1)
      @@
      match line with
      | L.LINE_INCLUDE f ->
        let f = if Filename.is_relative f then File.(file /../ f) else f in
        { acc with included_files = f :: acc.included_files }
      | L.LINE_MODULE_DEF m -> { acc with module_def = Some m }
      | L.LINE_MODULE_USE m -> { acc with used_modules = m :: acc.used_modules }
      | L.LINE_INLINE_TEST -> { acc with has_inline_tests = true }
      | _ -> acc)
  and parse_test id lines n =
    let test =
      {
        id;
        tested_filename = file;
        output_dir = File.(file /../ "output" / "");
        cmd = [];
      }
    in
    let err n =
      [Format.asprintf "'invalid test syntax at %a:%d'" File.format file n]
    in
    match Seq.uncons lines with
    | Some ((str, L.LINE_ANY), lines) -> (
      match test_command_args str with
      | Some cmd ->
        let cmd, lines, n = parse_block lines (n + 1) [cmd] in
        ( {
            test with
            cmd = List.flatten (List.map (String.split_on_char ' ') cmd);
          },
          lines,
          n + 1 )
      | None -> { test with cmd = err n }, lines, n + 1)
    | Some (_, lines) -> { test with cmd = err n }, lines, n + 1
    | None -> { test with cmd = err n }, lines, n
  and parse_block lines n acc =
    match Seq.uncons lines with
    | Some ((_, L.LINE_BLOCK_END), lines) -> List.rev acc, lines, n + 1
    | Some ((str, _), lines) -> String.trim str :: acc, lines, n + 1
    | None -> List.rev acc, lines, n
  in
  parse
    (Surface.Parser_driver.lines file lang)
    1
    {
      file_name = file;
      module_def = None;
      used_modules = [];
      included_files = [];
      legacy_tests = [];
      has_inline_tests = false;
    }

let get_lang file =
  Option.bind (Re.exec_opt catala_suffix_regex file)
  @@ fun g -> List.assoc_opt (Re.Group.get g 1) Catala_utils.Cli.languages

let tree (dir : File.t) : item Seq.t =
  File.scan_tree
    (fun f ->
      match get_lang f with
      | None -> None
      | Some lang -> Some (catala_file f lang))
    dir
