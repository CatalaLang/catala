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
module L = Surface.Lexer_common

type item = {
  file_name : File.t;
  module_def : string Mark.pos option;
  extrnal : bool;
  used_modules : string Mark.pos list;
  included_files : File.t Mark.pos list;
  has_inline_tests : bool;
  has_scope_tests : bool Lazy.t;
}

let catala_suffix_regex =
  Re.(
    compile
      (seq [str ".catala_"; group (seq [alpha; alpha]); opt (str ".md"); eos]))

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
    exec_opt re str |> Option.map (fun g -> String.trim (Re.Group.get g 1))

let get_lang file =
  Option.bind (Re.exec_opt catala_suffix_regex file)
  @@ fun g -> List.assoc_opt (Re.Group.get g 1) Catala_utils.Cli.languages

let rec find_test_scope ~lang file =
  (* Note: if efficiency becomes a problem, this could rely on a cached index of
     file items *)
  let lang = Option.value (get_lang file) ~default:lang in
  let rec scan lines =
    match Seq.uncons lines with
    | Some ((_, L.LINE_TEST_ATTRIBUTE, _), _) -> true
    | Some ((_, L.LINE_INCLUDE f, _), lines) ->
      let f = if Filename.is_relative f then File.(file /../ f) else f in
      scan lines || find_test_scope ~lang f
    | Some (_, lines) -> scan lines
    | None -> false
  in
  scan (Surface.Parser_driver.lines file lang)

let catala_file (file : File.t) (lang : Catala_utils.Global.backend_lang) : item
    =
  let rec parse
      (lines :
        (string * L.line_token * (Lexing.position * Lexing.position)) Seq.t)
      n
      acc =
    match Seq.uncons lines with
    | None -> acc
    | Some ((_, line, lpos), lines) -> (
      let pos = Pos.from_lpos lpos in
      parse lines (n + 1)
      @@
      match line with
      | L.LINE_INCLUDE f ->
        let f = if Filename.is_relative f then File.(file /../ f) else f in
        { acc with included_files = Mark.add pos f :: acc.included_files }
      | L.LINE_MODULE_DEF (m, extrnal) ->
        { acc with module_def = Some (Mark.add pos m); extrnal }
      | L.LINE_MODULE_USE m ->
        { acc with used_modules = Mark.add pos m :: acc.used_modules }
      | L.LINE_INLINE_TEST -> { acc with has_inline_tests = true }
      | L.LINE_TEST_ATTRIBUTE -> { acc with has_scope_tests = lazy true }
      | _ -> acc)
  in
  let item =
    parse
      (Surface.Parser_driver.lines file lang)
      1
      {
        file_name = file;
        module_def = None;
        extrnal = false;
        used_modules = [];
        included_files = [];
        has_inline_tests = false;
        has_scope_tests = lazy false;
      }
  in
  let has_scope_tests =
    lazy
      ((* If there are includes, they must be checked for test scopes as well *)
       Lazy.force item.has_scope_tests
      || List.exists
           (fun l ->
             let included_file = Mark.remove l in
             if File.check_file included_file = None then
               Message.error ~kind:Parsing ~pos:(Mark.get l)
                 "Included file '%s' is not a regular file or does not exist."
                 included_file;
             find_test_scope ~lang included_file)
           item.included_files)
  in
  { item with has_scope_tests }

let tree (dir : File.t) : (File.t * File.t list * item list) Seq.t =
  File.scan_tree
    (fun f ->
      match get_lang f with
      | None -> None
      | Some lang -> Some (catala_file f lang))
    dir

let target_file_name t =
  let open File in
  let dir =
    if Filename.is_relative t.file_name then File.dirname t.file_name
    else "libcatala"
  in
  match t.module_def with
  | Some m -> dir / String.to_id (Mark.remove m)
  | None -> dir / String.to_id (remove_extension (basename t.file_name))
