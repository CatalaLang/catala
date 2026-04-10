(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

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
open Var
open File

let handle_suffix = function
  | None | Some "" -> ""
  | Some s -> (
    match s.[0] with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "_" ^ s | _ -> s)

let target ?suffix ?backend ext =
  let ext =
    match ext.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "." ^ ext
    | _ -> ext
  in
  let suffix = handle_suffix suffix in
  let bdir =
    match backend with
    | None -> fun f -> f ^ suffix ^ ext
    | Some b -> fun f -> (b / f) ^ suffix ^ ext
  in
  !Var.tdir / bdir !Var.dst

let extern_src ~filename ~backend ~ext ~missing =
  let f = filename -.- ext in
  match check_file f with
  | Some f -> f, missing
  | None -> (
    match File.check_file (dirname f / backend / basename f) with
    | Some f -> f, missing
    | None -> f, f :: missing)

let check_missing ~backend ~module_def ~missing ~filename =
  if missing <> [] then
    Message.error
      ~pos:(Mark.get (Option.get module_def))
      "@[<v>@[<hov>Module @{<blue>%s@} is marked as external,@ but@ the@ \
       following@ files@ are@ missing:@ %a@]@,\
       @,\
       @[<hov 2>@{<bold>Hint:@} to generate a template, you can use:@ \
       @{<magenta>catala %s --gen-external %s@}@]@]"
      (Mark.remove (Option.get module_def))
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         File.format)
      missing backend filename

let modfile ?suffix ~backend same_dir_modules ext modname =
  match List.assoc_opt modname same_dir_modules with
  | Some _ ->
    Var.(!tdir / backend / String.to_id modname) ^ handle_suffix suffix ^ ext
  | None -> modname ^ "@" ^ backend ^ "-module"

let get_stdlib_module file_name =
  match Scan.get_lang file_name with
  | Some lg ->
    let lg = if Global.has_localised_stdlib lg then lg else Global.En in
    Some ("Stdlib_" ^ Cli.language_code lg)
  | None -> None
