(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, OCamlPro;
   contributors: Louis Gesbert <louis.gesbert@ocamlpro.com>

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
open Cmdliner

type t = unit Cmdliner.Cmd.t

let backend_plugins : (string, t) Hashtbl.t = Hashtbl.create 17

let register info term =
  let name = String.lowercase_ascii (Cmd.name (Cmd.v info (Term.const ()))) in
  Hashtbl.replace backend_plugins name
    (Cmd.v info Term.(term $ Cli.Flags.Global.options))

let register_subcommands info cmds =
  let name = String.lowercase_ascii (Cmd.name (Cmd.v info (Term.const ()))) in
  Hashtbl.replace backend_plugins name (Cmd.group info cmds)

let register_attribute = Desugared.Name_resolution.register_attribute
let list () = Hashtbl.to_seq_values backend_plugins |> List.of_seq
let names () = Hashtbl.to_seq_keys backend_plugins |> List.of_seq
let load_failures = Hashtbl.create 17

let print_failures () =
  if Hashtbl.length load_failures > 0 then
    Message.warning
      "@[<v>@[<v 2>Some plugins could not be loaded:@,\
       %a@]@,\
       @,\
       @[<hov>This generally means that they were compiled against a \
       different@ version@ of@ Catala.@]@]"
      (Format.pp_print_seq (fun ppf (n, s) ->
           Format.fprintf ppf "- @{<bold>%s@}: @[<hov>%a@]" n
             Format.pp_print_text s))
      (Hashtbl.to_seq load_failures)

let load_file f =
  try
    Dynlink.loadfile f;
    Message.debug "Plugin %S loaded" f
  with
  | Dynlink.Error (Dynlink.Module_already_loaded s) ->
    Message.debug "Plugin %S (%s) was already loaded, skipping" f s
  | Dynlink.Error err ->
    let msg = Dynlink.error_message err in
    Message.debug "Could not load plugin %S: %s" f msg;
    Hashtbl.add load_failures f msg
  | e -> Message.warning "Could not load plugin %S: %s" f (Printexc.to_string e)

let load_dir d =
  Message.debug "Loading plugins from %s" d;
  let dynlink_exts =
    if Dynlink.is_native then [".cmxs"] else [".cmo"; ".cma"]
  in
  let rec aux d =
    Array.iter
      (fun f ->
        if f.[0] = '.' then ()
        else
          let f = Filename.concat d f in
          match Sys.is_directory f with
          | true -> aux f
          | false ->
            if List.exists (Filename.check_suffix f) dynlink_exts then
              load_file f
          | exception Sys_error _ -> ())
      (Sys.readdir d)
  in
  aux d
