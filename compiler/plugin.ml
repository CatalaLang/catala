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

let list () = Hashtbl.to_seq_values backend_plugins |> List.of_seq
let names () = Hashtbl.to_seq_keys backend_plugins |> List.of_seq

let load_file f =
  try
    Dynlink.loadfile f;
    Message.emit_debug "Plugin %S loaded" f
  with
  | Dynlink.Error (Dynlink.Module_already_loaded s) ->
    Message.emit_debug "Plugin %S (%s) was already loaded, skipping" f s
  | e ->
    Message.emit_warning "Could not load plugin %S: %s" f (Printexc.to_string e)

let load_dir d =
  Message.emit_debug "Loading plugins from %s" d;
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
