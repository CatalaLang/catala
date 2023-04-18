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

type 'ast plugin_apply_fun_typ =
  source_file:Pos.input_file ->
  output_file:string option ->
  scope:Shared_ast.ScopeName.t option ->
  'ast ->
  Scopelang.Dependency.TVertex.t list ->
  unit

type 'ast gen = {
  name : string;
  extension : string;
  apply : 'ast plugin_apply_fun_typ;
}

type t =
  | Dcalc of Shared_ast.untyped Dcalc.Ast.program gen
  | Lcalc of Shared_ast.untyped Lcalc.Ast.program gen
  | Scalc of Scalc.Ast.program gen

let name = function
  | Dcalc { name; _ } | Lcalc { name; _ } | Scalc { name; _ } -> name

let backend_plugins : (string, t) Hashtbl.t = Hashtbl.create 17

let register t =
  Hashtbl.replace backend_plugins (String.lowercase_ascii (name t)) t

module PluginAPI = struct
  let register_dcalc ~name ~extension apply =
    register (Dcalc { name; extension; apply })

  let register_lcalc ~name ~extension apply =
    register (Lcalc { name; extension; apply })

  let register_scalc ~name ~extension apply =
    register (Scalc { name; extension; apply })
end

let find name = Hashtbl.find backend_plugins (String.lowercase_ascii name)

let load_file f =
  try
    Dynlink.loadfile f;
    Cli.debug_print "Plugin %S loaded" f
  with e ->
    Errors.format_warning "Could not load plugin %S: %s" f
      (Printexc.to_string e)

let rec load_dir d =
  let dynlink_exts =
    if Dynlink.is_native then [".cmxs"] else [".cmo"; ".cma"]
  in
  Array.iter
    (fun f ->
      if f.[0] = '.' then ()
      else
        let f = Filename.concat d f in
        if Sys.is_directory f then load_dir f
        else if List.exists (Filename.check_suffix f) dynlink_exts then
          load_file f)
    (Sys.readdir d)
