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

open Catala_utils
open Otoml

type backend = ..
type backend += C | OCaml

let registered_backends = ref []

let register_backend ~name backend =
  registered_backends := (name, backend) :: !registered_backends

let () =
  register_backend ~name:"c" C;
  register_backend ~name:"ocaml" OCaml

let registered_backends () = !registered_backends

type doc_backend = Html | Latex

type global = {
  include_dirs : string list;
  build_dir : string;
  catala_opts : string list;
}

type module_ = {
  name : string;
  module_uses : [ `Simple of string | `With_alias of string * string ] list;
  includes : string list;
}

type target = {
  name : string;
  entrypoints : string list;
  backend : backend;
  backend_options : string list;
}

type doc = {
  name : string;
  kind : doc_backend;
  entrypoints : string list;
  doc_options : string list;
}

type config_file = {
  global : global;
  modules : module_ list;
  targets : target list;
  docs : doc list;
}

type t = config_file

let default_global =
  { include_dirs = []; catala_opts = []; build_dir = "_build" }

let default_config =
  { global = default_global; modules = []; targets = []; docs = [] }

let project_encoding =
  let open Clerk_toml_encoding in
  conv
    (fun { include_dirs; catala_opts; build_dir } ->
      proj_empty_list include_dirs, proj_empty_list catala_opts, build_dir)
    (fun (include_dirs, catala_opts, build_dir) ->
      {
        include_dirs = inj_empty_list include_dirs;
        catala_opts = inj_empty_list catala_opts;
        build_dir;
      })
  @@ obj3
       (opt_field ~name:"include_dirs" @@ list string)
       (opt_field ~name:"catala_opts" @@ list string)
       (dft_field ~name:"build_dir" ~default:"_build" string)

let module_uses =
  let open Clerk_toml_encoding in
  let case_module =
    case ~info:{|"<module_name>"|}
      ~proj:(function `Simple n -> Some n | _ -> None)
      ~inj:(fun n -> Some (`Simple n))
      string
  in
  let case_module_alias =
    case ~info:{|["<module_name>", "<module_alias>"]|}
      ~proj:(function `With_alias (n, a) -> Some (n, a) | _ -> None)
      ~inj:(fun (n, a) -> Some (`With_alias (n, a)))
    @@ pair string string
  in
  union [case_module; case_module_alias]

let module_encoding =
  let open Clerk_toml_encoding in
  conv
    (fun { name; module_uses; includes } ->
      name, proj_empty_list module_uses, proj_empty_list includes)
    (fun (name, module_uses, includes) ->
      {
        name;
        module_uses = inj_empty_list module_uses;
        includes = inj_empty_list includes;
      })
  @@ obj3
       (req_field ~name:"name" @@ string)
       (opt_field ~name:"module_uses" @@ list module_uses)
       (opt_field ~name:"includes" @@ list string)

let target_encoding =
  let open Clerk_toml_encoding in
  conv
    (fun { name; entrypoints; backend; backend_options } ->
      name, entrypoints, backend, proj_empty_list backend_options)
    (fun (name, entrypoints, backend, backend_options) ->
      {
        name;
        entrypoints;
        backend;
        backend_options = inj_empty_list backend_options;
      })
  @@ obj4
       (req_field ~name:"name" @@ string)
       (req_field ~name:"entrypoints" @@ list string)
       (req_field ~name:"backend"
       @@ union (string_cases (registered_backends ())))
       (opt_field ~name:"backend_options" @@ list string)

let doc_encoding =
  let open Clerk_toml_encoding in
  conv
    (fun { name; entrypoints; kind; doc_options } ->
      name, entrypoints, kind, proj_empty_list doc_options)
    (fun (name, entrypoints, kind, doc_options) ->
      { name; entrypoints; kind; doc_options = inj_empty_list doc_options })
  @@ obj4
       (req_field ~name:"name" @@ string)
       (req_field ~name:"entrypoints" @@ list string)
       (req_field ~name:"kind"
       @@ union (string_cases ["latex", Latex; "html", Html]))
       (opt_field ~name:"doc_options" @@ list string)

let raw_config_encoding =
  let open Clerk_toml_encoding in
  table4
    (table_opt ~name:"project" project_encoding)
    (multi_table ~name:"module" module_encoding)
    (multi_table ~name:"target" target_encoding)
    (multi_table ~name:"doc" doc_encoding)

let config_encoding : config_file Clerk_toml_encoding.t =
  let open Clerk_toml_encoding in
  convt
    (fun { global; modules; targets; docs } ->
      Some global, modules, targets, docs)
    (fun (global, modules, targets, docs) ->
      {
        global = Option.value global ~default:default_global;
        modules;
        targets;
        docs;
      })
  @@ raw_config_encoding

let read f =
  let toml =
    try Parser.from_file f
    with Parse_error (Some (li, col), msg) ->
      Message.error
        ~pos:(Pos.from_info f li col li (col + 1))
        "Error in Clerk configuration:@ %a" Format.pp_print_text msg
  in
  Clerk_toml_encoding.decode toml config_encoding

let write f config =
  let toml = Clerk_toml_encoding.encode config config_encoding in
  File.with_out_channel f @@ fun oc -> Printer.to_channel oc toml
