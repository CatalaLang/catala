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
open Clerk_toml_encoding

type backend = ..

let registered_backends_descr = ref []

let register_backend_descr ~name ~default ~matches ~backend_descr =
  let default_config_descr : 'a case =
    case ~info:"default backend"
      ~proj:(fun _ -> None)
      ~inj:(fun name' -> if name = name' then Some default else None)
      string
  in
  let backend_with_config_descr : 'a case =
    case ~info:"configured backend"
      ~proj:(fun backend_and_config ->
        if matches backend_and_config then Some (name, backend_and_config)
        else None)
      ~inj:(fun (name', backend_and_config) ->
        if String.(equal (lowercase_ascii name) (lowercase_ascii name')) then
          Some backend_and_config
        else None)
      (pair string backend_descr)
  in
  let backend_case : 'a case =
    case ~info:(name ^ " backend") ~proj:Option.some ~inj:Option.some
      (union [default_config_descr; backend_with_config_descr])
  in
  registered_backends_descr := backend_case :: !registered_backends_descr

let registered_backends_descr () = !registered_backends_descr

type doc_backend = Html | Latex

type global = {
  include_dirs : File.t list;
  build_dir : File.t;
  target_dir : File.t;
  catala_exe : File.t option;
  catala_opts : string list;
  default_targets : string list;
}

type target = {
  tname : string;
  tmodules : string list;
  ttests : File.t list;
  backends : backend list;
  include_sources : bool;
  include_objects : bool;
}

type doc = {
  name : string;
  kind : doc_backend;
  entrypoints : string list;
  doc_options : string list;
}

type custom_rule = {
  backend : backend;
  in_exts : string list;
  out_exts : string list;
  commandline : string list;
}

type config_file = {
  global : global;
  variables : (string * string list) list;
  targets : target list;
  docs : doc list;
  custom_rules : custom_rule list;
}

type t = config_file

let default_global =
  {
    include_dirs = [];
    catala_exe = None;
    catala_opts = [];
    default_targets = [];
    build_dir = "_build";
    target_dir = "_targets";
  }

let default_config =
  {
    global = default_global;
    variables = [];
    targets = [];
    docs = [];
    custom_rules = [];
  }

let project_encoding =
  conv
    (fun {
           include_dirs;
           catala_exe;
           catala_opts;
           default_targets;
           build_dir;
           target_dir;
         } ->
      ( proj_empty_list include_dirs,
        catala_exe,
        proj_empty_list catala_opts,
        proj_empty_list default_targets,
        build_dir,
        target_dir ))
    (fun ( include_dirs,
           catala_exe,
           catala_opts,
           default_targets,
           build_dir,
           target_dir )
       ->
      {
        include_dirs = inj_empty_list include_dirs;
        catala_exe;
        catala_opts = inj_empty_list catala_opts;
        default_targets = inj_empty_list default_targets;
        build_dir;
        target_dir;
      })
  @@ obj6
       (opt_field ~name:"include_dirs" @@ list string)
       (opt_field ~name:"catala_exe" @@ string)
       (opt_field ~name:"catala_opts" @@ list string)
       (opt_field ~name:"default_targets" @@ list string)
       (dft_field ~name:"build_dir" ~default:default_global.build_dir string)
       (dft_field ~name:"target_dir" ~default:default_global.target_dir string)

let target_encoding =
  conv
    (fun { tname; tmodules; ttests; backends; include_sources; include_objects }
       -> tname, tmodules, ttests, backends, include_sources, include_objects)
    (fun (tname, tmodules, ttests, backends, include_sources, include_objects)
       ->
      { tname; tmodules; ttests; backends; include_sources; include_objects })
  @@ obj6
       (req_field ~name:"name" @@ string)
       (req_field ~name:"modules" @@ list string)
       (dft_field ~name:"tests" ~default:[] @@ list string)
       (dft_field ~name:"backends" ~default:[]
          (delayed (lazy (list (union (registered_backends_descr ()))))))
       (dft_field ~name:"include_sources" ~default:false @@ bool)
       (dft_field ~name:"include_objects" ~default:false @@ bool)

let doc_encoding =
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

let custom_rule_encoding =
  conv
    (fun { backend; in_exts; out_exts; commandline } ->
      backend, in_exts, out_exts, commandline)
    (fun (backend, in_exts, out_exts, commandline) ->
      { backend; in_exts; out_exts; commandline })
  @@ obj4
       (req_field ~name:"backend"
       @@ delayed (lazy (union (registered_backends_descr ()))))
       (req_field ~name:"in_exts" @@ list string)
       (req_field ~name:"out_exts" @@ list string)
       (req_field ~name:"commandline" @@ list string)

let variables_encoding = binding_list (list string)

let raw_config_encoding =
  table5
    (table_opt ~name:"project" project_encoding)
    (table_opt ~name:"variables" variables_encoding)
    (multi_table ~name:"target" target_encoding)
    (multi_table ~name:"doc" doc_encoding)
    (multi_table ~name:"rule" custom_rule_encoding)

let config_encoding : config_file table_descr =
  convt
    (fun { global; variables; targets; docs; custom_rules } ->
      Some global, proj_empty_list variables, targets, docs, custom_rules)
    (fun (global, variables, targets, docs, custom_rules) ->
      {
        global = Option.value global ~default:default_global;
        variables = inj_empty_list variables;
        targets;
        docs;
        custom_rules;
      })
  @@ raw_config_encoding

let pp_target_names fmt ts =
  Format.(
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
      (fun fmt tname -> fprintf fmt "@{<cyan>[%s]@}" tname))
    fmt ts

let validate path (config : config_file) : unit =
  let _, dups =
    List.fold_left
      (fun (s, dups) { tname; _ } ->
        if String.Set.mem tname s then s, String.Set.add tname dups
        else String.Set.add tname s, dups)
      (String.Set.empty, String.Set.empty)
      config.targets
  in
  if not (String.Set.is_empty dups) then
    Message.error "Multiple targets with a same name found in '%s':@ %a"
      (File.clean_path path) pp_target_names (String.Set.elements dups)

let read f =
  let toml =
    try Parser.from_file f
    with Parse_error (Some (li, col), msg) ->
      Message.error
        ~pos:(Pos.from_info f li col li (col + 1))
        "Error in Clerk configuration:@ %a" Format.pp_print_text msg
  in
  let config = decode toml config_encoding in
  validate f config;
  config

let write f config =
  let toml = encode config config_encoding in
  File.with_out_channel ~bin:false f @@ fun oc -> Printer.to_channel oc toml
