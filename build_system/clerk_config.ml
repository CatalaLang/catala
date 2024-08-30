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

type modul = {
  module_uses : (string * string option) list;
  includes : string list;
}

type t = {
  catala_opts : string list;
  build_dir : string;
  include_dirs : string list;
  modules : modul String.Map.t;
}

let default =
  {
    catala_opts = [];
    build_dir = "_build";
    include_dirs = [];
    modules = String.Map.empty;
  }

let parse_module_uses ~fname ~name modul =
  match find_opt modul (get_array get_value) ["module_uses"] with
  | None | (exception _) -> [] (* [opt] is a lie. *)
  | Some module_uses ->
    List.map
      (function
        | TomlString module_name | TomlArray [TomlString module_name] ->
          module_name, None
        | TomlArray [TomlString module_name; TomlString module_alias] ->
          module_name, Some module_alias
        | _ ->
          Message.error
            "While parsing %a: in module @{<italic>%s@}, invalid value for \
             @{<red>module_uses@}. Expected a module name as string or an \
             array of two strings (module name and its alias)."
            File.format fname name)
      module_uses

let check_module_sanity ~fname modul =
  match modul with
  | TomlTable kvs ->
    let format_module_name ppf =
      match Helpers.find_string_opt modul ["name"] with
      | None | (exception _) -> () (* [opt] is a lie. *)
      | Some name -> Format.fprintf ppf "in module @{<italic>%s@}, " name
    in
    List.iter
      (function
        | "name", TomlString _ -> ()
        | "name", _ ->
          Message.delayed_error ()
            "While parsing %a: invalid value for key @{<red>name@}.@\n\
             Expected a direct string." File.format fname
        | "module_uses", TomlArray _ -> (* checked in [parse_module_uses] *) ()
        | "module_uses", _ ->
          Message.delayed_error ()
            "While parsing %a: %tinvalid value for key @{<red>module_uses@}.@\n\
             It must be an array." File.format fname format_module_name
        | "includes", TomlArray srcs ->
          let only_strings =
            List.for_all (function TomlString _ -> true | _ -> false) srcs
          in
          if not only_strings then
            Message.delayed_error ()
              "While parsing %a: %tinvalid value for key @{<red>includes@}.@\n\
               It must only contain direct strings." File.format fname
              format_module_name
        | "includes", _ ->
          Message.delayed_error ()
            "While parsing %a: %tinvalid content for key @{<red>includes@}.@\n\
             Expected an array of strings." File.format fname format_module_name
        | k, _ ->
          Message.delayed_error ()
            "While parsing %a: %tunexpected key @{<red>%S@}.@\n\
             Allowed keys are: 'name', 'module_uses' and 'includes'."
            File.format fname format_module_name k)
      kvs
  | _ ->
    Message.delayed_error ()
      "While parsing %a: invalid module definition, expected a table."
      File.format fname

let find_and_parse_modules_exn ~fname toml =
  let modules = find toml (get_array get_value) ["module"] in
  let parse_module modul =
    let () = check_module_sanity ~fname modul in
    let name = Helpers.find_string_exn modul ["name"] in
    let module_uses = parse_module_uses ~fname ~name modul in
    let includes =
      try
        Helpers.find_strings_opt modul ["includes"] |> Option.value ~default:[]
      with _ ->
        (* opt is a lie, the error will be triggered later on by the sanity
           check. *)
        []
    in
    name, { module_uses; includes }
  in
  Message.with_delayed_errors
  @@ fun () -> List.map parse_module modules |> String.Map.of_list

let toml_to_config ~fname toml =
  {
    catala_opts = Helpers.find_strings_exn toml ["build"; "catala_opts"];
    build_dir = Helpers.find_string_exn toml ["build"; "build_dir"];
    include_dirs = Helpers.find_strings_exn toml ["project"; "include_dirs"];
    modules = find_and_parse_modules_exn ~fname toml;
  }

let module_to_toml name { module_uses; includes } =
  table
    [
      "name", string name;
      ( "module_uses",
        array
          (List.map
             (function
               | name, None -> string name
               | name, Some alias -> array [string name; string alias])
             module_uses) );
      "includes", array (List.map string includes);
    ]

let config_to_toml t =
  table
    [
      ( "build",
        table
          [
            "catala_opts", array (List.map string t.catala_opts);
            "build_dir", string t.build_dir;
          ] );
      "project", table ["include_dirs", array (List.map string t.include_dirs)];
      ( "module",
        TomlTableArray
          (String.Map.fold
             (fun name modul acc -> module_to_toml name modul :: acc)
             t.modules []
          |> List.rev) );
    ]

let default_toml = config_to_toml default

(* joins default and supplied conf, ensuring types match. The filename is for
   error reporting *)
let rec join ?(rpath = []) fname t1 t2 =
  match t1, t2 with
  | TomlString _, TomlString _
  | TomlInteger _, TomlInteger _
  | TomlFloat _, TomlFloat _
  | TomlBoolean _, TomlBoolean _
  | TomlOffsetDateTime _, TomlOffsetDateTime _
  | TomlLocalDateTime _, TomlLocalDateTime _
  | TomlLocalDate _, TomlLocalDate _
  | TomlLocalTime _, TomlLocalTime _
  | TomlArray _, TomlArray _
  | TomlTableArray _, TomlTableArray _ ->
    t2
  | TomlTable tt1, TomlTable tt2 | TomlInlineTable tt1, TomlInlineTable tt2 ->
    let m1 = String.Map.of_list tt1 in
    let m2 = String.Map.of_list tt2 in
    TomlTable
      (String.Map.merge
         (fun key t1 t2 ->
           match t1, t2 with
           | None, Some _ ->
             Message.error
               "While parsing %a: invalid key @{<red>%S@} at @{<bold>%s@}"
               File.format fname key
               (if rpath = [] then "file root"
                else String.concat "." (List.rev rpath))
           | Some t1, Some t2 -> Some (join ~rpath:(key :: rpath) fname t1 t2)
           | Some t1, None -> Some t1
           | None, None -> assert false)
         m1 m2
      |> String.Map.bindings)
  | _ ->
    Message.error
      "While parsing %a: Wrong type for config value @{<bold>%s@}, was \
       expecting @{<bold>%s@}"
      File.format fname
      (String.concat "." (List.rev rpath))
      (match t1 with
      | TomlString _ -> "a string"
      | TomlInteger _ -> "an integer"
      | TomlFloat _ -> "a float"
      | TomlBoolean _ -> "a boolean"
      | TomlOffsetDateTime _ -> "an offsetdatetime"
      | TomlLocalDateTime _ -> "a localdatetime"
      | TomlLocalDate _ -> "a localdate"
      | TomlLocalTime _ -> "a localtime"
      | TomlArray _ | TomlTableArray _ -> "an array"
      | TomlTable _ | TomlInlineTable _ -> "a table")

let read f =
  let toml =
    try Parser.from_file f
    with Parse_error (Some (li, col), msg) ->
      Message.error
        ~pos:(Pos.from_info f li col li (col + 1))
        "Error in Clerk configuration:@ %a" Format.pp_print_text msg
  in
  toml_to_config ~fname:f (join f default_toml toml)

let write f t =
  let toml = config_to_toml t in
  File.with_out_channel f @@ fun oc -> Printer.to_channel oc toml
