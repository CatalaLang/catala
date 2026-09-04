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
open Clerk_utils
module Nj = Ninja_utils
module Cli = Clerk_cli
module Config = Clerk_config
module OCaml = Clerk_backend.OCaml

(* - Utility functions - *)

let lastdirname f = File.(basename (dirname f))

let backend_src_extensions () =
  List.map
    (fun (module B : Clerk_backend.S) -> B.config_backend, B.src_extensions)
    (Clerk_backend.all ())

let backend_obj_extensions () =
  List.map
    (fun (module B : Clerk_backend.S) -> B.config_backend, B.all_obj_extensions)
    (Clerk_backend.all ())

let backend_extensions () =
  let bk_exts = backend_obj_extensions () in
  List.map
    (fun (bk, exts) -> bk, exts @ List.assoc bk bk_exts)
    (backend_src_extensions ())

let extensions_backend () =
  ("cmxa", Clerk_backend.OCaml.T)
  :: List.flatten
       (List.map
          (fun (bk, exts) -> List.map (fun e -> e, bk) exts)
          (backend_extensions ()))

let backend_subdir_list () =
  List.map
    (fun (module B : Clerk_backend.S) -> B.config_backend, B.name)
    (Clerk_backend.all ())

let normalize_backends backends =
  let bks =
    List.sort_uniq Stdlib.compare backends |> List.map Clerk_backend.get
  in
  Message.debug "@[<h>Enabled backends: %a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf b ->
         Format.fprintf ppf "@{<green>%s@}" (Clerk_backend.name b)))
    bks;
  bks

let subdir_backend_list () =
  List.map (fun (bk, dir) -> dir, bk) (backend_subdir_list ())

let backend_subdir bk = List.assoc bk (backend_subdir_list ())
let rule_subdir rule = backend_subdir rule.Config.backend

let backend_to_config = function
  | `Interpret | `OCaml -> Clerk_backend.OCaml.T
  | `C -> Clerk_backend.C.T
  | `Python -> Clerk_backend.Python.T
  | `Java -> Clerk_backend.Java.T

let backends_to_config bks =
  List.sort_uniq Stdlib.compare (List.map backend_to_config bks)

let linking_command ~build_dir ~backend ~info item target =
  let open File in
  let var_bindings = info.Clerk_rules.var_bindings in
  let link_deps it =
    List.map
      (fun m -> (String.Map.find m info.modules_map).item)
      (info.Clerk_rules.linking_deps it)
  in
  match backend with
  | `OCaml ->
    Clerk_backend.OCaml.linking_command ~build_dir ~var_bindings link_deps item
      target
  | `C ->
    Clerk_backend.C.linking_command ~build_dir ~var_bindings link_deps item
      target
  | `Python ->
    Clerk_backend.Python.linking_command ~build_dir link_deps item target
  | `Java ->
    Clerk_backend.Java.linking_command ~build_dir ~var_bindings link_deps item
      target
  | `Custom rule ->
    let var_bindings =
      Var.binding_of_words Var.src
        (List.flatten
           (List.map
              (fun it ->
                let f = Scan.target_file_name it in
                let f = dirname f / rule_subdir rule / basename f in
                List.map
                  (fun ext -> (build_dir / f) -.- ext)
                  rule.Config.in_exts)
              (link_deps item @ [item])))
      :: Var.binding_of_words Var.dst
           (let f = Scan.target_file_name item in
            let f = dirname f / rule_subdir rule / basename f in
            List.map (fun ext -> (build_dir / f) -.- ext) rule.Config.out_exts)
      :: var_bindings
    in
    List.flatten
    @@ List.map
         (fun s ->
           if String.length s > 1 && s.[0] = '$' && s.[1] <> '{' then
             Var.get var_bindings
               (Var.Vector (String.sub s 1 (String.length s - 1)))
           else [Var.expand var_bindings s])
         rule.Config.commandline

let backend_from_arg config ~enabled_backends t =
  let disambiguate_using_subdir t backends ext =
    let d = File.(basename (dirname t)) in
    try List.assoc d (subdir_backend_list ())
    with Not_found -> (
      match List.filter (fun bk -> List.mem bk enabled_backends) backends with
      | [bk] -> bk
      | _ ->
        Message.error
          "Ambiguous target file extension @{<red;bold>%s@} for target@ \
           @{<red>%S@},@ and the directory doesn't match a suitable backend."
          ext t)
  in
  let aux ext =
    match List.filter (fun (e, _) -> e = ext) (extensions_backend ()) with
    | [(_, bk)] -> bk
    | [] -> (
      let errmsg () =
        Message.error
          "The specified target @{<red>%s@} does@ not@ match@ a@ target@ from@ \
           @{<blue>%a@} or@ an@ existing@ directory@ or@ file,@ nor@ does@ it@ \
           have@ a@ recognised@ extension."
          t (Message.link ()) "clerk.toml"
      in
      if ext = "" then errmsg ()
      else
        match
          List.find_opt
            (fun rule -> List.mem ext rule.Config.out_exts)
            config.Config.custom_rules
        with
        | Some rule -> rule.Config.backend
        | None -> (
          let backends =
            List.filter_map
              (fun (e, bk) ->
                if e = ext then Some Clerk_backend.(name (get bk)) else None)
              (extensions_backend ())
          in
          match backends with
          | [] -> errmsg ()
          | bks ->
            Message.error
              "Extension @{<red;bold>%s@} of target@ @{<red>%S@}@ is@ only@ \
               supported@ by@ backend@ %s,@ which@ is@ not@ currently@ \
               enabled."
              ext t (String.concat " or " bks)))
    | conflict ->
      (* Both C and OCaml can generate .o files, for example *)
      disambiguate_using_subdir t (List.map snd conflict) ext
  in
  let bk =
    match File.extension t with
    | "exe" as ext -> (
      try List.assoc File.(basename (dirname t)) (subdir_backend_list ())
      with Not_found ->
        disambiguate_using_subdir t
          (List.filter
             (function
               | Clerk_backend.OCaml.T | Clerk_backend.C.T -> true | _ -> false)
             enabled_backends)
          ext)
    | ext -> aux ext
  in
  if List.mem bk enabled_backends then bk
  else
    Message.error
      "Target @{<red>%S@}@ requires@ the@ backend@ @{<cyan>%s@},@ which@ is@ \
       not@ enabled.@ Use option @{<yellow>--backend %s@}."
      t
      Clerk_backend.(name (get bk))
      Clerk_backend.(name (get bk))

let config_backend = function
  | Clerk_backend.OCaml.T -> `OCaml
  | Clerk_backend.C.T -> `C
  | Clerk_backend.Python.T -> `Python
  | Clerk_backend.Java.T -> `Java
  | _ -> invalid_arg __FUNCTION__

let obj_target ~build_dir:_ ~backend item =
  let name = Clerk_backend.(name (get (backend_to_config backend))) in
  Clerk_backend.obj_dep ~name item

let make_target ~build_dir ~backend ?(main_exec = false) item =
  let open File in
  let f = Scan.target_file_name item -.- File.extension item.Scan.file_name in
  let dir = dirname f in
  let base = basename f in
  let base =
    match backend with
    | `Interpret -> item.Scan.file_name
    | `Interpret_module -> (
      (dir / "ocaml" / base)
      -.- match Sys.backend_type with Sys.Native -> "cmxs" | _ -> "cmo")
    | `OCaml -> (dir / "ocaml" / base) -.- "cmx"
    | `C -> (dir / "c" / base) -.- "o"
    | `Python -> (dir / "python" / base) -.- "py"
    | `Java when item.is_stdlib ->
      (dir / "java" / "catala" / "stdlib" / base) -.- "class"
    | `Java -> (dir / "java" / base) -.- "class"
    | `Custom rule ->
      (dir / rule_subdir rule / base) -.- List.hd rule.Config.in_exts
  in
  let needs_main = match backend with `OCaml | `C -> main_exec | _ -> false in
  Nj.Expr.Word
    (if needs_main then
       (build_dir / remove_extension base) ^ ("+main" -.- extension base)
     else build_dir / base)

let target_backends targets =
  let open Clerk_config in
  if targets = [] then Clerk_backend.all ()
  else
    List.concat_map (fun target -> target.backends) targets
    |> normalize_backends

let setup_report_format ?fix_path verbosity diff_command coverage =
  (match verbosity with
  | `Summary ->
    Clerk_report.set_display_flags ~files:`None ~tests:`None ~coverage:false ()
  | `Short ->
    Clerk_report.set_display_flags ~files:`Failed ~tests:`Failed ~diffs:false
      ~coverage:false ()
  | `Failures ->
    if Catala_utils.Global.options.debug then
      Clerk_report.set_display_flags ~files:`All ()
  | `Verbose -> Clerk_report.set_display_flags ~files:`All ~tests:`All ());
  Clerk_report.set_display_flags ?fix_path ~diff_command ~coverage ()

let run_artifact config ~backend ~var_bindings ?scope ?quiet ~test ~trace src =
  match backend with
  | `OCaml -> Clerk_backend.OCaml.run_artifact ~test ~trace ?scope ?quiet src
  | `C -> Clerk_backend.C.run_artifact ~test ?scope ?quiet src
  | `Python ->
    Clerk_backend.Python.run_artifact config ~test ~trace ?scope ?quiet
      ~var_bindings src
  | `Java ->
    Clerk_backend.Java.run_artifact ~var_bindings ~test ?scope ?quiet src

(* - Ninja target distribution - *)
(* these functions take place in the clerk_run continuation, and explicit its targets. *)

(* The type of targets coming from the user command-line *)
type user_target_args = {
  clerk_targets : Config.target list; (* targets defined in clerk.toml *)
  modules : Clerk_rules.module_info list; (* Catala modules *)
  directories : (File.t * Scan.item list) list; (* whole directories *)
  source_files : Scan.item list;
      (* catala source files that don't define modules *)
  direct_targets : (string * Scan.item * Config.backend) list;
      (* explicit files to be built *)
}

let empty_targets =
  {
    clerk_targets = [];
    modules = [];
    directories = [];
    source_files = [];
    direct_targets = [];
  }

let target_debug_message (t : user_target_args) =
  Message.debug "Will build the following targets:";
  let ppl f =
    Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf item ->
        Format.fprintf ppf "@{<magenta>%s@}" (f item))
  in
  if t.clerk_targets <> [] then
    Message.debug " - Clerk targets: %a"
      (ppl (fun t -> t.Config.tname))
      t.clerk_targets;
  if t.modules <> [] then
    Message.debug " - Modules: %a"
      (ppl (fun t -> Mark.remove t.Clerk_rules.name))
      t.modules;
  if t.directories <> [] then
    Message.debug " - Directories: %a" (ppl fst) t.directories;
  if t.source_files <> [] then
    Message.debug " - Sources: %a"
      (ppl (fun t -> t.Scan.file_name))
      t.source_files;
  if t.direct_targets <> [] then
    Message.debug " - Artifacts: %a" (ppl (fun (f, _, _) -> f)) t.direct_targets

let items_in_subdirs info items dirs =
  List.filter
    (fun it ->
      String.Map.find_opt it.Scan.file_name info.Clerk_rules.inclusion_map
      = None
      && List.exists
           (fun dir ->
             String.starts_with it.Scan.file_name ~prefix:File.(dir / ""))
           dirs)
    items

let included_by info item =
  match
    String.Map.find_opt item.Scan.file_name info.Clerk_rules.inclusion_map
  with
  | Some parent -> parent
  | None -> item

let project_dir_targets ~config info items =
  let dir = config.Clerk_cli.fix_path Filename.current_dir_name in
  { empty_targets with directories = [dir, items_in_subdirs info items [dir]] }

(* default for the build and run commands, `clerk test` has a different rule *)
let default_targets ~config info items =
  match config.Cli.file.global.default_targets with
  | _ :: _ as tnames ->
    let clerk_targets =
      List.map
        (fun tname ->
          try List.find (fun t -> t.Config.tname = tname) config.file.targets
          with Not_found ->
            Message.error "No definition found for default target %s" tname
              ~suggestions:
                (Suggestions.best_candidates
                   (List.map (fun t -> t.Config.tname) config.file.targets)
                   tname))
        tnames
    in
    { empty_targets with clerk_targets }
  | [] -> (
    match config.file.targets with
    | _ :: _ as clerk_targets -> { empty_targets with clerk_targets }
    | [] -> project_dir_targets ~config info items)

let sort_user_target_args
    config
    ~autotest
    ~backends
    items
    (info : Clerk_rules.callback_info)
    (args : string list) : user_target_args =
  let build_dir = config.Cli.file.global.build_dir in
  let backends = if autotest then `OCaml :: backends else backends in
  let clerk_targets, others =
    List.partition_map
      (fun arg ->
        List.find_opt (fun ct -> arg = ct.Config.tname) config.file.targets
        |> function Some t -> Either.Left t | None -> Either.Right arg)
      args
  in
  let modules, others =
    List.partition_map
      (fun arg ->
        match String.Map.find_opt arg info.modules_map with
        | Some m -> Either.Left m
        | None -> Either.Right arg)
      others
  in
  let others =
    List.map
      (fun f ->
        String.remove_prefix
          ~prefix:File.(build_dir / "")
          (config.Cli.fix_path f))
      others
  in
  let directories, others =
    List.partition_map
      (fun f ->
        if Sys.file_exists f && Sys.is_directory f then
          Left (f, items_in_subdirs info items [f])
        else Right f)
      others
  in
  let modules, source_files, others =
    List.fold_left
      (fun (modules, source_files, others) arg ->
        if Scan.get_lang arg = None then modules, source_files, arg :: others
        else
          try
            let item = List.find (fun it -> it.Scan.file_name = arg) items in
            let item =
              if
                item.has_inline_tests
                || Lazy.force item.Scan.has_scope_tests > 0
              then item
              else included_by info item
            in
            match item.module_def with
            | Some m ->
              ( String.Map.find (Mark.remove m) info.modules_map :: modules,
                source_files,
                others )
            | None -> modules, item :: source_files, others
          with Not_found ->
            Message.error "Source file %a not found" File.format arg)
      (modules, [], []) others
  in
  let direct_targets =
    List.map
      (fun arg ->
        let bk =
          backend_from_arg config.file
            ~enabled_backends:(backends_to_config backends)
            arg
        in
        let subdir = backend_subdir bk in
        let fname =
          if lastdirname arg = subdir then arg
          else File.(dirname arg / subdir / basename arg)
        in
        let item =
          try
            List.find
              (fun it ->
                File.((dirname (dirname fname) / basename fname) -.- "")
                = Scan.target_file_name it)
              items
          with Not_found ->
            Message.error "No source to build argument %a found" File.format arg
        in
        arg, item, bk)
      others
  in
  { clerk_targets; modules; directories; source_files; direct_targets }

let ninja_interp_test_targets
    config
    { clerk_targets; modules; directories; source_files; direct_targets = _ } =
  let build_dir = config.Cli.file.global.build_dir in
  let dirs =
    List.concat_map (fun t -> t.Config.ttests) clerk_targets
    @ List.map fst directories
  in
  let no_trailing_slash dir =
    let suffix = Filename.dir_sep in
    if String.ends_with ~suffix dir then
      String.sub dir 0 (String.length dir - String.length suffix)
    else dir
  in
  List.map
    File.(
      fun dir -> Nj.Expr.Word ((build_dir / no_trailing_slash dir) ^ "@test"))
    dirs
  @ List.map
      File.(
        fun m ->
          Nj.Expr.Word ((build_dir / m.Clerk_rules.item.file_name) ^ "@test"))
      modules
  @ List.map
      File.(
        fun item -> Nj.Expr.Word ((build_dir / item.Scan.file_name) ^ "@test"))
      source_files

let module_backends info backends modname =
  let target_backends = Clerk_rules.module_backends info modname in
  List.filter
    (fun bk -> List.mem (backend_to_config bk) target_backends)
    backends

let item_backends info backends item =
  match item.Scan.module_def with
  | Some (m, _) -> module_backends info backends m
  | None ->
    List.fold_left
      (fun backends (m, _) -> module_backends info backends m)
      backends item.Scan.used_modules

let ninja_runtime_targets ~objs backends =
  List.map
    (fun bk ->
      Nj.Expr.Word
        ("@"
        ^ Clerk_backend.(name (get (backend_to_config bk)))
        ^ "/runtime/"
        ^ if objs then "obj" else "src"))
    backends

(* Note: these are the prerequisites for running that are built by ninja: the
   linking and execution are done further below, directly by Clerk *)
let ninja_build_targets
    config
    backends
    _items
    info
    { clerk_targets; modules; directories; source_files; direct_targets } =
  let backends = List.filter (( <> ) `Interpret) backends in
  (* This function is only concerned with the built artifacts *)
  let build_dir = config.Cli.file.global.build_dir in
  let item_build_target ?backends:explicit_backends it =
    let backends =
      match explicit_backends with
      | Some bks -> bks
      | None -> item_backends info backends it
    in
    List.map (fun backend -> make_target ~build_dir ~backend it) backends
  in
  let from_clerk_targets =
    List.concat_map
      (fun t ->
        let backends =
          List.filter
            (fun bk -> List.mem (backend_to_config bk) t.Clerk_config.backends)
            backends
        in
        if backends = [] then (
          Message.warning
            "Target @{<yellow>%s@}@ does@ not@ support@ any@ of@ the@ \
             selected@ backends@ and@ will@ be@ ignored."
            t.tname;
          [])
        else [Nj.Expr.Word ("#" ^ t.tname)])
      clerk_targets
  in
  let from_modules =
    List.concat_map
      (fun m ->
        let t = item_build_target m.Clerk_rules.item in
        if t = [] then
          Message.warning
            "Module @{<cyan>%s@}@ does@ not@ support@ any@ of@ the@ selected@ \
             backends@ and@ will@ be@ ignored."
            (Mark.remove m.name);
        t)
      modules
  in
  let from_directories =
    List.concat_map
      (fun (_, items) ->
        List.concat_map
          (fun it -> item_build_target (included_by info it))
          items)
      directories
  in
  let from_sources = List.concat_map item_build_target source_files in
  let from_direct_targets =
    List.concat_map
      (fun (str, item, backend) ->
        let t = item_build_target ~backends:[config_backend backend] item in
        if t = [] then
          Message.error
            "Could not find a way to build @{<blue>%s@}.@ Check in \
             @{<green>clerk.toml@} that @{<blue>%s@}@ supports@ the@ %s@ \
             backend?"
            str item.file_name
            Clerk_backend.(name (get backend));
        t)
      direct_targets
  in
  ninja_runtime_targets backends ~objs:config.include_objects
  @ from_clerk_targets
  @ from_modules
  @ from_directories
  @ from_sources
  @ from_direct_targets

(* Returns the ninja dependencies along with the items that should be
   executed *)
let ninja_run_targets
    config
    backends
    ~test_only
    items
    info
    { clerk_targets; modules; directories; source_files; direct_targets } :
    (Scan.item * [< `Interpret | `OCaml | `C | `Python | `Java ]) list
    * Nj.Expr.t =
  let build_dir = config.Cli.file.global.build_dir in
  let item_exec_target ?backends:explicit_backends it =
    let backends =
      match explicit_backends with
      | Some bks -> bks
      | None -> item_backends info backends it
    in
    List.map
      (fun backend ->
        ( it,
          backend,
          match backend with
          | `Interpret -> [Clerk_backend.catala_obj_dep it]
          | `OCaml | `C ->
            [
              obj_target ~build_dir ~backend it;
              make_target ~build_dir ~backend ~main_exec:true it;
            ]
          | _ -> [obj_target ~build_dir ~backend it] ))
      backends
  in
  let from_clerk_targets =
    List.concat_map
      (fun t ->
        List.concat_map
          (fun it ->
            let backends =
              List.filter
                (fun bk ->
                  let bk = backend_to_config bk in
                  List.mem bk t.Config.backends)
                backends
            in
            if test_only && Lazy.force it.Scan.has_scope_tests = 0 then []
            else item_exec_target ~backends it)
          (items_in_subdirs info items t.Config.ttests))
      clerk_targets
  in
  let from_modules =
    List.concat_map (fun m -> item_exec_target m.Clerk_rules.item) modules
  in
  let from_directories =
    List.concat_map
      (fun (_, items) ->
        List.concat_map
          (fun it ->
            if test_only && Lazy.force it.Scan.has_scope_tests = 0 then []
            else item_exec_target it)
          items)
      directories
  in
  let from_sources = List.concat_map item_exec_target source_files in
  let from_direct_targets =
    List.concat_map
      (fun (_, item, backend) ->
        item_exec_target ~backends:[config_backend backend] item)
      direct_targets
  in
  let all =
    from_clerk_targets
    @ from_modules
    @ from_directories
    @ from_sources
    @ from_direct_targets
  in
  let exec_targets, nj_targets =
    List.split (List.map (fun (it, bk, tg) -> (it, bk), tg) all)
  in
  ( List.sort_uniq
      (fun (it1, bk1) (it2, bk2) ->
        match String.compare it1.Scan.file_name it2.Scan.file_name with
        | 0 -> Stdlib.compare bk1 bk2
        | n -> n)
      exec_targets,
    List.flatten nj_targets )

let set_ninja_targets nin_ppf ninja_targets =
  if ninja_targets = [] then raise Clerk_rules.Stop_ninja
  else Nj.format_def nin_ppf (Nj.default ninja_targets)

(* - Finalisers - *)
(* These functions run post-build steps, after ninja has completed *)

(* Installs expected files to _targets/ *)
let install_backend_targets
    ~config
    (build_info : Clerk_rules.callback_info)
    (targets : Clerk_config.target list)
    (bk : Clerk_config.backend) =
  let open File in
  let module B = (val Clerk_backend.get bk) in
  let target_dir = config.Cli.file.global.target_dir in
  let build_dir = config.Cli.file.global.build_dir in
  if not (List.exists (fun t -> List.mem bk t.Clerk_config.backends) targets)
  then ()
  else
    let is_java = config_backend bk = `Java in
    let bk_dir = target_dir / backend_subdir bk in
    let extensions =
      B.src_extensions
      @
      if config.include_objects then
        List.sort_uniq compare (B.obj_extension :: B.module_extensions)
      else []
    in
    B.install_runtime ~config;
    let target_transitive_deps (t : Clerk_config.target) =
      let rec loop acc (curr : Clerk_config.target) =
        if String.Set.mem curr.tname acc then acc
        else
          let acc = String.Set.add curr.tname acc in
          let next_targets =
            List.filter_map
              (fun s -> String.Map.find_opt s build_info.targets_map)
              curr.dependencies
          in
          List.fold_left (fun acc next -> loop acc next) acc next_targets
      in
      String.Set.(
        loop (singleton Clerk_rules.stdlib_target_name) t |> remove t.tname)
    in
    let install_target target =
      if not (List.mem bk target.Config.backends) then ()
      else
        let target_name =
          if is_java then String.to_snake_case target.tname else target.tname
        in
        let dir = bk_dir / target_name in
        Message.debug "Installing target: %s" (B.name / target_name);
        if target.Config.tname <> Clerk_rules.stdlib_target_name then
          (* install_runtime already did the cleanup for the stdlib *)
          File.remove dir;
        ensure_dir dir;
        let tdeps = target_transitive_deps target in
        String.Map.iter
          (fun _ mod_info ->
            if String.Set.mem target.tname mod_info.Clerk_rules.targets then
              let item = mod_info.item in
              let file ext =
                (if Filename.is_relative item.file_name then
                   build_dir / item.file_name
                 else item.file_name)
                /../ backend_subdir bk
                / Scan.target_basename item
                -.- ext
              in
              List.iter
                (fun ext ->
                  let src = file ext in
                  let src =
                    if (not (exists src)) && item.is_stdlib then
                      build_dir
                      / Scan.libcatala
                      / backend_subdir bk
                      / B.stdlib_subdir
                      / basename src
                    else src
                  in
                  if not is_java then copy_in ~dir ~src
                  else if item.is_stdlib then ()
                  else
                    let prefix_lines =
                      ["package " ^ target_name ^ ";"]
                      @ List.map
                          (fun dep_name ->
                            "import " ^ String.to_snake_case dep_name ^ ".*;")
                          String.Set.(
                            remove Clerk_rules.stdlib_target_name tdeps
                            |> elements)
                    in
                    copy_in_with_prefix
                      ~prefix:(String.concat "\n" prefix_lines ^ "\n\n")
                      ~dir ~src)
                extensions)
          build_info.modules_map;
        let target =
          if is_java && not (target.tname = Clerk_rules.stdlib_target_name) then
            { target with dependencies = String.Set.elements tdeps }
          else target
        in
        B.write_target_def_file ~config ~dir target
    in
    let rec targets_and_deps acc targets =
      (* Always install its dependencies together with a target *)
      match targets with
      | [] -> acc
      | t :: targets ->
        if List.exists (fun t1 -> t1.Config.tname = t.Config.tname) acc then
          targets_and_deps acc targets
        else
          let acc = t :: acc in
          let deps =
            List.map
              (fun t -> String.Map.find t build_info.targets_map)
              t.dependencies
          in
          let acc = targets_and_deps acc deps in
          targets_and_deps acc targets
    in
    let stdlib =
      String.Map.find Clerk_rules.stdlib_target_name build_info.targets_map
    in
    let all_targets = targets_and_deps [stdlib] targets in
    List.iter install_target all_targets;
    if is_java then
      File.with_formatter_of_file (bk_dir / "pom.xml")
      @@ fun ppf ->
      List.filter (fun t -> List.mem bk t.Config.backends) all_targets
      |> Clerk_backend.Java.format_project_pom_xml ~config ppf
(*  ; if target.Config.include_sources then
 *     all_modules_deps
 *     |> List.map (fun it -> it.Scan.file_name)
 *     |> List.sort_uniq compare
 *     |> List.iter (fun src -> File.copy_in ~dir:prefix_dir ~src) *)

let advertise_installed ~config ~backends info targets =
  let open Format in
  let ppl f =
    pp_print_list
      ~pp_sep:(fun _ _ -> ())
      (fun ppf x ->
        pp_print_cut ppf ();
        f ppf x)
  in
  let clerk_targets =
    List.sort
      (fun t1 t2 -> String.compare t1.Config.tname t2.Config.tname)
      targets.clerk_targets
    |> List.filter_map (fun t ->
        let bks =
          List.sort compare
            (List.filter (fun bk -> List.mem bk backends) t.Config.backends)
        in
        if bks = [] then None else Some (t, bks))
  in
  let modules =
    List.sort
      (fun m1 m2 ->
        Mark.compare String.compare m1.Clerk_rules.name m2.Clerk_rules.name)
      targets.modules
    |> List.filter_map (fun m ->
        let bks =
          List.sort compare
            (module_backends info
               (List.map config_backend backends)
               (Mark.remove m.Clerk_rules.name))
        in
        if bks = [] then None else Some (m, bks))
  in
  let directories =
    List.filter
      (fun (dir, items) -> dir <> Filename.current_dir_name && items <> [])
      targets.directories
  in
  if
    clerk_targets <> []
    || modules <> []
    || directories <> []
    || targets.source_files <> []
    || targets.direct_targets <> []
  then
    Message.result
      "@[<v>Build successful. The artefacts can be found at the following:@,\
       %a%a%a%a%a@]"
      (ppl
      @@ fun ppf (t, bks) ->
      fprintf ppf "@[<v 2>[@{<yellow>%s@}]%a@]" t.Config.tname
        File.(
          ppl
          @@ fun ppf bk ->
          let target_name =
            if config_backend bk = `Java then String.to_snake_case t.tname
            else t.tname
          in
          format ppf
            (make_relative_to ~dir:original_cwd
               (config.Cli.file.global.target_dir
               / backend_subdir bk
               / target_name)))
        bks)
      clerk_targets
      (ppl
      @@ fun ppf (m, bks) ->
      fprintf ppf "@[<v 2>[@{<blue>%s@}]%a@]"
        (Mark.remove m.Clerk_rules.name)
        (ppl
        @@ fun ppf bk ->
        File.format ppf
          File.(
            make_relative_to ~dir:original_cwd
              (Var.expr_elt_to_string
                 (make_target ~build_dir:config.Cli.file.global.build_dir
                    ~backend:bk m.item))))
        bks)
      modules
      (ppl
      @@ fun ppf (d, _) ->
      fprintf ppf "@[<v 2>[%a]@,%a@]" File.format d File.format
        File.(
          make_relative_to ~dir:original_cwd
            (config.Cli.file.global.build_dir / d)))
      directories
      (ppl
      @@ fun ppf item ->
      fprintf ppf "@[<v 2>[%s]@,%a@]" item.Scan.file_name File.format
        File.(
          make_relative_to ~dir:original_cwd
            (config.Cli.file.global.build_dir / item.file_name)))
      targets.source_files
      (ppl
      @@ fun ppf (name, it, bk) ->
      fprintf ppf "@[<v 2>[%s]@,%a@]" name File.format
        File.(
          make_relative_to ~dir:original_cwd
            (Var.expr_elt_to_string
               (make_target ~build_dir:config.Cli.file.global.build_dir
                  ~backend:(config_backend bk) it)
            -.- File.extension name)))
      targets.direct_targets

(* Runs the artifacts generated from the given targets (after linking them using
   the appropriate backend compiler when needed) *)
let run_targets
    ?(whole_program = false)
    ?trace
    ?trace_format
    ~test
    config
    cmd
    scope
    scope_input
    (test_targets, info) =
  let build_dir = config.Cli.file.global.build_dir in
  let show_progress = (not Global.options.debug) && Unix.isatty Unix.stdout in
  let progress_pfx =
    if test then "Running backend tests..." else "Running compiled targets..."
  in
  Message.print_status "%s" progress_pfx;
  let msg target =
    if not show_progress then
      let multi_targets =
        match test_targets with [] | [_] -> false | _ -> true
      in
      if multi_targets then
        Format.fprintf (Message.err_ppf ()) "@{<blue>>@} @{<cyan>%s@}@."
          File.(make_relative_to ~dir:build_dir target -.- "")
  in
  let re_success =
    (* '\r' tolerated: Windows text-mode output keeps it past [input_line] *)
    Re.(
      compile
        (seq
           [
             str "RESULT";
             rep1 any;
             str "executed successfully.";
             rep (set "\r");
             eos;
           ]))
  in
  let count_tests item = Lazy.force item.Scan.has_scope_tests in
  let count_success item out_lines =
    ( List.fold_left
        (fun success line ->
          if Re.execp re_success line then success + 1 else success)
        0 out_lines,
      count_tests item )
  in
  let quiet = test && not Global.options.debug in
  let test_targets =
    if test then
      (* in test mode, interpreted tests have already be run through clerk *)
      List.filter
        (fun (item, backend) ->
          backend <> `Interpret && Lazy.force item.Scan.has_scope_tests > 0)
        test_targets
    else test_targets
  in
  let progress = ref 0 in
  let total = List.length test_targets in
  let run_target ((item, backend) as test_target) =
    Message.print_percent progress_pfx !progress total;
    incr progress;
    let target =
      Var.expr_elt_to_string ~var_bindings:info.Clerk_rules.var_bindings
        (make_target ~build_dir ~backend ~main_exec:true item)
    in
    match backend with
    | `Interpret ->
      let () =
        match scope_input, test_targets with
        | None, _ | Some _, [_] -> ()
        | Some _, _ ->
          Message.error
            "Multiple targets found for a single input, please specify a \
             single target."
      in
      let catala_flags =
        let bdgs = Var.get info.Clerk_rules.var_bindings Var.catala_flags in
        if trace <> None then List.filter (( <> ) "--trace") bdgs else bdgs
      in
      let catala_flags =
        catala_flags
        @ (match scope with
          | None -> []
          | Some scope -> [Printf.sprintf "--scope=%s" scope])
        @ (match scope_input with
          | None -> []
          | Some input ->
            [
              Printf.sprintf "--input=%s" (Yojson.Safe.to_string ~std:true input);
            ])
        @ (if whole_program then ["--whole-program"] else [])
        @ (match trace with
          | None -> []
          | Some `Stdout -> ["--trace"]
          | Some (`FileName (f : Global.raw_file)) ->
            [Printf.sprintf "--trace=%s" (f :> string)])
        @
        match trace, trace_format with
        | None, _ | _, None -> []
        | _, Some Catala_utils.Global.JSON -> ["--trace-format=json"]
        | _, Some Human -> ["--trace-format=human"]
      in
      let exec = Var.get info.Clerk_rules.var_bindings Var.catala_exe in
      let cmd = exec @ [cmd; target] @ catala_flags in
      msg target;
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      let code, lines = Clerk_cli.run_command_line ~quiet cmd in
      if code <> 0 && quiet then List.iter print_endline lines;
      test_target, if test then count_success item lines else code, 0
    | (`C | `OCaml | `Python | `Java) as backend -> (
      let link_cmd = linking_command ~build_dir ~backend ~info in
      let cmd = link_cmd item target in
      if cmd <> [] then (
        msg target;
        Message.debug "Running command: '%s'..." (String.concat " " cmd));
      match Clerk_cli.run_command_line ~quiet cmd with
      | 0, _ ->
        let code, lines =
          run_artifact ~test ~trace:(trace <> None) config ~backend
            ~var_bindings:info.Clerk_rules.var_bindings ?scope ~quiet target
        in
        if code <> 0 && quiet then List.iter print_endline lines;
        test_target, if test then count_success item lines else code, 0
      | code, out_lines ->
        if quiet then List.iter print_endline out_lines;
        test_target, if test then 0, count_tests item else code, 0)
  in
  List.map run_target test_targets

(* - CLI commands - *)

(* It is expected that [Clerk_rules.run_ninja] is only run from here, and once
   per command. *)

open Cmdliner

let raw_cmd : int Cmd.t =
  let run
      config
      autotest
      code_coverage
      (targets : string list)
      (ninja_flags : string list) =
    if targets <> [] then
      let targets =
        List.map
          (fun f ->
            if
              String.exists (function '/' | '\\' | '.' -> true | _ -> false) f
            then config.Cli.fix_path f
            else f)
          targets
      in
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~trace:false
        ~default:0 ~ninja_flags:(ninja_flags @ targets) (fun _ _ _ -> 0)
    else (
      Format.eprintf "Available targets:@.";
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~trace:false
        ~default:0
        ~ninja_flags:(ninja_flags @ ["-t"; "targets"])
        (fun _ _ _ -> 0))
  in
  let doc =
    "Low-level build command: can be used to forward build targets or options \
     directly to Ninja. Without a target argument, lists all available raw \
     targets to stdout."
  in
  Cmd.v
    (Cmd.info ~doc "raw-target")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.autotest
      $ Cli.code_coverage
      $ Cli.targets
      $ Cli.ninja_flags)

let build_cmd : int Cmd.t =
  let run
      config
      autotest
      code_coverage
      (target_args : string list)
      backends
      build_objects
      (ninja_flags : string list) =
    let backends =
      if backends = [] then [`OCaml; `C; `Python; `Java] else backends
    in
    let config =
      if build_objects then { config with Cli.include_objects = true }
      else config
    in
    let enabled_backends = backends_to_config backends in
    let targets, info =
      Clerk_rules.run_ninja ~code_coverage ~config ~enabled_backends
        ~trace:false
        ~default:(empty_targets, Clerk_rules.empty_info)
        ~ninja_flags ~autotest:false ~clean_up_env:false
      @@ fun nin_ppf items info ->
      let targets =
        if target_args = [] then default_targets ~config info items
        else
          sort_user_target_args config ~autotest ~backends items info
            target_args
      in
      target_debug_message targets;
      let ninja_targets =
        ninja_build_targets config backends items info targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      targets, info
    in
    List.iter
      (install_backend_targets ~config info targets.clerk_targets)
      enabled_backends;
    advertise_installed ~config ~backends:enabled_backends info targets;
    raise (Catala_utils.Cli.Exit_with 0)
  in
  let doc = "Builds the targets given as arguments." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Any $(i,clerk targets) specified on the command-line gets built and \
         written into $(i,target-dir) (by default $(b,_target)), according to \
         its specification in $(b,clerk.toml) ; any dependencies of these \
         targets are also included.";
      `P
        "For $(i,individual files), and given the corresponding Catala module \
         is declared, this can be used to build .ml, .cmxs, .c, .py files, \
         etc. These files, along with their dependencies, are written into \
         $(i,build-dir) (by default $(b,_build)). If a file with a catala \
         extension is used as target, this compiles all its dependencies. The \
         format of the targets is $(b,src-dir/BACKEND/file.ext). For example, \
         to build a C object file from $(b,foo/bar.catala_en), one would run:";
      `Pre "clerk build foo/c/bar.o";
      `P
        "and the resulting file would be in $(b,_build/foo/c/bar.o). \
         Specifying a directory will build all files below it.";
      `P
        "With no arguments, the default targets specified in $(b,clerk.toml) \
         are used if defined, or all specified targets if not. If no targets \
         are defined, the current directory is assumed.";
    ]
  in
  Cmd.v
    (Cmd.info ~doc ~man "build")
    Term.(
      const run
      $ Cli.init_term ()
      $ Cli.autotest
      $ Cli.code_coverage
      $ Cli.clerk_targets_or_files_or_folders
      $ Cli.backends
      $ Cli.objects
      $ Cli.ninja_flags
      $ Catala_utils.Cli.Flags.trace
      $ Catala_utils.Cli.Flags.trace_format)

let run_cmd =
  let run
      config
      (target_args : string list)
      backends
      cmd
      (scope : string option)
      scope_input
      (ninja_flags : string list)
      prepare_only
      whole_program
      trace
      trace_format =
    let config : Cli.config = config in
    let backends = if backends = [] then [`Interpret] else backends in
    let _test_only =
      match scope_input, scope, backends with
      | _, Some _, [`Interpret] -> `No
      | Some _, None, _ ->
        Message.error
          "A scope must be specified when providing a JSON input. See --scope \
           option."
      | Some _, Some _, _ ->
        Message.error "JSON input is only supported with the interpret backend."
      | _ ->
        if List.mem `Interpret backends then `Cli_or_scope
        else `Scope (* backends only offers entrypoints for test scopes *)
    in
    let enabled_backends = backends_to_config backends in
    let exec_targets, _items, info =
      Clerk_rules.run_ninja ~code_coverage:false ~config ~enabled_backends
        ~default:([], [], Clerk_rules.empty_info)
        ~trace:(trace <> None) ~ninja_flags ~autotest:false ~clean_up_env:false
      @@ fun nin_ppf items info ->
      let targets =
        if target_args = [] then default_targets ~config info items
        else
          sort_user_target_args config ~autotest:false ~backends items info
            target_args
      in
      target_debug_message targets;
      let exec_targets, nj_exec_targets =
        ninja_run_targets config backends ~test_only:false items info targets
      in
      let ninja_targets =
        ninja_runtime_targets backends
          ~objs:(List.exists (( <> ) `Interpret) backends)
        @ nj_exec_targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      exec_targets, items, info
    in
    if prepare_only then (
      Message.result "@[<v 4>Build successful@]";
      Cmd.Exit.ok)
    else
      let results =
        run_targets ~test:false ~whole_program ?trace ?trace_format config cmd
          scope scope_input (exec_targets, info)
      in
      if List.for_all (fun (_, (success, total)) -> success = total) results
      then Cmd.Exit.ok
      else Cmd.Exit.some_error
  in
  let doc =
    "Runs the Catala interpreter on the given files, after building their \
     dependencies. The scope to be executed can be specified using the $(i,-s) \
     option."
  in
  Cmd.v (Cmd.info ~doc "run")
    Term.(
      const run
      $ Cli.init_term ()
      $ Cli.files_or_folders
      $ Cli.backends
      $ Cli.run_command
      $ Cli.scope_opt
      $ Cli.scope_input
      $ Cli.ninja_flags
      $ Cli.prepare_only
      $ Cli.whole_program
      $ Catala_utils.Cli.Flags.trace
      $ Catala_utils.Cli.Flags.trace_format)

let typecheck_cmd =
  let run config (target_args : File.t list) disable_warnings =
    let items, info = Clerk_rules.scan_project ~config in
    let targets =
      if target_args = [] then project_dir_targets ~config info items
      else
        sort_user_target_args config ~autotest:false ~backends:[`Interpret]
          items info target_args
    in
    let check_items =
      List.concat_map
        (fun t ->
          List.map
            (fun m -> (String.Map.find m info.modules_map).item)
            t.Config.tmodules)
        targets.clerk_targets
      @ List.map (fun m -> m.Clerk_rules.item) targets.modules
      @ List.concat_map snd targets.directories
      @ targets.source_files
      @ List.map (fun (_, it, _) -> it) targets.direct_targets
    in
    let check_items = List.map (included_by info) check_items in
    let check_items =
      List.sort_uniq
        (fun it1 it2 -> File.compare it1.Scan.file_name it2.Scan.file_name)
        check_items
    in
    if check_items = [] then Message.error "Nothing to typecheck."
    else
      let catala_flags = Var.get info.var_bindings Var.catala_flags in
      let exec = Var.get info.var_bindings Var.catala_exe in
      let ret =
        List.map
          (fun it ->
            let cmd =
              exec
              @ ["typecheck"; "--quiet"]
              @ (if disable_warnings then ["--disable-warnings"] else [])
              @ catala_flags
              @ [it.Scan.file_name]
            in
            Message.debug "Running command: '%s'..." (String.concat " " cmd);
            fst (Clerk_cli.run_command_line cmd))
          check_items
      in
      let ret = List.fold_left max 0 ret in
      if ret = 0 then Message.result "Typechecking successful!";
      ret
  in
  let doc = "Runs the Catala type-checker on the given files." in
  Cmd.v
    (Cmd.info ~doc "typecheck")
    Term.(
      const run $ Cli.init_term () $ Cli.files_or_folders $ Cli.disable_warnings)

let clean_cmd =
  let run (config : Cli.config) =
    File.remove config.Cli.file.Config.global.build_dir;
    File.remove config.Cli.file.Config.global.target_dir;
    raise (Catala_utils.Cli.Exit_with 0)
  in
  let doc =
    "Removes files and directories previously generated by $(i,clerk) if any."
  in
  Cmd.v (Cmd.info ~doc "clean") Term.(const run $ Cli.init_term ())

let test_cmd =
  let run
      config
      (target_args : string list)
      (backends : [ `Interpret | `OCaml | `C | `Python | `Java ] list)
      (reset_test_outputs : bool)
      verbosity
      (report_format : [ `Terminal | `JUnitXML | `VSCodeJSON ])
      code_coverage
      (diff_command : string option option)
      (ninja_flags : string list) : int =
    let enable_backend_tests = List.exists (( <> ) `Interpret) backends in
    let backends = if backends = [] then [`Interpret] else backends in
    let build_dir = config.Cli.file.global.build_dir in
    setup_report_format ~fix_path:config.Cli.fix_path verbosity diff_command
      code_coverage;
    if not (List.mem `Interpret backends) then
      if config.Cli.test_flags <> [] then
        Message.error
          "Test flags can only be supplied with the default \
           @{<yellow>interpret@} backend"
      else if reset_test_outputs then
        Message.error
          "@{<cyan>--reset@} can only be supplied with the default \
           @{<yellow>interpret@} backend"
      else if report_format = `JUnitXML then
        (* TODO *)
        Message.error
          "Option @{<cyan>--report-format=json@} was specified, but the output \
           of a test report is only@ supported@ with@ the@ default@ \
           @{<yellow>interpret@}@ backend@ at@ the@ moment"
      else if report_format = `VSCodeJSON then
        (* TODO *)
        Message.error
          "Option @{<cyan>--report-format=vscode@} was specified, but the \
           output of a test report is@ only@ supported@ with@ the@ default@ \
           @{<yellow>interpret@}@ backend@ at@ the@ moment"
      else if code_coverage then
        Message.error
          "Option @{<cyan>--code-coverage@} was specified, but the measure of \
           code coverage is only@ supported@ with@ the@ default@ \
           @{<yellow>interpret@}@ backend.@ Please use a backend-specific \
           coverage tool instead.";
    let _test_only =
      if List.mem `Interpret backends then `Cli_or_scope else `Scope
    in
    let enabled_backends =
      backends_to_config (`Interpret :: backends)
      (* Autotests always require the interpret (OCaml) objects *)
    in
    let exec_targets, _items, info, test_targets =
      Clerk_rules.run_ninja ~code_coverage ~config ~keep_going:false
        ~enabled_backends ~ninja_flags ~clean_up_env:true ~autotest:true
        ~tests:true ~trace:false
        ~default:([], [], Clerk_rules.empty_info, [])
      @@ fun nin_ppf items info ->
      (* TODO: keep_going:true, to be able to still show a test report.
         We must not try to run the tests, however, since the artifacts we
         failed to build could remain from a previous run and that would be
         confusing. *)
      let targets =
        if target_args = [] then project_dir_targets ~config info items
        else
          sort_user_target_args config ~autotest:true ~backends items info
            target_args
      in
      target_debug_message targets;
      let test_targets =
        if List.mem `Interpret backends then
          ninja_interp_test_targets config targets
        else []
      in
      let exec_targets, ninja_targets =
        if enable_backend_tests then
          let backends = List.filter (( <> ) `Interpret) backends in
          let exec_targets, nj_exec_targets =
            ninja_run_targets config backends ~test_only:true items info targets
          in
          ( exec_targets,
            ninja_runtime_targets backends ~objs:true
            @ nj_exec_targets
            @ test_targets )
        else [], test_targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      exec_targets, items, info, test_targets
    in
    let open Clerk_report in
    let test_reports =
      if List.mem `Interpret backends then
        try
          List.fold_left
            (fun acc f ->
              File.Map.union
                (fun _ _ x -> Some x)
                acc
                (read_many (Var.expr_elt_to_string f)))
            File.Map.empty test_targets
          |> File.Map.values
        with Sys_error _ ->
          Message.error
            "Tests couldn't be run, check the above compilation errors."
      else []
    in
    let test_reports =
      if not reset_test_outputs then test_reports
      else
        let ppf = Message.formatter_of_out_channel stdout () in
        match
          List.filter
            (fun f -> List.exists (fun t -> not t.i_success) f.tests)
            test_reports
        with
        | [] ->
          Format.fprintf ppf
            "[@{<green>DONE@}] All cli tests passed, nothing to reset@.";
          test_reports
        | need_reset ->
          List.iter
            (fun f ->
              let files =
                List.fold_left
                  (fun files t ->
                    if t.i_success then files
                    else
                      File.Map.add (fst t.i_result).Lexing.pos_fname
                        (File.remove_prefix
                           File.(build_dir / "")
                           (fst t.i_expected).Lexing.pos_fname)
                        files)
                  File.Map.empty f.tests
              in
              File.Map.iter
                (fun result expected -> File.copy ~src:result ~dst:expected)
                files)
            need_reset;
          Format.fprintf ppf
            "[@{<green>DONE@}] @{<yellow;bold>%d@} test files were \
             @{<yellow>RESET@}@."
            (List.length need_reset);
          List.map (fun f -> { f with successful = f.total }) test_reports
    in
    let backend_tests =
      if enable_backend_tests then
        run_targets ~test:true config "interpret" None None (exec_targets, info)
      else []
    in
    if reset_test_outputs && report_format = `Terminal && backend_tests = []
    then raise (Catala_utils.Cli.Exit_with 0)
    else if
      (match report_format with
      | `JUnitXML -> print_xml
      | `Terminal -> summary ~backend_tests
      | `VSCodeJSON -> print_json)
        ~build_dir test_reports
    then raise (Catala_utils.Cli.Exit_with 0)
    else raise (Catala_utils.Cli.Exit_with 1)
  in
  let doc =
    "Scan the given files, directories or clerk targets for catala tests, \
     build their requirements and run them all. If $(b,--backend) is \
     unspecified or $(b,interpret), both scope tests and CLI tests are run ; \
     $(b,--reset) can be used to rewrite the expected results of CLI tests to \
     their current result. For any other $(b,--backend), CLI tests are skipped \
     and scope tests are compiled to the specified backend with the catala \
     option $(b,--autotest), and then run, ensuring the consistency of \
     results. When clerk targets are provided, only their specifically defined \
     tests will be executed."
  in
  Cmd.v (Cmd.info ~doc "test")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.clerk_targets_or_files_or_folders
      $ Cli.backends
      $ Cli.reset_test_outputs
      $ Cli.report_verbosity
      $ Cli.report_format
      $ Cli.code_coverage
      $ Cli.diff_command
      $ Cli.ninja_flags)

let runtest_cmd =
  let run
      catala_exe
      catala_opts
      include_dirs
      test_flags
      report
      code_coverage
      out
      file
      whole_program =
    let catala_opts =
      catala_opts
      @ List.fold_right (fun dir opts -> "-I" :: dir :: opts) include_dirs []
    in
    let test_flags = List.filter (( <> ) "") test_flags in
    let catala_opts =
      if whole_program then "--whole-program" :: catala_opts else catala_opts
    in
    Clerk_runtest.run_tests
      ~catala_exe:(Option.value ~default:"catala" catala_exe)
      ~catala_opts ~code_coverage ~test_flags ~report ~out file;
    0
  in
  let doc =
    "Mainly for internal purposes. Runs cli tests and annotated test scopes \
     from a Catala file, and outputs their results to stdout"
  in
  Cmd.v (Cmd.info ~doc "runtest")
    Term.(
      const run
      $ Cli.catala_exe
      $ Cli.catala_opts
      $ Cli.include_dirs
      $ Cli.test_flags
      $ Cli.runtest_report
      $ Cli.code_coverage
      $ Cli.runtest_out
      $ Cli.single_file
      $ Cli.whole_program)

let run_ninja_start ~config ~ninja_flags ~enabled_backends cont =
  let enabled_backends =
    (* Enforce OCaml backend: eventual targets may not have enabled it *)
    (module Clerk_backend.OCaml : Clerk_backend.S) :: enabled_backends
    |> List.sort_uniq compare
  in
  let default =
    List.fold_left
      (fun default_rules (module B : Clerk_backend.S) ->
        Nj.Expr.Word ("@" ^ B.name ^ "/runtime/src")
        :: Clerk_backend.src_dep ~name:B.name "Stdlib_fr"
        :: Clerk_backend.src_dep ~name:B.name "Stdlib_en"
        :: default_rules)
      [
        Clerk_backend.catala_obj_target "Stdlib_fr";
        Clerk_backend.catala_obj_target "Stdlib_en";
      ]
      enabled_backends
  in
  Clerk_rules.run_ninja ~skip_project_scan:true ~code_coverage:false ~default:0
    ~config ~enabled_backends:(List.map Clerk_backend.id enabled_backends)
    ~autotest:false ~ninja_flags (fun nin_ppf _ _ ->
      Nj.format_def nin_ppf (Nj.default default);
      cont ())

let start_cmd =
  let run config (ninja_flags : string list) =
    let enabled_backends = target_backends config.Cli.file.targets in
    run_ninja_start ~config ~ninja_flags ~enabled_backends ~trace:false
      (fun () -> 0)
  in
  let doc =
    "This command prepares the local build environment of the project with \
     objects that are needed by Catala, including the runtime and stdlib. It \
     is never needed before running another Clerk command, but may be useful \
     before direct calls to the $(i,catala) compiler."
  in
  Cmd.v (Cmd.info ~doc "start")
    Term.(const run $ Cli.init_term ~allow_test_flags:true () $ Cli.ninja_flags)

let ci_cmd =
  let run
      config
      (target_args : string list)
      backends
      build_objects
      verbosity
      (report_format : [ `Terminal | `JUnitXML | `VSCodeJSON ])
      code_coverage
      (diff_command : string option option)
      (ninja_flags : string list) =
    let backends =
      match backends with
      | [] -> [`Interpret; `OCaml; `C; `Python; `Java]
      | b when not (List.mem `Interpret b) -> `Interpret :: b
      (* Autotests always require the interpret (OCaml) objects *)
      | b -> b
    in
    let config =
      if build_objects then { config with Cli.include_objects = true }
      else config
    in
    let build_dir = config.Cli.file.global.build_dir in
    setup_report_format ~fix_path:config.Cli.fix_path verbosity diff_command
      code_coverage;
    let enabled_backends = backends_to_config backends in
    let targets, exec_targets, _items, info, test_targets =
      Clerk_rules.run_ninja ~code_coverage ~config ~enabled_backends
        ~ninja_flags ~clean_up_env:true ~autotest:true ~tests:true ~trace:false
        ~default:(empty_targets, [], [], Clerk_rules.empty_info, [])
      @@ fun nin_ppf items info ->
      let targets =
        if target_args = [] then
          {
            (default_targets ~config info items) with
            clerk_targets = config.file.targets;
          }
        else
          sort_user_target_args config ~autotest:true ~backends items info
            target_args
      in
      target_debug_message targets;
      let test_targets = ninja_interp_test_targets config targets in
      let build_targets =
        ninja_build_targets config backends items info targets
      in
      let exec_targets, nj_exec_targets =
        ninja_run_targets config
          (List.filter (( <> ) `Interpret) backends)
          ~test_only:true items info targets
      in
      let exec_targets_ninja =
        ninja_runtime_targets backends
          ~objs:(List.exists (( <> ) `Interpret) backends)
        @ nj_exec_targets
      in
      set_ninja_targets nin_ppf
        (build_targets @ test_targets @ exec_targets_ninja);
      targets, exec_targets, items, info, test_targets
    in
    let open Clerk_report in
    let test_reports =
      if List.mem `Interpret backends then
        try
          List.fold_left
            (fun acc f ->
              File.Map.union
                (fun _ _ x -> Some x)
                acc
                (read_many (Var.expr_elt_to_string f)))
            File.Map.empty test_targets
          |> File.Map.values
        with Sys_error _ ->
          Message.error
            "Tests couldn't be run, check the above compilation errors."
      else []
    in
    let backend_tests =
      run_targets ~test:true config "interpret" None None (exec_targets, info)
    in
    let test_results =
      (match report_format with
      | `JUnitXML -> print_xml
      | `Terminal -> summary ~backend_tests
      | `VSCodeJSON -> print_json)
        ~build_dir test_reports
    in
    if not test_results then raise (Catala_utils.Cli.Exit_with 1);
    List.iter
      (install_backend_targets ~config info targets.clerk_targets)
      enabled_backends;
    advertise_installed ~config ~backends:enabled_backends info targets;
    0
  in
  let doc =
    "Runs all available tests and builds all configured targets. This is the \
     recommended command for continuous integration (CI) workflows. Run with \
     $(b,--debug) for the full log of events."
  in
  Cmd.v (Cmd.info ~doc "ci")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:false ()
      $ Cli.clerk_targets_or_files_or_folders
      $ Cli.backends
      $ Cli.objects
      $ Cli.report_verbosity
      $ Cli.report_format
      $ Cli.code_coverage
      $ Cli.diff_command
      $ Cli.ninja_flags)

let report_cmd =
  let run
      color
      debug
      verbosity
      (report_format : [ `Terminal | `JUnitXML | `VSCodeJSON ])
      code_coverage
      diff_command
      build_dir
      files =
    let _options = Catala_utils.Global.enforce_options ~debug ~color () in
    let build_dir = Option.value ~default:"_build" build_dir in
    setup_report_format verbosity diff_command code_coverage;
    let open Clerk_report in
    let tests =
      List.fold_left
        (fun acc f -> File.Map.union (fun _ _ x -> Some x) acc (read_many f))
        File.Map.empty files
      |> File.Map.values
    in
    let success =
      (match report_format with
      | `JUnitXML -> print_xml
      | `Terminal ->
        fun ~build_dir tests -> summary ?backend_tests:None ~build_dir tests
      | `VSCodeJSON -> print_json)
        ~build_dir tests
    in
    exit (if success then 0 else 1)
  in
  let doc =
    "Mainly for internal purposes. Reads a test report file and displays a \
     summary of the results, returning 0 on success and 1 if any test failed."
  in
  Cmd.v (Cmd.info ~doc "report")
    Term.(
      const run
      $ Cli.color
      $ Cli.debug
      $ Cli.report_verbosity
      $ Cli.report_format
      $ Cli.code_coverage
      $ Cli.diff_command
      $ Cli.build_dir
      $ Cli.files)

let list_vars_cmd =
  let run config =
    let var_bindings =
      Var.env_of_bindings
        (Clerk_rules.base_bindings ~autotest:false ~trace:false
           ~code_coverage:false
           ~enabled_backends:
             (List.map snd (Clerk_config.registered_backends ()))
           ~config ~inplace:false)
    in
    Format.eprintf "Defined variables:@.";
    Format.open_vbox 0;
    (* one quoted token per element: joining them would hide how an override was
       split into words *)
    let _vars =
      List.fold_left
        (fun seen (s, value) ->
          if not (String.Set.mem s seen) then
            Format.printf "%s=%a@," s
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
                 (fun ppf w -> Format.fprintf ppf "%S" w))
              value;
          String.Set.add s seen)
        String.Set.empty
        (List.stable_sort compare var_bindings)
    in
    Format.close_box ();
    0
  in
  let doc =
    "List pre-defined build variables that can be overriden using the \
     $(i,--vars) flag, or in the [variables] section of $(b,clerk.toml)."
  in
  Cmd.v (Cmd.info ~doc "list-vars") Term.(const run $ Cli.init_term ())

let json_schema_cmd =
  let run config file scope =
    let var_bindings =
      Clerk_rules.base_bindings ~autotest:false ~code_coverage:false
        ~trace:false ~enabled_backends:[] ~config ~inplace:true
    in
    let catala_exe = Var.get var_bindings Var.catala_exe in
    let catala_flags = Var.get var_bindings Var.catala_flags in
    let cmd =
      catala_exe @ ["json-schema"; file; "--scope"; scope] @ catala_flags
    in
    Message.debug "Running command: '%s'..." (String.concat " " cmd);
    Sys.chdir File.original_cwd;
    fst (Clerk_cli.run_command_line cmd)
  in
  let doc =
    "Display the JSON-schema of the input and output JSON objects of the given \
     scope (using $(b,--scope <scope-name>)). Both schemas are contained in a \
     JSON array of two elements: first one being the input, the second one the \
     output."
  in
  Cmd.v
    (Cmd.info ~doc "json-schema")
    Term.(const run $ Cli.init_term () $ Cli.single_file $ Cli.scope)

let exceptions_cmd =
  let run config file scope variable =
    (* The exceptions command only needs the desugaring pass — no compiled
       artifacts required. Bypass ninja and call catala directly from the
       project root instead of the build dir (with [inplace:true]) *)
    let var_bindings =
      Clerk_rules.base_bindings ~autotest:false ~code_coverage:false
        ~trace:false ~enabled_backends:[] ~config ~inplace:true
    in
    let catala_exe = Var.get var_bindings Var.catala_exe in
    let catala_flags = Var.get var_bindings Var.catala_flags in
    let cmd =
      catala_exe
      @ ["exceptions"; file; "--scope"; scope; "--variable"; variable]
      @ catala_flags
    in
    Message.debug "Running command: '%s'..." (String.concat " " cmd);
    Sys.chdir File.original_cwd;
    fst (Clerk_cli.run_command_line cmd)
  in
  let doc =
    "Prints the exception tree for the definitions of a particular variable in \
     a scope. Use $(b,-s) to select the scope, $(b,-v) to select the variable, \
     and $(b,--output-format=json) for machine-readable output."
  in
  Cmd.v
    (Cmd.info ~doc "exceptions")
    Term.(
      const run $ Cli.init_term () $ Cli.single_file $ Cli.scope $ Cli.variable)

let main_cmd =
  Cmd.group Cli.info
    [
      build_cmd;
      test_cmd;
      run_cmd;
      typecheck_cmd;
      start_cmd;
      clean_cmd;
      ci_cmd;
      runtest_cmd;
      report_cmd;
      raw_cmd;
      list_vars_cmd;
      json_schema_cmd;
      exceptions_cmd;
    ]

let main () =
  let[@inline] exit_with_error excode emit =
    let bt = Printexc.get_raw_backtrace () in
    emit ();
    if Global.options.debug then Printexc.print_raw_backtrace stderr bt;
    exit excode
  in
  Sys.catch_break true;
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.emit content Error
  | Message.CompilerErrors contents ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.emit_n contents Error
  | Sys.Break ->
    Format.fprintf (Message.err_ppf ()) "@.- Interrupted -@.";
    exit_with_error 130 (fun () -> ())
  | Sys_error msg ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.(emit (of_string ("System error: " ^ msg)) Error)
  | e ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.(
      emit (of_string ("Unexpected error: " ^ Printexc.to_string e)) Error)
