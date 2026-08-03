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
      ( Var.make "src",
        List.flatten
          (List.map
             (fun it ->
               let f = Scan.target_file_name it in
               let f = dirname f / rule_subdir rule / basename f in
               List.map (fun ext -> (build_dir / f) -.- ext) rule.Config.in_exts)
             (link_deps item @ [item])) )
      :: ( Var.make "dst",
           let f = Scan.target_file_name item in
           let f = dirname f / rule_subdir rule / basename f in
           List.map (fun ext -> (build_dir / f) -.- ext) rule.Config.out_exts )
      :: var_bindings
    in
    List.flatten
    @@ List.map
         (fun s ->
           if String.length s > 1 && s.[0] = '$' && s.[1] <> '{' then
             Var.get var_bindings
               (Var.make (String.sub s 1 (String.length s - 1)))
           else [Var.expand var_bindings s])
         rule.Config.commandline

let backend_from_arg config ~enabled_backends t =
  let disambiguate_using_subdir t backends ext =
    let d = File.(basename (dirname t)) in
    match List.assoc_opt d (subdir_backend_list ()) with
    | Some bk when List.mem bk backends -> bk
    | _ ->
      Message.error
        "Ambiguous target file extension @{<red;bold>%s@} for target@ \
         @{<red>%S@},@ and the directory doesn't match a suitable backend."
        ext t
  in
  let aux ext =
    match
      List.filter
        (fun (e, bk) -> e = ext && List.mem bk enabled_backends)
        (extensions_backend ())
    with
    | [(_, bk)] -> bk
    | [] -> (
      if ext = "" then Message.error "Target without extension: @{<red>%S@}" t
      else
        match
          List.find_opt
            (fun rule -> List.mem ext rule.Config.out_exts)
            config.Config.custom_rules
        with
        | Some rule -> rule.Config.backend
        | None ->
          if List.mem_assoc ext (extensions_backend ()) then
            Message.error
              "Extension @{<red;bold>%s@} of target@ @{<red>%S@} is not \
               supported by the enabled backends"
              ext t
          else
            Message.error
              "Unhandled extension @{<red;bold>%s@} for target@ @{<red>%S@}" ext
              t)
    | conflict ->
      (* Both C and OCaml can generate .o files, for example *)
      disambiguate_using_subdir t (List.map snd conflict) ext
  in
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

let config_backend = function
  | Clerk_backend.OCaml.T -> `OCaml
  | Clerk_backend.C.T -> `C
  | Clerk_backend.Python.T -> `Python
  | Clerk_backend.Java.T -> `Java
  | _ -> invalid_arg __FUNCTION__

let obj_target ~build_dir:_ ~backend item =
  let name = Clerk_backend.(name (get (backend_to_config backend))) in
  Clerk_backend.obj_dep ~name item

let make_target ~build_dir ~backend item =
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
  build_dir / base

let target_backends targets =
  let open Clerk_config in
  List.concat_map (fun target -> target.backends) targets |> normalize_backends

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
    Clerk_backend.Python.run_artifact config ~test ?scope ?quiet ~var_bindings
      src
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

(* default for the build and run commands, `clerk test` has a different rule *)
let default_targets config =
  match config.Cli.options.global.default_targets with
  | _ :: _ as tnames ->
    let clerk_targets =
      List.map
        (fun tname ->
          try List.find (fun t -> t.Config.tname = tname) config.options.targets
          with Not_found ->
            Message.error "No definition found for default target %s" tname
              ~suggestions:
                (Suggestions.best_candidates
                   (List.map (fun t -> t.Config.tname) config.options.targets)
                   tname))
        tnames
    in
    { empty_targets with clerk_targets }
  | [] -> (
    match config.Cli.options.targets with
    | _ :: _ as clerk_targets -> { empty_targets with clerk_targets }
    | [] -> { empty_targets with directories = [] })

let items_in_subdirs items dirs =
  List.filter
    (fun it ->
      List.exists
        (fun dir ->
          String.starts_with it.Scan.file_name ~prefix:File.(dir / ""))
        dirs)
    items

let sort_user_target_args
    config
    ~autotest
    ~backends
    items
    (info : Clerk_rules.callback_info)
    (args : string list) : user_target_args =
  let build_dir = config.Cli.options.global.build_dir in
  let backends = if autotest then `OCaml :: backends else backends in
  let clerk_targets, others =
    List.partition_map
      (fun arg ->
        List.find_opt
          (fun ct -> arg = ct.Config.tname)
          config.Cli.options.targets
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
        if Sys.is_directory f then Left (f, items_in_subdirs items [f])
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
          backend_from_arg config.options
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
  let build_dir = config.Cli.options.global.build_dir in
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
  List.map File.(fun dir -> (build_dir / no_trailing_slash dir) ^ "@test") dirs
  @ List.map
      File.(fun m -> (build_dir / m.Clerk_rules.item.file_name) ^ "@test")
      modules
  @ List.map
      File.(fun item -> (build_dir / item.Scan.file_name) ^ "@test")
      source_files

(* The backends for a given module are detected by analysing what clerk targets it
   belongs to *)
let module_backends info backends modname =
  let m = String.Map.find modname info.Clerk_rules.modules_map in
  if String.Set.is_empty m.Clerk_rules.targets then backends
  else
    let target_backends =
      String.Set.fold
        (fun t acc ->
          List.fold_left
            (fun acc bk -> if List.mem bk acc then acc else bk :: acc)
            acc (String.Map.find t info.targets_map).Config.backends)
        m.Clerk_rules.targets []
    in
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

(* Note: these are the prerequisites for running that are built by ninja: the
   linking and execution are done further below, directly by Clerk *)
let ninja_build_targets
    ?(exec_targets = false)
    config
    ~autotest
    backends
    items
    info
    { clerk_targets; modules; directories; source_files; direct_targets } =
  let backends = List.filter (( <> ) `Interpret) backends in
  (* This function is only concerned with the built artifacts *)
  let build_dir = config.Cli.options.global.build_dir in
  let item_exec_target ?backends:explicit_backends it =
    let backends =
      match explicit_backends with
      | Some bks -> bks
      | None -> item_backends info backends it
    in
    let backends_full =
      if exec_targets && autotest then `OCaml :: backends else backends
    in
    List.concat_map
      (fun backend ->
        if List.mem backend backends then
          if exec_targets then
            let t = obj_target ~build_dir ~backend it in
            (* builds all the obj deps transitively *)
            match backend with
            | `OCaml | `C ->
              let t1 = make_target ~build_dir ~backend it in
              [t; File.(remove_extension t1 ^ ("+main" -.- extension t1))]
            | _ -> [t]
          else [make_target ~build_dir ~backend it]
            (* builds only the individual object *)
        else [])
      backends_full
  in
  let runtimes =
    List.map
      (fun bk ->
        "@"
        ^ Clerk_backend.(name (get (backend_to_config bk)))
        ^ "/runtime/"
        ^ if exec_targets then "obj" else "src")
      backends
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
             selected@ backends"
            t.tname;
          [])
        else
          let items =
            List.filter
              (fun it -> Lazy.force it.Scan.has_scope_tests > 0)
              (items_in_subdirs items t.Config.ttests)
          in
          if items = [] then
            Message.warning
              "Nothing to run was found in the test directories of target@ \
               @{<yellow>%s@}"
              t.tname;
          List.concat_map (item_exec_target ~backends) items)
      clerk_targets
  in
  let from_modules =
    List.concat_map
      (fun m ->
        let t = item_exec_target m.Clerk_rules.item in
        if t = [] then
          Message.warning
            "Module @{<cyan>%s@}@ does@ not@ support@ any@ of@ the@ selected@ \
             backends@ and@ was@ ignored."
            (Mark.remove m.name);
        t)
      modules
  in
  let from_directories =
    List.concat_map
      (fun (_, items) ->
        List.concat_map
          (fun it ->
            if Lazy.force it.Scan.has_scope_tests = 0 then []
            else item_exec_target it)
          items)
      directories
  in
  let from_sources = List.concat_map item_exec_target source_files in
  let from_direct_targets =
    List.concat_map
      (fun (str, item, backend) ->
        let t = item_exec_target ~backends:[config_backend backend] item in
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
  runtimes
  @ from_clerk_targets
  @ from_modules
  @ from_directories
  @ from_sources
  @ from_direct_targets

(* Returns the items that should be executed *)
let test_exec_targets
    config
    backends
    items
    info
    { clerk_targets; modules; directories; source_files; direct_targets } :
    (Scan.item * [< `Interpret | `OCaml | `C | `Python | `Java ] * string) list
    =
  let build_dir = config.Cli.options.global.build_dir in
  let item_exec_target ?backends:explicit_backends it =
    let backends =
      match explicit_backends with
      | Some bks -> bks
      | None -> item_backends info backends it
    in
    List.map
      (fun backend ->
        let t = make_target ~build_dir ~backend it in
        ( it,
          backend,
          match backend with
          | `OCaml | `C -> File.(remove_extension t ^ ("+main" -.- extension t))
          | _ -> t ))
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
            if Lazy.force it.Scan.has_scope_tests = 0 then []
            else item_exec_target ~backends it)
          (items_in_subdirs items t.Config.ttests))
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
            if Lazy.force it.Scan.has_scope_tests = 0 then []
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
  from_clerk_targets
  @ from_modules
  @ from_directories
  @ from_sources
  @ from_direct_targets

let set_ninja_targets nin_ppf ninja_targets =
  if ninja_targets = [] then raise Clerk_rules.Stop_ninja
  else Nj.format_def nin_ppf (Nj.Default (Nj.Default.make ninja_targets))

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
  let target_dir = config.Cli.options.global.target_dir in
  let build_dir = config.Cli.options.global.build_dir in
  let local_runtime_dir bk = build_dir / Scan.libcatala / backend_subdir bk in
  if not (List.exists (fun t -> List.mem bk t.Clerk_config.backends) targets)
  then ()
  else
    let bk_dir = target_dir / backend_subdir bk in
    let extensions =
      (* if target.include_objects then List.assoc bk backend_extensions
       * else *)
      B.src_extensions
    in
    let install_runtime_and_stdlib () =
      let dir = bk_dir / Clerk_rules.stdlib_target_name in
      remove dir;
      ensure_dir dir;
      match bk with
      | Clerk_backend.Java.T ->
        List.iter
          (fun subdir ->
            copy_dir ()
              ~filter:(fun f ->
                Filename.check_suffix f ".java"
                (* || (target.include_objects && Filename.check_suffix f ".class") *))
              ~src:(local_runtime_dir bk / subdir)
              ~dst:(dir / subdir))
          ["catala"; "org"]
      | _ ->
        let () =
          match bk with
          (* install runtime *)
          | Clerk_backend.Python.T ->
            copy_dir ()
              ~filter:(fun f ->
                Filename.check_suffix f ".py" && f <> "__init__.py")
              ~src:
                (Lazy.force Poll.stdlib_dir
                / backend_subdir bk
                / "src"
                / "catala")
              ~dst:dir
          | bk ->
            List.iter
              (fun ext ->
                let src =
                  Lazy.force Poll.stdlib_dir
                  / backend_subdir bk
                  / ("catala_runtime" -.- ext)
                in
                if File.exists src then copy_in ~dir ~src)
              extensions
        in
        let target_info =
          String.Map.find Clerk_rules.stdlib_target_name build_info.targets_map
        in
        List.iter
          (fun m ->
            let item = (String.Map.find m build_info.modules_map).item in
            List.iter
              (fun ext ->
                let src_catala_install =
                  item.file_name
                  /../ backend_subdir bk
                  / basename item.file_name
                  -.- ext
                in
                let src_libcatala =
                  build_dir
                  / Scan.libcatala
                  / backend_subdir bk
                  / Scan.target_basename item
                  -.- ext
                in
                if exists src_catala_install then
                  copy_in ~src:src_catala_install ~dir
                else copy_in ~dir ~src:src_libcatala)
              extensions)
          target_info.Config.tmodules
    in
    install_runtime_and_stdlib ();
    let install_target target =
      if
        target.Config.tname = Clerk_rules.stdlib_target_name
        || not (List.mem bk target.backends)
      then ()
      else
        let dir = bk_dir / target.tname in
        Message.debug "Installing target: %s" (B.name / target.tname);
        File.remove dir;
        ensure_dir dir;
        List.iter
          (fun m ->
            let item = (String.Map.find m build_info.modules_map).item in
            let base_src =
              build_dir
              / item.file_name
              /../ backend_subdir bk
              / Scan.target_basename item
            in
            List.iter
              (fun ext -> copy_in ~dir ~src:(base_src -.- ext))
              extensions)
          target.Config.tmodules
    in
    List.iter install_target targets
(* if target.Config.include_sources then
 *   all_modules_deps
 *   |> List.map (fun it -> it.Scan.file_name)
 *   |> List.sort_uniq compare
 *   |> List.iter (fun src -> File.copy_in ~dir:prefix_dir ~src) *)

(* Runs the artifacts generated from the given targets (after linking them using
   the appropriate backend compiler when needed) *)
let run_targets
    ?(whole_program = false)
    ?trace
    ~test
    config
    cmd
    scope
    scope_input
    (test_targets, info) =
  let build_dir = config.Cli.options.global.build_dir in
  let show_progress = (not Global.options.debug) && Unix.isatty Unix.stdout in
  let progress_pfx =
    if test then "Running backend tests..." else "Running compiled targets..."
  in
  let print_status fmt =
    if show_progress then
      Printf.fprintf stdout (fmt ^^ "\r\x1b[?25l%!\x1b[?25h\x1b[K")
    (* Print message, return to beginning of line, flush, then clear line but
       without flushing it yet; the ?25 codes are for hiding and showing back
       the cursor *)
      else Printf.ifprintf stdout fmt
  in
  print_status "%s" progress_pfx;
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
    Re.(
      compile (seq [str "RESULT"; rep1 any; str "executed successfully."; eos]))
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
        (fun (item, backend, _) ->
          backend <> `Interpret && Lazy.force item.Scan.has_scope_tests > 0)
        test_targets
    else test_targets
  in
  let progress = ref 0 in
  let total = List.length test_targets in
  let run_target ((item, backend, target) as test_target) =
    print_status "%s %3d%%" progress_pfx (100 * !progress / total);
    incr progress;
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
        Var.get info.Clerk_rules.var_bindings Var.catala_flags
        @ (match scope with
          | None -> []
          | Some scope -> [Printf.sprintf "--scope=%s" scope])
        @ (match scope_input with
          | None -> []
          | Some input ->
            [
              Printf.sprintf "--input=%s" (Yojson.Safe.to_string ~std:true input);
            ])
        @ if whole_program then ["--whole-program"] else []
      in
      let exec = Var.get info.Clerk_rules.var_bindings Var.catala_exe in
      let cmd = exec @ [cmd; target] @ catala_flags in
      msg target;
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      let code, lines = Clerk_cli.run_command_line ~quiet cmd in
      if code <> 0 && quiet then List.iter print_endline lines;
      test_target, count_success item lines
    | (`C | `OCaml | `Python | `Java) as backend -> (
      let link_cmd = linking_command ~build_dir ~backend ~info in
      let cmd = link_cmd item target in
      if cmd <> [] then (
        msg target;
        Message.debug "Running command: '%s'..." (String.concat " " cmd));
      match Clerk_cli.run_command_line ~quiet cmd with
      | 0, _ ->
        let code, lines =
          run_artifact ~test ~trace config ~backend
            ~var_bindings:info.Clerk_rules.var_bindings ?scope ~quiet target
        in
        if code <> 0 && quiet then List.iter print_endline lines;
        test_target, count_success item lines
      | _, out_lines ->
        if quiet then List.iter print_endline out_lines;
        test_target, (0, count_tests item))
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
      quiet
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
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~quiet ~default:0
        ~ninja_flags:(ninja_flags @ targets) (fun _ _ _ -> 0)
    else (
      Format.eprintf "Available targets:@.";
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~quiet ~default:0
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
      $ Cli.quiet
      $ Cli.targets
      $ Cli.ninja_flags)

let build_cmd : int Cmd.t =
  let run
      config
      autotest
      code_coverage
      quiet
      (target_args : string list)
      backends
      (ninja_flags : string list)
      trace
      trace_format =
    let backends =
      if backends = [] then [`OCaml; `C; `Python; `Java] else backends
    in
    let enabled_backends = backends_to_config backends in
    let targets, info =
      Clerk_rules.run_ninja ~quiet ~code_coverage ~config ~enabled_backends
        ~default:(empty_targets, Clerk_rules.empty_info)
        ~ninja_flags ~autotest:false ~clean_up_env:false ?trace ?trace_format
      @@ fun nin_ppf items info ->
      let targets =
        if target_args = [] then default_targets config
        else
          sort_user_target_args config ~autotest ~backends items info
            target_args
      in
      target_debug_message targets;
      let ninja_targets =
        ninja_build_targets config ~autotest backends items info targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      targets, info
    in
    Message.result "@[<v 4>Build successful@]";
    List.iter
      (install_backend_targets ~config info targets.clerk_targets)
      enabled_backends;
    (* else TODO restore
     *   Message.result
     *     "@[<v 4>Build successful. The targets can be found in the following \
     *      files:@,\
     *      %a%t%a@]"
     *     (Format.pp_print_list (fun ppf (t, f) ->
     *          Format.fprintf ppf "@{<cyan>[%s]@} → @{<cyan>%s@}" t.Config.tname
     *            (make_relative_to ~dir:original_cwd f)))
     *     clerk_targets_result
     *     (fun fmt ->
     *       if clerk_targets_result <> [] && direct_targets <> [] then
     *         Format.pp_print_cut fmt ())
     *     (Format.pp_print_list (fun ppf f ->
     *          Format.fprintf ppf "@{<cyan>%s@}"
     *            (make_relative_to ~dir:original_cwd f)))
     *     direct_targets_result; *)
    raise (Catala_utils.Cli.Exit_with 0)
  in
  let doc =
    "Build command for either $(i,individual files) or $(i,clerk targets)."
  in
  let man =
    [
      `S Manpage.s_description;
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
        "and the resulting file would be in $(b,_build/foo/c/bar.o). When \
         given $(i,clerk targets), that are defined in a $(b,clerk.toml) \
         configuration file, it will build all their required dependencies for \
         all their specified backends along with their source files and copy \
         them over to the $(i,target-dir) (by default $(b,_target)).";
      `P
        "For instance, $(b,clerk build my-target) will generate a directory \
         $(b,target-dir/my-target/c/) that contains all necessary files to \
         export the target as a self contained library. When no arguments are \
         given, $(b,clerk build) will build all the defined $(i,clerk targets) \
         found in the $(b,clerk.toml) or the project's default targets if any.";
    ]
  in
  Cmd.v
    (Cmd.info ~doc ~man "build")
    Term.(
      const run
      $ Cli.init_term ()
      $ Cli.autotest
      $ Cli.code_coverage
      $ Cli.quiet
      $ Cli.clerk_targets_or_files
      $ Cli.backends
      $ Cli.ninja_flags
      $ Catala_utils.Cli.Flags.trace
      $ Catala_utils.Cli.Flags.trace_format)

let run_cmd =
  let run
      config
      (target_args : string list)
      backends
      cmd
      quiet
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
    let targets, items, info =
      Clerk_rules.run_ninja ~quiet ~code_coverage:false ~config
        ~enabled_backends
        ~default:(empty_targets, [], Clerk_rules.empty_info)
        ~ninja_flags ~autotest:false ~clean_up_env:false ?trace ?trace_format
      @@ fun nin_ppf items info ->
      let targets =
        if target_args = [] then default_targets config
        else
          sort_user_target_args config ~autotest:false ~backends items info
            target_args
      in
      target_debug_message targets;
      let ninja_targets =
        ninja_build_targets ~exec_targets:true config ~autotest:false backends
          items info targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      targets, items, info
    in
    target_debug_message targets;
    if prepare_only then (
      Message.result "@[<v 4>Build successful@]";
      Cmd.Exit.ok)
    else
      let exec_targets = test_exec_targets config backends items info targets in
      let results =
        run_targets ~test:false ~whole_program ?trace config cmd scope
          scope_input (exec_targets, info)
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
      $ Cli.quiet
      $ Cli.scope_opt
      $ Cli.scope_input
      $ Cli.ninja_flags
      $ Cli.prepare_only
      $ Cli.whole_program
      $ Catala_utils.Cli.Flags.trace
      $ Catala_utils.Cli.Flags.trace_format)

let typecheck_cmd =
  let retrieve_typecheck_items items files_or_folders =
    (* todo: - allow to specify a clerk target - run in "inplace" mode like json or exceptions commands *)
    let files_or_folders = List.sort_uniq String.compare files_or_folders in
    let open File in
    let invalid_files =
      List.filter (fun f -> not (File.exists f)) files_or_folders
    in
    if invalid_files <> [] then
      Message.error "@[<hov>No source file or directory matching@ %a@ found.@]"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
            (fun fmt f -> fprintf fmt "@{<yellow>%s@}" f))
        invalid_files;
    let included_files =
      List.fold_left
        (fun m { Scan.file_name; included_files; _ } ->
          if included_files <> [] then
            List.fold_left
              (fun m inc_f -> String.Map.add (Mark.remove inc_f) file_name m)
              m included_files
          else m)
        String.Map.empty items
    in
    List.concat_map
      (fun file ->
        let is_dir = try Sys.is_directory file with Sys_error _ -> false in
        let filter item =
          let is_included = String.Map.mem item.Scan.file_name included_files in
          if is_dir then
            let is_prefix =
              String.starts_with ~prefix:(file / "") item.Scan.file_name
            in
            (* Silently skip included file *)
            (not is_included) && is_prefix
          else
            let valid =
              Option.map Mark.remove item.Scan.module_def
              = Some (File.basename file)
              || item.Scan.file_name = file
              || File.remove_extension item.Scan.file_name = file
            in
            if valid && is_included then (
              (* Warn valid included file *)
              Message.warning
                "Skipping file @{<yellow>%s@} included in @{<cyan>%s@}"
                item.Scan.file_name
                (String.Map.find item.Scan.file_name included_files);
              false)
            else valid
        in
        List.filter filter items)
      files_or_folders
  in
  let run
      config
      (files_or_folders : File.t list)
      quiet
      (ninja_flags : string list) =
    let files_or_folders =
      List.map config.Cli.fix_path
      @@ if files_or_folders = [] then [File.original_cwd] else files_or_folders
    in
    let exception Nothing_to_do in
    match
      Clerk_rules.run_ninja ~code_coverage:false ~config ~enabled_backends:[]
        ~autotest:false ~ninja_flags ~quiet ~default:([], [])
        (fun nin_ppf items info ->
          let target_items = retrieve_typecheck_items items files_or_folders in
          if target_items = [] then
            (* Prevents [run_ninja] to fail miserably with an obscure error *)
            raise Nothing_to_do
          else
            let ninja_targets =
              List.map
                (fun it ->
                  match it.Scan.module_def with
                  | Some mdef ->
                    let src = it.file_name in
                    let dir = File.dirname src in
                    if
                      it.is_stdlib
                      || List.mem dir config.options.global.include_dirs
                    then "@src/" ^ String.to_id (Mark.remove mdef)
                    else src
                  | None -> it.file_name)
                target_items
            in
            Nj.format_def nin_ppf (Nj.Default (Nj.Default.make ninja_targets));
            target_items, info.var_bindings)
    with
    | exception Nothing_to_do -> Message.error "Nothing to typecheck."
    | target_items, var_bindings ->
      let catala_flags = Var.get var_bindings Var.catala_flags in
      let exec = Var.get var_bindings Var.catala_exe in
      let ret =
        List.filter_map
          (fun it ->
            if it.Scan.is_stdlib then None
            else
              Option.some
              @@
              let cmd =
                exec
                @ ["typecheck"; "--quiet"]
                @ catala_flags
                @ [it.Scan.file_name]
              in
              Message.debug "Running command: '%s'..." (String.concat " " cmd);
              fst (Clerk_cli.run_command_line cmd))
          target_items
      in
      let ret = List.fold_left max 0 ret in
      if ret = 0 then Message.result "Typechecking successful!";
      ret
  in
  let doc = "Runs the Catala type-checker on the given files." in
  Cmd.v
    (Cmd.info ~doc "typecheck")
    Term.(
      const run
      $ Cli.init_term ()
      $ Cli.files_or_folders
      $ Cli.quiet
      $ Cli.ninja_flags)

let clean_cmd =
  let run (config : Cli.config) =
    File.remove config.Cli.options.Config.global.build_dir;
    File.remove config.Cli.options.Config.global.target_dir;
    raise (Catala_utils.Cli.Exit_with 0)
  in
  let doc =
    "Removes files and directories previously generated by $(i,clerk) if any."
  in
  Cmd.v (Cmd.info ~doc "clean") Term.(const run $ Cli.init_term ())

let test_cmd =
  let run
      config
      quiet
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
    let build_dir = config.Cli.options.global.build_dir in
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
        Message.error
          "Option @{<cyan>--report-format=json@} was specified, but the output \
           of a test report is only@ supported@ with@ the@ default@ \
           @{<yellow>interpret@}@ backend@ at@ the@ moment"
      else if report_format = `VSCodeJSON then
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
    let targets, items, info, test_targets =
      Clerk_rules.run_ninja ~quiet ~code_coverage ~config ~keep_going:true
        ~enabled_backends ~ninja_flags ~clean_up_env:true ~autotest:true
        ~tests:true
        ~default:(empty_targets, [], Clerk_rules.empty_info, [])
      @@ fun nin_ppf items info ->
      (* TODO: keep_going:true, to be able to still show a test report.
         We must not try to run the tests, however, since the artifacts we
         failed to build could remain from a previous run and that would be
         confusing. *)
      let targets =
        if target_args = [] then
          {
            empty_targets with
            directories =
              [
                ( Filename.current_dir_name,
                  items_in_subdirs items [Filename.current_dir_name] );
              ];
          }
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
      let ninja_targets =
        if enable_backend_tests then
          ninja_build_targets ~exec_targets:true config ~autotest:true backends
            items info targets
          @ test_targets
        else test_targets
      in
      set_ninja_targets nin_ppf ninja_targets;
      targets, items, info, test_targets
    in
    let open Clerk_report in
    let test_reports =
      if List.mem `Interpret backends then
        List.concat_map read_many test_targets
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
        let exec_targets =
          test_exec_targets config backends items info targets
        in
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
      $ Cli.quiet
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

let run_ninja_start ~config ~quiet ~ninja_flags ~enabled_backends cont =
  let default =
    List.fold_left
      (fun default_rules (module B : Clerk_backend.S) ->
        ("@" ^ B.name ^ "/runtime/src")
        :: Clerk_backend.module_dep ~name:B.name "Stdlib_fr"
        :: Clerk_backend.module_dep ~name:B.name "Stdlib_en"
        :: default_rules)
      ["@src/Stdlib_fr"; "@src/Stdlib_en"]
      enabled_backends
  in
  Clerk_rules.run_ninja ~include_dir:false ~code_coverage:false ~quiet
    ~default:0 ~config
    ~enabled_backends:(List.map Clerk_backend.id enabled_backends)
    ~autotest:false ~ninja_flags (fun nin_ppf _ _ ->
      Nj.format_def nin_ppf (Nj.Default (Nj.Default.make default));
      cont ())

let start_cmd =
  let run config quiet (ninja_flags : string list) =
    let enabled_backends = target_backends config.Cli.options.targets in
    run_ninja_start ~config ~quiet ~ninja_flags ~enabled_backends (fun () -> 0)
  in
  let doc =
    "This command prepares the local build environment of the project with \
     objects that are needed by Catala, including the runtime and stdlib. It \
     is never needed before running another Clerk command, but may be useful \
     before direct calls to the $(i,catala) compiler."
  in
  Cmd.v (Cmd.info ~doc "start")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.quiet
      $ Cli.ninja_flags)

(* TODO: this should just be an alias to `clerk test -b all` + ensuring all targets installation
let ci_cmd =
  let run
      config
      quiet
      verbosity
      code_coverage
      (report_format : [ `Terminal | `JUnitXML | `VSCodeJSON ])
      (diff_command : string option option) =
    setup_report_format ~fix_path:config.Cli.fix_path verbosity diff_command
      code_coverage;
    let stop_on_failure f =
      try
        let ret = f () in
        match ret with 0 -> () | n -> raise (Catala_utils.Cli.Exit_with n)
      with
      | Catala_utils.Cli.Exit_with 0 -> ()
      | exn -> raise exn
    in
    stop_on_failure (fun () ->
        Message.debug "Running @{<bold>clerk test@} on whole project";
        let root_dir =
          Filename.current_dir_name
          (* Post-[Cli.init], we are expected to be in the project's root dir *)
        in
        run_clerk_test config quiet [root_dir] [`Interpret] false verbosity
          report_format code_coverage diff_command []);
    let clerk_targets = config.Cli.options.targets in
    let enabled_backends =
      (* TODO *) List.map Clerk_backend.id (Clerk_backend.all ())
    in
    if clerk_targets = [] then raise (Catala_utils.Cli.Exit_with 0);
    build_targets ~quiet ~config ~enabled_backends ~ninja_flags:[]
      ~autotest:true ~code_coverage:false
      { clerk_targets; direct_targets = [] };
    (* TODO: what about tests belonging to no target ? *)
    List.iter
      (fun t ->
        List.iter
          (fun bk ->
            stop_on_failure
            @@ fun () ->
            Message.debug
              "Running @{<yellow>%s@} backend tests for @{<cyan>[%s]@} target"
              t.Config.tname
              (string_of_config_backend bk);
            run_clerk_test config quiet [t.tname]
              [config_backend bk]
              false verbosity report_format code_coverage diff_command [])
          t.backends)
      clerk_targets;
    raise (Catala_utils.Cli.Exit_with 0)
  in
  let doc =
    "Scan the project and run all possible actions. This includes the \
     interpretation of all catala tests and CLI tests (equivalent to running \
     the $(i,clerk test) command), and also, the build of all clerk targets \
     (equivalent to running the $(i,clerk build) command) alongside the \
     execution of their tests against all their defined backend. This command \
     is useful for the execution of continuous integrations (CIs) where all \
     build and test actions are often meant to be executed. Run with \
     $(b,--debug) for the full log of events."
  in
  Cmd.v (Cmd.info ~doc "ci")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.quiet
      $ Cli.report_verbosity
      $ Cli.code_coverage
      $ Cli.report_format
      $ Cli.diff_command)
*)

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
    let tests = List.flatten (List.map read_many files) in
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
      Clerk_rules.base_bindings ~autotest:false ~trace:None ~trace_format:None
        ~code_coverage:false
        ~enabled_backends:(List.map snd (Clerk_config.registered_backends ()))
        ~config ~inplace:false
    in
    Format.eprintf "Defined variables:@.";
    Format.open_vbox 0;
    String.Map.iter
      (fun s v ->
        Format.printf "%s=%S@," s
          (String.concat " " (List.assoc v var_bindings)))
      Var.all_vars;
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
      Clerk_rules.base_bindings ~autotest:false ~code_coverage:false ~trace:None
        ~trace_format:None ~enabled_backends:[] ~config ~inplace:true
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
      Clerk_rules.base_bindings ~autotest:false ~code_coverage:false ~trace:None
        ~trace_format:None ~enabled_backends:[] ~config ~inplace:true
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
      (* ci_cmd; *)
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
