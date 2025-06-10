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
module Nj = Ninja_utils
module Cli = Clerk_cli
module Scan = Clerk_scan
module Var = Clerk_rules.Var

let lastdirname f = File.(basename (dirname f))

let run_command ?(setenv = []) cmdline =
  if cmdline = [] then 0
  else
    let cmd = List.hd cmdline in
    let env =
      let cut s =
        let i = String.index s '=' in
        String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
      in
      Unix.environment ()
      |> Array.to_seq
      |> Seq.map cut
      |> String.Map.of_seq
      |> String.Map.add_seq (List.to_seq setenv)
      |> String.Map.to_seq
      |> Seq.map (fun (var, value) -> var ^ "=" ^ value)
      |> Array.of_seq
    in
    let npid =
      Unix.create_process_env cmd (Array.of_list cmdline) env Unix.stdin
        Unix.stdout Unix.stderr
    in
    let return_code =
      let rec wait () =
        match Unix.waitpid [] npid with
        | _, Unix.WEXITED n -> n
        | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
      in
      wait ()
    in
    return_code

let iter_commands ~build_dir targets f =
  let multi_targets = match targets with [] | [_] -> false | _ -> true in
  List.fold_left
    (fun code (item, target) ->
      if multi_targets then
        Format.fprintf (Message.err_ppf ()) "@{<blue>>@} @{<cyan>%s@}@."
          File.(make_relative_to ~dir:build_dir target -.- "");
      max code (f item target))
    0 targets

let rec get_var =
  (* replaces ${var} with its value, recursively *)
  let re_single_var, re_var =
    let open Re in
    let re = seq [str "${"; group (rep1 (diff any (char '}'))); char '}'] in
    compile (whole_string re), compile re
  in
  fun var_bindings v ->
    let s = List.assoc v var_bindings in
    let get_var = get_var (List.remove_assoc v var_bindings) in
    List.concat_map
      (fun s ->
        match Re.exec_opt re_single_var s with
        | Some g -> get_var (Var.make (Re.Group.get g 1))
        | None ->
          [
            Re.replace ~all:true re_var
              ~f:(fun g ->
                String.concat " " (get_var (Var.make (Re.Group.get g 1))))
              s;
          ])
      s

let linking_dependencies items =
  let modules =
    List.fold_left
      (fun acc it ->
        match it.Scan.module_def with
        | Some m -> String.Map.add (Mark.remove m) it acc
        | None -> acc)
      String.Map.empty items
  in
  let rem_dups =
    let rec aux seen = function
      | it :: r ->
        if String.Set.mem it.Scan.file_name seen then aux seen r
        else it :: aux (String.Set.add it.Scan.file_name seen) r
      | [] -> []
    in
    aux String.Set.empty
  in
  fun item ->
    let rec traverse acc item =
      List.fold_left
        (fun acc m ->
          let it = String.Map.find (Mark.remove m) modules in
          traverse (it :: acc) it)
        acc item.Scan.used_modules
    in
    rem_dups (traverse [] item)

let linking_command ~build_dir ~backend ~var_bindings link_deps item target =
  let open File in
  let get_var = get_var var_bindings in
  match backend with
  | `OCaml ->
    get_var Var.ocamlopt_exe
    @ get_var Var.ocaml_flags
    @ get_var Var.runtime_ocaml_libs
    @ List.map
        (fun it ->
          let f = Scan.target_file_name it in
          (build_dir / dirname f / "ocaml" / basename f) ^ ".cmx")
        (link_deps item)
    @ [
        target -.- "cmx";
        Filename.remove_extension target ^ "+main.cmx";
        "-o";
        target -.- "exe";
      ]
  | `C ->
    get_var Var.cc_exe
    @ List.map
        (fun it ->
          let f = Scan.target_file_name it in
          (build_dir / dirname f / "c" / basename f) ^ ".o")
        (link_deps item)
    @ [target -.- "o"; Filename.remove_extension target ^ "+main.o"]
    @ get_var Var.c_flags
    @ ["-o"; target -.- "exe"]
  | `Python ->
    (* a "linked" python module is a "Module.py" folder containing the module
       .py file along with the runtime and all dependencies, plus a __init__.py
       file *)
    let tdir = Filename.remove_extension target in
    remove tdir;
    ensure_dir tdir;
    List.iter
      (fun it ->
        let src =
          let f = Scan.target_file_name it in
          (build_dir / dirname f / "python" / basename f) ^ ".py"
        in
        copy_in ~src ~dir:tdir)
      (link_deps item);
    copy_in ~src:(target -.- "py") ~dir:tdir;
    close_out (open_out (tdir / "__init__.py"));
    []
  | `Java ->
    let jar_target = target -.- "jar" in
    let classes =
      let class_files =
        target
        :: List.map
             (fun it ->
               let f = Scan.target_file_name it in
               (build_dir / dirname f / "java" / basename f) -.- "class")
             (link_deps item)
      in
      let (h : (string, string list) Hashtbl.t) = Hashtbl.create 5 in
      (* 'javac' generates one file per inner class. Sadly, we do generate a lot
         of those. We need to pack those in the jar as well. *)
      let fetch_inner_classes class_file =
        let basename = Filename.(basename class_file |> chop_extension) in
        let dirname = Filename.dirname class_file in
        let dir_classes =
          Hashtbl.find_opt h dirname
          |> function
          | Some dir_classes -> dir_classes
          | None ->
            let dir_contents = Sys.readdir dirname in
            let dir_classes =
              Seq.filter
                (String.ends_with ~suffix:".class")
                (Array.to_seq dir_contents)
              |> List.of_seq
            in
            Hashtbl.replace h dirname dir_classes;
            dir_classes
        in
        List.filter_map
          (fun clazz ->
            if String.starts_with ~prefix:(basename ^ "$") clazz then
              Some (dirname / clazz)
            else None)
          dir_classes
      in
      List.concat_map
        (fun class_file -> class_file :: fetch_inner_classes class_file)
        class_files
    in
    get_var Var.jar
    @ ["--create"; "--file"; jar_target]
    @ List.concat_map
        (fun clazz -> ["-C"; Filename.dirname clazz; Filename.basename clazz])
        classes

let target_backend t =
  let aux = function
    | ".c" | ".h" -> Clerk_rules.C
    | ".ml" | ".mli" | ".cmi" | ".cmo" | ".cmx" | ".cmxs" -> Clerk_rules.OCaml
    | ".py" -> Clerk_rules.Python
    | ".java" | ".class" | ".jar" -> Clerk_rules.Java
    | ".catala_en" | ".catala_fr" | ".catala_pl" -> Clerk_rules.OCaml
    | "" -> Message.error "Target without extension: @{<red>%S@}" t
    | ext ->
      Message.error
        "Unhandled extension @{<red;bold>%s@} for target @{<red>%S@}" ext t
  in
  match Filename.extension t with
  | ".exe" -> (
    match File.(basename (dirname t)) with
    | "c" -> Clerk_rules.C
    | "python" -> Clerk_rules.Python
    | "java" -> Clerk_rules.Java
    | "ocaml" | _ -> Clerk_rules.OCaml)
  | ext -> aux ext

let make_target ~build_dir ~backend item =
  let open File in
  let f = Scan.target_file_name item ^ Filename.extension item.Scan.file_name in
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
    | `Java -> (dir / "java" / base) -.- "class"
  in
  build_dir / base

open Cmdliner

let raw_cmd : int Cmd.t =
  let run config autotest (targets : string list) (ninja_flags : string list) =
    if targets <> [] then
      let targets =
        List.map
          (fun f ->
            if String.exists (function '/' | '.' -> true | _ -> false) f then
              config.Clerk_cli.fix_path f
            else f)
          targets
      in
      Clerk_rules.run_ninja ~config ~autotest
        ~ninja_flags:(ninja_flags @ targets) (fun _ _ _ -> 0)
    else (
      Format.eprintf "Available targets:@.";
      Clerk_rules.run_ninja ~config ~autotest
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
      $ Cli.targets
      $ Cli.ninja_flags)

let build_cmd : int Cmd.t =
  let run config autotest (targets : string list) (ninja_flags : string list) =
    let open File in
    if targets = [] then Message.error "Please specify a target to build";
    let build_dir = config.Clerk_cli.options.global.build_dir in
    let targets =
      List.map
        (fun t ->
          let t = config.Clerk_cli.fix_path t in
          if String.starts_with ~prefix:(build_dir ^ Filename.dir_sep) t then t
          else build_dir / t)
        targets
    in
    let enabled_backends =
      List.fold_left (fun acc t -> target_backend t :: acc) [] targets
    in
    let ninja_targets, exec_targets, var_bindings, link_deps =
      Clerk_rules.run_ninja ~config ~enabled_backends ~ninja_flags ~autotest
      @@ fun nin_ppf items var_bindings ->
      let link_deps = linking_dependencies items in
      let build_dir = config.Clerk_cli.options.global.build_dir in
      let ensure_target_dir dname t =
        if lastdirname t = dname then t else dirname t / dname / basename t
      in
      let ninja_targets, exec_targets =
        List.partition_map
          (fun t ->
            match Filename.extension t with
            | ".ml" | ".mli" | ".cmi" | ".cmo" | ".cmx" | ".cmxs" ->
              Left (ensure_target_dir "ocaml" t)
            | ".c" | ".h" | ".o" -> Left (ensure_target_dir "c" t)
            | ".py" -> Left (ensure_target_dir "python" t)
            | ".java" | ".class" -> Left (ensure_target_dir "java" t)
            | ".catala_en" | ".catala_fr" | ".catala_pl" -> Left t
            | (".exe" | ".jar") as ext ->
              let t, backend =
                match ext, lastdirname t with
                | ".exe", "c" -> t, `C
                | ".exe", "python" -> t, `Python
                | ".jar", _ -> ensure_target_dir "java" t, `Java
                | ".exe", ("ocaml" | _) -> ensure_target_dir "ocaml" t, `OCaml
                | _ -> assert false
              in
              let item =
                try
                  List.find
                    (fun it ->
                      let item_name =
                        match it.Scan.module_def with
                        | Some m ->
                          File.dirname it.Scan.file_name / Mark.remove m
                        | None -> it.Scan.file_name -.- ""
                      in
                      (dirname (dirname t) / basename t) -.- ""
                      = build_dir / item_name)
                    items
                with Not_found ->
                  Message.error "No source to make target %a found" File.format
                    t
              in
              Right ((item, backend), t)
            | _ -> assert false)
          targets
      in
      let deps_targets =
        List.fold_left
          (fun acc ((item, backend), _) ->
            let deps = link_deps item in
            let targets = List.map (make_target ~build_dir ~backend) deps in
            make_target ~build_dir ~backend item :: List.rev_append targets acc)
          [] exec_targets
        |> List.rev
      in
      let object_exec_targets =
        List.map
          (fun ((item, backend), _) ->
            let t = make_target ~build_dir ~backend item in
            match backend with
            | `Java | `Python -> t
            | _ -> Filename.remove_extension t ^ "+main" ^ Filename.extension t)
          exec_targets
      in
      let final_ninja_targets =
        List.sort_uniq Stdlib.compare
          (object_exec_targets @ deps_targets @ ninja_targets)
      in
      Nj.format_def nin_ppf (Nj.Default (Nj.Default.make final_ninja_targets));
      ninja_targets, exec_targets, var_bindings, link_deps
    in
    let link_cmd = linking_command ~build_dir ~var_bindings link_deps in
    let exit_code =
      iter_commands ~build_dir exec_targets
      @@ fun (item, backend) target ->
      let cmd = link_cmd ~backend item target in
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      run_command cmd
    in
    if exit_code = 0 then
      Message.result
        "@[<v 4>Build successful. The targets can be found in the following \
         files:@,\
         %a@]"
        (Format.pp_print_list (fun ppf f ->
             Format.fprintf ppf "@{<cyan>%s@}"
               (make_relative_to ~dir:original_cwd f)))
        (ninja_targets @ List.map snd exec_targets);
    raise (Catala_utils.Cli.Exit_with exit_code)
  in
  let doc =
    "Base build command for individual file targets. Given the corresponding \
     Catala module is declared, this can be used to build .ml, .cmxs, .c, .py \
     files, etc. Targets, along with their dependencies, are always written \
     into $(i,build-dir) (by default $(b,_build)). If a catala extension is \
     used as target, this compiles all its dependencies.\n\n\
     The format of the targets is $(b,src-dir/BACKEND/file.ext). For example, \
     to build a C object file from $(b,foo/bar.catala_en), one would run:\n\
    \  $(b,clerk build foo/c/bar.o)\n\
     and the resulting file would be in $(b,_build/foo/c/bar.o)."
  in
  Cmd.v (Cmd.info ~doc "build")
    Term.(
      const run
      $ Cli.init_term ()
      $ Cli.autotest
      $ Cli.targets
      $ Cli.ninja_flags)

let setup_report_format ?fix_path verbosity diff_command =
  (match verbosity with
  | `Summary -> Clerk_report.set_display_flags ~files:`None ~tests:`None ()
  | `Short ->
    Clerk_report.set_display_flags ~files:`Failed ~tests:`Failed ~diffs:false ()
  | `Failures ->
    if Catala_utils.Global.options.debug then
      Clerk_report.set_display_flags ~files:`All ()
  | `Verbose -> Clerk_report.set_display_flags ~files:`All ~tests:`All ());
  Clerk_report.set_display_flags ?fix_path ~diff_command ()

let run_artifact ~backend ~var_bindings ?scope src =
  let open File in
  match backend with
  | `OCaml ->
    let cmd = (src -.- "exe") :: Option.to_list scope in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command cmd
  | `C ->
    let cmd =
      (src -.- "exe")
      :: Option.to_list scope (* NOTE: not handled yet by the backend *)
    in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command cmd
  | `Python ->
    let cmd =
      let base = Filename.(basename (remove_extension src)) in
      get_var var_bindings Var.python @ ["-m"; base ^ "." ^ base]
    in
    let pythonpath =
      String.concat ":"
        ((File.dirname src :: get_var var_bindings Var.runtime_python_dir)
        @ [Option.value ~default:"" (Sys.getenv_opt "PYTHONPATH")])
    in
    Message.debug "Executing artifact: 'PYTHONPATH=%s %s'..." pythonpath
      (String.concat " " cmd);
    run_command ~setenv:["PYTHONPATH", pythonpath] cmd
  | `Java ->
    let jars =
      String.concat ":"
        (get_var var_bindings Var.runtime_java_jar @ [src -.- "jar"])
    in
    let target_main = Filename.basename src |> Filename.chop_extension in
    let cmd = get_var var_bindings Var.java @ ["-cp"; jars; target_main] in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command cmd

let enable_backends = function
  | `Interpret | `OCaml -> [Clerk_rules.OCaml]
  | `C -> [Clerk_rules.C]
  | `Python -> [Clerk_rules.Python]
  | `Java -> [Clerk_rules.Java]

let build_test_deps ~config ~backend files_or_folders nin_ppf items var_bindings
    =
  let open File in
  let build_dir = config.Clerk_cli.options.global.build_dir in
  let target_items =
    List.concat_map
      (fun file ->
        let ffile = config.Clerk_cli.fix_path file in
        let is_dir = try Sys.is_directory ffile with Sys_error _ -> false in
        let filter item =
          if is_dir then
            String.starts_with ~prefix:(ffile / "") item.Scan.file_name
            && Lazy.force item.Scan.has_scope_tests
          else
            Option.map Mark.remove item.Scan.module_def = Some file
            || item.Scan.file_name = ffile
            || Filename.remove_extension item.Scan.file_name = ffile
        in
        let items = List.filter filter items in
        if items = [] then
          Message.error "No source file or module matching %a found" format file;
        items)
      files_or_folders
  in
  if target_items = [] then Message.error "Nothing to run";
  let base_targets =
    List.map (fun it -> it, make_target ~build_dir ~backend it) target_items
  in
  let link_deps = linking_dependencies items in
  let ninja_targets =
    let backend =
      match backend with
      | `Interpret -> `Interpret_module
      | (`C | `OCaml | `Python | `Java) as b -> b
    in
    List.fold_left
      (fun acc (it, t) ->
        if String.Set.mem t acc then acc
        else
          let acc =
            if backend = `Interpret_module then String.Set.add t acc
            else
              let t = make_target ~build_dir ~backend it in
              String.Set.add t
              @@ String.Set.add
                   (match backend with
                   | `Java | `Python -> t
                   | _ ->
                     Filename.remove_extension t
                     ^ "+main"
                     ^ Filename.extension t)
                   acc
          in
          List.fold_left
            (fun acc it ->
              String.Set.add (make_target ~build_dir ~backend it) acc)
            acc (link_deps it))
      String.Set.empty base_targets
    |> String.Set.elements
  in
  Nj.format_def nin_ppf (Nj.Default (Nj.Default.make ninja_targets));
  base_targets, link_deps, var_bindings

let run_tests config backend cmd scope (test_targets, link_deps, var_bindings) =
  let build_dir = config.Clerk_cli.options.global.build_dir in
  match (backend : [ `Interpret | `C | `OCaml | `Python | `Java ]) with
  | `Interpret ->
    let catala_flags =
      get_var var_bindings Var.catala_flags
      @
      match scope with None -> [] | Some s -> [Printf.sprintf "--scope=%s" s]
    in
    let exec = get_var var_bindings Var.catala_exe in
    iter_commands ~build_dir test_targets
    @@ fun _item target ->
    let cmd = exec @ [cmd; target] @ catala_flags in
    Message.debug "Running command: '%s'..." (String.concat " " cmd);
    run_command cmd
  | (`C | `OCaml | `Python | `Java) as backend -> (
    let link_cmd =
      linking_command ~build_dir ~backend ~var_bindings link_deps
    in
    iter_commands ~build_dir test_targets
    @@ fun item target ->
    let cmd = link_cmd item target in
    Message.debug "Running command: '%s'..." (String.concat " " cmd);
    match run_command cmd with
    | 0 -> run_artifact ~backend ~var_bindings ?scope target
    | n -> n)

let run_cmd =
  let run
      config
      (files_or_folders : File.t list)
      backend
      cmd
      (scope : string option)
      (ninja_flags : string list) =
    Clerk_rules.run_ninja ~config ~enabled_backends:(enable_backends backend)
      ~ninja_flags ~autotest:false
      (build_test_deps ~config ~backend files_or_folders)
    |> run_tests config backend cmd scope
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
      $ Cli.backend
      $ Cli.run_command
      $ Cli.scope
      $ Cli.ninja_flags)

let test_cmd =
  let run
      config
      (files_or_folders : string list)
      (backend : [ `Interpret | `OCaml | `C | `Python | `Java ])
      (reset_test_outputs : bool)
      verbosity
      xml
      (diff_command : string option option)
      (ninja_flags : string list) =
    let build_dir = config.Clerk_cli.options.global.build_dir in
    setup_report_format ~fix_path:config.Clerk_cli.fix_path verbosity
      diff_command;
    if backend <> `Interpret then
      if config.Clerk_cli.test_flags <> [] then
        Message.error
          "Test flags can only be supplied with the default \
           @{<yellow>interpret@} backend"
      else if reset_test_outputs then
        Message.error
          "@{<bold>--reset@} can only be supplied with the default \
           @{<yellow>interpret@} backend"
      else if xml then
        Message.error
          "Option @{<bold>--xml@} was specified, but the output of a test \
           report is only supported with the default @{<yellow>interpret@} \
           backend at the moment";
    let enabled_backends =
      (if backend = `Interpret then [Clerk_rules.Tests] else [])
      @ enable_backends backend
    in
    if backend <> `Interpret then
      let files_or_folders =
        match files_or_folders with
        | [] -> [Filename.current_dir_name]
        | fs -> fs
      in
      Clerk_rules.run_ninja ~config ~enabled_backends ~ninja_flags
        ~autotest:false ~clean_up_env:true
        (build_test_deps ~config ~backend files_or_folders)
      |> run_tests config backend "" None
    else
      let targets, missing =
        let fs = if files_or_folders = [] then ["."] else files_or_folders in
        List.partition_map
          File.(
            fun f0 ->
              let f = config.Clerk_cli.fix_path f0 in
              if File.exists f then Either.Left (build_dir / f)
              else Either.Right f0)
          fs
      in
      if missing <> [] then
        Message.error "@[<hv 2>Could not find files:@ @[<hov>%a@]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             Format.pp_print_string)
          missing;
      let test_targets = List.map (fun f -> f ^ "@test") targets in
      let _ =
        Clerk_rules.run_ninja ~config ~enabled_backends ~ninja_flags
          ~autotest:false ~clean_up_env:true (fun nin_ppf _items _vars ->
            Nj.format_def nin_ppf (Nj.Default (Nj.Default.make test_targets)))
      in
      let open Clerk_report in
      let reports = List.flatten (List.map read_many test_targets) in
      if reset_test_outputs then
        let () =
          if xml then
            Message.error
              "Options @{<bold>--xml@} and @{<bold>--reset@} are incompatible";
          let ppf = Message.formatter_of_out_channel stdout () in
          match
            List.filter
              (fun f -> List.exists (fun t -> not t.i_success) f.tests)
              reports
          with
          | [] ->
            Format.fprintf ppf
              "[@{<green>DONE@}] All cli tests passed, nothing to reset@."
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
              (List.length need_reset)
        in
        raise (Catala_utils.Cli.Exit_with 0)
      else if (if xml then print_xml else summary) ~build_dir reports then
        raise (Catala_utils.Cli.Exit_with 0)
      else raise (Catala_utils.Cli.Exit_with 1)
  in
  let doc =
    "Scan the given files or directories for catala tests, build their \
     requirement and run them all. If $(b,--backend) is unspecified or \
     $(b,interpret), both scope tests and CLI tests are run ; $(b,--reset) can \
     be used to rewrite the expected results of CLI tests to their current \
     result. For any other $(b,--backend), CLI tests are skipped and scope \
     tests are compiled to the specified backend with the catala option \
     $(b,--autotest), and then run, ensuring the consistency of results."
  in
  Cmd.v (Cmd.info ~doc "test")
    Term.(
      const run
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.files_or_folders
      $ Cli.backend
      $ Cli.reset_test_outputs
      $ Cli.report_verbosity
      $ Cli.report_xml
      $ Cli.diff_command
      $ Cli.ninja_flags)

let runtest_cmd =
  let run catala_exe catala_opts include_dirs test_flags report out file =
    let catala_opts =
      catala_opts
      @ List.fold_right (fun dir opts -> "-I" :: dir :: opts) include_dirs []
    in
    let test_flags = List.filter (( <> ) "") test_flags in
    Clerk_runtest.run_tests
      ~catala_exe:(Option.value ~default:"catala" catala_exe)
      ~catala_opts ~test_flags ~report ~out file;
    0
  in
  let doc =
    "Mainly for internal purposes. Runs cli tests from a Catala file, and \
     outputs their results to stdout"
  in
  Cmd.v (Cmd.info ~doc "runtest")
    Term.(
      const run
      $ Cli.catala_exe
      $ Cli.catala_opts
      $ Cli.include_dirs
      $ Cli.test_flags
      $ Cli.runtest_report
      $ Cli.runtest_out
      $ Cli.single_file)

let report_cmd =
  let run color debug verbosity xml diff_command build_dir files =
    let _options = Catala_utils.Global.enforce_options ~debug ~color () in
    let build_dir = Option.value ~default:"_build" build_dir in
    setup_report_format verbosity diff_command;
    let open Clerk_report in
    let tests = List.flatten (List.map read_many files) in
    let success = (if xml then print_xml else summary) ~build_dir tests in
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
      $ Cli.report_xml
      $ Cli.diff_command
      $ Cli.build_dir
      $ Cli.files)

let list_vars_cmd =
  let run config =
    let var_bindings =
      Clerk_rules.base_bindings ~autotest:false
        ~enabled_backends:Clerk_rules.all_backends ~config
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
     $(i,--var) flag, or in the [variables] section of $(b,clerk.toml)."
  in
  Cmd.v (Cmd.info ~doc "list-vars") Term.(const run $ Cli.init_term ())

let main_cmd =
  Cmd.group Cli.info
    [
      build_cmd;
      test_cmd;
      run_cmd;
      runtest_cmd;
      report_cmd;
      raw_cmd;
      list_vars_cmd;
    ]

let main () =
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Catala_utils.Global.options.debug then
      Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.some_error
  | Message.CompilerErrors contents ->
    List.iter (fun c -> Message.Content.emit c Error) contents;
    exit Cmd.Exit.some_error
  | Sys_error msg ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("System error: " ^ msg))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error
  | e ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("Unexpected error: " ^ Printexc.to_string e))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error
