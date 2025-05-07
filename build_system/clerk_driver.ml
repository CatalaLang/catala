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

let ninja_exec = "ninja"

let cleaned_up_env () =
  let passthrough_vars =
    ["CATALA_BIN="; "CATALA_INCLUDE="; "CATALA_TEST_FLAGS="]
  in
  Unix.environment ()
  |> Array.to_seq
  |> Seq.filter (fun s ->
         (not (String.starts_with ~prefix:"CATALA_" s))
         || List.exists
              (fun prefix -> String.starts_with ~prefix s)
              passthrough_vars
         ||
         (Message.warning "Ignoring environment variable %s" s;
          false))
  |> Array.of_seq

let ninja_cmdline =
  let ninja_version =
    lazy
      (try
         File.process_out
           ~check_exit:(function 0 -> () | _ -> raise Exit)
           ninja_exec ["--version"]
         |> String.split_on_char '.'
         |> List.map int_of_string
       with Exit | Failure _ -> [])
  in
  fun ?(quiet = true) ninja_flags nin_file targets ->
    let ninja_flags =
      if quiet && Lazy.force ninja_version >= [1; 12] then
        "--quiet" :: ninja_flags
      else ninja_flags
    in
    (ninja_exec :: "-f" :: nin_file :: ninja_flags)
    @ (if Catala_utils.Global.options.debug then ["-v"] else [])
    @ targets

let lastdirname f = File.(basename (dirname f))

let run_command ~clean_up_env ?(setenv = []) cmdline =
  if cmdline = [] then 0
  else
    let cmd = List.hd cmdline in
    let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
    let env =
      let cut s =
        let i = String.index s '=' in
        String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
      in
      env
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
        | Some m -> String.Map.add m it acc
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
          let it = String.Map.find m modules in
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
          build_dir
          / Filename.dirname it.Scan.file_name
          / "ocaml"
          / Option.get it.Scan.module_def
          ^ ".cmx")
        (link_deps item)
    @ [target -.- "cmx"; "-o"; target -.- "exe"]
  | `C ->
    get_var Var.cc_exe
    @ List.map
        (fun it ->
          build_dir
          / Filename.dirname it.Scan.file_name
          / "c"
          / Option.get it.Scan.module_def
          ^ ".o")
        (link_deps item)
    @ [target -.- "o"]
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
          build_dir
          / Filename.dirname it.Scan.file_name
          / "python"
          / Option.get it.Scan.module_def
          ^ ".py"
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
               build_dir
               / Filename.dirname it.Scan.file_name
               / "java"
               / Option.get it.Scan.module_def
               -.- "class")
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
    | ".java" | ".class" -> Clerk_rules.Java
    | ".catala_en" | ".catala_fr" | ".catala_pl" -> Clerk_rules.OCaml
    | "" -> Message.error "Target without extension: @{<red>%S@}" t
    | ext ->
      Message.error
        "Unhandled extension @{<red;bold>%s@} for target @{<red>%S@}" ext t
  in
  match Filename.extension t with
  | ".exe" -> (
    match lastdirname t with
    | "c" -> Clerk_rules.C
    | "python" -> Clerk_rules.Python
    | "java" -> Clerk_rules.Java
    | "ocaml" | _ -> Clerk_rules.OCaml)
  | ext -> aux ext

let make_target ~build_dir ~backend item =
  let open File in
  let f =
    match item.Scan.module_def with
    | None -> item.Scan.file_name
    | Some m ->
      (dirname item.Scan.file_name / m) ^ Filename.extension item.Scan.file_name
  in
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

let raw_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string list) =
    ninja_init ~enabled_backends:Clerk_rules.all_backends ~extra:Seq.empty
      ~test_flags:[]
    @@ fun ~build_dir:_ ~fix_path ~nin_file ~items:_ ~var_bindings:_ ->
    if targets <> [] then (
      let targets =
        List.map
          (fun f ->
            if String.exists (function '/' | '.' -> true | _ -> false) f then
              fix_path f
            else f)
          targets
      in
      let ninja_cmd = ninja_cmdline ninja_flags nin_file targets in
      Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
      raise
        (Catala_utils.Cli.Exit_with (run_command ~clean_up_env:false ninja_cmd)))
    else
      (* List targets command *)
      let ninja_cmd = ninja_cmdline ninja_flags nin_file ["-t"; "targets"] in
      let result = File.process_out (List.hd ninja_cmd) (List.tl ninja_cmd) in
      let targets =
        String.split_on_char '\n' result
        |> List.filter_map (fun line ->
               match String.split_on_char ':' line with
               | [] | [""] | "always" :: _ -> None
               | target :: _ -> Some (String.trim target))
      in
      Format.eprintf "Available targets:@.";
      List.iter print_endline targets;
      0
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
      $ Cli.Global.term Clerk_rules.ninja_init
      $ Cli.targets
      $ Cli.ninja_flags)

let build_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string list) =
    let enabled_backends =
      targets |> List.map target_backend |> List.sort_uniq compare
    in
    ninja_init ~enabled_backends ~extra:Seq.empty ~test_flags:[]
    @@ fun ~build_dir ~fix_path ~nin_file ~items ~var_bindings ->
    if targets = [] then Message.error "Please specify a target to build";
    let open File in
    let items = List.of_seq items in
    let link_deps = linking_dependencies items in
    let targets =
      List.map
        (fun t ->
          let t = fix_path t in
          if String.starts_with ~prefix:(build_dir ^ Filename.dir_sep) t then t
          else build_dir / t)
        targets
    in
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
          | ".exe" ->
            let t, backend =
              match lastdirname t with
              | "c" -> t, `C
              | "python" -> t, `Python
              | "java" -> t, `Java
              | "ocaml" | _ -> ensure_target_dir "ocaml" t, `OCaml
            in
            let item =
              try
                List.find
                  (fun it ->
                    (dirname (dirname t) / basename t) -.- ""
                    = (build_dir / it.Scan.file_name) -.- "")
                  items
              with Not_found ->
                Message.error "No source to make target %a found" File.format t
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
          List.rev_append targets acc)
        [] exec_targets
      |> List.rev
    in
    let final_exec_targets =
      List.map
        (fun ((item, backend), _) -> make_target ~build_dir ~backend item)
        exec_targets
    in
    let final_ninja_targets =
      List.sort_uniq Stdlib.compare
        (final_exec_targets @ ninja_targets @ deps_targets)
    in
    let ninja_cmd = ninja_cmdline ninja_flags nin_file final_ninja_targets in
    Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
    let exit_code = run_command ~clean_up_env:false ninja_cmd in
    if exit_code <> 0 then raise (Catala_utils.Cli.Exit_with exit_code);
    let link_cmd = linking_command ~build_dir ~var_bindings link_deps in
    let exit_code =
      iter_commands ~build_dir exec_targets
      @@ fun (item, backend) target ->
      let cmd = link_cmd ~backend item target in
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      run_command ~clean_up_env:false cmd
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
      $ Cli.Global.term Clerk_rules.ninja_init
      $ Cli.targets
      $ Cli.ninja_flags)

let set_report_verbosity = function
  | `Summary -> Clerk_report.set_display_flags ~files:`None ~tests:`None ()
  | `Short ->
    Clerk_report.set_display_flags ~files:`Failed ~tests:`Failed ~diffs:false ()
  | `Failures ->
    if Global.options.debug then Clerk_report.set_display_flags ~files:`All ()
  | `Verbose -> Clerk_report.set_display_flags ~files:`All ~tests:`All ()

let run_artifact ~backend ~var_bindings ?scope src =
  let open File in
  match backend with
  | `OCaml ->
    let cmd = (src -.- "exe") :: Option.to_list scope in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command ~clean_up_env:false cmd
  | `C ->
    let cmd =
      (src -.- "exe")
      :: Option.to_list scope (* NOTE: not handled yet by the backend *)
    in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command ~clean_up_env:false cmd
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
    run_command ~clean_up_env:false ~setenv:["PYTHONPATH", pythonpath] cmd
  | `Java ->
    let jars =
      String.concat ":"
        (get_var var_bindings Var.runtime_java_jar @ [src -.- "jar"])
    in
    let target_main = Filename.basename src |> Filename.chop_extension in
    let cmd = get_var var_bindings Var.java @ ["-cp"; jars; target_main] in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command ~clean_up_env:false cmd

let enable_backends = function
  | `Interpret | `OCaml -> [Clerk_rules.OCaml]
  | `C -> [Clerk_rules.C]
  | `Python -> [Clerk_rules.Python]
  | `Java -> [Clerk_rules.Java]

let run_tests
    ~build_dir
    ~fix_path
    ~nin_file
    ~items
    ~var_bindings
    ~ninja_flags
    ~backend
    ~scope
    ~cmd
    files_or_folders =
  let open File in
  let items = List.of_seq items in
  let target_items =
    List.concat_map
      (fun file ->
        let ffile = fix_path file in
        let is_dir = try Sys.is_directory ffile with Sys_error _ -> false in
        let filter item =
          if is_dir then
            String.starts_with ~prefix:(ffile / "") item.Scan.file_name
            && Lazy.force item.Scan.has_scope_tests
          else
            item.Scan.module_def = Some file
            || item.Scan.file_name = ffile
            || Filename.remove_extension item.file_name = ffile
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
  let ninja_cmd =
    let ninja_targets =
      List.fold_left
        (fun acc (it, t) ->
          if String.Set.mem t acc then acc
          else
            String.Set.add t
            @@ List.fold_left
                 (fun acc it ->
                   let backend =
                     match backend with
                     | `Interpret -> `Interpret_module
                     | b ->
                       (b
                         :> [ `C
                            | `Interpret
                            | `Interpret_module
                            | `Java
                            | `OCaml
                            | `Python ])
                   in
                   String.Set.add (make_target ~build_dir ~backend it) acc)
                 acc (link_deps it))
        String.Set.empty base_targets
    in
    ninja_cmdline ninja_flags nin_file (String.Set.elements ninja_targets)
  in
  Message.debug "Building dependencies: '%s'..." (String.concat " " ninja_cmd);
  let exit_code = run_command ~clean_up_env:false ninja_cmd in
  if exit_code <> 0 then raise (Catala_utils.Cli.Exit_with exit_code);
  let exit_code =
    match (backend : [ `Interpret | `C | `OCaml | `Python | `Java ]) with
    | `Interpret ->
      let catala_flags =
        get_var var_bindings Var.catala_flags
        @
        match scope with
        | None -> []
        | Some s -> [Printf.sprintf "--scope=%s" s]
      in
      let exec = get_var var_bindings Var.catala_exe in
      iter_commands ~build_dir base_targets
      @@ fun _item target ->
      let cmd = exec @ [cmd; target] @ catala_flags in
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      run_command ~clean_up_env:false cmd
    | (`C | `OCaml | `Python | `Java) as backend -> (
      let link_cmd =
        linking_command ~build_dir ~backend ~var_bindings link_deps
      in
      iter_commands ~build_dir base_targets
      @@ fun item target ->
      let cmd = link_cmd item target in
      Message.debug "Running command: '%s'..." (String.concat " " cmd);
      match run_command ~clean_up_env:false cmd with
      | 0 -> run_artifact ~backend ~var_bindings ?scope target
      | n -> n)
  in
  exit_code

let run_cmd =
  let run
      ninja_init
      (files_or_folders : File.t list)
      backend
      cmd
      (scope : string option)
      (ninja_flags : string list) =
    let enabled_backends = enable_backends backend in
    let exit_code =
      ninja_init ~enabled_backends ~extra:Seq.empty ~test_flags:[]
        (run_tests ~backend ~ninja_flags ~cmd ~scope files_or_folders)
    in
    raise (Catala_utils.Cli.Exit_with exit_code)
  in
  let doc =
    "Runs the Catala interpreter on the given files, after building their \
     dependencies. The scope to be executed can be specified using the $(i,-s) \
     option."
  in
  Cmd.v (Cmd.info ~doc "run")
    Term.(
      const run
      $ Cli.Global.term Clerk_rules.ninja_init
      $ Cli.files_or_folders
      $ Cli.backend
      $ Cli.run_command
      $ Cli.scope
      $ Cli.ninja_flags)

let test_cmd =
  let run
      ninja_init
      (files_or_folders : string list)
      (backend : [ `Interpret | `OCaml | `C | `Python | `Java ])
      (reset_test_outputs : bool)
      (test_flags : string list)
      verbosity
      xml
      (diff_command : string option option)
      (ninja_flags : string list) =
    set_report_verbosity verbosity;
    if backend <> `Interpret then
      if test_flags <> [] then
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
    ninja_init ~enabled_backends ~extra:Seq.empty ~test_flags
    @@ fun ~build_dir ~fix_path ~nin_file ~items ~var_bindings ->
    Clerk_report.set_display_flags ~diff_command ~fix_path ();
    if backend <> `Interpret then
      let files_or_folders =
        match files_or_folders with
        | [] -> [Filename.current_dir_name]
        | fs -> fs
      in
      let exit_code =
        run_tests ~build_dir ~fix_path ~nin_file ~items ~backend ~ninja_flags
          ~cmd:"unused" ~scope:None ~var_bindings files_or_folders
      in
      raise (Catala_utils.Cli.Exit_with exit_code)
    else
      let targets =
        let fs = if files_or_folders = [] then ["."] else files_or_folders in
        List.map File.(fun f -> build_dir / fix_path f) fs
      in
      let test_targets = List.map (fun f -> f ^ "@test") targets in
      let ninja_cmd = ninja_cmdline ninja_flags nin_file test_targets in
      Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
      match run_command ~clean_up_env:true ninja_cmd with
      | 0 ->
        Message.debug "gathering test results...";
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
      | 1 -> raise (Catala_utils.Cli.Exit_with 10) (* Ninja build failed *)
      | err -> raise (Catala_utils.Cli.Exit_with err)
    (* Other Ninja error ? *)
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
      $ Cli.Global.term (fun ~autotest:_ ->
            Clerk_rules.ninja_init ~autotest:true)
      $ Cli.files_or_folders
      $ Cli.backend
      $ Cli.reset_test_outputs
      $ Cli.test_flags
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
    set_report_verbosity verbosity;
    Clerk_report.set_display_flags ~diff_command ();
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
      $ Cli.Global.color
      $ Cli.Global.debug
      $ Cli.report_verbosity
      $ Cli.report_xml
      $ Cli.diff_command
      $ Cli.build_dir
      $ Cli.files)

let list_vars_cmd =
  let run catala_exe catala_flags build_dir include_dirs =
    let build_dir = Option.value ~default:"_build" build_dir in
    let var_bindings =
      Clerk_rules.base_bindings ~catala_exe ~catala_flags ~build_dir
        ~include_dirs ~autotest:false ()
      |> List.filter_map (function Nj.Binding b -> Some b | _ -> None)
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
     $(i,--var) flag"
    (* TODO: or the clerk.toml file *)
  in
  Cmd.v
    (Cmd.info ~doc "list-vars")
    Term.(
      const run
      $ Cli.catala_exe
      $ Cli.catala_opts
      $ Cli.build_dir
      $ Cli.include_dirs)

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
