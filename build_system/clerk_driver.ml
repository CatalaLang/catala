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
module Config = Clerk_config

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

let re_var =
  let open Re in
  seq [str "${"; group (rep1 (diff any (char '}'))); char '}']

let rec get_var =
  (* replaces ${var} with its value, recursively *)
  let re_single_var = Re.(compile (whole_string re_var)) in
  fun var_bindings v ->
    let s =
      try List.assoc v var_bindings
      with Not_found ->
        Message.error
          "Clerk configuration error: variable @{<blue;bold>$%s@} is undefined"
          (Nj.Var.name v)
    in
    let get_var = get_var (List.remove_assoc v var_bindings) in
    List.concat_map
      (fun s ->
        match Re.exec_opt re_single_var s with
        | Some g -> get_var (Var.make (Re.Group.get g 1))
        | None -> [expand_vars var_bindings s])
      s

and expand_vars =
  let re_var = Re.(compile re_var) in
  fun var_bindings s ->
    Re.replace ~all:true re_var
      ~f:(fun g ->
        String.concat " " (get_var var_bindings (Var.make (Re.Group.get g 1))))
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
  let rem_dups l =
    let rec aux seen = function
      | it :: r ->
        if String.Set.mem it.Scan.file_name seen then aux seen r
        else it :: aux (String.Set.add it.Scan.file_name seen) r
      | [] -> []
    in
    aux String.Set.empty l
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

let backend_src_extensions =
  [
    Clerk_rules.C, ["c"; "h"];
    Clerk_rules.OCaml, ["ml"; "mli"];
    Clerk_rules.Python, ["py"];
    Clerk_rules.Java, ["java"];
    Clerk_rules.Tests, ["catala_en"; "catala_fr"; "catala_pl"];
  ]

let backend_obj_extensions =
  [
    Clerk_rules.C, ["o"];
    Clerk_rules.OCaml, ["cmi"; "cmo"; "cmx"; "o"; "cmxs"];
    Clerk_rules.Python, [];
    Clerk_rules.Java, ["class"];
    Clerk_rules.Tests, [];
  ]

let backend_extensions =
  List.map
    (fun (bk, exts) -> bk, exts @ List.assoc bk backend_obj_extensions)
    backend_src_extensions

let extensions_backend =
  ("cmxa", Clerk_rules.OCaml)
  :: List.flatten
       (List.map
          (fun (bk, exts) -> List.map (fun e -> e, bk) exts)
          backend_extensions)

let backend_subdir_list =
  [
    Clerk_rules.C, "c";
    Clerk_rules.Python, "python";
    Clerk_rules.Java, "java";
    Clerk_rules.OCaml, "ocaml";
    Clerk_rules.Tests, "";
  ]

let subdir_backend_list =
  List.map (fun (bk, dir) -> dir, bk) backend_subdir_list

let backend_subdir bk = List.assoc bk backend_subdir_list

let rule_subdir rule =
  backend_subdir (Clerk_rules.backend_from_config rule.Config.backend)

let linking_command ~build_dir ~backend ~var_bindings link_deps item target =
  let open File in
  match backend with
  | `OCaml ->
    get_var var_bindings Var.ocamlopt_exe
    @ List.map (expand_vars var_bindings)
        (Lazy.force Clerk_poll.ocaml_link_flags)
    @ [build_dir / Scan.libcatala / "ocaml" / "dates_calc.cmx"]
    @ [build_dir / Scan.libcatala / "ocaml" / "catala_runtime.cmx"]
    @ get_var var_bindings Var.ocaml_flags
    @ get_var var_bindings Var.ocaml_include
    @ List.map
        (fun it ->
          let f = Scan.target_file_name it in
          (build_dir / dirname f / "ocaml" / basename f) ^ ".cmx")
        (link_deps item)
    @ [
        target -.- "cmx";
        File.remove_extension target ^ "+main.cmx";
        "-o";
        target -.- "exe";
      ]
  | `C ->
    get_var var_bindings Var.cc_exe
    @ [build_dir / Scan.libcatala / "c" / "dates_calc.o"]
    @ [build_dir / Scan.libcatala / "c" / "catala_runtime.o"]
    @ List.map
        (fun it ->
          let f = Scan.target_file_name it in
          (build_dir / dirname f / "c" / basename f) ^ ".o")
        (link_deps item)
    @ ["-lgmp"]
    @ [target -.- "o"; File.remove_extension target ^ "+main.o"]
    @ get_var var_bindings Var.c_flags
    @ get_var var_bindings Var.c_include
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
        let basename = File.(remove_extension (basename class_file)) in
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
    let runtime_class_files =
      File.scan_tree
        (fun f -> if Filename.check_suffix f ".class" then Some f else None)
        (build_dir / Scan.libcatala / "java")
      |> Seq.flat_map (fun (_, _, files) -> List.to_seq files)
      |> List.of_seq
    in
    get_var var_bindings Var.jar
    @ ["--create"; "--file"; jar_target]
    @ List.concat_map
        (fun clazz -> ["-C"; Filename.dirname clazz; Filename.basename clazz])
        classes
    @ runtime_class_files
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
             get_var var_bindings
               (Var.make (String.sub s 1 (String.length s - 1)))
           else [expand_vars var_bindings s])
         rule.Config.commandline

let target_backend config t =
  let aux ext =
    try List.assoc ext extensions_backend
    with Not_found -> (
      if ext = "" then Message.error "Target without extension: @{<red>%S@}" t
      else
        match
          List.find_opt
            (fun rule -> List.mem ext rule.Config.out_exts)
            config.Config.custom_rules
        with
        | Some rule -> Clerk_rules.backend_from_config rule.Config.backend
        | None ->
          Message.error
            "Unhandled extension @{<red;bold>%s@} for target @{<red>%S@}" ext t)
  in
  match File.extension t with
  | "exe" -> (
    try List.assoc File.(basename (dirname t)) subdir_backend_list
    with Not_found -> Clerk_rules.OCaml)
  | ext -> aux ext

let rules_backend = function
  | Clerk_rules.OCaml -> `OCaml
  | Clerk_rules.C -> `C
  | Clerk_rules.Python -> `Python
  | Clerk_rules.Java -> `Java
  | Clerk_rules.Tests -> `Interpret

let string_of_backend = function
  | `OCaml -> "ocaml"
  | `C -> "c"
  | `Python -> "python"
  | `Java -> "java"
  | `Interpret -> "interpret"

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
    | `Java -> (dir / "java" / base) -.- "class"
    | `Custom rule ->
      (dir / rule_subdir rule / base) -.- List.hd rule.Config.in_exts
  in
  build_dir / base

let backend_runtime_targets ?(only_source = false) enabled_backends =
  let src s = if only_source then s ^ "-src" else s in
  (if List.mem Clerk_rules.OCaml enabled_backends then [src "@runtime-ocaml"]
   else [])
  @ (if List.mem Clerk_rules.C enabled_backends then [src "@runtime-c"] else [])
  @ (if List.mem Clerk_rules.Python enabled_backends then ["@runtime-python"]
     else [])
  @
  if List.mem Clerk_rules.Java enabled_backends then [src "@runtime-java"]
  else []

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
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~quiet
        ~ninja_flags:(ninja_flags @ targets) (fun _ _ _ -> 0)
    else (
      Format.eprintf "Available targets:@.";
      Clerk_rules.run_ninja ~code_coverage ~config ~autotest ~quiet
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

let build_clerk_target
    ~(config : Cli.config)
    ~ninja_flags
    ~quiet
    (target : Config.target) =
  Message.debug "Building target @{<cyan>[%s]@}" target.tname;
  let target_dir = config.Cli.options.global.target_dir in
  let build_dir = config.Cli.options.global.build_dir in
  let local_runtime_dir bk =
    File.(build_dir / Scan.libcatala / backend_subdir bk)
  in
  let enabled_backends =
    List.map Clerk_rules.backend_from_config target.backends
    |> List.sort_uniq Stdlib.compare
  in
  let install_targets, all_modules_deps =
    Clerk_rules.run_ninja ~code_coverage:false ~config ~enabled_backends
      ~ninja_flags ~quiet ~autotest:false
    @@ fun nin_ppf items _var_bindings ->
    let find_module_item module_name =
      try
        List.find
          (fun it ->
            match it.Scan.module_def with
            | Some m -> module_name = Mark.remove m
            | None -> false)
          items
      with Not_found ->
        let all_module_names =
          List.filter_map
            (function
              | { Scan.module_def = Some m; _ } -> Some (Mark.remove m)
              | _ -> None)
            items
        in
        Message.error
          "Did not find module @{<yellow>\"%s\"@} used in target \
           @{<cyan>[%s]@}.@ %a"
          module_name target.tname Suggestions.format
          Suggestions.(best_candidates all_module_names module_name)
    in
    let module_items = List.map find_module_item target.tmodules in
    let get_deps = linking_dependencies items in
    let all_modules_deps =
      module_items @ List.concat_map get_deps module_items
    in
    let all_target_files =
      List.concat_map
        (fun bk ->
          List.concat_map
            (fun module_item ->
              let open File in
              let base =
                if module_item.Scan.is_stdlib then
                  local_runtime_dir bk / Scan.target_basename module_item
                else
                  build_dir
                  / module_item.Scan.file_name
                  /../ backend_subdir bk
                  / Scan.target_basename module_item
              in
              let extensions =
                if target.include_objects then List.assoc bk backend_extensions
                else List.assoc bk backend_src_extensions
              in
              List.map
                (fun ext -> (module_item, target, bk), base -.- ext)
                extensions)
            all_modules_deps)
        enabled_backends
      |> List.sort_uniq (fun ((_, _, _), l) ((_, _, _), r) ->
             String.compare l r)
    in
    let all_targets =
      List.fold_left
        (fun acc ((item, tg, backend), _) ->
          let targets =
            let f =
              File.(
                Scan.target_file_name item -.- extension item.Scan.file_name)
            in
            let tf =
              File.(build_dir / dirname f / backend_subdir backend / basename f)
            in
            let exts =
              if tg.Config.include_objects then
                List.assoc backend backend_extensions
              else List.assoc backend backend_src_extensions
            in
            List.map File.(fun ext -> tf -.- ext) exts
          in
          targets @ acc)
        (backend_runtime_targets
           ~only_source:(not target.Config.include_objects)
           enabled_backends)
        all_target_files
      |> List.rev
    in
    let install_targets =
      List.map (fun ((_item, _target, bk), file) -> bk, file) all_target_files
    in
    Nj.format_def nin_ppf (Nj.Default (Nj.Default.make all_targets));
    install_targets, all_modules_deps
  in
  let open File in
  let prefix_dir = target_dir / target.tname in
  List.iter
    (fun (bk, src) ->
      let dir = prefix_dir / backend_subdir bk in
      ensure_dir dir;
      copy_in ~dir ~src)
    install_targets;
  target.Config.backends
  |> List.iter (fun bk ->
         let bk = Clerk_rules.backend_from_config bk in
         let dir = prefix_dir / backend_subdir bk in
         let extensions =
           if target.include_objects then List.assoc bk backend_extensions
           else List.assoc bk backend_src_extensions
         in
         match bk with
         | Clerk_rules.Java ->
           List.iter
             (fun subdir ->
               copy_dir ()
                 ~filter:(fun f ->
                   Filename.check_suffix f ".java"
                   || target.include_objects
                      && Filename.check_suffix f ".class")
                 ~src:(local_runtime_dir bk / subdir)
                 ~dst:(dir / subdir))
             ["catala"; "org"]
         | Clerk_rules.Tests -> assert false
         | bk ->
           List.iter
             (fun ext ->
               let src = (local_runtime_dir bk / "catala_runtime") -.- ext in
               if File.exists src then copy_in ~dir ~src)
             extensions);
  if target.Config.include_sources then
    all_modules_deps
    |> List.map (fun it -> it.Scan.file_name)
    |> List.sort_uniq compare
    |> List.iter (fun src -> File.copy_in ~dir:prefix_dir ~src);
  target, prefix_dir

type targets = { clerk_targets : Config.target list; others : string list }

let classify_targets config (targets : string list) : targets =
  let classify_target t =
    List.find_opt (fun ct -> t = ct.Config.tname) config.Cli.options.targets
    |> function Some t -> Either.Left t | None -> Either.Right t
  in

  let clerk_targets, others = List.partition_map classify_target targets in
  { clerk_targets; others }

let build_direct_targets
    (config : Cli.config)
    ~ninja_flags
    ~autotest
    ~code_coverage
    ~quiet
    direct_targets =
  let open File in
  if direct_targets = [] then []
  else
    let build_dir = config.Cli.options.global.build_dir in
    let direct_targets =
      List.map
        (fun t ->
          let is_module = File.extension t = "" in
          if
            String.starts_with ~prefix:(build_dir ^ Filename.dir_sep) t
            || is_module
          then t
          else File.(build_dir / t))
        direct_targets
    in
    let enabled_backends =
      List.fold_left
        (fun acc t ->
          match File.extension t with
          | "" -> Clerk_rules.OCaml :: acc
          | _ -> target_backend config.options t :: acc)
        [] direct_targets
      |> List.sort_uniq Stdlib.compare
    in
    let ninja_targets, exec_targets, var_bindings, link_deps =
      Clerk_rules.run_ninja ~code_coverage ~config ~enabled_backends ~quiet
        ~ninja_flags ~autotest
      @@ fun nin_ppf items var_bindings ->
      let link_deps = linking_dependencies items in
      let build_dir = config.Cli.options.global.build_dir in
      let ensure_target_dir dname t =
        if lastdirname t = dname then t else dirname t / dname / basename t
      in
      let ninja_targets, exec_targets =
        let find_item t =
          try
            List.find
              (fun it ->
                (dirname (dirname t) / basename t) -.- ""
                = build_dir / Scan.target_file_name it)
              items
          with Not_found ->
            Message.error "No source to make target %a found" File.format t
        in
        List.partition_map
          (fun t ->
            let ext = File.extension t in
            let is_module = ext = "" in
            match List.assoc_opt ext extensions_backend, ext with
            | Some bk, _ -> Left (ensure_target_dir (backend_subdir bk) t)
            | None, ("catala_en" | "catala_fr" | "catala_pl") -> Left t
            | None, ("exe" | "jar") ->
              let t, backend =
                match ext, lastdirname t with
                | "exe", "c" -> t, `C
                | "exe", "python" -> t, `Python
                | "jar", _ -> ensure_target_dir "java" t, `Java
                | "exe", ("ocaml" | _) -> ensure_target_dir "ocaml" t, `OCaml
                | _ -> assert false
              in
              Right ((find_item t, backend), t)
            | None, ext -> (
              match
                List.find_opt
                  (fun rule -> List.mem ext rule.Config.out_exts)
                  config.options.custom_rules
              with
              | Some rule ->
                let tdir = rule_subdir rule in
                let t = ensure_target_dir tdir t in
                Right ((find_item t, `Custom rule), t)
              | None when is_module -> begin
                let is_toplevel_module =
                  File.dirname t = Filename.current_dir_name
                in
                let in_same_dir x = File.dirname x = File.dirname t in
                let found_l, not_included_l =
                  List.filter_map
                    (function
                      | { Scan.module_def = Some m; file_name; _ } ->
                        if Mark.remove m = File.basename t then
                          let is_in_included_dirs =
                            List.exists
                              (fun d -> File.dirname file_name = d)
                              config.options.Config.global.include_dirs
                          in
                          match is_toplevel_module, is_in_included_dirs with
                          | true, true -> Some (`Found t)
                          | true, false -> Some (`Not_included file_name)
                          | false, true ->
                            if in_same_dir file_name then
                              Some (`Found (File.basename t))
                            else None
                          | false, _ ->
                            Some
                              (`Found
                                File.(
                                  build_dir / dirname t / "ocaml" / basename t))
                        else None
                      | _ -> None)
                    items
                  |> List.partition_map (function
                       | `Found x -> Either.Left x
                       | `Not_included x -> Right x)
                in
                let found_l =
                  let in_same_dir_l = List.find_all in_same_dir found_l in
                  match in_same_dir_l with
                  | [] -> found_l
                  | _ :: _ -> in_same_dir_l
                in
                match found_l, not_included_l with
                | [t], _ -> Left (t ^ "@ocaml-module")
                | _ :: _ :: _, _ ->
                  Message.error
                    "Found multiple files that satisfy the module %s: %a. @\n\
                     Fix the include_dirs or change the module names." t
                    Format.(
                      pp_print_list
                        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                        pp_print_string)
                    found_l
                | [], fn :: _ ->
                  Message.error
                    "Module found in %s however it was not declared in \
                     'include_dirs'.@\n\
                     Try 'clerk build %s' instead or add the '%s' directory to \
                     'include_dirs'."
                    fn (File.dirname fn) (File.dirname fn)
                | [], [] ->
                  if is_toplevel_module then
                    Message.error "No module %s found in the clerk project." t
                  else
                    Message.error
                      "No file found that declares a module %s for the \
                       provided path."
                      (File.basename t)
              end
              | None -> Message.error "Unknown target %s" t))
          direct_targets
      in
      let object_exec_targets =
        List.map
          (fun ((item, backend), _) ->
            let t = make_target ~build_dir ~backend item in
            match backend with
            | `Java | `Python | `Custom _ -> t
            | _ -> File.((remove_extension t ^ "+main") -.- File.extension t))
          exec_targets
      in
      let final_ninja_targets =
        List.sort_uniq Stdlib.compare (object_exec_targets @ ninja_targets)
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
    if exit_code <> 0 then raise (Catala_utils.Cli.Exit_with exit_code);
    ninja_targets

let build_cmd : int Cmd.t =
  let run
      config
      autotest
      code_coverage
      quiet
      (clerk_targets_or_files : string list)
      (ninja_flags : string list) =
    let open File in
    let { clerk_targets; others = direct_targets } =
      classify_targets config clerk_targets_or_files
    in
    let clerk_targets, direct_targets =
      match clerk_targets_or_files with
      | _ :: _ -> clerk_targets, direct_targets
      | [] -> (
        match config.Cli.options.global.default_targets with
        | _ :: _ as tl ->
          Message.debug "Building default targets:@ %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Format.pp_print_string)
            tl;
          let { clerk_targets; others } =
            classify_targets config config.Cli.options.global.default_targets
          in
          if others <> [] then
            Message.error "Unknown default targets:@ %a"
              (Format.pp_print_list ~pp_sep:Format.pp_print_space
                 Format.pp_print_string)
              others;
          clerk_targets, []
        | [] -> (
          match
            List.map (fun t -> t.Config.tname) config.Cli.options.targets
          with
          | _ :: _ as tl ->
            Message.debug "Building all targets:@ %a"
              (Format.pp_print_list ~pp_sep:Format.pp_print_space
                 Format.pp_print_string)
              tl;
            config.Cli.options.targets, []
          | [] ->
            Message.error
              "Please specify a target to build, or set 'default_targets' in \
               @{<cyan>clerk.toml@}"))
    in
    (* Building Clerk named targets separately *)
    let clerk_targets_result =
      List.map
        (fun t -> build_clerk_target ~quiet ~config ~ninja_flags t)
        clerk_targets
    in
    let direct_targets_result =
      build_direct_targets config ~code_coverage ~quiet ~ninja_flags ~autotest
        direct_targets
      |> List.filter (fun s -> not (String.contains s '@'))
    in
    if clerk_targets_result = [] && direct_targets_result = [] then
      Message.result "@[<v 4>Build successful@]"
    else
      Message.result
        "@[<v 4>Build successful. The targets can be found in the following \
         files:@,\
         %a%t%a@]"
        (Format.pp_print_list (fun ppf (t, f) ->
             Format.fprintf ppf "@{<cyan>[%s]@} → @{<cyan>%s@}" t.Config.tname
               (make_relative_to ~dir:original_cwd f)))
        clerk_targets_result
        (fun fmt ->
          if clerk_targets_result <> [] && direct_targets <> [] then
            Format.pp_print_cut fmt ())
        (Format.pp_print_list (fun ppf f ->
             Format.fprintf ppf "@{<cyan>%s@}"
               (make_relative_to ~dir:original_cwd f)))
        direct_targets_result;
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
      $ Cli.ninja_flags)

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

let run_artifact config ~backend ~var_bindings ?scope src =
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
    let build_dir = config.Cli.options.global.build_dir in
    let cmd =
      let base = Filename.basename (File.remove_extension src) in
      get_var var_bindings Var.python @ ["-m"; base ^ "." ^ base]
    in
    let pythonpath =
      String.concat ":"
        [
          build_dir / Scan.libcatala / "python";
          File.dirname src;
          Option.value ~default:"" (Sys.getenv_opt "PYTHONPATH");
        ]
    in
    Message.debug "Executing artifact: 'PYTHONPATH=%s %s'..." pythonpath
      (String.concat " " cmd);
    run_command ~setenv:["PYTHONPATH", pythonpath] cmd
  | `Java ->
    let target_main = File.remove_extension (Filename.basename src) in
    let cmd =
      get_var var_bindings Var.java @ ["-cp"; src -.- "jar"; target_main]
    in
    Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
    run_command cmd

let enable_backend =
  let open Clerk_rules in
  function
  | `Interpret | `OCaml -> OCaml | `C -> C | `Python -> Python | `Java -> Java

let build_test_deps ~config ~backend files_or_folders nin_ppf items var_bindings
    =
  let open File in
  let build_dir = config.Cli.options.global.build_dir in
  let target_items =
    List.concat_map
      (fun file ->
        let is_dir = try Sys.is_directory file with Sys_error _ -> false in
        let filter item =
          if is_dir then
            String.starts_with ~prefix:(file / "") item.Scan.file_name
            && Lazy.force item.Scan.has_scope_tests
          else
            Option.map Mark.remove item.Scan.module_def
            = Some (File.basename file)
            || item.Scan.file_name = file
            || File.remove_extension item.Scan.file_name = file
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
  let runtime_targets = backend_runtime_targets [enable_backend backend] in
  let ninja_targets =
    let backend =
      match backend with
      | `Interpret -> `Interpret_module
      | (`OCaml | `C | `Python | `Java) as bk -> bk
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
                   | _ -> File.(remove_extension t ^ ("+main" -.- extension t)))
                   acc
          in
          List.fold_left
            (fun acc it ->
              String.Set.add (make_target ~build_dir ~backend it) acc)
            acc (link_deps it))
      (String.Set.of_list runtime_targets)
      base_targets
    |> String.Set.elements
  in
  Nj.format_def nin_ppf (Nj.Default (Nj.Default.make ninja_targets));
  base_targets, link_deps, var_bindings

let run_tests
    ?(whole_program = false)
    config
    backend
    cmd
    scope
    (test_targets, link_deps, var_bindings) =
  let build_dir = config.Cli.options.global.build_dir in
  match (backend : [ `Interpret | `C | `OCaml | `Python | `Java ]) with
  | `Interpret ->
    let catala_flags =
      get_var var_bindings Var.catala_flags
      @ (match scope with
        | None -> []
        | Some s -> [Printf.sprintf "--scope=%s" s])
      @ if whole_program then ["--whole-program"] else []
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
    | 0 -> run_artifact config ~backend ~var_bindings ?scope target
    | n -> n)

let run_cmd =
  let run
      config
      (files_or_folders : File.t list)
      backend
      cmd
      quiet
      (scope : string option)
      (ninja_flags : string list)
      prepare_only
      whole_program =
    let files_or_folders = List.map config.Cli.fix_path files_or_folders in
    Clerk_rules.run_ninja ~config ~code_coverage:false
      ~enabled_backends:[enable_backend backend]
      ~ninja_flags ~autotest:false ~quiet
      (build_test_deps ~config ~backend files_or_folders)
    |> fun tests ->
    if prepare_only then Cmd.Exit.ok
    else run_tests ~whole_program config backend cmd scope tests
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
      $ Cli.quiet
      $ Cli.scope
      $ Cli.ninja_flags
      $ Cli.prepare_only
      $ Cli.whole_program)

let typecheck_cmd =
  let run
      config
      quiet
      (files_or_folders : File.t list)
      (ninja_flags : string list) =
    let files_or_folders = List.map config.Cli.fix_path files_or_folders in
    let items, var_bindings =
      Clerk_rules.run_ninja ~code_coverage:false ~config
        ~enabled_backends:[Clerk_rules.Tests] ~autotest:false ~ninja_flags
        ~quiet (fun nin_ppf items var_bindings ->
          Nj.format_def nin_ppf
            (Nj.Default (Nj.Default.make ["Stdlib_fr@src"; "Stdlib_en@src"]));
          items, var_bindings)
    in
    let catala_flags = get_var var_bindings Var.catala_flags in
    let exec = get_var var_bindings Var.catala_exe in
    let files =
      List.filter_map
        (fun it ->
          if
            Filename.is_relative it.Scan.file_name
            && (files_or_folders = []
               || List.exists
                    (fun f ->
                      it.Scan.file_name = f
                      || String.starts_with
                           ~prefix:File.(f / "")
                           it.Scan.file_name)
                    files_or_folders)
          then Some it.Scan.file_name
          else None)
        items
    in
    if files = [] then Message.error "No matching files found";
    let ret =
      List.map
        (fun f ->
          let cmd = exec @ ["typecheck"; "--quiet"] @ catala_flags @ [f] in
          Message.debug "Running command: '%s'..." (String.concat " " cmd);
          run_command cmd)
        files
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
      $ Cli.quiet
      $ Cli.files_or_folders
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

let check_clerk_targets_tests backend clerk_targets =
  (* Check targets specific backend support *)
  let enabled_backend = enable_backend backend in
  let pp_target_list fmt ts =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
        (fun fmt t -> fprintf fmt "@{<cyan>[%s]@}" t.Config.tname))
      fmt ts
  in
  (if backend = `Interpret then ()
   else
     List.filter
       (fun t ->
         List.map Clerk_rules.backend_from_config t.Config.backends
         |> List.mem enabled_backend
         |> not)
       clerk_targets
     |> function
     | [] -> ()
     | t ->
       Message.error
         "Backend @{<bold>%s@} is not supported by the following target(s): %a"
         (string_of_backend backend)
         pp_target_list t);
  (* Check that targets have tests *)
  List.filter (fun t -> t.Config.ttests = []) clerk_targets
  |> function
  | [] -> ()
  | [t] ->
    Message.error "Target %a has no @{<bold>tests@} attached" pp_target_list [t]
  | ts ->
    Message.error "Targets { %a } have no @{<bold>tests@} attached"
      pp_target_list ts

let run_clerk_test
    config
    quiet
    (clerk_targets_or_files_or_folders : string list)
    (backend : [ `Interpret | `OCaml | `C | `Python | `Java ])
    (reset_test_outputs : bool)
    verbosity
    (report_format : [ `Terminal | `JUnitXML | `VSCodeJSON ])
    code_coverage
    (diff_command : string option option)
    (ninja_flags : string list) : int =
  let build_dir = config.Cli.options.global.build_dir in
  setup_report_format ~fix_path:config.Cli.fix_path verbosity diff_command
    code_coverage;
  if backend <> `Interpret then
    if config.Cli.test_flags <> [] then
      Message.error
        "Test flags can only be supplied with the default \
         @{<yellow>interpret@} backend"
    else if reset_test_outputs then
      Message.error
        "@{<bold>--reset@} can only be supplied with the default \
         @{<yellow>interpret@} backend"
    else if report_format = `JUnitXML then
      Message.error
        "Option @{<bold>--report-format=json@} was specified, but the output \
         of a test report is only supported with the default \
         @{<yellow>interpret@} backend at the moment"
    else if report_format = `VSCodeJSON then
      Message.error
        "Option @{<bold>--report-format=vscode@} was specified, but the output \
         of a test report is only supported with the default \
         @{<yellow>interpret@} backend at the moment"
    else if code_coverage then
      Message.error
        "Option @{<bold>--code-coverage@} was specified, but the measure of \
         code coverage is only supported with the default \
         @{<yellow>interpret@} backend. Please use a backend-specific coverage \
         tool instead.";
  let { clerk_targets; others = files_or_folders } =
    classify_targets config clerk_targets_or_files_or_folders
  in
  let () = check_clerk_targets_tests backend clerk_targets in
  let files_or_folders =
    List.concat_map
      (fun t -> List.map File.clean_path t.Config.ttests)
      clerk_targets
    @ List.map config.Cli.fix_path files_or_folders
    |> List.sort_uniq String.compare
  in
  let enabled_backends =
    enable_backend backend
    :: (if backend = `Interpret then [Clerk_rules.Tests] else [])
  in
  if backend <> `Interpret then
    let files_or_folders =
      match files_or_folders with [] -> [Filename.current_dir_name] | fs -> fs
    in
    Clerk_rules.run_ninja ~quiet ~code_coverage ~config ~enabled_backends
      ~ninja_flags ~autotest:true ~clean_up_env:true
      (build_test_deps ~config ~backend files_or_folders)
    |> run_tests config backend "" None
  else
    let targets, missing =
      let fs =
        if files_or_folders = [] then [Filename.current_dir_name]
        else files_or_folders
      in
      List.partition_map
        File.(
          fun f ->
            if File.exists f then Either.Left (build_dir / f)
            else Either.Right f)
        fs
    in
    if missing <> [] then
      Message.error "@[<hv 2>Could not find files:@ @[<hov>%a@]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           Format.pp_print_string)
        missing;
    let test_targets =
      Clerk_rules.run_ninja ~code_coverage ~config ~enabled_backends
        ~ninja_flags ~autotest:false ~quiet ~clean_up_env:true
        (fun nin_ppf _items _vars ->
          (* FIXME: remove and warn about files that have no @tests rule *)
          let test_targets = List.map (fun f -> f ^ "@test") targets in
          Nj.format_def nin_ppf (Nj.Default (Nj.Default.make test_targets));
          test_targets)
    in
    let open Clerk_report in
    let reports = List.flatten (List.map read_many test_targets) in
    if reset_test_outputs then
      let () =
        if report_format = `JUnitXML then
          Message.error
            "Options @{<bold>--report-format=xml@} and @{<bold>--reset@} are \
             incompatible";
        if report_format = `VSCodeJSON then
          Message.error
            "Options @{<bold>--report-format=json@} and @{<bold>--reset@} are \
             incompatible";
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
    else if
      (match report_format with
      | `JUnitXML -> print_xml
      | `Terminal -> summary
      | `VSCodeJSON -> print_json)
        ~build_dir reports
    then raise (Catala_utils.Cli.Exit_with 0)
    else raise (Catala_utils.Cli.Exit_with 1)

let test_cmd =
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
      const run_clerk_test
      $ Cli.init_term ~allow_test_flags:true ()
      $ Cli.quiet
      $ Cli.clerk_targets_or_files_or_folders
      $ Cli.backend
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

let start_cmd =
  let run config quiet (ninja_flags : string list) =
    Clerk_rules.run_ninja ~code_coverage:false ~quiet ~config
      ~enabled_backends:[OCaml] ~autotest:false ~ninja_flags (fun nin_ppf _ _ ->
        Nj.format_def nin_ppf
          (Nj.Default
             (Nj.Default.make
                [
                  "@runtime-ocaml";
                  "Stdlib_fr@ocaml-module";
                  "Stdlib_en@ocaml-module";
                ]));
        0)
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
        run_clerk_test config quiet [root_dir] `Interpret false verbosity
          report_format code_coverage diff_command []);
    let targets = config.Cli.options.targets in
    if targets = [] then raise (Catala_utils.Cli.Exit_with 0);
    List.iter
      (fun t ->
        let _ = build_clerk_target ~quiet ~config ~ninja_flags:[] t in
        List.iter
          (fun bk ->
            let bk_rule = rules_backend (Clerk_rules.backend_from_config bk) in
            stop_on_failure
            @@ fun () ->
            Message.debug
              "Running @{<yellow>%s@} backend tests for @{<cyan>[%s]@} target"
              t.tname
              (string_of_backend bk_rule);
            run_clerk_test config quiet [t.tname] bk_rule false verbosity
              report_format code_coverage diff_command [])
          t.backends)
      targets;
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
      | `Terminal -> summary
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
      Clerk_rules.base_bindings ~autotest:false ~code_coverage:false
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
      typecheck_cmd;
      start_cmd;
      clean_cmd;
      ci_cmd;
      runtest_cmd;
      report_cmd;
      raw_cmd;
      list_vars_cmd;
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
