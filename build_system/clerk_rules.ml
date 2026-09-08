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

(**{1 Building rules}*)

let base_bindings
    ~code_coverage
    ~trace
    ~autotest
    ~enabled_backends
    ~inplace
    ~config =
  let options = config.Clerk_cli.file in
  let test_flags = config.Clerk_cli.test_flags in
  let use_default_flags = test_flags = [] && options.global.catala_opts = [] in
  let default_flags =
    Clerk_backend.default_flags ~code_coverage ~trace ~inplace ~config
  in
  let backend_flags =
    List.concat_map
      (fun bk ->
        let module Backend : Clerk_backend.S = (val Clerk_backend.get bk) in
        Backend.var_defs ~variables:options.variables ~autotest
          ~use_default_flags ~test_flags
          ~include_dirs:options.global.include_dirs)
      enabled_backends
  in
  default_flags @ backend_flags

let static_base_rules ~tests enabled_backends =
  let open Var in
  let test_rules =
    if tests then
      [
        Nj.rule "tests"
          ~command:
            [
              !!clerk_exe;
              Word "runtest";
              !!clerk_flags;
              !!input;
              Word "--report";
              !!output;
            ]
          ~description:[Word "<catala>"; Word "tests"; Word "⇐"; !!input];
        Nj.rule "dir-tests"
          ~command:
            (if Sys.win32 then
               [
                 Raw "cmd";
                 Raw "/c";
                 Raw "copy";
                 Raw "/by";
                 Raw ">nul";
                 !!cat_files;
                 !!output;
               ]
             else [Word "cat"; !!input; Raw ">"; !!output])
          ~description:[Word "<test>"; !!test_id];
      ]
    else []
  in
  let backend_static_rules =
    List.concat_map
      (fun (module Backend : Clerk_backend.S) -> Backend.rules)
      enabled_backends
  in
  Clerk_backend.static_base_rules @ backend_static_rules @ test_rules

let gen_build_statements
    (include_dirs : string list)
    ~(tests : bool)
    (enabled_backends : (module Clerk_backend.S) list)
    (autotest : bool)
    (same_dir_modules : (string * File.t) list)
    ~is_stdlib
    (item : Scan.item) : Nj.ninja =
  let open File in
  let open Var.Op in
  let src = item.file_name in
  let dir = dirname src in
  let def_vars =
    [
      Nj.binding (Nj.Binding.make Var.src (basename src));
      Nj.binding
        (Nj.Binding.make Var.dst (basename (Scan.target_file_name item)));
    ]
  in
  let modules = List.rev_map Mark.remove item.used_modules in
  let catala_src = Nj.Expr.Word (!Var.tdir / !Var.src) in
  let include_deps =
    Nj.build "copy"
      ~inputs:[Word (dir / !Var.src)]
      ~implicit_in:
        (List.map
           (fun (f, _) ->
             if dir / basename f = f then Nj.Expr.Word (!Var.tdir / basename f)
             else Word (!Var.builddir / f))
           item.included_files
        @ List.map
            (fun m ->
              try
                Nj.Expr.Word
                  (!Var.tdir / basename (List.assoc m same_dir_modules))
              with Not_found -> Nj.Expr.Word ("@catala/src/" ^ String.to_id m))
            modules)
      ~outputs:[catala_src]
  in
  let has_scope_tests = Lazy.force item.has_scope_tests > 0 in
  let backend_sources =
    if item.extrnal then
      List.map
        (fun (module Backend : Clerk_backend.S) -> Backend.external_copy item)
        enabled_backends
    else
      let inputs = [catala_src] in
      let implicit_in =
        (* autotest requires interpretation at compile-time, which makes use of
           the dependent OCaml modules (cmxs) *)
        !!Var.catala_exe
        ::
        (if autotest then List.map Clerk_backend.catala_obj_target modules
         else [])
      in
      let vars =
        if is_stdlib then
          Some
            [
              Nj.Binding.make Var.catala_flags
                [!!Var.catala_flags; Word "--no-stdlib"];
            ]
        else None
      in
      List.map
        (fun (module Backend : Clerk_backend.S) ->
          Backend.catala ?vars ~is_stdlib ~inputs ~implicit_in ~has_scope_tests)
        enabled_backends
  in
  let backend_objects =
    List.map
      (fun (module Backend : Clerk_backend.S) ->
        Backend.build_object ~include_dirs ~same_dir_modules item)
      enabled_backends
  in
  let phony_targets =
    (match item.module_def with
      | Some _ ->
        [
          Nj.build "phony"
            ~outputs:[Word ("@catala/src/" ^ !Var.dst)]
            ~inputs:[catala_src];
        ]
      | None -> [])
    @ List.concat_map
        (fun (module Backend : Clerk_backend.S) ->
          let src_alias =
            match item.module_def with
            | Some _ ->
              [
                Ninja_utils.build "phony"
                  ~inputs:
                    (List.map
                       (Backend.current_target item)
                       Backend.src_extensions)
                  ~outputs:[Word ("@" ^ Backend.name ^ "/src/" ^ !Var.dst)];
              ]
            | None -> []
          in
          let interface_alias =
            match item.module_def with
            | Some _ ->
              [
                Ninja_utils.build "phony"
                  ~inputs:
                    (List.map
                       (Backend.current_target item)
                       Backend.module_extensions)
                  ~implicit_in:
                    (List.map
                       (fun (m, _) -> Backend.interface_dep m)
                       item.used_modules)
                  ~outputs:
                    [Word ("@" ^ Backend.name ^ "/interface/" ^ !Var.dst)];
              ]
            | None -> []
          in
          let obj_alias =
            Ninja_utils.build "phony"
              ~inputs:[Backend.current_target item Backend.obj_extension]
              ~implicit_in:
                (List.map
                   (fun (m, _) ->
                     Nj.Expr.Word ("@" ^ Backend.name ^ "/obj/" ^ String.to_id m))
                   item.used_modules)
              ~outputs:
                [
                  (match item.module_def with
                  | Some _ ->
                    Nj.Expr.Word ("@" ^ Backend.name ^ "/obj/" ^ !Var.dst)
                  | None ->
                    Nj.Expr.Word
                      ("@"
                      ^ Backend.name
                      ^ "/obj/"
                      ^ (dirname item.file_name / !Var.dst)));
                ]
          in
          src_alias @ interface_alias @ [obj_alias])
        enabled_backends
  in
  let tests_rules =
    if not (item.has_inline_tests || Lazy.force item.has_scope_tests > 0) then
      []
    else
      [
        Nj.build "tests" ~inputs:[catala_src]
          ~implicit_in:
            (!!Var.clerk_exe :: List.map Clerk_backend.catala_obj_target modules)
          ~outputs:
            [
              Nj.Expr.Word ((!Var.tdir / !Var.src) ^ "@test");
              Nj.Expr.Word ((!Var.tdir / !Var.src) ^ "@out");
            ];
      ]
  in
  let statements_backend =
    List.map2 Seq.append backend_sources backend_objects
  in
  let statements_list =
    [
      Seq.return (Nj.comment "");
      List.to_seq def_vars;
      Seq.return include_deps;
      List.to_seq phony_targets;
    ]
    @ if tests then [List.to_seq tests_rules] else []
  in
  Seq.concat (List.to_seq (statements_list @ statements_backend))

let gen_build_statements_dir
    ~is_stdlib
    (dir : string)
    (include_dirs : string list)
    ~(tests : bool)
    (enabled_backends : (module Clerk_backend.S) list)
    (autotest : bool)
    (items : Scan.item list) : Nj.ninja =
  let same_dir_modules =
    List.filter_map
      (fun item ->
        Option.map
          (fun name -> Mark.remove name, item.Scan.file_name)
          item.Scan.module_def)
      items
  in
  let check_conflicts seen item =
    let fname = item.Scan.file_name in
    let s = Scan.target_file_name item in
    match String.Map.find_opt s seen with
    | Some f1 ->
      Message.error
        "Conflicting file names:@ %a@ and@ %a@ would both generate the same \
         target file@ %a.@ Please rename one of them."
        File.format (File.basename f1) File.format (File.basename fname)
        File.format (File.basename s)
    | None -> String.Map.add s fname seen
  in
  let _names = List.fold_left check_conflicts String.Map.empty items in
  let dir =
    if Filename.is_relative dir (* Detect stdlib modules *) then dir
    else Scan.libcatala
  in
  let open File in
  let open Var.Op in
  Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.comment ("--- " ^ dir ^ " ---"))
  @@ Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.binding (Nj.Binding.make Var.tdir (!Var.builddir / dir)))
  @@ Seq.flat_map
       (gen_build_statements ~tests ~is_stdlib include_dirs enabled_backends
          autotest same_dir_modules)
       (List.to_seq items)

let dir_test_rules dir subdirs items =
  let open File in
  let subdirs =
    List.filter
      (fun d ->
        Lazy.force Poll.catala_source_tree_root = None
        || not (String.starts_with d ~prefix:"stdlib"))
      subdirs
  in
  let inputs =
    List.rev_append
      (List.rev_map (fun s -> (Var.(!builddir) / s) ^ "@test") subdirs)
      (List.filter_map
         (fun item ->
           if
             not
               (item.Scan.has_inline_tests
               || Lazy.force item.Scan.has_scope_tests > 0)
           then None
           else Some ((Var.(!builddir) / item.Scan.file_name) ^ "@test"))
         items)
  in
  List.to_seq
    [
      Nj.comment "";
      Nj.build "dir-tests"
        ~outputs:[Nj.Expr.Word ((Var.(!builddir) / dir) ^ "@test")]
        ~inputs:(List.map (fun w -> Nj.Expr.Word w) inputs)
        ~vars:
          (Nj.Binding.make Var.test_id dir
          ::
          (if Sys.win32 then
             [Nj.Binding.make Var.cat_files (Var.cmd_concat_operand inputs)]
           else []));
    ]

let runtime_build_statements ~config enabled_backends =
  let open File in
  let stdbase = Var.(!builddir) / Scan.libcatala in
  List.concat_map
    (fun (module Backend : Clerk_backend.S) ->
      Backend.build_runtime ~config ~stdbase)
    enabled_backends

let output_ninja_file_header pp ~config ~tests ~enabled_backends ~var_bindings =
  pp
    (Nj.comment
       (Printf.sprintf "File generated by Clerk v.%s\n" Catala_utils.Cli.version));
  pp (Nj.comment "- Global variables - #\n");
  List.iter (fun b -> pp (Nj.binding b)) var_bindings;
  pp (Nj.comment "\n- Base rules - #\n");
  List.iter pp (static_base_rules ~tests enabled_backends);
  pp (Nj.comment "\n- Runtime build statements - #\n");
  List.iter pp (runtime_build_statements ~config enabled_backends)

let output_ninja_file_item_statements
    nin_ppf
    ~config
    ~tests
    ~enabled_backends
    ~autotest
    ~is_stdlib
    item_tree
    next =
  let rec print_and_get_items seq () =
    match seq () with
    | Seq.Cons ((dir, subdirs, items), seq) ->
      Nj.format nin_ppf
      @@ gen_build_statements_dir dir ~is_stdlib ~tests
           config.Clerk_cli.file.global.include_dirs enabled_backends autotest
           items;
      if (not is_stdlib) && tests then
        Nj.format nin_ppf @@ dir_test_rules dir subdirs items;
      Seq.append (List.to_seq items) (print_and_get_items seq) ()
    | Seq.Nil -> next ()
  in
  print_and_get_items (Seq.once item_tree)

let output_ninja_file
    nin_ppf
    ~config
    ~tests
    ~enabled_backends
    ~autotest
    ~var_bindings
    stdlib_tree
    project_tree =
  let pp nj =
    Nj.format_def nin_ppf nj;
    Format.pp_print_cut nin_ppf ()
  in
  output_ninja_file_header pp ~config ~tests ~enabled_backends ~var_bindings;
  pp (Nj.comment "\n- Standard library build statements - #");
  Seq.memoize
  @@ output_ninja_file_item_statements nin_ppf ~config ~tests ~enabled_backends
       ~autotest ~is_stdlib:true stdlib_tree
  @@ Seq.append (fun () ->
      pp (Nj.comment "\n- Project-specific build statements - #");
      Seq.Nil)
  @@ output_ninja_file_item_statements nin_ppf ~config ~tests ~enabled_backends
       ~autotest ~is_stdlib:false project_tree
  @@ fun () -> Seq.Nil

(** {1 Driver} *)

let cleaned_up_env () =
  let passthrough_vars =
    ["CATALA_BIN="; "CATALA_INCLUDE="; "CATALA_TEST_FLAGS="]
  in
  let ignore_vars = ["CATALA_DEVELOPER="] in
  Unix.environment ()
  |> Array.to_seq
  |> Seq.filter (fun s ->
      (not (String.starts_with ~prefix:"CATALA_" s))
      || List.exists
           (fun prefix -> String.starts_with ~prefix s)
           passthrough_vars
      ||
      (if
         not
           (List.exists
              (fun prefix -> String.starts_with ~prefix s)
              ignore_vars)
       then Message.debug "Ignoring environment variable %s" s;
       false))
  |> Array.of_seq

let ninja_exec = try Sys.getenv "NINJA_BIN" with Not_found -> "ninja"

exception Stop_ninja

let with_ninja_process
    ~config
    ~clean_up_env
    ~ninja_flags
    ~default
    ?(keep_going = false)
    (callback : Format.formatter -> 'a) =
  let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
  let env =
    Array.concat
      [
        env;
        Message.env_forward_vars ();
        [| "NINJA_STATUS=[%f/%t] "; "CLICOLOR_FORCE=1" |];
      ]
  in
  let fname =
    match config.Clerk_cli.ninja_file with
    | Some fname -> Some fname
    | None ->
      if Global.options.debug then
        Some File.(config.file.global.build_dir / "clerk.ninja")
      else None
  in
  let ninja_process nin_file nin_fd =
    let args =
      ("-f" :: nin_file :: ninja_flags)
      @ if Catala_utils.Global.options.debug then ["-v"] else []
    in
    let cmdline = ninja_exec :: args in
    Message.debug "executing '%s'..." (String.concat " " cmdline);
    let nin_out_ic, nin_out_oc = Unix.pipe ~cloexec:true () in
    let npid =
      Fun.protect
        ~finally:(fun () ->
          try Unix.close nin_out_oc with Unix.Unix_error _ -> ())
        (fun () ->
          Unix.create_process_env ninja_exec (Array.of_list cmdline) env nin_fd
            nin_out_oc Unix.stderr)
    in
    let nin_out_ic = Unix.in_channel_of_descr nin_out_ic in
    let rec wait () =
      match Unix.waitpid [] npid with
      | _, Unix.WEXITED n ->
        flush stdout;
        n
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) ->
        flush stdout;
        128 - n
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
        flush stdout;
        130
    in
    let isatty = Unix.isatty Unix.stdout in
    let ninja_count_re =
      Re.(
        compile
          (seq
             [
               bos;
               char '[';
               group (rep1 digit);
               char '/';
               group (rep1 digit);
               char ']';
             ]))
    in
    let rec readwait () =
      match input_line nin_out_ic with
      | exception End_of_file -> wait ()
      | "ninja: no work to do." -> readwait ()
      | line ->
        (if Global.options.debug then print_endline line
         else if isatty then
           match Re.exec_opt ninja_count_re line with
           | None -> print_endline line
           | Some gs ->
             let count = int_of_string (Re.Group.get gs 1) in
             let total = int_of_string (Re.Group.get gs 2) in
             Message.print_percent "Compiling..." count total);
        readwait ()
    in
    ( npid,
      fun () ->
        match readwait () with
        | 0 -> ()
        | n -> if not keep_going then raise (Catala_utils.Cli.Exit_with n) )
  in
  match fname with
  | Some fname -> (
    match File.with_formatter_of_file fname callback with
    | ret ->
      let _, wait = ninja_process fname Unix.stdin in
      wait ();
      ret
    | exception Stop_ninja -> default)
  | None when Sys.os_type = "Win32" ->
    (* ninja requires the name of the file on the cli. No /dev/stdin on
       Windows *)
    File.with_temp_file "clerk_build_" ".ninja" (fun fname ->
        match File.with_formatter_of_file fname callback with
        | ret ->
          let _, wait = ninja_process fname Unix.stdin in
          wait ();
          ret
        | exception Stop_ninja -> default)
  | None -> (
    let ninja_in, clerk_out = Unix.pipe ~cloexec:true () in
    let npid, wait = ninja_process "/dev/stdin" ninja_in in
    Unix.close ninja_in;
    match
      File.with_formatter_of_out_channel
        (Unix.out_channel_of_descr clerk_out)
        callback
    with
    | exception Stop_ninja ->
      Unix.kill npid Sys.sigkill;
      (try wait () with _ -> ());
      default
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Message.debug "Exception caught, killing the ninja sub-process";
      Unix.kill npid Sys.sigkill;
      (try wait () with _ -> ());
      Printexc.raise_with_backtrace e bt
    | callback_ret ->
      Unix.close clerk_out;
      wait ();
      callback_ret)

let scan_stdlib_items () =
  let stdlib_dir = Lazy.force Poll.stdlib_dir in
  Seq.memoize (Scan.tree stdlib_dir)

let scan_project_items ~cleanup ~config =
  let insource = Lazy.force Poll.catala_source_tree_root <> None in
  let item_tree = Scan.tree Filename.current_dir_name in
  let item_tree =
    (* Cleanup leftover source files in _build when we scan the
       corresponding directory in the source tree *)
    if cleanup then
      (* This is a map rather than an iter so that it is performed lazily *)
      Seq.map
        (fun ((f, _, items) as elt) ->
          match
            File.(check_directory (config.Clerk_cli.file.global.build_dir / f))
          with
          | None -> elt
          | Some dir ->
            let current =
              List.fold_left
                File.(fun set it -> Set.add (basename it.Scan.file_name) set)
                File.Set.empty items
            in
            let in_build =
              Sys.readdir dir
              |> Array.to_seq
              |> Seq.filter (fun f -> Scan.get_lang f <> None)
              |> File.Set.of_seq
            in
            let leftover = File.Set.diff in_build current in
            if not (File.Set.is_empty leftover) then (
              Message.debug
                "@[<hov 2>Cleaning up leftover source files in %a:@ %a@]"
                File.format dir
                (Format.pp_print_list ~pp_sep:Format.pp_print_space File.format)
                (File.Set.elements leftover);
              File.Set.iter (fun f -> Sys.remove File.(dir / f)) leftover);
            elt)
        item_tree
    else item_tree
  in
  let item_tree =
    if insource then
      (* Special case for building within the catala compiler source tree *)
      Seq.filter
        (fun (f, _, _) -> not (String.starts_with ~prefix:"stdlib" f))
        item_tree
    else item_tree
  in
  let item_tree =
    (* Add dependencies towards the proper stdlib *)
    Seq.map
      (fun (f, fl, items) ->
        let items =
          List.map
            (fun it ->
              let used_modules =
                match Scan.get_lang it.Scan.file_name with
                | Some lg ->
                  let lg = if Global.has_localised_stdlib lg then lg else `En in
                  ("Stdlib_" ^ Cli.language_code lg, Pos.from_file f)
                  :: it.Scan.used_modules
                | None -> it.Scan.used_modules
              in
              { it with Scan.used_modules })
            items
        in
        f, fl, items)
      item_tree
  in
  Seq.memoize item_tree

let run_ninja
    ?(skip_project_scan = false)
    ~config
    ?(tests = false)
    ?(enabled_backends = List.map snd (Clerk_config.registered_backends ()))
    ~default
    ?keep_going
    ~code_coverage
    ~trace
    ~autotest
    ?(clean_up_env = false)
    ?(ninja_flags = [])
    callback =
  let var_bindings =
    base_bindings ~code_coverage ~trace ~config ~enabled_backends ~autotest
      ~inplace:false
  in
  let known =
    let var_bindings = Var.env_of_bindings var_bindings in
    List.fold_left
      (fun acc (n, _) -> String.Set.add n acc)
      String.Set.empty var_bindings
  in
  List.iter
    (fun (n, _) ->
      if not (String.Set.mem n known) then
        Message.warning
          "Variable @{<blue;bold>$%s@} from the configuration is not used by \
           this invocation"
          n)
    config.Clerk_cli.file.variables;
  let enabled_backends =
    List.map Clerk_backend.get (List.sort_uniq compare enabled_backends)
  in
  with_ninja_process ~config ~clean_up_env ~ninja_flags ~default ?keep_going
    (fun nin_ppf ->
      (* Design note: the idea here is to write the ninja file as a stream while
         the directories are being crawled, with the ninja exec already
         consuming the end of the pipe in parallel. Therefore, refrain from
         forcing the item sequence prematurely. *)
      let stdlib_tree = scan_stdlib_items () in
      let item_tree =
        if skip_project_scan then Seq.empty
        else scan_project_items ~cleanup:true ~config
      in
      let items =
        output_ninja_file nin_ppf ~config ~tests ~enabled_backends ~autotest
          ~var_bindings stdlib_tree item_tree
      in
      let callback_info =
        if skip_project_scan then { Module_graph.empty_info with var_bindings }
        else
          let item_seq =
            Seq.flat_map
              (fun (_, _, it) -> List.to_seq it)
              (Seq.append stdlib_tree item_tree)
          in
          Module_graph.organise_modules ~config ~var_bindings item_seq
      in
      let pp nj =
        Nj.format_def nin_ppf nj;
        Format.pp_print_cut nin_ppf ()
      in
      let items_list = List.of_seq items in
      pp (Nj.comment "\n- User-defined targets - #\n");
      let mk_target backend target =
        Nj.Expr.Word (Printf.sprintf "#%s@%s" target backend)
      in
      String.Map.iter
        (fun t target ->
          let modules = target.Clerk_config.tmodules in
          let conf_backend_name bk = Clerk_backend.(name (get bk)) in
          let backends =
            let open String.Set in
            inter
              (of_list (List.map Clerk_backend.name enabled_backends))
              (of_list (List.map conf_backend_name target.backends))
          in
          String.Set.iter
            (fun bk_name ->
              let inputs =
                List.map (mk_target bk_name) target.Clerk_config.dependencies
              in
              let inputs =
                List.fold_left
                  (fun acc m ->
                    if config.include_objects then
                      Nj.Expr.Word (Printf.sprintf "@%s/obj/%s" bk_name m)
                      :: acc
                    else
                      Nj.Expr.Word (Printf.sprintf "@%s/src/%s" bk_name m)
                      :: acc)
                  inputs modules
              in
              pp (Nj.build "phony" ~outputs:[mk_target bk_name t] ~inputs))
            backends;
          if not (String.Set.is_empty backends) then
            pp
              (Nj.build "phony"
                 ~outputs:[Word ("#" ^ t)]
                 ~inputs:
                   (List.map
                      (fun bk -> mk_target bk t)
                      (String.Set.elements backends))))
        callback_info.targets_map;
      pp (Nj.comment "\n- Global rules and defaults - #\n");
      if tests then
        pp
          (Nj.build "phony" ~outputs:[Word "test"]
             ~inputs:[Nj.Expr.Word File.(Var.(!builddir / ".@test"))]);
      let () =
        (* Check for missing externals *)
        String.Map.iter
          (fun mname m ->
            if m.Module_graph.item.Scan.extrnal then
              let supported =
                Module_graph.module_backends callback_info mname
              in
              let missing =
                List.fold_left
                  (fun missing (module Bk : Clerk_backend.S) ->
                    if List.mem Bk.T supported then
                      List.fold_right
                        (fun ext missing ->
                          let _, missing =
                            Clerk_backend.extern_src
                              ~filename:m.item.Scan.file_name ~name:Bk.name ~ext
                              ~missing
                          in
                          missing)
                        Bk.src_extensions missing
                    else missing)
                  [] enabled_backends
              in
              if missing <> [] then
                let modname, pos = Option.get m.item.Scan.module_def in
                Message.error ~pos
                  "@[<v>@[<hov>Module @{<blue>%s@} is marked as external,@ \
                   but@ the@ following@ files@ are@ missing:@ %a@]@,\
                   @,\
                   @[<hov 2>@{<bold>Hint:@} to generate a template, you can \
                   use:@ @{<magenta>catala %s --gen-external %s@}@]@]"
                  modname
                  (Format.pp_print_list
                     ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
                     File.format)
                  missing mname m.item.Scan.file_name)
          callback_info.modules_map
      in
      let ret = callback nin_ppf items_list callback_info in
      Format.pp_print_newline nin_ppf ();
      ret)

let scan_project ~config =
  let var_bindings =
    base_bindings ~code_coverage:false ~trace:false ~autotest:false
      ~enabled_backends:[] ~inplace:true ~config
  in
  let items =
    scan_stdlib_items ()
    |> Seq.append (scan_project_items ~cleanup:false ~config)
    |> Seq.flat_map (fun (_, _, it) -> List.to_seq it)
  in
  let info = Module_graph.organise_modules ~config ~var_bindings items in
  List.of_seq items, info
