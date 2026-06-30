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
module Backend_common = Clerk_backends.Common
module Nj = Ninja_utils

(**{1 Building rules}*)

type backend = (module Clerk_backends.Backend.S)

let all_backends : backend list =
  [
    (module Clerk_backends.Ocaml.Backend);
    (module Clerk_backends.C.Backend);
    (module Clerk_backends.Java.Backend);
    (module Clerk_backends.Python.Backend);
  ]

let backend_from_config = function
  | Clerk_config.OCaml ->
    (module Clerk_backends.Ocaml.Backend : Clerk_backends.Backend.S)
  | Clerk_config.Python ->
    (module Clerk_backends.Python.Backend : Clerk_backends.Backend.S)
  | Clerk_config.C ->
    (module Clerk_backends.C.Backend : Clerk_backends.Backend.S)
  | Clerk_config.Java ->
    (module Clerk_backends.Java.Backend : Clerk_backends.Backend.S)
  | _ -> invalid_arg __FUNCTION__

let base_bindings ~code_coverage ~autotest ~enabled_backends ~inplace ~config =
  let options = config.Clerk_cli.options in
  let test_flags = config.Clerk_cli.test_flags in
  let use_default_flags = test_flags = [] && options.global.catala_opts = [] in
  let default_flags =
    Backend_common.Flags.default ~code_coverage ~inplace ~config
  in
  let backend_flags =
    List.concat_map
      (fun (module Backend : Clerk_backends.Backend.S) ->
        Backend.Flags.default ~variables:options.variables ~autotest
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
            [!clerk_exe; "runtest"; !clerk_flags; !input; "--report"; !output]
          ~description:["<catala>"; "tests"; "⇐"; !input];
        Nj.rule "dir-tests"
          ~command:
            (if Sys.win32 then
               ["cmd"; "/c"; "copy /by >nul"; !cat_files; !output]
             else ["cat"; !input; ">"; !output])
          ~description:["<test>"; !test_id];
      ]
    else []
  in
  let backend_static_rules =
    List.concat_map
      (fun (module Backend : Clerk_backends.Backend.S) ->
        Backend.static_base_rules)
      enabled_backends
  in
  Backend_common.Ninja.static_base_rules @ backend_static_rules @ test_rules

let gen_build_statements
    (include_dirs : string list)
    ~(tests : bool)
    (enabled_backends : (module Clerk_backends.Backend.S) list)
    (autotest : bool)
    (same_dir_modules : (string * File.t) list)
    ~is_stdlib
    (item : Scan.item) : Nj.ninja =
  let open File in
  let ( ! ) = Var.( ! ) in
  let src = item.file_name in
  let dir = dirname src in
  let def_vars =
    [
      Nj.binding Var.src [basename src];
      Nj.binding Var.dst [basename (Scan.target_file_name item)];
    ]
  in
  let modules = List.rev_map Mark.remove item.used_modules in
  let module_target x =
    Ninja.modfile ~backend:"ocaml" same_dir_modules "@ocaml-module" x
  in
  let catala_src = !Var.tdir / !Var.src in
  let include_deps =
    Nj.build "copy"
      ~inputs:[dir / !Var.src]
      ~implicit_in:
        (List.map
           (fun (f, _) ->
             if dir / basename f = f then !Var.tdir / basename f
             else !Var.builddir / f)
           item.included_files
        @ List.map
            (fun m ->
              try !Var.tdir / basename (List.assoc m same_dir_modules)
              with Not_found -> m ^ "@src")
            modules)
      ~outputs:[catala_src]
  in
  let module_deps =
    match item.module_def with
    | None -> []
    | Some _ ->
      List.concat_map
        (fun (module Backend : Clerk_backends.Backend.S) ->
          Backend.expose_module ~same_dir_modules ~used_modules:modules)
        enabled_backends
  in
  let has_scope_tests = Lazy.force item.has_scope_tests in
  let backend_sources =
    if item.extrnal then
      List.map
        (fun (module Backend : Clerk_backends.Backend.S) ->
          Backend.external_copy item)
        enabled_backends
    else
      let inputs = [catala_src] in
      let implicit_in =
        (* autotest requires interpretation at compile-time, which makes use of
           the dependent OCaml modules (cmxs) *)
        !Var.catala_exe
        :: (if autotest then List.map module_target modules else [])
      in
      let vars =
        if is_stdlib then
          Some [Var.catala_flags, [Var.(!catala_flags); "--no-stdlib"]]
        else None
      in
      List.map
        (fun (module Backend : Clerk_backends.Backend.S) ->
          Backend.catala ?vars ~is_stdlib ~inputs ~implicit_in has_scope_tests)
        enabled_backends
  in
  let backend_objects =
    List.map
      (fun (module Backend : Clerk_backends.Backend.S) ->
        Backend.build_object ~include_dirs ~same_dir_modules ~item
          has_scope_tests)
      enabled_backends
  in
  let expose_module =
    (* Note: these rules define global (top-level) aliases for module targets of
       modules that are in include-dirs, so that Ninja can find them from
       wherever; they are only in implicit-in, because once they are built the
       compilers will find them independently through their '-I' arguments.

       This works but it might make things simpler to resolve these aliases at
       the Clerk level ; this would force an initial scan of the included dirs
       but then we could use the already resolved target files directly and get
       rid of these aliases. *)
    match item.module_def with
    | Some m when item.is_stdlib || List.mem dir include_dirs ->
      let modname = Mark.remove m in
      let backends_module =
        List.map
          (fun (module Backend : Clerk_backends.Backend.S) ->
            Nj.build "phony"
              ~outputs:[Format.sprintf "%s@%s-module" modname Backend.name]
              ~inputs:
                [
                  Backend.modfile ~is_stdlib same_dir_modules Backend.module_ext
                    modname;
                ])
          enabled_backends
      in
      Nj.build "phony" ~outputs:[modname ^ "@src"] ~inputs:[catala_src]
      :: backends_module
    | _ -> []
  in
  let tests_rules =
    if not (item.has_inline_tests || Lazy.force item.has_scope_tests) then []
    else
      [
        Nj.build "tests" ~inputs:[catala_src]
          ~implicit_in:
            (!Var.clerk_exe
            :: List.map
                 (Ninja.modfile ~backend:"ocaml" same_dir_modules
                    "@ocaml-module")
                 modules)
          ~outputs:[catala_src ^ "@test"; catala_src ^ "@out"];
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
      List.to_seq expose_module;
      List.to_seq module_deps;
    ]
    @ if tests then [List.to_seq tests_rules] else []
  in
  Seq.concat (List.to_seq (statements_list @ statements_backend))

let gen_build_statements_dir
    ~is_stdlib
    (dir : string)
    (include_dirs : string list)
    ~(tests : bool)
    (enabled_backends : (module Clerk_backends.Backend.S) list)
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
  let ( ! ) = Var.( ! ) in
  Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.comment ("--- " ^ dir ^ " ---"))
  @@ Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.binding Var.tdir [!Var.builddir / dir])
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
               || Lazy.force item.Scan.has_scope_tests)
           then None
           else Some ((Var.(!builddir) / item.Scan.file_name) ^ "@test"))
         items)
  in
  List.to_seq
    [
      Nj.Comment "";
      Nj.build "dir-tests"
        ~outputs:[(Var.(!builddir) / dir) ^ "@test"]
        ~inputs
        ~vars:
          ((Var.test_id, [dir])
          ::
          (if Sys.win32 then
             [Var.cat_files, [String.concat "+" ("nul" :: inputs)]]
           else []));
    ]

let runtime_build_statements ~config enabled_backends =
  let open File in
  let stdbase = Var.(!builddir) / Scan.libcatala in
  let options = config.Clerk_lib.Clerk_cli.options in
  List.concat_map
    (fun (module Backend : Clerk_backends.Backend.S) ->
      Backend.runtime_build_statements ~options ~stdbase)
    enabled_backends

let output_ninja_file_header pp ~config ~tests ~enabled_backends ~var_bindings =
  pp
    (Nj.Comment
       (Printf.sprintf "File generated by Clerk v.%s\n" Catala_utils.Cli.version));
  pp (Nj.Comment "- Global variables - #\n");
  List.iter (fun (var, contents) -> pp (Nj.binding var contents)) var_bindings;
  pp (Nj.Comment "\n- Base rules - #\n");
  List.iter pp (static_base_rules ~tests enabled_backends);
  pp (Nj.Comment "\n- Runtime build statements - #\n");
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
           config.Clerk_cli.options.global.include_dirs enabled_backends
           autotest items;
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
  pp (Nj.Comment "\n- Standard library build statements - #");
  Seq.memoize
  @@ output_ninja_file_item_statements nin_ppf ~config ~tests ~enabled_backends
       ~autotest ~is_stdlib:true stdlib_tree
  @@ Seq.append (fun () ->
      pp (Nj.Comment "\n- Project-specific build statements - #");
      Seq.Nil)
  @@ output_ninja_file_item_statements nin_ppf ~config ~tests ~enabled_backends
       ~autotest ~is_stdlib:false project_tree
  @@ fun () ->
  Seq.Nil

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

let ninja_version =
  lazy
    (try
       File.process_out
         ~check_exit:(function 0 -> () | _ -> raise Exit)
         ninja_exec ["--version"]
       |> String.trim
       |> String.split_on_char '.'
       |> List.map int_of_string
     with Exit | Failure _ -> [])

exception Stop_ninja

let with_ninja_process
    ~config
    ~clean_up_env
    ~ninja_flags
    ~quiet
    ~default
    (callback : Format.formatter -> 'a) =
  let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
  let fname =
    match config.Clerk_cli.ninja_file with
    | Some fname -> Some fname
    | None ->
      if Global.options.debug then
        Some File.(config.options.global.build_dir / "clerk.ninja")
      else None
  in
  let ninja_process nin_file nin_fd =
    let args =
      let ninja_flags =
        (* Newer versions of ninja have a flag to not print "nothing to do", we
           use that if available. *)
        if (not Global.options.debug) && Lazy.force ninja_version >= [1; 12]
        then "--quiet" :: ninja_flags
        else ninja_flags
      in
      ("-f" :: nin_file :: ninja_flags)
      @ if Catala_utils.Global.options.debug then ["-v"] else []
    in
    let cmdline = ninja_exec :: args in
    Message.debug "executing '%s'..." (String.concat " " cmdline);
    let npid =
      let out =
        (* In --quiet, we redirect all ninja's output to /dev/null *)
        if quiet then Unix.openfile Filename.null [O_WRONLY] 0o111
        else Unix.stdout
      in
      Fun.protect
        ~finally:(fun () ->
          if quiet then try Unix.close out with Unix.Unix_error _ -> ())
        (fun () ->
          Unix.create_process_env ninja_exec (Array.of_list cmdline) env nin_fd
            out Unix.stderr)
    in
    let rec wait () =
      match Unix.waitpid [] npid with
      | _, Unix.WEXITED n -> n
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) -> 130
    in
    ( npid,
      fun () ->
        match wait () with 0 -> () | n -> raise (Catala_utils.Cli.Exit_with n) )
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

type module_info = {
  name: string Mark.pos;
  item: Scan.item;
  (* extra_items: Scan.item list; (* e.g. included files *) *)
  targets: String.Set.t;
}

module G = struct[@warning "-32"]
  include Graph.Persistent.Digraph.ConcreteBidirectional(struct
      include String
      let hash = Stdlib.String.hash
    end)

  (* Attributes for Graphviz.Dot *)
  let graph_attributes _ = []
  let default_vertex_attributes _ = [`Fontname "sans"; `Shape `Box; `Style `Filled]
  let vertex_name v = v
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end

let organise_modules ~config items =
  let stdlib_target_name = "stdlib" in
  let module_g, modmap, stdlib_modules =
    Seq.fold_left (fun (mg, modmap, stdlib_modules) item ->
        match item.Scan.module_def with
        | None -> (mg, modmap, stdlib_modules)
        | Some (modname, pos) ->
          let info = {
            name = modname, pos;
            item;
            targets =
              if item.Scan.is_stdlib then String.Set.singleton stdlib_target_name
              else String.Set.empty;
          } in
          let modmap = String.Map.update modname
              (function None -> Some info
                      | Some conflict ->
                        (* Note: until now this was allowed. However, targets
                           select their contents by module name only, so this
                           could only be for local modules ? We could switch to
                           UIDs to support this, or somehow namespace the
                           modules by dir.

                           We need to implement something else than picking
                           randomly, in any case *)
                        Message.error
                          ~pos ~extra_pos:["", Mark.get conflict.name]
                          "Conflicting module name @{<blue>%s@}" modname)
              modmap
          in
          let mg =
            List.fold_left (fun g (m, _mpos) ->
                G.add_edge g modname m
              )
              mg item.Scan.used_modules
          in
          mg, modmap, if item.Scan.is_stdlib then modname :: stdlib_modules else stdlib_modules)
      (G.empty, String.Map.empty, [])
      items
  in
  let stdlib_target = {
    Clerk_config.tname = stdlib_target_name;
    tmodules = stdlib_modules;
    ttests = [];
    backends = [];
    include_sources = false; (* ??? *)
    include_objects = false; (* ??? *)
    dependencies = [];
  } in
  let target_g, modmap, tmap =
    List.fold_left (fun (tg, modmap, tmap) t ->
        let tname = t.Clerk_config.tname in
        let tmap =
          String.Map.update tname (function
              | None -> Some t
              | Some _ ->
                Message.error
                  "Conflicting target name: @{<yellow>%s@} is defined twice" tname)
            tmap
        in
        let modmap =
          List.fold_left (fun modmap m ->
              String.Map.update m
                (function
                  | Some i ->
                    Some { i with targets = String.Set.add tname i.targets }
                  | None -> assert false)
                modmap)
            modmap t.tmodules
        in
        let tg =
          List.fold_left (fun tg dep ->
              if not (List.exists (fun t -> t.Clerk_config.tname = dep)
                        config.Clerk_config.targets)
              then Message.error "Clerk target @{<yellow>%s@}@ lists@ @{<yellow>%s@}@ as@ dependency,@ but@ that@ target@ was@ not@ found." tname dep;
              G.add_edge tg t.Clerk_config.tname dep
            )
            tg t.dependencies
        in
        tg, modmap, tmap
      )
      (G.empty, modmap, String.Map.empty) (stdlib_target :: config.targets)
  in
  (* Todo: add a check that a target's backends is a subset of its depencies' *)
  let print_dot oc =
    let explicit_targets = String.Map.map (fun info -> info.targets) modmap in
    fun modmap ->
      let copy_vertex g v1 v2 =
        let g = G.add_vertex g v2 in
        let g = G.fold_pred (fun w g -> G.add_edge g w v2) g v1 g in
        G.fold_succ (fun w g -> G.add_edge g v2 w) g v1 g
      in
      let g, modmap, explicit = (* Duplicate modules that belong to multiple targets *)
        G.fold_vertex (fun v (g, modmap, explicit) ->
            let info = String.Map.find v modmap in
            if String.Set.cardinal info.targets <= 1 then g, modmap, explicit
            else
            let g, modmap, explicit =
              String.Set.fold (fun target (g, modmap, explicit) ->
                  let vn = v ^ " (" ^ target ^ ")" in
                  copy_vertex g v vn,
                  String.Map.add vn { info with targets = String.Set.singleton target } modmap,
                  if String.Set.mem target (String.Map.find v explicit_targets) then String.Set.add v explicit
                  else explicit)
                info.targets (g, modmap, explicit)
            in
            G.remove_vertex g v,
            modmap,
            explicit)
          module_g (module_g, modmap, String.Set.empty)
      in
      let module Dot = Graph.Graphviz.Dot (struct
          include G
          let get_subgraph v =
            match String.Set.choose_opt (String.Map.find v modmap).targets with
            | None -> None
            | Some target -> Some { Graph.Graphviz.DotAttributes.sg_name = target; sg_attributes = []; sg_parent = None }
          let vertex_attributes v = if String.Set.mem v explicit then [`Fillcolor 0xffff80] else []
        end)
      in
      Dot.output_graph oc g
  in
  let check_cycles label g =
    let module SCC = Graph.Components.Make (G) in
    let sccs = SCC.scc_list g in
    match List.find_opt (function [] | [_] -> false | _ -> true) sccs with
    | None | Some [] -> ()
    | Some (v::vs) ->
      Message.error "@[<v>@[Dependency between the following %s is cyclic:@]@,%a@]" label
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " depends on@,")
           (fun ppf v -> Format.fprintf ppf "@{<yellow>%s@}" v))
        (v :: vs @ [v])
  in
  check_cycles "targets" target_g;
  check_cycles "modules" module_g;
  let module Op = Graph.Oper.P(G) in
  let target_g = Op.transitive_closure target_g in
  let module_g = Op.transitive_closure module_g in
  let leaves g =
    G.fold_vertex (fun t set ->
        if G.out_degree target_g t = 0 then String.Set.add t set else set)
      g String.Set.empty
  in
  let subgraph g set =
    if String.Set.is_empty set then G.empty
    else
      G.fold_vertex (fun v g ->
          if String.Set.mem v set then g else G.remove_vertex g v)
        g g
  in
  let modmap =
    String.Map.fold (fun m info new_modmap ->
        let dependents = G.pred module_g m in
        (* All the targets that effectively depend on m *)
        let targets =
          List.fold_left (fun targets dm ->
              String.Set.union targets (String.Map.find dm modmap).targets)
            info.targets
            dependents
        in
        let dep_target_graph = subgraph target_g targets in
        (* The ones in which m needs to be actually included (the others depend
           on them and will access it that way) *)
        let base_targets = String.Set.union (leaves dep_target_graph) info.targets in
        Message.debug "Module @{<blue>%s@} (%s %a) to be attached to targets %a."
          m (if String.Set.is_empty info.targets then "no explicit target" else "targets")
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf s -> Format.fprintf ppf "@{<yellow>%s@}" s))
          (String.Set.elements info.targets)
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf s -> Format.fprintf ppf "@{<yellow>%s@}" s))
          (String.Set.elements base_targets);
        if String.Set.is_empty base_targets && not (Lazy.force info.item.has_scope_tests) then
          Message.warning "The module @{<blue>%s@} belongs to no target and appears to be unused" m;
        let check_conflicts () =
          let _, conflict_targets =
            (* We allow a module to get attached to multiple targets
               (`base_targets`). However, this creates a conflict between any two
               indepenednt users of the same module. The fix would be to attach
               the given module to a target that is a shared dependency of the
               using targets *)
            String.Set.fold (fun t (seen, conflicts) ->
                let depend_on_t = String.Set.of_list (G.pred target_g t) in
                let clash =
                  leaves (subgraph target_g (String.Set.inter seen depend_on_t))
                in
                String.Set.union seen depend_on_t,
                String.Set.union conflicts clash)
              base_targets (String.Set.empty, String.Set.empty)
          in
          let conflict_err cflt =
            let bases = String.Set.inter (String.Set.of_list (cflt :: G.succ target_g cflt)) base_targets in
            Message.error
              "@[<v>@[<hov>Target conflict error in@ @{<yellow>%s@}:@ module@ @{<blue>%s@}@ would@ be@ included@ multiple@ times.@]@,\
               @[<hov>The following targets require it (either explicitely, or because one of@ their@ modules@ uses@ it):@]@,\
              \    %a@,@,\
               @[<hov>@{<bold>Hint:@} @{<blue>%s@}@ should@ be@ included@ in@ a@ unique@ target@ that@ is@ listed@ in@ the@ @{<cyan>dependencies@}@ field@ of@ all@ targets@ that@ might@ use@ it."
              cflt m
              (Format.pp_print_list (fun ppf t -> Format.fprintf ppf "- @{<yellow>%s@}" t))
              (String.Set.elements bases)
              cflt
          in
          Option.iter conflict_err (String.Set.choose_opt conflict_targets)
        in
        check_conflicts ();
        String.Map.add m { info with targets = base_targets } new_modmap
      )
      modmap modmap
  in
  let tmap =
    String.Map.fold (fun m info tmap ->
        String.Set.fold (fun t tmap ->
            String.Map.update t (function
                | Some target -> Some { target with
                                        Clerk_config.tmodules = m :: target.Clerk_config.tmodules }
                | None -> assert false
              ) tmap)
          info.targets tmap)
      modmap tmap
    |> String.Map.map (fun target ->
        { target with
          Clerk_config.tmodules = List.sort_uniq String.compare target.Clerk_config.tmodules;
          dependencies = G.succ target_g target.tname;
        })
  in
  if Catala_utils.Global.options.debug then
    (let f = File.(config.global.build_dir / "modules.dot") in
     File.with_out_channel f (fun oc -> print_dot oc modmap);
     Message.debug "Module graph available at @{<blue;bold>%a@}"
       (Message.link ~target:(Message.file_url f) ()) f);
  modmap, tmap

type callback_info = {
  var_bindings: (Var.t * string list) list;
  modules_map: module_info String.Map.t;
  targets_map: Clerk_config.target String.Map.t;
}

let empty_info = { var_bindings = []; modules_map = String.Map.empty; targets_map = String.Map.empty }

let run_ninja
    ?(include_dir = true)
    ~config
    ?(tests = false)
    ?(enabled_backends = all_backends)
    ~quiet
    ~default
    ~code_coverage
    ~autotest
    ?(clean_up_env = false)
    ?(ninja_flags = [])
    callback =
  let var_bindings =
    base_bindings ~code_coverage ~config ~enabled_backends ~autotest
      ~inplace:false
  in
  with_ninja_process ~config ~clean_up_env ~ninja_flags ~quiet ~default
    (fun nin_ppf ->
      (* Design note: the idea here is to write the ninja file as a stream while
         the directories are being crawled, with the ninja exec already
         consuming the end of the pipe in parallel. Therefore, refrain from
         forcing the item sequence prematurely. *)
      let insource = Lazy.force Poll.catala_source_tree_root <> None in
      let stdlib_dir = Lazy.force Poll.stdlib_dir in
      let stdlib_tree =
        Scan.tree stdlib_dir
      in
      let item_tree = if include_dir then Scan.tree "." else Seq.empty in
      let item_tree =
        (* Cleanup leftover source files in _build when we scan the
           corresponding directory in the source tree *)
        Seq.map (fun ((f, _, items) as elt) ->
            match
              File.(check_directory (config.options.global.build_dir / f))
            with
            | None -> elt
            | Some dir ->
              let current =
                List.fold_left
                  File.(
                    fun set it -> Set.add (basename it.Scan.file_name) set)
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
                  (Format.pp_print_list ~pp_sep:Format.pp_print_space
                     File.format)
                  (File.Set.elements leftover);
                File.Set.iter (fun f -> Sys.remove File.(dir / f)) leftover);
              elt)
          item_tree
      in
      let item_tree =
        if insource then
          (* Special case for building within the catala compiler source tree *)
          Seq.filter (fun (f, _, _) -> not (String.starts_with ~prefix:"stdlib" f))
            item_tree
        else item_tree
      in
      let item_tree =
        (* Add dependencies towards the proper stdlib *)
        Seq.map (fun (f, fl, items) ->
            let items =
              List.map
                (fun it ->
                   let used_modules =
                     match Scan.get_lang it.Scan.file_name with
                     | Some lg ->
                       let lg =
                         if Global.has_localised_stdlib lg then lg else `En
                       in
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
      let items =
        output_ninja_file nin_ppf ~config ~tests ~enabled_backends ~autotest
          ~var_bindings stdlib_tree item_tree
      in
      let modules_map, targets_map =
        let item_seq = Seq.flat_map (fun (_, _, it) -> List.to_seq it) item_tree in
        organise_modules ~config:config.options item_seq
      in
      let pp nj =
        Nj.format_def nin_ppf nj;
        Format.pp_print_cut nin_ppf ()
      in
      pp (Nj.Comment "\n- User-defined targets - #\n");
      String.Map.iter (fun t target ->
          let modules = target.Clerk_config.tmodules in
          let backends =
            let backend_name (module Bk: Clerk_backends.Backend.S) = Bk.name in
            let conf_backend_name bk =
              backend_name (backend_from_config bk)
            in
            let open String.Set in
            inter
              (of_list (List.map backend_name enabled_backends))
              (of_list (List.map conf_backend_name target.backends))
          in
          (* TODO: warn if backend list empty ? Or is that already caught elsewhere ? *)
          let inputs =
            String.Set.fold (fun bk_name acc ->
                List.fold_left (fun acc m ->
                    (* We could to include Backend.runtime_targets here as well *)
                    Printf.sprintf "%s@%s-module" m bk_name :: acc)
                  modules acc)
              backends
              (List.map (fun t -> "@" ^ t) target.Clerk_config.dependencies)
            |> List.rev
          in
          pp (Nj.build "phony" ~outputs:["@" ^ t] ~inputs))
        targets_map;
      pp (Nj.Comment "\n- Global rules and defaults - #\n");
      if tests then
        pp
          (Nj.build "phony" ~outputs:["test"]
             ~inputs:[File.(Var.(!builddir / ".@test"))]);
      let ret =
        callback nin_ppf (List.of_seq items)
          {
            var_bindings;
            modules_map;
            targets_map;
          }
      in
      Format.pp_print_newline nin_ppf ();
      ret)
