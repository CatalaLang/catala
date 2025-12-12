(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
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
open Shared_ast

(** Associates a file extension with its corresponding
    {!type: Global.backend_lang} string representation. *)
let extensions =
  [
    ".catala_fr", "fr";
    ".catala_fr.md", "fr";
    ".catala_en", "en";
    ".catala_en.md", "en";
    ".catala_pl", "pl";
    ".catala_pl.md", "pl";
  ]

let load_modules
    options
    includes
    ~stdlib
    ?(more_includes = [])
    ?(allow_notmodules = false)
    program :
    ModuleName.t Ident.Map.t
    * (Surface.Ast.module_content * ModuleName.t Ident.Map.t) ModuleName.Map.t =
  let stdlib_root_module lang =
    let lang = if Global.has_localised_stdlib lang then lang else Global.En in
    "Stdlib_" ^ Cli.language_code lang
  in
  if stdlib <> None || program.Surface.Ast.program_used_modules <> [] then
    Message.debug "Loading module interfaces...";
  (* Recurse into program modules, looking up files in [using] and loading
     them *)
  let stdlib_includes =
    match stdlib with
    | Some dir -> File.Tree.build (options.Global.path_rewrite dir)
    | None -> File.Tree.empty
  in
  let stdlib_use file =
    let pos = Pos.from_info file 0 0 0 0 in
    let lang = Cli.file_lang file in
    {
      Surface.Ast.mod_use_name = stdlib_root_module lang, pos;
      Surface.Ast.mod_use_alias = "Stdlib", pos;
    }
  in
  let includes =
    List.map options.Global.path_rewrite includes @ more_includes
    |> List.map File.Tree.build
    |> List.fold_left File.Tree.union File.Tree.empty
  in
  let err_req_pos chain =
    List.map (fun mpos -> "Module required from", mpos) chain
  in
  let find_module in_stdlib req_chain (mname, mpos) =
    let required_from_file = Pos.get_file mpos in
    let includes =
      if in_stdlib then stdlib_includes
      else
        File.Tree.union includes
          (File.Tree.build (File.dirname required_from_file))
    in
    match
      List.filter_map
        (fun (ext, _) -> File.Tree.lookup includes (mname ^ ext))
        extensions
    with
    | [] ->
      if in_stdlib then
        Message.error
          "@[<v>@[<hov>The standard library module @{<magenta>%s@}@ could@ \
           not@ be@ found@ at@ %a.@]@,\
           @,\
           @[<hov>@{<bold>Hint:@} run command '@{<cyan>clerk start@}' first to \
           setup the@ standard@ library@ in@ the@ current@ project.@ In@ \
           general,@ prefer@ building@ with@ @{<cyan>clerk@}@ rather@ than@ \
           running@ @{<cyan>catala@}@ directly.@]@]"
          mname File.format
          (options.Global.path_rewrite (Option.get stdlib))
      else
        Message.error
          ~extra_pos:(err_req_pos (mpos :: req_chain))
          "Required module not found: @{<blue>%s@}" mname
    | [f] -> f
    | ms ->
      Message.error
        ~extra_pos:(err_req_pos (mpos :: req_chain))
        "Required module @{<blue>%s@} matches multiple files:@;<1 2>%a" mname
        (Format.pp_print_list ~pp_sep:Format.pp_print_space File.format)
        ms
  in
  let rec aux is_stdlib req_chain seen uses :
      (ModuleName.t * Surface.Ast.module_content * ModuleName.t Ident.Map.t)
      option
      File.Map.t
      * ModuleName.t Ident.Map.t =
    List.fold_left
      (fun (seen, use_map) use ->
        let f = find_module is_stdlib req_chain use.Surface.Ast.mod_use_name in
        match File.Map.find_opt f seen with
        | Some (Some (modname, _, _)) ->
          ( seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map )
        | Some None ->
          Message.error
            ~extra_pos:
              (err_req_pos (Mark.get use.Surface.Ast.mod_use_name :: req_chain))
            "Circular module dependency"
        | None ->
          let default_module_name =
            if allow_notmodules then
              (* This preserves the filename capitalisation, which corresponds
                 to the convention for files related to not-module compilation
                 artifacts and is used by [depends] below *)
              Some (Filename.basename (File.remove_extension f))
            else None
          in
          let module_content =
            if options.Global.whole_program then
              Surface.Parser_driver.load_interface_and_code ?default_module_name
                (Global.FileName f)
            else
              Surface.Parser_driver.load_interface ?default_module_name
                (Global.FileName f)
          in
          let modname =
            ModuleName.fresh
              module_content.Surface.Ast.module_modname.module_name
          in
          let seen = File.Map.add f None seen in
          let seen, file_use_map =
            aux is_stdlib
              (Mark.get use.Surface.Ast.mod_use_name :: req_chain)
              seen module_content.Surface.Ast.module_submodules
          in
          ( File.Map.add f (Some (modname, module_content, file_use_map)) seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map ))
      (seen, Ident.Map.empty) uses
  in
  let file =
    match program.Surface.Ast.program_module with
    | Some m -> Pos.get_file (Mark.get m.module_name)
    | None -> List.hd program.Surface.Ast.program_source_files
  in
  let modules_map file_map =
    File.Map.fold
      (fun _ info acc ->
        match info with
        | None -> acc
        | Some (mname, intf, use_map) ->
          ModuleName.Map.add mname (intf, use_map) acc)
      file_map ModuleName.Map.empty
  in
  let stdlib_files, stdlib_uses =
    if stdlib = None then File.Map.empty, Ident.Map.empty
    else
      let stdlib_files, stdlib_use_map =
        aux true [Pos.from_info file 0 0 0 0] File.Map.empty [stdlib_use file]
      in
      let stdlib_modules = modules_map stdlib_files in
      let _, (_, stdlib_uses) =
        (* Uses from the stdlib are "flattened" to the parent module, but can
           still be overriden *)
        ModuleName.Map.choose stdlib_modules
      in
      ( stdlib_files,
        Ident.Map.union (fun _ _ m -> Some m) stdlib_uses stdlib_use_map )
  in
  let file_module_map, root_uses =
    aux false [] stdlib_files program.Surface.Ast.program_used_modules
  in
  let file_module_map =
    File.Map.mapi
      (fun file ->
        Option.map (fun (mname, intf, use_map) ->
            ( mname,
              intf,
              if
                File.Map.mem file stdlib_files
                || intf.Surface.Ast.module_modname.module_external
              then use_map
              else Ident.Map.union (fun _ _ m -> Some m) stdlib_uses use_map )))
      file_module_map
  in
  ( Ident.Map.union (fun _ _ m -> Some m) stdlib_uses root_uses,
    modules_map file_module_map )

module Passes = struct
  (* Each pass takes only its cli options, then calls upon its dependent passes
     (forwarding their options as needed) *)

  let debug_pass_name s =
    Message.debug "@{<bold;magenta>=@} @{<bold>%s@} @{<bold;magenta>=@}"
      (String.uppercase_ascii s)

  let surface options : Surface.Ast.program =
    debug_pass_name "surface";
    Surface.Parser_driver.parse_top_level_file options.Global.input_src

  let desugared ?allow_external options ~includes ~stdlib :
      Desugared.Ast.program * Desugared.Name_resolution.context =
    let prg = surface options in
    let mod_uses, modules = load_modules options includes ~stdlib prg in
    debug_pass_name "desugared";
    Message.debug "Name resolution...";
    let ctx = Desugared.Name_resolution.form_context (prg, mod_uses) modules in
    Message.report_delayed_errors_if_any ();
    Message.debug "Desugaring...";
    let modules = ModuleName.Map.map fst modules in
    let prg =
      Desugared.From_surface.translate_program ctx ?allow_external modules prg
    in
    Message.report_delayed_errors_if_any ();
    Message.debug "Disambiguating...";
    let prg = Desugared.Disambiguate.program prg in
    Message.report_delayed_errors_if_any ();
    Message.debug "Linting...";
    Desugared.Linting.lint_program prg;
    prg, ctx

  let scopelang ?allow_external options ~includes ~stdlib :
      untyped Scopelang.Ast.program =
    let prg, _ = desugared options ?allow_external ~includes ~stdlib in
    debug_pass_name "scopelang";
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let prg =
      Scopelang.From_desugared.translate_program prg exceptions_graphs
    in
    Message.report_delayed_errors_if_any ();
    prg

  let dcalc :
      type ty.
      Global.options ->
      includes:Global.raw_file list ->
      stdlib:Global.raw_file option ->
      optimize:bool ->
      check_invariants:bool ->
      autotest:bool ->
      typed:ty mark ->
      ty Dcalc.Ast.program * TypeIdent.t list =
   fun options ~includes ~stdlib ~optimize ~check_invariants ~autotest ~typed ->
    let prg = scopelang options ~includes ~stdlib in
    debug_pass_name "dcalc";
    let type_ordering =
      Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
        prg.program_ctx.ctx_enums
    in
    let (prg : ty Scopelang.Ast.program) =
      match typed with
      | Typed _ ->
        Message.debug "Typechecking...";
        Scopelang.Ast.type_program prg
      | Untyped _ -> prg
      | Custom _ -> invalid_arg "Driver.Passes.dcalc"
    in
    Message.report_delayed_errors_if_any ();
    Message.debug "Translating to default calculus...";
    let prg = Dcalc.From_scopelang.translate_program prg in
    let prg =
      if autotest then (
        Interpreter.load_runtime_modules
          ~hashf:Hash.(finalise ~monomorphize_types:false)
          prg;
        Dcalc.Autotest.program prg)
      else prg
    in
    let prg =
      if optimize then begin
        Message.debug "Optimizing default calculus...";
        Optimizations.optimize_program prg
      end
      else prg
    in
    let (prg : ty Dcalc.Ast.program) =
      match typed with
      | Typed _ ->
        Message.debug "Typechecking again...";
        Typing.program ~internal_check:true prg
      | Untyped _ -> prg
      | Custom _ -> assert false
    in
    Message.report_delayed_errors_if_any ();
    if check_invariants then (
      Message.debug "Checking invariants...";
      match typed with
      | Typed _ ->
        if Dcalc.Invariants.check_all_invariants prg then
          Message.result "All invariant checks passed"
        else
          raise
            (Message.error ~internal:true "Some Dcalc invariants are invalid")
      | _ -> Message.error "--check-invariants cannot be used with --no-typing");
    prg, type_ordering

  let lcalc
      (type ty)
      options
      ~includes
      ~stdlib
      ~optimize
      ~check_invariants
      ~autotest
      ~(typed : ty mark)
      ~closure_conversion
      ~keep_special_ops
      ~monomorphize_types
      ~expand_ops
      ~renaming :
      typed Lcalc.Ast.program * TypeIdent.t list * Renaming.context option =
    let prg, type_ordering =
      dcalc options ~includes ~stdlib ~optimize ~check_invariants ~autotest
        ~typed
    in
    debug_pass_name "lcalc";
    let prg =
      match typed with
      | Untyped _ -> Lcalc.From_dcalc.translate_program prg
      | Typed _ -> Lcalc.From_dcalc.translate_program prg
      | Custom _ -> invalid_arg "Driver.Passes.lcalc"
    in
    let prg =
      if expand_ops then (
        Message.debug "Expanding polymorphic operators...";
        Lcalc.Expand_op.program prg)
      else prg
    in
    let prg =
      if optimize then begin
        Message.debug "Optimizing lambda calculus...";
        Optimizations.optimize_program prg
      end
      else prg
    in
    let prg =
      if not closure_conversion then (
        Message.debug "Retyping lambda calculus...";
        Typing.program ~internal_check:true prg)
      else (
        Message.debug "Performing closure conversion...";
        let prg =
          Lcalc.Closure_conversion.closure_conversion ~keep_special_ops prg
        in
        let prg =
          if optimize then (
            Message.debug "Optimizing lambda calculus...";
            Optimizations.optimize_program prg)
          else prg
        in
        Message.debug "Retyping lambda calculus...";
        Typing.program ~internal_check:true ~assume_op_types:true prg)
    in
    let prg, type_ordering =
      if monomorphize_types then (
        Message.debug "Monomorphizing types...";
        let prg, type_ordering = Lcalc.Monomorphize.program prg in
        Message.debug "Retyping lambda calculus...";
        let prg =
          Typing.program ~assume_op_types:true ~internal_check:true prg
        in
        prg, type_ordering)
      else prg, type_ordering
    in
    Message.report_delayed_errors_if_any ();
    match renaming with
    | None -> prg, type_ordering, None
    | Some renaming ->
      Message.debug "Renaming idents...";
      let prg, ren_ctx = Renaming.apply renaming prg in
      let type_ordering =
        let open TypeIdent in
        List.map
          (function
            | Struct s -> Struct (Renaming.struct_name ren_ctx s)
            | Enum e -> Enum (Renaming.enum_name ren_ctx e))
          type_ordering
      in
      prg, type_ordering, Some ren_ctx

  let scalc
      options
      ~includes
      ~stdlib
      ~optimize
      ~check_invariants
      ~autotest
      ~closure_conversion
      ~keep_special_ops
      ~dead_value_assignment
      ~no_struct_literals
      ~keep_module_names
      ~monomorphize_types
      ~expand_ops
      ~renaming : Scalc.Ast.program * TypeIdent.t list * Renaming.context =
    let prg, type_ordering, renaming_context =
      lcalc options ~includes ~stdlib ~optimize ~check_invariants ~autotest
        ~typed:Expr.typed ~closure_conversion ~keep_special_ops
        ~monomorphize_types ~expand_ops ~renaming
    in
    let renaming_context =
      match renaming_context with
      | None -> Renaming.(get_ctx default_config)
      | Some r -> r
    in
    debug_pass_name "scalc";
    ( Scalc.From_lcalc.translate_program
        ~config:
          {
            keep_special_ops;
            dead_value_assignment;
            no_struct_literals;
            keep_module_names;
            renaming_context;
          }
        prg,
      type_ordering,
      renaming_context )
end

module Commands = struct
  open Cmdliner

  let fix_trace options =
    if options.Global.trace <> None then (
      Message.warning "%a" Format.pp_print_text
        "Trace printing is not compatible with closure conversion or the C or \
         Java backends at the moment and has been disabled.";
      Global.enforce_options ~trace:None ())
    else options

  let get_scope_uid (ctx : decl_ctx) (scope : string) : ScopeName.t =
    if String.contains scope '.' then
      Message.error
        "Bad scope argument @{<yellow>%s@}: only references to the top-level \
         module are allowed"
        scope;
    try Ident.Map.find scope ctx.ctx_scope_index
    with Ident.Map.Not_found _ ->
      Message.error "There is no scope \"@{<yellow>%s@}\" inside the program."
        scope
        ~suggestion:(Ident.Map.keys ctx.ctx_scope_index)

  let get_scopelist_uids prg (scopes : string list) : ScopeName.t list =
    match scopes with
    | _ :: _ -> List.map (get_scope_uid prg.decl_ctx) scopes
    | [] ->
      let exports = BoundList.last prg.code_items in
      let test_scopes =
        List.filter_map
          (function KTest scope, _ -> Some scope | _ -> None)
          exports
      in
      if test_scopes = [] then
        Message.warning
          "The program defines no test scopes.@ Please specify option \
           @{<yellow>--scope@} to explicit what to execute@ or@ mark@ scopes@ \
           in@ the@ program@ with@ the@ @{<cyan>#[test]@}@ attribute.@ The \
           program defines the following scopes:@ @[<hv 4>%a@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space ScopeName.format)
          (List.filter_map
             (function KScope n, _ -> Some n | _ -> None)
             exports)
      else
        Message.debug "Will execute the following test scopes:@ %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space ScopeName.format)
          test_scopes;
      test_scopes

  let get_variable_uid
      (ctxt : Desugared.Name_resolution.context)
      (scope_uid : ScopeName.t)
      (variable : string) : Desugared.Ast.ScopeDef.t =
    (* Sometimes the variable selected is of the form [a.b] *)
    let first_part, second_part =
      match String.index_opt variable '.' with
      | Some i ->
        ( String.sub variable 0 i,
          Some (String.sub variable i (String.length variable - i)) )
      | None -> variable, None
    in
    match
      Ident.Map.find_opt first_part
        (ScopeName.Map.find scope_uid ctxt.scopes).var_idmap
    with
    | None ->
      Message.error
        "Variable @{<yellow>\"%s\"@} not found inside scope @{<yellow>\"%a\"@}"
        variable ScopeName.format scope_uid
    | Some (ScopeVar v | SubScope (v, _)) ->
      let state =
        second_part
        |> Option.map
           @@ fun id ->
           let var_sig = ScopeVar.Map.find v ctxt.var_typs in
           match Ident.Map.find_opt id var_sig.var_sig_states_idmap with
           | Some state -> state
           | None ->
             Message.error
               "State @{<yellow>\"%s\"@} is not found for variable \
                @{<yellow>\"%s\"@} of scope @{<yellow>\"%a\"@}"
               id first_part ScopeName.format scope_uid
      in
      (v, Pos.void), Desugared.Ast.ScopeDef.Var state

  let get_output ?ext options output_file =
    let output_file = Option.map options.Global.path_rewrite output_file in
    File.get_main_out_channel ~source_file:options.Global.input_src ~output_file
      ?ext ()

  let get_output_format options output_file =
    let output_file = Option.map options.Global.path_rewrite output_file in
    fun ?ext f ->
      let output_file, with_output =
        File.get_main_out_formatter ~source_file:options.Global.input_src
          ~output_file ?ext ()
      in
      Message.debug "Writing to %s" (Option.value ~default:"stdout" output_file);
      with_output (fun ppf -> f output_file ppf)

  let makefile options output =
    let prg = Passes.surface options in
    let backend_extensions_list = [".tex"] in
    let source_file = Global.input_src_file options.Global.input_src in
    let output_file, with_output = get_output options ~ext:"d" output in
    Message.debug "Writing list of dependencies to %s..."
      (Option.value ~default:"stdout" output_file);
    with_output
    @@ fun oc ->
    Printf.fprintf oc "%s:\\\n%s\n%s:"
      (String.concat "\\\n"
         (Option.value ~default:"stdout" output_file
         :: List.map
              (fun ext -> File.remove_extension source_file ^ ext)
              backend_extensions_list))
      (String.concat "\\\n" prg.Surface.Ast.program_source_files)
      (String.concat "\\\n" prg.Surface.Ast.program_source_files)

  let makefile_cmd =
    Cmd.v
      (Cmd.info "makefile" ~man:Cli.man_base
         ~doc:
           "Generates a Makefile-compatible list of the file dependencies of a \
            Catala program.")
      Term.(const makefile $ Cli.Flags.Global.options $ Cli.Flags.output)

  let html options output print_only_law wrap_weaved_output =
    let prg = Passes.surface options in
    Message.debug "Weaving literate program into HTML";
    get_output_format options ~ext:"html" output
    @@ fun output_file fmt ->
    let language =
      Cli.file_lang (Global.input_src_file options.Global.input_src)
    in
    let weave_output = Literate.Html.ast_to_html language ~print_only_law in
    Message.debug "Writing to %s" (Option.value ~default:"stdout" output_file);
    if wrap_weaved_output then
      Literate.Html.wrap_html prg.Surface.Ast.program_source_files language fmt
        (fun fmt -> weave_output fmt prg)
    else weave_output fmt prg

  let html_cmd =
    Cmd.v
      (Cmd.info "html" ~man:Cli.man_base
         ~doc:
           "Weaves an HTML literate programming output of the Catala program.")
      Term.(
        const html
        $ Cli.Flags.Global.options
        $ Cli.Flags.output
        $ Cli.Flags.print_only_law
        $ Cli.Flags.wrap_weaved_output)

  let latex options output print_only_law wrap_weaved_output extra_files =
    let prg = Passes.surface options in
    let prg_annex =
      List.map
        (fun f -> Surface.Parser_driver.parse_top_level_file (FileName f))
        extra_files
    in
    Message.debug "Weaving literate program into LaTeX";
    get_output_format options ~ext:"tex" output
    @@ fun _output_file fmt ->
    let language =
      Cli.file_lang (Global.input_src_file options.Global.input_src)
    in
    let weave_output = Literate.Latex.ast_to_latex language ~print_only_law in
    let weave fmt =
      weave_output fmt prg;
      List.iter
        (fun p ->
          Format.fprintf fmt "@,\\newpage@,";
          weave_output fmt p)
        prg_annex
    in
    if wrap_weaved_output then
      Literate.Latex.wrap_latex
        (List.flatten
           (List.map
              (fun p -> p.Surface.Ast.program_source_files)
              (prg :: prg_annex)))
        language fmt weave
    else weave fmt

  let latex_cmd =
    Cmd.v
      (Cmd.info "latex" ~man:Cli.man_base
         ~doc:
           "Weaves a LaTeX literate programming output of the Catala program.")
      Term.(
        const latex
        $ Cli.Flags.Global.options
        $ Cli.Flags.output
        $ Cli.Flags.print_only_law
        $ Cli.Flags.wrap_weaved_output
        $ Cli.Flags.extra_files)

  let exceptions options includes stdlib ex_scope ex_variable =
    let prg, ctxt = Passes.desugared options ~includes ~stdlib in
    Passes.debug_pass_name "scopelang";
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let scope_uid = get_scope_uid prg.program_ctx ex_scope in
    let variable_uid = get_variable_uid ctxt scope_uid ex_variable in
    Desugared.Print.exceptions_graph scope_uid variable_uid
      (Desugared.Ast.ScopeDef.Map.find variable_uid exceptions_graphs)

  let exceptions_cmd =
    Cmd.v
      (Cmd.info "exceptions" ~man:Cli.man_base
         ~doc:
           "Prints the exception tree for the definitions of a particular \
            variable, for debugging purposes. Use the $(b,-s) option to select \
            the scope and the $(b,-v) option to select the variable. Use \
            foo.bar to access state bar of variable foo or variable bar of \
            subscope foo.")
      Term.(
        const exceptions
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.ex_scope
        $ Cli.Flags.ex_variable)

  let dependency_graph options includes stdlib =
    let prg_desugared, _ctxt = Passes.desugared options ~includes ~stdlib in
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg_desugared
    in
    let intra_scope_dependency_graphs =
      ScopeName.Map.map
        (fun s ->
          Desugared.Dependency.build_scope_dependencies s
          |> Desugared.Dependency.scope_dependencies_to_json)
        prg_desugared.program_root.module_scopes
    in
    let prg_scopelang =
      Scopelang.From_desugared.translate_program prg_desugared exceptions_graphs
    in
    let types_dependency_graph =
      Scopelang.Dependency.build_type_graph
        prg_scopelang.program_ctx.ctx_structs
        prg_scopelang.program_ctx.ctx_enums
      |> Scopelang.Dependency.type_dependencies_graph_to_json
    in
    let inter_scope_dependencies =
      Scopelang.Dependency.build_program_dep_graph prg_scopelang
      |> Scopelang.Dependency.inter_scope_dependencies_graph_to_json
    in
    Passes.debug_pass_name "scopelang";
    let json_output =
      `Assoc
        [
          ( "intra_scopes",
            `Assoc
              (List.map
                 (fun (s_name, g) -> ScopeName.to_string s_name, g)
                 (ScopeName.Map.bindings intra_scope_dependency_graphs)) );
          "inter_scopes", inter_scope_dependencies;
          "types", types_dependency_graph;
        ]
    in
    Yojson.Safe.to_channel stdout json_output;
    print_newline ()

  let dependency_graph_cmd =
    Cmd.v
      (Cmd.info "dependency-graph" ~man:Cli.man_base
         ~doc:
           "Prints the inter-scope dependency graph (which scope calls which \
            scope), as well as the intra-scope dependency graphs (which scope \
            variable uses which scope variable), and the type dependency graph \
            (which type uses which type), in JSON format.")
      Term.(
        const dependency_graph
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir)

  let scopelang options includes stdlib output ex_scopes =
    let prg = Passes.scopelang options ~includes ~stdlib in
    get_output_format options output
    @@ fun _ fmt ->
    match ex_scopes with
    | _ :: _ ->
      List.iter
        (fun scope ->
          let scope_uid = get_scope_uid prg.program_ctx scope in
          Scopelang.Print.scope ~debug:options.Global.debug fmt
            (scope_uid, ScopeName.Map.find scope_uid prg.program_scopes);
          Format.pp_print_newline fmt ())
        ex_scopes
    | [] ->
      Scopelang.Print.program ~debug:options.Global.debug fmt prg;
      Format.pp_print_newline fmt ()

  let scopelang_cmd =
    Cmd.v
      (Cmd.info "scopelang" ~man:Cli.man_base ~docs:Cli.s_debug
         ~doc:
           "Prints a debugging verbatim of the scope language intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const scopelang
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.ex_scopes)

  let typecheck options check_invariants includes stdlib quiet =
    let prg = Passes.scopelang options ~allow_external:true ~includes ~stdlib in
    Message.debug "Typechecking...";
    let _type_ordering =
      Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
        prg.program_ctx.ctx_enums
    in
    let prg = Scopelang.Ast.type_program prg in
    Message.debug "Translating to default calculus...";
    (* Strictly type-checking could stop here, but we also want this pass to
       check full name-resolution and cycle detection. These are checked during
       translation to dcalc so we run it here and drop the result. *)
    let prg = Dcalc.From_scopelang.translate_program prg in

    (* Additionally, we might want to check the invariants. *)
    if check_invariants then (
      let prg = Shared_ast.Typing.program prg in
      Message.debug "Checking invariants...";
      if Dcalc.Invariants.check_all_invariants prg then
        if quiet then () else Message.result "All invariant checks passed"
      else
        raise (Message.error ~internal:true "Some Dcalc invariants are invalid"));
    Message.report_delayed_errors_if_any ();
    if not quiet then Message.result "Typechecking successful!"

  let typecheck_cmd =
    Cmd.v
      (Cmd.info "typecheck" ~man:Cli.man_base
         ~doc:"Parses and typechecks a Catala program, without interpreting it.")
      Term.(
        const typecheck
        $ Cli.Flags.Global.options
        $ Cli.Flags.check_invariants
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.quiet)

  let dcalc
      typed
      options
      includes
      stdlib
      output
      optimize
      ex_scopes
      check_invariants
      autotest =
    let prg, _ =
      Passes.dcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~typed
    in
    get_output_format options output
    @@ fun _ fmt ->
    match ex_scopes with
    | [] ->
      Print.program ~debug:options.Global.debug fmt prg;
      Format.pp_print_newline fmt ()
    | scopes ->
      List.iter
        (fun scope ->
          let scope_uid = get_scope_uid prg.decl_ctx scope in
          Print.scope ~debug:options.Global.debug fmt
            ( scope,
              BoundList.find
                ~f:(function
                  | ScopeDef (name, body) when ScopeName.equal name scope_uid ->
                    Some body
                  | _ -> None)
                prg.code_items );
          Format.pp_print_newline fmt ())
        scopes

  let dcalc_cmd =
    let f no_typing =
      if no_typing then dcalc Expr.untyped else dcalc Expr.typed
    in
    Cmd.v
      (Cmd.info "dcalc" ~man:Cli.man_base ~docs:Cli.s_debug
         ~doc:
           "Prints a debugging verbatim of the default calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const f
        $ Cli.Flags.no_typing
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scopes
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest)

  let proof
      options
      includes
      stdlib
      optimize
      ex_scope_opt
      check_invariants
      disable_counterexamples =
    let prg, _ =
      Passes.dcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest:false ~typed:Expr.typed
    in
    Verification.Globals.setup ~optimize ~disable_counterexamples;
    let vcs =
      Verification.Conditions.generate_verification_conditions prg
        (Option.map (get_scope_uid prg.decl_ctx) ex_scope_opt)
    in
    Verification.Solver.solve_vc prg.decl_ctx vcs

  let proof_cmd =
    Cmd.v
      (Cmd.info "proof" ~man:Cli.man_base
         ~doc:
           "Generates and proves verification conditions about the \
            well-behaved execution of the Catala program.")
      Term.(
        const proof
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scope_opt
        $ Cli.Flags.check_invariants
        $ Cli.Flags.disable_counterexamples)

  let print_interpretation_results
      options
      ?(quiet = false)
      interpreter
      scope_uid =
    try
      Message.debug "Starting interpretation...";
      let results, cov_opt = interpreter () in
      Message.debug "End of interpretation";
      let results =
        List.sort
          (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2)
          results
      in
      let language =
        Cli.file_lang (Global.input_src_file options.Global.input_src)
      in
      if quiet then begin
        (* Caution: this output is parsed by Clerk *)
        Format.fprintf (Message.std_ppf ()) "%a: @{<green>passed@}%t@."
          ScopeName.format scope_uid (fun fmt ->
            Option.iter
              (Format.fprintf fmt "|%a" Coverage.format_coverage_hex_dump)
              cov_opt)
      end
      else if results = [] then Message.result "Computation successful!"
      else
        Message.results
          ~title:(ScopeName.to_string scope_uid)
          (List.map
             (fun ((var, _), result) ppf ->
               Format.fprintf ppf "@[<hov 2>%s@ =@ %a@]" var
                 (if options.Global.debug then Print.expr ~debug:false ()
                  else Print.UserFacing.value language)
                 result)
             results);
      true
    with
    | Message.CompilerError content ->
      Message.Content.emit content Error;
      if quiet then
        Format.fprintf (Message.std_ppf ()) "%a: @{<red>failed@}@."
          ScopeName.format scope_uid;
      false
    | Message.CompilerErrors contents ->
      Message.Content.emit_n contents Error;
      if quiet then
        Format.fprintf (Message.std_ppf ()) "%a: @{<red>failed@}@."
          ScopeName.format scope_uid;
      false

  let interpret_dcalc
      typed
      code_coverage
      options
      includes
      stdlib
      optimize
      check_invariants
      quiet
      ex_scopes =
    let prg, _ =
      Passes.dcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest:false ~typed
    in
    Interpreter.load_runtime_modules
      ~hashf:Hash.(finalise ~monomorphize_types:false)
      prg;
    let success =
      List.fold_left
        (fun success scope ->
          if code_coverage then
            let interp () =
              let res, cov =
                Interpreter.interpret_program_dcalc_with_coverage ?stdlib prg
                  scope
              in
              res, Some cov
            in
            print_interpretation_results options ~quiet interp scope
          else
            let interp () =
              Interpreter.interpret_program_dcalc prg scope, None
            in
            print_interpretation_results ~quiet options interp scope && success)
        true
        (get_scopelist_uids prg ex_scopes)
    in
    if not success then raise (Cli.Exit_with 123)

  let lcalc
      typed
      options
      includes
      stdlib
      output
      optimize
      check_invariants
      autotest
      closure_conversion
      keep_special_ops
      monomorphize_types
      expand_ops
      ex_scopes =
    let options = if closure_conversion then fix_trace options else options in
    let prg, _, _ =
      Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~closure_conversion ~keep_special_ops ~typed
        ~monomorphize_types ~expand_ops ~renaming:(Some Renaming.default)
    in
    get_output_format options output
    @@ fun _ fmt ->
    match ex_scopes with
    | _ :: _ as scopes ->
      List.iter
        (fun scope ->
          let scope_uid = get_scope_uid prg.decl_ctx scope in
          Print.scope ~debug:options.Global.debug fmt
            (scope, Program.get_scope_body prg scope_uid);
          Format.pp_print_newline fmt ())
        scopes
    | [] ->
      Print.program ~debug:options.Global.debug fmt prg;
      Format.pp_print_newline fmt ()

  let lcalc_cmd =
    let f no_typing =
      if no_typing then lcalc Expr.untyped else lcalc Expr.typed
    in
    Cmd.v
      (Cmd.info "lcalc" ~man:Cli.man_base ~docs:Cli.s_debug
         ~doc:
           "Prints a debugging verbatim of the lambda calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const f
        $ Cli.Flags.no_typing
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.keep_special_ops
        $ Cli.Flags.monomorphize_types
        $ Cli.Flags.expand_ops
        $ Cli.Flags.ex_scopes)

  let interpret_lcalc
      typed
      closure_conversion
      keep_special_ops
      monomorphize_types
      expand_ops
      options
      includes
      stdlib
      optimize
      check_invariants
      quiet
      ex_scopes =
    let options = if closure_conversion then fix_trace options else options in
    let prg, _, _ =
      Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest:false ~closure_conversion ~keep_special_ops
        ~monomorphize_types ~typed ~expand_ops ~renaming:None
    in
    Interpreter.load_runtime_modules
      ~hashf:(Hash.finalise ~monomorphize_types)
      prg;
    let success =
      List.fold_left
        (fun success scope ->
          let interp () = Interpreter.interpret_program_lcalc prg scope, None in
          print_interpretation_results ~quiet options interp scope && success)
        true
        (get_scopelist_uids prg ex_scopes)
    in
    if not success then raise (Cli.Exit_with 123)

  let interpret_cmd =
    let f
        lcalc
        closure_conversion
        keep_special_ops
        monomorphize_types
        expand_ops
        no_typing
        code_coverage =
      if not lcalc then
        if closure_conversion || monomorphize_types then
          Message.error
            "The flags @{<bold>--closure-conversion@} and \
             @{<bold>--monomorphize-types@} only make sense with the \
             @{<bold>--lcalc@} option"
        else if no_typing then interpret_dcalc Expr.untyped code_coverage
        else interpret_dcalc Expr.typed code_coverage
      else if code_coverage then
        Message.error
          "The flag @{<bold>--code-coverage@} is not compatible with the \
           @{<bold>--lcalc@} option"
      else if no_typing then
        interpret_lcalc Expr.untyped closure_conversion keep_special_ops
          monomorphize_types expand_ops
      else
        interpret_lcalc Expr.typed closure_conversion keep_special_ops
          monomorphize_types expand_ops
    in
    Cmd.v
      (Cmd.info "interpret" ~man:Cli.man_base
         ~doc:
           "Runs the interpreter on the Catala program, executing the scopes \
            specified with the $(b,-s) option, or the scopes marked as \
            $(i,#[test]) if absent.")
      Term.(
        const f
        $ Cli.Flags.lcalc
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.monomorphize_types
        $ Cli.Flags.keep_special_ops
        $ Cli.Flags.expand_ops
        $ Cli.Flags.no_typing
        $ Cli.Flags.code_coverage
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.quiet
        $ Cli.Flags.ex_scopes)

  let ocaml
      options
      includes
      stdlib
      output
      optimize
      check_invariants
      autotest
      closure_conversion =
    let options = if closure_conversion then fix_trace options else options in
    let prg, type_ordering, _ =
      Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~typed:Expr.typed ~closure_conversion ~keep_special_ops:true
        ~monomorphize_types:false ~expand_ops:true
        ~renaming:(Some Lcalc.To_ocaml.renaming)
    in
    Message.debug "Compiling program into OCaml...";
    get_output_format options output
      ~ext:(if Global.options.gen_external then "template.ml" else "ml")
    @@ fun output_file fmt ->
    let hashf = Hash.finalise ~monomorphize_types:false in
    Lcalc.To_ocaml.format_program output_file fmt prg ~hashf type_ordering

  let ocaml_cmd =
    Cmd.v
      (Cmd.info "ocaml" ~man:Cli.man_base
         ~doc:"Generates an OCaml translation of the Catala program.")
      Term.(
        const ocaml
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion)

  let scalc
      options
      includes
      stdlib
      output
      optimize
      check_invariants
      autotest
      closure_conversion
      keep_special_ops
      dead_value_assignment
      no_struct_literals
      monomorphize_types
      expand_ops
      ex_scope_opt =
    let options = if closure_conversion then fix_trace options else options in
    let prg, _, _ =
      Passes.scalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~closure_conversion ~keep_special_ops ~dead_value_assignment
        ~no_struct_literals ~keep_module_names:false ~monomorphize_types
        ~expand_ops ~renaming:(Some Renaming.default)
    in
    get_output_format options output
    @@ fun _ fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid prg.ctx.decl_ctx scope in
      Scalc.Print.format_item ~debug:options.Global.debug prg.ctx.decl_ctx fmt
        (List.find
           (function
             | Scalc.Ast.SScope { scope_body_name; _ } ->
               scope_body_name = scope_uid
             | _ -> false)
           prg.code_items);
      Format.pp_print_newline fmt ()
    | None -> Scalc.Print.format_program fmt prg

  let scalc_cmd =
    Cmd.v
      (Cmd.info "scalc" ~man:Cli.man_base ~docs:Cli.s_debug
         ~doc:
           "Prints a debugging verbatim of the statement calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const scalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.keep_special_ops
        $ Cli.Flags.dead_value_assignment
        $ Cli.Flags.no_struct_literals
        $ Cli.Flags.monomorphize_types
        $ Cli.Flags.expand_ops
        $ Cli.Flags.ex_scope_opt)

  let python
      options
      includes
      stdlib
      output
      optimize
      check_invariants
      autotest
      closure_conversion =
    let options = if closure_conversion then fix_trace options else options in
    let prg, type_ordering, _ren_ctx =
      Passes.scalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~closure_conversion ~keep_special_ops:false
        ~dead_value_assignment:true ~no_struct_literals:false
        ~keep_module_names:false ~monomorphize_types:false ~expand_ops:false
        ~renaming:(Some Scalc.To_python.renaming)
    in
    Message.debug "Compiling program into Python...";
    get_output_format options output
      ~ext:(if Global.options.gen_external then "template.py" else "py")
    @@ fun output_file fmt ->
    Scalc.To_python.format_program output_file fmt prg type_ordering

  let python_cmd =
    Cmd.v
      (Cmd.info "python" ~man:Cli.man_base
         ~doc:"Generates a Python translation of the Catala program.")
      Term.(
        const python
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion)

  let java
      options
      includes
      stdlib
      (output : Global.raw_file option)
      optimize
      check_invariants
      autotest
      closure_conversion =
    let options = fix_trace options in
    let prg, _type_ordering, _ren_ctx =
      Passes.scalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~closure_conversion ~keep_special_ops:false
        ~dead_value_assignment:true ~no_struct_literals:false
        ~keep_module_names:true ~monomorphize_types:false ~expand_ops:false
        ~renaming:(Some Scalc.To_java.renaming)
    in
    Message.debug "Compiling program into Java...";
    get_output_format options output
      ~ext:(if Global.options.gen_external then "template.java" else "java")
    @@ fun output_file ppf ->
    let class_name =
      match output_file, options.Global.input_src with
      | Some file, _
      | None, (FileName (file : File.t) | Contents (_, (file : File.t))) ->
        let name = File.remove_extension file |> Filename.basename in
        if Global.options.gen_external then
          String.capitalize_ascii (File.remove_extension name)
        else name
      | None, Stdin _ -> "AnonymousClass"
    in
    Scalc.To_java.format_program ~class_name output_file ppf prg

  let java_cmd =
    Cmd.v
      (Cmd.info "java" ~man:Cli.man_base
         ~doc:"Generates a Java translation of the Catala program.")
      Term.(
        const java
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion)

  let c options includes stdlib output optimize check_invariants autotest =
    let options = fix_trace options in
    let prg, type_ordering, _ren_ctx =
      Passes.scalc options ~includes ~stdlib ~optimize ~check_invariants
        ~autotest ~closure_conversion:true ~keep_special_ops:false
        ~dead_value_assignment:false ~no_struct_literals:true
        ~keep_module_names:false ~monomorphize_types:false ~expand_ops:true
        ~renaming:(Some Scalc.To_c.renaming)
    in
    Message.debug "Compiling program into C...";
    get_output_format options output
      ~ext:(if Global.options.gen_external then "template.c" else "c")
    @@ fun output_file ppf ->
    Scalc.To_c.format_program output_file ppf prg type_ordering

  let c_cmd =
    Cmd.v
      (Cmd.info "c" ~man:Cli.man_base
         ~doc:"Generates an C translation of the Catala program.")
      Term.(
        const c
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest)

  let depends options includes stdlib prefix subdir extension extra_files =
    let file = Global.input_src_file options.Global.input_src in
    let more_includes = List.map Filename.dirname (file :: extra_files) in
    let prg =
      Surface.Ast.
        {
          program_module = None;
          program_items = [];
          program_source_files = [file];
          program_used_modules =
            List.map
              (fun f ->
                let name =
                  String.capitalize_ascii
                    (String.to_id (Filename.basename (File.remove_extension f)))
                in
                {
                  mod_use_name = name, Pos.void;
                  mod_use_alias = name, Pos.void;
                })
              (file :: extra_files);
          program_lang = Cli.file_lang file;
        }
    in
    let mod_uses, modules =
      load_modules options includes ~stdlib ~more_includes
        ~allow_notmodules:true prg
    in
    let d_ctx =
      Desugared.Name_resolution.form_context (prg, mod_uses) modules
    in
    let modules = ModuleName.Map.map fst modules in
    let prg = Desugared.From_surface.translate_program d_ctx modules prg in
    let modules_list_topo =
      Program.modules_to_list prg.program_ctx.ctx_modules
    in
    Format.open_hbox ();
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (m, _) ->
        let f = Pos.get_file (Mark.get (ModuleName.get_info m)) in
        let f =
          match prefix with
          | None -> f
          | Some pfx ->
            if not (Filename.is_relative f) then (
              Message.warning
                "Not adding prefix to %s, which is an absolute path" f;
              f)
            else File.(pfx / f)
        in
        let f =
          match subdir with
          | None -> f
          | Some d -> File.(dirname f / d / basename f)
        in
        let f = File.clean_path f in
        if extension = [] then Format.pp_print_string ppf f
        else
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf ext ->
              let base = File.(dirname f / ModuleName.to_string m) in
              Format.pp_print_string ppf base;
              if ext <> "" then (
                Format.(
                  pp_print_char ppf '.';
                  pp_print_string ppf ext)))
            ppf extension)
      Format.std_formatter modules_list_topo;
    Format.close_box ();
    Format.print_newline ()

  let depends_cmd =
    Cmd.v
      (Cmd.info "depends" ~man:Cli.man_base
         ~deprecated:
           "Prefer the use of Clerk Targets defined in a $(i,clerk.toml) file. \
            This may be unreliable with non-ASCII module names"
         ~doc:
           "Lists the dependencies of the given catala files, in linking \
            order. This includes recursive dependencies and is useful for \
            linking an application in a target language. The space-separated \
            list is printed to stdout. The names are printed as expected of \
            module identifiers, $(i,i.e.) capitalized.\n\
            NOTE: the files specified are also included in the returned list.")
      Term.(
        const depends
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.stdlib_dir
        $ Cli.Flags.prefix
        $ Cli.Flags.subdir
        $ Cli.Flags.extension
        $ Cli.Flags.extra_files)

  let pygmentize_cmd =
    Cmd.v
      (Cmd.info "pygmentize" ~man:Cli.man_base
         ~doc:
           "This special command is a wrapper around the $(b,pygmentize) \
            command that enables support for colorising Catala code.")
      Term.(
        const (fun _ ->
            assert false
            (* Not really a catala command, this is handled preemptively at
               startup *))
        $ Cli.Flags.Global.options)

  let commands =
    [
      interpret_cmd;
      typecheck_cmd;
      proof_cmd;
      ocaml_cmd;
      python_cmd;
      java_cmd;
      c_cmd;
      latex_cmd;
      html_cmd;
      makefile_cmd;
      scopelang_cmd;
      dcalc_cmd;
      lcalc_cmd;
      scalc_cmd;
      exceptions_cmd;
      dependency_graph_cmd;
      depends_cmd;
      pygmentize_cmd;
    ]
end

let raise_help cmdname cmds =
  let plugins = Plugin.names () in
  let cmds = List.filter (fun name -> not (List.mem name plugins)) cmds in
  Message.error
    "One of the following commands was expected:@;\
     <1 4>@[<v>@{<bold;blue>%a@}@]%a@\n\
     Run `@{<bold>%s --help@}' or `@{<bold>%s COMMAND --help@}' for details."
    (Format.pp_print_list Format.pp_print_string)
    (List.sort String.compare cmds)
    (fun ppf -> function
      | [] -> ()
      | plugins ->
        Format.fprintf ppf
          "@\n\
           Or one of the following installed plugins:@;\
           <1 4>@[<v>@{<blue>%a@}@]"
          (Format.pp_print_list Format.pp_print_string)
          plugins)
    plugins cmdname cmdname

let catala_t extra_commands =
  let open Cmdliner in
  let default =
    Term.(const raise_help $ main_name $ choice_names $ Cli.Flags.Global.flags)
  in
  Cmd.group ~default Cli.info (Commands.commands @ extra_commands)

let main () =
  let argv = Array.copy Sys.argv in
  (* Our command names (first argument) are case-insensitive *)
  if Array.length argv >= 2 then argv.(1) <- String.lowercase_ascii argv.(1);
  (* Pygmentize is a specific exec subcommand that doesn't go through
     cmdliner *)
  if
    Array.length Sys.argv >= 2
    && argv.(1) = "pygmentize"
    && not
         (Array.length Sys.argv >= 3
         && String.starts_with ~prefix:"--help" argv.(2))
  then Literate.Pygmentize.exec ();
  (* Peek to load plugins before the command-line is parsed proper (plugins add
     their own commands) *)
  let plugins =
    let plugins_dirs =
      match
        Cmdliner.Cmd.eval_peek_opts ~argv Cli.Flags.Global.flags
          ~version_opt:true
      with
      | Some opts, _ -> opts.Global.plugins_dirs
      | None, _ -> []
    in
    Passes.debug_pass_name "init";
    List.iter
      (fun d ->
        if d = "" then ()
        else
          match Sys.is_directory d with
          | true -> Plugin.load_dir d
          | false -> Message.debug "Could not read plugin directory %s" d
          | exception Sys_error _ ->
            Message.debug "Could not read plugin directory %s" d)
      plugins_dirs;
    Dynlink.allow_only
      (List.filter (( <> ) "Driver__Plugin") (Dynlink.all_units ()));
    (* From here on, no plugin registration is allowed. However, the interpreter
       may yet use Dynlink to load external modules. - TODO: This used to allow
       only "Runtime_ocaml__Runtime", but forbidding external Catala modules to
       use the OCaml Stdlib was a bit much. We should examine how to re-add some
       more filtering here without being too restrictive. *)
    Plugin.list ()
  in
  let command = catala_t plugins in
  let open Cmdliner in
  let[@inline] exit_with_error excode fcontent =
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit (fcontent ()) Error;
    if Global.options.debug then Printexc.print_raw_backtrace stderr bt;
    exit excode
  in
  let eval_cmd () =
    let r = Cmd.eval_value ~catch:false ~argv command in
    Message.report_delayed_errors_if_any ();
    r
  in
  match eval_cmd () with
  | Ok _ -> exit Cmd.Exit.ok
  | Error e ->
    if e = `Term then Plugin.print_failures ();
    exit Cmd.Exit.cli_error
  | exception Cli.Exit_with n -> exit n
  | exception Message.CompilerErrors contents ->
    Message.Content.emit_n contents Error;
    exit Cmd.Exit.some_error
  | exception Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    let contents = Message.combine_with_pending_errors content bt in
    Message.Content.emit_n contents Error;
    exit Cmd.Exit.some_error
  | exception Failure msg ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.of_string msg
  | exception Sys_error msg ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () -> Message.Content.of_string ("System error: " ^ msg)
  | exception e ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.of_string ("Unexpected error: " ^ Printexc.to_string e)

(* Export module PluginAPI, hide parent module Plugin *)
module Plugin = struct
  let register name ?man ?doc term =
    let name = String.lowercase_ascii name in
    let info = Cmdliner.Cmd.info name ?man ?doc ~docs:Cli.s_plugins in
    Plugin.register info term

  let register_subcommands name ?man ?doc cmds =
    let name = String.lowercase_ascii name in
    let info = Cmdliner.Cmd.info name ?man ?doc ~docs:Cli.s_plugins in
    Plugin.register_subcommands info cmds

  let register_attribute = Plugin.register_attribute
end
