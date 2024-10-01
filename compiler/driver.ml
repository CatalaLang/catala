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
let extensions = [".catala_fr", "fr"; ".catala_en", "en"; ".catala_pl", "pl"]

let modname_of_file f =
  (* Fixme: make this more robust *)
  String.capitalize_ascii Filename.(basename (remove_extension f))

let load_module_interfaces
    options
    includes
    ?(more_includes = [])
    ?(allow_notmodules = false)
    program =
  (* Recurse into program modules, looking up files in [using] and loading
     them *)
  if program.Surface.Ast.program_used_modules <> [] then
    Message.debug "Loading module interfaces...";
  let includes =
    List.map options.Global.path_rewrite includes @ more_includes
    |> List.map File.Tree.build
    |> List.fold_left File.Tree.union File.Tree.empty
  in
  let err_req_pos chain =
    List.map (fun mpos -> "Module required from", mpos) chain
  in
  let find_module req_chain (mname, mpos) =
    let required_from_file = Pos.get_file mpos in
    let includes =
      File.Tree.union includes
        (File.Tree.build (File.dirname required_from_file))
    in
    match
      List.filter_map
        (fun (ext, _) -> File.Tree.lookup includes (mname ^ ext))
        extensions
    with
    | [] ->
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
  let rec aux req_chain seen uses :
      (ModuleName.t * Surface.Ast.interface * ModuleName.t Ident.Map.t) option
      File.Map.t
      * ModuleName.t Ident.Map.t =
    List.fold_left
      (fun (seen, use_map) use ->
        let f = find_module req_chain use.Surface.Ast.mod_use_name in
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
              Some Filename.(basename (remove_extension f))
            else None
          in
          let intf =
            Surface.Parser_driver.load_interface ?default_module_name
              (Global.FileName f)
          in
          let modname = ModuleName.fresh intf.intf_modname.module_name in
          let seen = File.Map.add f None seen in
          let seen, sub_use_map =
            aux
              (Mark.get use.Surface.Ast.mod_use_name :: req_chain)
              seen intf.Surface.Ast.intf_submodules
          in
          ( File.Map.add f (Some (modname, intf, sub_use_map)) seen,
            Ident.Map.add
              (Mark.remove use.Surface.Ast.mod_use_alias)
              modname use_map ))
      (seen, Ident.Map.empty) uses
  in
  let seen =
    match program.Surface.Ast.program_module with
    | Some m ->
      let file = Pos.get_file (Mark.get m.module_name) in
      File.Map.singleton file None
    | None -> File.Map.empty
  in
  let file_module_map, root_uses =
    aux [] seen program.Surface.Ast.program_used_modules
  in
  let modules =
    File.Map.fold
      (fun _ info acc ->
        match info with
        | None -> acc
        | Some (mname, intf, use_map) ->
          ModuleName.Map.add mname (intf, use_map) acc)
      file_module_map ModuleName.Map.empty
  in
  root_uses, modules

module Passes = struct
  (* Each pass takes only its cli options, then calls upon its dependent passes
     (forwarding their options as needed) *)

  let debug_pass_name s =
    Message.debug "@{<bold;magenta>=@} @{<bold>%s@} @{<bold;magenta>=@}"
      (String.uppercase_ascii s)

  let surface options : Surface.Ast.program =
    debug_pass_name "surface";
    let prg =
      Surface.Parser_driver.parse_top_level_file options.Global.input_src
    in
    Surface.Fill_positions.fill_pos_with_legislative_info prg

  let desugared options ~includes :
      Desugared.Ast.program * Desugared.Name_resolution.context =
    let prg = surface options in
    let mod_uses, modules = load_module_interfaces options includes prg in
    debug_pass_name "desugared";
    Message.debug "Name resolution...";
    let ctx = Desugared.Name_resolution.form_context (prg, mod_uses) modules in
    Message.debug "Desugaring...";
    let prg = Desugared.From_surface.translate_program ctx prg in
    Message.debug "Disambiguating...";
    let prg = Desugared.Disambiguate.program prg in
    Message.debug "Linting...";
    Desugared.Linting.lint_program prg;
    prg, ctx

  let scopelang options ~includes : untyped Scopelang.Ast.program =
    let prg, _ = desugared options ~includes in
    debug_pass_name "scopelang";
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let prg =
      Scopelang.From_desugared.translate_program prg exceptions_graphs
    in
    prg

  let dcalc :
      type ty.
      Global.options ->
      includes:Global.raw_file list ->
      optimize:bool ->
      check_invariants:bool ->
      autotest:bool ->
      typed:ty mark ->
      ty Dcalc.Ast.program * TypeIdent.t list =
   fun options ~includes ~optimize ~check_invariants ~autotest ~typed ->
    let prg = scopelang options ~includes in
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
    Message.debug "Translating to default calculus...";
    let prg = Dcalc.From_scopelang.translate_program prg in
    let prg =
      if autotest then (
        Interpreter.load_runtime_modules
          ~hashf:
            Hash.(finalise ~closure_conversion:false ~monomorphize_types:false)
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
      dcalc options ~includes ~optimize ~check_invariants ~autotest ~typed
    in
    debug_pass_name "lcalc";
    let prg =
      match typed with
      | Untyped _ -> Lcalc.From_dcalc.translate_program prg
      | Typed _ -> Lcalc.From_dcalc.translate_program prg
      | Custom _ -> invalid_arg "Driver.Passes.lcalc"
    in
    let prg = if expand_ops then Lcalc.Expand_op.program prg else prg in
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
        let prg = Typing.program ~fail_on_any:false ~internal_check:true prg in
        if expand_ops then Lcalc.Expand_op.program prg else prg)
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
        Typing.program ~fail_on_any:false ~internal_check:true
          ~assume_op_types:true prg)
    in
    let prg, type_ordering =
      if monomorphize_types then (
        Message.debug "Monomorphizing types...";
        let prg, type_ordering = Lcalc.Monomorphize.program prg in
        Message.debug "Retyping lambda calculus...";
        let prg =
          Typing.program ~fail_on_any:false ~assume_op_types:true
            ~internal_check:true prg
        in
        prg, type_ordering)
      else prg, type_ordering
    in
    match renaming with
    | None -> prg, type_ordering, None
    | Some renaming ->
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
      ~optimize
      ~check_invariants
      ~autotest
      ~closure_conversion
      ~keep_special_ops
      ~dead_value_assignment
      ~no_struct_literals
      ~monomorphize_types
      ~expand_ops
      ~renaming : Scalc.Ast.program * TypeIdent.t list * Renaming.context =
    let prg, type_ordering, renaming_context =
      lcalc options ~includes ~optimize ~check_invariants ~autotest
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
            renaming_context;
          }
        prg,
      type_ordering,
      renaming_context )
end

module Commands = struct
  open Cmdliner

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

  let get_scopeopt_uid (ctx : decl_ctx) (scope_opt : string option) :
      ScopeName.t =
    match scope_opt with
    | Some s -> get_scope_uid ctx s
    | None -> (
      match ScopeName.Map.cardinal ctx.ctx_scopes with
      | 0 -> Message.error "The program defines no scopes"
      | 1 ->
        let s, _ = ScopeName.Map.choose ctx.ctx_scopes in
        Message.warning
          "No scope was specified, using the only one defined by the program:@ \
           %a"
          ScopeName.format s;
        s
      | _ ->
        Message.error
          "Please specify option @{<yellow>--scope@} or @{<yellow>-s@}.@ The \
           program defines the following scopes:@ @[<hv 4>%a@]"
          (ScopeName.Map.format_keys ~pp_sep:Format.pp_print_space)
          ctx.ctx_scopes)

  (* TODO: this is very weird but I'm trying to maintain the current behaviour
     for now *)
  let get_random_scope_uid (ctx : decl_ctx) : ScopeName.t =
    match Ident.Map.choose_opt ctx.ctx_scope_index with
    | Some (_, name) -> name
    | None -> Message.error "There isn't any scope inside the program."

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
      (v, Pos.no_pos), Desugared.Ast.ScopeDef.Var state

  let get_output ?ext options output_file =
    let output_file = Option.map options.Global.path_rewrite output_file in
    File.get_out_channel ~source_file:options.Global.input_src ~output_file ?ext
      ()

  let get_output_format ?ext options output_file =
    let output_file = Option.map options.Global.path_rewrite output_file in
    File.get_formatter_of_out_channel ~source_file:options.Global.input_src
      ~output_file ?ext ()

  let makefile options output =
    let prg = Passes.surface options in
    let backend_extensions_list = [".tex"] in
    let source_file = Global.input_src_file options.Global.input_src in
    let output_file, with_output = get_output options ~ext:".d" output in
    Message.debug "Writing list of dependencies to %s..."
      (Option.value ~default:"stdout" output_file);
    with_output
    @@ fun oc ->
    Printf.fprintf oc "%s:\\\n%s\n%s:"
      (String.concat "\\\n"
         (Option.value ~default:"stdout" output_file
         :: List.map
              (fun ext -> Filename.remove_extension source_file ^ ext)
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
    let output_file, with_output =
      get_output_format options ~ext:".html" output
    in
    with_output
    @@ fun fmt ->
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
        (fun f ->
          Surface.Parser_driver.parse_top_level_file (FileName f)
          |> Surface.Fill_positions.fill_pos_with_legislative_info)
        extra_files
    in
    Message.debug "Weaving literate program into LaTeX";
    let output_file, with_output =
      get_output_format options ~ext:".tex" output
    in
    with_output
    @@ fun fmt ->
    let language =
      Cli.file_lang (Global.input_src_file options.Global.input_src)
    in
    let weave_output = Literate.Latex.ast_to_latex language ~print_only_law in
    Message.debug "Writing to %s" (Option.value ~default:"stdout" output_file);
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

  let exceptions options includes ex_scope ex_variable =
    let prg, ctxt = Passes.desugared options ~includes in
    Passes.debug_pass_name "scopelang";
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let scope_uid = get_scope_uid prg.program_ctx ex_scope in
    let variable_uid = get_variable_uid ctxt scope_uid ex_variable in
    Desugared.Print.print_exceptions_graph scope_uid variable_uid
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
        $ Cli.Flags.ex_scope
        $ Cli.Flags.ex_variable)

  let scopelang options includes output ex_scope_opt =
    let prg = Passes.scopelang options ~includes in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid prg.program_ctx scope in
      Scopelang.Print.scope ~debug:options.Global.debug prg.program_ctx fmt
        (scope_uid, ScopeName.Map.find scope_uid prg.program_scopes);
      Format.pp_print_newline fmt ()
    | None ->
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
        $ Cli.Flags.output
        $ Cli.Flags.ex_scope_opt)

  let typecheck options check_invariants includes =
    let prg = Passes.scopelang options ~includes in
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
        Message.result "All invariant checks passed"
      else
        raise (Message.error ~internal:true "Some Dcalc invariants are invalid"));
    Message.result "Typechecking successful!"

  let typecheck_cmd =
    Cmd.v
      (Cmd.info "typecheck" ~man:Cli.man_base
         ~doc:"Parses and typechecks a Catala program, without interpreting it.")
      Term.(
        const typecheck
        $ Cli.Flags.Global.options
        $ Cli.Flags.check_invariants
        $ Cli.Flags.include_dirs)

  let dcalc
      typed
      options
      includes
      output
      optimize
      ex_scope_opt
      check_invariants
      autotest =
    let prg, _ =
      Passes.dcalc options ~includes ~optimize ~check_invariants ~autotest
        ~typed
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid prg.decl_ctx scope in
      Print.scope ~debug:options.Global.debug prg.decl_ctx fmt
        ( scope,
          BoundList.find
            ~f:(function
              | ScopeDef (name, body) when ScopeName.equal name scope_uid ->
                Some body
              | _ -> None)
            prg.code_items );
      Format.pp_print_newline fmt ()
    | None ->
      let scope_uid = get_random_scope_uid prg.decl_ctx in
      (* TODO: ??? *)
      let prg_dcalc_expr = Expr.unbox (Program.to_expr prg scope_uid) in
      Format.fprintf fmt "%a\n"
        (Print.expr ~debug:options.Global.debug ())
        prg_dcalc_expr

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
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scope_opt
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest)

  let proof
      options
      includes
      optimize
      ex_scope_opt
      check_invariants
      disable_counterexamples =
    let prg, _ =
      Passes.dcalc options ~includes ~optimize ~check_invariants ~autotest:false
        ~typed:Expr.typed
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
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scope_opt
        $ Cli.Flags.check_invariants
        $ Cli.Flags.disable_counterexamples)

  let print_interpretation_results options interpreter prg scope_uid =
    Message.debug "Starting interpretation...";
    let results = interpreter prg scope_uid in
    Message.debug "End of interpretation";
    let results =
      List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) results
    in
    let language =
      Cli.file_lang (Global.input_src_file options.Global.input_src)
    in
    if results = [] then Message.result "Computation successful!"
    else
      Message.results
        (List.map
           (fun ((var, _), result) ppf ->
             Format.fprintf ppf "@[<hov 2>%s@ =@ %a@]" var
               (if options.Global.debug then Print.expr ~debug:false ()
                else Print.UserFacing.value language)
               result)
           results)

  let interpret_dcalc
      typed
      options
      includes
      optimize
      check_invariants
      ex_scope_opt =
    let prg, _ =
      Passes.dcalc options ~includes ~optimize ~check_invariants ~autotest:false
        ~typed
    in
    Interpreter.load_runtime_modules
      ~hashf:Hash.(finalise ~closure_conversion:false ~monomorphize_types:false)
      prg;
    print_interpretation_results options Interpreter.interpret_program_dcalc prg
      (get_scopeopt_uid prg.decl_ctx ex_scope_opt)

  let lcalc
      typed
      options
      includes
      output
      optimize
      check_invariants
      autotest
      closure_conversion
      keep_special_ops
      monomorphize_types
      expand_ops
      ex_scope_opt =
    let prg, _, _ =
      Passes.lcalc options ~includes ~optimize ~check_invariants ~autotest
        ~closure_conversion ~keep_special_ops ~typed ~monomorphize_types
        ~expand_ops ~renaming:(Some Renaming.default)
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid prg.decl_ctx scope in
      Print.scope ~debug:options.Global.debug prg.decl_ctx fmt
        (scope, Program.get_scope_body prg scope_uid);
      Format.pp_print_newline fmt ()
    | None ->
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
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.keep_special_ops
        $ Cli.Flags.monomorphize_types
        $ Cli.Flags.expand_ops
        $ Cli.Flags.ex_scope_opt)

  let interpret_lcalc
      typed
      closure_conversion
      keep_special_ops
      monomorphize_types
      expand_ops
      options
      includes
      optimize
      check_invariants
      ex_scope_opt =
    let prg, _, _ =
      Passes.lcalc options ~includes ~optimize ~check_invariants ~autotest:false
        ~closure_conversion ~keep_special_ops ~monomorphize_types ~typed
        ~expand_ops ~renaming:None
    in
    Interpreter.load_runtime_modules
      ~hashf:(Hash.finalise ~closure_conversion ~monomorphize_types)
      prg;
    print_interpretation_results options Interpreter.interpret_program_lcalc prg
      (get_scopeopt_uid prg.decl_ctx ex_scope_opt)

  let interpret_cmd =
    let f
        lcalc
        closure_conversion
        keep_special_ops
        monomorphize_types
        expand_ops
        no_typing =
      if not lcalc then
        if closure_conversion || monomorphize_types then
          Message.error
            "The flags @{<bold>--closure-conversion@} and \
             @{<bold>--monomorphize-types@} only make sense with the \
             @{<bold>--lcalc@} option"
        else if no_typing then interpret_dcalc Expr.untyped
        else interpret_dcalc Expr.typed
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
           "Runs the interpreter on the Catala program, executing the scope \
            specified by the $(b,-s) option assuming no additional external \
            inputs.")
      Term.(
        const f
        $ Cli.Flags.lcalc
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.monomorphize_types
        $ Cli.Flags.keep_special_ops
        $ Cli.Flags.expand_ops
        $ Cli.Flags.no_typing
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.ex_scope_opt)

  let ocaml
      options
      includes
      output
      optimize
      check_invariants
      autotest
      closure_conversion
      ex_scope_opt =
    let prg, type_ordering, _ =
      Passes.lcalc options ~includes ~optimize ~check_invariants ~autotest
        ~typed:Expr.typed ~closure_conversion ~keep_special_ops:true
        ~monomorphize_types:false ~expand_ops:true
        ~renaming:(Some Lcalc.To_ocaml.renaming)
    in
    let output_file, with_output =
      get_output_format options ~ext:".ml" output
    in
    with_output
    @@ fun fmt ->
    Message.debug "Compiling program into OCaml...";
    Message.debug "Writing to %s..."
      (Option.value ~default:"stdout" output_file);
    let exec_scope = Option.map (get_scope_uid prg.decl_ctx) ex_scope_opt in
    let hashf = Hash.finalise ~closure_conversion ~monomorphize_types:false in
    Lcalc.To_ocaml.format_program fmt prg ?exec_scope ~hashf type_ordering

  let ocaml_cmd =
    Cmd.v
      (Cmd.info "ocaml" ~man:Cli.man_base
         ~doc:"Generates an OCaml translation of the Catala program.")
      Term.(
        const ocaml
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.ex_scope_opt)

  let scalc
      options
      includes
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
    let prg, _, _ =
      Passes.scalc options ~includes ~optimize ~check_invariants ~autotest
        ~closure_conversion ~keep_special_ops ~dead_value_assignment
        ~no_struct_literals ~monomorphize_types ~expand_ops
        ~renaming:(Some Renaming.default)
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
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
      output
      optimize
      check_invariants
      autotest
      closure_conversion =
    let prg, type_ordering, _ren_ctx =
      Passes.scalc options ~includes ~optimize ~check_invariants ~autotest
        ~closure_conversion ~keep_special_ops:false ~dead_value_assignment:true
        ~no_struct_literals:false ~monomorphize_types:false ~expand_ops:false
        ~renaming:(Some Scalc.To_python.renaming)
    in

    let output_file, with_output =
      get_output_format options ~ext:".py" output
    in
    Message.debug "Compiling program into Python...";
    Message.debug "Writing to %s..."
      (Option.value ~default:"stdout" output_file);
    with_output
    @@ fun fmt -> Scalc.To_python.format_program fmt prg type_ordering

  let python_cmd =
    Cmd.v
      (Cmd.info "python" ~man:Cli.man_base
         ~doc:"Generates a Python translation of the Catala program.")
      Term.(
        const python
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest
        $ Cli.Flags.closure_conversion)

  let c options includes output optimize check_invariants autotest =
    let prg, type_ordering, _ren_ctx =
      Passes.scalc options ~includes ~optimize ~check_invariants ~autotest
        ~closure_conversion:true ~keep_special_ops:false
        ~dead_value_assignment:false ~no_struct_literals:true
        ~monomorphize_types:false ~expand_ops:true
        ~renaming:(Some Scalc.To_c.renaming)
    in
    let output_file, with_output = get_output_format options ~ext:".c" output in
    let out_intf, with_output_intf =
      match output_file with
      | Some f when prg.module_name <> None ->
        let f = File.(f -.- "h") in
        File.get_formatter_of_out_channel ~source_file:options.Global.input_src
          ~output_file:(Some f) ~ext:".h" ()
      | _ -> None, fun pp -> pp (Format.make_formatter (fun _ _ _ -> ()) ignore)
    in
    Message.debug "Compiling program into C...";
    Message.debug "Writing to %s / %s..."
      (Option.value ~default:"stdout" output_file)
      (Option.value ~default:"no interface output" out_intf);
    with_output
    @@ fun ppf_src ->
    with_output_intf
    @@ fun ppf_intf ->
    Scalc.To_c.format_program ~ppf_src ~ppf_intf prg type_ordering

  let c_cmd =
    Cmd.v
      (Cmd.info "c" ~man:Cli.man_base
         ~doc:"Generates an C translation of the Catala program.")
      Term.(
        const c
        $ Cli.Flags.Global.options
        $ Cli.Flags.include_dirs
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.autotest)

  let depends options includes prefix extension extra_files =
    let file = Global.input_src_file options.Global.input_src in
    let more_includes = List.map Filename.dirname (file :: extra_files) in
    let prg =
      Surface.Ast.
        {
          program_module = None;
          program_items = [];
          program_source_files = [];
          program_used_modules =
            List.map
              (fun f ->
                let name = modname_of_file f in
                {
                  mod_use_name = name, Pos.no_pos;
                  mod_use_alias = name, Pos.no_pos;
                })
              (file :: extra_files);
          program_lang = Cli.file_lang file;
        }
    in
    let mod_uses, modules =
      load_module_interfaces options includes ~more_includes
        ~allow_notmodules:true prg
    in
    let d_ctx =
      Desugared.Name_resolution.form_context (prg, mod_uses) modules
    in
    let prg = Desugared.From_surface.translate_program d_ctx prg in
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
        let f = File.clean_path f in
        if extension = [] then Format.pp_print_string ppf f
        else
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf ext ->
               let base = File.(dirname f / ModuleName.to_string m) in
               Format.pp_print_string ppf base;
               if ext <> "" then Format.(pp_print_char ppf '.'; pp_print_string ppf ext)
            )
            ppf extension)
      Format.std_formatter modules_list_topo;
    Format.close_box ();
    Format.print_newline ()

  let depends_cmd =
    Cmd.v
      (Cmd.info "depends" ~man:Cli.man_base
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
        $ Cli.Flags.prefix
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
      c_cmd;
      latex_cmd;
      html_cmd;
      makefile_cmd;
      scopelang_cmd;
      dcalc_cmd;
      lcalc_cmd;
      scalc_cmd;
      exceptions_cmd;
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
  if Array.length Sys.argv >= 2 && argv.(1) = "pygmentize" then
    Literate.Pygmentize.exec ();
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
  match Cmd.eval_value ~catch:false ~argv command with
  | Ok _ -> exit Cmd.Exit.ok
  | Error e ->
    if e = `Term then Plugin.print_failures ();
    exit Cmd.Exit.cli_error
  | exception Cli.Exit_with n -> exit n
  | exception Message.CompilerError content ->
    exit_with_error Cmd.Exit.some_error @@ fun () -> content
  | exception Message.CompilerErrors contents ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit_n Error contents;
    if Global.options.debug then Printexc.print_raw_backtrace stderr bt;
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
end
