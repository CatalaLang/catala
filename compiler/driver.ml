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

(** Associates a file extension with its corresponding {!type: Cli.backend_lang}
    string representation. *)
let extensions = [".catala_fr", "fr"; ".catala_en", "en"; ".catala_pl", "pl"]

let modname_of_file f =
  (* Fixme: make this more robust *)
  String.capitalize_ascii Filename.(basename (remove_extension f))

let get_lang options file =
  let filename = match file with Cli.FileName s -> s | Contents _ -> "-" in
  Option.bind
    (List.assoc_opt (Filename.extension filename) extensions)
    (fun l -> List.assoc_opt l Cli.languages)
  |> function
  | Some lang -> lang
  | None -> (
    match options.Cli.language with
    | Some lang -> lang
    | None ->
      Message.raise_error
        "Could not infer language variant from the extension of \
         @{<yellow>%s@}, and @{<bold>--language@} was not specified"
        filename)

let load_module_interfaces options program files =
  let module MS = ModuleName.Set in
  let to_set intf_list =
    MS.of_list
      (List.map (fun (mname, _) -> ModuleName.of_string mname)
         intf_list)
  in
  let used_modules =
    to_set program.Surface.Ast.program_modules
  in
  let load_file f =
    let lang = get_lang options (FileName f) in
    let (mname, intf), using =
      Surface.Parser_driver.load_interface (FileName f) lang
    in
    (ModuleName.of_string mname, intf), using
  in
  let module_interfaces = List.map load_file files in
  let rec check (required, acc) interfaces =
    let required, acc, remaining =
      List.fold_left (fun (required, acc, skipped) ((modname, intf), using as modl) ->
          if MS.mem modname required then
            let required =
              List.fold_left (fun req m -> MS.add (ModuleName.of_string m) req) required using
            in
            required, (((modname :> string Mark.pos), intf) :: acc), skipped
          else
            required, acc, (modl :: skipped))
        (required, acc, [])
        interfaces
    in
    if List.length remaining < List.length interfaces then
      (* Loop until fixpoint *)
      check (required, acc) remaining
    else
      required, acc, remaining
  in
  let required, loaded, unused = check (used_modules, []) module_interfaces in
  let missing =
    MS.diff required (MS.of_list (List.map (fun (m,_) -> ModuleName.of_string m) loaded)) in
  if not (MS.is_empty missing) || unused <> [] then
    Message.raise_multispanned_error
      (List.map (fun m ->
           Some (Format.asprintf "Required module not found: %a"
                   ModuleName.format m),
           ModuleName.pos m)
          (ModuleName.Set.elements missing) @
       List.map (fun ((m, _), _) ->
           Some (Format.asprintf "No use was found for this module: %a"
                   ModuleName.format m),
           ModuleName.pos m)
         unused)
      "Modules used from the program don't match the command-line";
  loaded

module Passes = struct
  (* Each pass takes only its cli options, then calls upon its dependent passes
     (forwarding their options as needed) *)

  let debug_pass_name s =
    Message.emit_debug "@{<bold;magenta>=@} @{<bold>%s@} @{<bold;magenta>=@}"
      (String.uppercase_ascii s)

  let surface options ~link_modules : Surface.Ast.program * Cli.backend_lang =
    debug_pass_name "surface";
    let language = get_lang options options.input_file in
    let prg =
      Surface.Parser_driver.parse_top_level_file options.input_file language
    in
    let prg = Surface.Fill_positions.fill_pos_with_legislative_info prg in
    let program_modules = load_module_interfaces options prg link_modules in
    { prg with program_modules }, language

  let desugared options ~link_modules :
      Desugared.Ast.program * Desugared.Name_resolution.context =
    let prg, _ = surface options ~link_modules in
    debug_pass_name "desugared";
    Message.emit_debug "Name resolution...";
    let ctx = Desugared.Name_resolution.form_context prg in
    (* let scope_uid = get_scope_uid options backend ctx in
     * (\* This uid is a Desugared identifier *\)
     * let variable_uid = get_variable_uid options backend ctx scope_uid in *)
    Message.emit_debug "Desugaring...";
    let prg = Desugared.From_surface.translate_program ctx prg in
    Message.emit_debug "Disambiguating...";
    let prg = Desugared.Disambiguate.program prg in
    Message.emit_debug "Linting...";
    Desugared.Linting.lint_program prg;
    prg, ctx
  (* Note: we forward the name resolution context throughout in order to locate
     uids from strings. Maybe a reduced form should be included directly in
     [prg] for that purpose *)

  let scopelang options ~link_modules :
      untyped Scopelang.Ast.program
      * Desugared.Name_resolution.context
      * Desugared.Dependency.ExceptionsDependencies.t
        Desugared.Ast.ScopeDef.Map.t =
    let prg, ctx = desugared options ~link_modules in
    debug_pass_name "scopelang";
    let exceptions_graphs =
      Scopelang.From_desugared.build_exceptions_graph prg
    in
    let prg =
      Scopelang.From_desugared.translate_program prg exceptions_graphs
    in
    prg, ctx, exceptions_graphs

  let dcalc options ~link_modules ~optimize ~check_invariants :
      typed Dcalc.Ast.program
      * Desugared.Name_resolution.context
      * Scopelang.Dependency.TVertex.t list =
    let prg, ctx, _ = scopelang options ~link_modules in
    debug_pass_name "dcalc";
    let type_ordering =
      Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
        prg.program_ctx.ctx_enums
    in
    Message.emit_debug "Typechecking...";
    let prg = Scopelang.Ast.type_program prg in
    Message.emit_debug "Translating to default calculus...";
    let prg = Dcalc.From_scopelang.translate_program prg in
    let prg =
      if optimize then begin
        Message.emit_debug "Optimizing default calculus...";
        Optimizations.optimize_program prg
      end
      else prg
    in
    Message.emit_debug "Typechecking again...";
    let prg =
      try Typing.program ~leave_unresolved:false prg
      with Message.CompilerError error_content ->
        let bt = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace
          (Message.CompilerError
             (Message.Content.to_internal_error error_content))
          bt
    in
    if check_invariants then (
      Message.emit_debug "Checking invariants...";
      let result = Dcalc.Invariants.check_all_invariants prg in
      if not result then
        raise (Message.raise_internal_error "Some Dcalc invariants are invalid"));
    prg, ctx, type_ordering

  let lcalc
      options
      ~link_modules
      ~optimize
      ~check_invariants
      ~avoid_exceptions
      ~closure_conversion :
      untyped Lcalc.Ast.program
      * Desugared.Name_resolution.context
      * Scopelang.Dependency.TVertex.t list =
    let prg, ctx, type_ordering =
      dcalc options ~link_modules ~optimize ~check_invariants
    in
    debug_pass_name "lcalc";
    let avoid_exceptions = avoid_exceptions || closure_conversion in
    let optimize = optimize || closure_conversion in
    (* --closure_conversion implies --avoid_exceptions and --optimize *)
    let prg =
      if avoid_exceptions then (
        if options.trace then
          Message.raise_error
            "Option --avoid_exceptions is not compatible with option --trace";
        Lcalc.Compile_without_exceptions.translate_program prg)
      else Program.untype (Lcalc.Compile_with_exceptions.translate_program prg)
    in
    let prg =
      if optimize then begin
        Message.emit_debug "Optimizing lambda calculus...";
        Optimizations.optimize_program prg
      end
      else prg
    in
    let prg =
      if not closure_conversion then prg
      else (
        Message.emit_debug "Performing closure conversion...";
        let prg = Lcalc.Closure_conversion.closure_conversion prg in
        let prg = Bindlib.unbox prg in
        let prg =
          if optimize then (
            Message.emit_debug "Optimizing lambda calculus...";
            Optimizations.optimize_program prg)
          else prg
        in
        Message.emit_debug "Retyping lambda calculus...";
        let prg = Program.untype (Typing.program ~leave_unresolved:true prg) in
        prg)
    in
    prg, ctx, type_ordering

  let scalc
      options
      ~link_modules
      ~optimize
      ~check_invariants
      ~avoid_exceptions
      ~closure_conversion :
      Scalc.Ast.program
      * Desugared.Name_resolution.context
      * Scopelang.Dependency.TVertex.t list =
    let prg, ctx, type_ordering =
      lcalc options ~link_modules ~optimize ~check_invariants ~avoid_exceptions
        ~closure_conversion
    in
    debug_pass_name "scalc";
    Scalc.From_lcalc.translate_program prg, ctx, type_ordering
end

module Commands = struct
  open Cmdliner

  let get_scope_uid (ctxt : Desugared.Name_resolution.context) (scope : string)
      =
    match Ident.Map.find_opt scope ctxt.typedefs with
    | Some (Desugared.Name_resolution.TScope (uid, _)) -> uid
    | _ ->
      Message.raise_error
        "There is no scope @{<yellow>\"%s\"@} inside the program." scope

  (* TODO: this is very weird but I'm trying to maintain the current behaviour
     for now *)
  let get_random_scope_uid (ctxt : Desugared.Name_resolution.context) =
    let _, scope =
      try
        Shared_ast.Ident.Map.filter_map
          (fun _ -> function
            | Desugared.Name_resolution.TScope (uid, _) -> Some uid
            | _ -> None)
          ctxt.typedefs
        |> Shared_ast.Ident.Map.choose
      with Not_found ->
        Message.raise_error "There isn't any scope inside the program."
    in
    scope

  let get_variable_uid
      (ctxt : Desugared.Name_resolution.context)
      (scope_uid : ScopeName.t)
      (variable : string) =
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
      Message.raise_error
        "Variable @{<yellow>\"%s\"@} not found inside scope @{<yellow>\"%a\"@}"
        variable ScopeName.format scope_uid
    | Some
        (Desugared.Name_resolution.SubScope (subscope_var_name, subscope_name))
      -> (
      match second_part with
      | None ->
        Message.raise_error
          "Subscope @{<yellow>\"%a\"@} of scope @{<yellow>\"%a\"@} cannot be \
           selected by itself, please add \".<var>\" where <var> is a subscope \
           variable."
          SubScopeName.format subscope_var_name ScopeName.format scope_uid
      | Some second_part -> (
        match
          let ctxt =
            Desugared.Name_resolution.module_ctx ctxt
              (List.map
                 (fun m -> ModuleName.to_string m, Pos.no_pos)
                 (ScopeName.path subscope_name))
          in
          Ident.Map.find_opt second_part
            (ScopeName.Map.find subscope_name ctxt.scopes).var_idmap
        with
        | Some (Desugared.Name_resolution.ScopeVar v) ->
          Desugared.Ast.ScopeDef.SubScopeVar (subscope_var_name, v, Pos.no_pos)
        | _ ->
          Message.raise_error
            "Var @{<yellow>\"%s\"@} of subscope @{<yellow>\"%a\"@} in scope \
             @{<yellow>\"%a\"@} does not exist, please check your command line \
             arguments."
            second_part SubScopeName.format subscope_var_name ScopeName.format
            scope_uid))
    | Some (Desugared.Name_resolution.ScopeVar v) ->
      Desugared.Ast.ScopeDef.Var
        ( v,
          Option.map
            (fun second_part ->
              let var_sig = ScopeVar.Map.find v ctxt.var_typs in
              match
                Ident.Map.find_opt second_part var_sig.var_sig_states_idmap
              with
              | Some state -> state
              | None ->
                Message.raise_error
                  "State @{<yellow>\"%s\"@} is not found for variable \
                   @{<yellow>\"%s\"@} of scope @{<yellow>\"%a\"@}"
                  second_part first_part ScopeName.format scope_uid)
            second_part )

  let get_output ?ext options output_file =
    File.get_out_channel ~source_file:options.Cli.input_file ~output_file ?ext
      ()

  let get_output_format ?ext options output_file =
    File.get_formatter_of_out_channel ~source_file:options.Cli.input_file
      ~output_file ?ext ()

  let makefile options output =
    let prg, _ = Passes.surface options ~link_modules:[] in
    let backend_extensions_list = [".tex"] in
    let source_file =
      match options.Cli.input_file with
      | FileName f -> f
      | Contents _ ->
        Message.raise_error "The Makefile backend requires a filename as input"
    in
    let output_file, with_output = get_output options ~ext:".d" output in
    Message.emit_debug "Writing list of dependencies to %s..."
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
      (Cmd.info "makefile"
         ~doc:
           "Generates a Makefile-compatible list of the file dependencies of a \
            Catala program.")
      Term.(const makefile $ Cli.Flags.Global.options $ Cli.Flags.output)

  let html options output print_only_law wrap_weaved_output =
    let prg, language = Passes.surface options ~link_modules:[] in
    Message.emit_debug "Weaving literate program into HTML";
    let output_file, with_output =
      get_output_format options ~ext:".html" output
    in
    with_output
    @@ fun fmt ->
    let weave_output = Literate.Html.ast_to_html language ~print_only_law in
    Message.emit_debug "Writing to %s"
      (Option.value ~default:"stdout" output_file);
    if wrap_weaved_output then
      Literate.Html.wrap_html prg.Surface.Ast.program_source_files language fmt
        (fun fmt -> weave_output fmt prg)
    else weave_output fmt prg

  let html_cmd =
    Cmd.v
      (Cmd.info "html"
         ~doc:
           "Weaves an HTML literate programming output of the Catala program.")
      Term.(
        const html
        $ Cli.Flags.Global.options
        $ Cli.Flags.output
        $ Cli.Flags.print_only_law
        $ Cli.Flags.wrap_weaved_output)

  let latex options output print_only_law wrap_weaved_output =
    let prg, language = Passes.surface options ~link_modules:[] in
    Message.emit_debug "Weaving literate program into LaTeX";
    let output_file, with_output =
      get_output_format options ~ext:".tex" output
    in
    with_output
    @@ fun fmt ->
    let weave_output = Literate.Latex.ast_to_latex language ~print_only_law in
    Message.emit_debug "Writing to %s"
      (Option.value ~default:"stdout" output_file);
    if wrap_weaved_output then
      Literate.Latex.wrap_latex prg.Surface.Ast.program_source_files language
        fmt (fun fmt -> weave_output fmt prg)
    else weave_output fmt prg

  let latex_cmd =
    Cmd.v
      (Cmd.info "latex"
         ~doc:
           "Weaves a LaTeX literate programming output of the Catala program.")
      Term.(
        const latex
        $ Cli.Flags.Global.options
        $ Cli.Flags.output
        $ Cli.Flags.print_only_law
        $ Cli.Flags.wrap_weaved_output)

  let exceptions options link_modules ex_scope ex_variable =
    let _, ctxt, exceptions_graphs = Passes.scopelang options ~link_modules in
    let scope_uid = get_scope_uid ctxt ex_scope in
    let variable_uid = get_variable_uid ctxt scope_uid ex_variable in
    Desugared.Print.print_exceptions_graph scope_uid variable_uid
      (Desugared.Ast.ScopeDef.Map.find variable_uid exceptions_graphs)

  let exceptions_cmd =
    Cmd.v
      (Cmd.info "exceptions"
         ~doc:
           "Prints the exception tree for the definitions of a particular \
            variable, for debugging purposes. Use the $(b,-s) option to select \
            the scope and the $(b,-v) option to select the variable. Use \
            foo.bar to access state bar of variable foo or variable bar of \
            subscope foo.")
      Term.(
        const exceptions
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.ex_scope
        $ Cli.Flags.ex_variable)

  let scopelang options link_modules output ex_scope_opt =
    let prg, ctx, _ = Passes.scopelang options ~link_modules in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid ctx scope in
      Scopelang.Print.scope ~debug:options.Cli.debug prg.program_ctx fmt
        (scope_uid, ScopeName.Map.find scope_uid prg.program_scopes);
      Format.pp_print_newline fmt ()
    | None ->
      Scopelang.Print.program ~debug:options.Cli.debug fmt prg;
      Format.pp_print_newline fmt ()

  let scopelang_cmd =
    Cmd.v
      (Cmd.info "scopelang"
         ~doc:
           "Prints a debugging verbatim of the scope language intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const scopelang
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.ex_scope_opt)

  let typecheck options link_modules =
    let prg, _, _ = Passes.scopelang options ~link_modules in
    Message.emit_debug "Typechecking...";
    let _type_ordering =
      Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
        prg.program_ctx.ctx_enums
    in
    let prg = Scopelang.Ast.type_program prg in
    Message.emit_debug "Translating to default calculus...";
    (* Strictly type-checking could stop here, but we also want this pass to
       check full name-resolution and cycle detection. These are checked during
       translation to dcalc so we run it here and drop the result. *)
    let _prg = Dcalc.From_scopelang.translate_program prg in
    Message.emit_result "Typechecking successful!"

  let typecheck_cmd =
    Cmd.v
      (Cmd.info "typecheck"
         ~doc:"Parses and typechecks a Catala program, without interpreting it.")
      Term.(const typecheck $ Cli.Flags.Global.options $ Cli.Flags.link_modules)

  let dcalc options link_modules output optimize ex_scope_opt check_invariants =
    let prg, ctx, _ =
      Passes.dcalc options ~link_modules ~optimize ~check_invariants
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid ctx scope in
      Print.scope ~debug:options.Cli.debug prg.decl_ctx fmt
        ( scope_uid,
          Option.get
            (Scope.fold_left ~init:None
               ~f:(fun acc def _ ->
                 match def with
                 | ScopeDef (name, body) when ScopeName.equal name scope_uid ->
                   Some body
                 | _ -> acc)
               prg.code_items) );
      Format.pp_print_newline fmt ()
    | None ->
      let scope_uid = get_random_scope_uid ctx in
      (* TODO: ??? *)
      let prg_dcalc_expr = Expr.unbox (Program.to_expr prg scope_uid) in
      Format.fprintf fmt "%a\n"
        (Print.expr ~debug:options.Cli.debug ())
        prg_dcalc_expr

  let dcalc_cmd =
    Cmd.v
      (Cmd.info "dcalc"
         ~doc:
           "Prints a debugging verbatim of the default calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const dcalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scope_opt
        $ Cli.Flags.check_invariants)

  let proof
      options
      link_modules
      optimize
      ex_scope_opt
      check_invariants
      disable_counterexamples =
    let prg, ctx, _ =
      Passes.dcalc options ~link_modules ~optimize ~check_invariants
    in
    Verification.Globals.setup ~optimize ~disable_counterexamples;
    let vcs =
      Verification.Conditions.generate_verification_conditions prg
        (Option.map (get_scope_uid ctx) ex_scope_opt)
    in
    Verification.Solver.solve_vc prg.decl_ctx vcs

  let proof_cmd =
    Cmd.v
      (Cmd.info "proof"
         ~doc:
           "Generates and proves verification conditions about the \
            well-behaved execution of the Catala program.")
      Term.(
        const proof
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.optimize
        $ Cli.Flags.ex_scope_opt
        $ Cli.Flags.check_invariants
        $ Cli.Flags.disable_counterexamples)

  let print_interpretation_results options interpreter prg scope_uid =
    Message.emit_debug "Starting interpretation...";
    let results =
      try interpreter prg scope_uid
      with Shared_ast.Interpreter.CatalaException exn ->
        Message.raise_error
          "During interpretation, the error %a has been raised but not caught!"
          Shared_ast.Print.except exn
    in
    Message.emit_debug "End of interpretation";
    let results =
      List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) results
    in
    Message.emit_result "Computation successful!%s"
      (if List.length results > 0 then " Results:" else "");
    List.iter
      (fun ((var, _), result) ->
        Message.emit_result "@[<hov 2>%s@ =@ %a@]" var
          (if options.Cli.debug then Print.expr ~debug:false ()
           else Print.UserFacing.value (get_lang options options.input_file))
          result)
      results

  let interpret_dcalc options link_modules optimize check_invariants ex_scope =
    let prg, ctx, _ =
      Passes.dcalc options ~link_modules ~optimize ~check_invariants
    in
    Interpreter.load_runtime_modules prg;
    print_interpretation_results options Interpreter.interpret_program_dcalc prg
      (get_scope_uid ctx ex_scope)

  let interpret_cmd =
    Cmd.v
      (Cmd.info "interpret"
         ~doc:
           "Runs the interpreter on the Catala program, executing the scope \
            specified by the $(b,-s) option assuming no additional external \
            inputs.")
      Term.(
        const interpret_dcalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.ex_scope)

  let lcalc
      options
      link_modules
      output
      optimize
      check_invariants
      avoid_exceptions
      closure_conversion
      ex_scope_opt =
    let prg, ctx, _ =
      Passes.lcalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions ~closure_conversion
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid ctx scope in
      Print.scope ~debug:options.Cli.debug prg.decl_ctx fmt
        (scope_uid, Program.get_scope_body prg scope_uid);
      Format.pp_print_newline fmt ()
    | None ->
      Print.program ~debug:options.Cli.debug fmt prg;
      Format.pp_print_newline fmt ()

  let lcalc_cmd =
    Cmd.v
      (Cmd.info "lcalc"
         ~doc:
           "Prints a debugging verbatim of the lambda calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const lcalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.avoid_exceptions
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.ex_scope_opt)

  let interpret_lcalc
      options
      link_modules
      optimize
      check_invariants
      avoid_exceptions
      closure_conversion
      ex_scope =
    let prg, ctx, _ =
      Passes.lcalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions ~closure_conversion
    in
    print_interpretation_results options Interpreter.interpret_program_lcalc prg
      (get_scope_uid ctx ex_scope)

  let interpret_lcalc_cmd =
    Cmd.v
      (Cmd.info "interpret_lcalc"
         ~doc:
           "Runs the interpreter on the lcalc pass on the Catala program, \
            executing the scope specified by the $(b,-s) option assuming no \
            additional external inputs.")
      Term.(
        const interpret_lcalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.avoid_exceptions
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.ex_scope)

  let ocaml
      options
      link_modules
      output
      optimize
      check_invariants
      avoid_exceptions
      closure_conversion =
    let prg, _, type_ordering =
      Passes.lcalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions ~closure_conversion
    in
    let output_file, with_output =
      get_output_format options ~ext:".ml" output
    in
    with_output
    @@ fun fmt ->
    Message.emit_debug "Compiling program into OCaml...";
    Message.emit_debug "Writing to %s..."
      (Option.value ~default:"stdout" output_file);
    Lcalc.To_ocaml.format_program fmt prg type_ordering

  let ocaml_cmd =
    Cmd.v
      (Cmd.info "ocaml"
         ~doc:"Generates an OCaml translation of the Catala program.")
      Term.(
        const ocaml
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.avoid_exceptions
        $ Cli.Flags.closure_conversion)

  let scalc
      options
      link_modules
      output
      optimize
      check_invariants
      avoid_exceptions
      closure_conversion
      ex_scope_opt =
    let prg, ctx, _ =
      Passes.scalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions ~closure_conversion
    in
    let _output_file, with_output = get_output_format options output in
    with_output
    @@ fun fmt ->
    match ex_scope_opt with
    | Some scope ->
      let scope_uid = get_scope_uid ctx scope in
      Scalc.Print.format_item ~debug:options.Cli.debug prg.decl_ctx fmt
        (List.find
           (function
             | Scalc.Ast.SScope { scope_body_name; _ } ->
               scope_body_name = scope_uid
             | _ -> false)
           prg.code_items);
      Format.pp_print_newline fmt ()
    | None -> Scalc.Print.format_program prg.decl_ctx fmt prg

  let scalc_cmd =
    Cmd.v
      (Cmd.info "scalc"
         ~doc:
           "Prints a debugging verbatim of the statement calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(
        const scalc
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.avoid_exceptions
        $ Cli.Flags.closure_conversion
        $ Cli.Flags.ex_scope_opt)

  let python
      options
      link_modules
      output
      optimize
      check_invariants
      avoid_exceptions
      closure_conversion =
    let prg, _, type_ordering =
      Passes.scalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions ~closure_conversion
    in

    let output_file, with_output =
      get_output_format options ~ext:".py" output
    in
    Message.emit_debug "Compiling program into Python...";
    Message.emit_debug "Writing to %s..."
      (Option.value ~default:"stdout" output_file);
    with_output
    @@ fun fmt -> Scalc.To_python.format_program fmt prg type_ordering

  let python_cmd =
    Cmd.v
      (Cmd.info "python"
         ~doc:"Generates a Python translation of the Catala program.")
      Term.(
        const python
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.avoid_exceptions
        $ Cli.Flags.closure_conversion)

  let r options link_modules output optimize check_invariants closure_conversion
      =
    let prg, _, type_ordering =
      Passes.scalc options ~link_modules ~optimize ~check_invariants
        ~avoid_exceptions:false ~closure_conversion
    in

    let output_file, with_output = get_output_format options ~ext:".r" output in
    Message.emit_debug "Compiling program into R...";
    Message.emit_debug "Writing to %s..."
      (Option.value ~default:"stdout" output_file);
    with_output @@ fun fmt -> Scalc.To_r.format_program fmt prg type_ordering

  let r_cmd =
    Cmd.v
      (Cmd.info "r" ~doc:"Generates an R translation of the Catala program.")
      Term.(
        const r
        $ Cli.Flags.Global.options
        $ Cli.Flags.link_modules
        $ Cli.Flags.output
        $ Cli.Flags.optimize
        $ Cli.Flags.check_invariants
        $ Cli.Flags.closure_conversion)

  let pygmentize_cmd =
    Cmd.v
      (Cmd.info "pygmentize"
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
      interpret_lcalc_cmd;
      typecheck_cmd;
      proof_cmd;
      ocaml_cmd;
      python_cmd;
      r_cmd;
      latex_cmd;
      html_cmd;
      makefile_cmd;
      scopelang_cmd;
      dcalc_cmd;
      lcalc_cmd;
      scalc_cmd;
      exceptions_cmd;
      pygmentize_cmd;
    ]
end

let raise_help cmdname cmds =
  let plugins = Plugin.names () in
  let cmds = List.filter (fun name -> not (List.mem name plugins)) cmds in
  Message.raise_error
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
      | Some opts, _ -> opts.Cli.plugins_dirs
      | None, _ -> []
    in
    Passes.debug_pass_name "init";
    List.iter
      (fun d ->
        if d = "" then ()
        else
          match Sys.is_directory d with
          | true -> Plugin.load_dir d
          | false -> Message.emit_debug "Could not read plugin directory %s" d
          | exception Sys_error _ ->
            Message.emit_debug "Could not read plugin directory %s" d)
      plugins_dirs;
    Dynlink.allow_only ["Runtime_ocaml__Runtime"];
    (* We may use dynlink again, but only for runtime modules: no plugin
       registration after this point *)
    Plugin.list ()
  in
  let command = catala_t plugins in
  let open Cmdliner in
  match Cmd.eval_value ~catch:false ~argv command with
  | Ok _ -> exit Cmd.Exit.ok
  | Error _ -> exit Cmd.Exit.cli_error
  | exception Cli.Exit_with n -> exit n
  | exception Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Cli.globals.debug then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.some_error
  | exception Sys_error msg ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("System error: " ^ msg))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("Unexpected error: " ^ Printexc.to_string e))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error

(* Export module PluginAPI, hide parent module Plugin *)
module Plugin = struct
  let register name ?man ?doc term =
    let name = String.lowercase_ascii name in
    let info = Cmdliner.Cmd.info name ?man ?doc ~docs:Cli.s_plugins in
    Plugin.register info term
end
