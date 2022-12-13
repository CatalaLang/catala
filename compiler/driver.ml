(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

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

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = ["en", Cli.En; "fr", Cli.Fr; "pl", Cli.Pl]

(** Associates a file extension with its corresponding {!type: Cli.backend_lang}
    string representation. *)
let extensions = [".catala_fr", "fr"; ".catala_en", "en"; ".catala_pl", "pl"]

(** Entry function for the executable. Returns a negative number in case of
    error. Usage: [driver source_file options]*)
let driver source_file (options : Cli.options) : int =
  try
    List.iter
      (fun d ->
        match Sys.is_directory d with
        | true -> Plugin.load_dir d
        | false -> ()
        | exception Sys_error _ -> ())
      options.plugins_dirs;
    Cli.set_option_globals options;
    if options.debug then Printexc.record_backtrace true;
    Cli.debug_print "Reading files...";
    let filename = ref "" in
    (match source_file with
    | Pos.FileName f -> filename := f
    | Contents c -> Cli.contents := c);
    (match options.max_prec_digits with
    | None -> ()
    | Some i -> Cli.max_prec_digits := i);
    let l =
      match options.language with
      | Some l -> l
      | None -> (
        (* Try to infer the language from the intput file extension. *)
        let ext = Filename.extension !filename in
        if ext = "" then
          Errors.raise_error
            "No file extension found for the file '%s'. (Try to add one or to \
             specify the -l flag)"
            !filename;
        try List.assoc ext extensions with Not_found -> ext)
    in
    let language =
      try List.assoc l languages
      with Not_found ->
        Errors.raise_error
          "The selected language (%s) is not supported by Catala" l
    in
    Cli.locale_lang := language;
    let backend = options.backend in
    let backend =
      match Cli.backend_option_of_string backend with
      | #Cli.backend_option_builtin as backend -> backend
      | `Plugin s -> (
        try `Plugin (Plugin.find s)
        with Not_found ->
          Errors.raise_error
            "The selected backend (%s) is not supported by Catala, nor was a \
             plugin by this name found under %a"
            backend
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ or @ ")
               (fun ppf dir ->
                 Format.pp_print_string ppf
                   (try Unix.readlink dir with _ -> dir)))
            options.plugins_dirs)
    in
    let prgm =
      Surface.Parser_driver.parse_top_level_file source_file language
    in
    let prgm = Surface.Fill_positions.fill_pos_with_legislative_info prgm in
    let get_output ?ext =
      File.get_out_channel ~source_file ~output_file:options.output_file ?ext
    in
    let get_output_format ?ext =
      File.get_formatter_of_out_channel ~source_file
        ~output_file:options.output_file ?ext
    in
    (match backend with
    | `Makefile ->
      let backend_extensions_list = [".tex"] in
      let source_file =
        match source_file with
        | FileName f -> f
        | Contents _ ->
          Errors.raise_error
            "The Makefile backend does not work if the input is not a file"
      in
      let output_file, with_output = get_output ~ext:".d" () in
      Cli.debug_print "Writing list of dependencies to %s..."
        (Option.value ~default:"stdout" output_file);
      with_output
      @@ fun oc ->
      Printf.fprintf oc "%s:\\\n%s\n%s:"
        (String.concat "\\\n"
           (Option.value ~default:"stdout" output_file
           :: List.map
                (fun ext -> Filename.remove_extension source_file ^ ext)
                backend_extensions_list))
        (String.concat "\\\n" prgm.program_source_files)
        (String.concat "\\\n" prgm.program_source_files)
    | (`Latex | `Html) as backend ->
      Cli.debug_print "Weaving literate program into %s"
        (match backend with `Latex -> "LaTeX" | `Html -> "HTML");
      let output_file, with_output =
        get_output_format ()
          ~ext:(match backend with `Latex -> ".tex" | `Html -> ".html")
      in
      with_output (fun fmt ->
          let weave_output =
            match backend with
            | `Latex ->
              Literate.Latex.ast_to_latex language
                ~print_only_law:options.print_only_law
            | `Html ->
              Literate.Html.ast_to_html language
                ~print_only_law:options.print_only_law
          in
          Cli.debug_print "Writing to %s"
            (Option.value ~default:"stdout" output_file);
          if options.wrap_weaved_output then
            match backend with
            | `Latex ->
              Literate.Latex.wrap_latex prgm.Surface.Ast.program_source_files
                language fmt (fun fmt -> weave_output fmt prgm)
            | `Html ->
              Literate.Html.wrap_html prgm.Surface.Ast.program_source_files
                language fmt (fun fmt -> weave_output fmt prgm)
          else weave_output fmt prgm)
    | ( `Interpret | `Typecheck | `OCaml | `Python | `Scalc | `Lcalc | `Dcalc
      | `Scopelang | `Proof | `Plugin _ ) as backend -> (
      Cli.debug_print "Name resolution...";
      let ctxt = Desugared.Name_resolution.form_context prgm in
      let scope_uid =
        match options.ex_scope, backend with
        | None, `Interpret ->
          Errors.raise_error "No scope was provided for execution."
        | None, _ ->
          let _, scope =
            try
              Shared_ast.IdentName.Map.filter_map
                (fun _ -> function
                  | Desugared.Name_resolution.TScope (uid, _) -> Some uid
                  | _ -> None)
                ctxt.typedefs
              |> Shared_ast.IdentName.Map.choose
            with Not_found ->
              Errors.raise_error "There isn't any scope inside the program."
          in
          scope
        | Some name, _ -> (
          match Shared_ast.IdentName.Map.find_opt name ctxt.typedefs with
          | Some (Desugared.Name_resolution.TScope (uid, _)) -> uid
          | _ ->
            Errors.raise_error "There is no scope \"%s\" inside the program."
              name)
      in
      Cli.debug_print "Desugaring...";
      let prgm = Desugared.From_surface.translate_program ctxt prgm in
      Cli.debug_print "Disambiguating...";
      let prgm = Desugared.Disambiguate.program prgm in
      Cli.debug_print "Collecting rules...";
      let prgm = Scopelang.From_desugared.translate_program prgm in
      match backend with
      | `Scopelang ->
        let _output_file, with_output = get_output_format () in
        with_output
        @@ fun fmt ->
        if Option.is_some options.ex_scope then
          Format.fprintf fmt "%a\n"
            (Scopelang.Print.scope prgm.program_ctx ~debug:options.debug)
            ( scope_uid,
              Shared_ast.ScopeName.Map.find scope_uid prgm.program_scopes )
        else
          Format.fprintf fmt "%a\n"
            (Scopelang.Print.program ~debug:options.debug)
            prgm
      | ( `Interpret | `Typecheck | `OCaml | `Python | `Scalc | `Lcalc | `Dcalc
        | `Proof | `Plugin _ ) as backend -> (
        Cli.debug_print "Typechecking...";
        let type_ordering =
          Scopelang.Dependency.check_type_cycles prgm.program_ctx.ctx_structs
            prgm.program_ctx.ctx_enums
        in
        let prgm = Scopelang.Ast.type_program prgm in
        Cli.debug_print "Translating to default calculus...";
        let prgm = Dcalc.From_scopelang.translate_program prgm in
        let prgm =
          if options.optimize then begin
            Cli.debug_print "Optimizing default calculus...";
            Dcalc.Optimizations.optimize_program prgm
          end
          else prgm
        in
        (* Cli.debug_print (Format.asprintf "Typechecking results :@\n%a"
           (Print.typ prgm.decl_ctx) typ); *)
        match backend with
        | `Typecheck ->
          Cli.debug_print "Typechecking again...";
          let _ =
            try Shared_ast.Typing.program prgm
            with Errors.StructuredError (msg, details) ->
              let msg =
                "Typing error occured during re-typing on the 'default \
                 calculus'. This is a bug in the Catala compiler.\n"
                ^ msg
              in
              raise (Errors.StructuredError (msg, details))
          in
          (* That's it! *)
          Cli.result_print "Typechecking successful!"
        | `Dcalc ->
          let _output_file, with_output = get_output_format () in
          with_output
          @@ fun fmt ->
          if Option.is_some options.ex_scope then
            Format.fprintf fmt "%a\n"
              (Shared_ast.Scope.format ~debug:options.debug prgm.decl_ctx)
              ( scope_uid,
                Option.get
                  (Shared_ast.Scope.fold_left ~init:None
                     ~f:(fun acc scope_def _ ->
                       if
                         Shared_ast.ScopeName.compare scope_def.scope_name
                           scope_uid
                         = 0
                       then Some scope_def.scope_body
                       else acc)
                     prgm.scopes) )
          else
            let prgrm_dcalc_expr =
              Shared_ast.Expr.unbox (Shared_ast.Program.to_expr prgm scope_uid)
            in
            Format.fprintf fmt "%a\n"
              (Shared_ast.Expr.format ~debug:options.debug prgm.decl_ctx)
              prgrm_dcalc_expr
        | (`Interpret | `OCaml | `Python | `Scalc | `Lcalc | `Proof | `Plugin _)
          as backend -> (
          Cli.debug_print "Typechecking again...";
          let prgm =
            try Shared_ast.Typing.program ~leave_unresolved:false prgm
            with Errors.StructuredError (msg, details) ->
              let msg =
                "Typing error occured during re-typing on the 'default \
                 calculus'. This is a bug in the Catala compiler.\n"
                ^ msg
              in
              raise (Errors.StructuredError (msg, details))
          in
          match backend with
          | `Proof ->
            let vcs =
              Verification.Conditions.generate_verification_conditions prgm
                (match options.ex_scope with
                | None -> None
                | Some _ -> Some scope_uid)
            in

            Verification.Solver.solve_vc prgm.decl_ctx vcs
          | `Interpret ->
            Cli.debug_print "Starting interpretation...";
            let prgrm_dcalc_expr =
              Shared_ast.Expr.unbox (Shared_ast.Program.to_expr prgm scope_uid)
            in
            let results =
              Dcalc.Interpreter.interpret_program prgm.decl_ctx prgrm_dcalc_expr
            in
            let results =
              List.sort
                (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2)
                results
            in
            Cli.debug_print "End of interpretation";
            Cli.result_print "Computation successful!%s"
              (if List.length results > 0 then " Results:" else "");
            List.iter
              (fun ((var, _), result) ->
                Cli.result_format "@[<hov 2>%s@ =@ %a@]" var
                  (Shared_ast.Expr.format ~debug:options.debug prgm.decl_ctx)
                  result)
              results
          | (`OCaml | `Python | `Lcalc | `Scalc | `Plugin _) as backend -> (
            Cli.debug_print "Compiling program into lambda calculus...";
            let prgm =
              if options.avoid_exceptions then
                Lcalc.Compile_without_exceptions.translate_program prgm
              else Lcalc.Compile_with_exceptions.translate_program prgm
            in
            let prgm =
              if options.optimize then begin
                Cli.debug_print "Optimizing lambda calculus...";
                Lcalc.Optimizations.optimize_program prgm
              end
              else Shared_ast.Program.untype prgm
            in
            let prgm =
              if options.closure_conversion then (
                if not options.avoid_exceptions then
                  Errors.raise_error
                    "option --avoid_exceptions must be enabled for \
                     --closure_conversion"
                else Cli.debug_print "Performing closure conversion...";
                let prgm = Lcalc.Closure_conversion.closure_conversion prgm in
                let prgm = Bindlib.unbox prgm in
                let prgm =
                  if options.optimize then (
                    Cli.debug_print "Optimizing lambda calculus...";
                    Lcalc.Optimizations.optimize_program prgm)
                  else prgm
                in
                Cli.debug_print "Retyping lambda calculus...";
                let prgm =
                  Shared_ast.Program.untype
                    (Shared_ast.Typing.program ~leave_unresolved:true prgm)
                in
                prgm)
              else prgm
            in
            match backend with
            | `Lcalc ->
              let _output_file, with_output = get_output_format () in
              with_output
              @@ fun fmt ->
              if Option.is_some options.ex_scope then
                Format.fprintf fmt "%a\n"
                  (Shared_ast.Scope.format ~debug:options.debug prgm.decl_ctx)
                  (scope_uid, Shared_ast.Program.get_scope_body prgm scope_uid)
              else
                let prgrm_lcalc_expr =
                  Shared_ast.Expr.unbox
                    (Shared_ast.Program.to_expr prgm scope_uid)
                in
                Format.fprintf fmt "%a\n"
                  (Shared_ast.Expr.format ~debug:options.debug prgm.decl_ctx)
                  prgrm_lcalc_expr
            | (`OCaml | `Python | `Scalc | `Plugin _) as backend -> (
              match backend with
              | `OCaml ->
                let output_file, with_output =
                  get_output_format ~ext:".ml" ()
                in
                with_output
                @@ fun fmt ->
                Cli.debug_print "Compiling program into OCaml...";
                Cli.debug_print "Writing to %s..."
                  (Option.value ~default:"stdout" output_file);
                Lcalc.To_ocaml.format_program fmt prgm type_ordering
              | `Plugin (Plugin.Lcalc p) ->
                let output_file, _ =
                  get_output_format ~ext:p.Plugin.extension ()
                in
                Cli.debug_print "Compiling program through backend \"%s\"..."
                  p.Plugin.name;
                p.Plugin.apply ~source_file ~output_file ~scope:options.ex_scope
                  prgm type_ordering
              | (`Python | `Scalc | `Plugin (Plugin.Scalc _)) as backend -> (
                let prgm = Scalc.Compile_from_lambda.translate_program prgm in
                match backend with
                | `Scalc ->
                  let _output_file, with_output = get_output_format () in
                  with_output
                  @@ fun fmt ->
                  if Option.is_some options.ex_scope then
                    Format.fprintf fmt "%a\n"
                      (Scalc.Print.format_scope ~debug:options.debug
                         prgm.decl_ctx)
                      (List.find
                         (fun body ->
                           body.Scalc.Ast.scope_body_name = scope_uid)
                         prgm.scopes)
                  else
                    Format.fprintf fmt "%a\n"
                      (Format.pp_print_list
                         ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
                         (fun fmt scope ->
                           (Scalc.Print.format_scope prgm.decl_ctx) fmt scope))
                      prgm.scopes
                | `Python ->
                  let output_file, with_output =
                    get_output_format ~ext:".py" ()
                  in
                  Cli.debug_print "Compiling program into Python...";
                  Cli.debug_print "Writing to %s..."
                    (Option.value ~default:"stdout" output_file);
                  with_output
                  @@ fun fmt ->
                  Scalc.To_python.format_program fmt prgm type_ordering
                | `Plugin (Plugin.Lcalc _) -> assert false
                | `Plugin (Plugin.Scalc p) ->
                  let output_file, _ = get_output ~ext:p.Plugin.extension () in
                  Cli.debug_print "Compiling program through backend \"%s\"..."
                    p.Plugin.name;
                  Cli.debug_print "Writing to %s..."
                    (Option.value ~default:"stdout" output_file);
                  p.Plugin.apply ~source_file ~output_file
                    ~scope:options.ex_scope prgm type_ordering)))))));
    0
  with
  | Errors.StructuredError (msg, pos) ->
    let bt = Printexc.get_raw_backtrace () in
    Cli.error_print "%s" (Errors.print_structured_error msg pos);
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    -1
  | Sys_error msg ->
    let bt = Printexc.get_raw_backtrace () in
    Cli.error_print "System error: %s" msg;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    -1

let main () =
  let return_code =
    Cmdliner.Cmd.eval'
      (Cmdliner.Cmd.v Cli.info (Cli.catala_t (fun f -> driver (FileName f))))
  in
  exit return_code

(* Export module PluginAPI, hide parent module Plugin *)
module Plugin = Plugin.PluginAPI
