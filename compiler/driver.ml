(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Cli = Utils.Cli
module Errors = Utils.Errors
module Pos = Utils.Pos

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = [ ("en", Cli.En); ("fr", Cli.Fr); ("pl", Cli.Pl) ]

(** Associates a file extension with its corresponding {!type: Cli.frontend_lang} string
    representation. *)
let extensions = [ (".catala_fr", "fr"); (".catala_en", "en"); (".catala_pl", "pl") ]

(** Entry function for the executable. Returns a negative number in case of error. Usage:
    [driver source_file debug dcalc unstyled wrap_weaved_output backend language max_prec_digits trace optimize scope_to_execute output_file]*)
let driver (source_file : Pos.input_file) (debug : bool) (unstyled : bool)
    (wrap_weaved_output : bool) (backend : string) (language : string option)
    (max_prec_digits : int option) (trace : bool) (optimize : bool) (ex_scope : string option)
    (output_file : string option) : int =
  try
    Cli.debug_flag := debug;
    Cli.style_flag := not unstyled;
    Cli.trace_flag := trace;
    Cli.optimize_flag := optimize;
    Cli.debug_print "Reading files...";
    let filename = ref "" in
    (match source_file with FileName f -> filename := f | Contents c -> Cli.contents := c);
    (match max_prec_digits with None -> () | Some i -> Cli.max_prec_digits := i);
    let l =
      match language with
      | Some l -> l
      | None -> (
          (* Try to infer the language from the intput file extension. *)
          let ext = Filename.extension !filename in
          if ext = "" then
            Errors.raise_error
              (Printf.sprintf
                 "No file extension found for the file '%s'. (Try to add one or to specify the -l \
                  flag)"
                 !filename);
          try List.assoc ext extensions with Not_found -> ext)
    in
    let language =
      try List.assoc l languages
      with Not_found ->
        Errors.raise_error
          (Printf.sprintf "The selected language (%s) is not supported by Catala" l)
    in
    Cli.locale_lang := language;
    let backend =
      let backend = String.lowercase_ascii backend in
      if backend = "makefile" then Cli.Makefile
      else if backend = "latex" then Cli.Latex
      else if backend = "html" then Cli.Html
      else if backend = "interpret" then Cli.Run
      else if backend = "ocaml" then Cli.OCaml
      else if backend = "dcalc" then Cli.Dcalc
      else if backend = "scopelang" then Cli.Scopelang
      else if backend = "python" then Cli.Python
      else
        Errors.raise_error
          (Printf.sprintf "The selected backend (%s) is not supported by Catala" backend)
    in
    let prgm = Surface.Parser_driver.parse_top_level_file source_file language in
    let prgm = Surface.Fill_positions.fill_pos_with_legislative_info prgm in
    match backend with
    | Cli.Makefile ->
        let backend_extensions_list = [ ".tex" ] in
        let source_file =
          match source_file with
          | FileName f -> f
          | Contents _ ->
              Errors.raise_error "The Makefile backend does not work if the input is not a file"
        in
        let output_file =
          match output_file with
          | Some f -> f
          | None -> Filename.remove_extension source_file ^ ".d"
        in
        Cli.debug_print (Format.asprintf "Writing list of dependencies to %s..." output_file);
        let oc = open_out output_file in
        Printf.fprintf oc "%s:\\\n%s\n%s:"
          (String.concat "\\\n"
             (output_file
             :: List.map
                  (fun ext -> Filename.remove_extension source_file ^ ext)
                  backend_extensions_list))
          (String.concat "\\\n" prgm.program_source_files)
          (String.concat "\\\n" prgm.program_source_files);
        0
    | Cli.Latex | Cli.Html ->
        let source_file =
          match source_file with
          | FileName f -> f
          | Contents _ ->
              Errors.raise_error
                "The literate programming backends do not work if the input is not a file"
        in
        Cli.debug_print
          (Printf.sprintf "Weaving literate program into %s"
             (match backend with
             | Cli.Latex -> "LaTeX"
             | Cli.Html -> "HTML"
             | _ -> assert false (* should not happen *)));
        let output_file =
          match output_file with
          | Some f -> f
          | None -> (
              Filename.remove_extension source_file
              ^
              match backend with Cli.Latex -> ".tex" | Cli.Html -> ".html" | _ -> assert false
              (* should not happen *))
        in
        let oc = open_out output_file in
        let weave_output =
          match backend with
          | Cli.Latex -> Literate.Latex.ast_to_latex language
          | Cli.Html -> Literate.Html.ast_to_html language
          | _ -> assert false
          (* should not happen *)
        in
        Cli.debug_print (Printf.sprintf "Writing to %s" output_file);
        let fmt = Format.formatter_of_out_channel oc in
        if wrap_weaved_output then
          match backend with
          | Cli.Latex ->
              Literate.Latex.wrap_latex prgm.Surface.Ast.program_source_files language fmt
                (fun fmt -> weave_output fmt prgm)
          | Cli.Html ->
              Literate.Html.wrap_html prgm.Surface.Ast.program_source_files language fmt (fun fmt ->
                  weave_output fmt prgm)
          | _ -> assert false (* should not happen *)
        else weave_output fmt prgm;
        close_out oc;
        0
    | _ -> (
        Cli.debug_print "Name resolution...";
        let ctxt = Surface.Name_resolution.form_context prgm in
        let scope_uid =
          match (ex_scope, backend) with
          | None, Cli.Run -> Errors.raise_error "No scope was provided for execution."
          | None, _ ->
              snd
                (try Desugared.Ast.IdentMap.choose ctxt.scope_idmap
                 with Not_found ->
                   Errors.raise_error (Printf.sprintf "There isn't any scope inside the program."))
          | Some name, _ -> (
              match Desugared.Ast.IdentMap.find_opt name ctxt.scope_idmap with
              | None ->
                  Errors.raise_error
                    (Printf.sprintf "There is no scope \"%s\" inside the program." name)
              | Some uid -> uid)
        in
        Cli.debug_print "Desugaring...";
        let prgm = Surface.Desugaring.desugar_program ctxt prgm in
        Cli.debug_print "Collecting rules...";
        let prgm = Desugared.Desugared_to_scope.translate_program prgm in
        if backend = Cli.Scopelang then begin
          let fmt, at_end =
            match output_file with
            | Some f ->
                let oc = open_out f in
                (Format.formatter_of_out_channel oc, fun _ -> close_out oc)
            | None -> (Format.std_formatter, fun _ -> ())
          in
          if Option.is_some ex_scope then
            Format.fprintf fmt "%a\n" Scopelang.Print.format_scope
              (scope_uid, Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes)
          else Format.fprintf fmt "%a\n" Scopelang.Print.format_program prgm;
          at_end ();
          exit 0
        end;
        Cli.debug_print "Translating to default calculus...";
        let prgm, prgm_expr, type_ordering =
          Scopelang.Scope_to_dcalc.translate_program prgm scope_uid
        in
        let prgm =
          if optimize then begin
            Cli.debug_print "Optimizing default calculus...";
            Dcalc.Optimizations.optimize_program prgm
          end
          else prgm
        in
        if backend = Cli.Dcalc then begin
          let fmt, at_end =
            match output_file with
            | Some f ->
                let oc = open_out f in
                (Format.formatter_of_out_channel oc, fun _ -> close_out oc)
            | None -> (Format.std_formatter, fun _ -> ())
          in
          if Option.is_some ex_scope then
            Format.fprintf fmt "%a\n"
              (Dcalc.Print.format_expr prgm.decl_ctx)
              (let _, _, e = List.find (fun (name, _, _) -> name = scope_uid) prgm.scopes in
               e)
          else Format.fprintf fmt "%a\n" (Dcalc.Print.format_expr prgm.decl_ctx) prgm_expr;
          at_end ();
          exit 0
        end;
        Cli.debug_print "Typechecking...";
        let _typ = Dcalc.Typing.infer_type prgm.decl_ctx prgm_expr in
        (* Cli.debug_print (Format.asprintf "Typechecking results :@\n%a" Dcalc.Print.format_typ
           typ); *)
        match backend with
        | Cli.Run ->
            Cli.debug_print "Starting interpretation...";
            let results = Dcalc.Interpreter.interpret_program prgm.decl_ctx prgm_expr in
            let out_regex = Re.Pcre.regexp "\\_out$" in
            let results =
              List.map
                (fun ((v1, v1_pos), e1) ->
                  let v1 = Re.Pcre.substitute ~rex:out_regex ~subst:(fun _ -> "") v1 in
                  ((v1, v1_pos), e1))
                results
            in
            let results =
              List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) results
            in
            Cli.debug_print "End of interpretation";
            Cli.result_print
              (Format.asprintf "Computation successful!%s"
                 (if List.length results > 0 then " Results:" else ""));
            List.iter
              (fun ((var, _), result) ->
                Cli.result_print
                  (Format.asprintf "@[<hov 2>%s@ =@ %a@]" var
                     (Dcalc.Print.format_expr prgm.decl_ctx)
                     result))
              results;
            0
        | Cli.OCaml | Cli.Python ->
            Cli.debug_print "Compiling program into lambda calculus...";
            let prgm = Lcalc.Compile_with_exceptions.translate_program prgm in
            let prgm =
              if optimize then begin
                Cli.debug_print "Optimizing lambda calculus...";
                Lcalc.Optimizations.optimize_program prgm
              end
              else prgm
            in
            let source_file =
              match source_file with
              | FileName f -> f
              | Contents _ ->
                  Errors.raise_error "This backend does not work if the input is not a file"
            in
            let output_file (extension : string) : string =
              match output_file with
              | Some f -> f
              | None -> Filename.remove_extension source_file ^ extension
            in
            (match backend with
            | Cli.OCaml ->
                let output_file = output_file ".ml" in
                Cli.debug_print (Printf.sprintf "Writing to %s..." output_file);
                let oc = open_out output_file in
                let fmt = Format.formatter_of_out_channel oc in
                Cli.debug_print "Compiling program into OCaml...";
                Lcalc.To_ocaml.format_program fmt prgm type_ordering;
                close_out oc
            | Cli.Python ->
                let prgm = Scalc.Compile_from_lambda.translate_program prgm in
                let output_file = output_file ".py" in
                Cli.debug_print "Compiling program into Python...";
                Cli.debug_print (Printf.sprintf "Writing to %s..." output_file);
                let oc = open_out output_file in
                let fmt = Format.formatter_of_out_channel oc in
                Scalc.To_python.format_program fmt prgm type_ordering;
                close_out oc
            | _ -> assert false (* should not happen *));
            0
        | _ -> assert false
        (* should not happen *))
  with Errors.StructuredError (msg, pos) ->
    Cli.error_print (Errors.print_structured_error msg pos);
    -1

let main () =
  let return_code = Cmdliner.Term.eval (Cli.catala_t (fun f -> driver (FileName f)), Cli.info) in
  match return_code with
  | `Ok 0 -> Cmdliner.Term.exit (`Ok 0)
  | _ -> Cmdliner.Term.exit (`Error `Term)
