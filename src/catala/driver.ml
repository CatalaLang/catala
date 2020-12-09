(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Cli = Utils.Cli
module Errors = Utils.Errors

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (source_file : string) (debug : bool) (unstyled : bool) (wrap_weaved_output : bool)
    (pygmentize_loc : string option) (backend : string) (language : string option)
    (max_prec_digits : int option) (ex_scope : string option) (output_file : string option) : int =
  try
    Cli.debug_flag := debug;
    Cli.style_flag := not unstyled;
    Cli.debug_print "Reading files...";
    (match max_prec_digits with None -> () | Some i -> Cli.max_prec_digits := i);
    let language =
      match language with
      | Some l ->
          if l = "fr" then `Fr
          else if l = "en" then `En
          else if l = "non-verbose" then `NonVerbose
          else
            Errors.raise_error
              (Printf.sprintf "The selected language (%s) is not supported by Catala" l)
      | None -> `NonVerbose
    in
    let backend =
      if backend = "Makefile" then Cli.Makefile
      else if backend = "LaTeX" then Cli.Latex
      else if backend = "HTML" then Cli.Html
      else if backend = "Interpret" then Cli.Run
      else
        Errors.raise_error
          (Printf.sprintf "The selected backend (%s) is not supported by Catala" backend)
    in
    let program = Surface.Parser_driver.parse_source_files [ source_file ] language in
    match backend with
    | Cli.Makefile ->
        let backend_extensions_list = [ ".tex" ] in
        let output_file =
          match output_file with
          | Some f -> f
          | None -> Filename.remove_extension source_file ^ ".d"
        in
        let oc = open_out output_file in
        Printf.fprintf oc "%s:\\\n%s\n%s:"
          (String.concat "\\\n"
             ( output_file
             :: List.map
                  (fun ext -> Filename.remove_extension source_file ^ ext)
                  backend_extensions_list ))
          (String.concat "\\\n" program.program_source_files)
          (String.concat "\\\n" program.program_source_files);
        0
    | Cli.Latex | Cli.Html ->
        let language : Cli.backend_lang = Cli.to_backend_lang language in
        Cli.debug_print
          (Printf.sprintf "Weaving literate program into %s"
             ( match backend with
             | Cli.Latex -> "LaTeX"
             | Cli.Html -> "HTML"
             | _ -> assert false (* should not happen *) ));
        let output_file =
          match output_file with
          | Some f -> f
          | None -> (
              Filename.remove_extension source_file
              ^
              match backend with Cli.Latex -> ".tex" | Cli.Html -> ".html" | _ -> assert false
              (* should not happen *) )
        in
        let oc = open_out output_file in
        let weave_output =
          match backend with
          | Cli.Latex -> Literate.Latex.ast_to_latex language
          | Cli.Html -> Literate.Html.ast_to_html pygmentize_loc language
          | _ -> assert false
          (* should not happen *)
        in
        Cli.debug_print (Printf.sprintf "Writing to %s" output_file);
        let fmt = Format.formatter_of_out_channel oc in
        if wrap_weaved_output then
          match backend with
          | Cli.Latex ->
              Literate.Latex.wrap_latex program.Surface.Ast.program_source_files pygmentize_loc
                language fmt (fun fmt -> weave_output fmt program)
          | Cli.Html ->
              Literate.Html.wrap_html program.Surface.Ast.program_source_files pygmentize_loc
                language fmt (fun fmt -> weave_output fmt program)
          | _ -> assert false (* should not happen *)
        else weave_output fmt program;
        close_out oc;
        0
    | Cli.Run ->
        Cli.debug_print "Name resolution...";
        let ctxt = Surface.Name_resolution.form_context program in
        let scope_uid =
          match ex_scope with
          | None -> Errors.raise_error "No scope was provided for execution."
          | Some name -> (
              match Desugared.Ast.IdentMap.find_opt name ctxt.scope_idmap with
              | None ->
                  Errors.raise_error
                    (Printf.sprintf "There is no scope %s inside the program." name)
              | Some uid -> uid )
        in
        Cli.debug_print "Desugaring...";
        let prgm = Surface.Desugaring.desugar_program ctxt program in
        Cli.debug_print "Collecting rules...";
        let prgm = Desugared.Desugared_to_scope.translate_program prgm in
        Cli.debug_print "Translating to default calculus...";
        let prgm = Scopelang.Scope_to_dcalc.translate_program prgm scope_uid in
        Cli.debug_print (Format.asprintf "Output program:@\n%a" Dcalc.Print.format_expr prgm);
        let typ = Dcalc.Typing.infer_type prgm in
        Cli.debug_print (Format.asprintf "Typechecking results :@\n%a" Dcalc.Print.format_typ typ);
        let results = Dcalc.Interpreter.interpret_program prgm in
        let results =
          List.sort
            (fun (v1, _) (v2, _) -> String.compare (Bindlib.name_of v1) (Bindlib.name_of v2))
            results
        in
        List.iter
          (fun (var, result) ->
            Cli.result_print
              (Format.asprintf "%s -> %a" (Bindlib.name_of var) Dcalc.Print.format_expr result))
          results;
        0
  with Errors.StructuredError (msg, pos) ->
    Cli.error_print (Errors.print_structured_error msg pos);
    exit (-1)

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.catala_t driver, Cli.info)
