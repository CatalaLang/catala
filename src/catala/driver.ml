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
module Pos = Utils.Pos

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (source_file : Pos.input_file) (debug : bool) (unstyled : bool)
    (wrap_weaved_output : bool) (pygmentize_loc : string option) (backend : string)
    (language : string option) (max_prec_digits : int option) (trace : bool)
    (ex_scope : string option) (output_file : string option) : int =
  try
    Cli.debug_flag := debug;
    Cli.style_flag := not unstyled;
    Cli.trace_flag := trace;
    Cli.debug_print "Reading files...";
    (match source_file with FileName _ -> () | Contents c -> Cli.contents := c);
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
    Cli.locale_lang := Cli.to_backend_lang language;
    let backend =
      if backend = "Makefile" then Cli.Makefile
      else if backend = "LaTeX" then Cli.Latex
      else if backend = "HTML" then Cli.Html
      else if backend = "Interpret" then Cli.Run
      else if backend = "OCaml" then Cli.OCaml
      else
        Errors.raise_error
          (Printf.sprintf "The selected backend (%s) is not supported by Catala" backend)
    in
    let program = Surface.Parser_driver.parse_top_level_file source_file language in
    let program = Surface.Fill_positions.fill_pos_with_legislative_info program in
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
        let source_file =
          match source_file with
          | FileName f -> f
          | Contents _ ->
              Errors.raise_error
                "The literate programming backends do not work if the input is not a file"
        in
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
    | Cli.Run | Cli.OCaml -> (
        Cli.debug_print "Name resolution...";
        let ctxt = Surface.Name_resolution.form_context program in
        let scope_uid =
          match (ex_scope, backend) with
          | None, Cli.Run -> Errors.raise_error "No scope was provided for execution."
          | None, _ -> snd (Desugared.Ast.IdentMap.choose ctxt.scope_idmap)
          | Some name, _ -> (
              match Desugared.Ast.IdentMap.find_opt name ctxt.scope_idmap with
              | None ->
                  Errors.raise_error
                    (Printf.sprintf "There is no scope \"%s\" inside the program." name)
              | Some uid -> uid )
        in
        Cli.debug_print "Desugaring...";
        let prgm = Surface.Desugaring.desugar_program ctxt program in
        Cli.debug_print "Collecting rules...";
        let prgm = Desugared.Desugared_to_scope.translate_program prgm in
        Cli.debug_print "Translating to default calculus...";
        let prgm, prgm_expr, type_ordering =
          Scopelang.Scope_to_dcalc.translate_program prgm scope_uid
        in
        (* Cli.debug_print (Format.asprintf "Output program:@\n%a" (Dcalc.Print.format_expr
           prgm.decl_ctx) prgm_expr); *)
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
        | Cli.OCaml ->
            Cli.debug_print "Compiling program into OCaml...";
            let prgm = Lcalc.Compile_with_exceptions.translate_program prgm in
            let source_file =
              match source_file with
              | FileName f -> f
              | Contents _ ->
                  Errors.raise_error "The OCaml backend does not work if the input is not a file"
            in
            let output_file =
              match output_file with
              | Some f -> f
              | None -> Filename.remove_extension source_file ^ ".ml"
            in
            Cli.debug_print (Printf.sprintf "Writing to %s..." output_file);
            let oc = open_out output_file in
            let fmt = Format.formatter_of_out_channel oc in
            Lcalc.To_ocaml.format_program fmt prgm type_ordering;
            close_out oc;
            0
        | _ -> assert false
        (* should not happen *) )
  with Errors.StructuredError (msg, pos) ->
    Cli.error_print (Errors.print_structured_error msg pos);
    -1

let main () =
  let return_code = Cmdliner.Term.eval (Cli.catala_t (fun f -> driver (FileName f)), Cli.info) in
  match return_code with
  | `Ok 0 -> Cmdliner.Term.exit (`Ok 0)
  | _ -> Cmdliner.Term.exit (`Error `Term)
