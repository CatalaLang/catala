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

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (source_file : string) (debug : bool) (wrap_weaved_output : bool)
    (pygmentize_loc : string option) (backend : string) (language : string option)
    (output_file : string option) : int =
  Cli.debug_flag := debug;
  Cli.debug_print "Reading files...";
  if Filename.extension source_file <> ".catala" then begin
    Cli.error_print (Printf.sprintf "Source file %s must have the .catala extension!" source_file);
    exit 1
  end;
  let language =
    match language with
    | Some l ->
        if l = "fr" then `Fr
        else if l = "en" then `En
        else if l = "non-verbose" then `NonVerbose
        else begin
          Cli.error_print (Printf.sprintf "The selected language (%s) is not supported by Catala" l);
          exit 1
        end
    | None -> `NonVerbose
  in
  let backend =
    if backend = "Makefile" then Cli.Makefile
    else if backend = "LaTeX" then Cli.Latex
    else if backend = "HTML" then Cli.Html
    else if backend = "Interpret" then Cli.Interpret
    else begin
      Cli.error_print
        (Printf.sprintf "The selected backend (%s) is not supported by Catala" backend);
      exit 1
    end
  in
  let program = Parser_driver.parse_source_files [ source_file ] language in
  match backend with
  | Cli.Makefile ->
      let backend_extensions_list = [ ".tex" ] in
      let output_file =
        match output_file with Some f -> f | None -> Filename.remove_extension source_file ^ ".d"
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
  | Cli.Latex | Cli.Html -> (
      let language : Cli.backend_lang = Cli.to_backend_lang language in
      Cli.debug_print
        (Printf.sprintf "Weaving literate program into %s"
           (match backend with Cli.Latex -> "LaTeX" | Cli.Html -> "HTML" | _ -> assert false));
      try
        let weaved_output =
          match backend with
          | Cli.Latex -> Latex.ast_to_latex program language
          | Cli.Html -> Html.ast_to_html program pygmentize_loc language
          | _ -> assert false
        in
        let output_file =
          match output_file with
          | Some f -> f
          | None -> (
              Filename.remove_extension source_file
              ^ match backend with Cli.Latex -> ".tex" | Cli.Html -> ".html" | _ -> assert false )
        in
        let weaved_output =
          if wrap_weaved_output then
            match backend with
            | Cli.Latex ->
                Latex.wrap_latex weaved_output program.Ast.program_source_files pygmentize_loc
                  language
            | Cli.Html ->
                Html.wrap_html weaved_output program.Ast.program_source_files pygmentize_loc
                  language
            | _ -> assert false
          else weaved_output
        in
        Cli.debug_print (Printf.sprintf "Writing to %s" output_file);
        let oc = open_out output_file in
        Printf.fprintf oc "%s" weaved_output;
        close_out oc;
        0
      with Errors.WeavingError msg ->
        Cli.error_print msg;
        exit (-1) )
  | Cli.Interpret -> (
      try
        let ctxt = Context.form_context program in
        let prgm = Firstpass.translate_program_to_scope ctxt program in
        Uid.UidMap.iter
          (fun _uid scope ->
            Printf.printf "Execution of scope %s:\n" (Uid.get_ident scope.Scope.scope_uid);
            let exec_ctxt = Interpreter.execute_scope ctxt prgm scope in
            Uid.UidMap.iter
              (fun uid value ->
                Printf.printf "Var %s:\t%s\n" (Uid.get_ident uid)
                  (Debug.print_term ((value, Pos.no_pos), TDummy)))
              exec_ctxt;
            Printf.printf "\n")
          prgm;
        0
      with Errors.ContextError msg | Errors.DefaultConflict msg ->
        Cli.error_print msg;
        exit (-1) )

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.catala_t driver, Cli.info)
