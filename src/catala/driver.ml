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

open Cli
module I = Ir

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver (source_files : string list) (debug : bool) (wrap_latex_output : bool)
    (pygmentize_loc : string option) (backend : string) (output_file : string) : int =
  Cli.debug_flag := debug;
  Cli.debug_print "Reading files...";
  let program = ref [] in
  List.iter
    (fun source_file ->
      let input = open_in source_file in
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let lexbuf =
        Sedlex_menhir.create_lexbuf ~file:(Filename.basename source_file)
          (Sedlexing.Utf8.from_channel input)
      in
      try
        Parse_utils.current_file := source_file;
        let commands = Sedlex_menhir.sedlex_with_menhir Lexer.lexer Parser.source_file lexbuf in
        program := commands :: !program;
        close_in input
      with Errors.ParsingError msg ->
        error_print msg;
        close_in input;
        exit (-1))
    source_files;
  if backend = "LaTeX" then begin
    Cli.debug_print (Printf.sprintf "Weaving literate program into LaTeX");
    let weaved_output =
      String.concat "\n\n" (List.map (fun file -> Weave.ast_to_latex file) !program)
    in
    let weaved_output =
      if wrap_latex_output then Weave.wrap_latex weaved_output source_files pygmentize_loc
      else weaved_output
    in
    Cli.debug_print (Printf.sprintf "Writing to %s" output_file);
    let oc = open_out output_file in

    Printf.fprintf oc "%s" weaved_output;
    close_out oc;
    0
  end
  else begin
    Cli.error_print (Printf.sprintf "Unkown backend: %s" backend);
    1
  end

let main () = Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.catala_t driver, Cli.info)
