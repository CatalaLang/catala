(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>.

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

let action_flag =
  let docs = "ACTIONS" in
  let open Cmdliner.Arg in
  required
  & vflag None
      [
        ( Some `Compile,
          info ["compile"] ~docs
            ~doc:
              "Compiles a Catala file into a module: a $(b,.cmxs) file that \
               can be used by the Catala interpreter."
          (* "and $(b,cmo) and $(b,cmx) files that can be linked into an OCaml
             program" *) );
        ( Some `Link,
          info ["link"] ~docs
            ~doc:
              "Compiles and links a catala program into a binary (using the \
               ocaml backend). Specify a main scope using the $(b,--scope) \
               flag to be run upon execution. This is still pretty useless at \
               the moment besides for testing purposes, as there is no way to \
               feed input to the generated program, and the output will be \
               silent. Assertions will be checked, though." );
      ]

let gen_ocaml options link_modules optimize check_invariants modname main =
  let prg, ctx, type_ordering =
    Driver.Passes.lcalc options ~link_modules ~optimize ~check_invariants
      ~avoid_exceptions:false ~closure_conversion:false
  in
  let exec_scope = Option.map (Driver.Commands.get_scope_uid ctx) main in
  let filename, with_output =
    Driver.Commands.get_output_format options ~ext:".ml" None
  in
  with_output
  @@ fun ppf ->
  Lcalc.To_ocaml.format_program ppf ?register_module:modname ?exec_scope prg
    type_ordering;
  Option.get filename

let run_process cmd args =
  Message.emit_debug "Running @[<hov 4>@{<yellow>@{<bold>%s@} %a@}@}@]" cmd
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
    args;
  match
    Unix.waitpid []
      (Unix.create_process cmd
         (Array.of_list (cmd :: args))
         Unix.stdin Unix.stdout Unix.stderr)
  with
  | _, Unix.WEXITED 0 -> ()
  | _, _ -> Message.raise_error "Child process @{<bold>%s@} failed" cmd

let with_flag flag args =
  List.fold_right (fun p acc -> flag :: p :: acc) args []

let ocaml_libdir =
  lazy
    (try String.trim (File.process_out "opam" ["var"; "lib"])
     with Failure _ -> (
       try String.trim (File.process_out "ocamlc" ["-where"])
       with Failure _ -> (
         match File.(check_directory (Sys.executable_name / ".." / "lib")) with
         | Some d -> d
         | None ->
           Message.raise_error
             "Could not locate the OCaml library directory, make sure OCaml or \
              opam is installed")))

let runtime_dir =
  lazy
    (match
       List.find_map File.check_directory
         [
           "_build/install/default/lib/catala/runtime_ocaml";
           (* Relative dir when running from catala source *)
           File.(Lazy.force ocaml_libdir / "catala" / "runtime");
         ]
     with
    | Some dir ->
      Message.emit_debug "Catala runtime libraries found at @{<bold>%s@}." dir;
      dir
    | None ->
      Message.raise_error
        "Could not locate the Catala runtime library.@ Make sure that either \
         catala is correctly installed,@ or you are running from the root of a \
         compiled source tree.")

let compile options link_modules optimize check_invariants =
  let modname =
    match options.Cli.input_file with
    (* TODO: extract module name from directives *)
    | FileName n -> Driver.modname_of_file n
    | _ -> Message.raise_error "Input must be a file name for this command"
  in
  let basename = String.uncapitalize_ascii modname in
  let ml_file =
    gen_ocaml options link_modules optimize check_invariants (Some modname) None
  in
  let flags = ["-I"; Lazy.force runtime_dir] in
  let shared_out = File.((Filename.dirname ml_file / basename) ^ ".cmxs") in
  Message.emit_debug "Compiling OCaml shared object file @{<bold>%s@}..."
    shared_out;
  run_process "ocamlopt" ("-shared" :: ml_file :: "-o" :: shared_out :: flags);
  (* let byte_out = basename ^ ".cmo" in
   * Message.emit_debug "Compiling OCaml byte-code object file @{<bold>%s@}..." byte_out;
   * run_process "ocamlc" ("-c" :: ml_file :: "-o" :: byte_out :: flags);
   * let native_out = basename ^ ".cmx" in
   * Message.emit_debug "Compiling OCaml native object file @{<bold>%s@}..." native_out;
   * run_process "ocamlopt" ("-c" :: ml_file :: "-o" :: native_out ::flags); *)
  Message.emit_debug "Done."

let link options link_modules optimize check_invariants output ex_scope_opt =
  let ml_file =
    gen_ocaml options link_modules optimize check_invariants None ex_scope_opt
  in
  (* NOTE: assuming native target at the moment *)
  let cmd = "ocamlopt" in
  let ocaml_libdir = Lazy.force ocaml_libdir in
  let runtime_dir = Lazy.force runtime_dir in
  (* Recursive dependencies are expanded manually here. A shorter version would
     use [ocamlfind ocalmopt -linkpkg -package] with just ppx_yojson_conv_lib,
     zarith and dates_calc *)
  let link_libs =
    [
      "biniou";
      "easy-format";
      "yojson";
      "ppx_yojson_conv_lib";
      "zarith";
      "dates_calc";
    ]
  in
  let link_libdirs =
    List.map
      (fun lib ->
        match File.(check_directory (ocaml_libdir / lib)) with
        | None ->
          Message.raise_error
            "Required OCaml library not found at @{<bold>%s@}.@ Try `opam \
             install %s'"
            File.(ocaml_libdir / lib)
            lib
        | Some l -> l)
      link_libs
  in
  let runtime_lib = File.(runtime_dir / "runtime_ocaml.cmxa") in
  let modules =
    List.map (fun m -> Filename.remove_extension m ^ ".ml") link_modules
  in
  let output =
    match output with
    | Some o -> o
    | None -> Filename.remove_extension ml_file ^ ".exe"
  in
  let args =
    with_flag "-I" link_libdirs
    @ List.map
        (fun lib -> String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa")
        link_libs
    @ ("-I" :: runtime_dir :: runtime_lib :: modules)
    @ [ml_file; "-o"; output]
  in
  run_process cmd args;
  Message.emit_result "Successfully generated @{<bold>%s@}" output
(* Compile from ml and link the modules cmx. => ocamlfind ocamlopt -linkpkg
   -package ppx_yojson_conv_lib -package zarith -package dates_calc -I
   _build/default/runtimes/ocaml/.runtime_ocaml.objs/byte
   _build/default/runtimes/ocaml/runtime_ocaml.cmxa ext.cmx extuse.ml *)

let run
    action
    link_modules
    optimize
    check_invariants
    output
    ex_scope_opt
    options =
  match action with
  | `Compile -> compile options link_modules optimize check_invariants
  | `Link ->
    link options link_modules optimize check_invariants ex_scope_opt output

let term =
  let open Cmdliner.Term in
  const run
  $ action_flag
  $ Cli.Flags.link_modules
  $ Cli.Flags.optimize
  $ Cli.Flags.check_invariants
  $ Cli.Flags.ex_scope_opt
  $ Cli.Flags.output

let () =
  Driver.Plugin.register "module" term
    ~doc:
      "This plugin provides a few experimental tools related to module \
       generation and compilation"
