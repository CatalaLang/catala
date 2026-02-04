(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Emile Rolley <emile.rolley@tuta.io>, Louis Gesbert
   <louis.gesbert@inria.fr>, Romain Primet <romain.primet@inria.fr>

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
open Driver
open Js_of_ocaml

(* Register embedded stdlib files in jsoo's /static/ virtual filesystem *)
let () = Stdlib_embedded.register_stdlib ()

(* Format interpretation results as human-readable text *)
let format_results language results =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  List.iter
    (fun ((var, _), result) ->
      Format.fprintf ppf "@[<hov 2>%s =@ %a@]@\n" var
        (Shared_ast.Print.UserFacing.value language)
        result)
    results;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Format error content as human-readable text *)
let format_error content =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Message.Content.emit ~ppf content Message.Error;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Extract error positions from content as JS objects *)
let extract_positions content =
  let positions =
    List.filter_map
      (fun (pos, pos_message) ->
        let file = Pos.get_file pos in
        (* Include positions from user's code, exclude stdlib *)
        if not (String.starts_with ~prefix:Stdlib_embedded.stdlib_path file)
        then
          let msg =
            match pos_message with
            | Some fmt ->
              let buf = Buffer.create 64 in
              let ppf = Format.formatter_of_buffer buf in
              fmt ppf;
              Format.pp_print_flush ppf ();
              Buffer.contents buf
            | None -> ""
          in
          Some
            (object%js
               val startLine = Pos.get_start_line pos
               val startColumn = Pos.get_start_column pos
               val endLine = Pos.get_end_line pos
               val endColumn = Pos.get_end_column pos
               val message = Js.string msg
            end)
        else None)
      (Message.Content.get_positions content)
  in
  Js.array (Array.of_list positions)

(* User modules path in virtual filesystem *)
let user_modules_path = "/static/user"

(* Helper to extract files and options from JS object *)
let setup_files (js_options : 'a Js.t) =
  let files_obj = Js.Unsafe.get js_options (Js.string "files") in
  let language_str =
    Js.Optdef.get
      (Js.Unsafe.get js_options (Js.string "language"))
      (fun () -> Js.string "en")
    |> Js.to_string
  in
  let main_opt =
    Js.Optdef.to_option (Js.Unsafe.get js_options (Js.string "main"))
    |> Option.map Js.to_string
  in
  (* Extract files from JS object, preserving order *)
  let file_keys = Js.object_keys files_obj |> Js.to_array |> Array.to_list in
  let all_files =
    List.map
      (fun key ->
        let content = Js.Unsafe.get files_obj key |> Js.to_string in
        Js.to_string key, content)
      file_keys
  in
  (* Determine main file: explicit main param, or first file *)
  let main_filename, main_contents =
    match main_opt with
    | Some main_name -> (
      match List.find_opt (fun (name, _) -> name = main_name) all_files with
      | Some (name, content) -> name, content
      | None -> Message.error "Main file not found: %s" main_name)
    | None -> (
      match all_files with
      | (name, content) :: _ -> name, content
      | [] -> Message.error "No files provided")
  in
  (* Other files (not main) are registered as modules *)
  let module_files =
    List.filter (fun (name, _) -> name <> main_filename) all_files
  in
  let language =
    try List.assoc (String.lowercase_ascii language_str) Cli.languages
    with Not_found ->
      Message.error "Unrecognised input locale %S" language_str
  in
  (* Create virtual files for modules *)
  List.iter
    (fun (name, content) ->
      let path = user_modules_path ^ "/" ^ name in
      Sys_js.create_file ~name:path ~content)
    module_files;
  language, main_filename, main_contents, module_files

(* Helper to clean up virtual files *)
let cleanup_files module_files =
  List.iter
    (fun (name, _) ->
      let path = user_modules_path ^ "/" ^ name in
      try Sys.remove path with Sys_error _ -> ())
    module_files

(* Helper to handle errors and return JS result object *)
let handle_error exn =
  match exn with
  | Message.CompilerError content ->
    let error_text = format_error content in
    let positions = extract_positions content in
    object%js
      val success = Js._false
      val output = Js.string ""
      val error = Js.string error_text
      val errorPositions = positions
    end
  | Message.CompilerErrors contents_with_bt ->
    let all_contents = List.map fst contents_with_bt in
    let error_text =
      String.concat "\n\n" (List.map format_error all_contents)
    in
    let positions =
      Js.array
        (Array.of_list
           (List.concat_map
              (fun c -> Array.to_list (Js.to_array (extract_positions c)))
              all_contents))
    in
    object%js
      val success = Js._false
      val output = Js.string ""
      val error = Js.string error_text
      val errorPositions = positions
    end
  | e ->
    object%js
      val success = Js._false
      val output = Js.string ""
      val error = Js.string (Printexc.to_string e)
      val errorPositions = Js.array [||]
    end

let () =
  Js.export_all
    (object%js
       method typecheck (js_options : 'a Js.t) =
         let language, main_filename, main_contents, module_files =
           setup_files js_options
         in
         let options =
           Global.enforce_options
             ~input_src:(Contents (main_contents, main_filename))
             ~language:(Some language) ~debug:false ~color:Never ~trace:None
             ~path_rewrite:(fun f -> (f :> Global.file))
             ~whole_program:true ()
         in
         let result =
           try
             let includes =
               if module_files <> [] then [Global.raw_file user_modules_path]
               else []
             in
             (* Parse and get scopelang representation *)
             let prg =
               Passes.scopelang options ~allow_external:true ~includes
                 ~stdlib:(Some (Global.raw_file Stdlib_embedded.stdlib_path))
             in
             (* Check type cycles *)
             let _type_ordering =
               Scopelang.Dependency.check_type_cycles
                 prg.program_ctx.ctx_abstract_types prg.program_ctx.ctx_structs
                 prg.program_ctx.ctx_enums
             in
             (* Type the program *)
             let prg = Scopelang.Ast.type_program prg in
             (* Translate to dcalc for full name-resolution and cycle
                detection *)
             let _ = Dcalc.From_scopelang.translate_program prg in
             Message.report_delayed_errors_if_any ();
             object%js
               val success = Js._true
               val output = Js.string "Typechecking successful!"
               val error = Js.string ""
               val errorPositions = Js.array [||]
             end
           with exn -> handle_error exn
         in
         cleanup_files module_files;
         result

       method interpret (js_options : 'a Js.t) =
         let language, main_filename, main_contents, module_files =
           setup_files js_options
         in
         let scope =
           Js.Unsafe.get js_options (Js.string "scope") |> Js.to_string
         in
         let trace =
           Js.Optdef.get
             (Js.Unsafe.get js_options (Js.string "trace"))
             (fun () -> Js._false)
           |> Js.to_bool
         in
         let options =
           Global.enforce_options
             ~input_src:(Contents (main_contents, main_filename))
             ~language:(Some language) ~debug:false ~color:Never
             ~trace:(if trace then Some (lazy Format.std_formatter) else None)
             ~path_rewrite:(fun f -> (f :> Global.file))
             ~whole_program:true ()
         in
         let result =
           try
             let includes =
               if module_files <> [] then [Global.raw_file user_modules_path]
               else []
             in
             let prg, _type_order =
               Passes.dcalc options ~includes
                 ~stdlib:(Some (Global.raw_file Stdlib_embedded.stdlib_path))
                 ~optimize:false ~check_invariants:false ~autotest:false
                 ~typed:Shared_ast.Expr.typed
             in
             let results =
               Shared_ast.Interpreter.interpret_program_dcalc prg
                 (Commands.get_scope_uid prg.decl_ctx scope)
             in
             let formatted = format_results language results in
             object%js
               val success = Js._true
               val output = Js.string formatted
               val error = Js.string ""
               val errorPositions = Js.array [||]
             end
           with exn -> handle_error exn
         in
         cleanup_files module_files;
         result
    end)
