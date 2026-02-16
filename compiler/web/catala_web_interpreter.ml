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
module File = Catala_utils.File

let stdlib_path = "/static"

let format_results ppf scope language results =
  Message.results ~ppf ~title:scope
    (List.map
       (fun ((var, _), result) ppf ->
         Format.fprintf ppf "@[<hov 2>%s =@ %a@]" var
           (Shared_ast.Print.UserFacing.value language)
           result)
       results)

(* Terminal width: JS-configurable, avoids Unix.pipe calls from tput in jsoo *)
let terminal_width = ref 80
let () = Message.set_terminal_width_function (fun () -> !terminal_width)

(* Notification capture infrastructure *)
let notifications_acc : Message.lsp_error list ref = ref []

let () =
  Message.register_lsp_error_notifier (fun err ->
      notifications_acc := err :: !notifications_acc)

(* Convert a position to a JS object *)
let js_pos pos =
  object%js
    val startLine = Pos.get_start_line pos
    val startColumn = Pos.get_start_column pos
    val endLine = Pos.get_end_line pos
    val endColumn = Pos.get_end_column pos
    val message = Js.string ""
  end

(* Build a diagnostic JS object from an lsp_error *)
let diagnostic_of_lsp_error level_str (err : Message.lsp_error) =
  let message_str = Format.asprintf "%t" err.message in
  let positions =
    match err.pos with
    | Some pos
      when not (String.starts_with ~prefix:stdlib_path (Pos.get_file pos)) ->
      [| js_pos pos |]
    | _ -> [||]
  in
  object%js
    val level = Js.string level_str
    val message = Js.string message_str
    val positions = Js.array positions
  end

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

(* Drain all accumulated notifications, emptying the accumulator. Returns
   (warning_diags, error_notifs) where warning_diags are already converted to JS
   diagnostic objects and error_notifs are raw lsp_errors. *)
let drain_all () =
  let all = List.rev !notifications_acc in
  notifications_acc := [];
  let warnings, errors =
    List.partition (fun (e : Message.lsp_error) -> e.kind = Message.Warning) all
  in
  let warning_diags = List.map (diagnostic_of_lsp_error "warning") warnings in
  let error_notifs = errors in
  warning_diags, error_notifs

(* Extract JS position objects from a list of lsp_errors, filtering stdlib *)
let positions_of_notifications notifs =
  List.filter_map
    (fun (e : Message.lsp_error) ->
      match e.pos with
      | Some pos
        when not (String.starts_with ~prefix:stdlib_path (Pos.get_file pos)) ->
        Some (js_pos pos)
      | _ -> None)
    notifs

(* Format an exception into a list of error diagnostic JS objects. Does not
   touch the notification accumulator. *)
let error_diagnostics error_notifs exn =
  let positions = positions_of_notifications error_notifs in
  match exn with
  | Message.CompilerError content ->
    Message.Content.emit ~ppf:Format.str_formatter content Message.Error;
    let message_str = Format.flush_str_formatter () in
    [
      object%js
        val level = Js.string "error"
        val message = Js.string message_str
        val positions = Js.array (Array.of_list positions)
      end;
    ]
  | Message.CompilerErrors contents_with_bt ->
    Message.Content.emit_n ~ppf:Format.str_formatter contents_with_bt
      Message.Error;
    let message_str = Format.flush_str_formatter () in
    [
      object%js
        val level = Js.string "error"
        val message = Js.string message_str
        val positions = Js.array (Array.of_list positions)
      end;
    ]
  | e ->
    [
      object%js
        val level = Js.string "error"
        val message = Js.string (Printexc.to_string e)
        val positions = Js.array [||]
      end;
    ]

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
             ~disable_warnings:false
             ~path_rewrite:(fun f -> (f :> File.t))
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
                 ~stdlib:(Some (Global.raw_file stdlib_path))
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
             let warning_diags, _error_notifs = drain_all () in
             Message.results ~ppf:Format.str_formatter
               [(fun ppf -> Format.fprintf ppf "Typechecking successful!")];
             let output = Format.flush_str_formatter () in
             object%js
               val success = Js._true
               val output = Js.string output
               val diagnostics = Js.array (Array.of_list warning_diags)
             end
           with exn ->
             let warning_diags, error_notifs = drain_all () in
             let error_diags = error_diagnostics error_notifs exn in
             object%js
               val success = Js._false
               val output = Js.string ""

               val diagnostics =
                 Js.array (Array.of_list (warning_diags @ error_diags))
             end
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
             ~disable_warnings:false
             ~trace:(if trace then Some (lazy Format.std_formatter) else None)
             ~path_rewrite:(fun f -> (f :> File.t))
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
                 ~stdlib:(Some (Global.raw_file stdlib_path))
                 ~optimize:false ~check_invariants:false ~autotest:false
                 ~typed:Shared_ast.Expr.typed
             in
             let results =
               Shared_ast.Interpreter.interpret_program_dcalc prg
                 (Commands.get_scope_uid prg.decl_ctx scope)
             in
             (match results with
             | [] ->
               Message.results ~ppf:Format.str_formatter
                 [(fun ppf -> Format.fprintf ppf "Computation successful!")]
             | _ ->
               let results =
                 List.sort
                   (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2)
                   results
               in
               format_results Format.str_formatter scope language results);
             let formatted = Format.flush_str_formatter () in
             let warning_diags, _error_notifs = drain_all () in
             object%js
               val success = Js._true
               val output = Js.string formatted
               val diagnostics = Js.array (Array.of_list warning_diags)
             end
           with exn ->
             let warning_diags, error_notifs = drain_all () in
             let error_diags = error_diagnostics error_notifs exn in
             object%js
               val success = Js._false
               val output = Js.string ""

               val diagnostics =
                 Js.array (Array.of_list (warning_diags @ error_diags))
             end
         in
         cleanup_files module_files;
         result

       method setTerminalWidth (w : int) = terminal_width := w
    end)
