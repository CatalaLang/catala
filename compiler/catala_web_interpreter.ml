open Catala_utils
open Driver
open Js_of_ocaml

(* Register embedded stdlib files in jsoo's /static/ virtual filesystem *)
let () = Stdlib_embedded.register_stdlib ()

(* Force stdlib_internals modules to initialize by referencing them. Without
   this, jsoo may optimize away the module initialization. *)
let () =
  ignore Stdlib_internals.Date_internal.of_ymd;
  ignore Stdlib_internals.Decimal_internal.round_to_decimal;
  ignore Stdlib_internals.List_internal.sequence;
  ignore Stdlib_internals.Money_internal.round_to_decimal;
  ignore Stdlib_internals.Period_internal.sort

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

(* Track registered user modules for cleanup *)
let registered_user_modules : string list ref = ref []

let () =
  Js.export_all
    (object%js
       (* Register a user module file in the virtual filesystem *)
       method registerFile (filename : Js.js_string Js.t)
           (contents : Js.js_string Js.t) =
         let filename = Js.to_string filename in
         let contents = Js.to_string contents in
         let path = user_modules_path ^ "/" ^ filename in
         (* Remove old version if exists *)
         (try Sys.remove path with Sys_error _ -> ());
         Sys_js.create_file ~name:path ~content:contents;
         if not (List.mem filename !registered_user_modules) then
           registered_user_modules := filename :: !registered_user_modules;
         Js._true

       (* Clear all registered user modules *)
       method clearFiles =
         List.iter
           (fun filename ->
             let path = user_modules_path ^ "/" ^ filename in
             try Sys.remove path with Sys_error _ -> ())
           !registered_user_modules;
         registered_user_modules := [];
         Js._true

       (* List registered user modules *)
       method listFiles =
         Js.array (Array.of_list (List.map Js.string !registered_user_modules))

       method interpret (contents : Js.js_string Js.t)
           (scope : Js.js_string Js.t) (language : Js.js_string Js.t)
           (trace : bool) (filename : Js.js_string Js.t) =
         let contents = Js.to_string contents in
         let scope = Js.to_string scope in
         let language_str = Js.to_string language in
         let filename = Js.to_string filename in
         (* Use provided filename or default to "-inline-" if empty *)
         let filename = if filename = "" then "-inline-" else filename in
         let language =
           try List.assoc (String.lowercase_ascii language_str) Cli.languages
           with Not_found ->
             Message.error "Unrecognised input locale %S" language_str
         in
         let options =
           Global.enforce_options
             ~input_src:(Contents (contents, filename))
             ~language:(Some language) ~debug:false ~color:Never
             ~trace:(if trace then Some (lazy Format.std_formatter) else None)
             ~path_rewrite:(fun f -> (f :> Global.file))
             ~whole_program:true ()
         in
         try
           (* Include user modules path if any modules are registered *)
           let includes =
             if !registered_user_modules <> [] then
               [Global.raw_file user_modules_path]
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
         with
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
                     (fun c ->
                       Array.to_list (Js.to_array (extract_positions c)))
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
    end)
