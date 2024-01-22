open Catala_utils
open Driver
open Js_of_ocaml

let () =
  Js.export_all
    (object%js
       method interpret
           (contents : Js.js_string Js.t)
           (scope : Js.js_string Js.t)
           (language : Js.js_string Js.t)
           (trace : bool) =
         let contents = Js.to_string contents in
         let scope = Js.to_string scope in
         let language = Js.to_string language in
         let language =
           try List.assoc (String.lowercase_ascii language) Cli.languages
           with Not_found ->
             Message.raise_error "Unrecognised input locale %S" language
         in
         let options =
           Cli.enforce_globals
             ~input_src:(Contents (contents, "-inline-"))
             ~language:(Some language) ~debug:false ~color:Never ~trace ()
         in
         let prg, _type_order =
           Passes.dcalc options ~includes:[] ~optimize:false
             ~check_invariants:false ~typed:Shared_ast.Expr.typed
         in
         Shared_ast.Interpreter.interpret_program_dcalc prg
           (Commands.get_scope_uid prg.decl_ctx scope)
    end)
