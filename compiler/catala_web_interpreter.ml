open Driver
open Js_of_ocaml

let _ =
  Js.export_all
    (object%js
       method interpret (contents : Js.js_string Js.t) (scope : Js.js_string Js.t)
           (language : Js.js_string Js.t) (trace : bool) =
         driver
           (Contents (Js.to_string contents))
           {
             Utils.Cli.debug = false;
             unstyled = false;
             wrap_weaved_output = false;
             avoid_exceptions = false;
             backend = "Interpret";
             plugins_dirs = [];
             language = Some (Js.to_string language);
             max_prec_digits = None;
             trace = false;
             disable_counterexamples = false;
             optimize = false;
             ex_scope = Some (Js.to_string scope);
             output_file = None;
           }
    end)
