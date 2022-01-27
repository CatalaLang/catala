open Driver
open Js_of_ocaml

let _ =
  Js.export_all
    (object%js
       method interpret (contents : Js.js_string Js.t) (scope : Js.js_string Js.t)
           (language : Js.js_string Js.t) (trace : bool) =
         driver
           (Contents (Js.to_string contents))
           false false false "Interpret"
           (Some (Js.to_string language))
           None trace false false
           (Some (Js.to_string scope))
           None
    end)
