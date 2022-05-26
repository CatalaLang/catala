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

(** This file demonstrates the use of backend plugins for Catala. It's a simple
    wrapper on top of the OCaml backend that calls js_of_ocaml on the generated
    code. Not for production use. *)

let name = "jsoo"
let extension = ".js"

let finalise e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

let finally f k =
  match k () with
  | r ->
    f ();
    r
  | exception e -> finalise e f

let with_open_out file f =
  let oc = open_out file in
  finally (fun () -> close_out oc) (fun () -> f oc)

let with_temp_file pfx sfx f =
  let tmp = Filename.temp_file pfx sfx in
  match f tmp with
  | r ->
    Sys.remove tmp;
    r
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    Sys.remove tmp;
    Printexc.raise_with_backtrace e bt

let apply output_file prgm type_ordering =
  with_temp_file "catala_jsoo_" ".ml" @@ fun ml_file ->
  with_open_out ml_file (fun oc ->
      Lcalc.To_ocaml.format_program
        (Format.formatter_of_out_channel oc)
        prgm type_ordering;
      with_temp_file "catala_jsoo_" ".byte" @@ fun bytecode_file ->
      if
        Sys.command
          (Printf.sprintf
             "ocamlfind ocamlc -package catala.runtime -linkpkg %S -o %S"
             ml_file bytecode_file)
        <> 0
      then failwith "ocaml err";
      Utils.Cli.debug_print "OCaml compil ok";
      if
        Sys.command
          (Printf.sprintf
             "js_of_ocaml +zarith_stubs_js/biginteger.js \
              +zarith_stubs_js/runtime.js %S -o %S"
             bytecode_file output_file)
        <> 0
      then failwith "jsoo err";
      Utils.Cli.debug_print "Jsoo compil ok, output in %s" output_file)

let () = Driver.Plugin.register_lcalc ~name ~extension apply
