(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Emile Rolley <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Run finaliser [f] unconditionally after running [k ()], propagating any
    raised exception. *)
let finally f k =
  match k () with
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    f ();
    Printexc.raise_with_backtrace e bt
  | r ->
    f ();
    r

let with_out_channel filename f =
  let oc = open_out filename in
  finally (fun () -> close_out oc) (fun () -> f oc)

let with_in_channel filename f =
  let oc = open_in filename in
  finally (fun () -> close_in oc) (fun () -> f oc)

let with_formatter_of_out_channel oc f =
  let fmt = Format.formatter_of_out_channel oc in
  finally (fun () -> Format.pp_print_flush fmt ()) @@ fun () -> f fmt

let with_formatter_of_file filename f =
  with_out_channel filename (fun oc -> with_formatter_of_out_channel oc f)

let with_formatter_of_opt_file filename_opt f =
  match filename_opt with
  | None -> finally (fun () -> flush stdout) (fun () -> f Format.std_formatter)
  | Some filename -> with_formatter_of_file filename f

let ocamlformat_file_opt = function
  | Some f ->
    Cli.debug_print "Formatting %s..." f;
    if Sys.command (Printf.sprintf "ocamlformat %s -i" f) <> 0 then
      failwith ("Internal error: ocamlformat failed on " ^ f)
  | None -> ()
