(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

let with_formatter_of_out_channel oc f =
  let fmt = Format.formatter_of_out_channel oc in
  match f fmt with
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    Format.pp_print_flush fmt ();
    Printexc.raise_with_backtrace e bt
  | res ->
    Format.pp_print_flush fmt ();
    res

let with_formatter_of_file filename f =
  let oc = open_out filename in
  let res = with_formatter_of_out_channel oc f in
  close_out oc;
  res

let with_formatter_of_opt_file filename_opt f =
  match filename_opt with
  | None -> f Format.std_formatter
  | Some filename -> with_formatter_of_file filename f
