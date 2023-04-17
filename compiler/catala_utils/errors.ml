(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Error formatting and helper functions *)

(** {1 Error exception and printing} *)

exception StructuredError of (string * (string option * Pos.t) list)
(** The payload of the expression is a main error message, with a list of
    secondary positions related to the error, each carrying an optional
    secondary message to describe what is pointed by the position. *)

let print_structured_error (msg : string) (pos : (string option * Pos.t) list) :
    string =
  match !Cli.message_format_flag with
  | Cli.Human ->
    Printf.sprintf "%s%s%s" msg
      (if pos = [] then "" else "\n\n")
      (String.concat "\n\n"
         (List.map
            (fun (msg, pos) ->
              Printf.sprintf "%s%s"
                (match msg with None -> "" | Some msg -> msg ^ "\n")
                (Pos.retrieve_loc_text pos))
            pos))
  | Cli.EditorParsable ->
    let remove_new_lines s =
      Re.replace ~all:true (Re.compile (Re.char '\n')) ~f:(fun _ -> " | ") s
    in
    "\n"
    ^ String.concat "\n"
        (List.map
           (fun (msg', pos) ->
             Printf.sprintf "%s%s" (Pos.to_string_short pos)
               (match msg' with
               | None -> remove_new_lines msg
               | Some msg' ->
                 remove_new_lines msg ^ " | " ^ remove_new_lines msg'))
           pos)

(** {1 Error exception and printing} *)

let raise_spanned_error ?(span_msg : string option) (span : Pos.t) format =
  Format.kasprintf
    (fun msg -> raise (StructuredError (msg, [span_msg, span])))
    format

let raise_multispanned_error (spans : (string option * Pos.t) list) format =
  Format.kasprintf (fun msg -> raise (StructuredError (msg, spans))) format

let raise_error format =
  Format.kasprintf (fun msg -> raise (StructuredError (msg, []))) format

let raise_internal_error format =
  raise_error
    ("Internal Error, please report to \
      https://github.com/CatalaLang/catala/issues: "
    ^^ format)

(** {1 Warning printing}*)
let assert_internal_error condition fmt =
  if condition then raise_internal_error ("assertion failed: " ^^ fmt)
  else Format.ifprintf (Format.formatter_of_out_channel stdout) fmt

let format_multispanned_warning (pos : (string option * Pos.t) list) format =
  Format.kasprintf
    (fun msg -> Cli.warning_print "%s" (print_structured_error msg pos))
    format

let format_spanned_warning ?(span_msg : string option) (span : Pos.t) format =
  format_multispanned_warning [span_msg, span] format

let format_warning format = format_multispanned_warning [] format
