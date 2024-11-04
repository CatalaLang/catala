(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type file = string
type raw_file = file
type backend_lang = En | Fr | Pl
type when_enum = Auto | Always | Never
type message_format_enum = Human | GNU | Lsp

type 'file input_src =
  | FileName of 'file
  | Contents of string * 'file
  | Stdin of 'file
(* ['file] is expected to be [file] or [raw_file] *)

type options = {
  mutable input_src : file input_src;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : file list;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
  mutable path_rewrite : raw_file -> file;
  mutable include_resolver : (file -> file input_src) option;
  mutable stop_on_error : bool;
}

(* Note: we force that the global options (ie options common to all commands)
   and the options available through global refs are the same. While this is a
   bit arbitrary, it makes some sense code-wise and provides some safeguard
   against explosion of the number of global references. Reducing the number of
   globals further would be nice though. *)
let options =
  {
    input_src = Stdin "-stdin-";
    language = None;
    debug = false;
    color = Auto;
    message_format = Human;
    trace = false;
    plugins_dirs = [];
    disable_warnings = false;
    max_prec_digits = 20;
    path_rewrite = (fun _ -> assert false);
    include_resolver = None;
    stop_on_error = false;
  }

let enforce_options
    ?input_src
    ?language
    ?debug
    ?color
    ?message_format
    ?trace
    ?plugins_dirs
    ?disable_warnings
    ?max_prec_digits
    ?path_rewrite
    ?include_resolver
    ?stop_on_error
    () =
  Option.iter (fun x -> options.input_src <- x) input_src;
  Option.iter (fun x -> options.language <- x) language;
  Option.iter (fun x -> options.debug <- x) debug;
  Option.iter (fun x -> options.color <- x) color;
  Option.iter (fun x -> options.message_format <- x) message_format;
  Option.iter (fun x -> options.trace <- x) trace;
  Option.iter (fun x -> options.plugins_dirs <- x) plugins_dirs;
  Option.iter (fun x -> options.disable_warnings <- x) disable_warnings;
  Option.iter (fun x -> options.max_prec_digits <- x) max_prec_digits;
  Option.iter (fun x -> options.path_rewrite <- x) path_rewrite;
  Option.iter (fun x -> options.include_resolver <- Some x) include_resolver;
  Option.iter (fun x -> options.stop_on_error <- x) stop_on_error;
  options

let input_src_file = function FileName f | Contents (_, f) | Stdin f -> f
let raw_file f = f
