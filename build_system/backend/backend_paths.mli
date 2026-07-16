(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Backend path/command helpers, [~win32]-parameterized so Windows behaviour is
    testable on any host. Internal to [Clerk_backends]. *)

val os_path_sep : win32:bool -> string
(** PYTHONPATH/classpath separator (';' on Windows, ':' elsewhere). *)

val pythonpath : win32:bool -> string list -> string
val classpath : win32:bool -> backend:string -> string list -> string

val jar_argfile_content : (string * string) list -> string
(** Render '-C <dir> <file>' pairs into a jar argfile body (forward-slashed,
    quoted) — avoids the Windows command-line length limit and backslash
    escaping. *)
