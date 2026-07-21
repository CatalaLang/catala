(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Romain Primet <romain.primet@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Backend path/command helpers. Windows behaviour is testable on any host via
    [Path.win32]. Internal to [Clerk_backends]. *)

val pythonpath : string list -> string
(** PYTHONPATH value: the given dirs joined with the OS path-list separator (';'
    on Windows, ':' elsewhere). *)

val classpath : backend:string -> string list -> string
(** Java [-cp] value: the backend output dirs ([${tdir}/<backend>], then each
    include dir's [<backend>] subdir) joined with the OS path-list separator.
    Contains ninja variable references, for use in rule commands. *)

val jar_argfile_content : (string * string) list -> string
(** Render '-C <dir> <file>' pairs into a jar argfile body (forward-slashed,
    quoted) — avoids the Windows command-line length limit and backslash
    escaping. *)
