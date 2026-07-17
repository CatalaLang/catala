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

(** Pure path/URL helpers. Internal: [File]/[Message] expose the
    host-specialised versions. *)

val win32 : bool ref
(** Target OS, defaulting to the host ([Sys.win32]). Tests override it to
    exercise Windows behaviour on any host; production never assigns it. *)

val dir_sep_re : unit -> Re.re

val list_sep : unit -> string
(** Separator between entries of a path-list env var (PATH, PYTHONPATH,
    CLASSPATH). *)

val to_list : string -> string option * string list
val clean : string -> string
val make_absolute : cwd:string -> string -> string
val remove_prefix : cwd:string -> string -> string -> string
val common_prefix : cwd:string -> string -> string -> string
val make_relative_to : cwd:string -> dir:string -> string -> string
val reverse : cwd:string -> from_dir:string -> to_dir:string -> string -> string

val url_of_absolute : string -> string
(** Path part of a [file://] URL for an absolute OS path. Handles Windows drive
    paths ([C:\dir] -> [/C:/dir]) and UNC paths ([\\server\share] ->
    [server/share]). *)
