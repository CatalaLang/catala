(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Arthur Carcano <arthur.carcano@ocamlpro.com>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Rendering of the documentation of a module as an HTML page *)

val render : wrap:bool -> Format.formatter -> Extract.module_doc -> unit
(** With [~wrap:true], prints a standalone page, with its stylesheet; otherwise
    only the content of its body. The items are printed in source order, and a
    sidebar lists them in alphabetical order, grouped into types, then constants
    and functions, then scopes. Signatures are printed with the English
    keywords. Types declared in other modules link to [<Module>.html#<anchor>],
    so the pages of a set of modules are expected to sit in the same directory.
    Docstrings are printed as plain text, one paragraph per block of non-empty
    lines. *)
