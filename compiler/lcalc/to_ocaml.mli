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

open Shared_ast

(** Formats a lambda calculus program into a valid OCaml program *)

val avoid_keywords : string -> string
val typ_needs_parens : typ -> bool

(* val needs_parens : 'm expr -> bool *)
val format_enum_name : Format.formatter -> EnumName.t -> unit
val format_enum_cons_name : Format.formatter -> EnumConstructor.t -> unit
val format_struct_name : Format.formatter -> StructName.t -> unit

val format_struct_field_name :
  Format.formatter -> StructName.t option * StructField.t -> unit

val format_to_module_name :
  Format.formatter -> [< `Ename of EnumName.t | `Sname of StructName.t ] -> unit
(* * val format_lit : Format.formatter -> lit Mark.pos -> unit * val
   format_uid_list : Format.formatter -> Uid.MarkedString.info list -> unit *)

val format_var : Format.formatter -> 'm Var.t -> unit

val format_program :
  Format.formatter ->
  ?exec_scope:ScopeName.t ->
  'm Ast.program ->
  Scopelang.Dependency.TVertex.t list ->
  unit
(** Usage [format_program fmt p type_dependencies_ordering]. Either one of these
    may be set:

    - [exec_scope] will mark the named scope as "main" and execute it at the end
      of the program. It must have no inputs. *)
