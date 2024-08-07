(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

type translation_config = {
  keep_special_ops : bool;
      (** When [keep_special_ops] is true, then this translation uses special
          Scalc AST nodes for higher-order operators like map, fold,
          handle_default, etc. This is useful if the target language after Scalc
          does not support nested functions like C. *)
  dead_value_assignment : bool;
      (** When [dead_value_assignment] is true, the translation inserts dummy
          assignments of the variable being defined in the current code branch
          just before raising a terminal error. This is useful for languages
          like Python and their linting tools like mypy. The assignment uses the
          polymorphic [Ast.dead_value]. *)
  no_struct_literals : bool;
      (** When [no_struct_literals] is true, the translation inserts a temporary
          variable to hold the initialization of struct literals. This matches
          what C89 expects. *)
  renaming_context : Expr.Renaming.context;
}

val translate_program :
  config:translation_config -> typed Lcalc.Ast.program ->
  Ast.program
