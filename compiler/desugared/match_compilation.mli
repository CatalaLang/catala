(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Shared_ast

type column = {
  col_enum : EnumName.t;
  col_constr : EnumConstructor.t;
  col_vars : (Ast.expr Var.t * Pos.t) list;
  col_pos : Pos.t;
}

type row = {
  row_cols : column option list;
  row_body : Ast.expr boxed;
  row_pos : Pos.t;
}

val compile :
  Name_resolution.context ->
  scrutinees:Ast.expr boxed list ->
  no_wildcard_warning:bool ->
  untyped mark ->
  row list ->
  Ast.expr boxed
