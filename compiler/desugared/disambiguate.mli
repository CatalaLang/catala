(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

(** This module does local typing in order to fill some missing type information
    in the AST:

    - it fills the types of arguments in [EAbs] nodes, (untyped ones are
      inserted during desugaring, e.g. by `let-in` constructs), as well as
      [EApp] and [EAppOp] nodes
    - it resolves the structure names of [EDStructAccess] nodes. *)

val program : Ast.program -> Ast.program
