(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
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

open Catala_utils
open Shared_ast
open Ast

val subst_expr : VarName.t -> expr -> expr -> expr
(** [subst_expr var replacement within_expr] substitutes [replacement] for [var]
    within [within_expr]. *)

val subst_block : VarName.t -> expr -> typ -> Pos.t -> block -> block
(** [subst_expr var replacement typ pos block] substitutes [replacement] for
    [var] within the given [block]. If not possible (the variable appears in a
    variable-only position), the block is returned with an initialisation of
    [var] with [replacement] prepended *)

val find_block : (stmt Mark.pos -> bool) -> block -> stmt Mark.pos option
(** Recurses into branchings, but not function bodies *)

val get_vars : expr -> VarName.Set.t
