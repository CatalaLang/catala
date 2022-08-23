(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Generates verification conditions from scope definitions *)

open Utils
open Shared_ast

type verification_condition_kind =
  | NoEmptyError
      (** This verification condition checks whether a definition never returns
          an empty error *)
  | NoOverlappingExceptions
      (** This verification condition checks whether a definition never returns
          a conflict error *)

type verification_condition = {
  vc_guard : typed Dcalc.Ast.marked_expr;
      (** This expression should have type [bool]*)
  vc_kind : verification_condition_kind;
  vc_scope : ScopeName.t;
  vc_variable : typed Dcalc.Ast.expr Var.t Marked.pos;
  vc_free_vars_typ : (typed Dcalc.Ast.expr, typ Marked.pos) Var.Map.t;
      (** Types of the locally free variables in [vc_guard]. The types of other
          free variables linked to scope variables can be obtained with
          [Dcalc.Ast.variable_types]. *)
}

val generate_verification_conditions :
  typed Dcalc.Ast.program -> ScopeName.t option -> verification_condition list
(** [generate_verification_conditions p None] will generate the verification
    conditions for all the variables of all the scopes of the program [p], while
    [generate_verification_conditions p (Some s)] will focus only on the
    variables of scope [s]. *)
