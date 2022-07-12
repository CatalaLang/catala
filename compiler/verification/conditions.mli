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

type verification_condition_kind =
  | NoEmptyError
      (** This verification condition checks whether a definition never returns
          an empty error *)
  | NoOverlappingExceptions
      (** This verification condition checks whether a definition never returns
          a conflict error *)

type verification_condition = {
  vc_guard : Dcalc.Ast.typed Dcalc.Ast.marked_expr;
      (** This expression should have type [bool]*)
  vc_kind : verification_condition_kind;
  vc_scope : Dcalc.Ast.ScopeName.t;
  vc_variable : Dcalc.Ast.Var.t Utils.Marked.pos;
  vc_free_vars_typ : Dcalc.Ast.typ Utils.Marked.pos Dcalc.Ast.VarMap.t;
      (** Types of the locally free variables in [vc_guard]. The types of other
          free variables linked to scope variables can be obtained with
          [Dcalc.Ast.variable_types]. *)
}

val generate_verification_conditions :
  Dcalc.Ast.typed Dcalc.Ast.program ->
  Dcalc.Ast.ScopeName.t option ->
  verification_condition list
(** [generate_verification_conditions p None] will generate the verification
    conditions for all the variables of all the scopes of the program [p], while
    [generate_verification_conditions p (Some s)] will focus only on the
    variables of scope [s]. *)
