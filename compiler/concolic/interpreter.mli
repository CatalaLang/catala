(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët
   <alain.delaet--tixeuil@inria.Fr>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Reference interpreter for the default calculus *)

open Catala_utils
open Shared_ast
open Symb_expr
open Path_constraint
module Optimizations = Concolic_optimizations

type s_expr = SymbExpr.z3_expr

type _conc_info = {
  symb_expr : SymbExpr.t;
  constraints : PathConstraint.naked_path;
  ty : typ option;
}

type conc_info = _conc_info custom

(* DCalc with possibly genericErrors and customs *)
type ('c, 'e) conc_interpr_kind =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : yes
  ; genericErrors : 'e
  ; exceptions : no
  ; custom : 'c >

type conc_src_kind = (yes, no) conc_interpr_kind
type conc_dest_kind = (yes, yes) conc_interpr_kind
type conc_expr = (conc_src_kind, conc_info) gexpr

val interpret_program_concolic :
  bool ->
  Optimizations.flag list ->
  (dcalc, 'm) gexpr program ->
  ScopeName.t ->
  (Uid.MarkedString.info * conc_expr) list
(** Concolic interpreter *)
