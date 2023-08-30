(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree of the desugared representation *)

open Catala_utils
open Shared_ast

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef : sig
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of SubScopeName.t * ScopeVar.t * Pos.t

  val compare : t -> t -> int
  val get_position : t -> Pos.t
  val format : Format.formatter -> t -> unit
  val hash : t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module AssertionName : Uid.Id with type info = Uid.MarkedString.info

(** {1 AST} *)

(** {2 Expressions} *)

type expr = (desugared, untyped) gexpr
(** See {!type:Shared_ast.naked_gexpr} for the complete definition *)

type location = desugared glocation

module LocationSet : Set.S with type elt = location Mark.pos
module ExprMap : Map.S with type key = expr

(** {2 Rules and scopes}*)

type exception_situation =
  | BaseCase
  | ExceptionToLabel of LabelName.t Mark.pos
  | ExceptionToRule of RuleName.t Mark.pos

type label_situation = ExplicitlyLabeled of LabelName.t Mark.pos | Unlabeled

type rule = {
  rule_id : RuleName.t;
  rule_just : expr boxed;
  rule_cons : expr boxed;
  rule_parameter : (expr Var.t Mark.pos * typ) list Mark.pos option;
  rule_exception : exception_situation;
  rule_label : label_situation;
}

module Rule : Set.OrderedType with type t = rule

val empty_rule :
  Pos.t -> (Uid.MarkedString.info * typ) list Mark.pos option -> rule

val always_false_rule :
  Pos.t -> (Uid.MarkedString.info * typ) list Mark.pos option -> rule

type assertion = expr boxed
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law
type catala_option = DateRounding of variation_typ

type meta_assertion =
  | FixedBy of reference_typ Mark.pos
  | VariesWith of unit * variation_typ Mark.pos option

type io = {
  io_output : bool Mark.pos;
      (** [true] is present in the output of the scope. *)
  io_input : Runtime.io_input Mark.pos;
}
(** Characterization of the input/output status of a scope variable. *)

type scope_def = {
  scope_def_rules : rule RuleName.Map.t;
  scope_def_typ : typ;
  scope_def_parameters :
    (Uid.MarkedString.info * Shared_ast.typ) list Mark.pos option;
  scope_def_is_condition : bool;
  scope_def_io : io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVar.Map.t;
  scope_sub_scopes : ScopeName.t SubScopeName.Map.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDef.Map.t;
  scope_assertions : assertion AssertionName.Map.t;
  scope_options : catala_option Mark.pos list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope ScopeName.Map.t;
  program_topdefs : (expr option * typ) TopdefName.Map.t;
  program_ctx : decl_ctx;
  program_modules : program ModuleName.Map.t;
}

(** {1 Helpers} *)

val locations_used : expr -> LocationSet.t
val free_variables : rule RuleName.Map.t -> Pos.t ScopeDef.Map.t

val fold_exprs : f:('a -> expr -> 'a) -> init:'a -> program -> 'a
(** Usage: [fold_exprs ~f ~init program] applies ~f to all the expressions
    inside rules (justifications and consequences), expressions and top-level
    definitions of the program. Note that there may be free variables in these
    expressions. *)
