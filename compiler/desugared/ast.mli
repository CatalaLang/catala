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

(** Inside a scope, a definition can refer to a variable (possibly an
    intermediate state thereof) or an input of a subscope. *)
module ScopeDef : sig
  type kind =
    | Var of StateName.t option
    | SubScopeInput of {
        name : ScopeName.t;
        var_within_origin_scope : ScopeVar.t;
      }

  val equal_kind : kind -> kind -> bool
  val compare_kind : kind -> kind -> int
  val format_kind : Format.formatter -> kind -> unit
  val hash_kind : strip:Uid.Path.t -> kind -> Hash.t

  type t = ScopeVar.t Mark.pos * kind

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val get_position : t -> Pos.t
  val format : Format.formatter -> t -> unit
  val hash : strip:Uid.Path.t -> t -> Hash.t

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
      (** empty outside of the root module *)
  scope_def_typ : typ;
  scope_def_parameters :
    (Uid.MarkedString.info * Shared_ast.typ) list Mark.pos option;
  scope_def_is_condition : bool;
  scope_def_io : io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVar.Map.t;
  scope_sub_scopes : ScopeName.t ScopeVar.Map.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDef.Map.t;
  scope_assertions : assertion AssertionName.Map.t;
      (** empty outside of the root module *)
  scope_options : catala_option Mark.pos list;
  scope_meta_assertions : meta_assertion list;
  scope_visibility : visibility;
}

type topdef = {
  topdef_expr : expr option;  (** Always [None] outside of the root module *)
  topdef_type : typ;
  topdef_visibility : visibility;
      (** Necessarily [Public] outside of the root module *)
}

type modul = {
  module_scopes : scope ScopeName.Map.t;
  module_topdefs : topdef TopdefName.Map.t;
}

type program = {
  program_module_name : (ModuleName.t * module_intf_id) option;
  program_ctx : decl_ctx;
  program_modules : modul ModuleName.Map.t;
      (** Contains all submodules of the program, in a flattened structure *)
  program_root : modul;
  program_lang : Global.backend_lang;
}

(** {1 Interface hash computations} *)

(** These hashes are computed on interfaces: only signatures are considered. *)
module Hash : sig
  (** The [strip] argument below strips as many leading path components before
      hashing *)

  val scope : strip:Uid.Path.t -> scope -> Hash.t
  val modul : ?strip:Uid.Path.t -> modul -> Hash.t
  val module_binding : ModuleName.t -> modul -> Hash.t
end

(** {1 Helpers} *)

val locations_used : expr -> LocationSet.t
val free_variables : rule RuleName.Map.t -> Pos.t ScopeDef.Map.t

val fold_exprs : f:('a -> expr -> 'a) -> init:'a -> program -> 'a
(** Usage: [fold_exprs ~f ~init program] applies ~f to all the expressions
    inside rules (justifications and consequences), expressions and top-level
    definitions of the program. Note that there may be free variables in these
    expressions. *)
