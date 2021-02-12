(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr> Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Translation from {!module: Surface.Ast} to {!module: Desugaring.Ast}.

    - Removes syntactic sugars
    - Separate code from legislation *)

open Utils

(** {1 Translating expressions} *)

val translate_op_kind : Ast.op_kind -> Dcalc.Ast.op_kind

val translate_binop : Ast.binop -> Dcalc.Ast.binop

val translate_unop : Ast.unop -> Dcalc.Ast.unop

(** The two modules below help performing operations on map with the {!type: Bindlib.box}. Indeed,
    Catala uses the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library to represent bound
    variables in the AST. In this translation, bound variables are used to represent function
    parameters or pattern macthing bindings. *)
module LiftStructFieldMap : sig
  val lift_box :
    'a Bindlib.box Scopelang.Ast.StructFieldMap.t -> 'a Scopelang.Ast.StructFieldMap.t Bindlib.box
end

module LiftEnumConstructorMap : sig
  val lift_box :
    'a Bindlib.box Scopelang.Ast.EnumConstructorMap.t ->
    'a Scopelang.Ast.EnumConstructorMap.t Bindlib.box
end

val disambiguate_constructor :
  Name_resolution.context ->
  (string Pos.marked option * string Pos.marked) list ->
  Pos.t ->
  Scopelang.Ast.EnumName.t * Scopelang.Ast.EnumConstructor.t

val translate_expr :
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Ast.expression Pos.marked ->
  Scopelang.Ast.expr Pos.marked Bindlib.box
(** Usage: [translate_expr scope ctxt expr]

    Translates [expr] into its desugared equivalent. [scope] is used to disambiguate the scope and
    subscopes variables than occur in the expresion *)

val disambiguate_match_and_build_expression :
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Ast.match_cases ->
  Scopelang.Ast.expr Pos.marked Bindlib.box Scopelang.Ast.EnumConstructorMap.t
  * Scopelang.Ast.EnumName.t

(** {1 Translating scope definitions} *)

val merge_conditions :
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Pos.t ->
  Scopelang.Ast.expr Pos.marked Bindlib.box
(** A scope use can be annotated with a pervasive precondition, in which case this precondition has
    to be appended to the justifications of each definition in the subscope use. This is what this
    function does. *)

val process_default :
  Name_resolution.context ->
  Scopelang.Ast.ScopeName.t ->
  Desugared.Ast.ScopeDef.t Pos.marked ->
  Scopelang.Ast.Var.t Pos.marked option ->
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Desugared.Ast.RuleName.t Pos.marked option ->
  Ast.expression Pos.marked option ->
  Ast.expression Pos.marked ->
  Desugared.Ast.rule
(** Translates a surface definition into condition into a desugared {!type: Desugared.Ast.rule} *)

val process_def :
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program ->
  Ast.definition ->
  Desugared.Ast.program
(** Wrapper around {!val: process_default} that performs some name disambiguation *)

val rule_to_def : Ast.rule -> Ast.definition
(** Translates a {!type: Surface.Ast.rule} into the corresponding {!type: Surface.Ast.definition} *)

val process_rule :
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program ->
  Ast.rule ->
  Desugared.Ast.program
(** Translates a {!type: Surface.Ast.rule} from the surface language *)

val process_assert :
  Scopelang.Ast.expr Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program ->
  Ast.assertion ->
  Desugared.Ast.program
(** Translates assertions *)

val process_scope_use_item :
  Ast.expression Pos.marked option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program ->
  Ast.scope_use_item Pos.marked ->
  Desugared.Ast.program
(** Translates a surface definition, rule or assertion *)

(** {1 Translating top-level items} *)

val check_unlabeled_exception :
  Scopelang.Ast.ScopeName.t -> Name_resolution.context -> Ast.scope_use_item Pos.marked -> unit
(** If this is an unlabeled exception, ensures that it has a unique default definition *)

val process_scope_use :
  Name_resolution.context -> Desugared.Ast.program -> Ast.scope_use -> Desugared.Ast.program
(** Translates a surface scope use, which is a bunch of definitions *)

val desugar_program : Name_resolution.context -> Ast.program -> Desugared.Ast.program
(** Main function of this module *)
