(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
include module type of Astgen

(** Abstract syntax tree for the lambda calculus *)

(** {1 Abstract syntax tree} *)

type 'm mark = 'm Dcalc.Ast.mark

type 'm expr = (lcalc, 'm mark) gexpr
and 'm marked_expr = (lcalc, 'm mark) marked_gexpr

type 'm program = ('m expr, 'm) Dcalc.Ast.program_generic

(** {1 Variable helpers} *)

type 'm var = 'm expr Bindlib.var
type 'm vars = 'm expr Bindlib.mvar

module Var : sig
  type t

  val t : 'm expr Bindlib.var -> t
  val get : t -> 'm expr Bindlib.var
  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t
module VarSet : Set.S with type elt = Var.t

val new_var : string -> 'm var

type 'm binder = ('m expr, 'm marked_expr) Bindlib.binder

(** {2 Program traversal} *)

val map_expr :
  'a ->
  f:('a -> 'm1 marked_expr -> 'm2 marked_expr Bindlib.box) ->
  ('m1 expr, 'm2 mark) Marked.t ->
  'm2 marked_expr Bindlib.box
(** See [Dcalc.Ast.map_expr] *)

val map_expr_top_down :
  f:('m1 marked_expr -> ('m1 expr, 'm2 mark) Marked.t) ->
  'm1 marked_expr ->
  'm2 marked_expr Bindlib.box
(** See [Dcalc.Ast.map_expr_top_down] *)

val map_expr_marks :
  f:('m1 mark -> 'm2 mark) -> 'm1 marked_expr -> 'm2 marked_expr Bindlib.box
(** See [Dcalc.Ast.map_expr_marks] *)

val untype_expr : 'm marked_expr -> Dcalc.Ast.untyped marked_expr Bindlib.box
val untype_program : 'm program -> Dcalc.Ast.untyped program

(** {1 Boxed constructors} *)

val evar : 'm expr Bindlib.var -> 'm mark -> 'm marked_expr Bindlib.box

val etuple :
  'm marked_expr Bindlib.box list ->
  Dcalc.Ast.StructName.t option ->
  'm mark ->
  'm marked_expr Bindlib.box

val etupleaccess :
  'm marked_expr Bindlib.box ->
  int ->
  Dcalc.Ast.StructName.t option ->
  Dcalc.Ast.typ Marked.pos list ->
  'm mark ->
  'm marked_expr Bindlib.box

val einj :
  'm marked_expr Bindlib.box ->
  int ->
  Dcalc.Ast.EnumName.t ->
  Dcalc.Ast.typ Marked.pos list ->
  'm mark ->
  'm marked_expr Bindlib.box

val ematch :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  Dcalc.Ast.EnumName.t ->
  'm mark ->
  'm marked_expr Bindlib.box

val earray :
  'm marked_expr Bindlib.box list -> 'm mark -> 'm marked_expr Bindlib.box

val elit : lit -> 'm mark -> 'm marked_expr Bindlib.box

val eabs :
  ('m expr, 'm marked_expr) Bindlib.mbinder Bindlib.box ->
  Dcalc.Ast.typ Marked.pos list ->
  'm mark ->
  'm marked_expr Bindlib.box

val eapp :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

val eassert :
  'm marked_expr Bindlib.box -> 'm mark -> 'm marked_expr Bindlib.box

val eop : Dcalc.Ast.operator -> 'm mark -> 'm marked_expr Bindlib.box

val eifthenelse :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm mark ->
  'm marked_expr Bindlib.box

val ecatch :
  'm marked_expr Bindlib.box ->
  except ->
  'm marked_expr Bindlib.box ->
  'm mark ->
  'm marked_expr Bindlib.box

val eraise : except -> 'm mark -> 'm marked_expr Bindlib.box

(** {1 Language terms construction}*)

val make_var : ('m var, 'm) Dcalc.Ast.marked -> 'm marked_expr Bindlib.box

val make_abs :
  'm vars ->
  'm marked_expr Bindlib.box ->
  Dcalc.Ast.typ Marked.pos list ->
  'm mark ->
  'm marked_expr Bindlib.box

val make_app :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box list ->
  'm mark ->
  'm marked_expr Bindlib.box

val make_let_in :
  'm var ->
  Dcalc.Ast.typ Marked.pos ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  Pos.t ->
  'm marked_expr Bindlib.box

val make_multiple_let_in :
  'm vars ->
  Dcalc.Ast.typ Marked.pos list ->
  'm marked_expr Bindlib.box list ->
  'm marked_expr Bindlib.box ->
  Pos.t ->
  'm marked_expr Bindlib.box

val option_enum : Dcalc.Ast.EnumName.t
val none_constr : Dcalc.Ast.EnumConstructor.t
val some_constr : Dcalc.Ast.EnumConstructor.t

val option_enum_config :
  (Dcalc.Ast.EnumConstructor.t * Dcalc.Ast.typ Marked.pos) list

val make_none : 'm mark -> 'm marked_expr Bindlib.box
val make_some : 'm marked_expr Bindlib.box -> 'm marked_expr Bindlib.box

val make_matchopt_with_abs_arms :
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box

val make_matchopt :
  'm mark ->
  'm var ->
  Dcalc.Ast.typ Marked.pos ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box ->
  'm marked_expr Bindlib.box
(** [e' = make_matchopt'' pos v e e_none e_some] Builds the term corresponding
    to [match e with | None -> fun () -> e_none |Some -> fun v -> e_some]. *)

val box_expr : 'm marked_expr -> 'm marked_expr Bindlib.box

(** {1 Special symbols} *)

val handle_default : Var.t
val handle_default_opt : Var.t
