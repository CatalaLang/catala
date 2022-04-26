(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
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

(** Abstract syntax tree of the default calculus intermediate representation *)

open Utils
module ScopeName : Uid.Id with type info = Uid.MarkedString.info
module StructName : Uid.Id with type info = Uid.MarkedString.info
module StructFieldName : Uid.Id with type info = Uid.MarkedString.info
module StructMap : Map.S with type key = StructName.t
module EnumName : Uid.Id with type info = Uid.MarkedString.info
module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info
module EnumMap : Map.S with type key = EnumName.t

(** Abstract syntax tree for the default calculus *)

(** {1 Abstract syntax tree} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type typ =
  | TLit of typ_lit
  | TTuple of typ Pos.marked list * StructName.t option
  | TEnum of typ Pos.marked list * EnumName.t
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ Pos.marked
  | TAny

type date = Runtime.date
type duration = Runtime.duration

type lit =
  | LBool of bool
  | LEmptyError
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of date
  | LDuration of duration

type op_kind =
  | KInt
  | KRat
  | KMoney
  | KDate
  | KDuration  (** All ops don't have a KDate and KDuration. *)

type ternop = Fold

type binop =
  | And
  | Or
  | Xor
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
  | Map
  | Concat
  | Filter

type log_entry =
  | VarDef of typ
      (** During code generation, we need to know the type of the variable being
          logged for embedding *)
  | BeginCall
  | EndCall
  | PosRecordIfTrueBool

type unop =
  | Not
  | Minus of op_kind
  | Log of log_entry * Utils.Uid.MarkedString.info list
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear
  | RoundMoney

type operator = Ternop of ternop | Binop of binop | Unop of unop

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax*)
type expr =
  | EVar of expr Bindlib.var Pos.marked
  | ETuple of expr Pos.marked list * StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of
      expr Pos.marked * int * StructName.t option * typ Pos.marked list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of expr Pos.marked * int * EnumName.t * typ Pos.marked list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of expr Pos.marked * expr Pos.marked list * EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of expr Pos.marked list
  | ELit of lit
  | EAbs of
      ((expr, expr Pos.marked) Bindlib.mbinder[@opaque]) Pos.marked
      * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ErrorOnEmpty of expr Pos.marked

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t
type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }
type binder = (expr, expr Pos.marked) Bindlib.binder

(** This kind annotation signals that the let-binding respects a structural
    invariant. These invariants concern the shape of the expression in the
    let-binding, and are documented below. *)
type scope_let_kind =
  | DestructuringInputStruct  (** [let x = input.field]*)
  | ScopeVarDefinition  (** [let x = error_on_empty e]*)
  | SubScopeVarDefinition
      (** [let s.x = fun _ -> e] or [let s.x = error_on_empty e] for input-only
          subscope variables. *)
  | CallingSubScope  (** [let result = s ({ x = s.x; y = s.x; ...}) ]*)
  | DestructuringSubScopeResults  (** [let s.x = result.x ]**)
  | Assertion  (** [let _ = assert e]*)

type 'expr scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ Utils.Pos.marked;
  scope_let_expr : 'expr Utils.Pos.marked;
  scope_let_next : ('expr, 'expr scope_body_expr) Bindlib.binder;
  scope_let_pos : Utils.Pos.t;
}
(** This type is parametrized by the expression type so it can be reused in
    later intermediate representations. *)

(** A scope let-binding has all the information necessary to make a proper
    let-binding expression, plus an annotation for the kind of the let-binding
    that comes from the compilation of a {!module: Scopelang.Ast} statement. *)
and 'expr scope_body_expr =
  | Result of 'expr Utils.Pos.marked
  | ScopeLet of 'expr scope_let

type 'expr scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : ('expr, 'expr scope_body_expr) Bindlib.binder;
}
(** Instead of being a single expression, we give a little more ad-hoc structure
    to the scope body by decomposing it in an ordered list of let-bindings, and
    a result expression that uses the let-binded variables. The first binder is
    the argument of type [scope_body_input_struct]. *)

type 'expr scope_def = {
  scope_name : ScopeName.t;
  scope_body : 'expr scope_body;
  scope_next : ('expr, 'expr scopes) Bindlib.binder;
}

(** Finally, we do the same transformation for the whole program for the kinded
    lets. This permit us to use bindlib variables for scopes names. *)
and 'a scopes = Nil | ScopeDef of 'a scope_def

type program = { decl_ctx : decl_ctx; scopes : expr scopes }

(** {1 Helpers} *)

(** {2 Boxed constructors}*)

val evar : expr Bindlib.var -> Pos.t -> expr Pos.marked Bindlib.box

val etuple :
  expr Pos.marked Bindlib.box list ->
  StructName.t option ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val etupleaccess :
  expr Pos.marked Bindlib.box ->
  int ->
  StructName.t option ->
  typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val einj :
  expr Pos.marked Bindlib.box ->
  int ->
  EnumName.t ->
  typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val ematch :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box list ->
  EnumName.t ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val earray :
  expr Pos.marked Bindlib.box list -> Pos.t -> expr Pos.marked Bindlib.box

val elit : lit -> Pos.t -> expr Pos.marked Bindlib.box

val eabs :
  (expr, expr Pos.marked) Bindlib.mbinder Bindlib.box ->
  Pos.t ->
  typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val eapp :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val eassert :
  expr Pos.marked Bindlib.box -> Pos.t -> expr Pos.marked Bindlib.box

val eop : operator -> Pos.t -> expr Pos.marked Bindlib.box

val edefault :
  expr Pos.marked Bindlib.box list ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val eifthenelse :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val eerroronempty :
  expr Pos.marked Bindlib.box -> Pos.t -> expr Pos.marked Bindlib.box

val box_expr : expr Pos.marked -> expr Pos.marked Bindlib.box

type 'expr box_expr_sig = 'expr Pos.marked -> 'expr Pos.marked Bindlib.box

(**{2 Program traversal}*)

(** Be careful when using these traversal functions, as the bound variables they
    open will be different at each traversal. *)

val map_expr :
  'a ->
  f:('a -> expr Pos.marked -> expr Pos.marked Bindlib.box) ->
  expr Pos.marked ->
  expr Pos.marked Bindlib.box
(** If you want to apply a map transform to an expression, you can save up
    writing a painful match over all the cases of the AST. For instance, if you
    want to remove all errors on empty, you can write

    {[
      let remove_error_empty =
        let rec f () e =
          match Pos.unmark e with
          | ErrorOnEmpty e1 -> map_expr () f e1
          | _ -> map_expr () f e
        in
        f () e
    ]}

    The first argument of map_expr is an optional context that you can carry
    around during your map traversal. *)

val fold_left_scope_lets :
  f:('a -> 'expr scope_let -> 'expr Bindlib.var -> 'a) ->
  init:'a ->
  'expr scope_body_expr ->
  'a
(** Usage:
    [fold_left_scope_lets ~f:(fun acc scope_let scope_let_var -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined. *)

val fold_right_scope_lets :
  f:('expr scope_let -> 'expr Bindlib.var -> 'a -> 'a) ->
  init:('expr Pos.marked -> 'a) ->
  'expr scope_body_expr ->
  'a
(** Usage:
    [fold_right_scope_lets ~f:(fun scope_let scope_let_var acc -> ...) ~init scope_lets],
    where [scope_let_var] is the variable bound to the scope let in the next
    scope lets to be examined (which are before in the program order). *)

val map_exprs_in_scope_lets :
  f:('expr Pos.marked -> 'expr Pos.marked Bindlib.box) ->
  'expr scope_body_expr ->
  'expr scope_body_expr Bindlib.box

val fold_left_scope_defs :
  f:('a -> 'expr scope_def -> 'expr Bindlib.var -> 'a) ->
  init:'a ->
  'expr scopes ->
  'a
(** Usage:
    [fold_left_scope_defs ~f:(fun acc scope_def scope_var -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined. *)

val fold_right_scope_defs :
  f:('expr scope_def -> 'expr Bindlib.var -> 'a -> 'a) ->
  init:'a ->
  'expr scopes ->
  'a
(** Usage:
    [fold_right_scope_defs ~f:(fun  scope_def scope_var acc -> ...) ~init scope_def],
    where [scope_var] is the variable bound to the scope in the next scopes to
    be examined (which are before in the program order). *)

val map_scope_defs :
  f:('expr scope_def -> 'expr scope_def Bindlib.box) ->
  'expr scopes ->
  'expr scopes Bindlib.box

val map_exprs_in_scopes :
  f:('expr Pos.marked -> 'expr Pos.marked Bindlib.box) ->
  'expr scopes ->
  'expr scopes Bindlib.box
(** This is the main map visitor for all the expressions inside all the scopes
    of the program. *)

(** {2 Variables}*)

module Var : sig
  type t = expr Bindlib.var

  val make : string Pos.marked -> t
  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t
module VarSet : Set.S with type elt = Var.t

val free_vars_expr : expr Pos.marked -> VarSet.t
val free_vars_scope_body_expr : expr scope_body_expr -> VarSet.t
val free_vars_scope_body : expr scope_body -> VarSet.t
val free_vars_scopes : expr scopes -> VarSet.t

type vars = expr Bindlib.mvar

(** {2 Boxed term constructors}*)

val make_var : Var.t Pos.marked -> expr Pos.marked Bindlib.box

val make_abs :
  vars ->
  expr Pos.marked Bindlib.box ->
  Pos.t ->
  typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_app :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_let_in :
  Var.t ->
  typ Pos.marked ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  Pos.t ->
  expr Pos.marked Bindlib.box

(**{2 Other}*)

val empty_thunked_term : expr Pos.marked
val is_value : expr Pos.marked -> bool

val equal_exprs : expr Pos.marked -> expr Pos.marked -> bool
(** Determines if two expressions are equal, omitting their position information *)

(** {1 AST manipulation helpers}*)

type 'expr make_let_in_sig =
  'expr Bindlib.var ->
  typ Pos.marked ->
  'expr Pos.marked Bindlib.box ->
  'expr Pos.marked Bindlib.box ->
  Pos.t ->
  'expr Pos.marked Bindlib.box

type 'expr make_abs_sig =
  'expr Bindlib.mvar ->
  'expr Pos.marked Bindlib.box ->
  Pos.t ->
  typ Pos.marked list ->
  Pos.t ->
  'expr Pos.marked Bindlib.box

val build_whole_scope_expr :
  box_expr:'expr box_expr_sig ->
  make_abs:'expr make_abs_sig ->
  make_let_in:'expr make_let_in_sig ->
  decl_ctx ->
  'expr scope_body ->
  Pos.t ->
  'expr Pos.marked Bindlib.box
(** Usage: [build_whole_scope_expr ctx body scope_position] where
    [scope_position] corresponds to the line of the scope declaration for
    instance. *)

type 'expr scope_name_or_var =
  | ScopeName of ScopeName.t
  | ScopeVar of 'expr Bindlib.var

val unfold_scopes :
  box_expr:'expr box_expr_sig ->
  make_abs:'expr make_abs_sig ->
  make_let_in:'expr make_let_in_sig ->
  decl_ctx ->
  'expr scopes ->
  'expr scope_name_or_var ->
  'expr Pos.marked Bindlib.box

val build_whole_program_expr :
  program -> ScopeName.t -> expr Pos.marked Bindlib.box
(** Usage: [build_whole_program_expr program main_scope] builds an expression
    corresponding to the main program and returning the main scope as a
    function. *)

val expr_size : expr Pos.marked -> int
(** Used by the optimizer to know when to stop *)

val remove_logging_calls : expr Pos.marked -> expr Pos.marked Bindlib.box
(** Removes all calls to [Log] unary operators in the AST, replacing them by
    their argument. *)
