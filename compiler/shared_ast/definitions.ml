(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This module defines generic types for types, literals and expressions shared
    through several of the different ASTs. *)

(* Doesn't define values, so OK to have without an mli *)

open Catala_utils
module Runtime = Runtime_ocaml.Runtime
module ScopeName = Uid.Gen ()
module TopdefName = Uid.Gen ()
module StructName = Uid.Gen ()
module StructField = Uid.Gen ()
module EnumName = Uid.Gen ()
module EnumConstructor = Uid.Gen ()

(** Only used by surface *)

module RuleName = Uid.Gen ()
module LabelName = Uid.Gen ()

(** Used for unresolved structs/maps in desugared *)

module IdentName = String

(** Only used by desugared/scopelang *)

module ScopeVar = Uid.Gen ()
module SubScopeName = Uid.Gen ()
module StateName = Uid.Gen ()

(** {1 Abstract syntax tree} *)

(** Define a common base type for the expressions in most passes of the compiler *)

(** {2 Phantom types used to select relevant cases on the generic AST}

    we instantiate them with a polymorphic variant to take advantage of
    sub-typing. The values aren't actually used. *)

(** These types allow to select the features present in any given expression
    type *)

type op_kind = [ `Monomorphic | `Polymorphic | `Overloaded | `Resolved ]

type all_ast_features =
  [ `SyntacticNames
  | `ResolvedNames
  | `ScopeVarStates
  | `ScopeVarSimpl
  | `ExplicitScopes
  | `Assertions
  | `DefaultTerms
  | `Exceptions ]

type all = [ all_ast_features | op_kind ]

type desugared =
  [ `Monomorphic
  | `Polymorphic
  | `Overloaded
  | `SyntacticNames
  | `ExplicitScopes
  | `ScopeVarStates
  | `DefaultTerms ]

type scopelang =
  [ `Monomorphic
  | `Polymorphic
  | `Resolved
  | `ResolvedNames
  | `ExplicitScopes
  | `ScopeVarSimpl
  | `DefaultTerms ]

type dcalc =
  [ `Monomorphic
  | `Polymorphic
  | `Resolved
  | `ResolvedNames
  | `Assertions
  | `DefaultTerms ]

type lcalc =
  [ `Monomorphic
  | `Polymorphic
  | `Resolved
  | `ResolvedNames
  | `Assertions
  | `Exceptions ]

type 'a any = [< all ] as 'a
(** ['a any] is 'a, but adds the constraint that it should be restricted to
    valid AST kinds *)

(** {2 Types} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type typ = naked_typ Marked.pos

and naked_typ =
  | TLit of typ_lit
  | TTuple of typ list
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TOption of typ
  | TArrow of typ list * typ
  | TArray of typ
  | TAny

(** {2 Constants and operators} *)

type date = Runtime.date
type date_rounding = Runtime.date_rounding
type duration = Runtime.duration

type log_entry =
  | VarDef of naked_typ
      (** During code generation, we need to know the type of the variable being
          logged for embedding *)
  | BeginCall
  | EndCall
  | PosRecordIfTrueBool

module Op = struct
  (** Classification of operators on how they should be typed *)

  type 'a any = [> op_kind ] as 'a

  type monomorphic = [ `Monomorphic ]
  (** Operands and return types of the operator are fixed *)

  type polymorphic = [ `Polymorphic ]
  (** The operator is truly polymorphic: it's the same runtime function that may
      work on multiple types. We require that resolving the argument types from
      right to left trivially resolves all type variables declared in the
      operator type. *)

  type overloaded = [ `Overloaded ]
  (** The operator is ambiguous and requires the types of its arguments to be
      known before it can be typed, using a pre-defined table *)

  type resolved = [ `Resolved ]
  (** Explicit monomorphic versions of the overloaded operators *)

  type _ t =
    (* unary *)
    (* * monomorphic *)
    | Not : [> `Monomorphic ] t
    | GetDay : [> `Monomorphic ] t
    | GetMonth : [> `Monomorphic ] t
    | GetYear : [> `Monomorphic ] t
    | FirstDayOfMonth : [> `Monomorphic ] t
    | LastDayOfMonth : [> `Monomorphic ] t
    (* * polymorphic *)
    | Length : [> `Polymorphic ] t
    | Log : log_entry * Uid.MarkedString.info list -> [> `Polymorphic ] t
    (* * overloaded *)
    | Minus : [> `Overloaded ] t
    | Minus_int : [> `Resolved ] t
    | Minus_rat : [> `Resolved ] t
    | Minus_mon : [> `Resolved ] t
    | Minus_dur : [> `Resolved ] t
    | ToRat : [> `Overloaded ] t
    | ToRat_int : [> `Resolved ] t
    | ToRat_mon : [> `Resolved ] t
    | ToMoney : [> `Overloaded ] t
    | ToMoney_rat : [> `Resolved ] t
    | Round : [> `Overloaded ] t
    | Round_rat : [> `Resolved ] t
    | Round_mon : [> `Resolved ] t
    (* binary *)
    (* * monomorphic *)
    | And : [> `Monomorphic ] t
    | Or : [> `Monomorphic ] t
    | Xor : [> `Monomorphic ] t
    (* * polymorphic *)
    | Eq : [> `Polymorphic ] t
    | Map : [> `Polymorphic ] t
    | Concat : [> `Polymorphic ] t
    | Filter : [> `Polymorphic ] t
    | Reduce : [> `Polymorphic ] t
    (* * overloaded *)
    | Add : [> `Overloaded ] t
    | Add_int_int : [> `Resolved ] t
    | Add_rat_rat : [> `Resolved ] t
    | Add_mon_mon : [> `Resolved ] t
    | Add_dat_dur : date_rounding -> [> `Resolved ] t
    | Add_dur_dur : [> `Resolved ] t
    | Sub : [> `Overloaded ] t
    | Sub_int_int : [> `Resolved ] t
    | Sub_rat_rat : [> `Resolved ] t
    | Sub_mon_mon : [> `Resolved ] t
    | Sub_dat_dat : [> `Resolved ] t
    | Sub_dat_dur : [> `Resolved ] t
    | Sub_dur_dur : [> `Resolved ] t
    | Mult : [> `Overloaded ] t
    | Mult_int_int : [> `Resolved ] t
    | Mult_rat_rat : [> `Resolved ] t
    | Mult_mon_rat : [> `Resolved ] t
    | Mult_dur_int : [> `Resolved ] t
    | Div : [> `Overloaded ] t
    | Div_int_int : [> `Resolved ] t
    | Div_rat_rat : [> `Resolved ] t
    | Div_mon_rat : [> `Resolved ] t
    | Div_mon_mon : [> `Resolved ] t
    | Div_dur_dur : [> `Resolved ] t
    | Lt : [> `Overloaded ] t
    | Lt_int_int : [> `Resolved ] t
    | Lt_rat_rat : [> `Resolved ] t
    | Lt_mon_mon : [> `Resolved ] t
    | Lt_dat_dat : [> `Resolved ] t
    | Lt_dur_dur : [> `Resolved ] t
    | Lte : [> `Overloaded ] t
    | Lte_int_int : [> `Resolved ] t
    | Lte_rat_rat : [> `Resolved ] t
    | Lte_mon_mon : [> `Resolved ] t
    | Lte_dat_dat : [> `Resolved ] t
    | Lte_dur_dur : [> `Resolved ] t
    | Gt : [> `Overloaded ] t
    | Gt_int_int : [> `Resolved ] t
    | Gt_rat_rat : [> `Resolved ] t
    | Gt_mon_mon : [> `Resolved ] t
    | Gt_dat_dat : [> `Resolved ] t
    | Gt_dur_dur : [> `Resolved ] t
    | Gte : [> `Overloaded ] t
    | Gte_int_int : [> `Resolved ] t
    | Gte_rat_rat : [> `Resolved ] t
    | Gte_mon_mon : [> `Resolved ] t
    | Gte_dat_dat : [> `Resolved ] t
    | Gte_dur_dur : [> `Resolved ] t
    (* Todo: Eq is not an overload at the moment, but it should be one. The
       trick is that it needs generation of specific code for arrays, every
       struct and enum: operators [Eq_structs of StructName.t], etc. *)
    | Eq_int_int : [> `Resolved ] t
    | Eq_rat_rat : [> `Resolved ] t
    | Eq_mon_mon : [> `Resolved ] t
    | Eq_dur_dur : [> `Resolved ] t
    | Eq_dat_dat : [> `Resolved ] t
    (* ternary *)
    (* * polymorphic *)
    | Fold : [> `Polymorphic ] t
end

type 'a operator = 'a Op.t
type except = ConflictError | EmptyError | NoValueProvided | Crash

(** {2 Generic expressions} *)

(** Define a common base type for the expressions in most passes of the compiler *)

(** Literals are the same throughout compilation except for the [LEmptyError]
    case which is eliminated midway through. *)
type lit =
  | LBool of bool
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of date
  | LDuration of duration

(** Locations are handled differently in [desugared] and [scopelang] *)
type 'a glocation =
  | DesugaredScopeVar :
      ScopeVar.t Marked.pos * StateName.t option
      -> [> `ScopeVarStates ] glocation
  | ScopelangScopeVar : ScopeVar.t Marked.pos -> [> `ScopeVarSimpl ] glocation
  | SubScopeVar :
      ScopeName.t * SubScopeName.t Marked.pos * ScopeVar.t Marked.pos
      -> [> `ExplicitScopes ] glocation
  | ToplevelVar : TopdefName.t Marked.pos -> [> `ExplicitScopes ] glocation

type ('a, 't) gexpr = (('a, 't) naked_gexpr, 't) Marked.t
(** General expressions: groups all expression cases of the different ASTs, and
    uses a GADT to eliminate irrelevant cases for each one. The ['t] annotations
    are also totally unconstrained at this point. The dcalc exprs, for ex ample,
    are then defined with [type naked_expr = dcalc naked_gexpr] plus the
    annotations.

    A few tips on using this GADT:

    - To write a function that handles cases from different ASTs, explicit the
      type variables: [fun (type a) (x: a naked_gexpr) -> ...]
    - For recursive functions, you may need to additionally explicit the
      generalisation of the variable: [let rec f: type a . a naked_gexpr -> ...]
    - Always think of using the pre-defined map/fold functions in [Expr] rather
      than completely defining your recursion manually. *)

and ('a, 't) naked_gexpr =
  (* Constructors common to all ASTs *)
  | ELit : lit -> (([< all ] as 'a), 't) naked_gexpr
  | EApp : {
      f : ('a, 't) gexpr;
      args : ('a, 't) gexpr list;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EOp : {
      op : 'a operator;
      tys : typ list;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EArray : ('a, 't) gexpr list -> (([< all ] as 'a), 't) naked_gexpr
  | EVar :
      ('a, 't) naked_gexpr Bindlib.var
      -> (([< all ] as 'a), 't) naked_gexpr
  | EAbs : {
      binder : (('a, 't) naked_gexpr, ('a, 't) gexpr) Bindlib.mbinder;
      tys : typ list;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EIfThenElse : {
      cond : ('a, 't) gexpr;
      etrue : ('a, 't) gexpr;
      efalse : ('a, 't) gexpr;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EStruct : {
      name : StructName.t;
      fields : ('a, 't) gexpr StructField.Map.t;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EInj : {
      name : EnumName.t;
      e : ('a, 't) gexpr;
      cons : EnumConstructor.t;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | EMatch : {
      name : EnumName.t;
      e : ('a, 't) gexpr;
      cases : ('a, 't) gexpr EnumConstructor.Map.t;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  | ETuple : ('a, 't) gexpr list -> (([< all ] as 'a), 't) naked_gexpr
  | ETupleAccess : {
      e : ('a, 't) gexpr;
      index : int;
      size : int;
    }
      -> (([< all ] as 'a), 't) naked_gexpr
  (* Early stages *)
  | ELocation :
      'a glocation
      -> (([< all > `ExplicitScopes ] as 'a), 't) naked_gexpr
  | EScopeCall : {
      scope : ScopeName.t;
      args : ('a, 't) gexpr ScopeVar.Map.t;
    }
      -> (([< all > `ExplicitScopes ] as 'a), 't) naked_gexpr
  | EDStructAccess : {
      name_opt : StructName.t option;
      e : ('a, 't) gexpr;
      field : IdentName.t;
    }
      -> (([< all > `SyntacticNames ] as 'a), 't) naked_gexpr
      (** [desugared] has ambiguous struct fields *)
  | EStructAccess : {
      name : StructName.t;
      e : ('a, 't) gexpr;
      field : StructField.t;
    }
      -> (([< all > `ResolvedNames ] as 'a), 't) naked_gexpr
      (** Resolved struct/enums, after [desugared] *)
  (* Lambda-like *)
  | EAssert : ('a, 't) gexpr -> (([< all > `Assertions ] as 'a), 't) naked_gexpr
  (* Default terms *)
  | EDefault : {
      excepts : ('a, 't) gexpr list;
      just : ('a, 't) gexpr;
      cons : ('a, 't) gexpr;
    }
      -> (([< all > `DefaultTerms ] as 'a), 't) naked_gexpr
  | EEmptyError : (([< all > `DefaultTerms ] as 'a), 't) naked_gexpr
  | EErrorOnEmpty :
      ('a, 't) gexpr
      -> (([< all > `DefaultTerms ] as 'a), 't) naked_gexpr
  (* Lambda calculus with exceptions *)
  | ERaise : except -> (([< all > `Exceptions ] as 'a), 't) naked_gexpr
  | ECatch : {
      body : ('a, 't) gexpr;
      exn : except;
      handler : ('a, 't) gexpr;
    }
      -> (([< all > `Exceptions ] as 'a), 't) naked_gexpr

type ('a, 't) boxed_gexpr = (('a, 't) naked_gexpr Bindlib.box, 't) Marked.t
(** The annotation is lifted outside of the box for expressions *)

type 'e boxed = ('a, 't) boxed_gexpr constraint 'e = ('a, 't) gexpr
(** [('a, 't) gexpr boxed] is [('a, 't) boxed_gexpr]. The difference with
    [('a, 't) gexpr Bindlib.box] is that the annotations is outside of the box,
    and can therefore be accessed without the need to resolve the box *)

type ('e, 'b) binder = (('a, 't) naked_gexpr, 'b) Bindlib.binder
  constraint 'e = ('a, 't) gexpr
(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax *)

type ('e, 'b) mbinder = (('a, 't) naked_gexpr, 'b) Bindlib.mbinder
  constraint 'e = ('a, 't) gexpr

(** {2 Markings} *)

type untyped = { pos : Pos.t } [@@ocaml.unboxed]
type typed = { pos : Pos.t; ty : typ }

(** The generic type of AST markings. Using a GADT allows functions to be
    polymorphic in the marking, but still do transformations on types when
    appropriate. Expected to fill the ['t] parameter of [gexpr] and [gexpr] (a
    ['t] annotation different from this type is used in the middle of the typing
    processing, but all visible ASTs should otherwise use this. *)
type _ mark = Untyped : untyped -> untyped mark | Typed : typed -> typed mark

(** Useful for errors and printing, for example *)
type any_expr = AnyExpr : (_, _ mark) gexpr -> any_expr

(** {2 Higher-level program structure} *)

(** Constructs scopes and programs on top of expressions. The ['e] type
    parameter throughout is expected to match instances of the [gexpr] type
    defined above. Markings are constrained to the [mark] GADT defined above.
    Note that this structure is at the moment only relevant for [dcalc] and
    [lcalc], as [scopelang] has its own scope structure, as the name implies. *)

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

type 'e scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ;
  scope_let_expr : 'e;
  scope_let_next : ('e, 'e scope_body_expr) binder;
  (* todo ? Factorise the code_item _list type below and use it here *)
  scope_let_pos : Pos.t;
}
  constraint 'e = (_ any, _ mark) gexpr
(** This type is parametrized by the expression type so it can be reused in
    later intermediate representations. *)

(** A scope let-binding has all the information necessary to make a proper
    let-binding expression, plus an annotation for the kind of the let-binding
    that comes from the compilation of a {!module: Scopelang.Ast} statement. *)
and 'e scope_body_expr =
  | Result of 'e
  | ScopeLet of 'e scope_let
  constraint 'e = (_ any, _ mark) gexpr

type 'e scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : ('e, 'e scope_body_expr) binder;
}
  constraint 'e = (_ any, _ mark) gexpr
(** Instead of being a single expression, we give a little more ad-hoc structure
    to the scope body by decomposing it in an ordered list of let-bindings, and
    a result expression that uses the let-binded variables. The first binder is
    the argument of type [scope_body_input_struct]. *)

type 'e code_item =
  | ScopeDef of ScopeName.t * 'e scope_body
  | Topdef of TopdefName.t * typ * 'e

(* A chained list, but with a binder for each element into the next: [x := let a
   = e1 in e2] is thus [Cons (e1, {a. Cons (e2, {x. Nil})})] *)
type 'e code_item_list =
  | Nil
  | Cons of 'e code_item * ('e, 'e code_item_list) binder

type struct_ctx = typ StructField.Map.t StructName.Map.t
type enum_ctx = typ EnumConstructor.Map.t EnumName.Map.t

type scope_out_struct = {
  out_struct_name : StructName.t;
  out_struct_fields : StructField.t ScopeVar.Map.t;
}

type decl_ctx = {
  ctx_enums : enum_ctx;
  ctx_structs : struct_ctx;
  ctx_struct_fields : StructField.t StructName.Map.t IdentName.Map.t;
      (** needed for disambiguation (desugared -> scope) *)
  ctx_scopes : scope_out_struct ScopeName.Map.t;
}

type 'e program = { decl_ctx : decl_ctx; code_items : 'e code_item_list }
