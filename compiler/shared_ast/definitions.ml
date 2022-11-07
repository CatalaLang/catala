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

open Utils
module Runtime = Runtime_ocaml.Runtime

module ScopeName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module ScopeSet : Set.S with type elt = ScopeName.t = Set.Make (ScopeName)
module ScopeMap : Map.S with type key = ScopeName.t = Map.Make (ScopeName)

module StructName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructFieldName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructMap : Map.S with type key = StructName.t = Map.Make (StructName)

module EnumName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumMap : Map.S with type key = EnumName.t = Map.Make (EnumName)

(** Only used by surface *)

module RuleName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module RuleMap : Map.S with type key = RuleName.t = Map.Make (RuleName)
module RuleSet : Set.S with type elt = RuleName.t = Set.Make (RuleName)

module LabelName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module LabelMap : Map.S with type key = LabelName.t = Map.Make (LabelName)
module LabelSet : Set.S with type elt = LabelName.t = Set.Make (LabelName)

(** Only used by desugared/scopelang *)

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module ScopeVarSet : Set.S with type elt = ScopeVar.t = Set.Make (ScopeVar)
module ScopeVarMap : Map.S with type key = ScopeVar.t = Map.Make (ScopeVar)

module SubScopeName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module SubScopeNameSet : Set.S with type elt = SubScopeName.t =
  Set.Make (SubScopeName)

module SubScopeMap : Map.S with type key = SubScopeName.t =
  Map.Make (SubScopeName)

module StructFieldMap : Map.S with type key = StructFieldName.t =
  Map.Make (StructFieldName)

module EnumConstructorMap : Map.S with type key = EnumConstructor.t =
  Map.Make (EnumConstructor)

module StateName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

(** {1 Abstract syntax tree} *)

(** {2 Types} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type typ = naked_typ Marked.pos

and naked_typ =
  | TLit of typ_lit
  | TTuple of typ list
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TOption of typ
  | TArrow of typ * typ
  | TArray of typ
  | TAny

(** {2 Constants and operators} *)

type date = Runtime.date
type duration = Runtime.duration

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
  | VarDef of naked_typ
      (** During code generation, we need to know the type of the variable being
          logged for embedding *)
  | BeginCall
  | EndCall
  | PosRecordIfTrueBool

type unop =
  | Not
  | Minus of op_kind
  | Log of log_entry * Uid.MarkedString.info list
  | Length
  | IntToRat
  | MoneyToRat
  | RatToMoney
  | GetDay
  | GetMonth
  | GetYear
  | FirstDayOfMonth
  | LastDayOfMonth
  | RoundMoney
  | RoundDecimal

type operator = Ternop of ternop | Binop of binop | Unop of unop
type except = ConflictError | EmptyError | NoValueProvided | Crash

(** {2 Generic expressions} *)

(** Define a common base type for the expressions in most passes of the compiler *)

type desugared = [ `Desugared ]
type scopelang = [ `Scopelang ]
type dcalc = [ `Dcalc ]
type lcalc = [ `Lcalc ]
type 'a any = [< desugared | scopelang | dcalc | lcalc ] as 'a

(** Literals are the same throughout compilation except for the [LEmptyError]
    case which is eliminated midway through. *)
type 'a glit =
  | LBool : bool -> 'a glit
  | LEmptyError : [< desugared | scopelang | dcalc ] glit
  | LInt : Runtime.integer -> 'a glit
  | LRat : Runtime.decimal -> 'a glit
  | LMoney : Runtime.money -> 'a glit
  | LUnit : 'a glit
  | LDate : date -> 'a glit
  | LDuration : duration -> 'a glit

(** Locations are handled differently in [desugared] and [scopelang] *)
type 'a glocation =
  | DesugaredScopeVar :
      ScopeVar.t Marked.pos * StateName.t option
      -> desugared glocation
  | ScopelangScopeVar : ScopeVar.t Marked.pos -> scopelang glocation
  | SubScopeVar :
      ScopeName.t * SubScopeName.t Marked.pos * ScopeVar.t Marked.pos
      -> [< desugared | scopelang ] glocation

type ('a, 't) gexpr = (('a, 't) naked_gexpr, 't) Marked.t
(** General expressions: groups all expression cases of the different ASTs, and
    uses a GADT to eliminate irrelevant cases for each one. The ['t] annotations
    are also totally unconstrained at this point. The dcalc exprs, for example,
    are then defined with [type naked_expr = dcalc naked_gexpr] plus the
    annotations.

    A few tips on using this GADT:

    - To write a function that handles cases from different ASTs, explicit the
      type variables: [fun (type a) (x: a naked_gexpr) -> ...]
    - For recursive functions, you may need to additionally explicit the
      generalisation of the variable: [let rec f: type a . a naked_gexpr -> ...] *)

and ('a, 't) naked_gexpr =
  (* Constructors common to all ASTs *)
  | ELit : 'a glit -> ('a any, 't) naked_gexpr
  | EApp : {
      f : ('a, 't) gexpr;
      args : ('a, 't) gexpr list;
    }
      -> ('a any, 't) naked_gexpr
  | EOp : operator -> ('a any, 't) naked_gexpr
  | EArray : ('a, 't) gexpr list -> ('a any, 't) naked_gexpr
  | EVar : ('a, 't) naked_gexpr Bindlib.var -> ('a any, 't) naked_gexpr
  | EAbs : {
      binder : (('a, 't) naked_gexpr, ('a, 't) gexpr) Bindlib.mbinder;
      tys : typ list;
    }
      -> ('a any, 't) naked_gexpr
  | EIfThenElse : {
      cond : ('a, 't) gexpr;
      etrue : ('a, 't) gexpr;
      efalse : ('a, 't) gexpr;
    }
      -> ('a any, 't) naked_gexpr
  | EStruct : {
      name : StructName.t;
      fields : ('a, 't) gexpr StructFieldMap.t;
    }
      -> ('a any, 't) naked_gexpr
  | EStructAccess : {
      name : StructName.t;
      e : ('a, 't) gexpr;
      field : StructFieldName.t;
    }
      -> ('a any, 't) naked_gexpr
  | EInj : {
      name : EnumName.t;
      e : ('a, 't) gexpr;
      cons : EnumConstructor.t;
    }
      -> ('a any, 't) naked_gexpr
  | EMatch : {
      name : EnumName.t;
      e : ('a, 't) gexpr;
      cases : ('a, 't) gexpr EnumConstructorMap.t;
    }
      -> ('a any, 't) naked_gexpr
  (* Early stages *)
  | ELocation :
      'a glocation
      -> (([< desugared | scopelang ] as 'a), 't) naked_gexpr
  | EScopeCall : {
      scope : ScopeName.t;
      args : ('a, 't) gexpr ScopeVarMap.t;
    }
      -> (([< desugared | scopelang ] as 'a), 't) naked_gexpr
  (* Lambda-like *)
  | EAssert : ('a, 't) gexpr -> (([< dcalc | lcalc ] as 'a), 't) naked_gexpr
  (* Default terms *)
  | EDefault : {
      excepts : ('a, 't) gexpr list;
      just : ('a, 't) gexpr;
      cons : ('a, 't) gexpr;
    }
      -> (([< desugared | scopelang | dcalc ] as 'a), 't) naked_gexpr
  | EErrorOnEmpty :
      ('a, 't) gexpr
      -> (([< desugared | scopelang | dcalc ] as 'a), 't) naked_gexpr
  (* Lambda calculus with exceptions *)
  | ETuple : ('a, 't) gexpr list -> ((lcalc as 'a), 't) naked_gexpr
  | ETupleAccess : {
      e : ('a, 't) gexpr;
      index : int;
      size : int;
    }
      -> ((lcalc as 'a), 't) naked_gexpr
  | ERaise : except -> ((lcalc as 'a), 't) naked_gexpr
  | ECatch : {
      body : ('a, 't) gexpr;
      exn : except;
      handler : ('a, 't) gexpr;
    }
      -> ((lcalc as 'a), 't) naked_gexpr

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

type 'e scope_def = {
  scope_name : ScopeName.t;
  scope_body : 'e scope_body;
  scope_next : ('e, 'e scopes) binder;
}
  constraint 'e = (_ any, _ mark) gexpr

(** Finally, we do the same transformation for the whole program for the kinded
    lets. This permit us to use bindlib variables for scopes names. *)
and 'e scopes =
  | Nil
  | ScopeDef of 'e scope_def
  constraint 'e = (_ any, _ mark) gexpr

type struct_ctx = typ StructFieldMap.t StructMap.t
type enum_ctx = typ EnumConstructorMap.t EnumMap.t

type scope_out_struct = {
  out_struct_name : StructName.t;
  out_struct_fields : StructFieldName.t ScopeVarMap.t;
}

type decl_ctx = {
  ctx_enums : enum_ctx;
  ctx_structs : struct_ctx;
  ctx_scopes : scope_out_struct ScopeMap.t;
}

type 'e program = { decl_ctx : decl_ctx; scopes : 'e scopes }
