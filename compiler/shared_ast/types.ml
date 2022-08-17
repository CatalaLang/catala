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

(** Abstract syntax tree for the default calculus *)

(** {1 Abstract syntax tree} *)

(** {2 Types} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type marked_typ = typ Marked.pos

and typ =
  | TLit of typ_lit
  | TTuple of marked_typ list * StructName.t option
  | TEnum of marked_typ list * EnumName.t
  | TArrow of marked_typ * marked_typ
  | TArray of marked_typ
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
  | VarDef of typ
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

(* type scalc = [ `Scalc ] *)

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

type ('a, 't) marked_gexpr = (('a, 't) gexpr, 't) Marked.t
(** General expressions: groups all expression cases of the different ASTs, and
    uses a GADT to eliminate irrelevant cases for each one. The ['t] annotations
    are also totally unconstrained at this point. The dcalc exprs, for example,
    are then defined with [type expr = dcalc gexpr] plus the annotations. *)

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax *)
and ('a, 't) gexpr =
  (* Constructors common to all ASTs *)
  | ELit : 'a glit -> ('a any, 't) gexpr
  | EApp :
      ('a, 't) marked_gexpr * ('a, 't) marked_gexpr list
      -> ('a any, 't) gexpr
  | EOp : operator -> ('a any, 't) gexpr
  | EArray : ('a, 't) marked_gexpr list -> ('a any, 't) gexpr
  (* All but statement calculus *)
  | EVar :
      ('a, 't) gexpr Bindlib.var
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) gexpr
  | EAbs :
      (('a, 't) gexpr, ('a, 't) marked_gexpr) Bindlib.mbinder
      * typ Marked.pos list
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) gexpr
  | EIfThenElse :
      ('a, 't) marked_gexpr * ('a, 't) marked_gexpr * ('a, 't) marked_gexpr
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 't) gexpr
  (* (* Early stages *) | ELocation: location -> ([< desugared | scopelang ] as
     'a, 't) gexpr | EStruct: StructName.t * ('a, 't) marked_gexpr
     StructFieldMap.t -> ([< desugared | scopelang ] as 'a, 't) gexpr |
     EStructAccess: ('a, 't) marked_gexpr * StructFieldName.t * StructName.t ->
     ([< desugared | scopelang ] as 'a, 't) gexpr | EEnumInj: ('a, 't)
     marked_gexpr * EnumConstructor.t * EnumName.t -> ([< desugared | scopelang
     ] as 'a, 't) gexpr | EMatchS: ('a, 't) marked_gexpr * EnumName.t * ('a, 't)
     marked_gexpr EnumConstructorMap.t -> ([< desugared | scopelang ] as 'a, 't)
     gexpr *)
  (* Lambda-like *)
  | ETuple :
      ('a, 't) marked_gexpr list * StructName.t option
      -> (([< dcalc | lcalc ] as 'a), 't) gexpr
  | ETupleAccess :
      ('a, 't) marked_gexpr * int * StructName.t option * typ Marked.pos list
      -> (([< dcalc | lcalc ] as 'a), 't) gexpr
  | EInj :
      ('a, 't) marked_gexpr * int * EnumName.t * typ Marked.pos list
      -> (([< dcalc | lcalc ] as 'a), 't) gexpr
  | EMatch :
      ('a, 't) marked_gexpr * ('a, 't) marked_gexpr list * EnumName.t
      -> (([< dcalc | lcalc ] as 'a), 't) gexpr
  | EAssert : ('a, 't) marked_gexpr -> (([< dcalc | lcalc ] as 'a), 't) gexpr
  (* Default terms *)
  | EDefault :
      ('a, 't) marked_gexpr list * ('a, 't) marked_gexpr * ('a, 't) marked_gexpr
      -> (([< desugared | scopelang | dcalc ] as 'a), 't) gexpr
  | ErrorOnEmpty :
      ('a, 't) marked_gexpr
      -> (([< desugared | scopelang | dcalc ] as 'a), 't) gexpr
  (* Lambda calculus with exceptions *)
  | ERaise : except -> ((lcalc as 'a), 't) gexpr
  | ECatch :
      ('a, 't) marked_gexpr * except * ('a, 't) marked_gexpr
      -> ((lcalc as 'a), 't) gexpr

(* (\* Statement calculus *\)
 * | ESVar: LocalName.t -> (scalc as 'a, 't) gexpr
 * | ESStruct: ('a, 't) marked_gexpr list * StructName.t -> (scalc as 'a, 't) gexpr
 * | ESStructFieldAccess: ('a, 't) marked_gexpr * StructFieldName.t * StructName.t -> (scalc as 'a, 't) gexpr
 * | ESInj: ('a, 't) marked_gexpr * EnumConstructor.t * EnumName.t -> (scalc as 'a, 't) gexpr
 * | ESFunc: TopLevelName.t -> (scalc as 'a, 't) gexpr *)

type 'e anyexpr = 'e constraint 'e = (_ any, _) gexpr
(** Shorter alias for functions taking any kind of expression *)

(** {2 Markings} *)

type untyped = { pos : Pos.t } [@@ocaml.unboxed]
type typed = { pos : Pos.t; ty : marked_typ }
(* type inferring = { pos : Pos.t; uf : Infer.unionfind_typ } *)

(** The generic type of AST markings. Using a GADT allows functions to be
    polymorphic in the marking, but still do transformations on types when
    appropriate. Expected to fill the ['t] parameter of [gexpr] and
    [marked_gexpr] (a ['t] annotation different from this type is used in the
    middle of the typing processing, but all visible ASTs should otherwise use
    this. *)
type _ mark = Untyped : untyped -> untyped mark | Typed : typed -> typed mark

type 'e marked = ('e, 'm mark) Marked.t constraint 'e = ('a, 'm mark) gexpr

(** Useful for errors and printing, for example *)
type any_marked_expr =
  | AnyExpr : (_ any, _ mark) marked_gexpr -> any_marked_expr

(** {2 Higher-level program structure} *)

(** Constructs scopes and programs on top of expressions. We may use the [gexpr]
    type above at some point, but at the moment this is polymorphic in the types
    of the expressions. Their markings are constrained to belong to the [mark]
    GADT defined above. *)

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
  scope_let_typ : marked_typ;
  scope_let_expr : 'e marked;
  scope_let_next : ('e, 'e scope_body_expr) Bindlib.binder;
  scope_let_pos : Pos.t;
}
  constraint 'e = ('a, 'm mark) gexpr
(** This type is parametrized by the expression type so it can be reused in
    later intermediate representations. *)

(** A scope let-binding has all the information necessary to make a proper
    let-binding expression, plus an annotation for the kind of the let-binding
    that comes from the compilation of a {!module: Scopelang.Ast} statement. *)
and 'e scope_body_expr =
  | Result of 'e marked
  | ScopeLet of 'e scope_let
  constraint 'e = ('a, 'm mark) gexpr

type 'e scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : ('e, 'e scope_body_expr) Bindlib.binder;
}
(** Instead of being a single expression, we give a little more ad-hoc structure
    to the scope body by decomposing it in an ordered list of let-bindings, and
    a result expression that uses the let-binded variables. The first binder is
    the argument of type [scope_body_input_struct]. *)

type 'e scope_def = {
  scope_name : ScopeName.t;
  scope_body : 'e scope_body;
  scope_next : ('e, 'e scopes) Bindlib.binder;
}

(** Finally, we do the same transformation for the whole program for the kinded
    lets. This permit us to use bindlib variables for scopes names. *)
and 'e scopes =
  | Nil
  | ScopeDef of 'e scope_def
  constraint 'e = ('a, 'm mark) gexpr

type struct_ctx = (StructFieldName.t * marked_typ) list StructMap.t

type decl_ctx = {
  ctx_enums : (EnumConstructor.t * marked_typ) list EnumMap.t;
  ctx_structs : struct_ctx;
}

type 'e program = { decl_ctx : decl_ctx; scopes : 'e scopes }
