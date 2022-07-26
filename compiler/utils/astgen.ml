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

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type marked_typ = typ Marked.pos

and typ =
  | TLit of typ_lit
  | TTuple of marked_typ list * StructName.t option
  | TEnum of marked_typ list * EnumName.t
  | TArrow of marked_typ * marked_typ
  | TArray of marked_typ
  | TAny

type date = Runtime.date
type duration = Runtime.duration

type lit =
  | LBool of bool
  | LEmptyError (* Fixme: exclude for lcalc *)
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
type desugared = [ `Desugared ]
type scopelang = [ `Scopelang ]
type dcalc = [ `Dcalc ]
type lcalc = [ `Lcalc ]
type scalc = [ `Scalc ]

type ('a, 'm) marked_gexpr = (('a, 'm) gexpr, 'm) Marked.t

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax *)
and ('a, 'm) gexpr =
  (* Constructors common to all ASTs *)
  | ELit : lit -> ('a, 'm) gexpr
  | EApp : ('a, 'm) marked_gexpr * ('a, 'm) marked_gexpr list -> ('a, 'm) gexpr
  | EOp : operator -> ('a, 'm) gexpr
  | EArray : ('a, 'm) marked_gexpr list -> ('a, 'm) gexpr
  (* All but statement calculus *)
  | EVar :
      ('a, 'm) gexpr Bindlib.var
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 'm) gexpr
  | EAbs :
      (('a, 'm) gexpr, ('a, 'm) marked_gexpr) Bindlib.mbinder
      * typ Marked.pos list
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 'm) gexpr
  | EIfThenElse :
      ('a, 'm) marked_gexpr * ('a, 'm) marked_gexpr * ('a, 'm) marked_gexpr
      -> (([< desugared | scopelang | dcalc | lcalc ] as 'a), 'm) gexpr
  (* (* Early stages *) | ELocation: location -> ([< desugared | scopelang ] as
     'a, 'm) gexpr | EStruct: StructName.t * ('a, 'm) marked_gexpr
     StructFieldMap.t -> ([< desugared | scopelang ] as 'a, 'm) gexpr |
     EStructAccess: ('a, 'm) marked_gexpr * StructFieldName.t * StructName.t ->
     ([< desugared | scopelang ] as 'a, 'm) gexpr | EEnumInj: ('a, 'm)
     marked_gexpr * EnumConstructor.t * EnumName.t -> ([< desugared | scopelang ]
     as 'a, 'm) gexpr | EMatchS: ('a, 'm) marked_gexpr * EnumName.t * ('a, 'm)
     marked_gexpr EnumConstructorMap.t -> ([< desugared | scopelang ] as 'a, 'm)
     gexpr *)
  (* Default terms *)
  | EDefault :
      ('a, 'm) marked_gexpr list * ('a, 'm) marked_gexpr * ('a, 'm) marked_gexpr
      -> (([< desugared | scopelang | dcalc ] as 'a), 'm) gexpr
  | ErrorOnEmpty :
      ('a, 'm) marked_gexpr
      -> (([< desugared | scopelang | dcalc ] as 'a), 'm) gexpr
  (* Lambda-like *)
  | ETuple :
      ('a, 'm) marked_gexpr list * StructName.t option
      -> (([< dcalc | lcalc ] as 'a), 'm) gexpr
  | ETupleAccess :
      ('a, 'm) marked_gexpr * int * StructName.t option * typ Marked.pos list
      -> (([< dcalc | lcalc ] as 'a), 'm) gexpr
  | EInj :
      ('a, 'm) marked_gexpr * int * EnumName.t * typ Marked.pos list
      -> (([< dcalc | lcalc ] as 'a), 'm) gexpr
  | EMatch :
      ('a, 'm) marked_gexpr * ('a, 'm) marked_gexpr list * EnumName.t
      -> (([< dcalc | lcalc ] as 'a), 'm) gexpr
  | EAssert : ('a, 'm) marked_gexpr -> (([< dcalc | lcalc ] as 'a), 'm) gexpr
  (* Lambda calculus with exceptions *)
  | ERaise : except -> ((lcalc as 'a), 'm) gexpr
  | ECatch :
      ('a, 'm) marked_gexpr * except * ('a, 'm) marked_gexpr
      -> ((lcalc as 'a), 'm) gexpr

(* (\* Statement calculus *\)
 * | ESVar: LocalName.t -> (scalc as 'a, 'm) gexpr
 * | ESStruct: ('a, 'm) marked_gexpr list * StructName.t -> (scalc as 'a, 'm) gexpr
 * | ESStructFieldAccess: ('a, 'm) marked_gexpr * StructFieldName.t * StructName.t -> (scalc as 'a, 'm) gexpr
 * | ESInj: ('a, 'm) marked_gexpr * EnumConstructor.t * EnumName.t -> (scalc as 'a, 'm) gexpr
 * | ESFunc: TopLevelName.t -> (scalc as 'a, 'm) gexpr *)
