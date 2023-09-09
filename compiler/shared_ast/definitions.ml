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
module ModuleName = Uid.Module

module ScopeName =
  Uid.Gen_qualified
    (struct
      let style = Ocolor_types.(Fg (C4 hi_magenta))
    end)
    ()

module TopdefName =
  Uid.Gen_qualified
    (struct
      let style = Ocolor_types.(Fg (C4 hi_green))
    end)
    ()

module StructName =
  Uid.Gen_qualified
    (struct
      let style = Ocolor_types.(Fg (C4 cyan))
    end)
    ()

module StructField =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 magenta))
    end)
    ()

module EnumName =
  Uid.Gen_qualified
    (struct
      let style = Ocolor_types.(Fg (C4 cyan))
    end)
    ()

module EnumConstructor =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 magenta))
    end)
    ()

(** Only used by surface *)

module RuleName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_white))
    end)
    ()

module LabelName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_cyan))
    end)
    ()

(** Used for unresolved structs/maps in desugared *)

module Ident = String

(** Only used by desugared/scopelang *)

module ScopeVar =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_white))
    end)
    ()

module SubScopeName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_magenta))
    end)
    ()

module StateName =
  Uid.Gen
    (struct
      let style = Ocolor_types.(Fg (C4 hi_cyan))
    end)
    ()

(** {1 Abstract syntax tree} *)

(** Define a common base type for the expressions in most passes of the compiler *)

(** {2 Phantom types used to select relevant cases on the generic AST}

    we instantiate them with a polymorphic variant to take advantage of
    sub-typing. The values aren't actually used. *)

(** These types allow to select the features present in any given expression
    type *)

type yes = Yes

type no =
  | No
      (** Phantom types used in the definitions below. We don't make them
          abstract, because the typer needs to know that their intersection is
          empty. *)

type desugared =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : yes
  ; resolved : no
  ; syntacticNames : yes
  ; resolvedNames : no
  ; scopeVarStates : yes
  ; scopeVarSimpl : no
  ; explicitScopes : yes
  ; assertions : no
  ; defaultTerms : yes
  ; exceptions : no
  ; custom : no >

type scopelang =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; resolvedNames : yes
  ; scopeVarStates : no
  ; scopeVarSimpl : yes
  ; explicitScopes : yes
  ; assertions : no
  ; defaultTerms : yes
  ; exceptions : no
  ; custom : no >

type dcalc =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; resolvedNames : yes
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : yes
  ; exceptions : no
  ; custom : no >

type lcalc =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; resolvedNames : yes
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : no
  ; exceptions : yes
  ; custom : no >

type 'a any = < .. > as 'a
(** ['a any] is 'a, but adds the constraint that it should be restricted to
    valid AST kinds *)

type ('a, 'b) dcalc_lcalc =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; resolvedNames : yes
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : 'a
  ; exceptions : 'b
  ; custom : no >
(** This type regroups Dcalc and Lcalc ASTs. *)

(** {2 Types} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type typ = naked_typ Mark.pos

and naked_typ =
  | TLit of typ_lit
  | TTuple of typ list
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TOption of typ
  | TArrow of typ list * typ
  | TArray of typ
  | TAny
  | TClosureEnv  (** Hides an existential type needed for closure conversion *)

(** {2 Constants and operators} *)

type date = Runtime.date
type date_rounding = Runtime.date_rounding
type duration = Runtime.duration

type var_def_log = {
  log_typ : naked_typ;
  log_io_input : Runtime.io_input;
  log_io_output : bool;
}

type log_entry =
  | VarDef of var_def_log
      (** During code generation, we need to know the type of the variable being
          logged for embedding as well as its I/O properties. *)
  | BeginCall
  | EndCall
  | PosRecordIfTrueBool

module Op = struct
  (** Classification of operators on how they should be typed *)

  type monomorphic = < monomorphic : yes >
  (** Operands and return types of the operator are fixed *)

  type polymorphic = < polymorphic : yes >
  (** The operator is truly polymorphic: it's the same runtime function that may
      work on multiple types. We require that resolving the argument types from
      right to left trivially resolves all type variables declared in the
      operator type. *)

  type overloaded = < overloaded : yes >
  (** The operator is ambiguous and requires the types of its arguments to be
      known before it can be typed, using a pre-defined table *)

  type resolved = < resolved : yes >
  (** Explicit monomorphic versions of the overloaded operators *)

  type _ t =
    (* unary *)
    (* * monomorphic *)
    | Not : < monomorphic ; .. > t
    | GetDay : < monomorphic ; .. > t
    | GetMonth : < monomorphic ; .. > t
    | GetYear : < monomorphic ; .. > t
    | FirstDayOfMonth : < monomorphic ; .. > t
    | LastDayOfMonth : < monomorphic ; .. > t
    (* * polymorphic *)
    | Length : < polymorphic ; .. > t
    | Log : log_entry * Uid.MarkedString.info list -> < polymorphic ; .. > t
    | ToClosureEnv : < polymorphic ; .. > t
    | FromClosureEnv : < polymorphic ; .. > t
    (* * overloaded *)
    | Minus : < overloaded ; .. > t
    | Minus_int : < resolved ; .. > t
    | Minus_rat : < resolved ; .. > t
    | Minus_mon : < resolved ; .. > t
    | Minus_dur : < resolved ; .. > t
    | ToRat : < overloaded ; .. > t
    | ToRat_int : < resolved ; .. > t
    | ToRat_mon : < resolved ; .. > t
    | ToMoney : < overloaded ; .. > t
    | ToMoney_rat : < resolved ; .. > t
    | Round : < overloaded ; .. > t
    | Round_rat : < resolved ; .. > t
    | Round_mon : < resolved ; .. > t
    (* binary *)
    (* * monomorphic *)
    | And : < monomorphic ; .. > t
    | Or : < monomorphic ; .. > t
    | Xor : < monomorphic ; .. > t
    (* * polymorphic *)
    | Eq : < polymorphic ; .. > t
    | Map : < polymorphic ; .. > t
    | Concat : < polymorphic ; .. > t
    | Filter : < polymorphic ; .. > t
    (* * overloaded *)
    | Add : < overloaded ; .. > t
    | Add_int_int : < resolved ; .. > t
    | Add_rat_rat : < resolved ; .. > t
    | Add_mon_mon : < resolved ; .. > t
    | Add_dat_dur : date_rounding -> < resolved ; .. > t
    | Add_dur_dur : < resolved ; .. > t
    | Sub : < overloaded ; .. > t
    | Sub_int_int : < resolved ; .. > t
    | Sub_rat_rat : < resolved ; .. > t
    | Sub_mon_mon : < resolved ; .. > t
    | Sub_dat_dat : < resolved ; .. > t
    | Sub_dat_dur : < resolved ; .. > t
    | Sub_dur_dur : < resolved ; .. > t
    | Mult : < overloaded ; .. > t
    | Mult_int_int : < resolved ; .. > t
    | Mult_rat_rat : < resolved ; .. > t
    | Mult_mon_rat : < resolved ; .. > t
    | Mult_dur_int : < resolved ; .. > t
    | Div : < overloaded ; .. > t
    | Div_int_int : < resolved ; .. > t
    | Div_rat_rat : < resolved ; .. > t
    | Div_mon_rat : < resolved ; .. > t
    | Div_mon_mon : < resolved ; .. > t
    | Div_dur_dur : < resolved ; .. > t
    | Lt : < overloaded ; .. > t
    | Lt_int_int : < resolved ; .. > t
    | Lt_rat_rat : < resolved ; .. > t
    | Lt_mon_mon : < resolved ; .. > t
    | Lt_dat_dat : < resolved ; .. > t
    | Lt_dur_dur : < resolved ; .. > t
    | Lte : < overloaded ; .. > t
    | Lte_int_int : < resolved ; .. > t
    | Lte_rat_rat : < resolved ; .. > t
    | Lte_mon_mon : < resolved ; .. > t
    | Lte_dat_dat : < resolved ; .. > t
    | Lte_dur_dur : < resolved ; .. > t
    | Gt : < overloaded ; .. > t
    | Gt_int_int : < resolved ; .. > t
    | Gt_rat_rat : < resolved ; .. > t
    | Gt_mon_mon : < resolved ; .. > t
    | Gt_dat_dat : < resolved ; .. > t
    | Gt_dur_dur : < resolved ; .. > t
    | Gte : < overloaded ; .. > t
    | Gte_int_int : < resolved ; .. > t
    | Gte_rat_rat : < resolved ; .. > t
    | Gte_mon_mon : < resolved ; .. > t
    | Gte_dat_dat : < resolved ; .. > t
    | Gte_dur_dur : < resolved ; .. > t
    (* Todo: Eq is not an overload at the moment, but it should be one. The
       trick is that it needs generation of specific code for arrays, every
       struct and enum: operators [Eq_structs of StructName.t], etc. *)
    | Eq_int_int : < resolved ; .. > t
    | Eq_rat_rat : < resolved ; .. > t
    | Eq_mon_mon : < resolved ; .. > t
    | Eq_dur_dur : < resolved ; .. > t
    | Eq_dat_dat : < resolved ; .. > t
    (* ternary *)
    (* * polymorphic *)
    | Reduce : < polymorphic ; .. > t
    | Fold : < polymorphic ; .. > t
    | HandleDefault : < polymorphic ; .. > t
    | HandleDefaultOpt : < polymorphic ; .. > t
end

type 'a operator = 'a Op.t
type except = ConflictError | EmptyError | NoValueProvided | Crash

(** {2 Markings} *)

type untyped = { pos : Pos.t } [@@caml.unboxed]
type typed = { pos : Pos.t; ty : typ }
type 'a custom = { pos : Pos.t; custom : 'a }

(** Using empty markings will ensure terms can't be constructed: used for
    example in interfaces to ensure that they don't contain any expressions *)
type nil = |

(** The generic type of AST markings. Using a GADT allows functions to be
    polymorphic in the marking, but still do transformations on types when
    appropriate. The [Custom] case can be used within passes that need to store
    specific information, e.g. typing *)
type _ mark =
  | Untyped : untyped -> untyped mark
  | Typed : typed -> typed mark
  | Custom : 'a custom -> 'a custom mark

type ('a, 'm) marked = ('a, 'm mark) Mark.ed
(** Type of values marked with the above standard mark GADT *)

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

(** External references are resolved to strings that point to functions or
    constants in the end, but we need to keep different references for typing *)
type external_ref =
  | External_value of TopdefName.t
  | External_scope of ScopeName.t

(** Locations are handled differently in [desugared] and [scopelang] *)
type 'a glocation =
  | DesugaredScopeVar : {
      name : ScopeVar.t Mark.pos;
      state : StateName.t option;
    }
      -> < scopeVarStates : yes ; .. > glocation
  | ScopelangScopeVar : {
      name : ScopeVar.t Mark.pos;
    }
      -> < scopeVarSimpl : yes ; .. > glocation
  | SubScopeVar : {
      scope : ScopeName.t;
      alias : SubScopeName.t Mark.pos;
      var : ScopeVar.t Mark.pos;
    }
      -> < explicitScopes : yes ; .. > glocation
  | ToplevelVar : {
      name : TopdefName.t Mark.pos;
    }
      -> < explicitScopes : yes ; .. > glocation

type ('a, 'm) gexpr = (('a, 'm) naked_gexpr, 'm) marked

and ('a, 'm) naked_gexpr = ('a, 'a, 'm) base_gexpr
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
      than completely defining your recursion manually.

    The first argument of the base_gexpr type caracterises the "deep" type of
    the AST, while the second is the shallow type. They are always equal for
    well-formed AST types, but differentiating them ephemerally allows us to do
    well-typed recursive transformations on the AST that change its type *)

and ('a, 'b, 'm) base_gexpr =
  (* Constructors common to all ASTs *)
  | ELit : lit -> ('a, < .. >, 'm) base_gexpr
  | EApp : {
      f : ('a, 'm) gexpr;
      args : ('a, 'm) gexpr list;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | EOp : {
      op : 'b operator;
      tys : typ list;
    }
      -> ('a, (< .. > as 'b), 'm) base_gexpr
  | EArray : ('a, 'm) gexpr list -> ('a, < .. >, 'm) base_gexpr
  | EVar : ('a, 'm) naked_gexpr Bindlib.var -> ('a, _, 'm) base_gexpr
  | EAbs : {
      binder : (('a, 'a, 'm) base_gexpr, ('a, 'm) gexpr) Bindlib.mbinder;
      tys : typ list;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | EIfThenElse : {
      cond : ('a, 'm) gexpr;
      etrue : ('a, 'm) gexpr;
      efalse : ('a, 'm) gexpr;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | EStruct : {
      name : StructName.t;
      fields : ('a, 'm) gexpr StructField.Map.t;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | EInj : {
      name : EnumName.t;
      e : ('a, 'm) gexpr;
      cons : EnumConstructor.t;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | EMatch : {
      name : EnumName.t;
      e : ('a, 'm) gexpr;
      cases : ('a, 'm) gexpr EnumConstructor.Map.t;
    }
      -> ('a, < .. >, 'm) base_gexpr
  | ETuple : ('a, 'm) gexpr list -> ('a, < .. >, 'm) base_gexpr
  | ETupleAccess : {
      e : ('a, 'm) gexpr;
      index : int;
      size : int;
    }
      -> ('a, < .. >, 'm) base_gexpr
  (* Early stages *)
  | ELocation : 'b glocation -> ('a, (< .. > as 'b), 'm) base_gexpr
  | EScopeCall : {
      scope : ScopeName.t;
      args : ('a, 'm) gexpr ScopeVar.Map.t;
    }
      -> ('a, < explicitScopes : yes ; .. >, 'm) base_gexpr
  | EDStructAccess : {
      name_opt : StructName.t option;
      e : ('a, 'm) gexpr;
      field : Ident.t;
    }
      -> ('a, < syntacticNames : yes ; .. >, 'm) base_gexpr
      (** [desugared] has ambiguous struct fields *)
  | EStructAccess : {
      name : StructName.t;
      e : ('a, 'm) gexpr;
      field : StructField.t;
    }
      -> ('a, < resolvedNames : yes ; .. >, 'm) base_gexpr
      (** Resolved struct/enums, after [desugared] *)
  (* Lambda-like *)
  | EExternal : {
      name : external_ref Mark.pos;
    }
      -> ('a, < explicitScopes : no ; .. >, 't) base_gexpr
  | EAssert : ('a, 'm) gexpr -> ('a, < assertions : yes ; .. >, 'm) base_gexpr
  (* Default terms *)
  | EDefault : {
      excepts : ('a, 'm) gexpr list;
      just : ('a, 'm) gexpr;
      cons : ('a, 'm) gexpr;
    }
      -> ('a, < defaultTerms : yes ; .. >, 'm) base_gexpr
  | EEmptyError : ('a, < defaultTerms : yes ; .. >, 'm) base_gexpr
  | EErrorOnEmpty :
      ('a, 'm) gexpr
      -> ('a, < defaultTerms : yes ; .. >, 'm) base_gexpr
  (* Lambda calculus with exceptions *)
  | ERaise : except -> ('a, < exceptions : yes ; .. >, 'm) base_gexpr
  | ECatch : {
      body : ('a, 'm) gexpr;
      exn : except;
      handler : ('a, 'm) gexpr;
    }
      -> ('a, < exceptions : yes ; .. >, 'm) base_gexpr
  (* Only used during evaluation *)
  | ECustom : {
      obj : Obj.t;
      targs : typ list;
      tret : typ;
    }
      -> ('a, < custom : yes ; .. >, 't) base_gexpr
      (** A function of the given type, as a runtime OCaml object. The specified
          types for arguments and result must be the Catala types corresponding
          to the runtime types of the function. *)

(** Useful for errors and printing, for example *)
type any_expr = AnyExpr : ('a, _) gexpr -> any_expr

type ('a, 'm) boxed_gexpr = (('a, 'm) naked_gexpr Bindlib.box, 'm) marked
(** The annotation is lifted outside of the box for expressions *)

type 'e boxed = ('a, 'm) boxed_gexpr constraint 'e = ('a, 'm) gexpr
(** [('a, 'm) gexpr boxed] is [('a, 'm) boxed_gexpr]. The difference with
    [('a, 'm) gexpr Bindlib.box] is that the annotations is outside of the box,
    and can therefore be accessed without the need to resolve the box *)

type ('e, 'b) binder = (('a, 'm) naked_gexpr, 'b) Bindlib.binder
  constraint 'e = ('a, 'm) gexpr
(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax *)

type ('e, 'b) mbinder = (('a, 'm) naked_gexpr, 'b) Bindlib.mbinder
  constraint 'e = ('a, 'm) gexpr

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
  | Assertion  (** [let () = assert e]*)

type 'e scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ;
  scope_let_expr : 'e;
  scope_let_next : ('e, 'e scope_body_expr) binder;
  (* todo ? Factorise the code_item _list type below and use it here *)
  scope_let_pos : Pos.t;
}
  constraint 'e = ('a any, _) gexpr
(** This type is parametrized by the expression type so it can be reused in
    later intermediate representations. *)

(** A scope let-binding has all the information necessary to make a proper
    let-binding expression, plus an annotation for the kind of the let-binding
    that comes from the compilation of a {!module: Scopelang.Ast} statement. *)
and 'e scope_body_expr =
  | Result of 'e
  | ScopeLet of 'e scope_let
  constraint 'e = ('a any, _) gexpr

type 'e scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : ('e, 'e scope_body_expr) binder;
}
  constraint 'e = ('a any, _) gexpr
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

type scope_info = {
  in_struct_name : StructName.t;
  out_struct_name : StructName.t;
  out_struct_fields : StructField.t ScopeVar.Map.t;
}

type decl_ctx = {
  ctx_enums : enum_ctx;
  ctx_structs : struct_ctx;
  ctx_struct_fields : StructField.t StructName.Map.t Ident.Map.t;
      (** needed for disambiguation (desugared -> scope) *)
  ctx_scopes : scope_info ScopeName.Map.t;
  ctx_topdefs : typ TopdefName.Map.t;
  ctx_modules : decl_ctx ModuleName.Map.t;
}

type 'e program = { decl_ctx : decl_ctx; code_items : 'e code_item_list }
