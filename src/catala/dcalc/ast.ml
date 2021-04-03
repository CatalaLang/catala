(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

[@@@ocaml.warning "-7"]

open Utils

module ScopeName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module StructName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module StructFieldName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructMap : Map.S with type key = StructName.t = Map.Make (StructName)

module EnumName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumMap : Map.S with type key = EnumName.t = Map.Make (EnumName)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration
[@@deriving
  visitors { variety = "map"; name = "typ_lit_map"; nude = true },
    visitors { variety = "iter"; name = "typ_lit_iter"; nude = true }]

type struct_name = (StructName.t[@opaque])
[@@deriving
  visitors { variety = "map"; name = "struct_name_map"; nude = true },
    visitors { variety = "iter"; name = "struct_name_iter"; nude = true }]

type enum_name = (EnumName.t[@opaque])
[@@deriving
  visitors { variety = "map"; name = "enum_name_map"; nude = true },
    visitors { variety = "iter"; name = "enum_name_iter"; nude = true }]

type typ =
  | TLit of typ_lit
  | TTuple of typ Pos.marked list * struct_name option
  | TEnum of typ Pos.marked list * enum_name
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ Pos.marked
  | TAny
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map"; "typ_lit_map"; "struct_name_map"; "enum_name_map" ];
      name = "typ_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter"; "typ_lit_iter"; "struct_name_iter"; "enum_name_iter" ];
        name = "typ_iter";
      }]

type date = (Runtime.date[@opaque])
[@@deriving
  visitors { variety = "map"; name = "date_map"; nude = true },
    visitors { variety = "iter"; name = "date_iter"; nude = true }]

type duration = (Runtime.duration[@opaque])
[@@deriving
  visitors { variety = "map"; name = "duration_map"; nude = true },
    visitors { variety = "iter"; name = "duration_iter"; nude = true }]

type integer = (Runtime.integer[@opaque])
[@@deriving
  visitors { variety = "map"; name = "integer_map"; nude = true },
    visitors { variety = "iter"; name = "integer_iter"; nude = true }]

type decimal = (Runtime.decimal[@opaque])
[@@deriving
  visitors { variety = "map"; name = "decimal_map"; nude = true },
    visitors { variety = "iter"; name = "decimal_iter"; nude = true }]

type money = (Runtime.money[@opaque])
[@@deriving
  visitors { variety = "map"; name = "money_map"; nude = true },
    visitors { variety = "iter"; name = "money_iter"; nude = true }]

type lit =
  | LBool of bool
  | LEmptyError
  | LInt of integer
  | LRat of decimal
  | LMoney of money
  | LUnit
  | LDate of date
  | LDuration of duration
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "decimal_map"; "date_map"; "duration_map"; "integer_map"; "money_map" ];
      name = "lit_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "decimal_iter"; "date_iter"; "duration_iter"; "integer_iter"; "money_iter" ];
        name = "lit_iter";
      }]

type op_kind = KInt | KRat | KMoney | KDate | KDuration
[@@deriving
  visitors { variety = "map"; name = "op_kind_map"; nude = true },
    visitors { variety = "iter"; name = "op_kind_iter"; nude = true }]

type ternop = Fold
[@@deriving
  visitors { variety = "map"; name = "ternop_map"; nude = true },
    visitors { variety = "iter"; name = "ternop_iter"; nude = true }]

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
  | Filter
[@@deriving
  visitors { variety = "map"; ancestors = [ "op_kind_map" ]; name = "binop_map"; nude = true },
    visitors { variety = "iter"; ancestors = [ "op_kind_iter" ]; name = "binop_iter"; nude = true }]

type log_entry = VarDef | BeginCall | EndCall | PosRecordIfTrueBool
[@@deriving
  visitors { variety = "map"; name = "log_entry_map"; nude = true },
    visitors { variety = "iter"; name = "log_entry_iter"; nude = true }]

type unop =
  | Not
  | Minus of op_kind
  | ErrorOnEmpty
  | Log of log_entry * (Utils.Uid.MarkedString.info list[@opaque])
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear
[@@deriving
  visitors { variety = "map"; ancestors = [ "op_kind_map"; "log_entry_map" ]; name = "unop_map" },
    visitors
      { variety = "iter"; ancestors = [ "op_kind_iter"; "log_entry_iter" ]; name = "unop_iter" }]

type operator = Ternop of ternop | Binop of binop | Unop of unop
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "ternop_map"; "binop_map"; "unop_map" ];
      name = "operator_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "ternop_iter"; "binop_iter"; "unop_iter" ];
        name = "operator_iter";
      }]

type expr =
  | EVar of (expr Bindlib.var[@opaque]) Pos.marked
  | ETuple of expr Pos.marked list * struct_name option
  | ETupleAccess of expr Pos.marked * int * struct_name option * typ Pos.marked list
  | EInj of expr Pos.marked * int * enum_name * typ Pos.marked list
  | EMatch of expr Pos.marked * expr Pos.marked list * enum_name
  | EArray of expr Pos.marked list
  | ELit of lit
  | EAbs of ((expr, expr Pos.marked) Bindlib.mbinder[@opaque]) Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "Pos.marked_map"; "operator_map"; "struct_name_map"; "enum_name_map"; "typ_map"; "lit_map";
        ];
      name = "expr_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "Pos.marked_iter";
            "operator_iter";
            "struct_name_iter";
            "enum_name_iter";
            "typ_iter";
            "lit_iter";
          ];
        name = "expr_iter";
      }]

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t

type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t

type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Pos.get_position s))
      (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)

type vars = expr Bindlib.mvar

let make_var ((x, pos) : Var.t Pos.marked) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun x -> (x, pos)) (Bindlib.box_var x)

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t)
    (taus : typ Pos.marked list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs ((b, pos_binder), taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in (x : Var.t) (tau : typ Pos.marked) (e1 : expr Pos.marked Bindlib.box)
    (e2 : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2
    (fun e u -> (EApp (e, u), Pos.get_position (Bindlib.unbox e2)))
    (make_abs
       (Array.of_list [ x ])
       e2
       (Pos.get_position (Bindlib.unbox e2))
       [ tau ]
       (Pos.get_position (Bindlib.unbox e2)))
    (Bindlib.box_list [ e1 ])

let make_multiple_let_in (xs : Var.t array) (taus : typ Pos.marked list)
    (e1 : expr Pos.marked list Bindlib.box) (e2 : expr Pos.marked Bindlib.box) :
    expr Pos.marked Bindlib.box =
  Bindlib.box_apply2
    (fun e u -> (EApp (e, u), Pos.get_position (Bindlib.unbox e2)))
    (make_abs xs e2
       (Pos.get_position (Bindlib.unbox e2))
       taus
       (Pos.get_position (Bindlib.unbox e2)))
    e1

type binder = (expr, expr Pos.marked) Bindlib.binder

type program = { decl_ctx : decl_ctx; scopes : (ScopeName.t * Var.t * expr Pos.marked) list }
