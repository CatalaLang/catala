(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

type uid = Uid.t

module UidMap = Uid.UidMap

type typ = TBool | TInt | TArrow of typ * typ | TDummy

type literal = Ast.literal

type binop = Ast.binop

type unop = Ast.unop

type op = Binop of binop | Unop of unop

type binding = uid * typ

(*type enum_case = uid*)

type term = untyped_term Pos.marked * typ option

and untyped_term =
  | EVar of uid
  | EFun of binding * term
  | EApp of term * term
  | EIfThenElse of term * term * term
  | ELiteral of literal
  | EOp of op

(* Wrappers *)

type 'expr program = { rules : 'expr UidMap.t }

type program_with_normal_logic = term program

module IntMap = Map.Make (Int)

type justification = term

type consequence = term

type default_term = {
  defaults : (justification * consequence) IntMap.t;
  ordering : (int * int) list;
  nb_defaults : int;
}

let empty_default_term = { defaults = IntMap.empty; ordering = []; nb_defaults = 0 }

let add_default (just : justification) (cons : consequence) (term : default_term) =
  {
    term with
    defaults = IntMap.add term.nb_defaults (just, cons) term.defaults;
    nb_defaults = term.nb_defaults + 1;
  }

type program_with_default_logic = default_term program
