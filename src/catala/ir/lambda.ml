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
module UidSet = Uid.UidSet

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
  | EFun of binding list * term
  | EApp of term * term list
  | EIfThenElse of term * term * term
  | EInt of int
  | EBool of bool
  | EDec of int * int
  | EOp of op

let untype (((term, _), _) : term) : untyped_term = term

let print_literal (l : literal) : string =
  match l with
  | Number (num, _) -> (
      match Pos.unmark num with
      | Int i -> Printf.sprintf "%n" i
      | Dec (i, f) -> Printf.sprintf "%n.%n" i f )
  | Bool b -> if b then "true" else "false"
  | MoneyAmount { money_amount_units; money_amount_cents } ->
      Printf.sprintf "%n.%n" money_amount_units money_amount_cents
  | Date _ -> "[date]"

let print_op (op : op) : string =
  match op with
  | Binop binop -> (
      match binop with
      | And -> "and"
      | Or -> "or"
      | Add -> "+"
      | Sub -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Lt -> "<"
      | Lte -> "<="
      | Gt -> ">"
      | Gte -> ">="
      | Eq -> "="
      | Neq -> "!=" )
  | Unop Not -> "not"
  | Unop Minus -> "-"

let rec print_term (((t, _), _) : term) : string =
  match t with
  | EVar uid -> Printf.sprintf "var(%n)" uid
  | EFun (binders, body) ->
      let sbody = print_term body in
      Printf.sprintf "fun %s -> %s"
        (binders |> List.map (fun (x, _) -> Printf.sprintf "%d" x) |> String.concat " ")
        sbody
  | EApp (f, args) ->
      Printf.sprintf "(%s) [%s]" (print_term f) (args |> List.map print_term |> String.concat ";")
  | EIfThenElse (tif, tthen, telse) ->
      Printf.sprintf "IF %s THEN %s ELSE %s" (print_term tif) (print_term tthen) (print_term telse)
  | EInt i -> Printf.sprintf "%d" i
  | EBool b -> if b then "true" else "false"
  | EDec (i, f) -> Printf.sprintf "%d.%d" i f
  | EOp op -> print_op op

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

let print_default_term (term : default_term) : unit =
  IntMap.iter
    (fun _ (cond, body) -> Printf.printf "\t%s => %s\t\n" (print_term cond) (print_term body))
    term.defaults

let empty_default_term = { defaults = IntMap.empty; ordering = []; nb_defaults = 0 }

let add_default (just : justification) (cons : consequence) (term : default_term) =
  {
    term with
    defaults = IntMap.add term.nb_defaults (just, cons) term.defaults;
    nb_defaults = term.nb_defaults + 1;
  }

(** Merge two defalts terms, taking into account that one has higher precedence than the other *)
let merge_default_terms (lo_term : default_term) (hi_term : default_term) : default_term =
  let n = lo_term.nb_defaults in
  let n' = hi_term.nb_defaults in
  let defaults =
    IntMap.fold (fun k default -> IntMap.add (n + k) default) hi_term.defaults lo_term.defaults
  in
  let rec add_hi_prec = function
    | [] -> lo_term.ordering
    | (k, k') :: xs -> (n + k, n + k') :: add_hi_prec xs
  in
  let prec = add_hi_prec hi_term.ordering in
  let gen_prec lo hi =
    List.fold_left
      (fun acc x_lo ->
        let sub_list = List.fold_left (fun acc' x_hi -> (x_hi, x_lo) :: acc') [] hi in
        sub_list :: acc)
      [] lo
    |> List.flatten
  in
  let rec gen_list i j acc = if i = j then acc else gen_list (i + 1) j (i :: acc) in
  let gen_list i j = gen_list i j [] in
  let prec' = gen_prec (gen_list 0 n) (gen_list n (n + n')) in
  { defaults; ordering = prec @ prec'; nb_defaults = n + n' }

type program_with_default_logic = default_term program

let rec term_free_vars (term : term) : UidSet.t =
  match untype term with
  | EVar uid -> UidSet.singleton uid
  | EFun (bindings, body) ->
      let body_fv = term_free_vars body in
      let bindings = bindings |> List.map (fun (x, _) -> x) |> UidSet.of_list in
      UidSet.diff body_fv bindings
  | EApp (f, args) ->
      List.fold_left (fun fv arg -> UidSet.union fv (term_free_vars arg)) (term_free_vars f) args
  | EIfThenElse (t_if, t_then, t_else) ->
      UidSet.union (term_free_vars t_if)
        (UidSet.union (term_free_vars t_then) (term_free_vars t_else))
  | _ -> UidSet.empty

let default_term_fv (term : default_term) : UidSet.t =
  IntMap.fold
    (fun _ (cond, body) ->
      let fv = UidSet.union (term_free_vars cond) (term_free_vars body) in
      UidSet.union fv)
    term.defaults UidSet.empty
