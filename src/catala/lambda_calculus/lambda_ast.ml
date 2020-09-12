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

(* TDummy means the term is not typed *)
type typ = TBool | TInt | TArrow of typ * typ | TDummy

type literal = Catala_ast.literal

type binop = Catala_ast.binop

type unop = Catala_ast.unop

type op = Binop of binop | Unop of unop

type binding = Uid.Var.t * typ

(*type enum_case = uid*)

type term = untyped_term Pos.marked * typ

and untyped_term =
  | EVar of Uid.Var.t
  | EFun of binding list * term
  | EApp of term * term list
  | EIfThenElse of term * term * term
  | EInt of int
  | EBool of bool
  | EDec of int * int
  | EOp of op
  | EDefault of default_term

(* (x,y) in ordering means that default x has precedence over default y : if both are true then x
   would be choser over y *)
and default_term = { defaults : (term * term) list; ordering : (int * int) list }

let untype (((term, _), _) : term) : untyped_term = term

let get_pos (((_, pos), _) : term) : Pos.t = pos

let get_typ ((_, typ) : term) : typ = typ

let map_untype (f : untyped_term -> untyped_term) (((term, pos), typ) : term) : term =
  ((f term, pos), typ)

let map_untype2 (f : untyped_term -> untyped_term -> untyped_term) (((t1, pos), typ) : term)
    (((t2, _), _) : term) : term =
  ((f t1 t2, pos), typ)

let empty_default_term : default_term = { defaults = []; ordering = [] }

let add_default (just : term) (cons : term) (term : default_term) =
  { term with defaults = term.defaults @ [ (just, cons) ] }

(** Merge two defalts terms, taking into account that one has higher precedence than the other *)
let merge_default_terms (lo_term : default_term) (hi_term : default_term) : default_term =
  let n = List.length lo_term.defaults in
  let n' = List.length hi_term.defaults in
  let defaults = hi_term.defaults @ lo_term.defaults in
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
  { defaults; ordering = prec @ prec' }

(** Returns the free variables of a term *)
let rec term_fv (term : term) : Uid.VarSet.t =
  match untype term with
  | EVar uid -> Uid.VarSet.singleton uid
  | EFun (bindings, body) ->
      let body_fv = term_fv body in
      let bindings = bindings |> List.map (fun (x, _) -> x) |> Uid.VarSet.of_list in
      Uid.VarSet.diff body_fv bindings
  | EApp (f, args) ->
      List.fold_left (fun fv arg -> Uid.VarSet.union fv (term_fv arg)) (term_fv f) args
  | EIfThenElse (t_if, t_then, t_else) ->
      Uid.VarSet.union (term_fv t_if) (Uid.VarSet.union (term_fv t_then) (term_fv t_else))
  | EDefault default -> default_term_fv default
  | _ -> Uid.VarSet.empty

and default_term_fv (term : default_term) : Uid.VarSet.t =
  List.fold_left
    (fun acc (cond, body) ->
      let fv = Uid.VarSet.union (term_fv cond) (term_fv body) in
      Uid.VarSet.union fv acc)
    Uid.VarSet.empty term.defaults
