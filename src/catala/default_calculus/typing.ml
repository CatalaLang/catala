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

(** Typing for the default calculus. Because of the error terms, we perform type inference using the
    classical W algorithm with union-find unification. *)

module Pos = Utils.Pos
module Errors = Utils.Errors
module A = Ast
module Cli = Utils.Cli

type typ =
  | TUnit
  | TInt
  | TBool
  | TArrow of typ Pos.marked UnionFind.elem * typ Pos.marked UnionFind.elem
  | TTuple of typ Pos.marked UnionFind.elem list
  | TAny

let rec format_typ (fmt : Format.formatter) (ty : typ Pos.marked UnionFind.elem) : unit =
  let ty_repr = UnionFind.get (UnionFind.find ty) in
  match Pos.unmark ty_repr with
  | TUnit -> Format.fprintf fmt "unit"
  | TBool -> Format.fprintf fmt "bool"
  | TInt -> Format.fprintf fmt "int"
  | TAny -> Format.fprintf fmt "α"
  | TTuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ") format_typ)
        ts
  | TArrow (t1, t2) -> Format.fprintf fmt "%a → %a" format_typ t1 format_typ t2

let rec unify (t1 : typ Pos.marked UnionFind.elem) (t2 : typ Pos.marked UnionFind.elem) : unit =
  (* Cli.debug_print (Format.asprintf "Unifying %a and %a" format_typ t1 format_typ t2); *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  match (t1_repr, t2_repr) with
  | (TUnit, _), (TUnit, _) | (TBool, _), (TBool, _) | (TInt, _), (TInt, _) -> ()
  | (TArrow (t11, t12), _), (TArrow (t21, t22), _) ->
      unify t11 t21;
      unify t12 t22
  | (TTuple ts1, _), (TTuple ts2, _) -> List.iter2 unify ts1 ts2
  | (TAny, _), (TAny, _) -> ignore (UnionFind.union t1 t2)
  | (TAny, _), t_repr | t_repr, (TAny, _) ->
      let t_union = UnionFind.union t1 t2 in
      ignore (UnionFind.set t_union t_repr)
  | (_, t1_pos), (_, t2_pos) ->
      (* TODO: if we get weird error messages, then it means that we should use the persistent
         version of the union-find data structure. *)
      Errors.raise_multispanned_error
        (Format.asprintf "Error during typechecking, type mismatch: cannot unify %a and %a"
           format_typ t1 format_typ t2)
        [
          (Some (Format.asprintf "Type %a coming from expression:" format_typ t1), t1_pos);
          (Some (Format.asprintf "Type %a coming from expression:" format_typ t2), t2_pos);
        ]

let op_type (op : A.operator Pos.marked) : typ Pos.marked UnionFind.elem =
  let pos = Pos.get_position op in
  let bt = UnionFind.make (TBool, pos) in
  let it = UnionFind.make (TInt, pos) in
  let any = UnionFind.make (TAny, pos) in
  let arr x y = UnionFind.make (TArrow (x, y), pos) in
  match Pos.unmark op with
  | A.Binop (A.And | A.Or) -> arr bt (arr bt bt)
  | A.Binop (A.Add | A.Sub | A.Mult | A.Div) -> arr it (arr it it)
  | A.Binop (A.Lt | A.Lte | A.Gt | A.Gte) -> arr it (arr it bt)
  | A.Binop (A.Eq | A.Neq) -> arr any (arr any bt)
  | A.Unop A.Minus -> arr it it
  | A.Unop A.Not -> arr bt bt

let rec ast_to_typ (ty : A.typ) : typ =
  match ty with
  | A.TUnit -> TUnit
  | A.TBool -> TBool
  | A.TInt -> TInt
  | A.TArrow (t1, t2) ->
      TArrow
        ( UnionFind.make (Pos.map_under_mark ast_to_typ t1),
          UnionFind.make (Pos.map_under_mark ast_to_typ t2) )
  | A.TTuple ts -> TTuple (List.map (fun t -> UnionFind.make (Pos.map_under_mark ast_to_typ t)) ts)

let rec typ_to_ast (ty : typ Pos.marked UnionFind.elem) : A.typ Pos.marked =
  Pos.map_under_mark
    (fun ty ->
      match ty with
      | TUnit -> A.TUnit
      | TBool -> A.TBool
      | TInt -> A.TInt
      | TTuple ts -> A.TTuple (List.map typ_to_ast ts)
      | TArrow (t1, t2) -> A.TArrow (typ_to_ast t1, typ_to_ast t2)
      | TAny -> A.TUnit)
    (UnionFind.get (UnionFind.find ty))

type env = typ Pos.marked A.VarMap.t

let rec typecheck_expr_bottom_up (env : env) (e : A.expr Pos.marked) : typ Pos.marked UnionFind.elem
    =
  (* Cli.debug_print (Format.asprintf "Up begin: %a" Print.format_expr e); *)
  let out =
    match Pos.unmark e with
    | EVar v -> (
        match A.VarMap.find_opt (Pos.unmark v) env with
        | Some t -> UnionFind.make t
        | None ->
            Errors.raise_spanned_error "Variable not found in the current context"
              (Pos.get_position e) )
    | ELit (LBool _) -> UnionFind.make (Pos.same_pos_as TBool e)
    | ELit (LInt _) -> UnionFind.make (Pos.same_pos_as TInt e)
    | ELit LUnit -> UnionFind.make (Pos.same_pos_as TUnit e)
    | ELit LEmptyError -> UnionFind.make (Pos.same_pos_as TAny e)
    | ETuple es ->
        let ts = List.map (typecheck_expr_bottom_up env) es in
        UnionFind.make (Pos.same_pos_as (TTuple ts) e)
    | ETupleAccess (e1, n) -> (
        let t1 = typecheck_expr_bottom_up env e1 in
        match Pos.unmark (UnionFind.get (UnionFind.find t1)) with
        | TTuple ts -> (
            match List.nth_opt ts n with
            | Some t' -> t'
            | None ->
                Errors.raise_spanned_error
                  (Format.asprintf
                     "expression should have a tuple type with at least %d elements but only has %d"
                     n (List.length ts))
                  (Pos.get_position e1) )
        | _ ->
            Errors.raise_spanned_error
              (Format.asprintf "exprected a tuple, got a %a" format_typ t1)
              (Pos.get_position e1) )
    | EAbs (pos_binder, binder, taus) ->
        let xs, body = Bindlib.unmbind binder in
        if Array.length xs = List.length taus then
          let xstaus = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
          let env =
            List.fold_left
              (fun env (x, tau) -> A.VarMap.add x (ast_to_typ (Pos.unmark tau), pos_binder) env)
              env xstaus
          in
          List.fold_right
            (fun t_arg (acc : typ Pos.marked UnionFind.elem) ->
              UnionFind.make
                (TArrow (UnionFind.make (Pos.map_under_mark ast_to_typ t_arg), acc), pos_binder))
            taus
            (typecheck_expr_bottom_up env body)
        else
          Errors.raise_spanned_error
            (Format.asprintf "function has %d variables but was supplied %d types" (Array.length xs)
               (List.length taus))
            pos_binder
    | EApp (e1, args) ->
        let t_args = List.map (typecheck_expr_bottom_up env) args in
        let t_ret = UnionFind.make (Pos.same_pos_as TAny e) in
        let t_app =
          List.fold_right
            (fun t_arg acc -> UnionFind.make (Pos.same_pos_as (TArrow (t_arg, acc)) e))
            t_args t_ret
        in
        typecheck_expr_top_down env e1 t_app;
        t_ret
    | EOp op -> op_type (Pos.same_pos_as op e)
    | EDefault (just, cons, subs) ->
        typecheck_expr_top_down env just (UnionFind.make (Pos.same_pos_as TBool just));
        let tcons = typecheck_expr_bottom_up env cons in
        List.iter (fun sub -> typecheck_expr_top_down env sub tcons) subs;
        tcons
    | EIfThenElse (cond, et, ef) ->
        typecheck_expr_top_down env cond (UnionFind.make (Pos.same_pos_as TBool cond));
        let tt = typecheck_expr_bottom_up env et in
        typecheck_expr_top_down env ef tt;
        tt
  in
  (* Cli.debug_print (Format.asprintf "Up result: %a | %a" Print.format_expr e format_typ out); *)
  out

and typecheck_expr_top_down (env : env) (e : A.expr Pos.marked)
    (tau : typ Pos.marked UnionFind.elem) : unit =
  (* Cli.debug_print (Format.asprintf "Down: %a | %a" Print.format_expr e format_typ tau); *)
  match Pos.unmark e with
  | EVar v -> (
      match A.VarMap.find_opt (Pos.unmark v) env with
      | Some tau' -> ignore (unify tau (UnionFind.make tau'))
      | None ->
          Errors.raise_spanned_error "Variable not found in the current context"
            (Pos.get_position e) )
  | ELit (LBool _) -> unify tau (UnionFind.make (Pos.same_pos_as TBool e))
  | ELit (LInt _) -> unify tau (UnionFind.make (Pos.same_pos_as TInt e))
  | ELit LUnit -> unify tau (UnionFind.make (Pos.same_pos_as TUnit e))
  | ELit LEmptyError -> unify tau (UnionFind.make (Pos.same_pos_as TAny e))
  | ETuple es -> (
      let tau' = UnionFind.get (UnionFind.find tau) in
      match Pos.unmark tau' with
      | TTuple ts -> List.iter2 (typecheck_expr_top_down env) es ts
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf "exprected %a, got a tuple" format_typ tau)
            (Pos.get_position e) )
  | ETupleAccess (e1, n) -> (
      let t1 = typecheck_expr_bottom_up env e1 in
      match Pos.unmark (UnionFind.get (UnionFind.find t1)) with
      | TTuple t1s -> (
          match List.nth_opt t1s n with
          | Some t1n -> unify t1n tau
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf
                   "expression should have a tuple type with at least %d elements but only has %d" n
                   (List.length t1s))
                (Pos.get_position e1) )
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf "exprected a tuple , got %a" format_typ tau)
            (Pos.get_position e) )
  | EAbs (pos_binder, binder, t_args) ->
      let xs, body = Bindlib.unmbind binder in
      if Array.length xs = List.length t_args then
        let xstaus = List.map2 (fun x t_arg -> (x, t_arg)) (Array.to_list xs) t_args in
        let env =
          List.fold_left
            (fun env (x, t_arg) -> A.VarMap.add x (ast_to_typ (Pos.unmark t_arg), pos_binder) env)
            env xstaus
        in
        let t_out = typecheck_expr_bottom_up env body in
        let t_func =
          List.fold_right
            (fun t_arg acc ->
              UnionFind.make
                (Pos.same_pos_as
                   (TArrow (UnionFind.make (ast_to_typ (Pos.unmark t_arg), pos_binder), acc))
                   e))
            t_args t_out
        in
        unify t_func tau
      else
        Errors.raise_spanned_error
          (Format.asprintf "function has %d variables but was supplied %d types" (Array.length xs)
             (List.length t_args))
          pos_binder
  | EApp (e1, args) ->
      let t_args = List.map (typecheck_expr_bottom_up env) args in
      let te1 = typecheck_expr_bottom_up env e1 in
      let t_func =
        List.fold_right
          (fun t_arg acc -> UnionFind.make (Pos.same_pos_as (TArrow (t_arg, acc)) e))
          t_args tau
      in
      unify te1 t_func
  | EOp op ->
      let op_typ = op_type (Pos.same_pos_as op e) in
      unify op_typ tau
  | EDefault (just, cons, subs) ->
      typecheck_expr_top_down env just (UnionFind.make (Pos.same_pos_as TBool just));
      typecheck_expr_top_down env cons tau;
      List.iter (fun sub -> typecheck_expr_top_down env sub tau) subs
  | EIfThenElse (cond, et, ef) ->
      typecheck_expr_top_down env cond (UnionFind.make (Pos.same_pos_as TBool cond));
      typecheck_expr_top_down env et tau;
      typecheck_expr_top_down env ef tau

let infer_type (e : A.expr Pos.marked) : A.typ Pos.marked =
  let ty = typecheck_expr_bottom_up A.VarMap.empty e in
  typ_to_ast ty

let check_type (e : A.expr Pos.marked) (tau : A.typ Pos.marked) =
  typecheck_expr_top_down A.VarMap.empty e (UnionFind.make (Pos.map_under_mark ast_to_typ tau))
