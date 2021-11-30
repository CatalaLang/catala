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

open Utils
module D = Dcalc.Ast
module A = Ast

type ctx = A.expr Pos.marked Bindlib.box D.VarMap.t

let translate_lit (l : D.lit) : A.expr =
  let build lit =
    fst @@ Bindlib.unbox @@ A.make_some (Bindlib.box (Pos.mark Pos.no_pos (A.ELit lit)))
  in
  match l with
  | D.LBool l -> build (A.LBool l)
  | D.LInt i -> build (A.LInt i)
  | D.LRat r -> build (A.LRat r)
  | D.LMoney m -> build (A.LMoney m)
  | D.LUnit -> build A.LUnit
  | D.LDate d -> build (A.LDate d)
  | D.LDuration d -> build (A.LDuration d)
  | D.LEmptyError -> fst @@ Bindlib.unbox @@ A.make_none Pos.no_pos

let ( let+ ) x f = Bindlib.box_apply f x

let ( and+ ) x y = Bindlib.box_pair x y

let thunk_expr (e : A.expr Pos.marked Bindlib.box) (pos : Pos.t) : A.expr Pos.marked Bindlib.box =
  let dummy_var = A.Var.make ("_", pos) in
  A.make_abs [| dummy_var |] e pos [ (D.TAny, pos) ] pos

let rec translate_default (ctx : ctx) (exceptions : D.expr Pos.marked list)
    (just : D.expr Pos.marked) (cons : D.expr Pos.marked) (pos_default : Pos.t) :
    A.expr Pos.marked Bindlib.box =
  let exceptions = List.map (fun except -> translate_expr ctx except) exceptions in
  let exceptions =
    A.make_app
      (A.make_var (A.handle_default, pos_default))
      [
        Bindlib.box_apply
          (fun exceptions -> (A.EArray exceptions, pos_default))
          (Bindlib.box_list exceptions);
        thunk_expr (translate_expr ctx just) pos_default;
        thunk_expr (translate_expr ctx cons) pos_default;
      ]
      pos_default
  in
  exceptions

and translate_typ (t : D.typ Pos.marked) : D.typ Pos.marked =
  (* Hack: If the type is D.TAny, it means for the compiler to not put any type annotation.*)
  Pos.same_pos_as D.TAny t

and translate_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  let same_pos e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | D.EVar v -> D.VarMap.find (Pos.unmark v) ctx
  | D.ETuple (args, s) ->
      let+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
      Pos.same_pos_as (A.ETuple (args, s)) e
  | D.ETupleAccess (e1, i, s, ts) ->
      let e1 = translate_expr ctx e1 in

      (* e1 : [|'a|] array option *)
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in
      let tau = (D.TAny, pos) in

      let new_e new_e1_var =
        let+ e1 = Bindlib.box (A.EVar (new_e1_var, pos)) in
        same_pos @@ A.ETupleAccess ((e1, pos), i, s, ts)
      in

      A.make_letopt_in tau e1 e2
  | D.EInj (e1, i, en, ts) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in

      let tau = (D.TAny, pos) in

      let e2 =
        let+ e1 = Bindlib.box (A.EVar (x, pos)) in
        same_pos @@ A.EInj ((e1, pos), i, en, ts)
      in

      A.make_letopt_in x tau e1 e2
  | D.EMatch (e1, cases, en) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in

      let tau = (D.TAny, pos) in

      let e2 =
        let+ e1 = Bindlib.box (A.EVar (x, pos))
        and+ cases =
          cases
          |> List.map (fun (e', _pos) ->
                 match e' with
                 | D.EAbs ((binder, pos_binder), ts) ->
                     let vars, body = Bindlib.unmbind binder in
                     let ctx, lc_vars =
                       Array.fold_right
                         (fun var (ctx, lc_vars) ->
                           let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
                           let lc_var_expr = A.make_var (lc_var, pos_binder) in
                           (D.VarMap.add var lc_var_expr ctx, lc_var :: lc_vars))
                         vars (ctx, [])
                     in
                     let lc_vars = Array.of_list lc_vars in
                     let new_body = translate_expr ctx body in
                     let+ new_binder = Bindlib.bind_mvar lc_vars new_body in
                     same_pos @@ A.EAbs ((new_binder, pos_binder), List.map translate_typ ts)
                 | _ -> assert false)
          |> Bindlib.box_list
        in

        assert (List.for_all (fun (x, _) -> match x with A.EAbs _ -> true | _ -> false) cases);
        same_pos @@ A.EMatch ((e1, pos), cases, en)
      in

      A.make_letopt_in x tau e1 e2
  | D.EArray es ->
      let+ es = es |> List.map (translate_expr ctx) |> Bindlib.box_list in

      same_pos @@ A.make_some' (same_pos @@ A.EArray es)
  | D.ELit l -> Bindlib.box @@ same_pos @@ translate_lit l
  | D.EOp op -> Bindlib.box @@ same_pos @@ A.make_some' (same_pos @@ A.EOp op)
  | D.EIfThenElse (e1, e2, e3) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in

      (* we can say staticly what is the type of tau here. *)
      let tau = (D.TAny, pos) in

      let e2 =
        let+ e1 = Bindlib.box (A.EVar (x, pos))
        and+ e2 = translate_expr ctx e2
        and+ e3 = translate_expr ctx e3 in
        same_pos @@ A.EIfThenElse ((e1, pos), e2, e3)
      in

      A.make_letopt_in x tau e1 e2
  | D.EAssert e1 ->
      (* don't know the semantic of EAssert. *)
      (* Bindlib.box_apply (fun e1 -> Pos.same_pos_as (A.EAssert e1) e) (translate_expr ctx e1) *)
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in
      let tau = (D.TAny, pos) in

      let e2 = let+ e1 = Bindlib.box (A.EVar (x, pos)) in

               same_pos @@ A.EAssert (e1, pos) in

      A.make_letopt_in x tau e1 e2
  | D.ErrorOnEmpty arg ->
      let pos = Pos.get_position arg in
      let x = A.Var.make ("e1", pos) in
      let arg = translate_expr ctx arg in

      let tau = (D.TAny, pos) in

      let e3 =
        A.make_abs
          (Array.of_list [ x ])
          (Bindlib.box @@ same_pos @@ A.EVar (x, pos))
          pos [ tau ] pos
      and e1 = arg
      and e2 =
        A.make_abs
          (Array.of_list [ x ])
          (Bindlib.box @@ same_pos @@ A.ERaise A.NoValueProvided)
          pos [ tau ] pos
      in

      A.make_matchopt e1 e2 e3
  | D.EApp ((D.EOp op, pos), args) ->
      let+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
      same_pos @@ A.EApp ((A.EOp op, pos), args)
  | D.EApp (e1, args) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let x = A.Var.make ("e1", pos) in

      let tau = (D.TAny, pos) in

      let e2 =
        let+ e1 = Bindlib.box (A.EVar (x, pos))
        and+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
        same_pos @@ A.EApp ((e1, pos), args)
      in

      A.make_letopt_in x tau e1 e2
  | D.EAbs ((binder, pos_binder), ts) ->
      let vars, body = Bindlib.unmbind binder in
      let ctx, lc_vars =
        Array.fold_right
          (fun var (ctx, lc_vars) ->
            let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
            let lc_var_expr = A.make_var (lc_var, pos_binder) in
            (D.VarMap.add var lc_var_expr ctx, lc_var :: lc_vars))
          vars (ctx, [])
      in
      let lc_vars = Array.of_list lc_vars in
      let new_body = translate_expr ctx body in
      let+ new_binder = Bindlib.bind_mvar lc_vars new_body in
      same_pos
      @@ A.make_some' (same_pos @@ A.EAbs ((new_binder, pos_binder), List.map translate_typ ts))
  | D.EDefault (exceptions, just, cons) ->
      translate_default ctx exceptions just cons (Pos.get_position e)

let translate_program (prgm : D.program) : A.program =
  {
    scopes =
      (let acc, _ =
         List.fold_left
           (fun ((acc, ctx) : 'a * A.Var.t D.VarMap.t) (_, n, e) ->
             let new_n = A.Var.make (Bindlib.name_of n, Pos.no_pos) in
             let new_acc =
               ( new_n,
                 Bindlib.unbox
                   (translate_expr (D.VarMap.map (fun v -> A.make_var (v, Pos.no_pos)) ctx) e) )
               :: acc
             in
             let new_ctx = D.VarMap.add n new_n ctx in
             (new_acc, new_ctx))
           ([], D.VarMap.empty) prgm.scopes
       in
       List.rev acc);
    decl_ctx =
      {
        ctx_enums = prgm.decl_ctx.ctx_enums |> D.EnumMap.add A.option_enum A.option_enum_config;
        ctx_structs = prgm.decl_ctx.ctx_structs;
      };
  }
