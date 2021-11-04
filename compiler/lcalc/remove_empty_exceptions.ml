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
module T = Dcalc.Ast
module D = Ast
module A = Ast

type ctx = A.expr Pos.marked Bindlib.box D.VarMap.t

let translate_lit (l : D.lit) : A.expr =
  match l with
  | D.LBool l -> A.ELit (A.LBool l)
  | D.LInt i -> A.ELit (A.LInt i)
  | D.LRat r -> A.ELit (A.LRat r)
  | D.LMoney m -> A.ELit (A.LMoney m)
  | D.LUnit -> A.ELit A.LUnit
  | D.LDate d -> A.ELit (A.LDate d)
  | D.LDuration d -> A.ELit (A.LDuration d)


(* let thunk_expr (e : A.expr Pos.marked Bindlib.box) (pos : Pos.t) : A.expr Pos.marked Bindlib.box =
  let dummy_var = A.Var.make ("_", pos) in
  A.make_abs [| dummy_var |] e pos [ (T.TAny, pos) ] pos *)

let (let+) x f = Bindlib.box_apply f x

let (and+) x y = Bindlib.box_pair x y


let rec translate_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  let same_pos x = Pos.same_pos_as x e in
  match Pos.unmark e with
  | D.ETuple (args, s) ->
    let+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
    same_pos @@ A.ETuple (args, s)
  | D.ETupleAccess (e1, i, s, ts) ->
    let+ e1 = translate_expr ctx e1 in
    same_pos @@ A.ETupleAccess (e1, i, s, ts)
  | D.EInj (e1, i, en, ts) ->
    let+ e1 = translate_expr ctx e1 in
    same_pos @@ A.EInj (e1, i, en, ts)
  | D.EMatch (e1, cases, en) ->
    let+ e1 = translate_expr ctx e1
    and+ cases = Bindlib.box_list (List.map (translate_expr ctx) cases) in
    same_pos @@ A.EMatch (e1, cases, en)
  | D.EArray es ->
    let+ es = Bindlib.box_list (List.map (translate_expr ctx) es) in 
    same_pos @@ A.ESome (same_pos @@ A.EArray es)
  | D.EOp op ->
    Bindlib.box @@ same_pos @@  A.ESome (same_pos @@ A.EOp op)
  | D.EAssert e1 ->
    let+ e1 = translate_expr ctx e1 in
    same_pos (A.EAssert e1)
  | D.EApp (e1, args) ->
    (* e1 args* *)
    (* match e1 with | None -> None | Some e1 -> e1 args* *)
    let+ e1 = translate_expr ctx e1
    and+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
    same_pos @@ A.EApp (e1, args)
  | D.EAbs ((binder, pos_binder), ts) ->
      (* fun vars: tys -> body *)
      (* Some (fun vars: [|tys|] -> body) *)
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
      same_pos @@ A.EAbs ((new_binder, pos_binder), ts)

  | D.EIfThenElse (e1, e2, e3) ->
    let+ e1 = translate_expr ctx e1
    and+ e2 = translate_expr ctx e2
    and+ e3 = translate_expr ctx e3 in
    let x = A.Var.make ("e1", snd e1 ) in
    (* same_pos @@
      A.make_let_in x (T.TOption T.TBool)
        (Pos.same_pos_as e1 e1)
        (same_pos @@ A.EMatchopt(A.make_var x, A.ENone, A.EAbs A.EIfThenElse (A.make_var x, e2, e3))) *)
    assert false
  | D.ECatch (e1, exc, e2) ->
    begin match exc with
    | EmptyError ->
      begin
        (* let bla = e1 in matchopt bla with None -> e2 | Some _ -> bla *)
        (* (fun bla -> matchopt bla with None -> e2 | Some _ -> bla) e1 *)

        let pos = snd e1 in
        let x_var = (A.Var.make ("e1", pos), pos) in

        let e1 = translate_expr ctx e1 in
        let e2 = translate_expr ctx e2 in
        let x = A.make_var x_var in
        (* todo: /!\ exponential cost. Use let in instead *)

        let body = let+ e2 = e2 and+ x = x in
          same_pos @@ A.EMatchopt(x, e2, x) in

        let tau = assert false in

        A.make_let_in (fst x_var) tau e1 body
      end
    | _ ->
      let+ e1 = translate_expr ctx e1 
      and+ e2 = translate_expr ctx e2 in
      same_pos @@ A.ECatch (e1, exc, e2)
    end
  | D.ERaise exc ->
    Bindlib.box begin match exc with
    | EmptyError -> same_pos @@ A.ENone
    | _ -> same_pos @@ A.ERaise exc
  end
  | D.EVar v -> D.VarMap.find (Pos.unmark v) ctx
  | D.EMatchopt (e1, e2, e3) ->
    let+ e1 = translate_expr ctx e1
    and+ e2 = translate_expr ctx e2
    and+ e3 = translate_expr ctx e3 in
    same_pos @@ A.EMatchopt (e1, e2, e3)
  | D.ELit l ->
    Bindlib.box @@ same_pos  @@ A.ESome (same_pos (translate_lit l))
  | D.ENone ->
    Bindlib.box @@ same_pos @@ A.ESome (same_pos @@ A.ENone)
  | D.ESome e1 ->
    let+ e1 = translate_expr ctx e1 in
    same_pos @@ A.ESome (same_pos @@ A.ESome e1)


let translate_program (prgm : D.program) : A.program =
  {
    scopes = prgm.scopes
      |> ListLabels.fold_left ~init:([], D.VarMap.empty)
        ~f:begin fun (acc, ctx) (n, e) ->
          let new_n = n in
          let new_acc =
            ( new_n,
              Bindlib.unbox (translate_expr (A.VarMap.map (fun v -> D.make_var (v, Pos.no_pos)) ctx) e)
            ) :: acc in
          let new_ctx = A.VarMap.add n new_n ctx in
          (new_acc, new_ctx)
        end
      |> fst
      |> List.rev
    ;
    decl_ctx = prgm.decl_ctx;
  }
