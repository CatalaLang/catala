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

type ctx = {
  env: A.expr Pos.marked Bindlib.box D.VarMap.t;
  env_pure: bool D.VarMap.t; (* true if it is pure (without opt) *)
}

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

and translate_binder (ctx: ctx) ((binder, pos_binder): (D.expr, D.expr Pos.marked) Bindlib.mbinder Pos.marked): (A.expr, A.expr Pos.marked) Bindlib.mbinder Pos.marked Bindlib.box =

  let vars, body = Bindlib.unmbind binder in
  let ctx, lc_vars =
    Array.fold_right
      (fun var (ctx, lc_vars) ->
        let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
        let lc_var_expr = A.make_var (lc_var, pos_binder) in
        let new_ctx = {
          env=D.VarMap.add var lc_var_expr ctx.env;
          env_pure=D.VarMap.add var false ctx.env_pure;
        } in
        (new_ctx, lc_var :: lc_vars))
      vars (ctx, [])
  in
  let lc_vars = Array.of_list lc_vars in
  let new_body = translate_expr ctx body in
  let+ binder = Bindlib.bind_mvar lc_vars new_body in
  (binder, pos_binder)

and translate_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  let same_pos e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | D.EVar v ->
    (if D.VarMap.find (Pos.unmark v) ctx.env_pure then
      A.make_some
    else
      Fun.id)
    (D.VarMap.find (Pos.unmark v) ctx.env)
  | D.ETuple (args, s) ->
      let+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
      Pos.same_pos_as (A.ETuple (args, s)) e
  | D.ETupleAccess (e1, i, s, ts) ->
      let e1 = translate_expr ctx e1 in

      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ new_e1 = new_e1 in
        same_pos @@ A.ETupleAccess (new_e1, i, s, ts)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EInj (e1, i, en, ts) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ new_e1 = new_e1 in
        same_pos @@ A.EInj (new_e1, i, en, ts)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EMatch (e1, cases, en) ->
    let e1 = translate_expr ctx e1 in
    let pos = Pos.get_position (Bindlib.unbox e1) in
    let tau = (D.TAny, pos) in

    let new_e new_e1 =
      let+ new_e1 = new_e1
      and+ cases =
        cases
        |> List.map (fun (e', _pos) ->
            match e' with
            | D.EAbs (binder, ts) ->
              let+ new_binder = translate_binder ctx binder in
                same_pos @@ A.EAbs (new_binder, List.map translate_typ ts)
            | _ -> Errors.raise_spanned_error "Internal error: an error occured during the translation of a amtch." (Pos.get_position e))
        |> Bindlib.box_list
      in
        if (List.for_all (fun (x, _) -> match x with A.EAbs _ -> true | _ -> false) cases) then
          Errors.raise_spanned_error "Internal error: an error occured during the translation of a match." (Pos.get_position e);

        same_pos @@ A.EMatch (new_e1, cases, en)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EArray es ->
      let+ es = es |> List.map (translate_expr ctx) |> Bindlib.box_list in
      same_pos @@ A.make_some' (same_pos @@ A.EArray es)

  | D.ELit l -> Bindlib.box @@ same_pos @@ translate_lit l
  | D.EOp _op ->
    Errors.raise_spanned_error "Internal error: partial application of generic operator are not yet supported when using --avoid_exception." (Pos.get_position e)
  | D.EApp((D.EOp op, pos), args) ->
    begin
      let xs = List.mapi (fun i arg -> A.Var.make (Printf.sprintf "x_%d" i, Pos.get_position arg)) args in

      let dummy = A.Var.make ("unit", pos) in

      let e' final = args
      |> List.map (translate_expr ctx)
      |> List.combine xs
      |> List.fold_left (fun acc (x, arg) ->
        A.make_matchopt
          arg
          (A.make_abs (Array.of_list [dummy]) (A.make_none pos) (pos) [D.TLit D.TUnit, pos] pos)
          (A.make_abs (Array.of_list [x]) acc pos [D.TAny, pos] pos)
        ) final
      in

      let new_e =
        let+ args_var = xs
          |> List.map (fun x -> Bindlib.box_var x)
          |> Bindlib.box_list
        in

        let args_var = args_var
          |> List.combine args
          |> List.map (fun (arg, x) -> Pos.same_pos_as x arg)
        in
        same_pos @@ A.make_some' @@ same_pos @@ A.EApp ((A.EOp op, pos), args_var)
      in

      e' new_e
    end

  | D.EIfThenElse (e1, e2, e3) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in



      let new_e new_e1 =
        let+ e1_new = new_e1
        and+ e2 = translate_expr ctx e2
        and+ e3 = translate_expr ctx e3 in
        same_pos @@ A.EIfThenElse (e1_new, e2, e3)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EAssert e1 ->
      (* don't know the semantic of EAssert. *)
      (* Bindlib.box_apply (fun e1 -> Pos.same_pos_as (A.EAssert e1) e) (translate_expr ctx e1) *)
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ e1_new = new_e1 in
        same_pos @@ A.EAssert e1_new
      in

      A.make_bindopt pos tau e1 new_e

      | D.EApp (e1, args) ->
    let e1 = translate_expr ctx e1 in
    let pos = Pos.get_position (Bindlib.unbox e1) in
    let tau = (D.TAny, pos) in

    let new_e new_e1 =
      let+ new_e1 = new_e1
      and+ args = args
        |> List.map (translate_expr ctx)
        |> Bindlib.box_list
      in
      same_pos @@ A.EApp (new_e1, args)
    in

    A.make_bindopt pos tau e1 new_e

  | D.EAbs (binder, ts) ->
      let+ new_binder = translate_binder ctx binder in
      same_pos
      @@ A.make_some' (same_pos @@ A.EAbs (new_binder, List.map translate_typ ts))
  | D.EDefault (exceptions, just, cons) ->
      translate_default ctx exceptions just cons (Pos.get_position e)

  | D.ErrorOnEmpty _ ->

    Errors.raise_spanned_error "Internal error: Error on empty found in incorrect place when compiling using the --avoid_exception option." (Pos.get_position e)


let translate_scope_let (ctx: ctx) (s: D.scope_let) : A.expr Bindlib.box =

  let {
    scope_let_var;
    scope_let_kind;
    scope_let_typ;
    scope_let_expr;
  } = s in

  (* I need to match on the expression. *)
  let expr' : A.expr Bindlib.box =
    let+ expr = scope_let_expr in
    match scope_let_kind, scope_let_typ, expr with
    | ScopeVarDefinition, typ, D.ErrorOnEmpty arg ->
      (* ~> match [| arg |] with None -> raise NoValueProvided | Some x -> x *)
      let pos = Pos.get_position arg in
      let x = A.Var.make ("result", pos) in
      let arg = translate_expr ctx arg in

      let tau = (D.TAny, pos) in

      let e3 =
        A.make_abs
          (Array.of_list [ x ])
          (let+ v = Bindlib.box_var x in (v, pos))
          pos [ tau ] pos
      and e1 = arg
      and e2 =
        A.make_abs
          (Array.of_list [ x ])
          (Bindlib.box @@ same_pos @@ A.ERaise A.NoValueProvided)
          pos [ tau ] pos
      in

      A.make_matchopt e1 e2 e3

    | Assertion, typ, expr ->
      let pos = Pos.get_position arg in
      let x = A.Var.make ("result", pos) in
      let arg = translate_expr ctx expr in

      let tau = (D.TAny, pos) in

      let e3 =
        A.make_abs
          (Array.of_list [ x ])
          (let+ v = Bindlib.box_var x in (v, pos))
          pos [ tau ] pos
      and e1 = arg
      and e2 =
        A.make_abs
          (Array.of_list [ x ])
          (Bindlib.box @@ same_pos @@ A.ERaise A.NoValueProvided)
          pos [ tau ] pos
      in

      A.make_matchopt e1 e2 e3

    | SubScopeVarDefinition, typ, expr ->
      assert false

    | DestructuringInputStruct, typ, expr ->
      assert false

    | DestructuringSubScopeResults, typ, expr ->
      assert false

    | CallingSubScope, typ, expr ->
      assert false

    


    | kind, typ, expr ->
      Errors.raise_spanned_error (Printf.sprintf "Internal error: Found %s different to Error on empty at the toplevel when compiling using the --avoid_exception option." s) (Pos.get_position e)
  in

  expr'

let translate_scope_body (ctx: ctx) (s: D.scope_body): A.expr = assert false



let translate_program (prgm : D.program) : A.program =
{
    scopes =
    (* todo: réécrire *)
      (let acc, _ =
         List.fold_left
           (fun ((acc, ctx) : 'a * A.Var.t D.VarMap.t) (_, n, e) ->
             let new_n = A.Var.make (Bindlib.name_of n, Pos.no_pos) in
             let new_acc =
               ( new_n,
                 Bindlib.unbox
                   (translate_expr_toplevel {
                    env=D.VarMap.map (fun v -> A.make_var (v, Pos.no_pos)) ctx;
                    env_pure=D.VarMap.map (fun _ -> true) ctx } e) )
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
