(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributors: Alain Delaët
   <alain.delaet--tixeuil@inria.fr>, Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)
open Utils
open Ast

type partial_evaluation_ctx = { var_values : expr Pos.marked Ast.VarMap.t; decl_ctx : decl_ctx }

let rec partial_evaluation (ctx : partial_evaluation_ctx) (e : expr Pos.marked) :
    expr Pos.marked Bindlib.box =
  let pos = Pos.get_position e in
  let rec_helper = partial_evaluation ctx in
  match Pos.unmark e with
  | EApp
      ( ((EOp (Unop Not), _ | EApp ((EOp (Unop (Log _)), _), [ (EOp (Unop Not), _) ]), _) as op),
        [ e1 ] ) ->
      (* reduction of logical not *)
      (Bindlib.box_apply (fun e1 ->
           match e1 with
           | ELit (LBool false), _ -> (ELit (LBool true), pos)
           | ELit (LBool true), _ -> (ELit (LBool false), pos)
           | _ -> (EApp (op, [ e1 ]), pos)))
        (rec_helper e1)
  | EApp
      ( ((EOp (Binop Or), _ | EApp ((EOp (Unop (Log _)), _), [ (EOp (Binop Or), _) ]), _) as op),
        [ e1; e2 ] ) ->
      (* reduction of logical or *)
      (Bindlib.box_apply2 (fun e1 e2 ->
           match (e1, e2) with
           | (ELit (LBool false), _), new_e | new_e, (ELit (LBool false), _) -> new_e
           | (ELit (LBool true), _), _ | _, (ELit (LBool true), _) -> (ELit (LBool true), pos)
           | _ -> (EApp (op, [ e1; e2 ]), pos)))
        (rec_helper e1) (rec_helper e2)
  | EApp
      ( ((EOp (Binop And), _ | EApp ((EOp (Unop (Log _)), _), [ (EOp (Binop And), _) ]), _) as op),
        [ e1; e2 ] ) ->
      (* reduction of logical and *)
      (Bindlib.box_apply2 (fun e1 e2 ->
           match (e1, e2) with
           | (ELit (LBool true), _), new_e | new_e, (ELit (LBool true), _) -> new_e
           | (ELit (LBool false), _), _ | _, (ELit (LBool false), _) -> (ELit (LBool false), pos)
           | _ -> (EApp (op, [ e1; e2 ]), pos)))
        (rec_helper e1) (rec_helper e2)
  | EVar (x, _) -> Bindlib.box_apply (fun x -> (x, pos)) (Bindlib.box_var x)
  | ETuple (args, s_name) ->
      Bindlib.box_apply
        (fun args -> (ETuple (args, s_name), pos))
        (List.map rec_helper args |> Bindlib.box_list)
  | ETupleAccess (arg, i, s_name, typs) ->
      Bindlib.box_apply (fun arg -> (ETupleAccess (arg, i, s_name, typs), pos)) (rec_helper arg)
  | EInj (arg, i, e_name, typs) ->
      Bindlib.box_apply (fun arg -> (EInj (arg, i, e_name, typs), pos)) (rec_helper arg)
  | EMatch (arg, arms, e_name) ->
      Bindlib.box_apply2
        (fun arg arms ->
          match (arg, arms) with
          | (EInj (e1, i, e_name', _ts), _), _ when Ast.EnumName.compare e_name e_name' = 0 ->
              (* iota reduction *)
              (EApp (List.nth arms i, [ e1 ]), pos)
          | _ -> (EMatch (arg, arms, e_name), pos))
        (rec_helper arg)
        (List.map rec_helper arms |> Bindlib.box_list)
  | EArray args ->
      Bindlib.box_apply
        (fun args -> (EArray args, pos))
        (List.map rec_helper args |> Bindlib.box_list)
  | ELit l -> Bindlib.box (ELit l, pos)
  | EAbs ((binder, binder_pos), typs) ->
      let vars, body = Bindlib.unmbind binder in
      let new_body = rec_helper body in
      let new_binder = Bindlib.bind_mvar vars new_body in
      Bindlib.box_apply (fun binder -> (EAbs ((binder, binder_pos), typs), pos)) new_binder
  | EApp (f, args) ->
      Bindlib.box_apply2
        (fun f args ->
          match Pos.unmark f with
          | EAbs ((binder, _pos_binder), _ts) ->
              (* beta reduction *)
              Bindlib.msubst binder (List.map fst args |> Array.of_list)
          | _ -> (EApp (f, args), pos))
        (rec_helper f)
        (List.map rec_helper args |> Bindlib.box_list)
  | EAssert e1 -> Bindlib.box_apply (fun e1 -> (EAssert e1, pos)) (rec_helper e1)
  | EOp op -> Bindlib.box (EOp op, pos)
  | EDefault (exceptions, just, cons) ->
      Bindlib.box_apply3
        (fun exceptions just cons ->
          (* TODO: mechanically prove each of these optimizations correct :) *)
          match
            ( List.filter
                (fun except -> match Pos.unmark except with ELit LEmptyError -> false | _ -> true)
                exceptions
              (* we can discard the exceptions that are always empty error *),
              just,
              cons )
          with
          | exceptions, just, cons
            when List.fold_left
                   (fun nb except -> if is_value except then nb + 1 else nb)
                   0 exceptions
                 > 1 ->
              (* at this point we know a conflict error will be triggered so we just feed the
                 expression to the interpreter that will print the beautiful right error message *)
              Interpreter.evaluate_expr ctx.decl_ctx (EDefault (exceptions, just, cons), pos)
          | [ except ], _, _ when is_value except ->
              (* if there is only one exception and it is a non-empty value it is always chosen *)
              except
          | ( [],
              ((ELit (LBool true) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ])), _),
              cons ) ->
              cons
          | ( [],
              ((ELit (LBool false) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ])), _),
              _ ) ->
              (ELit LEmptyError, pos)
          | [], just, cons when not !Cli.avoid_exceptions_flag ->
              (* without exceptions, a default is just an [if then else] raising an error in the
                 else case. This exception is only valid in the context of
                 compilation_with_exceptions, so we desactivate with a global flag to know if we
                 will be compiling using exceptions or the option monad. *)
              (EIfThenElse (just, cons, (ELit LEmptyError, pos)), pos)
          | exceptions, just, cons -> (EDefault (exceptions, just, cons), pos))
        (List.map rec_helper exceptions |> Bindlib.box_list)
        (rec_helper just) (rec_helper cons)
  | EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 ->
          match (Pos.unmark e1, Pos.unmark e2, Pos.unmark e3) with
          | ELit (LBool true), _, _
          | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ]), _, _ ->
              e2
          | ELit (LBool false), _, _
          | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ]), _, _ ->
              e3
          | ( _,
              (ELit (LBool true) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ])),
              (ELit (LBool false) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ])) )
            ->
              e1
          | _ -> (EIfThenElse (e1, e2, e3), pos))
        (rec_helper e1) (rec_helper e2) (rec_helper e3)
  | ErrorOnEmpty e1 -> Bindlib.box_apply (fun e1 -> (ErrorOnEmpty e1, pos)) (rec_helper e1)

let optimize_expr (decl_ctx : decl_ctx) (e : expr Pos.marked) =
  partial_evaluation { var_values = VarMap.empty; decl_ctx } e

let program_map (t : 'a -> expr Pos.marked -> expr Pos.marked Bindlib.box) (ctx : 'a) (p : program)
    : program =
  {
    p with
    scopes =
      List.map
        (fun (s_name, s_var, s_body) ->
          let new_s_body =
            {
              s_body with
              scope_body_lets =
                List.map
                  (fun scope_let ->
                    {
                      scope_let with
                      scope_let_expr =
                        Bindlib.unbox (Bindlib.box_apply (t ctx) scope_let.scope_let_expr);
                    })
                  s_body.scope_body_lets;
            }
          in
          (s_name, s_var, new_s_body))
        p.scopes;
  }

let optimize_program (p : program) : program =
  program_map partial_evaluation { var_values = VarMap.empty; decl_ctx = p.decl_ctx } p

let rec remove_all_logs (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  let pos = Pos.get_position e in
  let rec_helper = remove_all_logs in
  match Pos.unmark e with
  | EVar (x, _) -> Bindlib.box_apply (fun x -> (x, pos)) (Bindlib.box_var x)
  | ETuple (args, s_name) ->
      Bindlib.box_apply
        (fun args -> (ETuple (args, s_name), pos))
        (List.map rec_helper args |> Bindlib.box_list)
  | ETupleAccess (arg, i, s_name, typs) ->
      Bindlib.box_apply (fun arg -> (ETupleAccess (arg, i, s_name, typs), pos)) (rec_helper arg)
  | EInj (arg, i, e_name, typs) ->
      Bindlib.box_apply (fun arg -> (EInj (arg, i, e_name, typs), pos)) (rec_helper arg)
  | EMatch (arg, arms, e_name) ->
      Bindlib.box_apply2
        (fun arg arms -> (EMatch (arg, arms, e_name), pos))
        (rec_helper arg)
        (List.map rec_helper arms |> Bindlib.box_list)
  | EArray args ->
      Bindlib.box_apply
        (fun args -> (EArray args, pos))
        (List.map rec_helper args |> Bindlib.box_list)
  | ELit l -> Bindlib.box (ELit l, pos)
  | EAbs ((binder, binder_pos), typs) ->
      let vars, body = Bindlib.unmbind binder in
      let new_body = rec_helper body in
      let new_binder = Bindlib.bind_mvar vars new_body in
      Bindlib.box_apply (fun binder -> (EAbs ((binder, binder_pos), typs), pos)) new_binder
  | EApp (f, args) ->
      Bindlib.box_apply2
        (fun f args ->
          match (Pos.unmark f, args) with
          | EOp (Unop (Log _)), [ arg ] -> arg
          | _ -> (EApp (f, args), pos))
        (rec_helper f)
        (List.map rec_helper args |> Bindlib.box_list)
  | EAssert e1 -> Bindlib.box_apply (fun e1 -> (EAssert e1, pos)) (rec_helper e1)
  | EOp op -> Bindlib.box (EOp op, pos)
  | EDefault (exceptions, just, cons) ->
      Bindlib.box_apply3
        (fun exceptions just cons -> (EDefault (exceptions, just, cons), pos))
        (List.map rec_helper exceptions |> Bindlib.box_list)
        (rec_helper just) (rec_helper cons)
  | EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 -> (EIfThenElse (e1, e2, e3), pos))
        (rec_helper e1) (rec_helper e2) (rec_helper e3)
  | ErrorOnEmpty e1 -> Bindlib.box_apply (fun e1 -> (ErrorOnEmpty e1, pos)) (rec_helper e1)
