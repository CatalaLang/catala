(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria,
   contributors: Alain Delaët <alain.delaet--tixeuil@inria.fr>, Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)
open Utils
open Shared_ast
open Ast

type partial_evaluation_ctx = {
  var_values : (typed naked_expr, typed expr) Var.Map.t;
  decl_ctx : decl_ctx;
}

let rec partial_evaluation (ctx : partial_evaluation_ctx) (e : 'm expr) :
    'm expr Bindlib.box =
  let pos = Marked.get_mark e in
  let rec_helper = partial_evaluation ctx in
  match Marked.unmark e with
  | EApp
      ( (( EOp (Unop Not), _
         | EApp ((EOp (Unop (Log _)), _), [(EOp (Unop Not), _)]), _ ) as op),
        [e1] ) ->
    (* reduction of logical not *)
    (Bindlib.box_apply (fun e1 ->
         match e1 with
         | ELit (LBool false), _ -> ELit (LBool true), pos
         | ELit (LBool true), _ -> ELit (LBool false), pos
         | _ -> EApp (op, [e1]), pos))
      (rec_helper e1)
  | EApp
      ( (( EOp (Binop Or), _
         | EApp ((EOp (Unop (Log _)), _), [(EOp (Binop Or), _)]), _ ) as op),
        [e1; e2] ) ->
    (* reduction of logical or *)
    (Bindlib.box_apply2 (fun e1 e2 ->
         match e1, e2 with
         | (ELit (LBool false), _), new_e | new_e, (ELit (LBool false), _) ->
           new_e
         | (ELit (LBool true), _), _ | _, (ELit (LBool true), _) ->
           ELit (LBool true), pos
         | _ -> EApp (op, [e1; e2]), pos))
      (rec_helper e1) (rec_helper e2)
  | EApp
      ( (( EOp (Binop And), _
         | EApp ((EOp (Unop (Log _)), _), [(EOp (Binop And), _)]), _ ) as op),
        [e1; e2] ) ->
    (* reduction of logical and *)
    (Bindlib.box_apply2 (fun e1 e2 ->
         match e1, e2 with
         | (ELit (LBool true), _), new_e | new_e, (ELit (LBool true), _) ->
           new_e
         | (ELit (LBool false), _), _ | _, (ELit (LBool false), _) ->
           ELit (LBool false), pos
         | _ -> EApp (op, [e1; e2]), pos))
      (rec_helper e1) (rec_helper e2)
  | EVar x -> Bindlib.box_apply (fun x -> x, pos) (Bindlib.box_var x)
  | ETuple (args, s_name) ->
    Bindlib.box_apply
      (fun args -> ETuple (args, s_name), pos)
      (List.map rec_helper args |> Bindlib.box_list)
  | ETupleAccess (arg, i, s_name, typs) ->
    Bindlib.box_apply
      (fun arg -> ETupleAccess (arg, i, s_name, typs), pos)
      (rec_helper arg)
  | EInj (arg, i, e_name, typs) ->
    Bindlib.box_apply
      (fun arg -> EInj (arg, i, e_name, typs), pos)
      (rec_helper arg)
  | EMatch (arg, arms, e_name) ->
    Bindlib.box_apply2
      (fun arg arms ->
        match arg, arms with
        | (EInj (e1, i, e_name', _ts), _), _
          when EnumName.compare e_name e_name' = 0 ->
          (* iota reduction *)
          EApp (List.nth arms i, [e1]), pos
        | _ -> EMatch (arg, arms, e_name), pos)
      (rec_helper arg)
      (List.map rec_helper arms |> Bindlib.box_list)
  | EArray args ->
    Bindlib.box_apply
      (fun args -> EArray args, pos)
      (List.map rec_helper args |> Bindlib.box_list)
  | ELit l -> Bindlib.box (ELit l, pos)
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    let new_body = rec_helper body in
    let new_binder = Bindlib.bind_mvar vars new_body in
    Bindlib.box_apply (fun binder -> EAbs (binder, typs), pos) new_binder
  | EApp (f, args) ->
    Bindlib.box_apply2
      (fun f args ->
        match Marked.unmark f with
        | EAbs (binder, _ts) ->
          (* beta reduction *)
          Bindlib.msubst binder (List.map fst args |> Array.of_list)
        | _ -> EApp (f, args), pos)
      (rec_helper f)
      (List.map rec_helper args |> Bindlib.box_list)
  | EAssert e1 -> Bindlib.box_apply (fun e1 -> EAssert e1, pos) (rec_helper e1)
  | EOp op -> Bindlib.box (EOp op, pos)
  | EDefault (exceptions, just, cons) ->
    Bindlib.box_apply3
      (fun exceptions just cons ->
        (* TODO: mechanically prove each of these optimizations correct :) *)
        match
          ( List.filter
              (fun except ->
                match Marked.unmark except with
                | ELit LEmptyError -> false
                | _ -> true)
              exceptions
            (* we can discard the exceptions that are always empty error *),
            just,
            cons )
        with
        | exceptions, just, cons
          when List.fold_left
                 (fun nb except -> if Expr.is_value except then nb + 1 else nb)
                 0 exceptions
               > 1 ->
          (* at this point we know a conflict error will be triggered so we just
             feed the expression to the interpreter that will print the
             beautiful right error message *)
          Interpreter.evaluate_expr ctx.decl_ctx
            (EDefault (exceptions, just, cons), pos)
        | [except], _, _ when Expr.is_value except ->
          (* if there is only one exception and it is a non-empty value it is
             always chosen *)
          except
        | ( [],
            ( ( ELit (LBool true)
              | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool true), _)]) ),
              _ ),
            cons ) ->
          cons
        | ( [],
            ( ( ELit (LBool false)
              | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool false), _)]) ),
              _ ),
            _ ) ->
          ELit LEmptyError, pos
        | [], just, cons when not !Cli.avoid_exceptions_flag ->
          (* without exceptions, a default is just an [if then else] raising an
             error in the else case. This exception is only valid in the context
             of compilation_with_exceptions, so we desactivate with a global
             flag to know if we will be compiling using exceptions or the option
             monad. *)
          EIfThenElse (just, cons, (ELit LEmptyError, pos)), pos
        | exceptions, just, cons -> EDefault (exceptions, just, cons), pos)
      (List.map rec_helper exceptions |> Bindlib.box_list)
      (rec_helper just) (rec_helper cons)
  | EIfThenElse (e1, e2, e3) ->
    Bindlib.box_apply3
      (fun e1 e2 e3 ->
        match Marked.unmark e1, Marked.unmark e2, Marked.unmark e3 with
        | ELit (LBool true), _, _
        | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool true), _)]), _, _ ->
          e2
        | ELit (LBool false), _, _
        | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool false), _)]), _, _ ->
          e3
        | ( _,
            ( ELit (LBool true)
            | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool true), _)]) ),
            ( ELit (LBool false)
            | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool false), _)]) ) ) ->
          e1
        | _ when Expr.equal e2 e3 -> e2
        | _ -> EIfThenElse (e1, e2, e3), pos)
      (rec_helper e1) (rec_helper e2) (rec_helper e3)
  | ErrorOnEmpty e1 ->
    Bindlib.box_apply (fun e1 -> ErrorOnEmpty e1, pos) (rec_helper e1)

let optimize_expr (decl_ctx : decl_ctx) (e : 'm expr) =
  partial_evaluation { var_values = Var.Map.empty; decl_ctx } e

let rec scope_lets_map
    (t : 'a -> 'm expr -> 'm expr Bindlib.box)
    (ctx : 'a)
    (scope_body_expr : 'm naked_expr scope_body_expr) :
    'm naked_expr scope_body_expr Bindlib.box =
  match scope_body_expr with
  | Result e -> Bindlib.box_apply (fun e' -> Result e') (t ctx e)
  | ScopeLet scope_let ->
    let var, next = Bindlib.unbind scope_let.scope_let_next in
    let new_scope_let_expr = t ctx scope_let.scope_let_expr in
    let new_next = scope_lets_map t ctx next in
    let new_next = Bindlib.bind_var var new_next in
    Bindlib.box_apply2
      (fun new_scope_let_expr new_next ->
        ScopeLet
          {
            scope_let with
            scope_let_expr = new_scope_let_expr;
            scope_let_next = new_next;
          })
      new_scope_let_expr new_next

let rec scopes_map
    (t : 'a -> 'm expr -> 'm expr Bindlib.box)
    (ctx : 'a)
    (scopes : 'm naked_expr scopes) : 'm naked_expr scopes Bindlib.box =
  match scopes with
  | Nil -> Bindlib.box Nil
  | ScopeDef scope_def ->
    let scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
    let scope_arg_var, scope_body_expr =
      Bindlib.unbind scope_def.scope_body.scope_body_expr
    in
    let new_scope_body_expr = scope_lets_map t ctx scope_body_expr in
    let new_scope_body_expr =
      Bindlib.bind_var scope_arg_var new_scope_body_expr
    in
    let new_scope_next = scopes_map t ctx scope_next in
    let new_scope_next = Bindlib.bind_var scope_var new_scope_next in
    Bindlib.box_apply2
      (fun new_scope_body_expr new_scope_next ->
        ScopeDef
          {
            scope_def with
            scope_next = new_scope_next;
            scope_body =
              {
                scope_def.scope_body with
                scope_body_expr = new_scope_body_expr;
              };
          })
      new_scope_body_expr new_scope_next

let program_map
    (t : 'a -> 'm expr -> 'm expr Bindlib.box)
    (ctx : 'a)
    (p : 'm program) : 'm program Bindlib.box =
  Bindlib.box_apply
    (fun new_scopes -> { p with scopes = new_scopes })
    (scopes_map t ctx p.scopes)

let optimize_program (p : 'm program) : untyped program =
  Bindlib.unbox
    (program_map partial_evaluation
       { var_values = Var.Map.empty; decl_ctx = p.decl_ctx }
       p)
  |> Program.untype
