(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Alain Delaët
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

let ( let+ ) x f = Bindlib.box_apply f x

let ( and+ ) x y = Bindlib.box_pair x y

let visitor_map (t : 'a -> expr Pos.marked -> expr Pos.marked Bindlib.box) (ctx : 'a)
    (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  (* calls [t ctx] on every direct childs of [e], then rebuild an abstract syntax tree modified.
     Used in other transformations. *)
  let default_mark e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | EVar (v, pos) ->
      let+ v = Bindlib.box_var v in
      (v, pos)
  | ETuple (args, n) ->
      let+ args = args |> List.map (t ctx) |> Bindlib.box_list in
      default_mark @@ ETuple (args, n)
  | ETupleAccess (e1, i, n, ts) ->
      let+ e1 = t ctx e1 in
      default_mark @@ ETupleAccess (e1, i, n, ts)
  | EInj (e1, i, n, ts) ->
      let+ e1 = t ctx e1 in
      default_mark @@ EInj (e1, i, n, ts)
  | EMatch (arg, cases, n) ->
      let+ arg = t ctx arg and+ cases = cases |> List.map (t ctx) |> Bindlib.box_list in
      default_mark @@ EMatch (arg, cases, n)
  | EArray args ->
      let+ args = args |> List.map (t ctx) |> Bindlib.box_list in
      default_mark @@ EArray args
  | EAbs ((binder, pos_binder), ts) ->
      let vars, body = Bindlib.unmbind binder in
      let body = t ctx body in
      let+ binder = Bindlib.bind_mvar vars body in
      default_mark @@ EAbs ((binder, pos_binder), ts)
  | EApp (e1, args) ->
      let+ e1 = t ctx e1 and+ args = args |> List.map (t ctx) |> Bindlib.box_list in
      default_mark @@ EApp (e1, args)
  | EAssert e1 ->
      let+ e1 = t ctx e1 in
      default_mark @@ EAssert e1
  | EIfThenElse (e1, e2, e3) ->
      let+ e1 = t ctx e1 and+ e2 = t ctx e2 and+ e3 = t ctx e3 in
      default_mark @@ EIfThenElse (e1, e2, e3)
  | ErrorOnEmpty e1 ->
      let+ e1 = t ctx e1 in
      default_mark @@ ErrorOnEmpty e1
  | EDefault (exceptions, just, cons) ->
      let+ exceptions = exceptions |> List.map (t ctx) |> Bindlib.box_list
      and+ just = t ctx just
      and+ cons = t ctx cons in
      default_mark @@ EDefault (exceptions, just, cons)
  | ELit _ | EOp _ -> Bindlib.box e

let rec iota_expr (_ : unit) (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  let default_mark e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | EMatch ((EInj (e1, i, n', _ts), _), cases, n) when Ast.EnumName.compare n n' = 0 ->
      let+ e1 = visitor_map iota_expr () e1
      and+ case = visitor_map iota_expr () (List.nth cases i) in
      default_mark @@ EApp (case, [ e1 ])
  | EMatch (e', cases, n)
    when begin
           cases
           |> List.mapi (fun i (case, _pos) ->
                  match case with
                  | EInj (_ei, i', n', _ts') -> i = i' && (* n = n' *) Ast.EnumName.compare n n' = 0
                  | _ -> false)
           |> List.for_all Fun.id
         end ->
      visitor_map iota_expr () e'
  | _ -> visitor_map iota_expr () e

let rec beta_expr (_ : unit) (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  let default_mark e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | EApp (e1, args) -> (
      let+ e1 = visitor_map beta_expr () e1
      and+ args = List.map (visitor_map beta_expr ()) args |> Bindlib.box_list in
      match Pos.unmark e1 with
      | EAbs ((binder, _pos_binder), _ts) ->
          let (_ : (_, _) Bindlib.mbinder) = binder in
          Bindlib.msubst binder (List.map fst args |> Array.of_list)
      | _ -> default_mark @@ EApp (e1, args))
  | _ -> visitor_map beta_expr () e

let count = ref 0

let rec peephole_expr (ctx : decl_ctx) (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  incr count;
  let icount = !count in
  Cli.debug_print
    (Format.asprintf "%d: On commence à optimiser %a" !count (Print.format_expr ctx) e);
  let default_mark e' = Pos.same_pos_as e' e in
  let out =
    match Pos.unmark e with
    | EIfThenElse (e1, e2, e3) -> (
        let+ new_e1 = visitor_map peephole_expr ctx e1
        and+ new_e2 = visitor_map peephole_expr ctx e2
        and+ new_e3 = visitor_map peephole_expr ctx e3 in
        match (Pos.unmark new_e1, Pos.unmark new_e2, Pos.unmark new_e3) with
        | ELit (LBool true), _, _ | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ]), _, _
          ->
            new_e2
        | ELit (LBool false), _, _
        | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ]), _, _ ->
            new_e3
        | ( _,
            (ELit (LBool true) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ])),
            (ELit (LBool false) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ])) ) ->
            e1
        | _ -> default_mark @@ EIfThenElse (new_e1, new_e2, new_e3))
    | EApp
        ( ((EOp (Binop Or), _ | EApp ((EOp (Unop (Log _)), _), [ (EOp (Binop Or), _) ]), _) as op),
          [ e1; e2 ] ) -> (
        Cli.debug_print
          (Format.asprintf "Trouvé ou %d avec arguments %a et %a" icount (Print.format_expr ctx) e1
             (Print.format_expr ctx) e2);
        let+ new_e1 = visitor_map peephole_expr ctx e1
        and+ new_e2 = visitor_map peephole_expr ctx e2 in
        match (Pos.unmark new_e1, Pos.unmark new_e2) with
        | ELit (LBool false), new_e1 | new_e1, ELit (LBool false) -> default_mark @@ new_e1
        | ELit (LBool true), _ | _, ELit (LBool true) -> default_mark @@ ELit (LBool true)
        | _ -> default_mark @@ EApp (op, [ new_e1; new_e2 ]))
    | EApp (((EOp (Binop And), _) as op), [ e1; e2 ]) -> (
        let+ new_e1 = visitor_map peephole_expr ctx e1
        and+ new_e2 = visitor_map peephole_expr ctx e2 in
        match (new_e1, new_e2) with
        | (ELit (LBool true), _), new_e1 | new_e1, (ELit (LBool true), _) -> new_e1
        | (ELit (LBool false), _), _ | _, (ELit (LBool false), _) ->
            default_mark @@ ELit (LBool false)
        | _ -> default_mark @@ EApp (op, [ new_e1; new_e2 ]))
    | _ -> visitor_map peephole_expr ctx e
  in
  Cli.debug_print
    (Format.asprintf "%d: Fin optimisation %a" icount (Print.format_expr ctx) (Bindlib.unbox out));
  out

let optimize_expr (ctx : decl_ctx) (e : expr Pos.marked) : expr Pos.marked =
  let e = ref e in
  let continue = ref true in
  while !continue do
    let new_e =
      !e |> peephole_expr ctx |> Bindlib.unbox |> beta_expr () |> Bindlib.unbox |> iota_expr ()
      |> Bindlib.unbox
    in
    (* Cli.debug_print (Format.asprintf "Optimizing %a into %a" (Print.format_expr ctx) !e
       (Print.format_expr ctx) new_e); *)
    if not (expr_size new_e < expr_size !e) then continue := false;
    e := new_e
  done;
  !e

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

let iota_optimizations (p : program) : program = program_map iota_expr () p

let beta_optimizations (p : program) : program = program_map beta_expr () p

let peephole_optimizations (p : program) : program = program_map peephole_expr p.decl_ctx p

let optimize_program (p : program) : program = p |> iota_optimizations |> peephole_optimizations
