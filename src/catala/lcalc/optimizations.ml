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
open Ast

let rec peephole_expr (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  match Pos.unmark e with
  | EVar (v, pos) -> Bindlib.box_apply (fun v -> (v, pos)) (Bindlib.box_var v)
  | ETuple (args, n) ->
      Bindlib.box_apply
        (fun args -> (ETuple (args, n), Pos.get_position e))
        (Bindlib.box_list (List.map peephole_expr args))
  | ETupleAccess (e1, i, n, ts) ->
      Bindlib.box_apply
        (fun e1 -> (ETupleAccess (e1, i, n, ts), Pos.get_position e))
        (peephole_expr e1)
  | EInj (e1, i, n, ts) ->
      Bindlib.box_apply (fun e1 -> (EInj (e1, i, n, ts), Pos.get_position e)) (peephole_expr e1)
  | EMatch (arg, cases, n) ->
      Bindlib.box_apply2
        (fun arg cases -> (EMatch (arg, cases, n), Pos.get_position e))
        (peephole_expr arg)
        (Bindlib.box_list (List.map peephole_expr cases))
  | EArray args ->
      Bindlib.box_apply
        (fun args -> (EArray args, Pos.get_position e))
        (Bindlib.box_list (List.map peephole_expr args))
  | EAbs ((binder, pos_binder), ts) ->
      let vars, body = Bindlib.unmbind binder in
      let body = peephole_expr body in
      Bindlib.box_apply
        (fun binder -> (EAbs ((binder, pos_binder), ts), Pos.get_position e))
        (Bindlib.bind_mvar vars body)
  | EApp (e1, args) ->
      Bindlib.box_apply2
        (fun e1 args -> (EApp (e1, args), Pos.get_position e))
        (peephole_expr e1)
        (Bindlib.box_list (List.map peephole_expr args))
  | EAssert e1 -> Bindlib.box_apply (fun e1 -> (EAssert e1, Pos.get_position e)) (peephole_expr e1)
  | EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 ->
          match Pos.unmark e1 with
          | ELit (LBool true) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool true), _) ]) -> e2
          | ELit (LBool false) | EApp ((EOp (Unop (Log _)), _), [ (ELit (LBool false), _) ]) -> e3
          | _ -> (EIfThenElse (e1, e2, e3), Pos.get_position e))
        (peephole_expr e1) (peephole_expr e2) (peephole_expr e3)
  | ECatch (e1, exn, e2) ->
      Bindlib.box_apply2
        (fun e1 e2 ->
          ( (match Pos.unmark e2 with
            | ERaise exn2 when exn2 = exn -> Pos.unmark e1
            | _ -> ECatch (e1, exn, e2)),
            Pos.get_position e ))
        (peephole_expr e1) (peephole_expr e2)
  | ERaise _ | ELit _ | EOp _ -> Bindlib.box e

let peephole_optimizations (p : program) : program =
  { p with scopes = List.map (fun (var, e) -> (var, Bindlib.unbox (peephole_expr e))) p.scopes }

let optimize_program (p : program) : program = peephole_optimizations p
