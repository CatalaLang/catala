(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)
open Catala_utils
open Shared_ast
open Ast
module D = Dcalc.Ast

let visitor_map (t : 'm expr -> 'm expr boxed) (e : 'm expr) : 'm expr boxed =
  Expr.map ~f:t e

let rec iota_expr (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EMatch { e = EInj { e = e'; cons; name = n' }, _; cases; name = n }
    when EnumName.equal n n' ->
    let e1 = visitor_map iota_expr e' in
    let case = visitor_map iota_expr (EnumConstructor.Map.find cons cases) in
    Expr.eapp case [e1] m
  | EMatch { e = e'; cases; name = n }
    when cases
         |> EnumConstructor.Map.mapi (fun i case ->
                match Marked.unmark case with
                | EInj { cons = i'; name = n'; _ } ->
                  EnumConstructor.equal i i' && EnumName.equal n n'
                | _ -> false)
         |> EnumConstructor.Map.for_all (fun _ b -> b) ->
    visitor_map iota_expr e'
  | _ -> visitor_map iota_expr e

let rec beta_expr (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EApp { f = e1; args } ->
    Expr.Box.app1n (beta_expr e1) (List.map beta_expr args)
      (fun e1 args ->
        match Marked.unmark e1 with
        | EAbs { binder; _ } -> Marked.unmark (Expr.subst binder args)
        | _ -> EApp { f = e1; args })
      m
  | _ -> visitor_map beta_expr e

let iota_optimizations (p : 'm program) : 'm program =
  let new_scopes = Scope.map_exprs ~f:iota_expr ~varf:(fun v -> v) p.scopes in
  { p with scopes = Bindlib.unbox new_scopes }

(* TODO: beta optimizations apply inlining of the program. We left the inclusion
   of beta-optimization as future work since its produce code that is harder to
   read, and can produce exponential blowup of the size of the generated
   program. *)
let _beta_optimizations (p : 'm program) : 'm program =
  let new_scopes = Scope.map_exprs ~f:beta_expr ~varf:(fun v -> v) p.scopes in
  { p with scopes = Bindlib.unbox new_scopes }

let rec peephole_expr (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EIfThenElse { cond; etrue; efalse } ->
    Expr.Box.app3 (peephole_expr cond) (peephole_expr etrue)
      (peephole_expr efalse)
      (fun cond etrue efalse ->
        match Marked.unmark cond with
        | ELit (LBool true)
        | EApp { f = EOp { op = Log _; _ }, _; args = [(ELit (LBool true), _)] }
          ->
          Marked.unmark etrue
        | ELit (LBool false)
        | EApp
            { f = EOp { op = Log _; _ }, _; args = [(ELit (LBool false), _)] }
          ->
          Marked.unmark efalse
        | _ -> EIfThenElse { cond; etrue; efalse })
      m
  | EApp { f = EAbs { binder; tys = [_ty] }, _; args = [arg] } -> (
    (* basic inlining *)
    let vars, body = Bindlib.unmbind binder in
    let v = Array.get vars 0 in
    match Marked.unmark body with
    | EVar v' when Var.eq v v' -> visitor_map peephole_expr arg
    | _ -> visitor_map peephole_expr e)
  | ECatch { body; exn; handler } ->
    Expr.Box.app2 (peephole_expr body) (peephole_expr handler)
      (fun body handler ->
        match Marked.unmark body, Marked.unmark handler with
        | ERaise exn', ERaise exn'' when exn' = exn && exn = exn'' -> ERaise exn
        | ERaise exn', _ when exn' = exn -> Marked.unmark handler
        | _, ERaise exn' when exn' = exn -> Marked.unmark body
        | _ -> ECatch { body; exn; handler })
      m
  | _ -> visitor_map peephole_expr e

let peephole_optimizations (p : 'm program) : 'm program =
  let new_scopes =
    Scope.map_exprs ~f:peephole_expr ~varf:(fun v -> v) p.scopes
  in
  { p with scopes = Bindlib.unbox new_scopes }

let optimize_program (p : 'm program) : untyped program =
  p |> iota_optimizations |> peephole_optimizations |> Program.untype
