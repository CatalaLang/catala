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
open Utils
open Shared_ast
open Ast
module D = Dcalc.Ast

let visitor_map (t : 'a -> 'm expr -> 'm expr boxed) (ctx : 'a) (e : 'm expr) :
    'm expr boxed =
  Expr.map ctx ~f:t e

let rec iota_expr (_ : unit) (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EMatch ((EInj (e1, i, n', _ts), _), cases, n) when EnumName.compare n n' = 0
    ->
    let e1 = visitor_map iota_expr () e1 in
    let case = visitor_map iota_expr () (List.nth cases i) in
    Expr.eapp case [e1] m
  | EMatch (e', cases, n)
    when cases
         |> List.mapi (fun i (case, _pos) ->
                match case with
                | EInj (_ei, i', n', _ts') ->
                  i = i' && (* n = n' *) EnumName.compare n n' = 0
                | _ -> false)
         |> List.for_all Fun.id ->
    visitor_map iota_expr () e'
  | _ -> visitor_map iota_expr () e

let rec beta_expr (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EApp (e1, args) ->
    Expr.Box.app1n (beta_expr e1) (List.map beta_expr args)
      (fun e1 args ->
        match Marked.unmark e1 with
        | EAbs (binder, _) -> Marked.unmark (Expr.subst binder args)
        | _ -> EApp (e1, args))
      m
  | _ -> visitor_map (fun () -> beta_expr) () e

let iota_optimizations (p : 'm program) : 'm program =
  let new_scopes =
    Scope.map_exprs ~f:(iota_expr ()) ~varf:(fun v -> v) p.scopes
  in
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
  | EIfThenElse (e1, e2, e3) ->
    Expr.Box.app3 (peephole_expr e1) (peephole_expr e2) (peephole_expr e3)
      (fun e1 e2 e3 ->
        match Marked.unmark e1 with
        | ELit (LBool true)
        | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool true), _)]) ->
          Marked.unmark e2
        | ELit (LBool false)
        | EApp ((EOp (Unop (Log _)), _), [(ELit (LBool false), _)]) ->
          Marked.unmark e3
        | _ -> EIfThenElse (e1, e2, e3))
      m
  | ECatch (e1, except, e2) ->
    Expr.Box.app2 (peephole_expr e1) (peephole_expr e2)
      (fun e1 e2 ->
        match Marked.unmark e1, Marked.unmark e2 with
        | ERaise except', ERaise except''
          when except' = except && except = except'' ->
          ERaise except
        | ERaise except', _ when except' = except -> Marked.unmark e2
        | _, ERaise except' when except' = except -> Marked.unmark e1
        | _ -> ECatch (e1, except, e2))
      m
  | _ -> visitor_map (fun () -> peephole_expr) () e

let peephole_optimizations (p : 'm program) : 'm program =
  let new_scopes =
    Scope.map_exprs ~f:peephole_expr ~varf:(fun v -> v) p.scopes
  in
  { p with scopes = Bindlib.unbox new_scopes }

let optimize_program (p : 'm program) : untyped program =
  p |> iota_optimizations |> peephole_optimizations |> Program.untype
