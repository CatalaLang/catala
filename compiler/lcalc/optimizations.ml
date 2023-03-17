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
    (* match E x with | E y -> e1 = e1[y |-> x]*)
    if true then
      match Marked.unmark @@ EnumConstructor.Map.find cons cases with
      | EAbs { binder; _ } -> visitor_map iota_expr (Expr.subst binder [e'])
      | _ -> assert false
    else
      let e1 = visitor_map iota_expr e' in
      let case = visitor_map iota_expr (EnumConstructor.Map.find cons cases) in
      Expr.eapp case [e1] m
      (* if the size of the expression is small enought, we just substitute. *)
  | EMatch { e = e'; cases; name = n }
    when cases
         |> EnumConstructor.Map.for_all (fun i case ->
                match Marked.unmark case with
                | EAbs { binder; _ } -> (
                  let var, body = Bindlib.unmbind binder in
                  (* because of invariant [invariant_match], the arity is always
                     one. *)
                  let[@warning "-8"] [| var |] = var in

                  match Marked.unmark body with
                  | EInj { cons = i'; name = n'; e = EVar x, _ } ->
                    EnumConstructor.equal i i'
                    && EnumName.equal n n'
                    && Bindlib.eq_vars x var
                  | EInj { cons = i'; name = n'; e = ELit LUnit, _ } ->
                    (* since unit is the only value of type unit. We don't need
                       to check the equality. *)
                    EnumConstructor.equal i i' && EnumName.equal n n'
                  | _ -> false)
                | _ ->
                  (* because of invariant [invariant_match], there is always
                     some EAbs in each cases. *)
                  assert false) ->
    visitor_map iota_expr e'
  | _ -> visitor_map iota_expr e

let rec iota2_expr (e : 'm expr) : 'm expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EMatch
      {
        e = EMatch { e = arg; cases = cases1; name = n1 }, _;
        cases = cases2;
        name = n2;
      }
    when n1 = n2
         &&
         let n = n1 in
         let b =
           EnumConstructor.MapLabels.for_all cases1 ~f:(fun i case ->
               match Marked.unmark case with
               | EAbs { binder; _ } -> (
                 let _, body = Bindlib.unmbind binder in
                 Cli.debug_format "expr %a" (Print.expr_debug ~debug:true) body;
                 match Marked.unmark body with
                 | EInj { cons = i'; name = n'; _ } ->
                   Cli.debug_format "bla %a %a %b" EnumConstructor.format_t i
                     EnumConstructor.format_t i'
                     (EnumConstructor.equal i i');

                   Cli.debug_format "bli %a %a %b" EnumName.format_t n
                     EnumName.format_t n' (EnumName.equal n n');
                   EnumConstructor.equal i i' && EnumName.equal n n'
                 | _ -> false)
               | _ -> assert false)
         in
         Cli.debug_format "%b" b;
         b ->
    let cases =
      EnumConstructor.MapLabels.merge cases1 cases2 ~f:(fun _i o1 o2 ->
          match o1, o2 with
          | Some b1, Some e2 -> (
            match Marked.unmark b1, Marked.unmark e2 with
            | EAbs { binder = b1; _ }, EAbs { binder = b2; tys } ->
              let v1, e1 = Bindlib.unmbind b1 in
              let[@warning "-8"] [| v1 |] = v1 in
              Some
                (Expr.make_abs [| v1 |]
                   (visitor_map iota_expr (Expr.subst b2 [e1]))
                   tys (Expr.pos e2))
            | _ -> assert false)
          | _ -> assert false)
    in
    Expr.ematch (visitor_map iota_expr arg) n1 cases m
  | _ -> visitor_map iota2_expr e

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

let rec fold_expr (e : 'm expr) : 'm expr boxed =
  match Marked.unmark e with
  | EApp { f = EOp { op = Op.Fold; _ }, _; args = [_f; init; (EArray [], _)] }
    ->
    visitor_map fold_expr init
  | EApp { f = EOp { op = Op.Fold; _ }, _; args = [f; init; (EArray [e'], _)] }
    ->
    Expr.make_app (visitor_map fold_expr f)
      [visitor_map fold_expr init; visitor_map fold_expr e']
      (Expr.pos e)
  | _ -> visitor_map fold_expr e

let iota_optimizations (p : 'm program) : 'm program =
  let new_code_items =
    Scope.map_exprs ~f:iota_expr ~varf:(fun v -> v) p.code_items
  in
  { p with code_items = Bindlib.unbox new_code_items }

let iota2_optimizations (p : 'm program) : 'm program =
  let new_code_items =
    Scope.map_exprs ~f:iota2_expr ~varf:(fun v -> v) p.code_items
  in
  { p with code_items = Bindlib.unbox new_code_items }

let fold_optimizations (p : 'm program) : 'm program =
  let new_code_items =
    Scope.map_exprs ~f:fold_expr ~varf:(fun v -> v) p.code_items
  in
  { p with code_items = Bindlib.unbox new_code_items }

(* TODO: beta optimizations apply inlining of the program. We left the inclusion
   of beta-optimization as future work since its produce code that is harder to
   read, and can produce exponential blowup of the size of the generated
   program. *)
let _beta_optimizations (p : 'm program) : 'm program =
  let new_code_items =
    Scope.map_exprs ~f:beta_expr ~varf:(fun v -> v) p.code_items
  in
  { p with code_items = Bindlib.unbox new_code_items }

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
  let new_code_items =
    Scope.map_exprs ~f:peephole_expr ~varf:(fun v -> v) p.code_items
  in
  { p with code_items = Bindlib.unbox new_code_items }

let rec fix_opti
    ?(maxiter = 100)
    ~(fs : ('m program -> 'm program) list)
    (p : 'm program) =
  assert (maxiter >= 0);
  let p' = ListLabels.fold_left ~init:p fs ~f:(fun p f -> f p) in

  if Program.equal p' p || maxiter = 0 then p'
  else fix_opti ~fs p ~maxiter:(maxiter - 1)

let optimize_program (p : 'm program) : untyped program =
  Program.untype
    (fix_opti p
       ~fs:
         [
           iota_optimizations;
           iota2_optimizations;
           fold_optimizations;
           (* _beta_optimizations; *)
           peephole_optimizations;
         ])
