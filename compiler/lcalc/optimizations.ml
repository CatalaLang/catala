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

(* To implement a new pass, you can describe how to optimize an expression, and
   then use the [lift_optim] function to lift it to program. *)

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
      (* holds because of invariant_match_inversion *)
      | EAbs { binder; _ } -> visitor_map iota_expr (Expr.subst binder [e'])
      | _ -> assert false
    else
      let e1 = visitor_map iota_expr e' in
      let case = visitor_map iota_expr (EnumConstructor.Map.find cons cases) in
      Expr.eapp case [e1] m
      (* if the size of the expression is small enought, we just substitute. *)
  | EMatch { e = e'; cases; name = n }
    when EnumConstructor.MapLabels.for_all cases ~f:(fun i case ->
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
                 (* since unit is the only value of type unit. We don't need to
                    check the equality. *)
                 EnumConstructor.equal i i' && EnumName.equal n n'
               | _ -> false)
             | _ ->
               (* because of invariant [invariant_match], there is always some
                  EAbs in each cases. *)
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
                 match Marked.unmark body with
                 | EInj { cons = i'; name = n'; _ } ->
                   EnumConstructor.equal i i' && EnumName.equal n n'
                 | _ -> false)
               | _ -> assert false)
         in
         (* Cli.debug_format "%b" b; *)
         b ->
    let cases =
      EnumConstructor.MapLabels.merge cases1 cases2 ~f:(fun _i o1 o2 ->
          match o1, o2 with
          | Some b1, Some e2 -> (
            match Marked.unmark b1, Marked.unmark e2 with
            | EAbs { binder = b1; _ }, EAbs { binder = b2; tys } -> (
              let v1, e1 = Bindlib.unmbind b1 in
              let[@warning "-8"] [| v1 |] = v1 in

              match Marked.unmark e1 with
              | EInj { e = e1; _ } ->
                Some
                  (Expr.make_abs [| v1 |]
                     (visitor_map iota_expr (Expr.subst b2 [e1]))
                     tys (Expr.pos e2))
              | _ -> assert false)
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
  | EApp { f = EAbs { binder; tys = [_ty] }, _; args = [(EVar _, _)] as args }
    ->
    (* basic inlining 1 *)
    visitor_map peephole_expr (Expr.subst binder args)
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

(** The function [fix_opti ~maxiter ~fs p] tries to apply [fs] optimizations to
    [p]. It stops after [max_iter] iterations have been reach, or if the program
    didn't changed between two versions. *)
let rec fix_opti
    ?(maxiter = 5)
    ~(fs : ('m program -> 'm program) list)
    (p : 'm program) =
  assert (maxiter >= 0);
  let p' = ListLabels.fold_left ~init:p fs ~f:(fun p f -> f p) in

  if Program.equal p' p then p' else fix_opti ~fs p' ~maxiter:(maxiter - 1)

(** the function [lift_optim f p] apply the expression transformation [f] all
    expression of the program [p]. *)
let lift_optim f p =
  let code_items = Scope.map_exprs ~f ~varf:(fun v -> v) p.code_items in
  Bindlib_ext.assert_closed code_items;
  let prgm = { p with code_items = Bindlib.unbox code_items } in
  prgm

let optimize_program (p : 'm program) : untyped program =
  Program.untype
    (fix_opti p
       ~fs:
         [
           lift_optim iota_expr;
           lift_optim iota2_expr;
           lift_optim fold_expr;
           lift_optim beta_expr;
           lift_optim peephole_expr;
         ])

let%expect_test _ =
  Cli.call_unstyled (fun _ ->
      let x = Var.make "x" in

      let enumT = EnumName.fresh ("t", Pos.no_pos) in
      let consA = EnumConstructor.fresh ("A", Pos.no_pos) in
      let consB = EnumConstructor.fresh ("B", Pos.no_pos) in
      let consC = EnumConstructor.fresh ("C", Pos.no_pos) in
      let consD = EnumConstructor.fresh ("D", Pos.no_pos) in

      let nomark = Untyped { pos = Pos.no_pos } in

      let injA = Expr.einj (Expr.evar x nomark) consA enumT nomark in
      (* let injB = Expr.einj (Expr.evar x nomark) consB enumT nomark in *)
      let injC = Expr.einj (Expr.evar x nomark) consC enumT nomark in
      let injD = Expr.einj (Expr.evar x nomark) consD enumT nomark in

      let cases : ('a, 't) boxed_gexpr EnumConstructor.Map.t =
        EnumConstructor.Map.of_seq
        @@ List.to_seq
        @@ [
             consA, Expr.eabs (Expr.bind [| x |] injC) [TAny, Pos.no_pos] nomark;
             consB, Expr.eabs (Expr.bind [| x |] injD) [TAny, Pos.no_pos] nomark;
           ]
      in

      let matchA = Expr.ematch injA enumT cases nomark in

      Format.printf "before=%a\n"
        (Print.expr_debug ~debug:false)
        (Expr.unbox matchA);
      Format.print_flush ();
      Format.printf "after=%a\n"
        (Print.expr_debug ~debug:false)
        (Expr.unbox (iota_expr (Expr.unbox matchA)));

      [%expect
        {|
    before=match A (x) with
             | A → λ (x: any) → C (x)
             | B → λ (x: any) → D (x) end
    after=C
    (x)
    |}])

let cases_of_list l : ('a, 't) boxed_gexpr EnumConstructor.Map.t =
  EnumConstructor.Map.of_seq
  @@ List.to_seq
  @@ ListLabels.map l ~f:(fun (cons, f) ->
         let var = Var.make "x" in
         ( cons,
           Expr.eabs
             (Expr.bind [| var |] (f var))
             [TAny, Pos.no_pos]
             (Untyped { pos = Pos.no_pos }) ))

let%expect_test _ =
  Cli.call_unstyled (fun _ ->
      let enumT = EnumName.fresh ("t", Pos.no_pos) in
      let consA = EnumConstructor.fresh ("A", Pos.no_pos) in
      let consB = EnumConstructor.fresh ("B", Pos.no_pos) in
      let consC = EnumConstructor.fresh ("C", Pos.no_pos) in
      let consD = EnumConstructor.fresh ("D", Pos.no_pos) in

      let nomark = Untyped { pos = Pos.no_pos } in

      let num n = Expr.elit (LInt (Runtime.integer_of_int n)) nomark in

      let injAe e = Expr.einj e consA enumT nomark in
      let injBe e = Expr.einj e consB enumT nomark in
      let injCe e = Expr.einj e consC enumT nomark in
      let injDe e = Expr.einj e consD enumT nomark in

      (* let injA x = injAe (Expr.evar x nomark) in *)
      let injB x = injBe (Expr.evar x nomark) in
      let injC x = injCe (Expr.evar x nomark) in
      let injD x = injDe (Expr.evar x nomark) in

      let matchA =
        Expr.ematch
          (Expr.ematch (num 1) enumT
             (cases_of_list
                [
                  (consB, fun x -> injBe (injB x));
                  (consA, fun _x -> injAe (num 20));
                ])
             nomark)
          enumT
          (cases_of_list [consA, injC; consB, injD])
          nomark
      in

      Format.printf "before=@[%a@]@."
        (Print.expr_debug ~debug:false)
        (Expr.unbox matchA);
      Format.printf "after=%a@."
        (Print.expr_debug ~debug:false)
        (Expr.unbox (iota2_expr (Expr.unbox matchA)));

      [%expect
        {|
   before=match
            match 1 with
              | A → λ (x: any) → A (20)
              | B → λ (x: any) → B (B (x)) end with
            | A → λ (x: any) → C (x)
            | B → λ (x: any) → D (x) end
   after=match 1 with
           | A → λ (x: any) → C (20)
           | B → λ (x: any) → D (B (x)) end |}])
