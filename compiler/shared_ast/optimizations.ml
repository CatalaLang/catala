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
open Catala_utils
open Definitions

type ('a, 'b, 'm) optimizations_ctx = {
  var_values :
    ( (('a, 'b) dcalc_lcalc, 'm mark) gexpr,
      (('a, 'b) dcalc_lcalc, 'm mark) gexpr )
    Var.Map.t;
  decl_ctx : decl_ctx;
}

let all_match_cases_are_id_fun cases n =
  EnumConstructor.MapLabels.for_all cases ~f:(fun i case ->
      match Marked.unmark case with
      | EAbs { binder; _ } -> (
        let var, body = Bindlib.unmbind binder in
        (* because of invariant [invariant_match], the arity is always one. *)
        let[@warning "-8"] [| var |] = var in
        match Marked.unmark body with
        | EInj { cons = i'; name = n'; e = EVar x, _ } ->
          EnumConstructor.equal i i'
          && EnumName.equal n n'
          && Bindlib.eq_vars x var
        | EInj { cons = i'; name = n'; e = ELit LUnit, _ } ->
          (* since unit is the only value of type unit. We don't need to check
             the equality. *)
          EnumConstructor.equal i i' && EnumName.equal n n'
        | _ -> false)
      | _ ->
        (* because of invariant [invariant_match], there is always some EAbs in
           each cases. *)
        assert false)

let all_match_cases_map_to_same_constructor cases n =
  EnumConstructor.MapLabels.for_all cases ~f:(fun i case ->
      match Marked.unmark case with
      | EAbs { binder; _ } -> (
        let _, body = Bindlib.unmbind binder in
        match Marked.unmark body with
        | EInj { cons = i'; name = n'; _ } ->
          EnumConstructor.equal i i' && EnumName.equal n n'
        | _ -> false)
      | _ -> assert false)

let rec optimize_expr :
    type a b.
    (a, b, 'm) optimizations_ctx ->
    ((a, b) dcalc_lcalc, 'm mark) gexpr ->
    ((a, b) dcalc_lcalc, 'm mark) boxed_gexpr =
 fun ctx e ->
  (* We proceed bottom-up, first apply on the subterms *)
  let e = Expr.map ~f:(optimize_expr ctx) e in
  let mark = Marked.get_mark e in
  (* Then reduce the parent node *)
  let reduce (e : ((a, b) dcalc_lcalc, 'm mark) gexpr) =
    (* Todo: improve the handling of eapp(log,elit) cases here, it obfuscates
       the matches and the log calls are not preserved, which would be a good
       property *)
    match Marked.unmark e with
    | EApp
        {
          f =
            ( EOp { op = Not; _ }, _
            | ( EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(EOp { op = Not; _ }, _)];
                  },
                _ ) ) as op;
          args = [e1];
        } -> (
      (* reduction of logical not *)
      match e1 with
      | ELit (LBool false), _ -> ELit (LBool true)
      | ELit (LBool true), _ -> ELit (LBool false)
      | e1 -> EApp { f = op; args = [e1] })
    | EApp
        {
          f =
            ( EOp { op = Or; _ }, _
            | ( EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(EOp { op = Or; _ }, _)];
                  },
                _ ) ) as op;
          args = [e1; e2];
        } -> (
      (* reduction of logical or *)
      match e1, e2 with
      | (ELit (LBool false), _), new_e | new_e, (ELit (LBool false), _) ->
        Marked.unmark new_e
      | (ELit (LBool true), _), _ | _, (ELit (LBool true), _) ->
        ELit (LBool true)
      | _ -> EApp { f = op; args = [e1; e2] })
    | EApp
        {
          f =
            ( EOp { op = And; _ }, _
            | ( EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(EOp { op = And; _ }, _)];
                  },
                _ ) ) as op;
          args = [e1; e2];
        } -> (
      (* reduction of logical and *)
      match e1, e2 with
      | (ELit (LBool true), _), new_e | new_e, (ELit (LBool true), _) ->
        Marked.unmark new_e
      | (ELit (LBool false), _), _ | _, (ELit (LBool false), _) ->
        ELit (LBool false)
      | _ -> EApp { f = op; args = [e1; e2] })
    | EMatch { e = EInj { e = e'; cons; name = n' }, _; cases; name = n }
    (* iota-reduction *)
      when EnumName.equal n n' -> (
      (* match E x with | E y -> e1 = e1[y |-> x]*)
      match Marked.unmark @@ EnumConstructor.Map.find cons cases with
      (* holds because of invariant_match_inversion *)
      | EAbs { binder; _ } ->
        Marked.unmark
          (Bindlib.msubst binder ([e'] |> List.map fst |> Array.of_list))
      | _ -> assert false)
    | EMatch { e = e'; cases; name = n } when all_match_cases_are_id_fun cases n
      ->
      (* iota-reduction when the match is equivalent to an identity function *)
      Marked.unmark e'
    | EMatch
        {
          e = EMatch { e = arg; cases = cases1; name = n1 }, _;
          cases = cases2;
          name = n2;
        }
      when false
           (* TODO: this case is buggy because of the box/unbox manipulation, it
              should be fixed before removing this [false] value*)
           && n1 = n2
           && all_match_cases_map_to_same_constructor cases1 n1 ->
      (* iota-reduction when the matched expression is itself a match of the
         same enum mapping all constructors to themselves *)
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
                    (Expr.unbox
                       (Expr.make_abs [| v1 |]
                          (Expr.box
                             (Bindlib.msubst b2
                                ([e1] |> List.map fst |> Array.of_list)))
                          tys (Expr.pos e2)))
                | _ -> assert false)
              | _ -> assert false)
            | _ -> assert false)
      in
      EMatch { e = arg; cases; name = n1 }
    | EApp { f = EAbs { binder; _ }, _; args } ->
      (* beta reduction *)
      Marked.unmark (Bindlib.msubst binder (List.map fst args |> Array.of_list))
    | EStructAccess { name; field; e = EStruct { name = name1; fields }, _ }
      when name = name1 ->
      Marked.unmark (StructField.Map.find field fields)
    | EDefault { excepts; just; cons } -> (
      (* TODO: mechanically prove each of these optimizations correct :) *)
      let excepts =
        List.filter (fun except -> Marked.unmark except <> EEmptyError) excepts
        (* we can discard the exceptions that are always empty error *)
      in
      let value_except_count =
        List.fold_left
          (fun nb except -> if Expr.is_value except then nb + 1 else nb)
          0 excepts
      in
      if value_except_count > 1 then
        (* at this point we know a conflict error will be triggered so we just
           feed the expression to the interpreter that will print the beautiful
           right error message *)
        Marked.unmark (Interpreter.evaluate_expr ctx.decl_ctx e)
      else
        match excepts, just with
        | [except], _ when Expr.is_value except ->
          (* if there is only one exception and it is a non-empty value it is
             always chosen *)
          Marked.unmark except
        | ( [],
            ( ( ELit (LBool true)
              | EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool true), _)];
                  } ),
              _ ) ) ->
          Marked.unmark cons
        | ( [],
            ( ( ELit (LBool false)
              | EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool false), _)];
                  } ),
              _ ) ) ->
          EEmptyError
        | excepts, just -> EDefault { excepts; just; cons })
    | EIfThenElse
        {
          cond =
            ( ELit (LBool true), _
            | ( EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool true), _)];
                  },
                _ ) );
          etrue;
          _;
        } ->
      Marked.unmark etrue
    | EIfThenElse
        {
          cond =
            ( ( ELit (LBool false)
              | EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool false), _)];
                  } ),
              _ );
          efalse;
          _;
        } ->
      Marked.unmark efalse
    | EIfThenElse
        {
          cond;
          etrue =
            ( ( ELit (LBool btrue)
              | EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool btrue), _)];
                  } ),
              _ );
          efalse =
            ( ( ELit (LBool bfalse)
              | EApp
                  {
                    f = EOp { op = Log _; _ }, _;
                    args = [(ELit (LBool bfalse), _)];
                  } ),
              _ );
        } ->
      if btrue && not bfalse then Marked.unmark cond
      else if (not btrue) && bfalse then
        EApp
          {
            f = EOp { op = Not; tys = [TLit TBool, Expr.mark_pos mark] }, mark;
            args = [cond];
          }
        (* note: this last call eliminates the condition & might skip log calls
           as well *)
      else (* btrue = bfalse *) ELit (LBool btrue)
    | EApp { f = EOp { op = Op.Fold; _ }, _; args = [_f; init; (EArray [], _)] }
      ->
      (*reduces a fold with an empty list *)
      Marked.unmark init
    | EApp
        { f = EOp { op = Op.Fold; _ }, _; args = [f; init; (EArray [e'], _)] }
      ->
      (* reduces a fold with one element *)
      EApp { f; args = [init; e'] }
    | ECatch { body; exn; handler } -> (
      (* peephole exception catching reductions *)
      match Marked.unmark body, Marked.unmark handler with
      | ERaise exn', ERaise exn'' when exn' = exn && exn = exn'' -> ERaise exn
      | ERaise exn', _ when exn' = exn -> Marked.unmark handler
      | _, ERaise exn' when exn' = exn -> Marked.unmark body
      | _ -> ECatch { body; exn; handler })
    | e -> e
  in
  Expr.Box.app1 e reduce mark

let optimize_expr
    (decl_ctx : decl_ctx)
    (e : (('a, 'b) dcalc_lcalc, 'm mark) gexpr) =
  optimize_expr { var_values = Var.Map.empty; decl_ctx } e

let optimize_program (p : 'm program) : 'm program =
  Bindlib.unbox
    (Program.map_exprs ~f:(optimize_expr p.decl_ctx) ~varf:(fun v -> v) p)

let test_iota_reduction_1 () =
  Cli.call_unstyled (fun _ ->
      let x = Var.make "x" in
      let enumT = EnumName.fresh ("t", Pos.no_pos) in
      let consA = EnumConstructor.fresh ("A", Pos.no_pos) in
      let consB = EnumConstructor.fresh ("B", Pos.no_pos) in
      let consC = EnumConstructor.fresh ("C", Pos.no_pos) in
      let consD = EnumConstructor.fresh ("D", Pos.no_pos) in
      let nomark = Untyped { pos = Pos.no_pos } in
      let injA = Expr.einj (Expr.evar x nomark) consA enumT nomark in
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
      Alcotest.(check string)
        "same string"
        "before=match (A x)\n\
        \       with\n\
        \       | A → (λ (x: any) → C x)\n\
        \       | B → (λ (x: any) → D x)\n\
         after=C\n\
         x"
        (Format.asprintf "before=%a\nafter=%a"
           (Print.expr_debug ~debug:false)
           (Expr.unbox matchA)
           (Print.expr_debug ~debug:false)
           (Expr.unbox
              (optimize_expr
                 {
                   ctx_enums = EnumName.Map.empty;
                   ctx_structs = StructName.Map.empty;
                   ctx_struct_fields = IdentName.Map.empty;
                   ctx_scopes = ScopeName.Map.empty;
                 }
                 (Expr.unbox matchA)))))

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

let test_iota_reduction_2 () =
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
      Alcotest.(check string)
        "same string "
        "before=match\n\
        \         (match 1\n\
        \          with\n\
        \          | A → (λ (x: any) → A 20)\n\
        \          | B → (λ (x: any) → B B x))\n\
        \       with\n\
        \       | A → (λ (x: any) → C x)\n\
        \       | B → (λ (x: any) → D x)\n\
         after=match 1\n\
        \      with\n\
        \      | A → (λ (x: any) → C 20)\n\
        \      | B → (λ (x: any) → D B x)\n"
        (Format.asprintf "before=@[%a@]@.after=%a@."
           (Print.expr_debug ~debug:false)
           (Expr.unbox matchA)
           (Print.expr_debug ~debug:false)
           (Expr.unbox
              (optimize_expr
                 {
                   ctx_enums = EnumName.Map.empty;
                   ctx_structs = StructName.Map.empty;
                   ctx_struct_fields = IdentName.Map.empty;
                   ctx_scopes = ScopeName.Map.empty;
                 }
                 (Expr.unbox matchA)))))
