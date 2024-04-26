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

type ('a, 'b, 'm) optimizations_ctx = { decl_ctx : decl_ctx }

let all_match_cases_are_id_fun cases n =
  EnumConstructor.Map.for_all
    (fun i case ->
      match Mark.remove case with
      | EAbs { binder; _ } -> (
        let var, body = Bindlib.unmbind binder in
        (* because of invariant [invariant_match], the arity is always one. *)
        let[@warning "-8"] [| var |] = var in
        match Mark.remove body with
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
    cases

let all_match_cases_map_to_same_constructor cases n =
  EnumConstructor.Map.for_all
    (fun i case ->
      match Mark.remove case with
      | EAbs { binder; _ } -> (
        let _, body = Bindlib.unmbind binder in
        match Mark.remove body with
        | EInj { cons = i'; name = n'; _ } ->
          EnumConstructor.equal i i' && EnumName.equal n n'
        | _ -> false)
      | _ -> assert false)
    cases

let binder_vars_used_at_most_once
    (binder :
      ( (('a, 'b) dcalc_lcalc, ('a, 'b) dcalc_lcalc, 'm) base_gexpr,
        (('a, 'b) dcalc_lcalc, 'm) gexpr )
      Bindlib.mbinder) : bool =
  (* fast path: variables not used at all *)
  (not (Array.exists Fun.id (Bindlib.mbinder_occurs binder)))
  ||
  let vars, body = Bindlib.unmbind binder in
  let rec vars_count (e : (('a, 'b) dcalc_lcalc, 'm) gexpr) : int array =
    match e with
    | EVar v, _ ->
      Array.map
        (fun i -> if Bindlib.eq_vars v (Array.get vars i) then 1 else 0)
        (Array.make (Array.length vars) 0)
    | e ->
      Expr.shallow_fold
        (fun e' acc -> Array.map2 (fun x y -> x + y) (vars_count e') acc)
        e
        (Array.make (Array.length vars) 0)
  in
  not (Array.exists (fun c -> c > 1) (vars_count body))

let rec optimize_expr :
    type a b.
    (a, b, 'm) optimizations_ctx ->
    ((a, b) dcalc_lcalc, 'm) gexpr ->
    ((a, b) dcalc_lcalc, 'm) boxed_gexpr =
 fun ctx e ->
  (* We proceed bottom-up, first apply on the subterms *)
  let e = Expr.map ~f:(optimize_expr ctx) ~op:Fun.id e in
  let mark = Mark.get e in
  (* Fixme: when removing enclosing expressions, it would be better if we were
     able to keep the inner position (see the division_by_zero test) *)
  (* Then reduce the parent node (this is applied through Box.apply, therefore
     delayed to unbinding time: no need to be concerned about reboxing) *)
  let reduce (e : ((a, b) dcalc_lcalc, 'm) gexpr) =
    (* Todo: improve the handling of eapp(log,elit) cases here, it obfuscates
       the matches and the log calls are not preserved, which would be a good
       property *)
    match Mark.remove e with
    | EAppOp { op = Not; args = [(ELit (LBool b), _)]; _ } ->
      (* reduction of logical not *)
      ELit (LBool (not b))
    | EAppOp { op = Or; args = [(ELit (LBool b), _); (e, _)]; _ }
    | EAppOp { op = Or; args = [(e, _); (ELit (LBool b), _)]; _ } ->
      (* reduction of logical or *)
      if b then ELit (LBool true) else e
    | EAppOp { op = And; args = [(ELit (LBool b), _); (e, _)]; _ }
    | EAppOp { op = And; args = [(e, _); (ELit (LBool b), _)]; _ } ->
      (* reduction of logical and *)
      if b then e else ELit (LBool false)
    | EMatch { e = EInj { e = e'; cons; name = n' }, _; cases; name = n }
    (* iota-reduction *)
      when EnumName.equal n n' -> (
      (* match E x with | E y -> e1 = e1[y |-> x]*)
      match Mark.remove @@ EnumConstructor.Map.find cons cases with
      (* holds because of invariant_match_inversion *)
      | EAbs { binder; _ } ->
        Mark.remove
          (Bindlib.msubst binder ([e'] |> List.map fst |> Array.of_list))
      | _ -> assert false)
    | EMatch { e = e'; cases; name = n } when all_match_cases_are_id_fun cases n
      ->
      (* iota-reduction when the match is equivalent to an identity function *)
      Mark.remove e'
    | EMatch
        {
          e = EMatch { e = arg; cases = cases1; name = n1 }, _;
          cases = cases2;
          name = n2;
        }
      when EnumName.equal n1 n2
           && all_match_cases_map_to_same_constructor cases1 n1 ->
      (* iota-reduction when the matched expression is itself a match of the
         same enum mapping all constructors to themselves *)
      let cases =
        EnumConstructor.Map.merge
          (fun _i o1 o2 ->
            match o1, o2 with
            | Some b1, Some e2 -> (
              match Mark.remove b1, Mark.remove e2 with
              | EAbs { binder = b1; _ }, EAbs { binder = b2; tys } -> (
                let v1, e1 = Bindlib.unmbind b1 in
                let[@warning "-8"] [| v1 |] = v1 in
                match Mark.remove e1 with
                | EInj { e = e1; _ } ->
                  Some
                    (Expr.unbox
                       (Expr.make_abs [| v1 |]
                          (Expr.rebox
                             (Bindlib.msubst b2
                                ([e1] |> List.map fst |> Array.of_list)))
                          tys (Expr.pos e2)))
                | _ -> assert false)
              | _ -> assert false)
            | _ -> assert false)
          cases1 cases2
      in
      EMatch { e = arg; cases; name = n1 }
    | EApp { f = EAbs { binder; _ }, _; args; _ }
      when binder_vars_used_at_most_once binder
           || List.for_all
                (function (EVar _ | ELit _), _ -> true | _ -> false)
                args ->
      (* beta reduction when variables not used, and for variable aliases and
         literal *)
      Mark.remove (Bindlib.msubst binder (List.map fst args |> Array.of_list))
    | EStructAccess { name; field; e = EStruct { name = name1; fields }, _ }
      when StructName.equal name name1 ->
      Mark.remove (StructField.Map.find field fields)
    | EErrorOnEmpty (EPureDefault (e, _), _) -> e
    | EDefault { excepts; just; cons } -> (
      (* TODO: mechanically prove each of these optimizations correct *)
      let excepts =
        List.filter (fun except -> Mark.remove except <> EEmpty) excepts
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
        let (_ : _ gexpr) =
          Interpreter.evaluate_expr ctx.decl_ctx Global.En
            (* Default language to English, no errors should be raised normally
               so we don't care *)
            e
        in
        assert false
      else
        match excepts, just with
        | [(EDefault { excepts = []; just = ELit (LBool true), _; cons }, _)], _
          ->
          (* No exceptions with condition [true] *)
          Mark.remove cons
        | ( [],
            ( ( ELit (LBool false)
              | EAppOp { op = Log _; args = [(ELit (LBool false), _)]; _ } ),
              _ ) ) ->
          (* No exceptions and condition false *)
          EEmpty
        | ( [except],
            ( ( ELit (LBool false)
              | EAppOp { op = Log _; args = [(ELit (LBool false), _)]; _ } ),
              _ ) ) ->
          (* Single exception and condition false *)
          Mark.remove except
        | excepts, just -> EDefault { excepts; just; cons })
    | EIfThenElse
        {
          cond =
            ( ELit (LBool true), _
            | EAppOp { op = Log _; args = [(ELit (LBool true), _)]; _ }, _ );
          etrue;
          _;
        } ->
      Mark.remove etrue
    | EIfThenElse
        {
          cond =
            ( ( ELit (LBool false)
              | EAppOp { op = Log _; args = [(ELit (LBool false), _)]; _ } ),
              _ );
          efalse;
          _;
        } ->
      Mark.remove efalse
    | EIfThenElse
        {
          cond;
          etrue =
            ( ( ELit (LBool btrue)
              | EAppOp { op = Log _; args = [(ELit (LBool btrue), _)]; _ } ),
              _ );
          efalse =
            ( ( ELit (LBool bfalse)
              | EAppOp { op = Log _; args = [(ELit (LBool bfalse), _)]; _ } ),
              _ );
        } ->
      if btrue && not bfalse then Mark.remove cond
      else if (not btrue) && bfalse then
        EAppOp
          { op = Not; tys = [TLit TBool, Expr.mark_pos mark]; args = [cond] }
        (* note: this last call eliminates the condition & might skip log calls
           as well *)
      else (* btrue = bfalse *) ELit (LBool btrue)
    | EAppOp { op = Op.Fold; args = [_f; init; (EArray [], _)]; _ } ->
      (*reduces a fold with an empty list *)
      Mark.remove init
    | EAppOp
        {
          op = Map;
          args =
            [
              f1;
              ( EAppOp
                  {
                    op = Map;
                    args = [f2; ls];
                    tys = [_; ((TArray xty, _) as lsty)];
                  },
                m2 );
            ];
          tys = [_; (TArray yty, _)];
        } ->
      (* map f (map g l) => map (f o g) l *)
      let fg =
        let v =
          Var.make
            (match f2 with
            | EAbs { binder; _ }, _ -> (Bindlib.mbinder_names binder).(0)
            | _ -> "x")
        in
        let mty m =
          Expr.map_ty (function TArray ty, _ -> ty | _, pos -> TAny, pos) m
        in
        let x = Expr.evar v (mty (Mark.get ls)) in
        Expr.make_abs [| v |]
          (Expr.eapp ~f:(Expr.box f1)
             ~args:[Expr.eapp ~f:(Expr.box f2) ~args:[x] ~tys:[xty] (mty m2)]
             ~tys:[yty] (mty mark))
          [xty] (Expr.pos e)
      in
      let fg = optimize_expr ctx (Expr.unbox fg) in
      let mapl =
        Expr.eappop ~op:Map
          ~args:[fg; Expr.box ls]
          ~tys:[Expr.maybe_ty (Mark.get fg); lsty]
          mark
      in
      Mark.remove (Expr.unbox mapl)
    | EAppOp
        {
          op = Map;
          args =
            [
              f1;
              ( EAppOp
                  {
                    op = Map2;
                    args = [f2; ls1; ls2];
                    tys =
                      [
                        _;
                        ((TArray x1ty, _) as ls1ty);
                        ((TArray x2ty, _) as ls2ty);
                      ];
                  },
                m2 );
            ];
          tys = [_; (TArray yty, _)];
        } ->
      (* map f (map2 g l1 l2) => map2 (f o g) l1 l2 *)
      let fg =
        let v1, v2 =
          match f2 with
          | EAbs { binder; _ }, _ ->
            let names = Bindlib.mbinder_names binder in
            Var.make names.(0), Var.make names.(1)
          | _ -> Var.make "x", Var.make "y"
        in
        let mty m =
          Expr.map_ty (function TArray ty, _ -> ty | _, pos -> TAny, pos) m
        in
        let x1 = Expr.evar v1 (mty (Mark.get ls1)) in
        let x2 = Expr.evar v2 (mty (Mark.get ls2)) in
        Expr.make_abs [| v1; v2 |]
          (Expr.eapp ~f:(Expr.box f1)
             ~args:
               [
                 Expr.eapp ~f:(Expr.box f2) ~args:[x1; x2] ~tys:[x1ty; x2ty]
                   (mty m2);
               ]
             ~tys:[yty] (mty mark))
          [x1ty; x2ty] (Expr.pos e)
      in
      let fg = optimize_expr ctx (Expr.unbox fg) in
      let mapl =
        Expr.eappop ~op:Map2
          ~args:[fg; Expr.box ls1; Expr.box ls2]
          ~tys:[Expr.maybe_ty (Mark.get fg); ls1ty; ls2ty]
          mark
      in
      Mark.remove (Expr.unbox mapl)
    | EAppOp
        {
          op = Op.Fold;
          args = [f; init; (EArray [e'], _)];
          tys = [_; tinit; (TArray tx, _)];
        } ->
      (* reduces a fold with one element *)
      EApp { f; args = [init; e']; tys = [tinit; tx] }
    | ETuple ((ETupleAccess { e; index = 0; _ }, _) :: el)
      when List.for_all Fun.id
             (List.mapi
                (fun i -> function
                  | ETupleAccess { e = en; index; _ }, _ ->
                    index = i + 1 && Expr.equal en e
                  | _ -> false)
                el) ->
      (* identity tuple reconstruction *)
      Mark.remove e
    | ECatchEmpty { body; handler } -> (
      (* peephole exception catching reductions *)
      match Mark.remove body, Mark.remove handler with
      | ERaiseEmpty, _ -> Mark.remove handler
      | _, ERaiseEmpty -> Mark.remove body
      | _ -> ECatchEmpty { body; handler })
    | e -> e
  in
  Expr.Box.app1 e reduce mark

let optimize_expr :
      'm.
      decl_ctx ->
      (('a, 'b) dcalc_lcalc, 'm) gexpr ->
      (('a, 'b) dcalc_lcalc, 'm) boxed_gexpr =
 fun (decl_ctx : decl_ctx) (e : (('a, 'b) dcalc_lcalc, 'm) gexpr) ->
  optimize_expr { decl_ctx } e

let optimize_program (p : 'm program) : 'm program =
  Program.map_exprs ~f:(optimize_expr p.decl_ctx) ~varf:(fun v -> v) p

let test_iota_reduction_1 () =
  let x = Var.make "x" in
  let enumT = EnumName.fresh [] ("t", Pos.no_pos) in
  let consA = EnumConstructor.fresh ("A", Pos.no_pos) in
  let consB = EnumConstructor.fresh ("B", Pos.no_pos) in
  let consC = EnumConstructor.fresh ("C", Pos.no_pos) in
  let consD = EnumConstructor.fresh ("D", Pos.no_pos) in
  let nomark = Untyped { pos = Pos.no_pos } in
  let injA = Expr.einj ~e:(Expr.evar x nomark) ~cons:consA ~name:enumT nomark in
  let injC = Expr.einj ~e:(Expr.evar x nomark) ~cons:consC ~name:enumT nomark in
  let injD = Expr.einj ~e:(Expr.evar x nomark) ~cons:consD ~name:enumT nomark in
  let cases : ('a, 't) boxed_gexpr EnumConstructor.Map.t =
    EnumConstructor.Map.of_list
      [
        consA, Expr.eabs (Expr.bind [| x |] injC) [TAny, Pos.no_pos] nomark;
        consB, Expr.eabs (Expr.bind [| x |] injD) [TAny, Pos.no_pos] nomark;
      ]
  in
  let matchA = Expr.ematch ~e:injA ~name:enumT ~cases nomark in
  Alcotest.(check string)
    "same string"
    begin[@ocamlformat "disable"]
      "before=match (A x) with\n\
      \       | A x → C x\n\
      \       | B x → D x\n\
       after=C x"
    end
    (Format.asprintf "before=%a\nafter=%a" Expr.format (Expr.unbox matchA)
       Expr.format
       (Expr.unbox (optimize_expr Program.empty_ctx (Expr.unbox matchA))))

let cases_of_list l : ('a, 't) boxed_gexpr EnumConstructor.Map.t =
  EnumConstructor.Map.of_list
  @@ ListLabels.map l ~f:(fun (cons, f) ->
         let var = Var.make "x" in
         ( cons,
           Expr.eabs
             (Expr.bind [| var |] (f var))
             [TAny, Pos.no_pos]
             (Untyped { pos = Pos.no_pos }) ))

let test_iota_reduction_2 () =
  let enumT = EnumName.fresh [] ("t", Pos.no_pos) in
  let consA = EnumConstructor.fresh ("A", Pos.no_pos) in
  let consB = EnumConstructor.fresh ("B", Pos.no_pos) in
  let consC = EnumConstructor.fresh ("C", Pos.no_pos) in
  let consD = EnumConstructor.fresh ("D", Pos.no_pos) in

  let nomark = Untyped { pos = Pos.no_pos } in

  let num n = Expr.elit (LInt (Runtime.integer_of_int n)) nomark in

  let injAe e = Expr.einj ~e ~cons:consA ~name:enumT nomark in
  let injBe e = Expr.einj ~e ~cons:consB ~name:enumT nomark in
  let injCe e = Expr.einj ~e ~cons:consC ~name:enumT nomark in
  let injDe e = Expr.einj ~e ~cons:consD ~name:enumT nomark in

  (* let injA x = injAe (Expr.evar x nomark) in *)
  let injB x = injBe (Expr.evar x nomark) in
  let injC x = injCe (Expr.evar x nomark) in
  let injD x = injDe (Expr.evar x nomark) in

  let matchA =
    Expr.ematch
      ~e:
        (Expr.ematch ~e:(num 1) ~name:enumT
           ~cases:
             (cases_of_list
                [
                  (consB, fun x -> injBe (injB x));
                  (consA, fun _x -> injAe (num 20));
                ])
           nomark)
      ~name:enumT
      ~cases:(cases_of_list [consA, injC; consB, injD])
      nomark
  in
  Alcotest.(check string)
    "same string "
    begin[@ocamlformat "disable"]
      "before=match (match 1 with\n\
      \              | A x → A 20\n\
      \              | B x → B (B x)) with\n\
      \       | A x → C x\n\
      \       | B x → D x\n\
       after=match 1 with\n\
      \      | A x → C 20\n\
      \      | B x → D (B x)\n"
    end
    (Format.asprintf "before=@[%a@]@.after=%a@." Expr.format (Expr.unbox matchA)
       Expr.format
       (Expr.unbox (optimize_expr Program.empty_ctx (Expr.unbox matchA))))
