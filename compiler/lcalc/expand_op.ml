(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

let rec resolve_eq ctx pos ty args m =
  let conjunction = function
    | [] -> Expr.elit (LBool true) m
    | e0 :: el ->
      List.fold_left
        (fun acc e ->
          Expr.eappop ~op:(And, pos) ~args:[acc; e]
            ~tys:[TLit TBool, pos; TLit TBool, pos]
            m)
        e0 el
  in
  match Mark.remove ty with
  | TArrow _ | TClosureEnv -> Message.error "Invalid comparison of functions"
  | TLit TUnit -> Expr.elit (LBool true) m
  | TLit TBool -> Expr.eappop ~op:(Eq_boo_boo, pos) ~args ~tys:[ty; ty] m
  | TLit TInt -> Expr.eappop ~op:(Eq_int_int, pos) ~args ~tys:[ty; ty] m
  | TLit TRat -> Expr.eappop ~op:(Eq_rat_rat, pos) ~args ~tys:[ty; ty] m
  | TLit TMoney -> Expr.eappop ~op:(Eq_mon_mon, pos) ~args ~tys:[ty; ty] m
  | TLit TDuration -> Expr.eappop ~op:(Eq_dur_dur, pos) ~args ~tys:[ty; ty] m
  | TLit TDate -> Expr.eappop ~op:(Eq_dat_dat, pos) ~args ~tys:[ty; ty] m
  | TLit TPos -> Expr.elit (LBool true) m
  | TTuple tys ->
    let size = List.length tys in
    let eqs =
      List.mapi
        (fun i ty ->
          resolve_eq ctx pos ty
            (List.map (fun e -> Expr.make_tupleaccess e i size pos) args)
            m)
        tys
    in
    conjunction eqs
  | TStruct name ->
    let fields = StructName.Map.find name ctx.ctx_structs in
    let eqs =
      List.rev
      @@ StructField.Map.fold
           (fun field ty acc ->
             resolve_eq ctx pos ty
               (List.map
                  (fun e ->
                    Expr.estructaccess ~name ~field ~e (Expr.with_ty m ty))
                  args)
               m
             :: acc)
           fields []
    in
    conjunction eqs
  | TEnum name ->
    (* FIXME: this is terrible (quadratic in size in the number of variants) ;
       but we need a new operator or specific backend constructs to be able to
       do better, matching is the only possible way to deconstruct an enum at
       the moment *)
    let arg1, arg2 =
      match args with [arg1; arg2] -> arg1, arg2 | _ -> assert false
    in
    let constrs = EnumName.Map.find name ctx.ctx_enums in
    let cases =
      EnumConstructor.Map.mapi
        (fun cstr ty ->
          let v1 = Var.make "v1" in
          let cases =
            EnumConstructor.Map.mapi
              (fun cstr2 ty ->
                if EnumConstructor.equal cstr cstr2 then
                  let v2 = Var.make "v2" in
                  Expr.make_ghost_abs [v2]
                    (resolve_eq ctx pos ty
                       [
                         Expr.evar v1 (Expr.with_ty m ty);
                         Expr.evar v2 (Expr.with_ty m ty);
                       ]
                       m)
                    [ty] pos
                else
                  Expr.make_ghost_abs
                    [Var.make "_"]
                    (Expr.elit (LBool false) m)
                    [ty] pos)
              constrs
          in
          Expr.make_ghost_abs [v1] (Expr.ematch ~name ~e:arg2 ~cases m) [ty] pos)
        constrs
    in
    Expr.ematch ~name ~e:arg1 ~cases m
  | TArray ty1 ->
    let tbool = TLit TBool, pos in
    let vargs =
      List.mapi (fun i _ -> Var.make ("l" ^ string_of_int (i + 1))) args
    in
    let vargs_e =
      List.map2 (fun v a -> Expr.make_var v (Mark.get a)) vargs args
    in
    let same_length =
      resolve_eq ctx pos (TLit TInt, pos)
        (List.map
           (fun e ->
             Expr.eappop ~op:(Length, pos) ~args:[e] ~tys:[ty]
               (Expr.with_ty m (TLit TInt, pos)))
           vargs_e)
        m
    in
    let map2_f =
      let x = Var.make "x" in
      let y = Var.make "y" in
      Expr.make_ghost_abs [x; y]
        (resolve_eq ctx pos ty1
           [Expr.evar x (Expr.with_ty m ty1); Expr.evar y (Expr.with_ty m ty1)]
           m)
        [ty1; ty1] pos
    in
    let fold_f =
      let acc = Var.make "acc" in
      let x = Var.make "x" in
      Expr.make_ghost_abs [acc; x]
        (conjunction [Expr.evar acc m; Expr.evar x m])
        [tbool; tbool] pos
    in
    let bool_list =
      Expr.eappop ~op:(Map2, pos) ~args:(map2_f :: vargs_e)
        ~tys:[TArrow ([ty1; ty1], tbool), pos; TArray ty1, pos; TArray ty1, pos]
        (Expr.with_ty m (TArray tbool, pos))
    in
    let same_elements =
      Expr.eappop ~op:(Fold, pos)
        ~args:[fold_f; Expr.elit (LBool true) m; bool_list]
        ~tys:[TArrow ([tbool; tbool], tbool), pos; tbool; TArray tbool, pos]
        m
    in
    Expr.make_multiple_let_in
      (List.map (fun v -> v, Pos.void) vargs)
      [ty; ty] args
      (Expr.eappop ~op:(And, pos)
         ~args:[same_length; same_elements]
         ~tys:[tbool; tbool] m)
      pos
  | TOption ty ->
    let arg1, arg2 =
      match args with [arg1; arg2] -> arg1, arg2 | _ -> assert false
    in
    Expr.ematch ~name:Expr.option_enum ~e:arg1 m
      ~cases:
        (EnumConstructor.Map.of_list
           [
             ( Expr.none_constr,
               Expr.thunk_term
                 (Expr.ematch ~name:Expr.option_enum ~e:arg2 m
                    ~cases:
                      (EnumConstructor.Map.of_list
                         [
                           ( Expr.none_constr,
                             Expr.thunk_term (Expr.elit (LBool true) m) );
                           ( Expr.some_constr,
                             Expr.make_ghost_abs
                               [Var.make "_"]
                               (Expr.elit (LBool false) m)
                               [ty] pos );
                         ])) );
             ( Expr.some_constr,
               let v1 = Var.make "x" in
               let v2 = Var.make "y" in
               Expr.make_ghost_abs [v1]
                 (Expr.ematch ~name:Expr.option_enum ~e:arg2 m
                    ~cases:
                      (EnumConstructor.Map.of_list
                         [
                           ( Expr.none_constr,
                             Expr.thunk_term (Expr.elit (LBool false) m) );
                           ( Expr.some_constr,
                             Expr.make_ghost_abs [v2]
                               (resolve_eq ctx pos ty
                                  [
                                    Expr.evar v1 (Expr.with_ty m ty);
                                    Expr.evar v2 (Expr.with_ty m ty);
                                  ]
                                  m)
                               [ty] pos );
                         ]))
                 [ty] pos );
           ])
  | TDefault _ -> assert false
  | TVar _ | TForAll _ ->
    Message.error ~internal:true "Unknown type for equality resolution"

let rec expr ctx = function
  | EAppOp { op = Eq, pos; args; tys = [ty; ty2] }, m ->
    assert (Type.equal ty ty2);
    let args = List.map (expr ctx) args in
    resolve_eq ctx pos ty args m
  | e -> Expr.map ~f:(expr ctx) ~op:Fun.id e

let program p = Program.map_exprs ~varf:Fun.id ~f:(expr p.decl_ctx) p
