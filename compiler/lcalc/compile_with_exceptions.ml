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
module D = Dcalc.Ast
module A = Ast

let rec translate_typ (tau : typ) : typ =
  Mark.map
    (function
      | TDefault t -> Mark.remove (translate_typ t)
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ ->
        Message.error ~internal:true
          "The types option should not appear before the dcalc -> lcalc \
           translation step."
      | TClosureEnv ->
        Message.error ~internal:true
          "The types closure_env should not appear before the dcalc -> lcalc \
           translation step."
      | TAny -> TAny
      | TArray ts -> TArray (translate_typ ts)
      | TArrow (t1, t2) -> TArrow (List.map translate_typ t1, translate_typ t2))
    tau

let translate_mark m = Expr.map_ty translate_typ m

let rec translate_default
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr boxed =
  let pos = Expr.mark_pos mark_default in
  let exceptions =
    List.map (fun except -> Expr.thunk_term (translate_expr except)) exceptions
  in
  Expr.eappop
    ~op:(Op.HandleDefault, Expr.pos cons)
    ~tys:
      [
        TArray (TArrow ([TLit TUnit, pos], (TAny, pos)), pos), pos;
        TArrow ([TLit TUnit, pos], (TLit TBool, pos)), pos;
        TArrow ([TLit TUnit, pos], (TAny, pos)), pos;
      ]
    ~args:
      [
        Expr.earray exceptions
          (Expr.map_ty
             (fun ty -> TArray (TArrow ([TLit TUnit, pos], ty), pos), pos)
             mark_default);
        Expr.thunk_term (translate_expr just);
        Expr.thunk_term (translate_expr cons);
      ]
    mark_default

and translate_expr (e : 'm D.expr) : 'm A.expr boxed =
  match e with
  | EEmpty, m -> Expr.eraiseempty (translate_mark m)
  | EErrorOnEmpty arg, m ->
    let m = translate_mark m in
    Expr.ecatchempty (translate_expr arg) (Expr.efatalerror Runtime.NoValue m) m
  | EDefault { excepts; just; cons }, m ->
    translate_default excepts just cons (translate_mark m)
  | EPureDefault e, _ -> translate_expr e
  | EAppOp { op; args; tys }, m ->
    Expr.eappop ~op:(Operator.translate op)
      ~args:(List.map translate_expr args)
      ~tys:(List.map translate_typ tys)
      (translate_mark m)
  | ( ( ELit _ | EArray _ | EVar _ | EAbs _ | EApp _ | EExternal _
      | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EAssert _
      | EFatalError _ | EStruct _ | EStructAccess _ | EMatch _ ),
      _ ) as e ->
    Expr.map ~f:translate_expr ~typ:translate_typ e
  | _ -> .

let translate_program (prg : 'm D.program) : 'm A.program =
  Program.map_exprs prg ~typ:translate_typ ~varf:Var.translate ~f:translate_expr
