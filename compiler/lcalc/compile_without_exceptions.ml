(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Alain Delaët-Tixeuil <alain.delaet--tixeuil@inria.fr>

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

(** We make use of the strong invriants on the structure of programs:
    Defaultable values can only appear in certin positions. This information is
    given by the type structure of expressions. In particular this mean we don't
    need to use the monadic bind while computing arithmetic opertions or
    function calls. The resulting function is not more difficult than what we
    had when translating without exceptions.

    The typing translation is to simply trnsform defult type into option types. *)

let rec translate_typ (tau : typ) : typ =
  Mark.copy tau
    begin
      match Mark.remove tau with
      | TDefault t -> TOption (translate_typ t)
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ ->
        Message.raise_internal_error
          "The types option should not appear before the dcalc -> lcalc \
           translation step."
      | TClosureEnv ->
        Message.raise_internal_error
          "The types closure_env should not appear before the dcalc -> lcalc \
           translation step."
      | TAny -> TAny
      | TArray ts -> TArray (translate_typ ts)
      | TArrow (t1, t2) -> TArrow (List.map translate_typ t1, translate_typ t2)
    end

let translate_mark m = Expr.map_ty translate_typ m

let rec translate_default
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr boxed =
  (* Since the program is well typed, all exceptions have as type [option 't] *)
  let pos = Expr.mark_pos mark_default in
  let exceptions = List.map translate_expr exceptions in
  let exceptions_and_cons_ty = Expr.maybe_ty mark_default in
  Expr.eappop ~op:Op.HandleDefaultOpt
    ~tys:
      [
        TArray exceptions_and_cons_ty, pos;
        TArrow ([TLit TUnit, pos], (TLit TBool, pos)), pos;
        TArrow ([TLit TUnit, pos], exceptions_and_cons_ty), pos;
      ]
    ~args:
      [
        Expr.earray exceptions
          (Expr.map_ty (fun ty -> TArray ty, pos) mark_default);
        (* In call-by-value programming languages, as lcalc, arguments are
           evalulated before calling the function. Since we don't want to
           execute the justification and conclusion while before checking every
           exceptions, we need to thunk them. *)
        Expr.thunk_term (translate_expr just);
        Expr.thunk_term (translate_expr cons);
      ]
    mark_default

and translate_expr (e : 'm D.expr) : 'm A.expr boxed =
  match e with
  | EEmptyError, m ->
    let m = translate_mark m in
    let pos = Expr.mark_pos m in
    Expr.einj
      ~e:(Expr.elit LUnit (Expr.with_ty m (TLit TUnit, pos)))
      ~cons:Expr.none_constr ~name:Expr.option_enum m
  | EErrorOnEmpty arg, m ->
    let m = translate_mark m in
    let pos = Expr.mark_pos m in
    let cases =
      EnumConstructor.Map.of_list
        [
          ( Expr.none_constr,
            let x = Var.make "_" in
            Expr.make_abs [| x |]
              (Expr.eraise NoValueProvided m)
              [TAny, pos]
              pos );
          (* | None x -> raise NoValueProvided *)
          Expr.some_constr, Expr.fun_id ~var_name:"arg" m (* | Some x -> x *);
        ]
    in
    Expr.ematch ~e:(translate_expr arg) ~name:Expr.option_enum ~cases m
  | EDefault { excepts; just; cons }, m ->
    translate_default excepts just cons (translate_mark m)
  | EPureDefault e, m ->
    Expr.einj ~e:(translate_expr e) ~cons:Expr.some_constr
      ~name:Expr.option_enum (translate_mark m)
  | EAppOp { op; tys; args }, m ->
    Expr.eappop ~op:(Operator.translate op)
      ~tys:(List.map translate_typ tys)
      ~args:(List.map translate_expr args)
      (translate_mark m)
  | ( ( ELit _ | EArray _ | EVar _ | EApp _ | EAbs _ | EExternal _
      | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EAssert _
      | EStruct _ | EStructAccess _ | EMatch _ ),
      _ ) as e ->
    Expr.map ~f:translate_expr ~typ:translate_typ e
  | _ -> .

let translate_program (prg : 'm D.program) : 'm A.program =
  Program.map_exprs prg ~typ:translate_typ ~varf:Var.translate ~f:translate_expr
