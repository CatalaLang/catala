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

(** We make use of the strong invariants on the structure of programs:
    Defaultable values can only appear in certain positions. This information is
    given by the type structure of expressions. In particular this mean we don't
    need to use the monadic bind while computing arithmetic operations or
    function calls. The resulting function is not more difficult than what we
    had when translating without exceptions.

    The typing translation is to simply transform default type into option
    types. *)

let translate_typ (tau : typ) : typ =
  let rec aux = function
    | TDefault ((_, pos2) as t), pos1 ->
      Bindlib.box_apply
        (fun t -> TOption (TTuple [t; TLit TPos, pos2], pos2), pos1)
        (aux t)
    | TClosureEnv, pos ->
      Message.error ~internal:true ~pos
        "The type closure_env should not appear before the dcalc -> lcalc \
         translation step."
    | t -> Type.map aux t
  in
  Bindlib.unbox (aux tau)

let translate_mark m = Expr.map_ty translate_typ m

let rec translate_default
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr boxed =
  (* Since the program is well typed, all exceptions have as type [option ('t *
     pos)] *)
  let pos = Expr.mark_pos mark_default in
  let ty_option = Expr.maybe_ty mark_default in
  let ty_array = TArray ty_option, pos in
  let ty_alpha =
    match ty_option with
    | TOption ty, _ -> ty
    | (TVar _, _) as ty -> ty
    | _ -> assert false
  in
  let mark_alpha = Expr.with_ty mark_default ty_alpha in
  let if_just_then_cons =
    let none =
      Expr.einj ~cons:Expr.none_constr ~name:Expr.option_enum ~e:None
        mark_default
    in
    match just with
    | ELit (LBool b), _ -> if b then translate_expr cons else none
    | just ->
      Expr.eifthenelse (translate_expr just) (translate_expr cons)
        (Expr.einj ~e:None ~cons:Expr.none_constr ~name:Expr.option_enum
           mark_default)
        mark_default
  in
  let match_some e =
    match just with
    | ELit (LBool false), _ ->
      (* in this case we can just forward the option in the argument *)
      e
    | _ ->
      Expr.ematch ~name:Expr.option_enum ~e
        ~cases:
          (EnumConstructor.Map.of_list
             [
               (* Some x -> Some x *)
               ( Expr.some_constr,
                 let x = Var.make "x" in
                 Expr.make_ghost_abs [x]
                   (Expr.einj ~name:Expr.option_enum ~cons:Expr.some_constr
                      ~e:(Some (Expr.evar x mark_alpha))
                      mark_default)
                   [ty_alpha] pos );
               (* None -> if just then cons else None *)
               Expr.none_constr, Expr.thunk_term if_just_then_cons;
             ])
        mark_default
  in
  match exceptions with
  | [] -> if_just_then_cons
  | [((EInj { cons; _ }, _) as e)] ->
    if EnumConstructor.equal cons Expr.none_constr then
      Expr.thunk_term if_just_then_cons
    else if EnumConstructor.equal cons Expr.some_constr then translate_expr e
    else assert false
  | [single_exception] -> match_some (translate_expr single_exception)
  | exceptions ->
    let exceptions = List.map translate_expr exceptions in
    match_some
      (Expr.eappop
         ~op:(Op.HandleExceptions, Expr.pos cons)
         ~tys:[ty_array]
         ~args:[Expr.earray exceptions (Expr.with_ty mark_default ty_array)]
         mark_default)

and translate_expr (e : 'm D.expr) : 'm A.expr boxed =
  match e with
  | EEmpty, m ->
    let m = translate_mark m in
    Expr.einj ~e:None ~cons:Expr.none_constr ~name:Expr.option_enum m
  | EErrorOnEmpty arg, m ->
    let m = translate_mark m in
    let pos = Expr.mark_pos m in
    let m_pos_pair =
      Expr.map_ty (fun ty -> TTuple [ty; TLit TPos, pos], pos) m
    in
    let cases =
      EnumConstructor.Map.of_list
        [
          ( Expr.none_constr,
            let x = Var.make "_" in
            Expr.make_ghost_abs [x]
              (Expr.efatalerror NoValue m)
              [TLit TUnit, pos]
              pos );
          (* | None x -> raise NoValueProvided *)
          ( Expr.some_constr,
            let var = Var.make "arg" in
            Expr.make_abs
              [var, pos]
              (Expr.make_tupleaccess (Expr.evar var m_pos_pair) 0 2 pos)
              [Expr.maybe_ty m_pos_pair]
              pos );
        ]
    in
    Expr.ematch ~e:(translate_expr arg) ~name:Expr.option_enum ~cases m
  | EDefault { excepts; just; cons }, m ->
    translate_default excepts just cons (translate_mark m)
  | EPureDefault e, m ->
    let pos = Expr.mark_pos m in
    let e = Expr.make_tuple [translate_expr e; Expr.make_pos pos m] m in
    Expr.einj ~e:(Some e) ~cons:Expr.some_constr ~name:Expr.option_enum
      (translate_mark m)
  | EAppOp { op; tys; args }, m ->
    Expr.eappop ~op:(Operator.translate op)
      ~tys:(List.map translate_typ tys)
      ~args:(List.map translate_expr args)
      (translate_mark m)
  | ( ( ELit _ | EArray _ | EVar _ | EApp _ | EAbs _ | EExternal _
      | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EAssert _
      | EFatalError _ | EStruct _ | EStructAccess _ | EMatch _ | EPos _ | EBad
        ),
      _ ) as e ->
    Expr.map ~f:translate_expr ~typ:translate_typ e
  | _ -> .

let translate_program (prg : 'm D.program) : 'm A.program =
  Program.map_exprs prg ~typ:translate_typ ~varf:Var.translate ~f:translate_expr
