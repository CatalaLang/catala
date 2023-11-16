(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Alain Delaët <alain.delaet--tixeuil@inria.Fr>, Louis
   Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Concolic interpreter for the default calculus *)

open Catala_utils
open Shared_ast
module Concrete = Shared_ast.Interpreter

let lit_of_tlit t =
  match t with
  | TBool -> LBool true
  | TInt -> LInt (Z.of_int 42)
  | _ -> failwith "not implemented"

let expr_of_typ mark ty =
  match Mark.remove ty with
  | TLit t -> Expr.elit (lit_of_tlit t) mark
  | TArrow (ty_in, ty_out) ->
    Expr.make_abs
      (Array.of_list @@ List.map (fun _ -> Var.make "_") ty_in)
      (Bindlib.box EEmptyError, Expr.with_ty mark ty_out)
      ty_in (Expr.mark_pos mark)
  | _ -> failwith "not implemented"

let make_default_inputs (input_typs : typ StructField.Map.t) mark =
  StructField.Map.map (expr_of_typ mark) input_typs

let input_of_list = StructField.Map.of_list

(* Currently interprets with defaults values when available *)
let interpret_concrete_with_inputs :
    ((yes, no, 'c) interpr_kind, 't) gexpr program ->
    ScopeName.t ->
    ((yes, no, 'c) interpr_kind, 't) boxed_gexpr StructField.Map.t ->
    ((yes, no, 'c) interpr_kind, 't) gexpr StructField.Map.t =
 fun p s i ->
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match Concrete.evaluate_expr p.decl_ctx p.lang (Concrete.addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let all_fields_in_input =
      (* sanity check to catch a missing input before interpretation
       * TODO CONC should there be a typecheck as well?
       * TODO CONC better error message to show which field is missing *)
      StructField.Map.for_all (fun field _ -> StructField.Map.mem field i) taus
    in
    if all_fields_in_input then begin
      let to_interpret =
        Expr.make_app (Expr.box e)
          [Expr.estruct ~name:s_in ~fields:i mark_e]
          (Expr.pos e)
      in
      match
        Mark.remove
          (Concrete.evaluate_expr ctx p.lang (Expr.unbox to_interpret))
      with
      | EStruct { fields; _ } -> fields
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "The interpretation of a program should always yield a struct \
           corresponding to the scope variables"
    end
    else
      Message.raise_spanned_error (Expr.pos e)
        "Concolic concrete execution expects values in all input fields"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"

let interpret_program_concolic p s :
    (Uid.MarkedString.info * ('a, 'm) gexpr) list =
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match Concrete.evaluate_expr p.decl_ctx p.lang (Concrete.addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let application_term =
      make_default_inputs taus mark_e (* TODO CONC should this mark change? *)
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (Expr.pos e)
    in
    match
      Mark.remove (Concrete.evaluate_expr ctx p.lang (Expr.unbox to_interpret))
    with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructField.get_info fld, e)
        (StructField.Map.bindings fields)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
