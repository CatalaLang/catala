(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Alain Delaët <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Shared_ast
open Ast
open Catala_utils

type invariant_status = Fail | Pass | Ignore
type invariant_expr = typed expr -> invariant_status

let check_invariant (inv : string * invariant_expr) (p : typed program) : bool =
  (* TODO: add a Program.fold_left_map_exprs to get rid of the mutable
     reference *)
  let result = ref true in
  let name, inv = inv in
  let total = ref 0 in
  let ok = ref 0 in
  let p' =
    Program.map_exprs p ~varf:Fun.id ~f:(fun e ->
        (* let currente = e in *)
        let rec f e =
          let r =
            match inv e with
            | Ignore -> true
            | Fail ->
              Message.raise_spanned_error (Expr.pos e) "%s failed\n\n%a" name
                (Print.expr ()) e
            | Pass ->
              incr ok;
              incr total;
              true
          in
          Expr.map_gather e ~acc:r ~join:( && ) ~f
        in

        let res, e' = f e in
        result := res && !result;
        e')
  in
  assert (Bindlib.free_vars p' = Bindlib.empty_ctxt);
  Message.emit_result "Invariant %s\n   checked. result: [%d/%d]" name !ok
    !total;
  !result

(* Structural invariant: no default can have as type A -> B *)
let invariant_default_no_arrow () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Mark.remove e with
      | EDefault _ -> begin
        match Mark.remove (Expr.ty e) with TArrow _ -> Fail | _ -> Pass
      end
      | _ -> Ignore )

(* Structural invariant: no partial evaluation *)
let invariant_no_partial_evaluation () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Mark.remove e with
      | EApp { f = EOp { op = Op.Log _; _ }, _; _ } ->
        (* logs are differents. *) Pass
      | EApp _ -> begin
        match Mark.remove (Expr.ty e) with TArrow _ -> Fail | _ -> Pass
      end
      | _ -> Ignore )

(* Structural invariant: no function can return an function *)
let invariant_no_return_a_function () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Mark.remove e with
      | EAbs _ -> begin
        match Mark.remove (Expr.ty e) with
        | TArrow (_, (TArrow _, _)) -> Fail
        | _ -> Pass
      end
      | _ -> Ignore )

let invariant_app_inversion () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Mark.remove e with
      | EApp { f = EOp _, _; _ } -> Pass
      | EApp { f = EAbs { binder; _ }, _; args } ->
        if Bindlib.mbinder_arity binder = 1 && List.length args = 1 then Pass
        else Fail
      | EApp { f = EVar _, _; _ } -> Pass
      | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
        Pass
      | EApp { f = EStructAccess _, _; _ } -> Pass
      | EApp _ -> Fail
      | _ -> Ignore )

(** the arity of constructors when matching is always one. *)
let invariant_match_inversion () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Mark.remove e with
      | EMatch { cases; _ } ->
        if
          EnumConstructor.Map.for_all
            (fun _ case ->
              match Mark.remove case with
              | EAbs { binder; _ } -> Bindlib.mbinder_arity binder = 1
              | _ -> false)
            cases
        then Pass
        else Fail
      | _ -> Ignore )

let check_all_invariants prgm =
  List.fold_left ( && ) true
    [
      check_invariant (invariant_default_no_arrow ()) prgm;
      check_invariant (invariant_no_partial_evaluation ()) prgm;
      check_invariant (invariant_no_return_a_function ()) prgm;
      check_invariant (invariant_app_inversion ()) prgm;
      check_invariant (invariant_match_inversion ()) prgm;
    ]
