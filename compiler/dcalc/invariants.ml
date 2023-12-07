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
type invariant_expr = decl_ctx -> typed expr -> invariant_status

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
            match inv p.decl_ctx e with
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
    fun _ctx e ->
      match Mark.remove e with
      | EDefault _ -> begin
        match Mark.remove (Expr.ty e) with TArrow _ -> Fail | _ -> Pass
      end
      | _ -> Ignore )

(* Structural invariant: no partial evaluation *)
let invariant_no_partial_evaluation () : string * invariant_expr =
  ( __FUNCTION__,
    fun _ctx e ->
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
    fun _ctx e ->
      match Mark.remove e with
      | EAbs _ -> begin
        match Mark.remove (Expr.ty e) with
        | TArrow (_, (TArrow _, _)) -> Fail
        | _ -> Pass
      end
      | _ -> Ignore )

let invariant_app_inversion () : string * invariant_expr =
  ( __FUNCTION__,
    fun _ctx e ->
      match Mark.remove e with
      | EApp { f = EOp _, _; _ } -> Pass
      | EApp { f = EAbs { binder; _ }, _; args } ->
        if Bindlib.mbinder_arity binder = 1 && List.length args = 1 then Pass
        else Fail
      | EApp { f = EVar _, _; _ } -> Pass
      | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
        Pass
      | EApp { f = EStructAccess _, _; _ } -> Pass
      | EApp { f = EExternal _, _; _ } -> Pass
      | EApp _ -> Fail
      | _ -> Ignore )

(** the arity of constructors when matching is always one. *)
let invariant_match_inversion () : string * invariant_expr =
  ( __FUNCTION__,
    fun _ctx e ->
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

(* The purpose of these functions is to determine whether the type `TDefault`
   can only appear in certain positions, such as:

   * On the left-hand side of an arrow with arity 1, as the type of a scope (for
   scope calls).

   * At the root of the type tree (outside a default).

   * On the right-hand side of the arrow at the root of the type (occurs for
   rentrant variables).

   For instance, the following types do follow the invariant:

   int; bool; int -> bool; <bool>; <int -> bool>; int -> <bool>; S_in {x: int ->
   <bool>} -> S {y: bool}

   While the following types does not follow the invariant:

   <<int>>; <int -> <bool>>; <bool> -> int; S_in {x: int -> <bool>} -> S {y:
   <bool>}

   This is crucial to maintain the safety of the type system, as demonstrated in
   the formal development. *)

let rec check_typ_no_default ctx ty =
  match Mark.remove ty with
  | TLit _ -> true
  | TTuple ts -> List.for_all (check_typ_no_default ctx) ts
  | TStruct n ->
    let s = StructName.Map.find n ctx.ctx_structs in
    StructField.Map.for_all (fun _k ty -> check_typ_no_default ctx ty) s
  | TEnum n ->
    let s = EnumName.Map.find n ctx.ctx_enums in
    EnumConstructor.Map.for_all (fun _k ty -> check_typ_no_default ctx ty) s
  | TOption ty -> check_typ_no_default ctx ty
  | TArrow (args, res) ->
    List.for_all (check_typ_no_default ctx) args && check_typ_no_default ctx res
  | TArray ty -> check_typ_no_default ctx ty
  | TDefault _t -> false
  | TAny ->
    Message.raise_internal_error
      "Some Dcalc invariants are invalid: TAny was found whereas it should be \
       fully resolved."
  | TClosureEnv ->
    Message.raise_internal_error
      "Some Dcalc invariants are invalid: TClosureEnv was found whereas it \
       should only appear later in the compilation process."

let check_type_thunked_or_nodefault ctx ty =
  check_typ_no_default ctx ty
  ||
  match Mark.remove ty with
  | TArrow (args, res) -> (
    List.for_all (check_typ_no_default ctx) args
    &&
    match Mark.remove res with
    | TDefault ty -> check_typ_no_default ctx ty
    | _ -> check_typ_no_default ctx ty)
  | _ -> false

let check_type_root ctx ty =
  check_type_thunked_or_nodefault ctx ty
  ||
  match Mark.remove ty with
  | TStruct n ->
    let s = StructName.Map.find n ctx.ctx_structs in
    ScopeName.Map.exists
      (fun _k info -> StructName.equal info.in_struct_name n)
      ctx.ctx_scopes
    && StructField.Map.for_all
         (fun _k ty -> check_type_thunked_or_nodefault ctx ty)
         s
  | TArrow ([(TStruct n, _)], res) ->
    let s = StructName.Map.find n ctx.ctx_structs in
    ScopeName.Map.exists
      (fun _k info -> StructName.equal info.in_struct_name n)
      ctx.ctx_scopes
    && StructField.Map.for_all
         (fun _k ty -> check_type_thunked_or_nodefault ctx ty)
         s
    && check_typ_no_default ctx res
  | TDefault arg -> check_typ_no_default ctx arg
  | _ -> false

let invariant_typing_defaults () : string * invariant_expr =
  ( __FUNCTION__,
    fun ctx e ->
      if check_type_root ctx (Expr.ty e) then Pass
      else (
        Message.emit_warning "typing error %a@." (Print.typ ctx) (Expr.ty e);
        Fail) )

let check_all_invariants prgm =
  List.fold_left ( && ) true
    [
      check_invariant (invariant_default_no_arrow ()) prgm;
      check_invariant (invariant_no_partial_evaluation ()) prgm;
      check_invariant (invariant_no_return_a_function ()) prgm;
      check_invariant (invariant_app_inversion ()) prgm;
      check_invariant (invariant_match_inversion ()) prgm;
      check_invariant (invariant_typing_defaults ()) prgm;
    ]
