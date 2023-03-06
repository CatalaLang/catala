(* Structural invariant: *)

open Shared_ast
open Ast
open Catala_utils

type invariant_status = Fail | Pass | Ignore
type invariant_expr = typed expr -> invariant_status

(* Structural invariant: no default can have as type A -> B *)
let invariant_default_no_arrow () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EDefault _ -> begin
        match Marked.unmark (Expr.ty e) with TArrow _ -> Fail | _ -> Pass
      end
      | _ -> Ignore )

(* Structural invariant: no partial evaluation *)
let invariant_no_partial_evaluation () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EApp { f = EOp { op = Op.Log _; _ }, _; _ } ->
        (* logs are differents. *)
        Pass
      | EApp _ -> begin
        match Marked.unmark (Expr.ty e) with TArrow _ -> Fail | _ -> Pass
      end
      | _ -> Ignore )

(* Structural invariant: no function can return an function *)
let invariant_no_return_a_function () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EAbs _ -> begin
        match Marked.unmark (Expr.ty e) with
        | TArrow (_, (TArrow _, _)) -> Fail
        | _ -> Pass
      end
      | _ -> Ignore )

let invariant_app_is_either_op_var_let () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EApp { f = EOp _, _; _ } -> Pass
      | EApp { f = EAbs _, _; _ } -> Pass
      | EApp { f = EVar _, _; _ } -> Pass
      | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
        Pass
      | EApp { f = EStructAccess _, _; _ } -> Pass
      | EApp _ -> Fail
      | _ -> Ignore )

(** the arity of constructors when matching is always one. *)
let invariant_match () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EMatch { cases; _ } ->
        if
          EnumConstructor.Map.for_all
            (fun _ case ->
              match Marked.unmark case with
              | EAbs { binder; _ } -> Bindlib.mbinder_arity binder = 1
              | _ -> false)
            cases
        then Pass
        else Fail
      | _ -> Ignore )

let check_invariant (inv : string * invariant_expr) (p : typed program) : bool =
  (* TODO: add a Program.fold_exprs to get rid of the reference 0:-)? *)
  let result = ref true in
  let name, inv = inv in
  let _ = name in
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
              Cli.error_format "%s failed in %s.\n\n %a" name
                (Pos.to_string_short (Expr.pos e))
                (Print.expr ~debug:true p.decl_ctx)
                e;
              incr total;
              false
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
  Cli.result_print "Invariant %s checked. result: [%d/%d]" name !ok !total;
  !result
