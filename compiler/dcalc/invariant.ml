(* Structural invariant: *)

open Shared_ast
open Ast
open Catala_utils

type invariant_expr = typed expr -> bool option

(* Structural invariant: no default can have as type A -> B *)
let invariant_default_no_arrow () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EDefault _ -> begin
        match Marked.unmark (Expr.ty e) with
        | TArrow _ -> Some false
        | _ -> Some true
      end
      | _ -> None )

(* Structural invariant: no partial evaluation *)
let invariant_no_partial_evaluation () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EApp { f = EOp { op = Op.Log _; _ }, _; _ } ->
        (* logs are differents. *)
        Some true
      | EApp _ -> begin
        match Marked.unmark (Expr.ty e) with
        | TArrow _ -> Some false
        | _ -> Some true
      end
      | _ -> None )

(* Structural invariant: no function can return an function *)
let invariant_no_return_a_function () : string * invariant_expr =
  ( __FUNCTION__,
    fun e ->
      match Marked.unmark e with
      | EAbs _ -> begin
        match Marked.unmark (Expr.ty e) with
        | TArrow (_, (TArrow _, _)) -> Some false
        | _ -> Some true
      end
      | _ -> None )

let check_invariant (inv : string * invariant_expr) (p : typed program) =
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
            | None -> true
            | Some false ->
              Cli.error_format "%s failed in %s.\n\n %a" name
                (Pos.to_string_short (Expr.pos e))
                (Print.expr ~debug:true p.decl_ctx)
                e;
              incr total;
              false
            | Some true ->
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
  Cli.result_print "Invariant %s checked. result: [%d/%d]" name !ok !total
