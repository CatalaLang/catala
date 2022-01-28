
open Utils
module D = Dcalc.Ast
module A = Ast

(** information about the variables *)
type info = {
  expr: A.expr Pos.marked Bindlib.box;
  var: A.expr Bindlib.var;
  is_pure: bool
}

(** information context about variables in the current scope *)
type ctx = info D.VarMap.t

type cuts = D.expr Pos.marked D.VarMap.t



(** translate an expression, assuming there is absolutely no issues with options until it finds a eventual issue. *)
let translate_expr_and_cut
  (ctx : ctx)
  (e : D.expr Pos.marked)
  (acc: cuts)
: A.expr Pos.marked Bindlib.box * cuts =

  let current_pos = Pos.get_position e in
  match Pos.unmark e with
  | D.EVar v ->
    (* TODO: check error *)
    let info = D.VarMap.find (Pos.unmark v) ctx in
    if not (info.is_pure) then
      Errors.raise_spanned_error
        "Internal error: an unpure variable was found in a pure environement"
        current_pos
    else
    info.expr
  
  (* Implicit application *)
  | D.ETuple (args, s) ->
    assert false
  | D.ETupleAccess (e1, i, s, ts) ->
    assert false
  | D.EInj (e1, i, en, ts) ->
    assert false
  | D.EMatch (e1, cases, en) ->
    assert false
  | D.EArray es ->
    assert false
  | D.EIfThenElse (e1, e2, e3) ->
    assert false
  | D.EAssert e1 ->
    assert false
  | _ -> assert false

and translate_expr_monad (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  assert false