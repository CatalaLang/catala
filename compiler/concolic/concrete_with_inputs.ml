open Catala_utils
open Shared_ast
module Concrete = Shared_ast.Interpreter

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
