module A = Ast
val is_empty_error : A.expr Utils.Pos.marked -> bool
val empty_thunked_term : Ast.expr Utils.Pos.marked
val type_eq : A.typ Utils.Pos.marked -> A.typ Utils.Pos.marked -> bool
val log_indent : int ref
val compare_periods :
  CalendarLib.Date.Period.t Utils.Pos.marked ->
  CalendarLib.Date.Period.t Utils.Pos.marked -> int
val evaluate_operator :
  Ast.decl_ctx ->
  A.operator Utils.Pos.marked ->
  A.expr Utils.Pos.marked list -> A.expr Utils.Pos.marked
val evaluate_expr :
  Ast.decl_ctx -> A.expr Utils.Pos.marked -> A.expr Utils.Pos.marked
val interpret_program :
  Ast.decl_ctx ->
  Ast.expr Utils.Pos.marked ->
  (Utils.Uid.MarkedString.info * Ast.expr Utils.Pos.marked) list
