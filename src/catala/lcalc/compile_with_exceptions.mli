module D = Dcalc.Ast
module A = Ast
type ctx = A.expr Utils.Pos.marked Bindlib.box D.VarMap.t
val handle_default : Utils.Pos.t -> A.expr Utils.Pos.marked Bindlib.box
val translate_lit : D.lit -> A.expr
val thunk_expr :
  A.expr Utils.Pos.marked Bindlib.box ->
  Utils.Pos.t -> A.expr Utils.Pos.marked Bindlib.box
val translate_default :
  ctx ->
  D.expr Utils.Pos.marked list ->
  D.expr Utils.Pos.marked ->
  D.expr Utils.Pos.marked ->
  Utils.Pos.t -> A.expr Utils.Pos.marked Bindlib.box
val translate_expr :
  ctx -> D.expr Utils.Pos.marked -> A.expr Utils.Pos.marked Bindlib.box
val translate_program : D.program -> A.program
