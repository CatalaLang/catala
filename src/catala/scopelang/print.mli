val needs_parens : Ast.expr Utils.Pos.marked -> bool
val format_var : Format.formatter -> Ast.Var.t -> unit
val format_location : Format.formatter -> Ast.location -> unit
val typ_needs_parens : Ast.typ Utils.Pos.marked -> bool
val format_typ :
  Format.formatter -> Ast.typ Utils.Pos.marked -> unit
val format_expr :
  Format.formatter -> Ast.expr Utils.Pos.marked -> unit
