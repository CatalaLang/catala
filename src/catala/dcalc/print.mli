val typ_needs_parens : Ast.typ Utils.Pos.marked -> bool
val is_uppercase : CamomileLibraryDefault.Camomile.UChar.t -> bool
val begins_with_uppercase : string -> bool
val format_uid_list :
  Format.formatter -> Utils.Uid.MarkedString.info list -> unit
val format_tlit : Format.formatter -> Ast.typ_lit -> unit
val format_typ :
  Ast.decl_ctx ->
  Format.formatter -> Ast.typ Utils.Pos.marked -> unit
val format_lit : Format.formatter -> Ast.lit Utils.Pos.marked -> unit
val format_op_kind : Format.formatter -> Ast.op_kind -> unit
val format_binop :
  Format.formatter -> Ast.binop Utils.Pos.marked -> unit
val format_ternop :
  Format.formatter -> Ast.ternop Utils.Pos.marked -> unit
val format_log_entry : Format.formatter -> Ast.log_entry -> unit
val format_unop : Format.formatter -> Ast.unop Utils.Pos.marked -> unit
val needs_parens : Ast.expr Utils.Pos.marked -> bool
val format_var : Format.formatter -> Ast.Var.t -> unit
val format_expr :
  Ast.decl_ctx ->
  Format.formatter -> Ast.expr Utils.Pos.marked -> unit
