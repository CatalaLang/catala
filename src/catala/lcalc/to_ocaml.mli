val format_lit : Format.formatter -> Ast.lit Utils.Pos.marked -> unit
val format_op_kind : Format.formatter -> Dcalc.Ast.op_kind -> unit
val format_log_entry : Format.formatter -> Dcalc.Ast.log_entry -> unit
val format_binop :
  Format.formatter -> Dcalc.Ast.binop Utils.Pos.marked -> unit
val format_ternop :
  Format.formatter -> Dcalc.Ast.ternop Utils.Pos.marked -> unit
val format_unop : Format.formatter -> Dcalc.Ast.unop Utils.Pos.marked -> unit
val to_ascii : string -> string
val to_lowercase : string -> string
val format_struct_name : Format.formatter -> Dcalc.Ast.StructName.t -> unit
val format_struct_field_name :
  Format.formatter -> Dcalc.Ast.StructFieldName.t -> unit
val format_enum_name : Format.formatter -> Dcalc.Ast.EnumName.t -> unit
val format_enum_cons_name :
  Format.formatter -> Dcalc.Ast.EnumConstructor.t -> unit
val typ_needs_parens : Dcalc.Ast.typ Utils.Pos.marked -> bool
val format_typ : Format.formatter -> Dcalc.Ast.typ Utils.Pos.marked -> unit
val format_var : Format.formatter -> Ast.Var.t -> unit
val needs_parens : Ast.expr Utils.Pos.marked -> bool
val format_exception : Format.formatter -> Ast.except -> unit
val format_expr :
  Dcalc.Ast.decl_ctx ->
  Format.formatter -> Ast.expr Utils.Pos.marked -> unit
val format_ctx :
  Scopelang.Dependency.TVertex.t list ->
  Format.formatter -> Ast.D.decl_ctx -> unit
val format_program :
  Format.formatter ->
  Ast.program -> Scopelang.Dependency.TVertex.t list -> unit
