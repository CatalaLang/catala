val translate_op_kind : Ast.op_kind -> Dcalc.Ast.op_kind
val translate_binop : Ast.binop -> Dcalc.Ast.binop
val translate_unop : Ast.unop -> Dcalc.Ast.unop
module LiftStructFieldMap :
  sig
    val lift_box :
      'a Bindlib.box Scopelang.Ast.StructFieldMap.t ->
      'a Scopelang.Ast.StructFieldMap.t Bindlib.box
  end
module LiftEnumConstructorMap :
  sig
    val lift_box :
      'a Bindlib.box Scopelang.Ast.EnumConstructorMap.t ->
      'a Scopelang.Ast.EnumConstructorMap.t Bindlib.box
  end
val disambiguate_constructor :
  Name_resolution.context ->
  (string Utils.Pos.marked option * string Utils.Pos.marked) list ->
  Utils.Pos.t -> Scopelang.Ast.EnumName.t * Scopelang.Ast.EnumConstructor.t
val translate_expr :
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Ast.expression Utils.Pos.marked ->
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box
val disambiguate_match_and_build_expression :
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Ast.match_cases ->
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box
  Scopelang.Ast.EnumConstructorMap.t * Scopelang.Ast.EnumName.t
val merge_conditions :
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Utils.Pos.t -> Scopelang.Ast.expr Utils.Pos.marked Bindlib.box
val process_default :
  Name_resolution.context ->
  Scopelang.Ast.ScopeName.t ->
  Desugared.Ast.ScopeDef.t Utils.Pos.marked ->
  Scopelang.Ast.Var.t Utils.Pos.marked option ->
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Desugared.Ast.RuleName.t Utils.Pos.marked option ->
  Ast.expression Utils.Pos.marked option ->
  Ast.expression Utils.Pos.marked -> Desugared.Ast.rule
val process_def :
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program -> Ast.definition -> Desugared.Ast.program
val rule_to_def : Ast.rule -> Ast.definition
val process_rule :
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program -> Ast.rule -> Desugared.Ast.program
val process_assert :
  Scopelang.Ast.expr Utils.Pos.marked Bindlib.box option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program -> Ast.assertion -> Desugared.Ast.program
val process_scope_use_item :
  Ast.expression Utils.Pos.marked option ->
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Desugared.Ast.program ->
  Ast.scope_use_item Utils.Pos.marked -> Desugared.Ast.program
val check_unlabeled_exception :
  Scopelang.Ast.ScopeName.t ->
  Name_resolution.context ->
  Ast.scope_use_item Utils.Pos.marked -> unit
val process_scope_use :
  Name_resolution.context ->
  Desugared.Ast.program -> Ast.scope_use -> Desugared.Ast.program
val desugar_program :
  Name_resolution.context ->
  Ast.program -> Desugared.Ast.program
