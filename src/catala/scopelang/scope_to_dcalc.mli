type scope_sigs_ctx =
    ((Ast.ScopeVar.t * Dcalc.Ast.typ) list * Dcalc.Ast.Var.t *
     Dcalc.Ast.Var.t * Ast.StructName.t *
     Ast.StructName.t)
    Ast.ScopeMap.t
type ctx = {
  structs : Ast.struct_ctx;
  enums : Ast.enum_ctx;
  scope_name : Ast.ScopeName.t;
  scopes_parameters : scope_sigs_ctx;
  scope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t;
  subscope_vars :
    (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t
    Ast.SubScopeMap.t;
  local_vars : Dcalc.Ast.Var.t Ast.VarMap.t;
}
val empty_ctx :
  Ast.struct_ctx ->
  Ast.enum_ctx ->
  scope_sigs_ctx -> Ast.ScopeName.t -> ctx
type scope_ctx = Dcalc.Ast.Var.t Ast.ScopeMap.t
val hole_var : Dcalc.Ast.Var.t
val translate_typ :
  ctx -> Ast.typ Utils.Pos.marked -> Dcalc.Ast.typ Utils.Pos.marked
val merge_defaults :
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box
val tag_with_log_entry :
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box ->
  Dcalc.Ast.log_entry ->
  Utils.Uid.MarkedString.info list ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box
val translate_expr :
  ctx ->
  Ast.expr Utils.Pos.marked ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box
val translate_rule :
  ctx ->
  Ast.rule ->
  Ast.rule list ->
  Utils.Uid.MarkedString.info ->
  Ast.StructName.t ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box * ctx
val translate_rules :
  ctx ->
  Ast.rule list ->
  Utils.Uid.MarkedString.info ->
  Ast.StructName.t ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box * ctx
val translate_scope_decl :
  Ast.struct_ctx ->
  Ast.enum_ctx ->
  scope_sigs_ctx ->
  Ast.ScopeName.t ->
  Ast.scope_decl ->
  Dcalc.Ast.expr Utils.Pos.marked Bindlib.box * Dcalc.Ast.struct_ctx
val build_scope_typ_from_sig :
  (Ast.ScopeVar.t * Dcalc.Ast.typ) list ->
  Ast.StructName.t ->
  Ast.StructName.t -> Utils.Pos.t -> Dcalc.Ast.typ Utils.Pos.marked
val translate_program :
  Ast.program ->
  Ast.ScopeName.t ->
  Dcalc.Ast.program * Dcalc.Ast.expr Utils.Pos.marked *
  Dependency.TVertex.t list
