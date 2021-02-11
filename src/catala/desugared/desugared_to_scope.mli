type rule_tree =
    Leaf of Ast.rule
  | Node of rule_tree list * Ast.rule
val def_map_to_tree :
  Ast.ScopeDef.t ->
  Ast.rule Ast.RuleMap.t -> rule_tree list
val rule_tree_to_expr :
  toplevel:bool ->
  Utils.Pos.t ->
  Scopelang.Ast.Var.t option ->
  rule_tree -> Scopelang.Ast.expr Utils.Pos.marked Bindlib.box
val translate_def :
  Ast.ScopeDef.t ->
  Ast.rule Ast.RuleMap.t ->
  Scopelang.Ast.typ Utils.Pos.marked ->
  bool -> Scopelang.Ast.expr Utils.Pos.marked
val translate_scope : Ast.scope -> Scopelang.Ast.scope_decl
val translate_program : Ast.program -> Scopelang.Ast.program
