type ident = string
type typ = Scopelang.Ast.typ
type unique_rulename = Ambiguous | Unique of Desugared.Ast.RuleName.t
type scope_context = {
  var_idmap : Scopelang.Ast.ScopeVar.t Desugared.Ast.IdentMap.t;
  label_idmap : Desugared.Ast.RuleName.t Desugared.Ast.IdentMap.t;
  default_rulemap : unique_rulename Desugared.Ast.ScopeDefMap.t;
  sub_scopes_idmap : Scopelang.Ast.SubScopeName.t Desugared.Ast.IdentMap.t;
  sub_scopes : Scopelang.Ast.ScopeName.t Scopelang.Ast.SubScopeMap.t;
}
type struct_context = typ Utils.Pos.marked Scopelang.Ast.StructFieldMap.t
type enum_context = typ Utils.Pos.marked Scopelang.Ast.EnumConstructorMap.t
type context = {
  local_var_idmap : Scopelang.Ast.Var.t Desugared.Ast.IdentMap.t;
  scope_idmap : Scopelang.Ast.ScopeName.t Desugared.Ast.IdentMap.t;
  struct_idmap : Scopelang.Ast.StructName.t Desugared.Ast.IdentMap.t;
  field_idmap :
    Scopelang.Ast.StructFieldName.t Scopelang.Ast.StructMap.t
    Desugared.Ast.IdentMap.t;
  enum_idmap : Scopelang.Ast.EnumName.t Desugared.Ast.IdentMap.t;
  constructor_idmap :
    Scopelang.Ast.EnumConstructor.t Scopelang.Ast.EnumMap.t
    Desugared.Ast.IdentMap.t;
  scopes : scope_context Scopelang.Ast.ScopeMap.t;
  structs : struct_context Scopelang.Ast.StructMap.t;
  enums : enum_context Scopelang.Ast.EnumMap.t;
  var_typs : (typ Utils.Pos.marked * bool) Scopelang.Ast.ScopeVarMap.t;
}
val raise_unsupported_feature : string -> Utils.Pos.t -> 'a
val raise_unknown_identifier : string -> ident Utils.Pos.marked -> 'a
val get_var_typ : context -> Scopelang.Ast.ScopeVar.t -> typ Utils.Pos.marked
val is_var_cond : context -> Scopelang.Ast.ScopeVar.t -> bool
val get_var_uid :
  Scopelang.Ast.ScopeName.t ->
  context -> ident Utils.Pos.marked -> Scopelang.Ast.ScopeVar.t
val get_subscope_uid :
  Scopelang.Ast.ScopeName.t ->
  context -> ident Utils.Pos.marked -> Scopelang.Ast.SubScopeName.t
val is_subscope_uid : Scopelang.Ast.ScopeName.t -> context -> ident -> bool
val belongs_to :
  context -> Scopelang.Ast.ScopeVar.t -> Scopelang.Ast.ScopeName.t -> bool
val get_def_typ : context -> Desugared.Ast.ScopeDef.t -> typ Utils.Pos.marked
val is_def_cond : context -> Desugared.Ast.ScopeDef.t -> bool
val process_subscope_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_scope -> context
val is_type_cond : Ast.typ Utils.Pos.marked -> bool
val process_base_typ :
  context ->
  Ast.base_typ Utils.Pos.marked -> Scopelang.Ast.typ Utils.Pos.marked
val process_type :
  context ->
  Ast.typ Utils.Pos.marked -> Scopelang.Ast.typ Utils.Pos.marked
val process_data_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_data -> context
val process_item_decl :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_decl_context_item -> context
val add_def_local_var :
  context -> ident Utils.Pos.marked -> context * Scopelang.Ast.Var.t
val process_scope_decl : context -> Ast.scope_decl -> context
val process_struct_decl : context -> Ast.struct_decl -> context
val process_enum_decl : context -> Ast.enum_decl -> context
val process_decl_item :
  context -> Ast.code_item Utils.Pos.marked -> context
val process_code_block :
  context ->
  Ast.code_block ->
  (context -> Ast.code_item Utils.Pos.marked -> context) -> context
val process_law_article_item :
  context ->
  Ast.law_article_item ->
  (context -> Ast.code_item Utils.Pos.marked -> context) -> context
val process_law_structure :
  context ->
  Ast.law_structure ->
  (context -> Ast.code_item Utils.Pos.marked -> context) -> context
val process_program_item :
  context ->
  Ast.program_item ->
  (context -> Ast.code_item Utils.Pos.marked -> context) -> context
val get_def_key :
  Ast.qident ->
  Scopelang.Ast.ScopeName.t ->
  context -> Utils.Pos.t -> Desugared.Ast.ScopeDef.t
val process_rule :
  context -> Scopelang.Ast.ScopeName.t -> Ast.rule -> context
val process_definition :
  context -> Scopelang.Ast.ScopeName.t -> Ast.definition -> context
val process_scope_use_item :
  Scopelang.Ast.ScopeName.t ->
  context -> Ast.scope_use_item Utils.Pos.marked -> context
val process_scope_use : context -> Ast.scope_use -> context
val process_use_item :
  context -> Ast.code_item Utils.Pos.marked -> context
val form_context : Ast.program -> context
