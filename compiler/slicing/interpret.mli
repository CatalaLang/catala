
val evaluate_expr_with_trace :
  Shared_ast.decl_ctx ->
  Catala_utils.Global.backend_lang ->
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr ->
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr *
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Trace_ast.t

val evaluate_expr_safe :
  Shared_ast.decl_ctx ->
  Catala_utils.Global.backend_lang ->
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr ->
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr *
  (('d, Shared_ast.yes, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Trace_ast.t
