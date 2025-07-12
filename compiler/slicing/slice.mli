val unevaluate :
  Shared_ast.decl_ctx ->
  ((Shared_ast.yes, Shared_ast.yes, Shared_ast.yes)
   Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr ->
  ((Shared_ast.yes, Shared_ast.yes, Shared_ast.yes)
   Shared_ast.slicing_interpr_kind, 't)
  Trace_ast.t ->
  ((Shared_ast.yes, Shared_ast.yes, Shared_ast.yes)
   Shared_ast.slicing_interpr_kind, 't)
  Shared_ast.gexpr

val slice :
  ?debug:bool ->
  (Shared_ast.dcalc, 'm) Shared_ast.gexpr Shared_ast.program ->
  Shared_ast.ScopeName.t ->
  ((Shared_ast.yes, Shared_ast.yes, Shared_ast.yes)
   Shared_ast.slicing_interpr_kind, 'm)
  Shared_ast.gexpr *
  ((Shared_ast.yes, Shared_ast.yes, Shared_ast.yes)
   Shared_ast.slicing_interpr_kind, 'm)
  Shared_ast__Definitions.gexpr
  
val test :
  ?debug:bool ->
  (Shared_ast.dcalc, 'm) Shared_ast.gexpr Shared_ast.program ->
  Shared_ast.ScopeName.t -> bool
