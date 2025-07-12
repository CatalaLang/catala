(* Format and print functions for expr, contexts, sets of variables and traces *)
val expr_with_cut :
  cut_app:int option -> Format.formatter -> ('a, 'b) Shared_ast.gexpr -> unit

val expr : Format.formatter -> ('a, 'b) Shared_ast.gexpr -> unit

val print_expr : ?fmt:Format.formatter -> ('a, 'm) Shared_ast.gexpr -> unit

val context :
  Format.formatter ->
  (('a, 'b) Shared_ast__Definitions.gexpr, ('c, 'd) Shared_ast.gexpr)
  Shared_ast.Var.Map.t -> unit

val print_context :
  ?fmt:Format.formatter ->
  (('a, 'b) Shared_ast__Definitions.gexpr, ('c, 'd) Shared_ast.gexpr)
  Shared_ast.Var.Map.t -> unit

val set :
  Format.formatter ->
  ('a, 'b) Shared_ast__Definitions.gexpr Shared_ast.Var.Set.t -> unit

val trace : Format.formatter -> ('a, 'b) Trace_ast.t -> unit

val print_trace : ?fmt:Format.formatter -> ('a, 'm) Trace_ast.t -> unit
