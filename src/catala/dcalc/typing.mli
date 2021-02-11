module A = Ast
module Any :
  sig
    type t
    type info = unit
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
type typ =
    TLit of A.typ_lit
  | TArrow of typ Utils.Pos.marked UnionFind.elem *
      typ Utils.Pos.marked UnionFind.elem
  | TTuple of typ Utils.Pos.marked UnionFind.elem list *
      Ast.StructName.t option
  | TEnum of typ Utils.Pos.marked UnionFind.elem list * Ast.EnumName.t
  | TArray of typ Utils.Pos.marked UnionFind.elem
  | TAny of Any.t
val typ_needs_parens : typ Utils.Pos.marked UnionFind.elem -> bool
val format_typ :
  Ast.decl_ctx ->
  Format.formatter -> typ Utils.Pos.marked UnionFind.elem -> unit
val unify :
  Ast.decl_ctx ->
  typ Utils.Pos.marked UnionFind.elem ->
  typ Utils.Pos.marked UnionFind.elem -> unit
val op_type :
  A.operator Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem
val ast_to_typ : A.typ -> typ
val typ_to_ast :
  typ Utils.Pos.marked UnionFind.elem -> A.typ Utils.Pos.marked
type env = typ Utils.Pos.marked UnionFind.elem A.VarMap.t
val typecheck_expr_bottom_up :
  Ast.decl_ctx ->
  env -> A.expr Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem
val typecheck_expr_top_down :
  Ast.decl_ctx ->
  env ->
  A.expr Utils.Pos.marked -> typ Utils.Pos.marked UnionFind.elem -> unit
val infer_type :
  Ast.decl_ctx -> A.expr Utils.Pos.marked -> A.typ Utils.Pos.marked
val check_type :
  Ast.decl_ctx ->
  A.expr Utils.Pos.marked -> A.typ Utils.Pos.marked -> unit
