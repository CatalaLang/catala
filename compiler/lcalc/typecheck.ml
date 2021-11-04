module A = Ast
module T = Dcalc.Ast


let rec infer ctx (e: A.expr) : Dcalc.Ast.typ  =
  assert false