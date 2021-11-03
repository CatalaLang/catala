module A = Ast


let rec infer ctx (e: A.expr) =
  match e with
  | 