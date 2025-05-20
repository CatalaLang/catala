open Catala_utils
open Shared_ast
open Shared_ast.Print
open Global

let rec print_expr _ = (*(expr : ('a, 'm) gexpr) =*)
  (*
  match expr with
  | ELit l -> print_string (UserFacing.lit_to_string En l)
  | EApp { f; args; tys } ->
      print_string "EApp(";
      (*print_expr f;*)
      print_string ", [";
      (*List.iter (fun arg -> print_expr arg; print_string ", ") args;*)
      print_string "])"
  | EAppOp { op; args; tys } ->
      print_string "EAppOp(";
      print_string (operator_to_string (Mark.remove op));
      print_string ", [";
      (*List.iter (fun arg -> print_expr arg; print_string ", ") args;*)
      print_string "])"
  | EArray arr ->
      print_string "EArray(";
      (*List.iter (fun elem -> print_string ", "; print_expr elem) arr;*)
      print_string ")"
  | EVar _ -> print_string "EVar"
  | EAbs { binder; pos; tys } -> print_string "EAbs"
  | EIfThenElse { cond; etrue; efalse } ->
      print_string "EIfThenElse(";
      (*print_expr cond;*)
      print_string ", ";
      (*print_expr etrue;*)
      print_string ", ";
      (*print_expr efalse;*)
      print_string ")"
  | EStruct { name; fields } ->
      print_string "EStruct(";
      print_string (StructName.to_string name);
      print_string ", [";
      StructField.Map.iter (fun _ e -> print_expr e; print_string ", ") fields;
      print_string "])"
  | EInj { name; e; cons } ->
      print_string "EInj(";
      print_string (EnumName.to_string name);
      print_string ", ";
      print_expr e;
      print_string ")"
  | EMatch { name; e; cases } ->
      print_string "EMatch(";
      print_string (EnumName.to_string name);
      print_string ", ";
      print_expr e;
      print_string ", [";
      EnumConstructor.Map.iter (fun _ e -> print_expr e; print_string ", ")
        cases;
      print_string "])"
  | ETuple lst ->
      print_string "ETuple(";
      List.iter (fun elem -> print_string ", "; print_expr elem) lst;
      print_string ")"
  | ETupleAccess { e; index; size } ->
      Printf.printf "ETupleAccess(%d, %d, " index size;
      print_expr e;
      print_string ")"
  | EStructAccess { name; e; field } ->
      print_string "EStructAccess(";
      print_string (StructName.to_string name);
      print_string ", ";
      print_expr e;
      print_string ")"
  | EExternal { name } -> print_string "EExternal"
  | EAssert e ->
      print_string "EAssert(";
      print_expr e;
      print_string ")"
  | EFatalError _ -> print_string "EFatalError"
  | EPos pos -> print_string "EPos"
  | EDefault { excepts; just; cons } ->
      print_string "EDefault([";
      List.iter (fun exc -> print_expr exc; print_string ", ") excepts;
      print_string "], ";
      print_expr just;
      print_string ", ";
      print_expr cons;
      print_string ")"
  | EPureDefault e ->
      print_string "EPureDefault(";
      print_expr e;
      print_string ")"
  | EEmpty -> print_string "∅"
  | EErrorOnEmpty e ->
      print_string "EErrorOnEmpty(";
      print_expr e;
      print_string ")"
  | ECustom { obj; targs; tret } -> print_string "ECustom"
  | EHole _ -> print_string "□"
  | _ -> assert false
  *)
  print_string "□"

let rec print_trace (trace : 'a Trace_ast.t) =
  match trace with
  | TrExpr _ -> print_string "TrExpr(";print_expr (); print_string ")"
  | TrLit l -> 
    (*print_string "TrLit(";*)
    print_string (UserFacing.lit_to_string En l)
    (*;print_string ")"*)
  | TrApp { trf; trargs; tys; trv } ->
      print_string "TrApp(";
      print_trace trf;
      print_string ", [";
      List.iter (fun arg -> print_trace arg; print_string ", ") trargs;
      print_string "], ";
      print_trace trv;
      print_string ")"
  | TrAppOp { op; trargs; tys; trv } ->
      print_string "TrAppOp(";
      print_string (operator_to_string (Mark.remove op));
      print_string ", [";
      List.iter (fun arg -> print_trace arg; print_string ", ") trargs;
      print_string "], ";
      print_trace trv;
      print_string ")"
  | TrArray arr ->
      print_string "TrArray(";
      List.iter (fun elem -> print_string ", "; print_trace elem) arr;
      print_string ")"
  | TrVar _ -> print_string "TrVar"
  | TrAbs { binder; pos; tys } -> print_string "TrAbs"
  | TrIfThenElse { trcond; trtrue; trfalse } ->
      print_string "TrIfThenElse(";
      print_trace trcond;
      print_string ", ";
      print_trace trtrue;
      print_string ", ";
      print_trace trfalse;
      print_string ")"
  | TrStruct { name; fields } ->
      print_string "TrStruct(";
      print_string(StructName.to_string name);
      print_string ", [";
      StructField.Map.iter (fun _ tr -> print_trace tr; print_string ", ")
        fields;
      print_string "])"
  | TrInj { name; tr; cons } ->
      print_string "TrInj("; 
      print_string (EnumName.to_string name);
      print_string ", ";
      print_trace tr;
      print_string ")"
  | TrMatch { name; tr; cases } ->
      print_string "TrMatch(%s("; 
      print_string (EnumName.to_string name);
      print_string ", ";
      print_trace tr;
      print_string ", [";
      EnumConstructor.Map.iter (fun _ tr -> print_trace tr; print_string ", ")
        cases;
      print_string "])"
  | TrTuple lst ->
      print_string "TrTuple(";
      List.iter (fun elem -> print_string ", "; print_trace elem) lst;
      print_string ")"
  | TrTupleAccess { tr; index; size } ->
      Printf.printf "TrTupleAccess(%d, %d, " index size;
      print_trace tr;
      print_string ")"
  | TrStructAccess { name; tr; field } ->
      print_string "TrStructAccess("; 
      print_string (StructName.to_string name);
      print_string ", ";
      print_trace tr;
      print_string ")"
  | TrExternal { name } -> print_string "TrExternal"
  | TrAssert tr ->
      print_string "TrAssert(";
      print_trace tr;
      print_string ")"
  | TrFatalError { err; tr } ->
      print_string "TrFatalError(";
      print_trace tr;
      print_string ")"
  | TrDefault { trexcepts; trjust; trcons } ->
      print_string "TrDefault(";
      print_string "[";
      List.iter (fun exc -> print_trace exc; print_string ", ") trexcepts;
      print_string "], ";
      print_trace trjust;
      print_string ", ";
      print_trace trcons;
      print_string ")"
  | TrPureDefault tr ->
      print_string "TrPureDefault(";
      print_trace tr;
      print_string ")"
  | TrEmpty -> print_string "∅"
  | TrErrorOnEmpty tr ->
      print_string "TrErrorOnEmpty(";
      print_trace tr;
      print_string ")"
  | TrCustom { obj; targs; tret } -> print_string "TrCustom"
  | TrHole _ -> print_string "□"