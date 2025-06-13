open Catala_utils
open Shared_ast
open Shared_ast.Print
open Global

let string_of_list ?(inline = false) f lst indent =
  let new_line, spaces = if inline then "","" else "\n", String.make indent ' ' in 
  Format.asprintf "%s[%s%s%s%s]" spaces new_line (String.concat (", "^new_line) (List.map f lst)) new_line spaces

let string_of_container ?(inline = false) iter key_to_string f container indent =
  let new_line, spaces, add_space = if inline then "","","" else "\n", String.make indent ' ', "  " in 
  let parts = ref [] in
  iter 
    (fun key value ->
      parts := Format.asprintf "%s%s%s -> %s%s%s" spaces add_space (key_to_string key) new_line add_space (f value) :: !parts) 
    container;
  Format.asprintf "%s[%s%s%s%s]" spaces new_line (String.concat ",\n" (List.rev !parts)) new_line spaces

let rec string_of_expr:
    type d t.
    ?indent:int -> 
    ?inline:bool -> 
    (d, t) gexpr ->
    file =
fun ?(indent=0) ?(inline=false) expr ->
  let new_line, spaces = if inline then "", "" else "\n", String.make indent ' ' in
  match Mark.remove expr with
  | ELit l -> Format.asprintf "%s%s" spaces (UserFacing.lit_to_string En l)
  | EApp { f; args; tys = _ } ->
      Format.asprintf "%sEApp(%s%s,%s%s%s%s)" 
        spaces 
        new_line (string_of_expr ~indent:(indent + 2) ~inline f) 
        new_line (string_of_list ~inline (string_of_expr ~indent:(indent + 4) ~inline) args (indent + 2))
        new_line spaces
  | EAppOp { op; args; tys = _ } ->
      Format.asprintf "%sEAppOp(%s,%s%s%s%s)" 
        spaces (operator_to_string (Mark.remove op))
        new_line (string_of_list ~inline (string_of_expr ~indent:(indent + 4) ~inline) args (indent + 2))
        new_line spaces
  | EArray arr ->
      Format.asprintf "%sEArray(%s%s%s%s)" 
        spaces 
        new_line (string_of_list ~inline (string_of_expr ~indent:(indent + 4) ~inline) arr (indent + 2))
        new_line spaces
  | EVar x -> Format.asprintf "%sEVar(%s)" spaces (Bindlib.name_of x)
  | EAbs { binder; pos = _; tys = _ } ->
      let (vars, body) = Bindlib.unmbind binder in
      let add_space = if inline then "" else "    " in
      Format.asprintf "%sEAbs(%s%s,%s%s%s%s)" 
        spaces 
        new_line (string_of_list ~inline (fun x -> Format.asprintf "%s%s%s" spaces add_space (Bindlib.name_of x)) (Array.to_list vars) (indent + 2))
        new_line (string_of_expr ~indent:(indent + 2) ~inline body)
        new_line spaces
  | EIfThenElse { cond; etrue; efalse } ->
      Format.asprintf "%sEIfThenElse(%s%s,%s%s,%s%s%s%s)" 
        spaces 
        new_line (string_of_expr ~indent:(indent + 2) ~inline cond)
        new_line (string_of_expr ~indent:(indent + 2) ~inline etrue)
        new_line (string_of_expr ~indent:(indent + 2) ~inline efalse)
        new_line spaces
  | EStruct { name; fields } ->
      Format.asprintf "%sEStruct(%s,%s%s%s%s)" 
        spaces (StructName.to_string name) 
        new_line (string_of_container ~inline StructField.Map.iter StructField.to_string (string_of_expr ~indent:(indent + 4) ~inline) fields (indent + 2))
        new_line spaces
  | EInj { name; e; cons } ->
      Format.asprintf "%sEInj(%s.%s,%s%s%s%s)" 
        spaces (EnumName.to_string name) (EnumConstructor.to_string cons) 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | EMatch { name; e; cases } ->
      Format.asprintf "%sEMatch(%s,%s%s,%s%s%s%s)" 
        spaces (EnumName.to_string name) 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line (string_of_container ~inline EnumConstructor.Map.iter EnumConstructor.to_string (string_of_expr ~indent:(indent + 4) ~inline) cases (indent + 2))
        new_line spaces
  | ETuple lst ->
      Format.asprintf "%sETuple(%s%s%s%s)" 
        spaces 
        new_line (string_of_list ~inline (string_of_expr ~indent:(indent + 4) ~inline) lst (indent + 2))
        new_line spaces
  | ETupleAccess { e; index; size } ->
      Format.asprintf "%sETupleAccess(index = %d, size = %d,%s%s%s%s)" 
        spaces index size 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | EStructAccess { name; e; field } ->
      Format.asprintf "%sEStructAccess(%s.%s,%s%s%s%s)" 
        spaces (StructName.to_string name) (StructField.to_string field) 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | EExternal _ -> Format.asprintf "%sEExternal" spaces
  | EAssert e ->
      Format.asprintf "%sEAssert(%s%s%s%s)" 
        spaces 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | EFatalError err ->
      Format.asprintf "%sEFatalError(%s)" spaces (Runtime.error_to_string err)
  | EPos _ ->
      Format.asprintf "%sEPos" spaces
  | EDefault { excepts; just; cons } ->
      Format.asprintf "%sEDefault(%s%s,%s%s,%s%s%s%s)" 
        spaces 
        new_line (string_of_list ~inline (string_of_expr ~indent:(indent + 4) ~inline) excepts (indent + 2))
        new_line (string_of_expr ~indent:(indent + 2) ~inline just)
        new_line (string_of_expr ~indent:(indent + 2) ~inline cons)
        new_line spaces
  | EPureDefault e ->
      Format.asprintf "%sEPureDefault(%s%s%s%s)" 
        spaces 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | EEmpty -> Format.asprintf "%s∅" spaces
  | EErrorOnEmpty e ->
      Format.asprintf "%sEErrorOnEmpty(%s%s%s%s)" spaces 
        new_line (string_of_expr ~indent:(indent + 2) ~inline e)
        new_line spaces
  | ECustom { obj = _; targs = _; tret = _ } -> Format.asprintf "%sECustom" spaces
  | EHole _ -> Format.asprintf "%s□" spaces 
  | _ -> assert false


let rec string_of_trace :
    type d t.
    ?indent:int ->
    (d, t) Trace_ast.t ->
    file =
fun ?(indent=0) trace ->
  let spaces = String.make indent ' ' in
  match trace with
  | TrExpr e -> Format.asprintf "%sTrExpr(%s)" spaces (string_of_expr ~inline:true e)
  | TrLit l -> Format.asprintf "%s%s" spaces (UserFacing.lit_to_string En l)
  | TrApp { trf; trargs; tys = _; vars = _; trv } ->
      Format.asprintf "%sTrApp(\n%s,\n%s,\n%s\n%s)" spaces
        (string_of_trace ~indent:(indent + 2) trf)
        (string_of_list (string_of_trace ~indent:(indent + 4)) trargs (indent + 2))
        (string_of_trace ~indent:(indent + 2) trv)
        spaces
  | TrAppOp { op; trargs; tys = _; vargs; _ } ->
      Format.asprintf "%sTrAppOp(%s,\n%s,\n%s\n%s)" spaces
        (operator_to_string (Mark.remove op))
        (string_of_list (string_of_trace ~indent:(indent + 4)) trargs (indent + 2))
        (string_of_list (string_of_expr ~inline:true) vargs (indent + 2))
        spaces
  | TrArray arr ->
      Format.asprintf "%sTrArray(\n%s\n%s)" spaces
        (string_of_list (string_of_trace ~indent:(indent + 4)) arr (indent + 2))
        spaces
  | TrVar x -> Format.asprintf "%sTrVar(%s)" spaces (Bindlib.name_of x)
  | TrAbs { binder; pos = _; tys = _ } ->
      let (vars, body) = Bindlib.unmbind binder in
      Format.asprintf "%sTrAbs(\n%s  %s,\n%s  %s\n%s)" spaces spaces
        (string_of_list ~inline:true (fun x -> Format.asprintf "%s" (Bindlib.name_of x)) (Array.to_list vars) (indent + 2))
        spaces 
        (string_of_expr ~inline:true body)
        spaces
  | TrIfThenElse { trcond; trtrue; trfalse } ->
      Format.asprintf "%sTrIfThenElse(\n%s,\n%s,\n%s\n%s)" spaces
        (string_of_trace ~indent:(indent + 2) trcond)
        (string_of_trace ~indent:(indent + 2) trtrue)
        (string_of_trace ~indent:(indent + 2) trfalse)
        spaces
  | TrStruct { name; fields } ->
      Format.asprintf "%sTrStruct(%s,\n%s\n%s)" spaces (StructName.to_string name)
        (string_of_container StructField.Map.iter StructField.to_string (string_of_trace ~indent:(indent + 4)) fields (indent+2))
        spaces
  | TrInj { name; tr; cons } ->
      Format.asprintf "%sTrInj(%s.%s,\n%s\n%s)" spaces (EnumName.to_string name) (EnumConstructor.to_string cons)
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrMatch { name; tr; cases } ->
      Format.asprintf "%sTrMatch(%s,\n%s,\n%s\n%s)" spaces (EnumName.to_string name)
        (string_of_trace ~indent:(indent + 2) tr)
        (string_of_container EnumConstructor.Map.iter EnumConstructor.to_string (string_of_trace ~indent:(indent + 4)) cases (indent + 2))
        spaces
  | TrTuple lst ->
      Format.asprintf "%sTrTuple(\n%s\n%s)" spaces
        (string_of_list (string_of_trace ~indent:(indent + 4)) lst (indent + 2))
        spaces
  | TrTupleAccess { tr; index; size } ->
      Format.asprintf "%sTrTupleAccess(%d, %d,\n%s\n%s)" spaces index size
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrStructAccess { name; tr; field } ->
      Format.asprintf "%sTrStructAccess(%s.%s,\n%s\n%s)" spaces
        (StructName.to_string name) (StructField.to_string field)
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrExternal _ -> Format.asprintf "%sTrExternal" spaces
  | TrAssert tr ->
      Format.asprintf "%sTrAssert(\n%s\n%s)" spaces
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrFatalError { err; tr } ->
      Format.asprintf "%sTrFatalError(%s,\n%s\n%s)" spaces (Runtime.error_to_string err)
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrDefault { trexcepts; vexcepts; trjust; trcons } ->
      Format.asprintf "%sTrDefault(\n%s,\n%s,\n%s,\n%s\n%s)" spaces
        (string_of_list (string_of_trace ~indent:(indent + 4)) trexcepts (indent + 2))
        (string_of_list (string_of_expr ~inline:true) vexcepts (indent + 2))
        (string_of_trace ~indent:(indent + 2) trjust)
        (string_of_trace ~indent:(indent + 2) trcons)
        spaces
  | TrPureDefault tr ->
      Format.asprintf "%sTrPureDefault(\n%s\n%s)" spaces
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrEmpty -> Format.asprintf "%s∅" spaces
  | TrErrorOnEmpty tr ->
      Format.asprintf "%sTrErrorOnEmpty(\n%s\n%s)" spaces
        (string_of_trace ~indent:(indent + 2) tr)
        spaces
  | TrCustom { obj = _; targs = _; tret = _ } -> Format.asprintf "%sTrCustom" spaces
  | TrHole _ -> Format.asprintf "%s□" spaces

let print_expr ?(fmt = Message.std_ppf ()) (expr : ('a, 'm) gexpr) =
  Format.fprintf fmt "%a@." (Print.expr ()) expr

let print_trace (trace : ('a, 'm) Trace_ast.t) = print_string (string_of_trace trace)