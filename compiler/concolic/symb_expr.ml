open Catala_utils
open Shared_ast

module RuntimeError = struct
  type span = string option * Pos.t
  type span_list = span list

  type runtime_error =
    | EmptyError
    | ConflictError of { spans : span_list }
    | DivisionByZeroError of { spans : span_list }  (** TODO factorize? *)
    | AssertionError

  type message = string

  type t = {
    except : runtime_error; (* TODO use actual exceptions from [Runtime]? *)
    message : message; (* TODO use formatted stuff instead *)
  }

  let make (except : runtime_error) (message : message) = { except; message }

  (* TODO use formatter *)
  let string_of_span (s : span) : string =
    let _, pos = s in
    Pos.to_string_short pos

  let string_of_spans (spans : span_list) : string =
    let span_strings = List.map string_of_span spans in
    List.fold_left (fun acc a -> a ^ "," ^ acc) "" span_strings

  let to_string { except; _ } =
    let except_string =
      match except with
      | EmptyError -> "Empty"
      | ConflictError { spans } -> "Conflict(" ^ string_of_spans spans ^ ")"
      | DivisionByZeroError { spans } ->
        "DivisionByZero(" ^ string_of_spans spans ^ ")"
      | AssertionError -> "AssertionError"
    in
    "↯" ^ except_string ^ "↯"
end

module SymbExpr = struct
  type z3_expr = Z3.Expr.expr
  type z3_symbol = Z3.Symbol.symbol
  type reentrant = { name : StructField.t; symbol : z3_expr }

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_incomplete
    | Symb_abs
    | Symb_error of RuntimeError.t
        (** TODO make sure that this can only be used on errors? *)

  let mk_z3 s = Symb_z3 s
  let mk_reentrant name symbol = Symb_reentrant { name; symbol }
  let is_reentrant = function Symb_reentrant _ -> true | _ -> false
  let none = Symb_none
  let incomplete = Symb_incomplete
  let abs = Symb_abs

  let mk_emptyerror message =
    let err = RuntimeError.(make EmptyError message) in
    Symb_error err

  let mk_conflicterror message spans =
    let open RuntimeError in
    let conflict = ConflictError { spans } in
    let err = make conflict message in
    Symb_error err

  let mk_divisionbyzeroerror message spans =
    let open RuntimeError in
    let conflict = DivisionByZeroError { spans } in
    let err = make conflict message in
    Symb_error err

  let mk_assertionerror message =
    let open RuntimeError in
    let conflict = AssertionError in
    let err = make conflict message in
    Symb_error err

  let map_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | x -> x

  let app_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | Symb_incomplete -> Symb_incomplete
    | _ ->
      invalid_arg "[SymbExpr.app_z3] expected a z3 expression or incomplete"

  let app2_z3 (f : z3_expr -> z3_expr -> z3_expr) e1 e2 =
    match e1, e2 with
    | Symb_incomplete, _ | _, Symb_incomplete -> Symb_incomplete
    | Symb_z3 e1, Symb_z3 e2 -> Symb_z3 (f e1 e2)
    | _ ->
      invalid_arg
        "[SymbExpr.app2_z3] expected two z3 expressions or an incomplete"

  let applist_z3 (f : z3_expr list -> z3_expr) (l : t list) =
    let extract_z3 = function
      | Symb_z3 e -> e
      | _ -> invalid_arg "[SymbExpr.applist_z3] expected z3 expressions"
    in
    let l_z3 = List.map extract_z3 l in
    Symb_z3 (f l_z3)

  let map_none ~none = function Symb_none -> none | e -> e
  let simplify = map_z3 (fun e -> Z3.Expr.simplify e None)

  let format_reentrant fmt { name; _ } =
    Format.fprintf fmt "<%a>#" StructField.format name

  let rec format_sexpr fmt e =
    let open Format in
    let n = Z3.Expr.get_num_args e in
    if n = 0 then pp_print_string fmt (Z3.Expr.to_string e)
    else
      let f = Z3.Expr.get_func_decl e in
      let args = Z3.Expr.get_args e in
      let f_name = Z3.Symbol.get_string (Z3.FuncDecl.get_name f) in
      let is_binop =
        String.length f_name <= 2
        &&
        let c = Char.code f_name.[0] in
        c < 0x41 || c > 0x7A
      in
      if n <> 2 || not is_binop then
        fprintf fmt "(%s %a)" f_name
          (Format.pp_print_list ~pp_sep:pp_print_space format_sexpr)
          args
      else
        let e1, e2 = match args with [e1; e2] -> e1, e2 | _ -> assert false in
        fprintf fmt "(%a %s %a)" format_sexpr e1 f_name format_sexpr e2

  (* TODO formatter *)
  let format ~typed fmt e =
    let open Format in
    match e with
    | Symb_z3 s ->
      if not typed then fprintf fmt "%a" format_sexpr s
      else
        fprintf fmt "(%a : %s)" format_sexpr s
          (Z3.Sort.to_string (Z3.Expr.get_sort s))
    | Symb_reentrant r -> format_reentrant fmt r
    | Symb_none -> pp_print_string fmt "None"
    | Symb_incomplete -> pp_print_string fmt "Incomplete"
    | Symb_abs -> pp_print_string fmt "Abs"
    | Symb_error err -> pp_print_string fmt (RuntimeError.to_string err)

  let to_string ?(typed = false) e = Format.asprintf "%a" (format ~typed) e

  let format_typed (fmt : Format.formatter) (symb_expr : t) : unit =
    format ~typed:true fmt symb_expr

  let format (fmt : Format.formatter) (symb_expr : t) : unit =
    format ~typed:false fmt symb_expr
end
