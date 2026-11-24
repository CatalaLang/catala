open Catala_utils
open Shared_ast
open Catala_runtime

module RuntimeError = struct
  type t = {
    error : Catala_runtime.error;
    pos : Pos.t;
    extra_pos : string Mark.pos list;
  }

  let make (error : Catala_runtime.error) pos extra_pos =
    { error; pos; extra_pos }

  let format fmt { error; pos; extra_pos } =
    let open Format in
    let pp_extra_pos fmt (msg, pos) =
      fprintf fmt "%s:@ %a" msg (Pos.format_loc_text ()) pos
    in
    pp_open_vbox fmt 2;
    fprintf fmt "@[<h>↯%s↯ at %s@]" (error_to_string error)
      (Pos.to_string_shorter pos);
    if extra_pos = [] then pp_close_box fmt ()
    else (
      pp_print_space fmt ();
      pp_print_list ~pp_sep:pp_print_space pp_extra_pos fmt extra_pos;
      pp_close_box fmt ())
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

  let mk_emptyerror pos =
    let err = RuntimeError.(make NoValue pos []) in
    Symb_error err

  let mk_conflicterror pos extra_pos =
    let err = RuntimeError.make Conflict pos extra_pos in
    Symb_error err

  let mk_divisionbyzeroerror pos extra_pos =
    let open RuntimeError in
    let err = make DivisionByZero pos extra_pos in
    Symb_error err

  let mk_assertionerror pos =
    let open RuntimeError in
    let err = make AssertionFailed pos [] in
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
    Format.fprintf fmt "<%a>" StructField.format name

  let rec format_sexpr fmt e =
    let open Format in
    let e = Z3.Expr.simplify e None in
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
    | Symb_error err -> RuntimeError.format fmt err

  let to_string ?(typed = false) e = Format.asprintf "%a" (format ~typed) e

  let format_typed (fmt : Format.formatter) (symb_expr : t) : unit =
    format ~typed:true fmt symb_expr

  let format (fmt : Format.formatter) (symb_expr : t) : unit =
    format ~typed:false fmt symb_expr
end
