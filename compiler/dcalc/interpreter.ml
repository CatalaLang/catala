(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Reference interpreter for the default calculus *)

open Utils
open Shared_ast
module Runtime = Runtime_ocaml.Runtime

(** {1 Helpers} *)

let is_empty_error (e : 'm Ast.expr) : bool =
  match Marked.unmark e with ELit LEmptyError -> true | _ -> false

let log_indent = ref 0

(** {1 Evaluation} *)

let rec evaluate_operator
    (ctx : decl_ctx)
    (op : operator)
    (pos : Pos.t)
    (args : 'm Ast.expr list) : 'm Ast.naked_expr =
  (* Try to apply [div] and if a [Division_by_zero] exceptions is catched, use
     [op] to raise multispanned errors. *)
  let apply_div_or_raise_err (div : unit -> 'm Ast.naked_expr) :
      'm Ast.naked_expr =
    try div ()
    with Division_by_zero ->
      Errors.raise_multispanned_error
        [
          Some "The division operator:", pos;
          Some "The null denominator:", Expr.pos (List.nth args 1);
        ]
        "division by zero at runtime"
  in
  let get_binop_args_pos = function
    | (arg0 :: arg1 :: _ : 'm Ast.expr list) ->
      [None, Expr.pos arg0; None, Expr.pos arg1]
    | _ -> assert false
  in
  (* Try to apply [cmp] and if a [UncomparableDurations] exceptions is catched,
     use [args] to raise multispanned errors. *)
  let apply_cmp_or_raise_err
      (cmp : unit -> 'm Ast.naked_expr)
      (args : 'm Ast.expr list) : 'm Ast.naked_expr =
    try cmp ()
    with Runtime.UncomparableDurations ->
      Errors.raise_multispanned_error (get_binop_args_pos args)
        "Cannot compare together durations that cannot be converted to a \
         precise number of days"
  in
  match op, List.map Marked.unmark args with
  | Ternop Fold, [_f; _init; EArray es] ->
    Marked.unmark
      (List.fold_left
         (fun acc e' ->
           evaluate_expr ctx
             (Marked.same_mark_as
                (EApp { f = List.nth args 0; args = [acc; e'] })
                e'))
         (List.nth args 1) es)
  | Binop And, [ELit (LBool b1); ELit (LBool b2)] -> ELit (LBool (b1 && b2))
  | Binop Or, [ELit (LBool b1); ELit (LBool b2)] -> ELit (LBool (b1 || b2))
  | Binop Xor, [ELit (LBool b1); ELit (LBool b2)] -> ELit (LBool (b1 <> b2))
  | Binop (Add KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LInt Runtime.(i1 +! i2))
  | Binop (Sub KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LInt Runtime.(i1 -! i2))
  | Binop (Mult KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LInt Runtime.(i1 *! i2))
  | Binop (Div KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    apply_div_or_raise_err (fun _ -> ELit (LInt Runtime.(i1 /! i2)))
  | Binop (Add KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LRat Runtime.(i1 +& i2))
  | Binop (Sub KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LRat Runtime.(i1 -& i2))
  | Binop (Mult KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LRat Runtime.(i1 *& i2))
  | Binop (Div KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    apply_div_or_raise_err (fun _ -> ELit (LRat Runtime.(i1 /& i2)))
  | Binop (Add KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LMoney Runtime.(m1 +$ m2))
  | Binop (Sub KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LMoney Runtime.(m1 -$ m2))
  | Binop (Mult KMoney), [ELit (LMoney m1); ELit (LRat m2)] ->
    ELit (LMoney Runtime.(m1 *$ m2))
  | Binop (Div KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    apply_div_or_raise_err (fun _ -> ELit (LRat Runtime.(m1 /$ m2)))
  | Binop (Add KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    ELit (LDuration Runtime.(d1 +^ d2))
  | Binop (Sub KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    ELit (LDuration Runtime.(d1 -^ d2))
  | Binop (Sub KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LDuration Runtime.(d1 -@ d2))
  | Binop (Add KDate), [ELit (LDate d1); ELit (LDuration d2)] ->
    ELit (LDate Runtime.(d1 +@ d2))
  | Binop (Mult KDuration), [ELit (LDuration d1); ELit (LInt i1)] ->
    ELit (LDuration Runtime.(d1 *^ i1))
  | Binop (Lt KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LBool Runtime.(i1 <! i2))
  | Binop (Lte KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LBool Runtime.(i1 <=! i2))
  | Binop (Gt KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LBool Runtime.(i1 >! i2))
  | Binop (Gte KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LBool Runtime.(i1 >=! i2))
  | Binop (Lt KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LBool Runtime.(i1 <& i2))
  | Binop (Lte KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LBool Runtime.(i1 <=& i2))
  | Binop (Gt KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LBool Runtime.(i1 >& i2))
  | Binop (Gte KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LBool Runtime.(i1 >=& i2))
  | Binop (Lt KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LBool Runtime.(m1 <$ m2))
  | Binop (Lte KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LBool Runtime.(m1 <=$ m2))
  | Binop (Gt KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LBool Runtime.(m1 >$ m2))
  | Binop (Gte KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LBool Runtime.(m1 >=$ m2))
  | Binop (Lt KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> ELit (LBool Runtime.(d1 <^ d2))) args
  | Binop (Lte KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> ELit (LBool Runtime.(d1 <=^ d2))) args
  | Binop (Gt KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> ELit (LBool Runtime.(d1 >^ d2))) args
  | Binop (Gte KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> ELit (LBool Runtime.(d1 >=^ d2))) args
  | Binop (Lt KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LBool Runtime.(d1 <@ d2))
  | Binop (Lte KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LBool Runtime.(d1 <=@ d2))
  | Binop (Gt KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LBool Runtime.(d1 >@ d2))
  | Binop (Gte KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LBool Runtime.(d1 >=@ d2))
  | Binop Eq, [ELit LUnit; ELit LUnit] -> ELit (LBool true)
  | Binop Eq, [ELit (LDuration d1); ELit (LDuration d2)] ->
    ELit (LBool Runtime.(d1 =^ d2))
  | Binop Eq, [ELit (LDate d1); ELit (LDate d2)] ->
    ELit (LBool Runtime.(d1 =@ d2))
  | Binop Eq, [ELit (LMoney m1); ELit (LMoney m2)] ->
    ELit (LBool Runtime.(m1 =$ m2))
  | Binop Eq, [ELit (LRat i1); ELit (LRat i2)] ->
    ELit (LBool Runtime.(i1 =& i2))
  | Binop Eq, [ELit (LInt i1); ELit (LInt i2)] ->
    ELit (LBool Runtime.(i1 =! i2))
  | Binop Eq, [ELit (LBool b1); ELit (LBool b2)] -> ELit (LBool (b1 = b2))
  | Binop Eq, [EArray es1; EArray es2] ->
    ELit
      (LBool
         (try
            List.for_all2
              (fun e1 e2 ->
                match evaluate_operator ctx op pos [e1; e2] with
                | ELit (LBool b) -> b
                | _ -> assert false
                (* should not happen *))
              es1 es2
          with Invalid_argument _ -> false))
  | ( Binop Eq,
      [EStruct { fields = es1; name = s1 }; EStruct { fields = es2; name = s2 }]
    ) ->
    ELit
      (LBool
         (StructName.equal s1 s2
         && StructFieldMap.equal
              (fun e1 e2 ->
                match evaluate_operator ctx op pos [e1; e2] with
                | ELit (LBool b) -> b
                | _ -> assert false
                (* should not happen *))
              es1 es2))
  | ( Binop Eq,
      [
        EInj { e = e1; cons = i1; name = en1 };
        EInj { e = e2; cons = i2; name = en2 };
      ] ) ->
    ELit
      (LBool
         (try
            EnumName.equal en1 en2
            && EnumConstructor.equal i1 i2
            &&
            match evaluate_operator ctx op pos [e1; e2] with
            | ELit (LBool b) -> b
            | _ -> assert false
            (* should not happen *)
          with Invalid_argument _ -> false))
  | Binop Eq, [_; _] ->
    ELit (LBool false) (* comparing anything else return false *)
  | Binop Neq, [_; _] -> (
    match evaluate_operator ctx (Binop Eq) pos args with
    | ELit (LBool b) -> ELit (LBool (not b))
    | _ -> assert false (*should not happen *))
  | Binop Concat, [EArray es1; EArray es2] -> EArray (es1 @ es2)
  | Binop Map, [_; EArray es] ->
    EArray
      (List.map
         (fun e' ->
           evaluate_expr ctx
             (Marked.same_mark_as (EApp { f = List.hd args; args = [e'] }) e'))
         es)
  | Binop Filter, [_; EArray es] ->
    EArray
      (List.filter
         (fun e' ->
           match
             evaluate_expr ctx
               (Marked.same_mark_as (EApp { f = List.hd args; args = [e'] }) e')
           with
           | ELit (LBool b), _ -> b
           | _ ->
             Errors.raise_spanned_error
               (Expr.pos (List.nth args 0))
               "This predicate evaluated to something else than a boolean \
                (should not happen if the term was well-typed)")
         es)
  | Binop _, ([ELit LEmptyError; _] | [_; ELit LEmptyError]) -> ELit LEmptyError
  | Unop (Minus KInt), [ELit (LInt i)] ->
    ELit (LInt Runtime.(integer_of_int 0 -! i))
  | Unop (Minus KRat), [ELit (LRat i)] ->
    ELit (LRat Runtime.(decimal_of_string "0" -& i))
  | Unop (Minus KMoney), [ELit (LMoney i)] ->
    ELit (LMoney Runtime.(money_of_units_int 0 -$ i))
  | Unop (Minus KDuration), [ELit (LDuration i)] ->
    ELit (LDuration Runtime.(~-^i))
  | Unop Not, [ELit (LBool b)] -> ELit (LBool (not b))
  | Unop Length, [EArray es] ->
    ELit (LInt (Runtime.integer_of_int (List.length es)))
  | Unop GetDay, [ELit (LDate d)] ->
    ELit (LInt Runtime.(day_of_month_of_date d))
  | Unop GetMonth, [ELit (LDate d)] ->
    ELit (LInt Runtime.(month_number_of_date d))
  | Unop GetYear, [ELit (LDate d)] -> ELit (LInt Runtime.(year_of_date d))
  | Unop FirstDayOfMonth, [ELit (LDate d)] ->
    ELit (LDate Runtime.(first_day_of_month d))
  | Unop LastDayOfMonth, [ELit (LDate d)] ->
    ELit (LDate Runtime.(first_day_of_month d))
  | Unop IntToRat, [ELit (LInt i)] -> ELit (LRat Runtime.(decimal_of_integer i))
  | Unop MoneyToRat, [ELit (LMoney i)] ->
    ELit (LRat Runtime.(decimal_of_money i))
  | Unop RatToMoney, [ELit (LRat i)] ->
    ELit (LMoney Runtime.(money_of_decimal i))
  | Unop RoundMoney, [ELit (LMoney m)] -> ELit (LMoney Runtime.(money_round m))
  | Unop RoundDecimal, [ELit (LRat m)] -> ELit (LRat Runtime.(decimal_round m))
  | Unop (Log (entry, infos)), [e'] ->
    if !Cli.trace_flag then (
      match entry with
      | VarDef _ ->
        (* TODO: this usage of Format is broken, Formatting requires that all is
           formatted in one pass, without going through intermediate "%s" *)
        Cli.log_format "%*s%a %a: %s" (!log_indent * 2) "" Print.log_entry entry
          Print.uid_list infos
          (match e' with
          | EAbs _ -> Cli.with_style [ANSITerminal.green] "<function>"
          | _ ->
            let expr_str =
              Format.asprintf "%a" (Expr.format ctx ~debug:false) (List.hd args)
            in
            let expr_str =
              Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
                ~subst:(fun _ -> " ")
                expr_str
            in
            Cli.with_style [ANSITerminal.green] "%s" expr_str)
      | PosRecordIfTrueBool -> (
        match pos <> Pos.no_pos, e' with
        | true, ELit (LBool true) ->
          Cli.log_format "%*s%a%s:\n%s" (!log_indent * 2) "" Print.log_entry
            entry
            (Cli.with_style [ANSITerminal.green] "Definition applied")
            (Cli.add_prefix_to_each_line (Pos.retrieve_loc_text pos) (fun _ ->
                 Format.asprintf "%*s" (!log_indent * 2) ""))
        | _ -> ())
      | BeginCall ->
        Cli.log_format "%*s%a %a" (!log_indent * 2) "" Print.log_entry entry
          Print.uid_list infos;
        log_indent := !log_indent + 1
      | EndCall ->
        log_indent := !log_indent - 1;
        Cli.log_format "%*s%a %a" (!log_indent * 2) "" Print.log_entry entry
          Print.uid_list infos)
    else ();
    e'
  | Unop _, [ELit LEmptyError] -> ELit LEmptyError
  | _ ->
    Errors.raise_multispanned_error
      ([Some "Operator:", pos]
      @ List.mapi
          (fun i arg ->
            ( Some
                (Format.asprintf "Argument n°%d, value %a" (i + 1)
                   (Expr.format ctx ~debug:true)
                   arg),
              Expr.pos arg ))
          args)
      "Operator applied to the wrong arguments\n\
       (should not happen if the term was well-typed)"

and evaluate_expr (ctx : decl_ctx) (e : 'm Ast.expr) : 'm Ast.expr =
  match Marked.unmark e with
  | EVar _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "free variable found at evaluation (should not happen if term was \
       well-typed"
  | EApp { f = e1; args } -> (
    let e1 = evaluate_expr ctx e1 in
    let args = List.map (evaluate_expr ctx) args in
    match Marked.unmark e1 with
    | EAbs { binder; _ } ->
      if Bindlib.mbinder_arity binder = List.length args then
        evaluate_expr ctx
          (Bindlib.msubst binder (Array.of_list (List.map Marked.unmark args)))
      else
        Errors.raise_spanned_error (Expr.pos e)
          "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | EOp op ->
      Marked.same_mark_as (evaluate_operator ctx op (Expr.pos e) args) e
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e)
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed")
  | EAbs _ | ELit _ | EOp _ -> e (* these are values *)
  | EStruct { fields = es; name } ->
    let new_es = StructFieldMap.map (evaluate_expr ctx) es in
    if StructFieldMap.exists (fun _ e -> is_empty_error e) new_es then
      Marked.same_mark_as (ELit LEmptyError) e
    else Marked.same_mark_as (EStruct { fields = new_es; name }) e
  | EStructAccess { e = e1; name = s; field } -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | EStruct { fields = es; name = s' } -> (
      if not (StructName.equal s s') then
        Errors.raise_multispanned_error
          [None, Expr.pos e; None, Expr.pos e1]
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      match StructFieldMap.find_opt field es with
      | Some e' -> e'
      | None ->
        Errors.raise_spanned_error (Expr.pos e1)
          "Invalid field access %a in struct %a (should not happen if the term \
           was well-typed)"
          StructFieldName.format_t field StructName.format_t s)
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e1)
        "The expression %a should be a struct %a but is not (should not happen \
         if the term was well-typed)"
        (Expr.format ctx ~debug:true)
        e StructName.format_t s)
  | EInj { e = e1; name; cons } ->
    let e1' = evaluate_expr ctx e1 in
    if is_empty_error e then Marked.same_mark_as (ELit LEmptyError) e
    else Marked.same_mark_as (EInj { e = e1'; name; cons }) e
  | EMatch { e = e1; cases = es; name } -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | EInj { e = e1; cons; name = name' } ->
      if not (EnumName.equal name name') then
        Errors.raise_multispanned_error
          [None, Expr.pos e; None, Expr.pos e1]
          "Error during match: two different enums found (should not happen if \
           the term was well-typed)";
      let es_n =
        match EnumConstructorMap.find_opt cons es with
        | Some es_n -> es_n
        | None ->
          Errors.raise_spanned_error (Expr.pos e)
            "sum type index error (should not happen if the term was \
             well-typed)"
      in
      let new_e = Marked.same_mark_as (EApp { f = es_n; args = [e1] }) e in
      evaluate_expr ctx new_e
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e1)
        "Expected a term having a sum type as an argument to a match (should \
         not happen if the term was well-typed")
  | EDefault { excepts; just; cons } -> (
    let excepts = List.map (evaluate_expr ctx) excepts in
    let empty_count = List.length (List.filter is_empty_error excepts) in
    match List.length excepts - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx just in
      match Marked.unmark just with
      | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
      | ELit (LBool true) -> evaluate_expr ctx cons
      | ELit (LBool false) -> Marked.same_mark_as (ELit LEmptyError) e
      | _ ->
        Errors.raise_spanned_error (Expr.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 -> List.find (fun sub -> not (is_empty_error sub)) excepts
    | _ ->
      Errors.raise_multispanned_error
        (List.map
           (fun except ->
             Some "This consequence has a valid justification:", Expr.pos except)
           (List.filter (fun sub -> not (is_empty_error sub)) excepts))
        "There is a conflict between multiple valid consequences for assigning \
         the same variable.")
  | EIfThenElse { cond; etrue; efalse } -> (
    match Marked.unmark (evaluate_expr ctx cond) with
    | ELit (LBool true) -> evaluate_expr ctx etrue
    | ELit (LBool false) -> evaluate_expr ctx efalse
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos cond)
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray es ->
    let new_es = List.map (evaluate_expr ctx) es in
    if List.exists is_empty_error new_es then
      Marked.same_mark_as (ELit LEmptyError) e
    else Marked.same_mark_as (EArray new_es) e
  | EErrorOnEmpty e' ->
    let e' = evaluate_expr ctx e' in
    if Marked.unmark e' = ELit LEmptyError then
      Errors.raise_spanned_error (Expr.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    else e'
  | EAssert e' -> (
    match Marked.unmark (evaluate_expr ctx e') with
    | ELit (LBool true) -> Marked.same_mark_as (ELit LUnit) e'
    | ELit (LBool false) -> (
      match Marked.unmark e' with
      | EErrorOnEmpty
          ( EApp
              {
                f = EOp (Binop op), _;
                args = [((ELit _, _) as e1); ((ELit _, _) as e2)];
              },
            _ )
      | EApp
          {
            f = EOp (Unop (Log _)), _;
            args =
              [
                ( EApp
                    {
                      f = EOp (Binop op), _;
                      args = [((ELit _, _) as e1); ((ELit _, _) as e2)];
                    },
                  _ );
              ];
          }
      | EApp
          {
            f = EOp (Binop op), _;
            args = [((ELit _, _) as e1); ((ELit _, _) as e2)];
          } ->
        Errors.raise_spanned_error (Expr.pos e') "Assertion failed: %a %a %a"
          (Expr.format ctx ~debug:false)
          e1 Print.binop op
          (Expr.format ctx ~debug:false)
          e2
      | _ ->
        Cli.debug_format "%a" (Expr.format ctx) e';
        Errors.raise_spanned_error (Expr.pos e') "Assertion failed")
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e')
        "Expected a boolean literal for the result of this assertion (should \
         not happen if the term was well-typed)")

(** {1 API} *)

let interpret_program :
      'm. decl_ctx -> 'm Ast.expr -> (Uid.MarkedString.info * 'm Ast.expr) list
    =
 fun (ctx : decl_ctx) (e : 'm Ast.expr) :
     (Uid.MarkedString.info * 'm Ast.expr) list ->
  match evaluate_expr ctx e with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructMap.find s_in ctx.ctx_structs in
    let application_term =
      StructFieldMap.map
        (fun ty ->
          match Marked.unmark ty with
          | TArrow ((TLit TUnit, _), ty_in) ->
            Expr.empty_thunked_term (Expr.with_ty mark_e ty_in)
          | _ ->
            Errors.raise_spanned_error (Marked.get_mark ty)
              "This scope needs input arguments to be executed. But the Catala \
               built-in interpreter does not have a way to retrieve input \
               values from the command line, so it cannot execute this scope. \
               Please create another scope thatprovide the input arguments to \
               this one and execute it instead. ")
        taus
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct s_in application_term mark_e]
        (Expr.pos e)
    in
    match Marked.unmark (evaluate_expr ctx (Expr.unbox to_interpret)) with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructFieldName.get_info fld, e)
        (StructFieldMap.bindings fields)
    | _ ->
      Errors.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
