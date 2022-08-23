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

let is_empty_error (e : 'm Ast.marked_expr) : bool =
  match Marked.unmark e with ELit LEmptyError -> true | _ -> false

let log_indent = ref 0

(** {1 Evaluation} *)

let rec evaluate_operator
    (ctx : decl_ctx)
    (op : operator)
    (pos : Pos.t)
    (args : 'm Ast.marked_expr list) : 'm Ast.expr =
  (* Try to apply [div] and if a [Division_by_zero] exceptions is catched, use
     [op] to raise multispanned errors. *)
  let apply_div_or_raise_err (div : unit -> 'm Ast.expr) : 'm Ast.expr =
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
    | (arg0 :: arg1 :: _ : 'm Ast.marked_expr list) ->
      [None, Expr.pos arg0; None, Expr.pos arg1]
    | _ -> assert false
  in
  (* Try to apply [cmp] and if a [UncomparableDurations] exceptions is catched,
     use [args] to raise multispanned errors. *)
  let apply_cmp_or_raise_err
      (cmp : unit -> 'm Ast.expr)
      (args : 'm Ast.marked_expr list) : 'm Ast.expr =
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
             (Marked.same_mark_as (EApp (List.nth args 0, [acc; e'])) e'))
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
  | Binop (Div KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_div_or_raise_err (fun _ ->
        try ELit (LRat Runtime.(d1 /^ d2))
        with Runtime.IndivisableDurations ->
          Errors.raise_multispanned_error (get_binop_args_pos args)
            "Cannot divide durations that cannot be converted to a precise \
             number of days")
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
  | Binop Eq, [ETuple (es1, s1); ETuple (es2, s2)] ->
    ELit
      (LBool
         (try
            s1 = s2
            && List.for_all2
                 (fun e1 e2 ->
                   match evaluate_operator ctx op pos [e1; e2] with
                   | ELit (LBool b) -> b
                   | _ -> assert false
                   (* should not happen *))
                 es1 es2
          with Invalid_argument _ -> false))
  | Binop Eq, [EInj (e1, i1, en1, _ts1); EInj (e2, i2, en2, _ts2)] ->
    ELit
      (LBool
         (try
            en1 = en2
            && i1 = i2
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
             (Marked.same_mark_as (EApp (List.nth args 0, [e'])) e'))
         es)
  | Binop Filter, [_; EArray es] ->
    EArray
      (List.filter
         (fun e' ->
           match
             evaluate_expr ctx
               (Marked.same_mark_as (EApp (List.nth args 0, [e'])) e')
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

and evaluate_expr (ctx : decl_ctx) (e : 'm Ast.marked_expr) : 'm Ast.marked_expr
    =
  match Marked.unmark e with
  | EVar _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "free variable found at evaluation (should not happen if term was \
       well-typed"
  | EApp (e1, args) -> (
    let e1 = evaluate_expr ctx e1 in
    let args = List.map (evaluate_expr ctx) args in
    match Marked.unmark e1 with
    | EAbs (binder, _) ->
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
  | ETuple (es, s) ->
    let new_es = List.map (evaluate_expr ctx) es in
    if List.exists is_empty_error new_es then
      Marked.same_mark_as (ELit LEmptyError) e
    else Marked.same_mark_as (ETuple (new_es, s)) e
  | ETupleAccess (e1, n, s, _) -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | ETuple (es, s') -> (
      (match s, s' with
      | None, None -> ()
      | Some s, Some s' when s = s' -> ()
      | _ ->
        Errors.raise_multispanned_error
          [None, Expr.pos e; None, Expr.pos e1]
          "Error during tuple access: not the same structs (should not happen \
           if the term was well-typed)");
      match List.nth_opt es n with
      | Some e' -> e'
      | None ->
        Errors.raise_spanned_error (Expr.pos e1)
          "The tuple has %d components but the %i-th element was requested \
           (should not happen if the term was well-type)"
          (List.length es) n)
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e1)
        "The expression %a should be a tuple with %d components but is not \
         (should not happen if the term was well-typed)"
        (Expr.format ctx ~debug:true)
        e n)
  | EInj (e1, n, en, ts) ->
    let e1' = evaluate_expr ctx e1 in
    if is_empty_error e1' then Marked.same_mark_as (ELit LEmptyError) e
    else Marked.same_mark_as (EInj (e1', n, en, ts)) e
  | EMatch (e1, es, e_name) -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | EInj (e1, n, e_name', _) ->
      if e_name <> e_name' then
        Errors.raise_multispanned_error
          [None, Expr.pos e; None, Expr.pos e1]
          "Error during match: two different enums found (should not happend \
           if the term was well-typed)";
      let es_n =
        match List.nth_opt es n with
        | Some es_n -> es_n
        | None ->
          Errors.raise_spanned_error (Expr.pos e)
            "sum type index error (should not happend if the term was \
             well-typed)"
      in
      let new_e = Marked.same_mark_as (EApp (es_n, [e1])) e in
      evaluate_expr ctx new_e
    | ELit LEmptyError -> Marked.same_mark_as (ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (Expr.pos e1)
        "Expected a term having a sum type as an argument to a match (should \
         not happend if the term was well-typed")
  | EDefault (exceptions, just, cons) -> (
    let exceptions = List.map (evaluate_expr ctx) exceptions in
    let empty_count = List.length (List.filter is_empty_error exceptions) in
    match List.length exceptions - empty_count with
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
    | 1 -> List.find (fun sub -> not (is_empty_error sub)) exceptions
    | _ ->
      Errors.raise_multispanned_error
        (List.map
           (fun except ->
             Some "This consequence has a valid justification:", Expr.pos except)
           (List.filter (fun sub -> not (is_empty_error sub)) exceptions))
        "There is a conflict between multiple valid consequences for assigning \
         the same variable.")
  | EIfThenElse (cond, et, ef) -> (
    match Marked.unmark (evaluate_expr ctx cond) with
    | ELit (LBool true) -> evaluate_expr ctx et
    | ELit (LBool false) -> evaluate_expr ctx ef
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
  | ErrorOnEmpty e' ->
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
      | ErrorOnEmpty
          ( EApp
              ((EOp (Binop op), _), [((ELit _, _) as e1); ((ELit _, _) as e2)]),
            _ )
      | EApp
          ( (EOp (Unop (Log _)), _),
            [
              ( EApp
                  ( (EOp (Binop op), _),
                    [((ELit _, _) as e1); ((ELit _, _) as e2)] ),
                _ );
            ] )
      | EApp ((EOp (Binop op), _), [((ELit _, _) as e1); ((ELit _, _) as e2)])
        ->
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
      'm.
      decl_ctx ->
      'm Ast.marked_expr ->
      (Uid.MarkedString.info * 'm Ast.marked_expr) list =
 fun (ctx : decl_ctx) (e : 'm Ast.marked_expr) :
     (Uid.MarkedString.info * 'm Ast.marked_expr) list ->
  match evaluate_expr ctx e with
  | EAbs (_, [((TStruct s_in, _) as targs)]), mark_e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructMap.find s_in ctx.ctx_structs in
    let application_term =
      List.map
        (fun (_, ty) ->
          match Marked.unmark ty with
          | TArrow ((TLit TUnit, _), ty_in) ->
            Expr.empty_thunked_term
              (Expr.map_mark (fun pos -> pos) (fun _ -> ty_in) mark_e)
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
      ( EApp
          ( e,
            [
              ( ETuple (application_term, Some s_in),
                let pos =
                  match application_term with
                  | a :: _ -> Expr.pos a
                  | [] -> Pos.no_pos
                in
                Expr.map_mark (fun _ -> pos) (fun _ -> targs) mark_e );
            ] ),
        Expr.map_mark
          (fun pos -> pos)
          (fun ty ->
            match application_term, ty with
            | [], t_out -> t_out
            | _ :: _, (TArrow (_, t_out), _) -> t_out
            | _ :: _, (_, bad_pos) ->
              Errors.raise_spanned_error bad_pos
                "@[<hv 2>(bug) Result of interpretation doesn't have the \
                 expected type:@ @[%a@]@]"
                (Print.typ ctx) (fst @@ ty))
          mark_e )
    in
    match Marked.unmark (evaluate_expr ctx to_interpret) with
    | ETuple (args, Some s_out) ->
      let s_out_fields =
        List.map
          (fun (f, _) -> StructFieldName.get_info f)
          (StructMap.find s_out ctx.ctx_structs)
      in
      List.map2 (fun arg var -> var, arg) args s_out_fields
    | _ ->
      Errors.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
