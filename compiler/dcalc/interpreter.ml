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
module A = Ast
module Runtime = Runtime_ocaml.Runtime

(** {1 Helpers} *)

let is_empty_error (e : 'm A.marked_expr) : bool =
  match Marked.unmark e with ELit LEmptyError -> true | _ -> false

let log_indent = ref 0

(** {1 Evaluation} *)

let rec evaluate_operator
    (ctx : Ast.decl_ctx)
    (op : A.operator)
    (pos : Pos.t)
    (args : 'm A.marked_expr list) : 'm A.expr =
  (* Try to apply [div] and if a [Division_by_zero] exceptions is catched, use
     [op] to raise multispanned errors. *)
  let apply_div_or_raise_err (div : unit -> 'm A.expr) : 'm A.expr =
    try div ()
    with Division_by_zero ->
      Errors.raise_multispanned_error
        [
          Some "The division operator:", pos;
          Some "The null denominator:", Ast.pos (List.nth args 1);
        ]
        "division by zero at runtime"
  in
  let get_binop_args_pos = function
    | (arg0 :: arg1 :: _ : 'm A.marked_expr list) ->
      [None, Ast.pos arg0; None, Ast.pos arg1]
    | _ -> assert false
  in
  (* Try to apply [cmp] and if a [UncomparableDurations] exceptions is catched,
     use [args] to raise multispanned errors. *)
  let apply_cmp_or_raise_err
      (cmp : unit -> 'm A.expr)
      (args : 'm A.marked_expr list) : 'm A.expr =
    try cmp ()
    with Runtime.UncomparableDurations ->
      Errors.raise_multispanned_error (get_binop_args_pos args)
        "Cannot compare together durations that cannot be converted to a \
         precise number of days"
  in
  match op, List.map Marked.unmark args with
  | A.Ternop A.Fold, [_f; _init; EArray es] ->
    Marked.unmark
      (List.fold_left
         (fun acc e' ->
           evaluate_expr ctx
             (Marked.same_mark_as (A.EApp (List.nth args 0, [acc; e'])) e'))
         (List.nth args 1) es)
  | A.Binop A.And, [ELit (LBool b1); ELit (LBool b2)] ->
    A.ELit (LBool (b1 && b2))
  | A.Binop A.Or, [ELit (LBool b1); ELit (LBool b2)] ->
    A.ELit (LBool (b1 || b2))
  | A.Binop A.Xor, [ELit (LBool b1); ELit (LBool b2)] ->
    A.ELit (LBool (b1 <> b2))
  | A.Binop (A.Add KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LInt Runtime.(i1 +! i2))
  | A.Binop (A.Sub KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LInt Runtime.(i1 -! i2))
  | A.Binop (A.Mult KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LInt Runtime.(i1 *! i2))
  | A.Binop (A.Div KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    apply_div_or_raise_err (fun _ -> A.ELit (LInt Runtime.(i1 /! i2)))
  | A.Binop (A.Add KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LRat Runtime.(i1 +& i2))
  | A.Binop (A.Sub KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LRat Runtime.(i1 -& i2))
  | A.Binop (A.Mult KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LRat Runtime.(i1 *& i2))
  | A.Binop (A.Div KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    apply_div_or_raise_err (fun _ -> A.ELit (LRat Runtime.(i1 /& i2)))
  | A.Binop (A.Add KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LMoney Runtime.(m1 +$ m2))
  | A.Binop (A.Sub KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LMoney Runtime.(m1 -$ m2))
  | A.Binop (A.Mult KMoney), [ELit (LMoney m1); ELit (LRat m2)] ->
    A.ELit (LMoney Runtime.(m1 *$ m2))
  | A.Binop (A.Div KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    apply_div_or_raise_err (fun _ -> A.ELit (LRat Runtime.(m1 /$ m2)))
  | A.Binop (A.Add KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    A.ELit (LDuration Runtime.(d1 +^ d2))
  | A.Binop (A.Sub KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    A.ELit (LDuration Runtime.(d1 -^ d2))
  | A.Binop (A.Sub KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LDuration Runtime.(d1 -@ d2))
  | A.Binop (A.Add KDate), [ELit (LDate d1); ELit (LDuration d2)] ->
    A.ELit (LDate Runtime.(d1 +@ d2))
  | A.Binop (A.Div KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_div_or_raise_err (fun _ ->
        try A.ELit (LRat Runtime.(d1 /^ d2))
        with Runtime.IndivisableDurations ->
          Errors.raise_multispanned_error (get_binop_args_pos args)
            "Cannot divide durations that cannot be converted to a precise \
             number of days")
  | A.Binop (A.Mult KDuration), [ELit (LDuration d1); ELit (LInt i1)] ->
    A.ELit (LDuration Runtime.(d1 *^ i1))
  | A.Binop (A.Lt KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LBool Runtime.(i1 <! i2))
  | A.Binop (A.Lte KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LBool Runtime.(i1 <=! i2))
  | A.Binop (A.Gt KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LBool Runtime.(i1 >! i2))
  | A.Binop (A.Gte KInt), [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LBool Runtime.(i1 >=! i2))
  | A.Binop (A.Lt KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LBool Runtime.(i1 <& i2))
  | A.Binop (A.Lte KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LBool Runtime.(i1 <=& i2))
  | A.Binop (A.Gt KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LBool Runtime.(i1 >& i2))
  | A.Binop (A.Gte KRat), [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LBool Runtime.(i1 >=& i2))
  | A.Binop (A.Lt KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LBool Runtime.(m1 <$ m2))
  | A.Binop (A.Lte KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LBool Runtime.(m1 <=$ m2))
  | A.Binop (A.Gt KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LBool Runtime.(m1 >$ m2))
  | A.Binop (A.Gte KMoney), [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LBool Runtime.(m1 >=$ m2))
  | A.Binop (A.Lt KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> A.ELit (LBool Runtime.(d1 <^ d2))) args
  | A.Binop (A.Lte KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> A.ELit (LBool Runtime.(d1 <=^ d2))) args
  | A.Binop (A.Gt KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> A.ELit (LBool Runtime.(d1 >^ d2))) args
  | A.Binop (A.Gte KDuration), [ELit (LDuration d1); ELit (LDuration d2)] ->
    apply_cmp_or_raise_err (fun _ -> A.ELit (LBool Runtime.(d1 >=^ d2))) args
  | A.Binop (A.Lt KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LBool Runtime.(d1 <@ d2))
  | A.Binop (A.Lte KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LBool Runtime.(d1 <=@ d2))
  | A.Binop (A.Gt KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LBool Runtime.(d1 >@ d2))
  | A.Binop (A.Gte KDate), [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LBool Runtime.(d1 >=@ d2))
  | A.Binop A.Eq, [ELit LUnit; ELit LUnit] -> A.ELit (LBool true)
  | A.Binop A.Eq, [ELit (LDuration d1); ELit (LDuration d2)] ->
    A.ELit (LBool Runtime.(d1 =^ d2))
  | A.Binop A.Eq, [ELit (LDate d1); ELit (LDate d2)] ->
    A.ELit (LBool Runtime.(d1 =@ d2))
  | A.Binop A.Eq, [ELit (LMoney m1); ELit (LMoney m2)] ->
    A.ELit (LBool Runtime.(m1 =$ m2))
  | A.Binop A.Eq, [ELit (LRat i1); ELit (LRat i2)] ->
    A.ELit (LBool Runtime.(i1 =& i2))
  | A.Binop A.Eq, [ELit (LInt i1); ELit (LInt i2)] ->
    A.ELit (LBool Runtime.(i1 =! i2))
  | A.Binop A.Eq, [ELit (LBool b1); ELit (LBool b2)] -> A.ELit (LBool (b1 = b2))
  | A.Binop A.Eq, [EArray es1; EArray es2] ->
    A.ELit
      (LBool
         (try
            List.for_all2
              (fun e1 e2 ->
                match evaluate_operator ctx op pos [e1; e2] with
                | A.ELit (LBool b) -> b
                | _ -> assert false
                (* should not happen *))
              es1 es2
          with Invalid_argument _ -> false))
  | A.Binop A.Eq, [ETuple (es1, s1); ETuple (es2, s2)] ->
    A.ELit
      (LBool
         (try
            s1 = s2
            && List.for_all2
                 (fun e1 e2 ->
                   match evaluate_operator ctx op pos [e1; e2] with
                   | A.ELit (LBool b) -> b
                   | _ -> assert false
                   (* should not happen *))
                 es1 es2
          with Invalid_argument _ -> false))
  | A.Binop A.Eq, [EInj (e1, i1, en1, _ts1); EInj (e2, i2, en2, _ts2)] ->
    A.ELit
      (LBool
         (try
            en1 = en2
            && i1 = i2
            &&
            match evaluate_operator ctx op pos [e1; e2] with
            | A.ELit (LBool b) -> b
            | _ -> assert false
            (* should not happen *)
          with Invalid_argument _ -> false))
  | A.Binop A.Eq, [_; _] ->
    A.ELit (LBool false) (* comparing anything else return false *)
  | A.Binop A.Neq, [_; _] -> (
    match evaluate_operator ctx (A.Binop A.Eq) pos args with
    | A.ELit (A.LBool b) -> A.ELit (A.LBool (not b))
    | _ -> assert false (*should not happen *))
  | A.Binop A.Concat, [A.EArray es1; A.EArray es2] -> A.EArray (es1 @ es2)
  | A.Binop A.Map, [_; A.EArray es] ->
    A.EArray
      (List.map
         (fun e' ->
           evaluate_expr ctx
             (Marked.same_mark_as (A.EApp (List.nth args 0, [e'])) e'))
         es)
  | A.Binop A.Filter, [_; A.EArray es] ->
    A.EArray
      (List.filter
         (fun e' ->
           match
             evaluate_expr ctx
               (Marked.same_mark_as (A.EApp (List.nth args 0, [e'])) e')
           with
           | A.ELit (A.LBool b), _ -> b
           | _ ->
             Errors.raise_spanned_error
               (A.pos (List.nth args 0))
               "This predicate evaluated to something else than a boolean \
                (should not happen if the term was well-typed)")
         es)
  | A.Binop _, ([ELit LEmptyError; _] | [_; ELit LEmptyError]) ->
    A.ELit LEmptyError
  | A.Unop (A.Minus KInt), [ELit (LInt i)] ->
    A.ELit (LInt Runtime.(integer_of_int 0 -! i))
  | A.Unop (A.Minus KRat), [ELit (LRat i)] ->
    A.ELit (LRat Runtime.(decimal_of_string "0" -& i))
  | A.Unop (A.Minus KMoney), [ELit (LMoney i)] ->
    A.ELit (LMoney Runtime.(money_of_units_int 0 -$ i))
  | A.Unop (A.Minus KDuration), [ELit (LDuration i)] ->
    A.ELit (LDuration Runtime.(~-^i))
  | A.Unop A.Not, [ELit (LBool b)] -> A.ELit (LBool (not b))
  | A.Unop A.Length, [EArray es] ->
    A.ELit (LInt (Runtime.integer_of_int (List.length es)))
  | A.Unop A.GetDay, [ELit (LDate d)] ->
    A.ELit (LInt Runtime.(day_of_month_of_date d))
  | A.Unop A.GetMonth, [ELit (LDate d)] ->
    A.ELit (LInt Runtime.(month_number_of_date d))
  | A.Unop A.GetYear, [ELit (LDate d)] -> A.ELit (LInt Runtime.(year_of_date d))
  | A.Unop A.FirstDayOfMonth, [ELit (LDate d)] ->
    A.ELit (LDate Runtime.(first_day_of_month d))
  | A.Unop A.LastDayOfMonth, [ELit (LDate d)] ->
    A.ELit (LDate Runtime.(first_day_of_month d))
  | A.Unop A.IntToRat, [ELit (LInt i)] ->
    A.ELit (LRat Runtime.(decimal_of_integer i))
  | A.Unop A.MoneyToRat, [ELit (LMoney i)] ->
    A.ELit (LRat Runtime.(decimal_of_money i))
  | A.Unop A.RatToMoney, [ELit (LRat i)] ->
    A.ELit (LMoney Runtime.(money_of_decimal i))
  | A.Unop A.RoundMoney, [ELit (LMoney m)] ->
    A.ELit (LMoney Runtime.(money_round m))
  | A.Unop A.RoundDecimal, [ELit (LRat m)] ->
    A.ELit (LRat Runtime.(decimal_round m))
  | A.Unop (A.Log (entry, infos)), [e'] ->
    if !Cli.trace_flag then (
      match entry with
      | VarDef _ ->
        (* TODO: this usage of Format is broken, Formatting requires that all is
           formatted in one pass, without going through intermediate "%s" *)
        Cli.log_format "%*s%a %a: %s" (!log_indent * 2) ""
          Print.format_log_entry entry Print.format_uid_list infos
          (match e' with
          (* | Ast.EAbs _ -> Cli.with_style [ ANSITerminal.green ]
             "<function>" *)
          | _ ->
            let expr_str =
              Format.asprintf "%a"
                (Print.format_expr ctx ~debug:false)
                (List.hd args)
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
          Cli.log_format "%*s%a%s:\n%s" (!log_indent * 2) ""
            Print.format_log_entry entry
            (Cli.with_style [ANSITerminal.green] "Definition applied")
            (Cli.add_prefix_to_each_line (Pos.retrieve_loc_text pos) (fun _ ->
                 Format.asprintf "%*s" (!log_indent * 2) ""))
        | _ -> ())
      | BeginCall ->
        Cli.log_format "%*s%a %a" (!log_indent * 2) "" Print.format_log_entry
          entry Print.format_uid_list infos;
        log_indent := !log_indent + 1
      | EndCall ->
        log_indent := !log_indent - 1;
        Cli.log_format "%*s%a %a" (!log_indent * 2) "" Print.format_log_entry
          entry Print.format_uid_list infos)
    else ();
    e'
  | A.Unop _, [ELit LEmptyError] -> A.ELit LEmptyError
  | _ ->
    Errors.raise_multispanned_error
      ([Some "Operator:", pos]
      @ List.mapi
          (fun i arg ->
            ( Some
                (Format.asprintf "Argument n°%d, value %a" (i + 1)
                   (Print.format_expr ctx ~debug:true)
                   arg),
              A.pos arg ))
          args)
      "Operator applied to the wrong arguments\n\
       (should not happen if the term was well-typed)"

and evaluate_expr (ctx : Ast.decl_ctx) (e : 'm A.marked_expr) : 'm A.marked_expr
    =
  match Marked.unmark e with
  | EVar _ ->
    Errors.raise_spanned_error (A.pos e)
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
        Errors.raise_spanned_error (A.pos e)
          "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | EOp op -> Marked.same_mark_as (evaluate_operator ctx op (A.pos e) args) e
    | ELit LEmptyError -> Marked.same_mark_as (A.ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (A.pos e)
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed")
  | EAbs _ | ELit _ | EOp _ -> e (* these are values *)
  | ETuple (es, s) ->
    let new_es = List.map (evaluate_expr ctx) es in
    if List.exists is_empty_error new_es then
      Marked.same_mark_as (A.ELit LEmptyError) e
    else Marked.same_mark_as (A.ETuple (new_es, s)) e
  | ETupleAccess (e1, n, s, _) -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | ETuple (es, s') -> (
      (match s, s' with
      | None, None -> ()
      | Some s, Some s' when s = s' -> ()
      | _ ->
        Errors.raise_multispanned_error
          [None, A.pos e; None, A.pos e1]
          "Error during tuple access: not the same structs (should not happen \
           if the term was well-typed)");
      match List.nth_opt es n with
      | Some e' -> e'
      | None ->
        Errors.raise_spanned_error (A.pos e1)
          "The tuple has %d components but the %i-th element was requested \
           (should not happen if the term was well-type)"
          (List.length es) n)
    | ELit LEmptyError -> Marked.same_mark_as (A.ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (A.pos e1)
        "The expression %a should be a tuple with %d components but is not \
         (should not happen if the term was well-typed)"
        (Print.format_expr ctx ~debug:true)
        e n)
  | EInj (e1, n, en, ts) ->
    let e1' = evaluate_expr ctx e1 in
    if is_empty_error e1' then Marked.same_mark_as (A.ELit LEmptyError) e
    else Marked.same_mark_as (A.EInj (e1', n, en, ts)) e
  | EMatch (e1, es, e_name) -> (
    let e1 = evaluate_expr ctx e1 in
    match Marked.unmark e1 with
    | A.EInj (e1, n, e_name', _) ->
      if e_name <> e_name' then
        Errors.raise_multispanned_error
          [None, A.pos e; None, A.pos e1]
          "Error during match: two different enums found (should not happend \
           if the term was well-typed)";
      let es_n =
        match List.nth_opt es n with
        | Some es_n -> es_n
        | None ->
          Errors.raise_spanned_error (A.pos e)
            "sum type index error (should not happend if the term was \
             well-typed)"
      in
      let new_e = Marked.same_mark_as (A.EApp (es_n, [e1])) e in
      evaluate_expr ctx new_e
    | A.ELit A.LEmptyError -> Marked.same_mark_as (A.ELit A.LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (A.pos e1)
        "Expected a term having a sum type as an argument to a match (should \
         not happend if the term was well-typed")
  | EDefault (exceptions, just, cons) -> (
    let exceptions = List.map (evaluate_expr ctx) exceptions in
    let empty_count = List.length (List.filter is_empty_error exceptions) in
    match List.length exceptions - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx just in
      match Marked.unmark just with
      | ELit LEmptyError -> Marked.same_mark_as (A.ELit LEmptyError) e
      | ELit (LBool true) -> evaluate_expr ctx cons
      | ELit (LBool false) -> Marked.same_mark_as (A.ELit LEmptyError) e
      | _ ->
        Errors.raise_spanned_error (A.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 -> List.find (fun sub -> not (is_empty_error sub)) exceptions
    | _ ->
      Errors.raise_multispanned_error
        (List.map
           (fun except ->
             Some "This consequence has a valid justification:", A.pos except)
           (List.filter (fun sub -> not (is_empty_error sub)) exceptions))
        "There is a conflict between multiple valid consequences for assigning \
         the same variable.")
  | EIfThenElse (cond, et, ef) -> (
    match Marked.unmark (evaluate_expr ctx cond) with
    | ELit (LBool true) -> evaluate_expr ctx et
    | ELit (LBool false) -> evaluate_expr ctx ef
    | ELit LEmptyError -> Marked.same_mark_as (A.ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (A.pos cond)
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray es ->
    let new_es = List.map (evaluate_expr ctx) es in
    if List.exists is_empty_error new_es then
      Marked.same_mark_as (A.ELit LEmptyError) e
    else Marked.same_mark_as (A.EArray new_es) e
  | ErrorOnEmpty e' ->
    let e' = evaluate_expr ctx e' in
    if Marked.unmark e' = A.ELit LEmptyError then
      Errors.raise_spanned_error (A.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    else e'
  | EAssert e' -> (
    match Marked.unmark (evaluate_expr ctx e') with
    | ELit (LBool true) -> Marked.same_mark_as (Ast.ELit LUnit) e'
    | ELit (LBool false) -> (
      match Marked.unmark e' with
      | Ast.ErrorOnEmpty
          ( EApp
              ( (Ast.EOp (Binop op), _),
                [((ELit _, _) as e1); ((ELit _, _) as e2)] ),
            _ )
      | EApp
          ( (Ast.EOp (Ast.Unop (Ast.Log _)), _),
            [
              ( Ast.EApp
                  ( (Ast.EOp (Binop op), _),
                    [((ELit _, _) as e1); ((ELit _, _) as e2)] ),
                _ );
            ] )
      | EApp
          ((Ast.EOp (Binop op), _), [((ELit _, _) as e1); ((ELit _, _) as e2)])
        ->
        Errors.raise_spanned_error (A.pos e') "Assertion failed: %a %a %a"
          (Print.format_expr ctx ~debug:false)
          e1 Print.format_binop op
          (Print.format_expr ctx ~debug:false)
          e2
      | _ ->
        Cli.debug_format "%a" (Print.format_expr ctx) e';
        Errors.raise_spanned_error (A.pos e') "Assertion failed")
    | ELit LEmptyError -> Marked.same_mark_as (A.ELit LEmptyError) e
    | _ ->
      Errors.raise_spanned_error (A.pos e')
        "Expected a boolean literal for the result of this assertion (should \
         not happen if the term was well-typed)")

(** {1 API} *)

let interpret_program :
      'm.
      Ast.decl_ctx ->
      'm Ast.marked_expr ->
      (Uid.MarkedString.info * 'm Ast.marked_expr) list =
 fun (ctx : Ast.decl_ctx) (e : 'm Ast.marked_expr) :
     (Uid.MarkedString.info * 'm Ast.marked_expr) list ->
  match evaluate_expr ctx e with
  | Ast.EAbs (_, [((Ast.TTuple (taus, Some s_in), _) as targs)]), mark_e ->
    begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we cann
       provide an empty thunked term. But for [input] arguments of another type,
       we cannot provide anything so we have to fail. *)
    let application_term =
      List.map
        (fun ty ->
          match Marked.unmark ty with
          | A.TArrow ((A.TLit A.TUnit, _), ty_in) ->
            Ast.empty_thunked_term
              (A.map_mark (fun pos -> pos) (fun _ -> ty_in) mark_e)
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
      ( Ast.EApp
          ( e,
            [
              ( Ast.ETuple (application_term, Some s_in),
                let pos =
                  match application_term with
                  | a :: _ -> A.pos a
                  | [] -> Pos.no_pos
                in
                A.map_mark (fun _ -> pos) (fun _ -> targs) mark_e );
            ] ),
        A.map_mark
          (fun pos -> pos)
          (fun ty ->
            match application_term, ty with
            | [], t_out -> t_out
            | _ :: _, (A.TArrow (_, t_out), _) -> t_out
            | _ :: _, (_, bad_pos) ->
              Errors.raise_spanned_error bad_pos
                "@[<hv 2>(bug) Result of interpretation doesn't have the \
                 expected type:@ @[%a@]@]"
                (Print.format_typ ctx) (fst @@ ty))
          mark_e )
    in
    match Marked.unmark (evaluate_expr ctx to_interpret) with
    | Ast.ETuple (args, Some s_out) ->
      let s_out_fields =
        List.map
          (fun (f, _) -> Ast.StructFieldName.get_info f)
          (Ast.StructMap.find s_out ctx.ctx_structs)
      in
      List.map2 (fun arg var -> var, arg) args s_out_fields
    | _ ->
      Errors.raise_spanned_error (A.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Errors.raise_spanned_error (A.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
