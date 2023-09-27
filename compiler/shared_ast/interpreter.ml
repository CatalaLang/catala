(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Alain Delaët <alain.delaet--tixeuil@inria.Fr>, Louis
   Gesbert <louis.gesbert@inria.fr>

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

open Catala_utils
open Definitions
open Op
module Runtime = Runtime_ocaml.Runtime

type features =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; resolvedNames : yes
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes >

type ('d, 'e, 'c) astk =
  < features ; defaultTerms : 'd ; exceptions : 'e ; custom : 'c >

(** {1 Helpers} *)

let is_empty_error : type a. (a, 'm) gexpr -> bool =
 fun e -> match Mark.remove e with EEmptyError -> true | _ -> false

(** [e' = propagate_empty_error e f] return [EEmptyError] if [e] is
    [EEmptyError], else it apply [f] on not-empty term [e]. *)
let propagate_empty_error :
    type a. (a, 'm) gexpr -> ((a, 'm) gexpr -> (a, 'm) gexpr) -> (a, 'm) gexpr =
 fun e f -> match e with (EEmptyError, _) as e -> e | e -> f e

(** [e' = propagate_empty_error_list elist f] return [EEmptyError] if one lement
    of [es] is [EEmptyError], else it apply [f] on not-empty term list [elist]. *)
let propagate_empty_error_list elist f =
  let rec aux acc = function
    | [] -> f (List.rev acc)
    | e :: r -> propagate_empty_error e (fun e -> aux (e :: acc) r)
  in
  aux [] elist

(* TODO: we should provide a generic way to print logs, that work across the
   different backends: python, ocaml, javascript, and interpreter *)

let indent_str = ref ""

(** {1 Evaluation} *)
let print_log entry infos pos e =
  if Cli.globals.trace then
    match entry with
    | VarDef _ ->
      let module Printer = Print.ExprGen (struct
        include Print.ExprConciseParam

        let bypass : type a. Format.formatter -> (a, 't) gexpr -> bool =
         fun ppf e ->
          match e with
          | EAbs _, _ ->
            Print.op_style ppf "<function>";
            true
          | _ -> false
      end) in
      Message.emit_log "%s%a %a: @{<green>%s@}" !indent_str Print.log_entry
        entry Print.uid_list infos
        (Message.unformat (fun ppf -> Printer.expr ppf e))
    | PosRecordIfTrueBool -> (
      match pos <> Pos.no_pos, Mark.remove e with
      | true, ELit (LBool true) ->
        Message.emit_log "%s@[<v>%a@{<green>Definition applied@}:@,%a@]"
          !indent_str Print.log_entry entry Pos.format_loc_text pos
      | _ -> ())
    | BeginCall ->
      Message.emit_log "%s%a %a" !indent_str Print.log_entry entry
        Print.uid_list infos;
      indent_str := !indent_str ^ "  "
    | EndCall ->
      indent_str := String.sub !indent_str 0 (String.length !indent_str - 2);
      Message.emit_log "%s%a %a" !indent_str Print.log_entry entry
        Print.uid_list infos

exception CatalaException of except

(* Todo: this should be handled early when resolving overloads. Here we have
   proper structural equality, but the OCaml backend for example uses the
   builtin equality function instead of this. *)
let handle_eq evaluate_operator pos lang e1 e2 =
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> true
  | ELit (LBool b1), ELit (LBool b2) -> not (o_xor b1 b2)
  | ELit (LInt x1), ELit (LInt x2) -> o_eq_int_int x1 x2
  | ELit (LRat x1), ELit (LRat x2) -> o_eq_rat_rat x1 x2
  | ELit (LMoney x1), ELit (LMoney x2) -> o_eq_mon_mon x1 x2
  | ELit (LDuration x1), ELit (LDuration x2) -> o_eq_dur_dur x1 x2
  | ELit (LDate x1), ELit (LDate x2) -> o_eq_dat_dat x1 x2
  | EArray es1, EArray es2 -> (
    try
      List.for_all2
        (fun e1 e2 ->
          match Mark.remove (evaluate_operator Eq pos lang [e1; e2]) with
          | ELit (LBool b) -> b
          | _ -> assert false
          (* should not happen *))
        es1 es2
    with Invalid_argument _ -> false)
  | EStruct { fields = es1; name = s1 }, EStruct { fields = es2; name = s2 } ->
    StructName.equal s1 s2
    && StructField.Map.equal
         (fun e1 e2 ->
           match Mark.remove (evaluate_operator Eq pos lang [e1; e2]) with
           | ELit (LBool b) -> b
           | _ -> assert false
           (* should not happen *))
         es1 es2
  | ( EInj { e = e1; cons = i1; name = en1 },
      EInj { e = e2; cons = i2; name = en2 } ) -> (
    try
      EnumName.equal en1 en2
      && EnumConstructor.equal i1 i2
      &&
      match Mark.remove (evaluate_operator Eq pos lang [e1; e2]) with
      | ELit (LBool b) -> b
      | _ -> assert false
      (* should not happen *)
    with Invalid_argument _ -> false)
  | _, _ -> false (* comparing anything else return false *)

(* Call-by-value: the arguments are expected to be already evaluated here *)
let rec evaluate_operator
    evaluate_expr
    (op : < overloaded : no ; .. > operator)
    m
    lang
    args =
  let pos = Expr.mark_pos m in
  let protect f x y =
    let get_binop_args_pos = function
      | (arg0 :: arg1 :: _ : ('t, 'm) gexpr list) ->
        [None, Expr.pos arg0; None, Expr.pos arg1]
      | _ -> assert false
    in
    try f x y with
    | Division_by_zero ->
      Message.raise_multispanned_error
        [
          Some "The division operator:", pos;
          Some "The null denominator:", Expr.pos (List.nth args 1);
        ]
        "division by zero at runtime"
    | Runtime.UncomparableDurations ->
      Message.raise_multispanned_error (get_binop_args_pos args)
        "Cannot compare together durations that cannot be converted to a \
         precise number of days"
  in
  let err () =
    Message.raise_multispanned_error
      ([
         ( Some
             (Format.asprintf "Operator (value %a):"
                (Print.operator ~debug:true)
                op),
           pos );
       ]
      @ List.mapi
          (fun i arg ->
            ( Some
                (Format.asprintf "Argument n°%d, value %a" (i + 1)
                   (Print.UserFacing.expr lang)
                   arg),
              Expr.pos arg ))
          args)
      "Operator %a applied to the wrong arguments\n\
       (should not happen if the term was well-typed)%a"
      (Print.operator ~debug:true)
      op Expr.format
      (EApp { f = EOp { op; tys = [] }, m; args }, m)
  in
  propagate_empty_error_list args
  @@ fun args ->
  let open Runtime.Oper in
  Mark.add m
  @@
  match op, args with
  | Length, [(EArray es, _)] ->
    ELit (LInt (Runtime.integer_of_int (List.length es)))
  | Log (entry, infos), [e'] ->
    print_log entry infos pos e';
    Mark.remove e'
  | (FromClosureEnv | ToClosureEnv), [e'] ->
    (* [FromClosureEnv] and [ToClosureEnv] are just there to bypass the need for
       existential types when typing code after closure conversion. There are
       effectively no-ops. *)
    Mark.remove e'
  | (ToClosureEnv | FromClosureEnv), _ -> err ()
  | Eq, [(e1, _); (e2, _)] ->
    ELit (LBool (handle_eq (evaluate_operator evaluate_expr) m lang e1 e2))
  | Map, [f; (EArray es, _)] ->
    EArray
      (List.map
         (fun e' -> evaluate_expr (Mark.copy e' (EApp { f; args = [e'] })))
         es)
  | Reduce, [_; default; (EArray [], _)] -> Mark.remove default
  | Reduce, [f; _; (EArray (x0 :: xn), _)] ->
    Mark.remove
      (List.fold_left
         (fun acc x ->
           evaluate_expr (Mark.copy f (EApp { f; args = [acc; x] })))
         x0 xn)
  | Concat, [(EArray es1, _); (EArray es2, _)] -> EArray (es1 @ es2)
  | Filter, [f; (EArray es, _)] ->
    EArray
      (List.filter
         (fun e' ->
           match evaluate_expr (Mark.copy e' (EApp { f; args = [e'] })) with
           | ELit (LBool b), _ -> b
           | _ ->
             Message.raise_spanned_error
               (Expr.pos (List.nth args 0))
               "This predicate evaluated to something else than a boolean \
                (should not happen if the term was well-typed)")
         es)
  | Fold, [f; init; (EArray es, _)] ->
    Mark.remove
      (List.fold_left
         (fun acc e' ->
           evaluate_expr (Mark.copy e' (EApp { f; args = [acc; e'] })))
         init es)
  | (Length | Log _ | Eq | Map | Concat | Filter | Fold | Reduce), _ -> err ()
  | Not, [(ELit (LBool b), _)] -> ELit (LBool (o_not b))
  | GetDay, [(ELit (LDate d), _)] -> ELit (LInt (o_getDay d))
  | GetMonth, [(ELit (LDate d), _)] -> ELit (LInt (o_getMonth d))
  | GetYear, [(ELit (LDate d), _)] -> ELit (LInt (o_getYear d))
  | FirstDayOfMonth, [(ELit (LDate d), _)] -> ELit (LDate (o_firstDayOfMonth d))
  | LastDayOfMonth, [(ELit (LDate d), _)] -> ELit (LDate (o_lastDayOfMonth d))
  | And, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
    ELit (LBool (o_and b1 b2))
  | Or, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
    ELit (LBool (o_or b1 b2))
  | Xor, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
    ELit (LBool (o_xor b1 b2))
  | ( ( Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth
      | And | Or | Xor ),
      _ ) ->
    err ()
  | Minus_int, [(ELit (LInt x), _)] -> ELit (LInt (o_minus_int x))
  | Minus_rat, [(ELit (LRat x), _)] -> ELit (LRat (o_minus_rat x))
  | Minus_mon, [(ELit (LMoney x), _)] -> ELit (LMoney (o_minus_mon x))
  | Minus_dur, [(ELit (LDuration x), _)] -> ELit (LDuration (o_minus_dur x))
  | ToRat_int, [(ELit (LInt i), _)] -> ELit (LRat (o_torat_int i))
  | ToRat_mon, [(ELit (LMoney i), _)] -> ELit (LRat (o_torat_mon i))
  | ToMoney_rat, [(ELit (LRat i), _)] -> ELit (LMoney (o_tomoney_rat i))
  | Round_mon, [(ELit (LMoney m), _)] -> ELit (LMoney (o_round_mon m))
  | Round_rat, [(ELit (LRat m), _)] -> ELit (LRat (o_round_rat m))
  | Add_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LInt (o_add_int_int x y))
  | Add_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LRat (o_add_rat_rat x y))
  | Add_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LMoney (o_add_mon_mon x y))
  | Add_dat_dur r, [(ELit (LDate x), _); (ELit (LDuration y), _)] ->
    ELit (LDate (o_add_dat_dur r x y))
  | Add_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LDuration (o_add_dur_dur x y))
  | Sub_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LInt (o_sub_int_int x y))
  | Sub_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LRat (o_sub_rat_rat x y))
  | Sub_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LMoney (o_sub_mon_mon x y))
  | Sub_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LDuration (o_sub_dat_dat x y))
  | Sub_dat_dur, [(ELit (LDate x), _); (ELit (LDuration y), _)] ->
    ELit (LDate (o_sub_dat_dur x y))
  | Sub_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LDuration (o_sub_dur_dur x y))
  | Mult_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LInt (o_mult_int_int x y))
  | Mult_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LRat (o_mult_rat_rat x y))
  | Mult_mon_rat, [(ELit (LMoney x), _); (ELit (LRat y), _)] ->
    ELit (LMoney (o_mult_mon_rat x y))
  | Mult_dur_int, [(ELit (LDuration x), _); (ELit (LInt y), _)] ->
    ELit (LDuration (o_mult_dur_int x y))
  | Div_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LRat (protect o_div_int_int x y))
  | Div_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LRat (protect o_div_rat_rat x y))
  | Div_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LRat (protect o_div_mon_mon x y))
  | Div_mon_rat, [(ELit (LMoney x), _); (ELit (LRat y), _)] ->
    ELit (LMoney (protect o_div_mon_rat x y))
  | Div_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LRat (protect o_div_dur_dur x y))
  | Lt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_lt_int_int x y))
  | Lt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_lt_rat_rat x y))
  | Lt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_lt_mon_mon x y))
  | Lt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_lt_dat_dat x y))
  | Lt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (protect o_lt_dur_dur x y))
  | Lte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_lte_int_int x y))
  | Lte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_lte_rat_rat x y))
  | Lte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_lte_mon_mon x y))
  | Lte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_lte_dat_dat x y))
  | Lte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (protect o_lte_dur_dur x y))
  | Gt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_gt_int_int x y))
  | Gt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_gt_rat_rat x y))
  | Gt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_gt_mon_mon x y))
  | Gt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_gt_dat_dat x y))
  | Gt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (protect o_gt_dur_dur x y))
  | Gte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_gte_int_int x y))
  | Gte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_gte_rat_rat x y))
  | Gte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_gte_mon_mon x y))
  | Gte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_gte_dat_dat x y))
  | Gte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (protect o_gte_dur_dur x y))
  | Eq_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_eq_int_int x y))
  | Eq_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_eq_rat_rat x y))
  | Eq_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_eq_mon_mon x y))
  | Eq_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_eq_dat_dat x y))
  | Eq_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (protect o_eq_dur_dur x y))
  | HandleDefault, _ ->
    Message.raise_internal_error
      "The interpreter is trying to evaluate the \"handle_default\" operator, \
       which is the leftover from the dcalc->lcalc compilation pass. This \
       indicates that you are trying to interpret the lcalc without having \
       activating --avoid_exceptions. This interpretation is not implemented, \
       just try to interpret the dcalc (with \"Interpret\") instead."
  | HandleDefaultOpt, [(EArray exps, _); justification; conclusion] -> (
    let valid_exceptions =
      ListLabels.filter exps ~f:(function
        | EInj { name; cons; _ }, _ when EnumName.equal name Expr.option_enum ->
          EnumConstructor.equal cons Expr.some_constr
        | _ -> err ())
    in
    match valid_exceptions with
    | [] -> (
      match
        Mark.remove (evaluate_expr (Expr.unthunk_term_nobox justification m))
      with
      | EInj { name; cons; e = ELit (LBool true), _ }
        when EnumName.equal name Expr.option_enum
             && EnumConstructor.equal cons Expr.some_constr ->
        Mark.remove (evaluate_expr (Expr.unthunk_term_nobox conclusion m))
      | EInj { name; cons; e = (ELit (LBool false), _) as e }
        when EnumName.equal name Expr.option_enum
             && EnumConstructor.equal cons Expr.some_constr ->
        EInj
          {
            name = Expr.option_enum;
            cons = Expr.none_constr;
            e = Mark.copy e (ELit LUnit);
          }
      | EInj { name; cons; e }
        when EnumName.equal name Expr.option_enum
             && EnumConstructor.equal cons Expr.none_constr ->
        EInj
          {
            name = Expr.option_enum;
            cons = Expr.none_constr;
            e = Mark.copy e (ELit LUnit);
          }
      | _ -> err ())
    | [((EInj { cons; name; _ } as e), _)]
      when EnumName.equal name Expr.option_enum
           && EnumConstructor.equal cons Expr.some_constr ->
      e
    | [_] -> err ()
    | _ -> raise (CatalaException ConflictError))
  | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToRat_int | ToRat_mon
      | ToMoney_rat | Round_rat | Round_mon | Add_int_int | Add_rat_rat
      | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Sub_int_int | Sub_rat_rat
      | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur | Sub_dur_dur | Mult_int_int
      | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Div_int_int | Div_rat_rat
      | Div_mon_mon | Div_mon_rat | Div_dur_dur | Lt_int_int | Lt_rat_rat
      | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur | Lte_int_int | Lte_rat_rat
      | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur | Gt_int_int | Gt_rat_rat
      | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur | Gte_int_int | Gte_rat_rat
      | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur | Eq_int_int | Eq_rat_rat
      | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur | HandleDefaultOpt ),
      _ ) ->
    err ()

(* /S\ dark magic here. This relies both on internals of [Lcalc.to_ocaml] *and*
   of the OCaml runtime *)
let rec runtime_to_val :
    (decl_ctx -> ('a, 'm) gexpr -> ('a, 'm) gexpr) ->
    decl_ctx ->
    'm mark ->
    typ ->
    Obj.t ->
    (((_, _, yes) astk as 'a), 'm) gexpr =
 fun eval_expr ctx m ty o ->
  let m = Expr.map_ty (fun _ -> ty) m in
  match Mark.remove ty with
  | TLit TBool -> ELit (LBool (Obj.obj o)), m
  | TLit TUnit -> ELit LUnit, m
  | TLit TInt -> ELit (LInt (Obj.obj o)), m
  | TLit TRat -> ELit (LRat (Obj.obj o)), m
  | TLit TMoney -> ELit (LMoney (Obj.obj o)), m
  | TLit TDate -> ELit (LDate (Obj.obj o)), m
  | TLit TDuration -> ELit (LDuration (Obj.obj o)), m
  | TTuple ts ->
    ( ETuple
        (List.map2
           (runtime_to_val eval_expr ctx m)
           ts
           (Array.to_list (Obj.obj o))),
      m )
  | TStruct name ->
    StructName.Map.find name ctx.ctx_structs
    |> StructField.Map.to_seq
    |> Seq.map2
         (fun o (fld, ty) -> fld, runtime_to_val eval_expr ctx m ty o)
         (Array.to_seq (Obj.obj o))
    |> StructField.Map.of_seq
    |> fun fields -> EStruct { name; fields }, m
  | TEnum name ->
    (* we only use non-constant constructors of arity 1, which allows us to
       always use the tag directly (ordered as declared in the constr map), and
       the field 0 *)
    let cons_map = EnumName.Map.find name ctx.ctx_enums in
    let cons, ty =
      List.nth
        (EnumConstructor.Map.bindings cons_map)
        (Obj.tag o - Obj.first_non_constant_constructor_tag)
    in
    let e = runtime_to_val eval_expr ctx m ty (Obj.field o 0) in
    EInj { name; cons; e }, m
  | TOption _ty -> assert false
  | TClosureEnv -> assert false
  | TArray ty ->
    ( EArray
        (List.map
           (runtime_to_val eval_expr ctx m ty)
           (Array.to_list (Obj.obj o))),
      m )
  | TArrow (targs, tret) -> ECustom { obj = o; targs; tret }, m
  | TAny -> assert false

and val_to_runtime :
    (decl_ctx -> ('a, 'm) gexpr -> ('a, 'm) gexpr) ->
    decl_ctx ->
    typ ->
    ('b, 'm) gexpr ->
    Obj.t =
 fun eval_expr ctx ty v ->
  match Mark.remove ty, Mark.remove v with
  | TLit TBool, ELit (LBool b) -> Obj.repr b
  | TLit TUnit, ELit LUnit -> Obj.repr ()
  | TLit TInt, ELit (LInt i) -> Obj.repr i
  | TLit TRat, ELit (LRat r) -> Obj.repr r
  | TLit TMoney, ELit (LMoney m) -> Obj.repr m
  | TLit TDate, ELit (LDate t) -> Obj.repr t
  | TLit TDuration, ELit (LDuration d) -> Obj.repr d
  | TTuple ts, ETuple es ->
    List.map2 (val_to_runtime eval_expr ctx) ts es |> Array.of_list |> Obj.repr
  | TStruct name1, EStruct { name; fields } ->
    assert (StructName.equal name name1);
    let fld_tys = StructName.Map.find name ctx.ctx_structs in
    Seq.map2
      (fun (_, ty) (_, v) -> val_to_runtime eval_expr ctx ty v)
      (StructField.Map.to_seq fld_tys)
      (StructField.Map.to_seq fields)
    |> Array.of_seq
    |> Obj.repr
  | TEnum name1, EInj { name; cons; e } ->
    assert (EnumName.equal name name1);
    let cons_map = EnumName.Map.find name ctx.ctx_enums in
    let rec find_tag n = function
      | [] -> assert false
      | (c, ty) :: _ when EnumConstructor.equal c cons -> n, ty
      | _ :: r -> find_tag (n + 1) r
    in
    let tag, ty =
      find_tag Obj.first_non_constant_constructor_tag
        (EnumConstructor.Map.bindings cons_map)
    in
    let o = Obj.with_tag tag (Obj.repr (Some ())) in
    Obj.set_field o 0 (val_to_runtime eval_expr ctx ty e);
    o
  | TOption _ty, _ -> assert false
  | TArray ty, EArray es ->
    Array.of_list (List.map (val_to_runtime eval_expr ctx ty) es) |> Obj.repr
  | TArrow (targs, tret), _ ->
    let m = Mark.get v in
    (* we want stg like [fun args -> val_to_runtime (eval_expr ctx (EApp (v,
       args)))] but in curried form *)
    let rec curry acc = function
      | [] ->
        let args = List.rev acc in
        val_to_runtime eval_expr ctx tret
          (eval_expr ctx (EApp { f = v; args }, m))
      | targ :: targs ->
        Obj.repr (fun x ->
            curry (runtime_to_val eval_expr ctx m targ x :: acc) targs)
    in
    curry [] targs
  | _ -> assert false

let rec evaluate_expr :
    type d e.
    decl_ctx ->
    Cli.backend_lang ->
    ((d, e, yes) astk, 't) gexpr ->
    ((d, e, yes) astk, 't) gexpr =
 fun ctx lang e ->
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  match Mark.remove e with
  | EVar _ ->
    Message.raise_spanned_error pos
      "free variable found at evaluation (should not happen if term was \
       well-typed)"
  | EExternal { name } ->
    let path =
      match Mark.remove name with
      | External_value td -> TopdefName.path td
      | External_scope s -> ScopeName.path s
    in
    let ty =
      try
        let ctx = Program.module_ctx ctx path in
        match Mark.remove name with
        | External_value name -> TopdefName.Map.find name ctx.ctx_topdefs
        | External_scope name ->
          let scope_info = ScopeName.Map.find name ctx.ctx_scopes in
          ( TArrow
              ( [TStruct scope_info.in_struct_name, pos],
                (TStruct scope_info.out_struct_name, pos) ),
            pos )
      with TopdefName.Map.Not_found _ | ScopeName.Map.Not_found _ ->
        Message.raise_spanned_error pos "Reference to %a could not be resolved"
          Print.external_ref name
    in
    let runtime_path =
      ( List.map ModuleName.to_string path,
        match Mark.remove name with
        | External_value name -> Mark.remove (TopdefName.get_info name)
        | External_scope name -> Mark.remove (ScopeName.get_info name) )
      (* we have the guarantee that the two cases won't collide because they
         have different capitalisation rules inherited from the input *)
    in
    let o = Runtime.lookup_value runtime_path in
    runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m ty o
  | EApp { f = e1; args } -> (
    let e1 = evaluate_expr ctx lang e1 in
    let args = List.map (evaluate_expr ctx lang) args in
    propagate_empty_error e1
    @@ fun e1 ->
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      if Bindlib.mbinder_arity binder = List.length args then
        evaluate_expr ctx lang
          (Bindlib.msubst binder (Array.of_list (List.map Mark.remove args)))
      else
        Message.raise_spanned_error pos
          "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | EOp { op; _ } -> evaluate_operator (evaluate_expr ctx lang) op m lang args
    | ECustom { obj; targs; tret } ->
      (* Applies the arguments one by one to the curried form *)
      List.fold_left2
        (fun fobj targ arg ->
          (Obj.obj fobj : Obj.t -> Obj.t)
            (val_to_runtime (fun ctx -> evaluate_expr ctx lang) ctx targ arg))
        obj targs args
      |> Obj.obj
      |> fun o ->
      runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m tret o
    | _ ->
      Message.raise_spanned_error pos
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed")
  | EAbs { binder; tys } -> Expr.unbox (Expr.eabs (Bindlib.box binder) tys m)
  | ELit _ as e -> Mark.add m e
  | EOp { op; tys } -> Expr.unbox (Expr.eop (Operator.translate op) tys m)
  (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
  | EStruct { fields = es; name } ->
    let fields, es = List.split (StructField.Map.bindings es) in
    let es = List.map (evaluate_expr ctx lang) es in
    propagate_empty_error_list es
    @@ fun es ->
    Mark.add m
      (EStruct
         {
           fields =
             StructField.Map.of_seq
               (Seq.zip (List.to_seq fields) (List.to_seq es));
           name;
         })
  | EStructAccess { e; name = s; field } -> (
    propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e ->
    match Mark.remove e with
    | EStruct { fields = es; name } -> (
      if not (StructName.equal s name) then
        Message.raise_multispanned_error
          [None, pos; None, Expr.pos e]
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      match StructField.Map.find_opt field es with
      | Some e' -> e'
      | None ->
        Message.raise_spanned_error (Expr.pos e)
          "Invalid field access %a in struct %a (should not happen if the term \
           was well-typed)"
          StructField.format field StructName.format s)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The expression %a should be a struct %a but is not (should not happen \
         if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e StructName.format s)
  | ETuple es -> Mark.add m (ETuple (List.map (evaluate_expr ctx lang) es))
  | ETupleAccess { e = e1; index; size } -> (
    match evaluate_expr ctx lang e1 with
    | ETuple es, _ when List.length es = size -> List.nth es index
    | e ->
      Message.raise_spanned_error (Expr.pos e)
        "The expression %a was expected to be a tuple of size %d (should not \
         happen if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e size)
  | EInj { e; name; cons } ->
    propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e -> Mark.add m (EInj { e; name; cons })
  | EMatch { e; cases; name } -> (
    propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e ->
    match Mark.remove e with
    | EInj { e = e1; cons; name = name' } ->
      if not (EnumName.equal name name') then
        Message.raise_multispanned_error
          [None, Expr.pos e; None, Expr.pos e1]
          "Error during match: two different enums found (should not happen if \
           the term was well-typed)";
      let es_n =
        match EnumConstructor.Map.find_opt cons cases with
        | Some es_n -> es_n
        | None ->
          Message.raise_spanned_error (Expr.pos e)
            "sum type index error (should not happen if the term was \
             well-typed)"
      in
      let new_e = Mark.add m (EApp { f = es_n; args = [e1] }) in
      evaluate_expr ctx lang new_e
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "Expected a term having a sum type as an argument to a match (should \
         not happen if the term was well-typed")
  | EIfThenElse { cond; etrue; efalse } -> (
    propagate_empty_error (evaluate_expr ctx lang cond)
    @@ fun cond ->
    match Mark.remove cond with
    | ELit (LBool true) -> evaluate_expr ctx lang etrue
    | ELit (LBool false) -> evaluate_expr ctx lang efalse
    | _ ->
      Message.raise_spanned_error (Expr.pos cond)
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray es ->
    propagate_empty_error_list (List.map (evaluate_expr ctx lang) es)
    @@ fun es -> Mark.add m (EArray es)
  | EAssert e' ->
    propagate_empty_error (evaluate_expr ctx lang e') (fun e ->
        match Mark.remove e with
        | ELit (LBool true) -> Mark.add m (ELit LUnit)
        | ELit (LBool false) ->
          Message.raise_spanned_error (Expr.pos e') "Assertion failed:@\n%a"
            (Print.UserFacing.expr lang)
            (partially_evaluate_expr_for_assertion_failure_message ctx lang
               (Expr.skip_wrappers e'))
        | _ ->
          Message.raise_spanned_error (Expr.pos e')
            "Expected a boolean literal for the result of this assertion \
             (should not happen if the term was well-typed)")
  | ECustom _ -> e
  | EEmptyError -> Mark.copy e EEmptyError
  | EErrorOnEmpty e' -> (
    match evaluate_expr ctx lang e' with
    | EEmptyError, _ ->
      Message.raise_spanned_error (Expr.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    | e -> e)
  | EDefault { excepts; just; cons } -> (
    let excepts = List.map (evaluate_expr ctx lang) excepts in
    let empty_count = List.length (List.filter is_empty_error excepts) in
    match List.length excepts - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx lang just in
      match Mark.remove just with
      | EEmptyError -> Mark.add m EEmptyError
      | ELit (LBool true) -> evaluate_expr ctx lang cons
      | ELit (LBool false) -> Mark.copy e EEmptyError
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 -> List.find (fun sub -> not (is_empty_error sub)) excepts
    | _ ->
      Message.raise_multispanned_error
        (List.map
           (fun except ->
             Some "This consequence has a valid justification:", Expr.pos except)
           (List.filter (fun sub -> not (is_empty_error sub)) excepts))
        "There is a conflict between multiple valid consequences for assigning \
         the same variable.")
  | ERaise exn -> raise (CatalaException exn)
  | ECatch { body; exn; handler } -> (
    try evaluate_expr ctx lang body
    with CatalaException caught when Expr.equal_except caught exn ->
      evaluate_expr ctx lang handler)
  | _ -> .

and partially_evaluate_expr_for_assertion_failure_message :
    type d e.
    decl_ctx ->
    Cli.backend_lang ->
    ((d, e, yes) astk, 't) gexpr ->
    ((d, e, yes) astk, 't) gexpr =
 fun ctx lang e ->
  (* Here we want to print an expression that explains why an assertion has
     failed. Since assertions have type [bool] and are usually constructed with
     comparisons and logical operators, we leave those unevaluated at the top of
     the AST while evaluating everything below. This makes for a good error
     message. *)
  match Mark.remove e with
  | EApp { f = EOp ({ op = op_kind; _ } as op), m; args = [e1; e2] }
    when match op_kind with
         | And | Or | Xor | Eq | Lt_int_int | Lt_rat_rat | Lt_mon_mon
         | Lt_dat_dat | Lt_dur_dur | Lte_int_int | Lte_rat_rat | Lte_mon_mon
         | Lte_dat_dat | Lte_dur_dur | Gt_int_int | Gt_rat_rat | Gt_mon_mon
         | Gt_dat_dat | Gt_dur_dur | Gte_int_int | Gte_rat_rat | Gte_mon_mon
         | Gte_dat_dat | Gte_dur_dur | Eq_int_int | Eq_rat_rat | Eq_mon_mon
         | Eq_dur_dur | Eq_dat_dat ->
           true
         | _ -> false ->
    ( EApp
        {
          f = EOp op, m;
          args =
            [
              partially_evaluate_expr_for_assertion_failure_message ctx lang e1;
              partially_evaluate_expr_for_assertion_failure_message ctx lang e2;
            ];
        },
      Mark.get e )
  | _ -> evaluate_expr ctx lang e

(* Typing shenanigan to add custom terms to the AST type. This is an identity
   and could be optimised into [Obj.magic]. *)
let addcustom e =
  let rec f :
      type c d e.
      ((d, e, c) astk, 't) gexpr -> ((d, e, yes) astk, 't) gexpr boxed =
    function
    | (ECustom _, _) as e -> Expr.map ~f e
    | EOp { op; tys }, m -> Expr.eop (Operator.translate op) tys m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EEmptyError, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | (ECatch _, _) as e -> Expr.map ~f e
    | (ERaise _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EAbs _
        | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EStruct _
        | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  Expr.unbox (f e)

let delcustom e =
  let rec f :
      type c d e.
      ((d, e, c) astk, 't) gexpr -> ((d, e, no) astk, 't) gexpr boxed = function
    | ECustom _, _ -> invalid_arg "Custom term remaining in evaluated term"
    | EOp { op; tys }, m -> Expr.eop (Operator.translate op) tys m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EEmptyError, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | (ECatch _, _) as e -> Expr.map ~f e
    | (ERaise _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EAbs _
        | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EStruct _
        | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  Expr.unbox (f e)

let interpret_program_lcalc p s : (Uid.MarkedString.info * ('a, 'm) gexpr) list
    =
  let e = Expr.unbox @@ Program.to_expr p s in
  let ctx = p.decl_ctx in
  match evaluate_expr ctx p.lang (addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let application_term =
      StructField.Map.map
        (fun ty ->
          match Mark.remove ty with
          | TOption _ ->
            (Expr.einj ~e:(Expr.elit LUnit mark_e) ~cons:Expr.none_constr
               ~name:Expr.option_enum mark_e
              : (_, _) boxed_gexpr)
          | _ ->
            Message.raise_spanned_error (Mark.get ty)
              "This scope needs input arguments to be executed. But the Catala \
               built-in interpreter does not have a way to retrieve input \
               values from the command line, so it cannot execute this scope. \
               Please create another scope that provides the input arguments \
               to this one and execute it instead. ")
        taus
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (Expr.pos e)
    in
    match Mark.remove (evaluate_expr ctx p.lang (Expr.unbox to_interpret)) with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructField.get_info fld, delcustom e)
        (StructField.Map.bindings fields)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"

(** {1 API} *)
let interpret_program_dcalc p s : (Uid.MarkedString.info * ('a, 'm) gexpr) list
    =
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match evaluate_expr p.decl_ctx p.lang (addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let application_term =
      StructField.Map.map
        (fun ty ->
          match Mark.remove ty with
          | TArrow (ty_in, ty_out) ->
            Expr.make_abs
              (Array.of_list @@ List.map (fun _ -> Var.make "_") ty_in)
              (Bindlib.box EEmptyError, Expr.with_ty mark_e ty_out)
              ty_in (Expr.mark_pos mark_e)
          | _ ->
            Message.raise_spanned_error (Mark.get ty)
              "This scope needs input arguments to be executed. But the Catala \
               built-in interpreter does not have a way to retrieve input \
               values from the command line, so it cannot execute this scope. \
               Please create another scope that provides the input arguments \
               to this one and execute it instead. ")
        taus
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (Expr.pos e)
    in
    match Mark.remove (evaluate_expr ctx p.lang (Expr.unbox to_interpret)) with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructField.get_info fld, delcustom e)
        (StructField.Map.bindings fields)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"

(* Evaluation may introduce intermediate custom terms ([ECustom], pointers to
   external functions), straying away from the DCalc and LCalc ASTS. [addcustom]
   and [delcustom] are needed to expand and shrink the type of the terms to
   reflect that. *)
let evaluate_expr ctx lang e = delcustom (evaluate_expr ctx lang (addcustom e))

let load_runtime_modules prg =
  let load m =
    let obj_file =
      Dynlink.adapt_filename File.(Pos.get_file (ModuleName.pos m) /../ ModuleName.to_string m ^ ".cmo")
    in
    if not (Sys.file_exists obj_file) then
      Message.raise_spanned_error
        ~span_msg:(fun ppf -> Format.pp_print_string ppf "Module defined here")
        (ModuleName.pos m)
        "Compiled OCaml object %a not found. Make sure it has been suitably compiled." File.format obj_file
    else
      try Dynlink.loadfile obj_file
      with Dynlink.Error dl_err ->
        Message.raise_error
          "Error loading compiled module from %a:@;\
           <1 2>@[<hov>%a@]" File.format obj_file
          Format.pp_print_text
          (Dynlink.error_message dl_err)
  in
  let rec aux loaded decl_ctx =
    ModuleName.Map.fold (fun mname sub_decl_ctx loaded ->
        if ModuleName.Set.mem mname loaded then loaded else
          let loaded = ModuleName.Set.add mname loaded in
          let loaded = aux loaded sub_decl_ctx in
          load mname;
          loaded)
      decl_ctx.ctx_modules loaded
  in
  if not (ModuleName.Map.is_empty prg.decl_ctx.ctx_modules) then
    Message.emit_debug "Loading shared modules... %a"
      (fun ppf -> ModuleName.Map.format_keys ppf)
      prg.decl_ctx.ctx_modules;
  let (_loaded: ModuleName.Set.t) = aux ModuleName.Set.empty prg.decl_ctx in
  ()
