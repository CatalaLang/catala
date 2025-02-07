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

(** {1 Helpers} *)

let is_empty_error : type a. (a, 'm) gexpr -> bool =
 fun e -> match Mark.remove e with EEmpty -> true | _ -> false

(* TODO: we should provide a generic way to print logs, that work across the
   different backends: python, ocaml, javascript, and interpreter *)

(** {1 Evaluation} *)

let rec format_runtime_value lang ppf = function
  | Runtime.Unit -> Print.UserFacing.unit lang ppf ()
  | Runtime.Bool b -> Print.UserFacing.bool lang ppf b
  | Runtime.Money m -> Print.UserFacing.money lang ppf m
  | Runtime.Integer i -> Print.UserFacing.integer lang ppf i
  | Runtime.Decimal d -> Print.UserFacing.decimal lang ppf d
  | Runtime.Date t -> Print.UserFacing.date lang ppf t
  | Runtime.Duration dt -> Print.UserFacing.duration lang ppf dt
  | Runtime.Enum (name, (constr, v)) ->
    Format.fprintf ppf "@[<hov 2>%s.%s@ (%a)@]" name constr
      (format_runtime_value lang)
      v
  | Runtime.Struct (name, fields) ->
    Format.fprintf ppf "@[<hv 2>%s {@ %a@;<1 -2>}@]" name
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (fld, v) ->
           Format.fprintf ppf "@[<hov 2>-- %s:@ %a@]" fld
             (format_runtime_value lang)
             v))
      fields
  | Runtime.Array elts ->
    Format.fprintf ppf "@[<hv 2>[@,@[<hov>%a@]@;<0 -2>]@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         (format_runtime_value lang))
      (Array.to_list elts)
  | Runtime.Tuple elts ->
    Format.fprintf ppf "@[<hv 2>(@,@[<hov>%a@]@;<0 -2>)@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (format_runtime_value lang))
      (Array.to_list elts)
  | Runtime.Unembeddable -> Format.pp_print_string ppf "<object>"

let print_log ppf lang level entry =
  let pp_infos =
    Format.(
      pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ".@,") pp_print_string)
  in
  let logprintf level entry fmt =
    if ppf == Message.std_ppf () then Format.fprintf ppf "[@{<bold;grey>LOG@}] ";
    Format.fprintf ppf
      ("@[<hov>%*s%a" ^^ fmt ^^ "@]@,")
      (level * 2) "" Print.log_entry entry
  in
  match entry with
  | Runtime.BeginCall infos ->
    logprintf level BeginCall " %a" pp_infos infos;
    level + 1
  | Runtime.EndCall infos ->
    let level = max 0 (level - 1) in
    logprintf level EndCall " %a" pp_infos infos;
    level
  | Runtime.VariableDefinition (infos, io, value) ->
    logprintf level
      (VarDef
         {
           log_typ = TAny;
           log_io_input = io.Runtime.io_input;
           log_io_output = io.Runtime.io_output;
         })
      " %a: @{<green>%s@}" pp_infos infos
      (Message.unformat (fun ppf -> format_runtime_value lang ppf value));
    level
  | Runtime.DecisionTaken rtpos ->
    let pos = Expr.runtime_to_pos rtpos in
    logprintf level PosRecordIfTrueBool
      "@[<v -2>@{<green>Definition applied@}:@,%a@]@," Pos.format_loc_text pos;
    level

let rec value_to_runtime_embedded = function
  | ELit LUnit -> Runtime.Unit
  | ELit (LBool b) -> Runtime.Bool b
  | ELit (LMoney m) -> Runtime.Money m
  | ELit (LInt i) -> Runtime.Integer i
  | ELit (LRat r) -> Runtime.Decimal r
  | ELit (LDate d) -> Runtime.Date d
  | ELit (LDuration dt) -> Runtime.Duration dt
  | EInj { name; cons; e } ->
    Runtime.Enum
      ( EnumName.to_string name,
        ( EnumConstructor.to_string cons,
          value_to_runtime_embedded (Mark.remove e) ) )
  | EStruct { name; fields } ->
    Runtime.Struct
      ( StructName.to_string name,
        List.map
          (fun (f, e) ->
            StructField.to_string f, value_to_runtime_embedded (Mark.remove e))
          (StructField.Map.bindings fields) )
  | EArray el ->
    Runtime.Array
      (Array.of_list
         (List.map (fun e -> value_to_runtime_embedded (Mark.remove e)) el))
  | ETuple el ->
    Runtime.Tuple
      (Array.of_list
         (List.map (fun e -> value_to_runtime_embedded (Mark.remove e)) el))
  | _ -> Runtime.Unembeddable

(* Todo: this should be handled early when resolving overloads. Here we have
   proper structural equality, but the OCaml backend for example uses the
   builtin equality function instead of this. *)
let handle_eq pos evaluate_operator m lang e1 e2 =
  let eq_eval = evaluate_operator (Eq, pos) m lang in
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> true
  | ELit (LBool b1), ELit (LBool b2) -> o_eq_boo_boo b1 b2
  | ELit (LInt x1), ELit (LInt x2) -> o_eq_int_int x1 x2
  | ELit (LRat x1), ELit (LRat x2) -> o_eq_rat_rat x1 x2
  | ELit (LMoney x1), ELit (LMoney x2) -> o_eq_mon_mon x1 x2
  | ELit (LDuration x1), ELit (LDuration x2) ->
    o_eq_dur_dur (Expr.pos_to_runtime (Expr.mark_pos m)) x1 x2
  | ELit (LDate x1), ELit (LDate x2) -> o_eq_dat_dat x1 x2
  | EArray es1, EArray es2 | ETuple es1, ETuple es2 -> (
    try
      List.for_all2
        (fun e1 e2 ->
          match Mark.remove (eq_eval [e1; e2]) with
          | ELit (LBool b) -> b
          | _ -> assert false
          (* should not happen *))
        es1 es2
    with Invalid_argument _ -> false)
  | EStruct { fields = es1; name = s1 }, EStruct { fields = es2; name = s2 } ->
    StructName.equal s1 s2
    && StructField.Map.equal
         (fun e1 e2 ->
           match Mark.remove (eq_eval [e1; e2]) with
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
      match Mark.remove (eq_eval [e1; e2]) with
      | ELit (LBool b) -> b
      | _ -> assert false
      (* should not happen *)
    with Invalid_argument _ -> false)
  | _, _ -> false (* comparing anything else return false *)

(* This evaluation of functional application is used by operators in order to
   make them compatible with execution after closure-conversion: the case where
   we need to apply a closure instead is detected and handled transparently *)
let eval_application evaluate_expr f args =
  match f with
  | EAbs _, _ ->
    let ty =
      match Expr.maybe_ty (Mark.get f) with TArrow (_, ty), _ -> ty | ty -> ty
    in
    evaluate_expr
      ( EApp
          { f; args; tys = List.map (fun e -> Expr.maybe_ty (Mark.get e)) args },
        Expr.with_ty (Mark.get f) ty )
  | ETuple [closure; closure_env], _ ->
    let ty =
      match Expr.maybe_ty (Mark.get closure) with
      | TArrow (_, ty), _ -> ty
      | ty -> ty
    in
    evaluate_expr
      ( EApp
          {
            f = closure;
            args = closure_env :: args;
            tys =
              (TClosureEnv, Expr.pos closure)
              :: List.map (fun e -> Expr.maybe_ty (Mark.get e)) args;
          },
        Expr.with_ty (Mark.get f) ty )
  | _ ->
    Message.error ~internal:true
      "Trying to apply non-function passed as operator argument"

(* Call-by-value: the arguments are expected to be already evaluated here *)
let rec evaluate_operator
    evaluate_expr
    ((op, opos) : < overloaded : no ; .. > operator Mark.pos)
    m
    lang
    args =
  let pos = Expr.mark_pos m in
  let rpos () = Expr.pos_to_runtime opos in
  let div_pos () =
    (* Division by 0 errors point to their 2nd operand *)
    Expr.pos_to_runtime
    @@ match args with _ :: denom :: _ -> Expr.pos denom | _ -> opos
  in
  let err () =
    Message.error
      ~extra_pos:
        ([
           ( Format.asprintf "Operator (value %a):"
               (Print.operator ~debug:true)
               op,
             opos );
         ]
        @ List.mapi
            (fun i arg ->
              ( Format.asprintf "Argument n°%d, value %a" (i + 1)
                  (Print.UserFacing.expr lang)
                  arg,
                Expr.pos arg ))
            args)
      "Operator %a applied to the wrong@ arguments@ (should not happen if the \
       term was well-typed)"
      (Print.operator ~debug:true)
      op
  in
  let open Runtime.Oper in
  Mark.add m
  @@
  match op, args with
  | Length, [(EArray es, _)] ->
    ELit (LInt (Runtime.integer_of_int (List.length es)))
  | Log (entry, infos), [(e, _)] when Global.options.trace <> None -> (
    let rtinfos = List.map Uid.MarkedString.to_string infos in
    match entry with
    | BeginCall -> Runtime.log_begin_call rtinfos e
    | EndCall -> Runtime.log_end_call rtinfos e
    | PosRecordIfTrueBool ->
      (match e with
      | ELit (LBool b) ->
        Runtime.log_decision_taken (Expr.pos_to_runtime pos) b |> ignore
      | _ -> ());
      e
    | VarDef def ->
      Runtime.log_variable_definition rtinfos
        { Runtime.io_input = def.log_io_input; io_output = def.log_io_output }
        value_to_runtime_embedded e)
  | Log _, [(e', _)] -> e'
  | (FromClosureEnv | ToClosureEnv), [e'] ->
    (* [FromClosureEnv] and [ToClosureEnv] are just there to bypass the need for
       existential types when typing code after closure conversion. There are
       effectively no-ops. *)
    Mark.remove e'
  | (ToClosureEnv | FromClosureEnv), _ -> err ()
  | Eq, [(e1, _); (e2, _)] ->
    ELit (LBool (handle_eq opos (evaluate_operator evaluate_expr) m lang e1 e2))
  | Map, [f; (EArray es, _)] ->
    EArray (List.map (fun e' -> eval_application evaluate_expr f [e']) es)
  | Map2, [f; (EArray es1, _); (EArray es2, _)] ->
    EArray
      (List.map2
         (fun e1 e2 -> eval_application evaluate_expr f [e1; e2])
         es1 es2)
  | Reduce, [_; default; (EArray [], _)] -> Mark.remove default
  | Reduce, [f; _; (EArray (x0 :: xn), _)] ->
    Mark.remove
      (List.fold_left
         (fun acc x -> eval_application evaluate_expr f [acc; x])
         x0 xn)
  | Concat, [(EArray es1, _); (EArray es2, _)] -> EArray (es1 @ es2)
  | Filter, [f; (EArray es, _)] ->
    EArray
      (List.filter
         (fun e' ->
           match eval_application evaluate_expr f [e'] with
           | ELit (LBool b), _ -> b
           | _ ->
             Message.error
               ~pos:(Expr.pos (List.nth args 0))
               "%a" Format.pp_print_text
               "This predicate evaluated to something else than a boolean \
                (should not happen if the term was well-typed)")
         es)
  | Fold, [f; init; (EArray es, _)] ->
    Mark.remove
      (List.fold_left
         (fun acc e' -> eval_application evaluate_expr f [acc; e'])
         init es)
  | (Length | Log _ | Eq | Map | Map2 | Concat | Filter | Fold | Reduce), _ ->
    err ()
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
  | ToInt_rat, [(ELit (LRat x), _)] -> ELit (LInt (o_toint_rat x))
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
    ELit (LDate (o_add_dat_dur r (rpos ()) x y))
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
  | Sub_dat_dur r, [(ELit (LDate x), _); (ELit (LDuration y), _)] ->
    ELit (LDate (o_sub_dat_dur r (rpos ()) x y))
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
    ELit (LRat (o_div_int_int (div_pos ()) x y))
  | Div_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LRat (o_div_rat_rat (div_pos ()) x y))
  | Div_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LRat (o_div_mon_mon (div_pos ()) x y))
  | Div_mon_rat, [(ELit (LMoney x), _); (ELit (LRat y), _)] ->
    ELit (LMoney (o_div_mon_rat (div_pos ()) x y))
  | Div_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LRat (o_div_dur_dur (div_pos ()) x y))
  | Lt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_lt_int_int x y))
  | Lt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_lt_rat_rat x y))
  | Lt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_lt_mon_mon x y))
  | Lt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_lt_dat_dat x y))
  | Lt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (o_lt_dur_dur (rpos ()) x y))
  | Lte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_lte_int_int x y))
  | Lte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_lte_rat_rat x y))
  | Lte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_lte_mon_mon x y))
  | Lte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_lte_dat_dat x y))
  | Lte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (o_lte_dur_dur (rpos ()) x y))
  | Gt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_gt_int_int x y))
  | Gt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_gt_rat_rat x y))
  | Gt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_gt_mon_mon x y))
  | Gt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_gt_dat_dat x y))
  | Gt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (o_gt_dur_dur (rpos ()) x y))
  | Gte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_gte_int_int x y))
  | Gte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_gte_rat_rat x y))
  | Gte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_gte_mon_mon x y))
  | Gte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_gte_dat_dat x y))
  | Gte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (o_gte_dur_dur (rpos ()) x y))
  | Eq_boo_boo, [(ELit (LBool x), _); (ELit (LBool y), _)] ->
    ELit (LBool (o_eq_boo_boo x y))
  | Eq_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
    ELit (LBool (o_eq_int_int x y))
  | Eq_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
    ELit (LBool (o_eq_rat_rat x y))
  | Eq_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
    ELit (LBool (o_eq_mon_mon x y))
  | Eq_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
    ELit (LBool (o_eq_dat_dat x y))
  | Eq_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
    ELit (LBool (o_eq_dur_dur (rpos ()) x y))
  | HandleExceptions, [(EArray exps, _)] -> (
    let valid_exceptions =
      ListLabels.filter exps ~f:(function
        | EInj { name; cons; _ }, _ when EnumName.equal name Expr.option_enum ->
          EnumConstructor.equal cons Expr.some_constr
        | _ -> err ())
    in
    match valid_exceptions with
    | [] ->
      EInj
        { name = Expr.option_enum; cons = Expr.none_constr; e = ELit LUnit, m }
    | [((EInj { cons; name; _ } as e), _)]
      when EnumName.equal name Expr.option_enum
           && EnumConstructor.equal cons Expr.some_constr ->
      e
    | [_] -> err ()
    | excs ->
      raise
        Runtime.(
          Error (Conflict, List.map Expr.(fun e -> pos_to_runtime (pos e)) excs))
    )
  | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToInt_rat | ToRat_int
      | ToRat_mon | ToMoney_rat | Round_rat | Round_mon | Add_int_int
      | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Sub_int_int
      | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur _ | Sub_dur_dur
      | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Div_int_int
      | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur | Lt_int_int
      | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur | Lte_int_int
      | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur | Gt_int_int
      | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur | Gte_int_int
      | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur | Eq_boo_boo
      | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur
      | HandleExceptions ),
      _ ) ->
    err ()

(* /S\ dark magic here. This relies both on internals of [Lcalc.to_ocaml] *and*
   of the OCaml runtime *)
let rec runtime_to_val :
    type d.
    (decl_ctx ->
    ((d, _) interpr_kind, 'm) gexpr ->
    ((d, _) interpr_kind, 'm) gexpr) ->
    decl_ctx ->
    'm mark ->
    typ ->
    Obj.t ->
    (((d, yes) interpr_kind as 'a), 'm) gexpr =
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
  | TOption ty -> (
    match Obj.tag o - Obj.first_non_constant_constructor_tag with
    | 0 ->
      let e =
        runtime_to_val eval_expr ctx m (TLit TUnit, Pos.no_pos) (Obj.field o 0)
      in
      EInj { name = Expr.option_enum; cons = Expr.none_constr; e }, m
    | 1 ->
      let e = runtime_to_val eval_expr ctx m ty (Obj.field o 0) in
      EInj { name = Expr.option_enum; cons = Expr.some_constr; e }, m
    | _ -> assert false)
  | TClosureEnv ->
    (* By construction, a closure environment can only be consumed from the same
       scope where it was built (compiled or not) ; for this reason, we can
       safely avoid converting in depth here *)
    Obj.obj o, m
  | TArray ty ->
    ( EArray
        (List.map
           (runtime_to_val eval_expr ctx m ty)
           (Array.to_list (Obj.obj o))),
      m )
  | TArrow (targs, tret) -> ECustom { obj = o; targs; tret }, m
  | TDefault ty -> (
    (* This case is only valid for ASTs including default terms; but the typer
       isn't aware so we need some additional dark arts. *)
    match (Obj.obj o : 'a Runtime.Eoption.t) with
    | Runtime.Eoption.ENone () -> Obj.magic EEmpty, m
    | Runtime.Eoption.ESome o -> Obj.magic (runtime_to_val eval_expr ctx m ty o)
    )
  | TAny -> assert false

and val_to_runtime :
    type d.
    (decl_ctx ->
    ((d, _) interpr_kind, 'm) gexpr ->
    ((d, _) interpr_kind, 'm) gexpr) ->
    decl_ctx ->
    typ ->
    ((d, _) interpr_kind, 'm) gexpr ->
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
    let field = val_to_runtime eval_expr ctx ty e in
    let o = Obj.with_tag tag (Obj.repr (Some ())) in
    Obj.set_field o 0 field;
    o
  | TOption ty, EInj { name; cons; e } ->
    assert (EnumName.equal name Expr.option_enum);
    let tag, ty =
      (* None is before Some because the constructors have been defined in this
         order in [expr.ml], and the ident maps preserve definition ordering *)
      if EnumConstructor.equal cons Expr.none_constr then
        Obj.first_non_constant_constructor_tag, (TLit TUnit, Pos.no_pos)
      else if EnumConstructor.equal cons Expr.some_constr then
        Obj.first_non_constant_constructor_tag + 1, ty
      else assert false
    in
    let field = val_to_runtime eval_expr ctx ty e in
    let o = Obj.with_tag tag (Obj.repr (Some ())) in
    Obj.set_field o 0 field;
    o
  | TArray ty, EArray es ->
    Array.of_list (List.map (val_to_runtime eval_expr ctx ty) es) |> Obj.repr
  | TArrow (targs, tret), _ ->
    let m = Mark.get v in
    (* we want stg like [fun args -> val_to_runtime (eval_expr ctx (EApp (v,
       args)))] but in curried form *)
    let rec curry acc = function
      | [] ->
        let args = List.rev acc in
        let tys = List.map (fun a -> Expr.maybe_ty (Mark.get a)) args in
        val_to_runtime eval_expr ctx tret
          (eval_expr ctx (EApp { f = v; args; tys }, m))
      | targ :: targs ->
        Obj.repr (fun x ->
            curry (runtime_to_val eval_expr ctx m targ x :: acc) targs)
    in
    curry [] targs
  | TDefault ty, _ -> (
    match v with
    | EEmpty, _ -> Obj.repr (Runtime.Eoption.ENone ())
    | EPureDefault e, _ | e ->
      Obj.repr (Runtime.Eoption.ESome (val_to_runtime eval_expr ctx ty e)))
  | TClosureEnv, v ->
    (* By construction, a closure environment can only be consumed from the same
       scope where it was built (compiled or not) ; for this reason, we can
       safely avoid converting in depth here *)
    Obj.repr v
  | _ ->
    Message.error ~internal:true
      "Could not convert value of type %a@ to@ runtime:@ %a" (Print.typ ctx) ty
      Expr.format v

let rec evaluate_expr :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr =
 fun ctx lang e ->
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  match Mark.remove e with
  | EVar _ ->
    Message.error ~pos "%a" Format.pp_print_text
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
        match Mark.remove name with
        | External_value name ->
          let typ, _vis = TopdefName.Map.find name ctx.ctx_topdefs in
          typ
        | External_scope name ->
          let scope_info = ScopeName.Map.find name ctx.ctx_scopes in
          ( TArrow
              ( [TStruct scope_info.in_struct_name, pos],
                (TStruct scope_info.out_struct_name, pos) ),
            pos )
      with TopdefName.Map.Not_found _ | ScopeName.Map.Not_found _ ->
        Message.error ~pos "Reference to %a@ could@ not@ be@ resolved"
          Print.external_ref name
    in
    let runtime_path =
      ( List.map ModuleName.to_string path,
        match Mark.remove name with
        | External_value name -> TopdefName.base name
        | External_scope name -> ScopeName.base name )
      (* we have the guarantee that the two cases won't collide because they
         have different capitalisation rules inherited from the input *)
    in
    let o = Runtime.lookup_value runtime_path in
    runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m ty o
  | EApp { f = e1; args; _ } -> (
    let e1 = evaluate_expr ctx lang e1 in
    let args = List.map (evaluate_expr ctx lang) args in
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      if Bindlib.mbinder_arity binder = List.length args then
        evaluate_expr ctx lang
          (Bindlib.msubst binder (Array.of_list (List.map Mark.remove args)))
      else
        Message.error ~pos "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | ECustom { obj; targs; tret } ->
      (* Applies the arguments one by one to the curried form *)
      let o =
        List.fold_left2
          (fun fobj targ arg ->
            (Obj.obj fobj : Obj.t -> Obj.t)
              (val_to_runtime (fun ctx -> evaluate_expr ctx lang) ctx targ arg))
          obj targs args
      in
      runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m tret o
    | _ ->
      Message.error ~pos ~internal:true "%a%a" Format.pp_print_text
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed"
        (fun ppf e ->
          if Global.options.debug then Format.fprintf ppf ":@ %a" Expr.format e
          else ())
        e1)
  | EAppOp { op; args; _ } ->
    let args = List.map (evaluate_expr ctx lang) args in
    evaluate_operator (evaluate_expr ctx lang) op m lang args
  | EAbs _ | ELit _ | ECustom _ | EEmpty -> e (* these are values *)
  | EStruct { fields = es; name } ->
    let fields, es = List.split (StructField.Map.bindings es) in
    let es = List.map (evaluate_expr ctx lang) es in
    Mark.add m
      (EStruct
         {
           fields =
             StructField.Map.of_seq
               (Seq.zip (List.to_seq fields) (List.to_seq es));
           name;
         })
  | EStructAccess { e; name = s; field } -> (
    let e = evaluate_expr ctx lang e in
    match Mark.remove e with
    | EStruct { fields = es; name } -> (
      if not (StructName.equal s name) then
        Message.error
          ~extra_pos:["", pos; "", Expr.pos e]
          "%a" Format.pp_print_text
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      match StructField.Map.find_opt field es with
      | Some e' -> e'
      | None ->
        Message.error ~pos:(Expr.pos e)
          "Invalid field access %a@ in@ struct@ %a@ (should not happen if the \
           term was well-typed)"
          StructField.format field StructName.format s)
    | _ ->
      Message.error ~pos:(Expr.pos e)
        "The expression %a@ should@ be@ a@ struct@ %a@ but@ is@ not@ (should \
         not happen if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e StructName.format s)
  | ETuple es -> Mark.add m (ETuple (List.map (evaluate_expr ctx lang) es))
  | ETupleAccess { e = e1; index; size } -> (
    match evaluate_expr ctx lang e1 with
    | ETuple es, _ when List.length es = size -> List.nth es index
    | e ->
      Message.error ~pos:(Expr.pos e)
        "The expression %a@ was@ expected@ to@ be@ a@ tuple@ of@ size@ %d@ \
         (should not happen if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e size)
  | EInj { e; name; cons } ->
    let e = evaluate_expr ctx lang e in
    Mark.add m (EInj { e; name; cons })
  | EMatch { e; cases; name } -> (
    let e = evaluate_expr ctx lang e in
    match Mark.remove e with
    | EInj { e = e1; cons; name = name' } ->
      if not (EnumName.equal name name') then
        Message.error
          ~extra_pos:["", Expr.pos e; "", Expr.pos e1]
          "%a" Format.pp_print_text
          "Error during match: two different enums found (should not happen if \
           the term was well-typed)";
      let es_n =
        match EnumConstructor.Map.find_opt cons cases with
        | Some es_n -> es_n
        | None ->
          Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
            "sum type index error (should not happen if the term was \
             well-typed)"
      in
      let ty =
        EnumConstructor.Map.find cons (EnumName.Map.find name ctx.ctx_enums)
      in
      let new_e = Mark.add m (EApp { f = es_n; args = [e1]; tys = [ty] }) in
      evaluate_expr ctx lang new_e
    | _ ->
      Message.error ~pos:(Expr.pos e)
        "Expected a term having a sum type as an argument to a match (should \
         not happen if the term was well-typed")
  | EIfThenElse { cond; etrue; efalse } -> (
    let cond = evaluate_expr ctx lang cond in
    match Mark.remove cond with
    | ELit (LBool true) -> evaluate_expr ctx lang etrue
    | ELit (LBool false) -> evaluate_expr ctx lang efalse
    | _ ->
      Message.error ~pos:(Expr.pos cond) "%a" Format.pp_print_text
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray es ->
    let es = List.map (evaluate_expr ctx lang) es in
    Mark.add m (EArray es)
  | EAssert e' -> (
    let e = evaluate_expr ctx lang e' in
    match Mark.remove e with
    | ELit (LBool true) -> Mark.add m (ELit LUnit)
    | ELit (LBool false) ->
      if Global.options.stop_on_error then
        raise Runtime.(Error (AssertionFailed, [Expr.pos_to_runtime pos]))
      else
        let partially_evaluated_assertion_failure_expr =
          partially_evaluate_expr_for_assertion_failure_message ctx lang
            (Expr.skip_wrappers e')
        in
        (match Mark.remove partially_evaluated_assertion_failure_expr with
        | ELit (LBool false) ->
          if Global.options.no_fail_on_assert then
            Message.warning ~pos "Assertion failed:"
          else Message.delayed_error ~kind:Generic () ~pos "Assertion failed:"
        | _ ->
          if Global.options.no_fail_on_assert then
            Message.warning ~pos "Assertion failed:@ %a"
              (Print.UserFacing.expr lang)
              partially_evaluated_assertion_failure_expr
          else
            Message.delayed_error ~kind:Generic () ~pos "Assertion failed:@ %a"
              (Print.UserFacing.expr lang)
              partially_evaluated_assertion_failure_expr);
        Mark.add m (ELit LUnit)
    | _ ->
      Message.error ~pos:(Expr.pos e') "%a" Format.pp_print_text
        "Expected a boolean literal for the result of this assertion (should \
         not happen if the term was well-typed)")
  | EFatalError err -> raise (Runtime.Error (err, [Expr.pos_to_runtime pos]))
  | EErrorOnEmpty e' -> (
    match evaluate_expr ctx lang e' with
    | EEmpty, _ -> raise Runtime.(Error (NoValue, [Expr.pos_to_runtime pos]))
    | exception Runtime.Empty ->
      raise Runtime.(Error (NoValue, [Expr.pos_to_runtime pos]))
    | e -> e)
  | EDefault { excepts; just; cons } -> (
    let excepts = List.map (evaluate_expr ctx lang) excepts in
    let empty_count = List.length (List.filter is_empty_error excepts) in
    match List.length excepts - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx lang just in
      match Mark.remove just with
      | ELit (LBool true) -> evaluate_expr ctx lang cons
      | ELit (LBool false) -> Mark.copy e EEmpty
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 -> List.find (fun sub -> not (is_empty_error sub)) excepts
    | _ ->
      let poslist =
        List.filter_map
          (fun ex ->
            if is_empty_error ex then None
            else Some Expr.(pos_to_runtime (pos ex)))
          excepts
      in
      raise Runtime.(Error (Conflict, poslist)))
  | EPureDefault e -> evaluate_expr ctx lang e
  | _ -> .

and partially_evaluate_expr_for_assertion_failure_message :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr =
 fun ctx lang e ->
  (* Here we want to print an expression that explains why an assertion has
     failed. Since assertions have type [bool] and are usually constructed with
     comparisons and logical operators, we leave those unevaluated at the top of
     the AST while evaluating everything below. This makes for a good error
     message. *)
  match Mark.remove e with
  | EAppOp
      {
        args = [e1; e2];
        tys;
        op =
          ( ( And | Or | Xor | Eq | Lt_int_int | Lt_rat_rat | Lt_mon_mon
            | Lt_dat_dat | Lt_dur_dur | Lte_int_int | Lte_rat_rat | Lte_mon_mon
            | Lte_dat_dat | Lte_dur_dur | Gt_int_int | Gt_rat_rat | Gt_mon_mon
            | Gt_dat_dat | Gt_dur_dur | Gte_int_int | Gte_rat_rat | Gte_mon_mon
            | Gte_dat_dat | Gte_dur_dur | Eq_int_int | Eq_rat_rat | Eq_mon_mon
            | Eq_dur_dur | Eq_dat_dat ),
            _ ) as op;
      } ->
    ( EAppOp
        {
          op;
          tys;
          args =
            [
              partially_evaluate_expr_for_assertion_failure_message ctx lang e1;
              partially_evaluate_expr_for_assertion_failure_message ctx lang e2;
            ];
        },
      Mark.get e )
  (* TODO: improve this heuristic, because if the assertion is not [e1 <op> e2],
     the error message merely displays [false]... *)
  | _ -> evaluate_expr ctx lang e

let evaluate_expr_trace :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr =
 fun ctx lang e ->
  Runtime.reset_log ();
  Fun.protect
    (fun () -> evaluate_expr ctx lang e)
    ~finally:(fun () ->
      match Global.options.trace with
      | None -> ()
      | Some (lazy ppf) ->
        let trace = Runtime.retrieve_log () in
        if trace = [] then
          (* FIXME: we call evaluate twice: once to generate the scope function
             and once for the actual call scope call. A proper fix would be to
             disable the trace for the the first pass. *)
          ()
        else
          let output_trace fmt =
            match Global.options.trace_format with
            | Human ->
              Format.pp_open_vbox ppf 0;
              ignore @@ List.fold_left (print_log ppf lang) 0 trace;
              Format.pp_close_box ppf ()
            | JSON ->
              Format.fprintf fmt "@[<v 2>[@,";
              Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
                Format.pp_print_string fmt
                (List.map Runtime.Json.raw_event trace);
              Format.fprintf fmt "]@]@."
          in
          Fun.protect
            (fun () -> output_trace ppf)
            ~finally:(fun () -> Format.pp_print_flush ppf ()))

let evaluate_expr_safe :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr =
 fun ctx lang e ->
  try evaluate_expr_trace ctx lang e
  with Runtime.Error (err, rpos) ->
    Message.error
      ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) rpos)
      "During evaluation: %a." Format.pp_print_text
      (Runtime.error_message err)

(* Typing shenanigan to add custom terms to the AST type. *)
let addcustom e =
  let rec f :
      type c d.
      ((d, c) interpr_kind, 't) gexpr -> ((d, yes) interpr_kind, 't) gexpr boxed
      = function
    | (ECustom _, _) as e -> Expr.map ~f e
    | EAppOp { op; tys; args }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmpty, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | EFatalError _ | ELit _ | EApp _ | EArray _ | EVar _
        | EExternal _ | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _
        | EInj _ | EStruct _ | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  let open struct
    external id :
      (('d, 'c) interpr_kind, 't) gexpr -> (('d, yes) interpr_kind, 't) gexpr
      = "%identity"
  end in
  if false then Expr.unbox (f e)
    (* We keep the implementation as a typing proof, but bypass the AST
       traversal for performance. Note that it's not completely 1-1 since the
       traversal would do a reboxing of all bound variables *)
  else id e

let delcustom e =
  let rec f :
      type c d.
      ((d, c) interpr_kind, 't) gexpr -> ((d, no) interpr_kind, 't) gexpr boxed
      = function
    | ECustom _, _ -> invalid_arg "Custom term remaining in evaluated term"
    | EAppOp { op; args; tys }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmpty, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | EFatalError _ | ELit _ | EApp _ | EArray _ | EVar _
        | EExternal _ | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _
        | EInj _ | EStruct _ | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  (* /!\ don't be tempted to use the same trick here, the function does one
     thing: validate at runtime that the term does not contain [ECustom]
     nodes. *)
  Expr.unbox (f e)

let interpret_program_lcalc p s : (Uid.MarkedString.info * ('a, 'm) gexpr) list
    =
  Message.with_delayed_errors (fun () ->
      let e = Expr.unbox @@ Program.to_expr p s in
      let ctx = p.decl_ctx in
      match evaluate_expr_safe ctx p.lang (addcustom e) with
      | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e ->
        begin
        (* At this point, the interpreter seeks to execute the scope but does
           not have a way to retrieve input values from the command line. [taus]
           contain the types of the scope arguments. For [context] arguments, we
           can provide an empty thunked term. But for [input] arguments of
           another type, we cannot provide anything so we have to fail. *)
        let taus = StructName.Map.find s_in ctx.ctx_structs in
        let application_term =
          let pos = Expr.mark_pos mark_e in
          StructField.Map.map
            (fun ty ->
              match Mark.remove ty with
              | TArrow (ty_in, (TOption _, _)) ->
                (* Context args should return an option *)
                Expr.make_abs
                  (List.map (fun _ -> Mark.ghost (Var.make "_")) ty_in)
                  (Expr.einj ~e:(Expr.elit LUnit mark_e) ~cons:Expr.none_constr
                     ~name:Expr.option_enum mark_e
                    : (_, _) boxed_gexpr)
                  ty_in pos
              | TTuple ((TArrow (ty_in, (TOption _, _)), _) :: _) ->
                (* ... or a closure if closure conversion is enabled *)
                Expr.make_tuple
                  [
                    Expr.make_abs
                      (List.map (fun _ -> Mark.ghost (Var.make "_")) ty_in)
                      (Expr.einj ~e:(Expr.elit LUnit mark_e)
                         ~cons:Expr.none_constr ~name:Expr.option_enum mark_e)
                      ty_in (Expr.mark_pos mark_e);
                    Expr.eappop
                      ~op:(Operator.ToClosureEnv, pos)
                      ~args:[Expr.etuple [] mark_e]
                      ~tys:[TClosureEnv, pos]
                      mark_e;
                  ]
                  mark_e
              | TOption ty ->
                Expr.einj ~cons:Expr.none_constr ~name:Expr.option_enum
                  ~e:
                    (Expr.elit LUnit
                       (Expr.with_ty mark_e (TLit TUnit, Expr.pos e)))
                  (Expr.with_ty mark_e (TOption ty, Expr.pos e))
              | _ ->
                Message.error ~pos:(Mark.get ty)
                  "This scope needs an input argument of type@ %a@ %a"
                  Print.typ_debug ty Format.pp_print_text
                  "to be executed. But the Catala built-in interpreter does \
                   not have a way to retrieve input values from the command \
                   line, so it cannot execute this scope. Please create \
                   another scope that provides the input arguments to this one \
                   and execute it instead.")
            taus
        in
        let to_interpret =
          Expr.make_app (Expr.box e)
            [
              Expr.estruct ~name:s_in ~fields:application_term
                (Expr.map_ty (fun (_, pos) -> TStruct s_in, pos) mark_e);
            ]
            [TStruct s_in, Expr.pos e]
            (Expr.pos e)
        in
        match
          Mark.remove (evaluate_expr_safe ctx p.lang (Expr.unbox to_interpret))
        with
        | EStruct { fields; _ } ->
          List.map
            (fun (fld, e) -> StructField.get_info fld, e)
            (StructField.Map.bindings fields)
        | exception Runtime.Error (err, rpos) ->
          Message.error
            ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) rpos)
            "%a" Format.pp_print_text
            (Runtime.error_message err)
        | _ ->
          Message.error ~pos:(Expr.pos e) ~internal:true "%a"
            Format.pp_print_text
            "The interpretation of the program doesn't yield a struct \
             corresponding to the scope variables"
      end
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "The interpreter can only interpret terms starting with functions \
           having thunked arguments")

(** {1 API} *)
let interpret_program_dcalc p s : (Uid.MarkedString.info * ('a, 'm) gexpr) list
    =
  Message.with_delayed_errors (fun () ->
      let ctx = p.decl_ctx in
      let e = Expr.unbox (Program.to_expr p s) in
      match evaluate_expr_safe p.decl_ctx p.lang (addcustom e) with
      | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e ->
        begin
        (* At this point, the interpreter seeks to execute the scope but does
           not have a way to retrieve input values from the command line. [taus]
           contain the types of the scope arguments. For [context] arguments, we
           can provide an empty thunked term. But for [input] arguments of
           another type, we cannot provide anything so we have to fail. *)
        let taus = StructName.Map.find s_in ctx.ctx_structs in
        let application_term =
          StructField.Map.map
            (fun ty0 ->
              match Mark.remove ty0 with
              | TArrow (ty_in, ty_out) ->
                Expr.make_abs
                  (List.map (fun _ -> Mark.ghost (Var.make "_")) ty_in)
                  (Bindlib.box EEmpty, Expr.with_ty mark_e ty_out)
                  ty_in (Expr.mark_pos mark_e)
              | TDefault _ -> Bindlib.box EEmpty, Expr.with_ty mark_e ty0
              | _ ->
                Message.error ~pos:(Mark.get ty0) "%a" Format.pp_print_text
                  "This scope needs input arguments to be executed. But the \
                   Catala built-in interpreter does not have a way to retrieve \
                   input values from the command line, so it cannot execute \
                   this scope. Please create another scope that provides the \
                   input arguments to this one and execute it instead.")
            taus
        in
        let to_interpret =
          Expr.make_app (Expr.box e)
            [
              Expr.estruct ~name:s_in ~fields:application_term
                (Expr.map_ty (fun (_, pos) -> TStruct s_in, pos) mark_e);
            ]
            [TStruct s_in, Expr.pos e]
            (Expr.pos e)
        in
        match
          Mark.remove (evaluate_expr_safe ctx p.lang (Expr.unbox to_interpret))
        with
        | EStruct { fields; _ } ->
          List.map
            (fun (fld, e) -> StructField.get_info fld, e)
            (StructField.Map.bindings fields)
        | _ ->
          Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
            "The interpretation of a program should always yield a struct \
             corresponding to the scope variables"
      end
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "The interpreter can only interpret terms starting with functions \
           having thunked arguments")

(* Evaluation may introduce intermediate custom terms ([ECustom], pointers to
   external functions), straying away from the DCalc and LCalc ASTS. [addcustom]
   and [delcustom] are needed to expand and shrink the type of the terms to
   reflect that. *)
let evaluate_expr ctx lang e =
  Fun.protect ~finally:Runtime.reset_log
  @@ fun () -> evaluate_expr ctx lang (addcustom e)

let loaded_modules = Hashtbl.create 17

let load_runtime_modules ~hashf prg =
  let load (mname, intf_id) =
    let hash = hashf intf_id.hash in
    if Hashtbl.mem loaded_modules mname then ()
    else
      let expect_hash =
        if intf_id.is_external then Hash.external_placeholder
        else Hash.to_string hash
      in
      let obj_file =
        let src = Pos.get_file (Mark.get (ModuleName.get_info mname)) in
        Dynlink.adapt_filename
          File.((dirname src / ModuleName.to_string mname) ^ ".cmo")
      in
      (if not (Sys.file_exists obj_file) then
         Message.error
           ~pos_msg:(fun ppf ->
             Format.pp_print_string ppf "Module defined here")
           ~pos:(Mark.get (ModuleName.get_info mname))
           "Compiled OCaml object %a@ not@ found.@ Make sure it has been \
            suitably compiled."
           File.format obj_file
       else
         try Dynlink.loadfile obj_file
         with Dynlink.Error dl_err ->
           Message.error
             "While loading compiled module from %a:@;<1 2>@[<hov>%a@]"
             File.format obj_file Format.pp_print_text
             (Dynlink.error_message dl_err));
      match Runtime.check_module (ModuleName.to_string mname) expect_hash with
      | Ok () -> Hashtbl.add loaded_modules mname hash
      | Error bad_hash ->
        Message.debug
          "Module hash mismatch for %a:@ @[<v>Expected: %a@,Found:    %a@]"
          ModuleName.format mname Hash.format hash
          (fun ppf h ->
            try Hash.format ppf (Hash.of_string h)
            with Failure _ ->
              if h = Hash.external_placeholder then
                Format.fprintf ppf "@{<cyan>%s@}" Hash.external_placeholder
              else Format.fprintf ppf "@{<red><invalid>@}")
          bad_hash;
        Message.error
          "Module %a@ needs@ recompiling:@ %a@ was@ likely@ compiled@ from@ \
           an@ older@ version@ or@ with@ incompatible@ flags."
          ModuleName.format mname File.format obj_file
      | exception Not_found ->
        Message.error
          "Module %a@ was loaded from file %a but did not register properly, \
           there is something wrong in its code."
          ModuleName.format mname File.format obj_file
  in
  let modules_list_topo = Program.modules_to_list prg.decl_ctx.ctx_modules in
  if modules_list_topo <> [] then
    Message.debug "Loading shared modules... %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space ModuleName.format)
      (List.map (fun (m, _) -> m) modules_list_topo);
  List.iter load modules_list_topo
