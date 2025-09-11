(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This file is mostly a copy-paste of catala's
    [compiler/shared_ast/interpreter.ml] adapted for environment based
    interpretation *)

open Catala_utils
open Lwt.Syntax
open Definitions

type ('a, 'b) env =
  | Env of
      ( (('a, yes) interpr_kind, 'b) gexpr,
        (('a, yes) interpr_kind, 'b) gexpr * ('a, 'b) env )
      Var.Map.t

let empty_env = Env Var.Map.empty

let join_env (Env env) (Env env') =
  Env
    (Var.Map.union
       (fun _ l _r ->
         (* Variables are immutable so collisions reference the same values *)
         Some l)
       env env')

let rec pp_env fmt (Env env) =
  let open Format in
  Var.Map.bindings env
  |> fprintf fmt "@[<hov 2>%a@]"
       (pp_print_list ~pp_sep:pp_print_space (fun fmt (var, (v, env)) ->
            fprintf fmt "%s#%i: %a {%a}" (Bindlib.name_of var)
              (Bindlib.uid_of var) Expr.format v pp_env env))

let pp_toplevel_env fmt (Env env) =
  let open Format in
  Var.Map.bindings env
  |> fprintf fmt "@[<hov 2>%a@]"
       (pp_print_list ~pp_sep:pp_print_space (fun fmt (var, (v, _env)) ->
            fprintf fmt "%s#%i: %a" (Bindlib.name_of var) (Bindlib.uid_of var)
              Expr.format v))

let is_empty_error : type a. (a, 'm) gexpr -> bool =
 fun e -> match Mark.remove e with EEmpty -> true | _ -> false

let lwt_map2 f l l' =
  let ll = List.combine l l' in
  Lwt_list.map_s (fun (x, y) -> f x y) ll

let rec runtime_to_val :
    type d.
    (decl_ctx ->
    ((d, _) interpr_kind, 'm) gexpr ->
    ((d, _) interpr_kind, 'm) gexpr Lwt.t) ->
    decl_ctx ->
    'm mark ->
    typ ->
    Obj.t ->
    (((d, yes) interpr_kind as 'a), 'm) gexpr Lwt.t =
 fun eval_expr ctx m ty o ->
  let m = Expr.map_ty (fun _ -> ty) m in
  match Mark.remove ty with
  | TLit TBool -> Lwt.return (ELit (LBool (Obj.obj o)), m)
  | TLit TUnit -> Lwt.return (ELit LUnit, m)
  | TLit TInt -> Lwt.return (ELit (LInt (Obj.obj o)), m)
  | TLit TRat -> Lwt.return (ELit (LRat (Obj.obj o)), m)
  | TLit TMoney -> Lwt.return (ELit (LMoney (Obj.obj o)), m)
  | TLit TDate -> Lwt.return (ELit (LDate (Obj.obj o)), m)
  | TLit TDuration -> Lwt.return (ELit (LDuration (Obj.obj o)), m)
  | TLit TPos ->
    Lwt.return
    @@
    let rpos : Runtime.source_position = Obj.obj o in
    let p =
      Pos.from_info rpos.filename rpos.start_line rpos.start_column
        rpos.end_line rpos.end_column
    in
    let p = Pos.overwrite_law_info p rpos.law_headings in
    EPos p, m
  | TTuple ts ->
    let* ll =
      lwt_map2 (runtime_to_val eval_expr ctx m) ts (Array.to_list (Obj.obj o))
    in
    Lwt.return (ETuple ll, m)
  | TStruct name ->
    let bindings =
      StructName.Map.find name ctx.ctx_structs |> StructField.Map.bindings
    in
    let* l =
      lwt_map2
        (fun o (fld, ty) ->
          let* e = runtime_to_val eval_expr ctx m ty o in
          Lwt.return (fld, e))
        (Array.to_list (Obj.obj o))
        bindings
    in
    StructField.Map.of_list l
    |> fun fields -> Lwt.return (EStruct { name; fields }, m)
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
    let* e = runtime_to_val eval_expr ctx m ty (Obj.field o 0) in
    Lwt.return (EInj { name; cons; e }, m)
  | TOption ty -> (
    match Obj.tag o - Obj.first_non_constant_constructor_tag with
    | 0 ->
      let* e =
        runtime_to_val eval_expr ctx m (TLit TUnit, Pos.void) (Obj.field o 0)
      in
      Lwt.return
        (EInj { name = Expr.option_enum; cons = Expr.none_constr; e }, m)
    | 1 ->
      let* e = runtime_to_val eval_expr ctx m ty (Obj.field o 0) in
      Lwt.return
        (EInj { name = Expr.option_enum; cons = Expr.some_constr; e }, m)
    | _ -> assert false)
  | TClosureEnv ->
    (* By construction, a closure environment can only be consumed from the same
       scope where it was built (compiled or not) ; for this reason, we can
       safely avoid converting in depth here *)
    Lwt.return (Obj.obj o, m)
  | TArray ty ->
    let* l =
      Lwt_list.map_s
        (runtime_to_val eval_expr ctx m ty)
        (Array.to_list (Obj.obj o))
    in
    Lwt.return (EArray l, m)
  | TArrow (targs, tret) -> Lwt.return (ECustom { obj = o; targs; tret }, m)
  | TDefault ty -> (
    (* This case is only valid for ASTs including default terms; but the typer
       isn't aware so we need some additional dark arts. *)
    match (Obj.obj o : 'a Runtime.Optional.t) with
    | Runtime.Optional.Absent () -> Lwt.return (Obj.magic EEmpty, m)
    | Runtime.Optional.Present o -> (
      let* r = runtime_to_val eval_expr ctx m ty o in
      match r with
      | ETuple [(e, m); (EPos pos, _)], _ -> Lwt.return (e, Expr.with_pos pos m)
      | _ -> assert false))
  | TForAll tb ->
    let _v, ty = Bindlib.unmbind tb in
    runtime_to_val eval_expr ctx m ty o
  | TVar _ ->
    (* A type variable being an unresolved type, it can't be deconstructed, so
       we can let it pass through. *)
    Lwt.return (Obj.obj o, m)

and val_to_runtime :
    type d.
    (decl_ctx ->
    ((d, _) interpr_kind, 'm) gexpr ->
    ((d, _) interpr_kind, 'm) gexpr Lwt.t) ->
    decl_ctx ->
    typ ->
    ((d, _) interpr_kind, 'm) gexpr ->
    Obj.t Lwt.t =
 fun eval_expr ctx ty v ->
  match Mark.remove ty, Mark.remove v with
  | TLit TBool, ELit (LBool b) -> Lwt.return (Obj.repr b)
  | TLit TUnit, ELit LUnit -> Lwt.return (Obj.repr ())
  | TLit TInt, ELit (LInt i) -> Lwt.return (Obj.repr i)
  | TLit TRat, ELit (LRat r) -> Lwt.return (Obj.repr r)
  | TLit TMoney, ELit (LMoney m) -> Lwt.return (Obj.repr m)
  | TLit TDate, ELit (LDate t) -> Lwt.return (Obj.repr t)
  | TLit TDuration, ELit (LDuration d) -> Lwt.return (Obj.repr d)
  | TLit TPos, EPos p ->
    let rpos : Runtime.source_position =
      {
        Runtime.filename = Pos.get_file p;
        start_line = Pos.get_start_line p;
        start_column = Pos.get_start_column p;
        end_line = Pos.get_end_line p;
        end_column = Pos.get_end_column p;
        law_headings = Pos.get_law_info p;
      }
    in
    Lwt.return (Obj.repr rpos)
  | TTuple ts, ETuple es ->
    let* ll = lwt_map2 (val_to_runtime eval_expr ctx) ts es in
    Lwt.return (Array.of_list ll |> Obj.repr)
  | TStruct name1, EStruct { name; fields } ->
    assert (StructName.equal name name1);
    let fld_tys = StructName.Map.find name ctx.ctx_structs in
    let* ll =
      lwt_map2
        (fun (_, ty) (_, v) -> val_to_runtime eval_expr ctx ty v)
        (StructField.Map.bindings fld_tys)
        (StructField.Map.bindings fields)
    in
    Lwt.return (Array.of_list ll |> Obj.repr)
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
    let* field = val_to_runtime eval_expr ctx ty e in
    let o = Obj.with_tag tag (Obj.repr (Some ())) in
    Obj.set_field o 0 field;
    Lwt.return o
  | TOption ty, EInj { name; cons; e } ->
    assert (EnumName.equal name Expr.option_enum);
    let tag, ty =
      (* None is before Some because the constructors have been defined in this
         order in [expr.ml], and the ident maps preserve definition ordering *)
      if EnumConstructor.equal cons Expr.none_constr then
        Obj.first_non_constant_constructor_tag, (TLit TUnit, Pos.void)
      else if EnumConstructor.equal cons Expr.some_constr then
        Obj.first_non_constant_constructor_tag + 1, ty
      else assert false
    in
    let* field = val_to_runtime eval_expr ctx ty e in
    let o = Obj.with_tag tag (Obj.repr (Some ())) in
    Obj.set_field o 0 field;
    Lwt.return o
  | TArray ty, EArray es ->
    let* l = Lwt_list.map_s (val_to_runtime eval_expr ctx ty) es in
    Lwt.return (Array.of_list l |> Obj.repr)
  | TArrow (targs, tret), _ ->
    let m = Mark.get v in
    (* we want stg like [fun args -> val_to_runtime (eval_expr ctx (EApp (v,
       args)))] but in curried form *)
    let rec curry acc = function
      | [] ->
        let args = List.rev acc in
        let tys = List.map (fun a -> Expr.maybe_ty (Mark.get a)) args in
        let* e = eval_expr ctx (EApp { f = v; args; tys }, m) in
        val_to_runtime eval_expr ctx tret e
      | targ :: targs ->
        Lwt.return
          (Obj.repr (fun x ->
               let* x = runtime_to_val eval_expr ctx m targ x in
               curry (x :: acc) targs))
    in
    curry [] targs
  | TDefault ty, _ -> (
    (* In dcalc, this is an expression. in the runtime (lcalc), this is an
       option(pair(expression, pos)) *)
    match v with
    | EEmpty, _ -> Lwt.return (Obj.repr (Runtime.Optional.Absent ()))
    | EPureDefault e, m | ((_, m) as e) ->
      let* e = eval_expr ctx e in
      let pos = Expr.pos e in
      let ty = TTuple [ty; TLit TPos, pos], pos in
      let with_pos =
        ETuple [e; EPos pos, Expr.with_ty m (TLit TPos, pos)], Expr.with_ty m ty
      in
      let* v = val_to_runtime eval_expr ctx ty with_pos in
      Lwt.return (Obj.repr (Runtime.Optional.Present v)))
  | TForAll tb, _ ->
    let _v, ty = Bindlib.unmbind tb in
    val_to_runtime eval_expr ctx ty v
  | TVar _, v ->
    (* A type variable being an unresolved type, it can't be deconstructed, so
       we can let it pass through. *)
    Lwt.return (Obj.repr v)
  | TClosureEnv, v ->
    (* By construction, a closure environment can only be consumed from the same
       scope where it was built (compiled or not) ; for this reason, we can
       safely avoid converting in depth here *)
    Lwt.return (Obj.repr v)
  | _ ->
    Message.error ~internal:true
      "Could not convert value of type %a@ to@ runtime:@ %a" Print.typ ty
      Expr.format v

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
let handle_eq
    pos
    (evaluate_operator :
      _ operator Mark.pos ->
      'a mark ->
      Global.backend_lang ->
      ((< .. > as 'b), 'c) Shared_ast__Definitions.gexpr list ->
      (('b, 'b, 'c) base_gexpr, 'a mark) Mark.ed Lwt.t)
    m
    lang
    e1
    e2 =
  let eq_eval = evaluate_operator (Op.Eq, pos) m lang in
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> Lwt.return_true
  | ELit (LBool b1), ELit (LBool b2) -> Lwt.return (o_eq_boo_boo b1 b2)
  | ELit (LInt x1), ELit (LInt x2) -> Lwt.return (o_eq_int_int x1 x2)
  | ELit (LRat x1), ELit (LRat x2) -> Lwt.return (o_eq_rat_rat x1 x2)
  | ELit (LMoney x1), ELit (LMoney x2) -> Lwt.return (o_eq_mon_mon x1 x2)
  | ELit (LDuration x1), ELit (LDuration x2) ->
    Lwt.return (o_eq_dur_dur (Expr.pos_to_runtime (Expr.mark_pos m)) x1 x2)
  | ELit (LDate x1), ELit (LDate x2) -> Lwt.return (o_eq_dat_dat x1 x2)
  | EArray es1, EArray es2 | ETuple es1, ETuple es2 -> (
    try
      Lwt_list.for_all_s
        (fun (e1, e2) ->
          let* r = eq_eval [e1; e2] in
          match Mark.remove r with
          | ELit (LBool b) -> Lwt.return b
          | _ -> assert false
          (* should not happen *))
        (List.combine es1 es2)
    with Invalid_argument _ -> Lwt.return_false)
  | EStruct { fields = es1; name = s1 }, EStruct { fields = es2; name = s2 } ->
    if not (StructName.equal s1 s2) then Lwt.return_false
    else
      let bls =
        List.combine
          (StructField.Map.bindings es1)
          (StructField.Map.bindings es2)
      in
      Lwt_list.for_all_p
        (fun ((k1, e1), (k2, e2)) ->
          if not (StructField.equal k1 k2) then Lwt.return_false
          else
            let* r = eq_eval [e1; e2] in
            match Mark.remove r with
            | ELit (LBool b) -> Lwt.return b
            | _ -> assert false (* should not happen *))
        bls
  | ( EInj { e = e1; cons = i1; name = en1 },
      EInj { e = e2; cons = i2; name = en2 } ) -> (
    try
      if not (EnumName.equal en1 en2 && EnumConstructor.equal i1 i2) then
        Lwt.return_false
      else
        let* r = eq_eval [e1; e2] in
        match Mark.remove r with
        | ELit (LBool b) -> Lwt.return b
        | _ -> assert false (* should not happen *)
    with Invalid_argument _ -> Lwt.return_false)
  | _, _ -> Lwt.return_false (* comparing anything else return false *)

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
    (evaluate_expr :
      (_ interpr_kind, 'm) gexpr -> (_ interpr_kind, 'm) gexpr Lwt.t)
    ((op, opos) : < overloaded : no ; .. > operator Mark.pos)
    m
    lang
    args : 'a Lwt.t =
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
  let* r =
    match op, args with
    | Length, [(EArray es, _)] ->
      Lwt.return (ELit (LInt (Runtime.integer_of_int (List.length es))))
    | Log (entry, infos), [(e, _)] when Global.options.trace <> None -> (
      let rtinfos = List.map Uid.MarkedString.to_string infos in
      match entry with
      | BeginCall -> Lwt.return (Runtime.log_begin_call rtinfos e)
      | EndCall -> Lwt.return (Runtime.log_end_call rtinfos e)
      | PosRecordIfTrueBool ->
        (match e with
        | ELit (LBool b) ->
          Runtime.log_decision_taken (Expr.pos_to_runtime pos) b |> ignore
        | _ -> ());
        Lwt.return e
      | VarDef def ->
        Lwt.return
          (Runtime.log_variable_definition rtinfos
             {
               Runtime.io_input = def.log_io_input;
               io_output = def.log_io_output;
             }
             value_to_runtime_embedded e))
    | Log _, [(e', _)] -> Lwt.return e'
    | (FromClosureEnv | ToClosureEnv), [e'] ->
      (* [FromClosureEnv] and [ToClosureEnv] are just there to bypass the need
         for existential types when typing code after closure conversion. There
         are effectively no-ops. *)
      Lwt.return (Mark.remove e')
    | (ToClosureEnv | FromClosureEnv), _ -> err ()
    | Eq, [(e1, _); (e2, _)] ->
      let* b = handle_eq opos (evaluate_operator evaluate_expr) m lang e1 e2 in
      Lwt.return (ELit (LBool b))
    | Map, [f; (EArray es, _)] ->
      let* l =
        Lwt_list.map_s (fun e' -> eval_application evaluate_expr f [e']) es
      in
      Lwt.return (EArray l)
    | Map2, [f; (EArray es1, _); (EArray es2, _)] -> (
      try
        let* l =
          lwt_map2
            (fun e1 e2 -> eval_application evaluate_expr f [e1; e2])
            es1 es2
        in
        Lwt.return (EArray l)
      with Invalid_argument _ ->
        raise Runtime.(Error (NotSameLength, [Expr.pos_to_runtime opos])))
    | Reduce, [_; default; (EArray [], _)] ->
      let* r =
        eval_application evaluate_expr default
          [ELit LUnit, Expr.with_ty m (TLit TUnit, pos)]
      in
      Lwt.return (Mark.remove r)
    | Reduce, [f; _; (EArray (x0 :: xn), _)] ->
      let* r =
        Lwt_list.fold_left_s
          (fun acc x -> eval_application evaluate_expr f [acc; x])
          x0 xn
      in
      Lwt.return (Mark.remove r)
    | Concat, [(EArray es1, _); (EArray es2, _)] ->
      Lwt.return (EArray (es1 @ es2))
    | Filter, [f; (EArray es, _)] ->
      let* l =
        Lwt_list.filter_s
          (fun e' ->
            let* r = eval_application evaluate_expr f [e'] in
            match r with
            | ELit (LBool b), _ -> Lwt.return b
            | _ ->
              Message.error
                ~pos:(Expr.pos (List.nth args 0))
                "%a" Format.pp_print_text
                "This predicate evaluated to something else than a boolean \
                 (should not happen if the term was well-typed)")
          es
      in
      Lwt.return (EArray l)
    | Fold, [f; init; (EArray es, _)] ->
      let* r =
        Lwt_list.fold_left_s
          (fun acc e' -> eval_application evaluate_expr f [acc; e'])
          init es
      in
      Lwt.return (Mark.remove r)
    | (Length | Log _ | Eq | Map | Map2 | Concat | Filter | Fold | Reduce), _ ->
      err ()
    | Not, [(ELit (LBool b), _)] -> Lwt.return (ELit (LBool (o_not b)))
    | GetDay, [(ELit (LDate d), _)] -> Lwt.return (ELit (LInt (o_getDay d)))
    | GetMonth, [(ELit (LDate d), _)] -> Lwt.return (ELit (LInt (o_getMonth d)))
    | GetYear, [(ELit (LDate d), _)] -> Lwt.return (ELit (LInt (o_getYear d)))
    | FirstDayOfMonth, [(ELit (LDate d), _)] ->
      Lwt.return (ELit (LDate (o_firstDayOfMonth d)))
    | LastDayOfMonth, [(ELit (LDate d), _)] ->
      Lwt.return (ELit (LDate (o_lastDayOfMonth d)))
    | And, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
      Lwt.return (ELit (LBool (o_and b1 b2)))
    | Or, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
      Lwt.return (ELit (LBool (o_or b1 b2)))
    | Xor, [(ELit (LBool b1), _); (ELit (LBool b2), _)] ->
      Lwt.return (ELit (LBool (o_xor b1 b2)))
    | ( ( Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth
        | And | Or | Xor ),
        _ ) ->
      err ()
    | Minus_int, [(ELit (LInt x), _)] ->
      Lwt.return (ELit (LInt (o_minus_int x)))
    | Minus_rat, [(ELit (LRat x), _)] ->
      Lwt.return (ELit (LRat (o_minus_rat x)))
    | Minus_mon, [(ELit (LMoney x), _)] ->
      Lwt.return (ELit (LMoney (o_minus_mon x)))
    | Minus_dur, [(ELit (LDuration x), _)] ->
      Lwt.return (ELit (LDuration (o_minus_dur x)))
    | ToInt_rat, [(ELit (LRat x), _)] ->
      Lwt.return (ELit (LInt (o_toint_rat x)))
    | ToRat_int, [(ELit (LInt i), _)] ->
      Lwt.return (ELit (LRat (o_torat_int i)))
    | ToRat_mon, [(ELit (LMoney i), _)] ->
      Lwt.return (ELit (LRat (o_torat_mon i)))
    | ToMoney_rat, [(ELit (LRat i), _)] ->
      Lwt.return (ELit (LMoney (o_tomoney_rat i)))
    | Round_mon, [(ELit (LMoney m), _)] ->
      Lwt.return (ELit (LMoney (o_round_mon m)))
    | Round_rat, [(ELit (LRat m), _)] ->
      Lwt.return (ELit (LRat (o_round_rat m)))
    | Add_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LInt (o_add_int_int x y)))
    | Add_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LRat (o_add_rat_rat x y)))
    | Add_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LMoney (o_add_mon_mon x y)))
    | Add_dat_dur r, [(ELit (LDate x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LDate (o_add_dat_dur r (rpos ()) x y)))
    | Add_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LDuration (o_add_dur_dur x y)))
    | Sub_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LInt (o_sub_int_int x y)))
    | Sub_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LRat (o_sub_rat_rat x y)))
    | Sub_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LMoney (o_sub_mon_mon x y)))
    | Sub_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LDuration (o_sub_dat_dat x y)))
    | Sub_dat_dur r, [(ELit (LDate x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LDate (o_sub_dat_dur r (rpos ()) x y)))
    | Sub_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LDuration (o_sub_dur_dur x y)))
    | Mult_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LInt (o_mult_int_int x y)))
    | Mult_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LRat (o_mult_rat_rat x y)))
    | Mult_mon_int, [(ELit (LMoney x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LMoney (o_mult_mon_int x y)))
    | Mult_mon_rat, [(ELit (LMoney x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LMoney (o_mult_mon_rat x y)))
    | Mult_dur_int, [(ELit (LDuration x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LDuration (o_mult_dur_int x y)))
    | Div_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LRat (o_div_int_int (div_pos ()) x y)))
    | Div_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LRat (o_div_rat_rat (div_pos ()) x y)))
    | Div_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LRat (o_div_mon_mon (div_pos ()) x y)))
    | Div_mon_int, [(ELit (LMoney x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LMoney (o_div_mon_int (div_pos ()) x y)))
    | Div_mon_rat, [(ELit (LMoney x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LMoney (o_div_mon_rat (div_pos ()) x y)))
    | Div_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LRat (o_div_dur_dur (div_pos ()) x y)))
    | Lt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LBool (o_lt_int_int x y)))
    | Lt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LBool (o_lt_rat_rat x y)))
    | Lt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LBool (o_lt_mon_mon x y)))
    | Lt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LBool (o_lt_dat_dat x y)))
    | Lt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LBool (o_lt_dur_dur (rpos ()) x y)))
    | Lte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LBool (o_lte_int_int x y)))
    | Lte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LBool (o_lte_rat_rat x y)))
    | Lte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LBool (o_lte_mon_mon x y)))
    | Lte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LBool (o_lte_dat_dat x y)))
    | Lte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LBool (o_lte_dur_dur (rpos ()) x y)))
    | Gt_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LBool (o_gt_int_int x y)))
    | Gt_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LBool (o_gt_rat_rat x y)))
    | Gt_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LBool (o_gt_mon_mon x y)))
    | Gt_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LBool (o_gt_dat_dat x y)))
    | Gt_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LBool (o_gt_dur_dur (rpos ()) x y)))
    | Gte_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LBool (o_gte_int_int x y)))
    | Gte_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LBool (o_gte_rat_rat x y)))
    | Gte_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LBool (o_gte_mon_mon x y)))
    | Gte_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LBool (o_gte_dat_dat x y)))
    | Gte_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LBool (o_gte_dur_dur (rpos ()) x y)))
    | Eq_boo_boo, [(ELit (LBool x), _); (ELit (LBool y), _)] ->
      Lwt.return (ELit (LBool (o_eq_boo_boo x y)))
    | Eq_int_int, [(ELit (LInt x), _); (ELit (LInt y), _)] ->
      Lwt.return (ELit (LBool (o_eq_int_int x y)))
    | Eq_rat_rat, [(ELit (LRat x), _); (ELit (LRat y), _)] ->
      Lwt.return (ELit (LBool (o_eq_rat_rat x y)))
    | Eq_mon_mon, [(ELit (LMoney x), _); (ELit (LMoney y), _)] ->
      Lwt.return (ELit (LBool (o_eq_mon_mon x y)))
    | Eq_dat_dat, [(ELit (LDate x), _); (ELit (LDate y), _)] ->
      Lwt.return (ELit (LBool (o_eq_dat_dat x y)))
    | Eq_dur_dur, [(ELit (LDuration x), _); (ELit (LDuration y), _)] ->
      Lwt.return (ELit (LBool (o_eq_dur_dur (rpos ()) x y)))
    | HandleExceptions, [(EArray exps, _)] -> (
      (* Shallow conversion to runtime option, so that we can call
         [handle_exceptions] *)
      let exps =
        List.map
          (function
            | EInj { name; cons; e }, _
              when EnumName.equal name Expr.option_enum ->
              if EnumConstructor.equal cons Expr.some_constr then
                match e with
                | ETuple [e; (EPos p, _)], _ ->
                  Runtime.Optional.Present (e, Expr.pos_to_runtime p)
                | _ -> err ()
              else Runtime.Optional.Absent ()
            | _ -> err ())
          exps
      in
      match Runtime.handle_exceptions (Array.of_list exps) with
      | Runtime.Optional.Absent () ->
        Lwt.return
          (EInj
             {
               name = Expr.option_enum;
               cons = Expr.none_constr;
               e = ELit LUnit, m;
             })
      | Runtime.Optional.Present (e, rpos) ->
        let p = Expr.runtime_to_pos rpos in
        Lwt.return
          (EInj
             {
               name = Expr.option_enum;
               cons = Expr.some_constr;
               e = ETuple [e; EPos p, Expr.with_pos p m], m;
             }))
    | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToInt_rat | ToRat_int
        | ToRat_mon | ToMoney_rat | Round_rat | Round_mon | Add_int_int
        | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Sub_int_int
        | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur _ | Sub_dur_dur
        | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat
        | Mult_dur_int | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int
        | Div_mon_rat | Div_dur_dur | Lt_int_int | Lt_rat_rat | Lt_mon_mon
        | Lt_dat_dat | Lt_dur_dur | Lte_int_int | Lte_rat_rat | Lte_mon_mon
        | Lte_dat_dat | Lte_dur_dur | Gt_int_int | Gt_rat_rat | Gt_mon_mon
        | Gt_dat_dat | Gt_dur_dur | Gte_int_int | Gte_rat_rat | Gte_mon_mon
        | Gte_dat_dat | Gte_dur_dur | Eq_boo_boo | Eq_int_int | Eq_rat_rat
        | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur | HandleExceptions ),
        _ ) ->
      err ()
  in
  Lwt.return (Mark.add m r)

let rec partially_evaluate_expr_for_assertion_failure_message :
    type d.
    (((d, _) interpr_kind, 'm) gexpr -> ((d, _) interpr_kind, 'm) gexpr Lwt.t) ->
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr Lwt.t =
 fun evaluate_expr ctx lang e ->
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
    let* e1 =
      partially_evaluate_expr_for_assertion_failure_message evaluate_expr ctx
        lang e1
    in
    let* e2 =
      partially_evaluate_expr_for_assertion_failure_message evaluate_expr ctx
        lang e2
    in
    Lwt.return (EAppOp { op; tys; args = [e1; e2] }, Mark.get e)
  (* TODO: improve this heuristic, because if the assertion is not [e1 <op> e2],
     the error message merely displays [false]... *)
  | _ -> evaluate_expr e

let on_expr_void _ _ = Lwt.return_unit

let rec evaluate_expr_with_env :
    type d.
    on_expr:(((d, yes) interpr_kind, 't) gexpr -> (d, 't) env -> unit Lwt.t) ->
    (d, 't) env ->
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    (((d, yes) interpr_kind, 't) gexpr * (d, 't) env) Lwt.t =
 fun ~on_expr env ctx lang e ->
  let* () = on_expr e env in
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  match Mark.remove e with
  | EVar v -> begin
    let (Env env) = env in
    Var.Map.find_opt v env
    |> function
    | Some (e, env') ->
      evaluate_expr_with_env
        ~on_expr:(fun _ _ -> Lwt.return_unit (* Skip variable evaluations? *))
        env' ctx lang e
    | None ->
      Format.kasprintf
        (Message.error ~pos "%a" Format.pp_print_text)
        "Variable '%s#%d' missing from environment at evaluation (should not \
         happen if term was well-typed)"
        (Bindlib.name_of v) (Bindlib.uid_of v)
  end
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
    let* e =
      runtime_to_val
        (fun ctx e ->
          let* e, _ =
            evaluate_expr_with_env ~on_expr:on_expr_void env ctx lang e
          in
          Lwt.return e)
        ctx m ty o
    in
    Lwt.return (e, env)
  | EApp { f = e1; args; _ } -> (
    let (Env env) = env in
    let* e1, Env env_f =
      evaluate_expr_with_env ~on_expr (Env env) ctx lang e1
    in
    let (Env env) = join_env (Env env) (Env env_f) in
    let* args =
      Lwt_list.map_s (evaluate_expr_with_env ~on_expr (Env env) ctx lang) args
    in
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      if Bindlib.mbinder_arity binder = List.length args then
        let vars, e = Bindlib.unmbind binder in
        let env =
          List.fold_left2
            (fun (Env env) v (e, Env env') ->
              Env (Var.Map.add v (e, Env env') env))
            (Env env_f) (Array.to_list vars) args
        in
        evaluate_expr_with_env ~on_expr env ctx lang e
      else
        Message.error ~pos "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | ECustom { obj; targs; tret } ->
      (* Applies the arguments one by one to the curried form *)
      let* o =
        List.fold_left2
          (fun fobj targ arg ->
            let* fobj = fobj in
            let* arg =
              val_to_runtime
                (fun ctx e ->
                  let* e, _ =
                    evaluate_expr_with_env ~on_expr:on_expr_void (Env env) ctx
                      lang e
                  in
                  Lwt.return e)
                ctx targ arg
            in
            let f : Obj.t -> Obj.t =
              if Obj.tag fobj = Obj.first_non_constant_constructor_tag then
                (* Function is not a closure, but a pair, we assume closure
                   conversion has been done *)
                let (f, x0) : ('a -> Obj.t -> Obj.t) * 'a = Obj.obj fobj in
                f x0
              else Obj.obj fobj
            in
            Lwt.return (f arg))
          (Lwt.return obj) targs (List.map fst args)
      in
      let* rval =
        runtime_to_val
          (fun ctx e ->
            let* e, _ =
              evaluate_expr_with_env ~on_expr:on_expr_void (Env env) ctx lang e
            in
            Lwt.return e)
          ctx m tret o
      in
      Lwt.return (rval, Env env)
    | _ ->
      Message.error ~pos ~internal:true "%a%a" Format.pp_print_text
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed"
        (fun ppf e ->
          if Global.options.debug then Format.fprintf ppf ":@ %a" Expr.format e
          else ())
        e1)
  | EAppOp { op; args; _ } ->
    let* args =
      let* l =
        Lwt_list.map_s (evaluate_expr_with_env ~on_expr env ctx lang) args
      in
      Lwt.return (List.map fst l)
    in
    let* e =
      evaluate_operator
        (fun e ->
          let* e, _ = evaluate_expr_with_env ~on_expr env ctx lang e in
          Lwt.return e)
        op m lang args
    in
    Lwt.return (e, env)
  | EAbs _ | ELit _ | EPos _ | ECustom _ | EEmpty ->
    Lwt.return (e, env) (* these are values *)
  | EStruct { fields = es; name } ->
    let fields, es = List.split (StructField.Map.bindings es) in
    let* ll =
      Lwt_list.map_s (evaluate_expr_with_env ~on_expr env ctx lang) es
    in
    let es, env =
      List.split ll |> fun (l, r) -> l, List.fold_left join_env env r
    in
    Lwt.return
      ( Mark.add m
          (EStruct
             {
               fields =
                 StructField.Map.of_seq
                   (Seq.zip (List.to_seq fields) (List.to_seq es));
               name;
             }),
        env )
  | EStructAccess { e; name = s; field } -> (
    let* e, env = evaluate_expr_with_env ~on_expr env ctx lang e in
    match Mark.remove e with
    | EStruct { fields = es; name } -> (
      if not (StructName.equal s name) then
        Message.error
          ~extra_pos:["", pos; "", Expr.pos e]
          "%a" Format.pp_print_text
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      match StructField.Map.find_opt field es with
      | Some e' -> Lwt.return (e', env)
      | None ->
        Message.error ~pos:(Expr.pos e)
          "Invalid field access %a@ in@ struct@ %a@ (should not happen if the \
           term was well-typed). Fields: %a"
          StructField.format field StructName.format s
          (fun ppf -> StructField.Map.format_keys ppf)
          es)
    | _ ->
      Message.error ~pos:(Expr.pos e)
        "The expression %a@ should@ be@ a@ struct@ %a@ but@ is@ not@ (should \
         not happen if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e StructName.format s)
  | ETuple es ->
    let* l =
      Lwt_list.map_s
        (fun e -> evaluate_expr_with_env ~on_expr env ctx lang e)
        es
    in
    let e, env =
      List.split l |> fun (l, r) -> l, List.fold_left join_env env r
    in
    Lwt.return (Mark.add m (ETuple e), env)
  | ETupleAccess { e = e1; index; size } -> (
    let* r = evaluate_expr_with_env ~on_expr env ctx lang e1 in
    match r with
    | (ETuple es, _), env when List.length es = size ->
      Lwt.return (List.nth es index, env)
    | e, _ ->
      Message.error ~pos:(Expr.pos e)
        "The expression %a@ was@ expected@ to@ be@ a@ tuple@ of@ size@ %d@ \
         (should not happen if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e size)
  | EInj { e; name; cons } ->
    let* e, env = evaluate_expr_with_env ~on_expr env ctx lang e in
    Lwt.return (Mark.add m (EInj { e; name; cons }), env)
  | EMatch { e; cases; name } -> (
    let* e, _env = evaluate_expr_with_env ~on_expr env ctx lang e in
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
      evaluate_expr_with_env ~on_expr env ctx lang new_e
    | _ ->
      Message.error ~pos:(Expr.pos e)
        "Expected a term having a sum type as an argument to a match (should \
         not happen if the term was well-typed")
  | EIfThenElse { cond; etrue; efalse } -> (
    let* cond, _env = evaluate_expr_with_env ~on_expr env ctx lang cond in
    match Mark.remove cond with
    | ELit (LBool true) -> evaluate_expr_with_env ~on_expr env ctx lang etrue
    | ELit (LBool false) -> evaluate_expr_with_env ~on_expr env ctx lang efalse
    | _ ->
      Message.error ~pos:(Expr.pos cond) "%a" Format.pp_print_text
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray es ->
    let* es =
      let* l =
        Lwt_list.map_s
          (fun e -> evaluate_expr_with_env ~on_expr env ctx lang e)
          es
      in
      Lwt.return (List.map fst l)
    in
    Lwt.return (Mark.add m (EArray es), env)
  | EAssert e' -> (
    let* e, env = evaluate_expr_with_env ~on_expr env ctx lang e' in
    match Mark.remove e with
    | ELit (LBool true) -> Lwt.return (Mark.add m (ELit LUnit), env)
    | ELit (LBool false) ->
      if Global.options.stop_on_error then
        raise Runtime.(Error (AssertionFailed, [Expr.pos_to_runtime pos]))
      else
        let* partially_evaluated_assertion_failure_expr =
          partially_evaluate_expr_for_assertion_failure_message
            (fun e ->
              let* e, _ = evaluate_expr_with_env ~on_expr env ctx lang e in
              Lwt.return e)
            ctx lang (Expr.skip_wrappers e')
        in
        (match Mark.remove partially_evaluated_assertion_failure_expr with
        | ELit (LBool false) ->
          if Global.options.no_fail_on_assert then
            Message.warning ~pos "Assertion failed"
          else
            Message.delayed_error ~kind:AssertFailure () ~pos "Assertion failed"
        | _ ->
          if Global.options.no_fail_on_assert then
            Message.warning ~pos "Assertion failed:@ %a"
              (Print.UserFacing.expr lang)
              partially_evaluated_assertion_failure_expr
          else
            Message.delayed_error ~kind:AssertFailure () ~pos
              "Assertion failed:@ %a"
              (Print.UserFacing.expr lang)
              partially_evaluated_assertion_failure_expr);
        Lwt.return (Mark.add m (ELit LUnit), env)
    | _ ->
      Message.error ~pos:(Expr.pos e') "%a" Format.pp_print_text
        "Expected a boolean literal for the result of this assertion (should \
         not happen if the term was well-typed)")
  | EFatalError err -> raise (Runtime.Error (err, [Expr.pos_to_runtime pos]))
  | EErrorOnEmpty e' -> (
    let* r = evaluate_expr_with_env ~on_expr env ctx lang e' in
    match r with
    | (EEmpty, _), _ ->
      raise Runtime.(Error (NoValue, [Expr.pos_to_runtime pos]))
    | exception Runtime.Empty ->
      raise Runtime.(Error (NoValue, [Expr.pos_to_runtime pos]))
    | e, _env -> Lwt.return (e, env))
  | EDefault { excepts; just; cons } -> (
    let* l =
      Lwt_list.map_s (evaluate_expr_with_env ~on_expr env ctx lang) excepts
    in
    let excepts = List.map fst l in
    let empty_count = List.length (List.filter is_empty_error excepts) in
    match List.length excepts - empty_count with
    | 0 -> (
      let* just, _env = evaluate_expr_with_env ~on_expr env ctx lang just in
      match just with
      | ELit (LBool true), _ ->
        evaluate_expr_with_env ~on_expr env ctx lang cons
      | ELit (LBool false), _ -> Lwt.return (Mark.copy e EEmpty, env)
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 ->
      Lwt.return (List.find (fun sub -> not (is_empty_error sub)) excepts, env)
    | _ ->
      let poslist =
        List.filter_map
          (fun ex ->
            if is_empty_error ex then None
            else Some Expr.(pos_to_runtime (pos ex)))
          excepts
      in
      raise Runtime.(Error (Conflict, poslist)))
  | EPureDefault e ->
    let* e, _env = evaluate_expr_with_env ~on_expr env ctx lang e in
    Lwt.return (e, env)
  | _ -> .

let interpret_with_env
    ?(on_expr = on_expr_void)
    (p : (yes Shared_ast__Definitions.dcalc_lcalc, typed) gexpr program)
    scope =
  Message.with_delayed_errors (fun () ->
      let e = Expr.unbox (Program.to_expr p scope) |> Interpreter.addcustom in
      let ctx = p.decl_ctx in
      let scope_info = ScopeName.Map.find scope ctx.ctx_scopes in
      let scope_input_struct = scope_info.in_struct_name in
      let ty = TStruct scope_input_struct, Pos.void in
      let app_term =
        Scope.empty_input_struct_dcalc ctx scope_input_struct
          (Typed { pos = Pos.void; ty })
      in
      let to_interpret =
        Expr.make_app (Expr.box e) [app_term]
          [TStruct scope_input_struct, Expr.pos e]
          (Expr.pos e)
      in
      let* r, _ =
        evaluate_expr_with_env empty_env ~on_expr ctx p.lang
          (Expr.unbox to_interpret)
      in
      match r with
      | EStruct { fields; _ }, _ ->
        Lwt.return
          (List.map
             (fun (fld, e) -> StructField.get_info fld, e)
             (StructField.Map.bindings fields))
      | exception Runtime.Error (err, rpos) ->
        Message.error
          ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) rpos)
          "%a" Format.pp_print_text
          (Runtime.error_message err)
      | _ ->
        Message.error ~pos:(Expr.pos e) ~internal:true "%a" Format.pp_print_text
          "The interpretation of the program doesn't yield a struct \
           corresponding to the scope variables")
