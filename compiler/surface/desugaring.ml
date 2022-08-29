(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr> Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module SurfacePrint = Print
open Shared_ast
module Runtime = Runtime_ocaml.Runtime

(** Translation from {!module: Surface.Ast} to {!module: Desugaring.Ast}.

    - Removes syntactic sugars
    - Separate code from legislation *)

(** {1 Translating expressions} *)

let translate_op_kind (k : Ast.op_kind) : op_kind =
  match k with
  | KInt -> KInt
  | KDec -> KRat
  | KMoney -> KMoney
  | KDate -> KDate
  | KDuration -> KDuration

let translate_binop (op : Ast.binop) : binop =
  match op with
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Add l -> Add (translate_op_kind l)
  | Sub l -> Sub (translate_op_kind l)
  | Mult l -> Mult (translate_op_kind l)
  | Div l -> Div (translate_op_kind l)
  | Lt l -> Lt (translate_op_kind l)
  | Lte l -> Lte (translate_op_kind l)
  | Gt l -> Gt (translate_op_kind l)
  | Gte l -> Gte (translate_op_kind l)
  | Eq -> Eq
  | Neq -> Neq
  | Concat -> Concat

let translate_unop (op : Ast.unop) : unop =
  match op with Not -> Not | Minus l -> Minus (translate_op_kind l)

(** The two modules below help performing operations on map with the {!type:
    Bindlib.box}. Indeed, Catala uses the {{:https://lepigre.fr/ocaml-bindlib/}
    Bindlib} library to represent bound variables in the AST. In this
    translation, bound variables are used to represent function parameters or
    pattern macthing bindings. *)

module LiftStructFieldMap = Bindlib.Lift (StructFieldMap)
module LiftEnumConstructorMap = Bindlib.Lift (EnumConstructorMap)

let disambiguate_constructor
    (ctxt : Name_resolution.context)
    (constructor : (string Marked.pos option * string Marked.pos) list)
    (pos : Pos.t) : EnumName.t * EnumConstructor.t =
  let enum, constructor =
    match constructor with
    | [c] -> c
    | _ ->
      Errors.raise_spanned_error pos
        "The deep pattern matching syntactic sugar is not yet supported"
  in
  let possible_c_uids =
    try
      Desugared.Ast.IdentMap.find
        (Marked.unmark constructor)
        ctxt.constructor_idmap
    with Not_found ->
      Errors.raise_spanned_error
        (Marked.get_mark constructor)
        "The name of this constructor has not been defined before, maybe it is \
         a typo?"
  in
  match enum with
  | None ->
    if EnumMap.cardinal possible_c_uids > 1 then
      Errors.raise_spanned_error
        (Marked.get_mark constructor)
        "This constructor name is ambiguous, it can belong to %a. Disambiguate \
         it by prefixing it with the enum name."
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
           (fun fmt (s_name, _) ->
             Format.fprintf fmt "%a" EnumName.format_t s_name))
        (EnumMap.bindings possible_c_uids);
    EnumMap.choose possible_c_uids
  | Some enum -> (
    try
      (* The path is fully qualified *)
      let e_uid =
        Desugared.Ast.IdentMap.find (Marked.unmark enum) ctxt.enum_idmap
      in
      try
        let c_uid = EnumMap.find e_uid possible_c_uids in
        e_uid, c_uid
      with Not_found ->
        Errors.raise_spanned_error pos "Enum %s does not contain case %s"
          (Marked.unmark enum)
          (Marked.unmark constructor)
    with Not_found ->
      Errors.raise_spanned_error (Marked.get_mark enum)
        "Enum %s has not been defined before" (Marked.unmark enum))

(** Usage: [translate_expr scope ctxt expr]

    Translates [expr] into its desugared equivalent. [scope] is used to
    disambiguate the scope and subscopes variables than occur in the expression *)
let rec translate_expr
    (scope : ScopeName.t)
    (inside_definition_of : Desugared.Ast.ScopeDef.t Marked.pos option)
    (ctxt : Name_resolution.context)
    ((expr, pos) : Ast.expression Marked.pos) : Desugared.Ast.expr Bindlib.box =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  let rec_helper = translate_expr scope inside_definition_of ctxt in
  match expr with
  | Binop
      ( (Ast.And, _pos_op),
        ( TestMatchCase (e1_sub, ((constructors, Some binding), pos_pattern)),
          _pos_e1 ),
        e2 ) ->
    (* This sugar corresponds to [e is P x && e'] and should desugar to [match e
       with P x -> e' | _ -> false] *)
    let enum_uid, c_uid =
      disambiguate_constructor ctxt constructors pos_pattern
    in
    let cases =
      EnumConstructorMap.mapi
        (fun c_uid' tau ->
          if EnumConstructor.compare c_uid c_uid' <> 0 then
            let nop_var = Var.make "_" in
            Bindlib.unbox
              (Expr.make_abs [| nop_var |]
                 (Bindlib.box (ELit (LBool false), pos))
                 [tau] pos)
          else
            let ctxt, binding_var =
              Name_resolution.add_def_local_var ctxt (Marked.unmark binding)
            in
            let e2 = translate_expr scope inside_definition_of ctxt e2 in
            Bindlib.unbox (Expr.make_abs [| binding_var |] e2 [tau] pos))
        (EnumMap.find enum_uid ctxt.enums)
    in
    Bindlib.box_apply
      (fun e1_sub -> EMatchS (e1_sub, enum_uid, cases), pos)
      (translate_expr scope inside_definition_of ctxt e1_sub)
  | IfThenElse (e_if, e_then, e_else) ->
    Bindlib.box_apply3
      (fun e_if e_then e_else -> EIfThenElse (e_if, e_then, e_else), pos)
      (rec_helper e_if) (rec_helper e_then) (rec_helper e_else)
  | Binop (op, e1, e2) ->
    let op_term =
      Marked.same_mark_as (EOp (Binop (translate_binop (Marked.unmark op)))) op
    in
    Bindlib.box_apply2
      (fun e1 e2 -> EApp (op_term, [e1; e2]), pos)
      (rec_helper e1) (rec_helper e2)
  | Unop (op, e) ->
    let op_term =
      Marked.same_mark_as (EOp (Unop (translate_unop (Marked.unmark op)))) op
    in
    Bindlib.box_apply (fun e -> EApp (op_term, [e]), pos) (rec_helper e)
  | Literal l ->
    let untyped_term =
      match l with
      | LNumber ((Int i, _), None) -> ELit (LInt (Runtime.integer_of_string i))
      | LNumber ((Int i, _), Some (Percent, _)) ->
        ELit (LRat Runtime.(decimal_of_string i /& decimal_of_string "100"))
      | LNumber ((Dec (i, f), _), None) ->
        ELit (LRat Runtime.(decimal_of_string (i ^ "." ^ f)))
      | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
        ELit
          (LRat
             Runtime.(
               decimal_of_string (i ^ "." ^ f) /& decimal_of_string "100"))
      | LBool b -> ELit (LBool b)
      | LMoneyAmount i ->
        ELit
          (LMoney
             Runtime.(
               money_of_cents_integer
                 ((integer_of_string i.money_amount_units *! integer_of_int 100)
                 +! integer_of_string i.money_amount_cents)))
      | LNumber ((Int i, _), Some (Year, _)) ->
        ELit (LDuration (Runtime.duration_of_numbers (int_of_string i) 0 0))
      | LNumber ((Int i, _), Some (Month, _)) ->
        ELit (LDuration (Runtime.duration_of_numbers 0 (int_of_string i) 0))
      | LNumber ((Int i, _), Some (Day, _)) ->
        ELit (LDuration (Runtime.duration_of_numbers 0 0 (int_of_string i)))
      | LNumber ((Dec (_, _), _), Some ((Year | Month | Day), _)) ->
        Errors.raise_spanned_error pos
          "Impossible to specify decimal amounts of days, months or years"
      | LDate date ->
        if date.literal_date_month > 12 then
          Errors.raise_spanned_error pos
            "There is an error in this date: the month number is bigger than 12";
        if date.literal_date_day > 31 then
          Errors.raise_spanned_error pos
            "There is an error in this date: the day number is bigger than 31";
        ELit
          (LDate
             (try
                Runtime.date_of_numbers date.literal_date_year
                  date.literal_date_month date.literal_date_day
              with Runtime.ImpossibleDate ->
                Errors.raise_spanned_error pos
                  "There is an error in this date, it does not correspond to a \
                   correct calendar day"))
    in
    Bindlib.box (untyped_term, pos)
  | Ident x -> (
    (* first we check whether this is a local var, then we resort to scope-wide
       variables *)
    match Desugared.Ast.IdentMap.find_opt x ctxt.local_var_idmap with
    | None -> (
      match Desugared.Ast.IdentMap.find_opt x scope_ctxt.var_idmap with
      | Some uid ->
        (* If the referenced variable has states, then here are the rules to
           desambiguate. In general, only the last state can be referenced.
           Except if defining a state of the same variable, then it references
           the previous state in the chain. *)
        let x_sig = ScopeVarMap.find uid ctxt.var_typs in
        let x_state =
          match x_sig.var_sig_states_list with
          | [] -> None
          | states -> (
            match inside_definition_of with
            | Some (Var (x'_uid, sx'), _) when ScopeVar.compare uid x'_uid = 0
              -> (
              match sx' with
              | None ->
                failwith
                  "inconsistent state: inside a definition of a variable with \
                   no state but variable has states"
              | Some inside_def_state ->
                if StateName.compare inside_def_state (List.hd states) = 0 then
                  Errors.raise_spanned_error pos
                    "It is impossible to refer to the variable you are \
                     defining when defining its first state."
                else
                  (* Tricky: we have to retrieve in the list the previous state
                     with respect to the state that we are defining. *)
                  let correct_state = ref None in
                  ignore
                    (List.fold_left
                       (fun previous_state state ->
                         if StateName.equal inside_def_state state then
                           correct_state := previous_state;
                         Some state)
                       None states);
                  !correct_state)
            | _ ->
              (* we take the last state in the chain *)
              Some (List.hd (List.rev states)))
        in
        Bindlib.box (ELocation (DesugaredScopeVar ((uid, pos), x_state)), pos)
      | None ->
        Name_resolution.raise_unknown_identifier
          "for a local or scope-wide variable" (x, pos))
    | Some uid ->
      Expr.make_var (uid, pos)
      (* the whole box thing is to accomodate for this case *))
  | Dotted (e, c, x) -> (
    match Marked.unmark e with
    | Ident y when Name_resolution.is_subscope_uid scope ctxt y ->
      (* In this case, y.x is a subscope variable *)
      let subscope_uid : SubScopeName.t =
        Name_resolution.get_subscope_uid scope ctxt (Marked.same_mark_as y e)
      in
      let subscope_real_uid : ScopeName.t =
        Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
      in
      let subscope_var_uid =
        Name_resolution.get_var_uid subscope_real_uid ctxt x
      in
      Bindlib.box
        ( ELocation
            (SubScopeVar
               (subscope_real_uid, (subscope_uid, pos), (subscope_var_uid, pos))),
          pos )
    | _ -> (
      (* In this case e.x is the struct field x access of expression e *)
      let e = translate_expr scope inside_definition_of ctxt e in
      let x_possible_structs =
        try Desugared.Ast.IdentMap.find (Marked.unmark x) ctxt.field_idmap
        with Not_found ->
          Errors.raise_spanned_error (Marked.get_mark x)
            "Unknown subscope or struct field name"
      in
      match c with
      | None ->
        (* No constructor name was specified *)
        if StructMap.cardinal x_possible_structs > 1 then
          Errors.raise_spanned_error (Marked.get_mark x)
            "This struct field name is ambiguous, it can belong to %a. \
             Disambiguate it by prefixing it with the struct name."
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
               (fun fmt (s_name, _) ->
                 Format.fprintf fmt "%a" StructName.format_t s_name))
            (StructMap.bindings x_possible_structs)
        else
          let s_uid, f_uid = StructMap.choose x_possible_structs in
          Bindlib.box_apply (fun e -> EStructAccess (e, f_uid, s_uid), pos) e
      | Some c_name -> (
        try
          let c_uid =
            Desugared.Ast.IdentMap.find (Marked.unmark c_name) ctxt.struct_idmap
          in
          try
            let f_uid = StructMap.find c_uid x_possible_structs in
            Bindlib.box_apply (fun e -> EStructAccess (e, f_uid, c_uid), pos) e
          with Not_found ->
            Errors.raise_spanned_error pos "Struct %s does not contain field %s"
              (Marked.unmark c_name) (Marked.unmark x)
        with Not_found ->
          Errors.raise_spanned_error (Marked.get_mark c_name)
            "Struct %s has not been defined before" (Marked.unmark c_name))))
  | FunCall (f, arg) ->
    Bindlib.box_apply2
      (fun f arg -> EApp (f, [arg]), pos)
      (rec_helper f) (rec_helper arg)
  | LetIn (x, e1, e2) ->
    let ctxt, v = Name_resolution.add_def_local_var ctxt (Marked.unmark x) in
    let tau = TAny, Marked.get_mark x in
    let fn =
      Expr.make_abs [| v |]
        (translate_expr scope inside_definition_of ctxt e2)
        [tau] pos
    in
    Bindlib.box_apply2 (fun fn arg -> EApp (fn, [arg]), pos) fn (rec_helper e1)
  | StructLit (s_name, fields) ->
    let s_uid =
      try Desugared.Ast.IdentMap.find (Marked.unmark s_name) ctxt.struct_idmap
      with Not_found ->
        Errors.raise_spanned_error (Marked.get_mark s_name)
          "This identifier should refer to a struct name"
    in

    let s_fields =
      List.fold_left
        (fun s_fields (f_name, f_e) ->
          let f_uid =
            try
              StructMap.find s_uid
                (Desugared.Ast.IdentMap.find (Marked.unmark f_name)
                   ctxt.field_idmap)
            with Not_found ->
              Errors.raise_spanned_error (Marked.get_mark f_name)
                "This identifier should refer to a field of struct %s"
                (Marked.unmark s_name)
          in
          (match StructFieldMap.find_opt f_uid s_fields with
          | None -> ()
          | Some e_field ->
            Errors.raise_multispanned_error
              [
                None, Marked.get_mark f_e;
                None, Marked.get_mark (Bindlib.unbox e_field);
              ]
              "The field %a has been defined twice:" StructFieldName.format_t
              f_uid);
          let f_e = translate_expr scope inside_definition_of ctxt f_e in
          StructFieldMap.add f_uid f_e s_fields)
        StructFieldMap.empty fields
    in
    let expected_s_fields = StructMap.find s_uid ctxt.structs in
    StructFieldMap.iter
      (fun expected_f _ ->
        if not (StructFieldMap.mem expected_f s_fields) then
          Errors.raise_spanned_error pos
            "Missing field for structure %a: \"%a\"" StructName.format_t s_uid
            StructFieldName.format_t expected_f)
      expected_s_fields;

    Bindlib.box_apply
      (fun s_fields -> EStruct (s_uid, s_fields), pos)
      (LiftStructFieldMap.lift_box s_fields)
  | EnumInject (enum, constructor, payload) -> (
    let possible_c_uids =
      try
        Desugared.Ast.IdentMap.find
          (Marked.unmark constructor)
          ctxt.constructor_idmap
      with Not_found ->
        Errors.raise_spanned_error
          (Marked.get_mark constructor)
          "The name of this constructor has not been defined before, maybe it \
           is a typo?"
    in

    match enum with
    | None ->
      if
        (* No constructor name was specified *)
        EnumMap.cardinal possible_c_uids > 1
      then
        Errors.raise_spanned_error
          (Marked.get_mark constructor)
          "This constructor name is ambiguous, it can belong to %a. \
           Desambiguate it by prefixing it with the enum name."
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
             (fun fmt (s_name, _) ->
               Format.fprintf fmt "%a" EnumName.format_t s_name))
          (EnumMap.bindings possible_c_uids)
      else
        let e_uid, c_uid = EnumMap.choose possible_c_uids in
        let payload =
          Option.map (translate_expr scope inside_definition_of ctxt) payload
        in
        Bindlib.box_apply
          (fun payload ->
            ( EEnumInj
                ( (match payload with
                  | Some e' -> e'
                  | None -> ELit LUnit, Marked.get_mark constructor),
                  c_uid,
                  e_uid ),
              pos ))
          (Bindlib.box_opt payload)
    | Some enum -> (
      try
        (* The path has been fully qualified *)
        let e_uid =
          Desugared.Ast.IdentMap.find (Marked.unmark enum) ctxt.enum_idmap
        in
        try
          let c_uid = EnumMap.find e_uid possible_c_uids in
          let payload =
            Option.map (translate_expr scope inside_definition_of ctxt) payload
          in
          Bindlib.box_apply
            (fun payload ->
              ( EEnumInj
                  ( (match payload with
                    | Some e' -> e'
                    | None -> ELit LUnit, Marked.get_mark constructor),
                    c_uid,
                    e_uid ),
                pos ))
            (Bindlib.box_opt payload)
        with Not_found ->
          Errors.raise_spanned_error pos "Enum %s does not contain case %s"
            (Marked.unmark enum)
            (Marked.unmark constructor)
      with Not_found ->
        Errors.raise_spanned_error (Marked.get_mark enum)
          "Enum %s has not been defined before" (Marked.unmark enum)))
  | MatchWith (e1, (cases, _cases_pos)) ->
    let e1 = translate_expr scope inside_definition_of ctxt e1 in
    let cases_d, e_uid =
      disambiguate_match_and_build_expression scope inside_definition_of ctxt
        cases
    in
    Bindlib.box_apply2
      (fun e1 cases_d -> EMatchS (e1, e_uid, cases_d), pos)
      e1
      (LiftEnumConstructorMap.lift_box cases_d)
  | TestMatchCase (e1, pattern) ->
    (match snd (Marked.unmark pattern) with
    | None -> ()
    | Some binding ->
      Errors.format_spanned_warning (Marked.get_mark binding)
        "This binding will be ignored (remove it to suppress warning)");
    let enum_uid, c_uid =
      disambiguate_constructor ctxt
        (fst (Marked.unmark pattern))
        (Marked.get_mark pattern)
    in
    let cases =
      EnumConstructorMap.mapi
        (fun c_uid' tau ->
          let nop_var = Var.make "_" in
          Bindlib.unbox
            (Expr.make_abs [| nop_var |]
               (Bindlib.box
                  (ELit (LBool (EnumConstructor.compare c_uid c_uid' = 0)), pos))
               [tau] pos))
        (EnumMap.find enum_uid ctxt.enums)
    in
    Bindlib.box_apply
      (fun e -> EMatchS (e, enum_uid, cases), pos)
      (translate_expr scope inside_definition_of ctxt e1)
  | ArrayLit es ->
    Bindlib.box_apply
      (fun es -> EArray es, pos)
      (Bindlib.box_list (List.map rec_helper es))
  | CollectionOp
      ( (((Ast.Filter | Ast.Map) as op'), _pos_op'),
        param',
        collection,
        predicate ) ->
    let collection = rec_helper collection in
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param')
    in
    let f_pred =
      Expr.make_abs [| param |]
        (translate_expr scope inside_definition_of ctxt predicate)
        [TAny, pos]
        pos
    in
    Bindlib.box_apply2
      (fun f_pred collection ->
        ( EApp
            ( ( EOp
                  (match op' with
                  | Ast.Map -> Binop Map
                  | Ast.Filter -> Binop Filter
                  | _ -> assert false (* should not happen *)),
                pos ),
              [f_pred; collection] ),
          pos ))
      f_pred collection
  | CollectionOp
      ( ( Ast.Aggregate (Ast.AggregateArgExtremum (max_or_min, pred_typ, init)),
          pos_op' ),
        param',
        collection,
        predicate ) ->
    let init = rec_helper init in
    let collection = rec_helper collection in
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param')
    in
    let op_kind =
      match pred_typ with
      | Ast.Integer -> KInt
      | Ast.Decimal -> KRat
      | Ast.Money -> KMoney
      | Ast.Duration -> KDuration
      | Ast.Date -> KDate
      | _ ->
        Errors.raise_spanned_error pos
          "It is impossible to compute the arg-%s of two values of type %a"
          (if max_or_min then "max" else "min")
          SurfacePrint.format_primitive_typ pred_typ
    in
    let cmp_op = if max_or_min then Gt op_kind else Lt op_kind in
    let f_pred =
      Expr.make_abs [| param |]
        (translate_expr scope inside_definition_of ctxt predicate)
        [TAny, pos]
        pos
    in
    let f_pred_var = Var.make "predicate" in
    let f_pred_var_e = Expr.make_var (f_pred_var, Marked.get_mark predicate) in
    let acc_var = Var.make "acc" in
    let acc_var_e = Expr.make_var (acc_var, pos) in
    let item_var = Var.make "item" in
    let item_var_e =
      Expr.make_var (item_var, Marked.get_mark (Bindlib.unbox collection))
    in
    let fold_body =
      Bindlib.box_apply3
        (fun acc_var_e item_var_e f_pred_var_e ->
          ( EIfThenElse
              ( ( EApp
                    ( (EOp (Binop cmp_op), pos_op'),
                      [
                        EApp (f_pred_var_e, [acc_var_e]), pos;
                        EApp (f_pred_var_e, [item_var_e]), pos;
                      ] ),
                  pos ),
                acc_var_e,
                item_var_e ),
            pos ))
        acc_var_e item_var_e f_pred_var_e
    in
    let fold_f =
      Expr.make_abs [| acc_var; item_var |] fold_body [TAny, pos; TAny, pos] pos
    in
    let fold =
      Bindlib.box_apply3
        (fun fold_f collection init ->
          EApp ((EOp (Ternop Fold), pos), [fold_f; init; collection]), pos)
        fold_f collection init
    in
    Expr.make_let_in_raw f_pred_var (TAny, pos) f_pred fold pos
  | CollectionOp (op', param', collection, predicate) ->
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param')
    in
    let collection = rec_helper collection in
    let init =
      match Marked.unmark op' with
      | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
        assert false (* should not happen *)
      | Ast.Exists -> Bindlib.box (ELit (LBool false), Marked.get_mark op')
      | Ast.Forall -> Bindlib.box (ELit (LBool true), Marked.get_mark op')
      | Ast.Aggregate (Ast.AggregateSum Ast.Integer) ->
        Bindlib.box (ELit (LInt (Runtime.integer_of_int 0)), Marked.get_mark op')
      | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) ->
        Bindlib.box
          (ELit (LRat (Runtime.decimal_of_string "0")), Marked.get_mark op')
      | Ast.Aggregate (Ast.AggregateSum Ast.Money) ->
        Bindlib.box
          ( ELit
              (LMoney
                 (Runtime.money_of_cents_integer (Runtime.integer_of_int 0))),
            Marked.get_mark op' )
      | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
        Bindlib.box
          ( ELit (LDuration (Runtime.duration_of_numbers 0 0 0)),
            Marked.get_mark op' )
      | Ast.Aggregate (Ast.AggregateSum t) ->
        Errors.raise_spanned_error pos
          "It is impossible to sum two values of type %a together"
          SurfacePrint.format_primitive_typ t
      | Ast.Aggregate (Ast.AggregateExtremum (_, _, init)) -> rec_helper init
      | Ast.Aggregate Ast.AggregateCount ->
        Bindlib.box (ELit (LInt (Runtime.integer_of_int 0)), Marked.get_mark op')
    in
    let acc_var = Var.make "acc" in
    let acc = Expr.make_var (acc_var, Marked.get_mark param') in
    let f_body =
      let make_body (op : binop) =
        Bindlib.box_apply2
          (fun predicate acc ->
            EApp ((EOp (Binop op), Marked.get_mark op'), [acc; predicate]), pos)
          (translate_expr scope inside_definition_of ctxt predicate)
          acc
      in
      let make_extr_body (cmp_op : binop) (t : typ) =
        let tmp_var = Var.make "tmp" in
        let tmp = Expr.make_var (tmp_var, Marked.get_mark param') in
        Expr.make_let_in_raw tmp_var t
          (translate_expr scope inside_definition_of ctxt predicate)
          (Bindlib.box_apply2
             (fun acc tmp ->
               ( EIfThenElse
                   ( ( EApp
                         ((EOp (Binop cmp_op), Marked.get_mark op'), [acc; tmp]),
                       pos ),
                     acc,
                     tmp ),
                 pos ))
             acc tmp)
          pos
      in
      match Marked.unmark op' with
      | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
        assert false (* should not happen *)
      | Ast.Exists -> make_body Or
      | Ast.Forall -> make_body And
      | Ast.Aggregate (Ast.AggregateSum Ast.Integer) -> make_body (Add KInt)
      | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) -> make_body (Add KRat)
      | Ast.Aggregate (Ast.AggregateSum Ast.Money) -> make_body (Add KMoney)
      | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
        make_body (Add KDuration)
      | Ast.Aggregate (Ast.AggregateSum _) ->
        assert false (* should not happen *)
      | Ast.Aggregate (Ast.AggregateExtremum (max_or_min, t, _)) ->
        let op_kind, typ =
          match t with
          | Ast.Integer -> KInt, (TLit TInt, pos)
          | Ast.Decimal -> KRat, (TLit TRat, pos)
          | Ast.Money -> KMoney, (TLit TMoney, pos)
          | Ast.Duration -> KDuration, (TLit TDuration, pos)
          | Ast.Date -> KDate, (TLit TDate, pos)
          | _ ->
            Errors.raise_spanned_error pos
              "ssible to compute the %s of two values of type %a"
              (if max_or_min then "max" else "min")
              SurfacePrint.format_primitive_typ t
        in
        let cmp_op = if max_or_min then Gt op_kind else Lt op_kind in
        make_extr_body cmp_op typ
      | Ast.Aggregate Ast.AggregateCount ->
        Bindlib.box_apply2
          (fun predicate acc ->
            ( EIfThenElse
                ( predicate,
                  ( EApp
                      ( (EOp (Binop (Add KInt)), Marked.get_mark op'),
                        [
                          acc;
                          ( ELit (LInt (Runtime.integer_of_int 1)),
                            Marked.get_mark predicate );
                        ] ),
                    pos ),
                  acc ),
              pos ))
          (translate_expr scope inside_definition_of ctxt predicate)
          acc
    in
    let f =
      let make_f (t : typ_lit) =
        Bindlib.box_apply
          (fun binder ->
            ( EAbs
                ( binder,
                  [
                    TLit t, Marked.get_mark op';
                    TAny, pos
                    (* we put any here because the type of the elements of the
                       arrays is not always the type of the accumulator; for
                       instance in AggregateCount. *);
                  ] ),
              pos ))
          (Bindlib.bind_mvar [| acc_var; param |] f_body)
      in
      match Marked.unmark op' with
      | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
        assert false (* should not happen *)
      | Ast.Exists -> make_f TBool
      | Ast.Forall -> make_f TBool
      | Ast.Aggregate (Ast.AggregateSum Ast.Integer)
      | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Integer, _)) ->
        make_f TInt
      | Ast.Aggregate (Ast.AggregateSum Ast.Decimal)
      | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Decimal, _)) ->
        make_f TRat
      | Ast.Aggregate (Ast.AggregateSum Ast.Money)
      | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Money, _)) ->
        make_f TMoney
      | Ast.Aggregate (Ast.AggregateSum Ast.Duration)
      | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Duration, _)) ->
        make_f TDuration
      | Ast.Aggregate (Ast.AggregateSum _)
      | Ast.Aggregate (Ast.AggregateExtremum _) ->
        assert false (* should not happen *)
      | Ast.Aggregate Ast.AggregateCount -> make_f TInt
    in
    Bindlib.box_apply3
      (fun f collection init ->
        EApp ((EOp (Ternop Fold), pos), [f; init; collection]), pos)
      f collection init
  | MemCollection (member, collection) ->
    let param_var = Var.make "collection_member" in
    let param = Expr.make_var (param_var, pos) in
    let collection = rec_helper collection in
    let init = Bindlib.box (ELit (LBool false), pos) in
    let acc_var = Var.make "acc" in
    let acc = Expr.make_var (acc_var, pos) in
    let f_body =
      Bindlib.box_apply3
        (fun member acc param ->
          ( EApp
              ( (EOp (Binop Or), pos),
                [EApp ((EOp (Binop Eq), pos), [member; param]), pos; acc] ),
            pos ))
        (translate_expr scope inside_definition_of ctxt member)
        acc param
    in
    let f =
      Bindlib.box_apply
        (fun binder -> EAbs (binder, [TLit TBool, pos; TAny, pos]), pos)
        (Bindlib.bind_mvar [| acc_var; param_var |] f_body)
    in
    Bindlib.box_apply3
      (fun f collection init ->
        EApp ((EOp (Ternop Fold), pos), [f; init; collection]), pos)
      f collection init
  | Builtin IntToDec -> Bindlib.box (EOp (Unop IntToRat), pos)
  | Builtin MoneyToDec -> Bindlib.box (EOp (Unop MoneyToRat), pos)
  | Builtin DecToMoney -> Bindlib.box (EOp (Unop RatToMoney), pos)
  | Builtin Cardinal -> Bindlib.box (EOp (Unop Length), pos)
  | Builtin GetDay -> Bindlib.box (EOp (Unop GetDay), pos)
  | Builtin GetMonth -> Bindlib.box (EOp (Unop GetMonth), pos)
  | Builtin GetYear -> Bindlib.box (EOp (Unop GetYear), pos)
  | Builtin FirstDayOfMonth -> Bindlib.box (EOp (Unop FirstDayOfMonth), pos)
  | Builtin LastDayOfMonth -> Bindlib.box (EOp (Unop LastDayOfMonth), pos)
  | Builtin RoundMoney -> Bindlib.box (EOp (Unop RoundMoney), pos)
  | Builtin RoundDecimal -> Bindlib.box (EOp (Unop RoundDecimal), pos)

and disambiguate_match_and_build_expression
    (scope : ScopeName.t)
    (inside_definition_of : Desugared.Ast.ScopeDef.t Marked.pos option)
    (ctxt : Name_resolution.context)
    (cases : Ast.match_case Marked.pos list) :
    Desugared.Ast.expr Bindlib.box EnumConstructorMap.t * EnumName.t =
  let create_var = function
    | None -> ctxt, Var.make "_"
    | Some param ->
      let ctxt, param_var = Name_resolution.add_def_local_var ctxt param in
      ctxt, param_var
  in
  let bind_case_body
      (c_uid : EnumConstructor.t)
      (e_uid : EnumName.t)
      (ctxt : Name_resolution.context)
      (case_body : ('a * Pos.t) Bindlib.box)
      (e_binder : (Desugared.Ast.expr, Desugared.Ast.expr) mbinder Bindlib.box)
      : 'c Bindlib.box =
    Bindlib.box_apply2
      (fun e_binder case_body ->
        Marked.same_mark_as
          (EAbs
             ( e_binder,
               [
                 EnumConstructorMap.find c_uid
                   (EnumMap.find e_uid ctxt.Name_resolution.enums);
               ] ))
          case_body)
      e_binder case_body
  in
  let bind_match_cases (cases_d, e_uid, curr_index) (case, case_pos) =
    match case with
    | Ast.MatchCase case ->
      let constructor, binding = Marked.unmark case.Ast.match_case_pattern in
      let e_uid', c_uid =
        disambiguate_constructor ctxt constructor
          (Marked.get_mark case.Ast.match_case_pattern)
      in
      let e_uid =
        match e_uid with
        | None -> e_uid'
        | Some e_uid ->
          if e_uid = e_uid' then e_uid
          else
            Errors.raise_spanned_error
              (Marked.get_mark case.Ast.match_case_pattern)
              "This case matches a constructor of enumeration %a but previous \
               case were matching constructors of enumeration %a"
              EnumName.format_t e_uid EnumName.format_t e_uid'
      in
      (match EnumConstructorMap.find_opt c_uid cases_d with
      | None -> ()
      | Some e_case ->
        Errors.raise_multispanned_error
          [
            None, Marked.get_mark case.match_case_expr;
            None, Marked.get_mark (Bindlib.unbox e_case);
          ]
          "The constructor %a has been matched twice:" EnumConstructor.format_t
          c_uid);
      let ctxt, param_var = create_var (Option.map Marked.unmark binding) in
      let case_body =
        translate_expr scope inside_definition_of ctxt case.Ast.match_case_expr
      in
      let e_binder = Bindlib.bind_mvar [| param_var |] case_body in
      let case_expr = bind_case_body c_uid e_uid ctxt case_body e_binder in
      EnumConstructorMap.add c_uid case_expr cases_d, Some e_uid, curr_index + 1
    | Ast.WildCard match_case_expr -> (
      let nb_cases = List.length cases in
      let raise_wildcard_not_last_case_err () =
        Errors.raise_multispanned_error
          [
            Some "Not ending wildcard:", case_pos;
            ( Some "Next reachable case:",
              curr_index + 1 |> List.nth cases |> Marked.get_mark );
          ]
          "Wildcard must be the last match case"
      in
      match e_uid with
      | None ->
        if 1 = nb_cases then
          Errors.raise_spanned_error case_pos
            "Couldn't infer the enumeration name from lonely wildcard \
             (wildcard cannot be used as single match case)"
        else raise_wildcard_not_last_case_err ()
      | Some e_uid ->
        if curr_index < nb_cases - 1 then raise_wildcard_not_last_case_err ();
        let missing_constructors =
          EnumMap.find e_uid ctxt.Name_resolution.enums
          |> EnumConstructorMap.filter_map (fun c_uid _ ->
                 match EnumConstructorMap.find_opt c_uid cases_d with
                 | Some _ -> None
                 | None -> Some c_uid)
        in
        if EnumConstructorMap.is_empty missing_constructors then
          Errors.format_spanned_warning case_pos
            "Unreachable match case, all constructors of the enumeration %a \
             are already specified"
            EnumName.format_t e_uid;
        (* The current used strategy is to replace the wildcard branch:
               match foo with
               | Case1 x -> x
               | _ -> 1
           with:
               let wildcard_payload = 1 in
               match foo with
               | Case1 x -> x
               | Case2 -> wildcard_payload
                ...
               | CaseN -> wildcard_payload *)
        (* Creates the wildcard payload *)
        let ctxt, payload_var = create_var None in
        let case_body =
          translate_expr scope inside_definition_of ctxt match_case_expr
        in
        let e_binder = Bindlib.bind_mvar [| payload_var |] case_body in

        (* For each missing cases, binds the wildcard payload. *)
        EnumConstructorMap.fold
          (fun c_uid _ (cases_d, e_uid_opt, curr_index) ->
            let case_expr =
              bind_case_body c_uid e_uid ctxt case_body e_binder
            in
            ( EnumConstructorMap.add c_uid case_expr cases_d,
              e_uid_opt,
              curr_index + 1 ))
          missing_constructors
          (cases_d, Some e_uid, curr_index))
  in
  let expr, e_name, _ =
    List.fold_left bind_match_cases (EnumConstructorMap.empty, None, 0) cases
  in
  expr, Option.get e_name
  [@@ocamlformat "wrap-comments=false"]

(** {1 Translating scope definitions} *)

(** A scope use can be annotated with a pervasive precondition, in which case
    this precondition has to be appended to the justifications of each
    definition in the subscope use. This is what this function does. *)
let merge_conditions
    (precond : Desugared.Ast.expr Bindlib.box option)
    (cond : Desugared.Ast.expr Bindlib.box option)
    (default_pos : Pos.t) : Desugared.Ast.expr Bindlib.box =
  match precond, cond with
  | Some precond, Some cond ->
    let op_term = EOp (Binop And), Marked.get_mark (Bindlib.unbox cond) in
    Bindlib.box_apply2
      (fun precond cond ->
        EApp (op_term, [precond; cond]), Marked.get_mark cond)
      precond cond
  | Some precond, None ->
    Bindlib.box_apply
      (fun precond -> Marked.unmark precond, default_pos)
      precond
  | None, Some cond -> cond
  | None, None -> Bindlib.box (ELit (LBool true), default_pos)

(** Translates a surface definition into condition into a desugared {!type:
    Desugared.Ast.rule} *)
let process_default
    (ctxt : Name_resolution.context)
    (scope : ScopeName.t)
    (def_key : Desugared.Ast.ScopeDef.t Marked.pos)
    (rule_id : Desugared.Ast.RuleName.t)
    (param_uid : Desugared.Ast.expr Var.t Marked.pos option)
    (precond : Desugared.Ast.expr Bindlib.box option)
    (exception_situation : Desugared.Ast.exception_situation)
    (label_situation : Desugared.Ast.label_situation)
    (just : Ast.expression Marked.pos option)
    (cons : Ast.expression Marked.pos) : Desugared.Ast.rule =
  let just =
    match just with
    | Some just -> Some (translate_expr scope (Some def_key) ctxt just)
    | None -> None
  in
  let just = merge_conditions precond just (Marked.get_mark def_key) in
  let cons = translate_expr scope (Some def_key) ctxt cons in
  {
    rule_just = just;
    rule_cons = cons;
    rule_parameter =
      (let def_key_typ =
         Name_resolution.get_def_typ ctxt (Marked.unmark def_key)
       in
       match Marked.unmark def_key_typ, param_uid with
       | TArrow (t_in, _), Some param_uid -> Some (Marked.unmark param_uid, t_in)
       | TArrow _, None ->
         Errors.raise_spanned_error
           (Marked.get_mark (Bindlib.unbox cons))
           "This definition has a function type but the parameter is missing"
       | _, Some _ ->
         Errors.raise_spanned_error
           (Marked.get_mark (Bindlib.unbox cons))
           "This definition has a parameter but its type is not a function"
       | _ -> None);
    rule_exception = exception_situation;
    rule_id;
    rule_label = label_situation;
  }

(** Wrapper around {!val: process_default} that performs some name
    disambiguation *)
let process_def
    (precond : Desugared.Ast.expr Bindlib.box option)
    (scope_uid : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program)
    (def : Ast.definition) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope =
    Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes
  in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let def_key =
    Name_resolution.get_def_key
      (Marked.unmark def.definition_name)
      def.definition_state scope_uid ctxt
      (Marked.get_mark def.definition_expr)
  in
  let scope_def_ctxt =
    Desugared.Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts
  in
  (* We add to the name resolution context the name of the parameter variable *)
  let param_uid, new_ctxt =
    match def.definition_parameter with
    | None -> None, ctxt
    | Some param ->
      let ctxt, param_var =
        Name_resolution.add_def_local_var ctxt (Marked.unmark param)
      in
      Some (Marked.same_mark_as param_var param), ctxt
  in
  let scope_updated =
    let scope_def = Desugared.Ast.ScopeDefMap.find def_key scope.scope_defs in
    let rule_name = def.definition_id in
    let label_situation =
      match def.definition_label with
      | Some (label_str, label_pos) ->
        Desugared.Ast.ExplicitlyLabeled
          ( Desugared.Ast.IdentMap.find label_str scope_def_ctxt.label_idmap,
            label_pos )
      | None -> Desugared.Ast.Unlabeled
    in
    let exception_situation =
      match def.Ast.definition_exception_to with
      | NotAnException -> Desugared.Ast.BaseCase
      | UnlabeledException -> (
        match scope_def_ctxt.default_exception_rulename with
        | None | Some (Name_resolution.Ambiguous _) ->
          (* This should have been caught previously by
             check_unlabeled_exception *)
          assert false (* should not happen *)
        | Some (Name_resolution.Unique (name, pos)) ->
          ExceptionToRule (name, pos))
      | ExceptionToLabel label_str -> (
        try
          let label_id =
            Desugared.Ast.IdentMap.find (Marked.unmark label_str)
              scope_def_ctxt.label_idmap
          in
          ExceptionToLabel (label_id, Marked.get_mark label_str)
        with Not_found ->
          Errors.raise_spanned_error
            (Marked.get_mark label_str)
            "Unknown label for the scope variable %a: \"%s\""
            Desugared.Ast.ScopeDef.format_t def_key (Marked.unmark label_str))
    in
    let scope_def =
      {
        scope_def with
        scope_def_rules =
          Desugared.Ast.RuleMap.add rule_name
            (process_default new_ctxt scope_uid
               (def_key, Marked.get_mark def.definition_name)
               rule_name param_uid precond exception_situation label_situation
               def.definition_condition def.definition_expr)
            scope_def.scope_def_rules;
      }
    in
    {
      scope with
      scope_defs =
        Desugared.Ast.ScopeDefMap.add def_key scope_def scope.scope_defs;
    }
  in
  {
    prgm with
    program_scopes =
      Scopelang.Ast.ScopeMap.add scope_uid scope_updated prgm.program_scopes;
  }

(** Translates a {!type: Surface.Ast.rule} from the surface language *)
let process_rule
    (precond : Desugared.Ast.expr Bindlib.box option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program)
    (rule : Ast.rule) : Desugared.Ast.program =
  let def = Ast.rule_to_def rule in
  process_def precond scope ctxt prgm def

(** Translates assertions *)
let process_assert
    (precond : Desugared.Ast.expr Bindlib.box option)
    (scope_uid : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program)
    (ass : Ast.assertion) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope =
    Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes
  in
  let ass =
    translate_expr scope_uid None ctxt
      (match ass.Ast.assertion_condition with
      | None -> ass.Ast.assertion_content
      | Some cond ->
        ( Ast.IfThenElse
            ( cond,
              ass.Ast.assertion_content,
              Marked.same_mark_as (Ast.Literal (Ast.LBool true)) cond ),
          Marked.get_mark cond ))
  in
  let ass =
    match precond with
    | Some precond ->
      Bindlib.box_apply2
        (fun precond ass ->
          ( EIfThenElse
              (precond, ass, Marked.same_mark_as (ELit (LBool true)) precond),
            Marked.get_mark precond ))
        precond ass
    | None -> ass
  in
  let new_scope =
    { scope with scope_assertions = ass :: scope.scope_assertions }
  in
  {
    prgm with
    program_scopes =
      Scopelang.Ast.ScopeMap.add scope_uid new_scope prgm.program_scopes;
  }

(** Translates a surface definition, rule or assertion *)
let process_scope_use_item
    (precond : Ast.expression Marked.pos option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program)
    (item : Ast.scope_use_item Marked.pos) : Desugared.Ast.program =
  let precond = Option.map (translate_expr scope None ctxt) precond in
  match Marked.unmark item with
  | Ast.Rule rule -> process_rule precond scope ctxt prgm rule
  | Ast.Definition def -> process_def precond scope ctxt prgm def
  | Ast.Assertion ass -> process_assert precond scope ctxt prgm ass
  | _ -> prgm

(** {1 Translating top-level items} *)

(* If this is an unlabeled exception, ensures that it has a unique default
   definition *)
let check_unlabeled_exception
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (item : Ast.scope_use_item Marked.pos) : unit =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  match Marked.unmark item with
  | Ast.Rule _ | Ast.Definition _ -> (
    let def_key, exception_to =
      match Marked.unmark item with
      | Ast.Rule rule ->
        ( Name_resolution.get_def_key
            (Marked.unmark rule.rule_name)
            rule.rule_state scope ctxt
            (Marked.get_mark rule.rule_name),
          rule.rule_exception_to )
      | Ast.Definition def ->
        ( Name_resolution.get_def_key
            (Marked.unmark def.definition_name)
            def.definition_state scope ctxt
            (Marked.get_mark def.definition_name),
          def.definition_exception_to )
      | _ -> assert false
      (* should not happen *)
    in
    let scope_def_ctxt =
      Desugared.Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts
    in
    match exception_to with
    | Ast.NotAnException | Ast.ExceptionToLabel _ -> ()
    (* If this is an unlabeled exception, we check that it has a unique default
       definition *)
    | Ast.UnlabeledException -> (
      match scope_def_ctxt.default_exception_rulename with
      | None ->
        Errors.raise_spanned_error (Marked.get_mark item)
          "This exception does not have a corresponding definition"
      | Some (Ambiguous pos) ->
        Errors.raise_multispanned_error
          ([Some "Ambiguous exception", Marked.get_mark item]
          @ List.map (fun p -> Some "Candidate definition", p) pos)
          "This exception can refer to several definitions. Try using labels \
           to disambiguate"
      | Some (Unique _) -> ()))
  | _ -> ()

(** Translates a surface scope use, which is a bunch of definitions *)
let process_scope_use
    (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program)
    (use : Ast.scope_use) : Desugared.Ast.program =
  let name = fst use.scope_use_name in
  let scope_uid = Desugared.Ast.IdentMap.find name ctxt.scope_idmap in
  (* Make sure the scope exists *)
  let prgm =
    match Scopelang.Ast.ScopeMap.find_opt scope_uid prgm.program_scopes with
    | Some _ -> prgm
    | None -> assert false
    (* should not happen *)
  in
  let precond = use.scope_use_condition in
  List.iter (check_unlabeled_exception scope_uid ctxt) use.scope_use_items;
  List.fold_left
    (process_scope_use_item precond scope_uid ctxt)
    prgm use.scope_use_items

let attribute_to_io (attr : Ast.scope_decl_context_io) : Scopelang.Ast.io =
  {
    Scopelang.Ast.io_output = attr.scope_decl_context_io_output;
    Scopelang.Ast.io_input =
      Marked.map_under_mark
        (fun io ->
          match io with
          | Ast.Input -> Scopelang.Ast.OnlyInput
          | Ast.Internal -> Scopelang.Ast.NoInput
          | Ast.Context -> Scopelang.Ast.Reentrant)
        attr.scope_decl_context_io_input;
  }

(** Main function of this module *)
let desugar_program (ctxt : Name_resolution.context) (prgm : Ast.program) :
    Desugared.Ast.program =
  let empty_prgm =
    {
      Desugared.Ast.program_ctx =
        {
          ctx_structs =
            StructMap.map StructFieldMap.bindings ctxt.Name_resolution.structs;
          ctx_enums =
            EnumMap.map EnumConstructorMap.bindings ctxt.Name_resolution.enums;
        };
      Desugared.Ast.program_scopes =
        Scopelang.Ast.ScopeMap.mapi
          (fun s_uid s_context ->
            {
              Desugared.Ast.scope_vars =
                Desugared.Ast.IdentMap.fold
                  (fun _ v acc ->
                    let v_sig = ScopeVarMap.find v ctxt.var_typs in
                    match v_sig.var_sig_states_list with
                    | [] -> ScopeVarMap.add v Desugared.Ast.WholeVar acc
                    | states ->
                      ScopeVarMap.add v (Desugared.Ast.States states) acc)
                  s_context.Name_resolution.var_idmap ScopeVarMap.empty;
              Desugared.Ast.scope_sub_scopes =
                s_context.Name_resolution.sub_scopes;
              Desugared.Ast.scope_defs =
                (* Initializing the definitions of all scopes and subscope vars,
                   with no rules yet inside *)
                (let scope_vars_defs =
                   Desugared.Ast.IdentMap.fold
                     (fun _ v acc ->
                       let v_sig =
                         ScopeVarMap.find v ctxt.Name_resolution.var_typs
                       in
                       match v_sig.var_sig_states_list with
                       | [] ->
                         let def_key = Desugared.Ast.ScopeDef.Var (v, None) in
                         Desugared.Ast.ScopeDefMap.add def_key
                           {
                             Desugared.Ast.scope_def_rules =
                               Desugared.Ast.RuleMap.empty;
                             Desugared.Ast.scope_def_typ = v_sig.var_sig_typ;
                             Desugared.Ast.scope_def_is_condition =
                               v_sig.var_sig_is_condition;
                             Desugared.Ast.scope_def_io =
                               attribute_to_io v_sig.var_sig_io;
                           }
                           acc
                       | states ->
                         fst
                           (List.fold_left
                              (fun (acc, i) state ->
                                let def_key =
                                  Desugared.Ast.ScopeDef.Var (v, Some state)
                                in
                                ( Desugared.Ast.ScopeDefMap.add def_key
                                    {
                                      Desugared.Ast.scope_def_rules =
                                        Desugared.Ast.RuleMap.empty;
                                      Desugared.Ast.scope_def_typ =
                                        v_sig.var_sig_typ;
                                      Desugared.Ast.scope_def_is_condition =
                                        v_sig.var_sig_is_condition;
                                      Desugared.Ast.scope_def_io =
                                        (* The first state should have the input
                                           I/O of the original variable, and the
                                           last state should have the output I/O
                                           of the original variable. All
                                           intermediate states shall have
                                           "internal" I/O.*)
                                        (let original_io =
                                           attribute_to_io v_sig.var_sig_io
                                         in
                                         let io_input =
                                           if i = 0 then original_io.io_input
                                           else
                                             ( Scopelang.Ast.NoInput,
                                               Marked.get_mark
                                                 (StateName.get_info state) )
                                         in
                                         let io_output =
                                           if i = List.length states - 1 then
                                             original_io.io_output
                                           else
                                             ( false,
                                               Marked.get_mark
                                                 (StateName.get_info state) )
                                         in
                                         { io_input; io_output });
                                    }
                                    acc,
                                  i + 1 ))
                              (acc, 0) states))
                     s_context.Name_resolution.var_idmap
                     Desugared.Ast.ScopeDefMap.empty
                 in
                 let scope_and_subscope_vars_defs =
                   Scopelang.Ast.SubScopeMap.fold
                     (fun subscope_name subscope_uid acc ->
                       Desugared.Ast.IdentMap.fold
                         (fun _ v acc ->
                           let v_sig =
                             ScopeVarMap.find v ctxt.Name_resolution.var_typs
                           in
                           let def_key =
                             Desugared.Ast.ScopeDef.SubScopeVar
                               (subscope_name, v)
                           in
                           Desugared.Ast.ScopeDefMap.add def_key
                             {
                               Desugared.Ast.scope_def_rules =
                                 Desugared.Ast.RuleMap.empty;
                               Desugared.Ast.scope_def_typ = v_sig.var_sig_typ;
                               Desugared.Ast.scope_def_is_condition =
                                 v_sig.var_sig_is_condition;
                               Desugared.Ast.scope_def_io =
                                 attribute_to_io v_sig.var_sig_io;
                             }
                             acc)
                         (Scopelang.Ast.ScopeMap.find subscope_uid
                            ctxt.Name_resolution.scopes)
                           .Name_resolution.var_idmap acc)
                     s_context.sub_scopes scope_vars_defs
                 in
                 scope_and_subscope_vars_defs);
              Desugared.Ast.scope_assertions = [];
              Desugared.Ast.scope_meta_assertions = [];
              Desugared.Ast.scope_uid = s_uid;
            })
          ctxt.Name_resolution.scopes;
    }
  in
  let rec processer_structure
      (prgm : Desugared.Ast.program)
      (item : Ast.law_structure) : Desugared.Ast.program =
    match item with
    | LawHeading (_, children) ->
      List.fold_left
        (fun prgm child -> processer_structure prgm child)
        prgm children
    | CodeBlock (block, _, _) ->
      List.fold_left
        (fun prgm item ->
          match Marked.unmark item with
          | Ast.ScopeUse use -> process_scope_use ctxt prgm use
          | _ -> prgm)
        prgm block
    | LawInclude _ | LawText _ -> prgm
  in
  List.fold_left processer_structure empty_prgm prgm.program_items
