(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr> Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
(** Translation from {!module: Surface.Ast} to {!module: Desugaring.Ast}.

    - Removes syntactic sugars
    - Separate code from legislation *)

(** {1 Translating expressions} *)

let translate_op_kind (k : Ast.op_kind) : Dcalc.Ast.op_kind =
  match k with
  | KInt -> KInt
  | KDec -> KRat
  | KMoney -> KMoney
  | KDate -> KDate
  | KDuration -> KDuration

let translate_binop (op : Ast.binop) : Dcalc.Ast.binop =
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

let translate_unop (op : Ast.unop) : Dcalc.Ast.unop =
  match op with Not -> Not | Minus l -> Minus (translate_op_kind l)

(** The two modules below help performing operations on map with the {!type: Bindlib.box}. Indeed,
    Catala uses the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library to represent bound
    variables in the AST. In this translation, bound variables are used to represent function
    parameters or pattern macthing bindings. *)

module LiftStructFieldMap = Bindlib.Lift (Scopelang.Ast.StructFieldMap)
module LiftEnumConstructorMap = Bindlib.Lift (Scopelang.Ast.EnumConstructorMap)

let disambiguate_constructor (ctxt : Name_resolution.context)
    (constructor : (string Pos.marked option * string Pos.marked) list) (pos : Pos.t) :
    Scopelang.Ast.EnumName.t * Scopelang.Ast.EnumConstructor.t =
  let enum, constructor =
    match constructor with
    | [ c ] -> c
    | _ ->
        Errors.raise_spanned_error "The deep pattern matching syntactic sugar is not yet supported"
          pos
  in
  let possible_c_uids =
    try Desugared.Ast.IdentMap.find (Pos.unmark constructor) ctxt.constructor_idmap
    with Not_found ->
      Errors.raise_spanned_error
        "The name of this constructor has not been defined before, maybe it is a typo?"
        (Pos.get_position constructor)
  in
  match enum with
  | None ->
      if Scopelang.Ast.EnumMap.cardinal possible_c_uids > 1 then
        Errors.raise_spanned_error
          (Format.asprintf
             "This constructor name is ambiguous, it can belong to %a. Disambiguate it by \
              prefixing it with the enum name."
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                (fun fmt (s_name, _) ->
                  Format.fprintf fmt "%a" Scopelang.Ast.EnumName.format_t s_name))
             (Scopelang.Ast.EnumMap.bindings possible_c_uids))
          (Pos.get_position constructor);
      Scopelang.Ast.EnumMap.choose possible_c_uids
  | Some enum -> (
      try
        (* The path is fully qualified *)
        let e_uid = Desugared.Ast.IdentMap.find (Pos.unmark enum) ctxt.enum_idmap in
        try
          let c_uid = Scopelang.Ast.EnumMap.find e_uid possible_c_uids in
          (e_uid, c_uid)
        with Not_found ->
          Errors.raise_spanned_error
            (Format.asprintf "Enum %s does not contain case %s" (Pos.unmark enum)
               (Pos.unmark constructor))
            pos
      with Not_found ->
        Errors.raise_spanned_error
          (Format.asprintf "Enum %s has not been defined before" (Pos.unmark enum))
          (Pos.get_position enum))

(** Usage: [translate_expr scope ctxt expr]

    Translates [expr] into its desugared equivalent. [scope] is used to disambiguate the scope and
    subscopes variables than occur in the expresion *)
let rec translate_expr (scope : Scopelang.Ast.ScopeName.t)
    (inside_definition_of : Desugared.Ast.ScopeDef.t Pos.marked option)
    (ctxt : Name_resolution.context) ((expr, pos) : Ast.expression Pos.marked) :
    Desugared.Ast.expr Pos.marked Bindlib.box =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  let rec_helper = translate_expr scope inside_definition_of ctxt in
  match expr with
  | Binop
      ( (Ast.And, _pos_op),
        (TestMatchCase (e1_sub, ((constructors, Some binding), pos_pattern)), _pos_e1),
        e2 ) ->
      (* This sugar corresponds to [e is P x && e'] and should desugar to [match e with P x -> e' |
         _ -> false] *)
      let enum_uid, c_uid = disambiguate_constructor ctxt constructors pos_pattern in
      let cases =
        Scopelang.Ast.EnumConstructorMap.mapi
          (fun c_uid' tau ->
            if Scopelang.Ast.EnumConstructor.compare c_uid c_uid' <> 0 then
              let nop_var = Desugared.Ast.Var.make ("_", pos) in
              Bindlib.unbox
                (Desugared.Ast.make_abs [| nop_var |]
                   (Bindlib.box (Desugared.Ast.ELit (Dcalc.Ast.LBool false), pos))
                   pos [ tau ] pos)
            else
              let ctxt, binding_var = Name_resolution.add_def_local_var ctxt binding in
              let e2 = translate_expr scope inside_definition_of ctxt e2 in
              Bindlib.unbox (Desugared.Ast.make_abs [| binding_var |] e2 pos [ tau ] pos))
          (Scopelang.Ast.EnumMap.find enum_uid ctxt.enums)
      in
      Bindlib.box_apply
        (fun e1_sub -> (Desugared.Ast.EMatch (e1_sub, enum_uid, cases), pos))
        (translate_expr scope inside_definition_of ctxt e1_sub)
  | IfThenElse (e_if, e_then, e_else) ->
      Bindlib.box_apply3
        (fun e_if e_then e_else -> (Desugared.Ast.EIfThenElse (e_if, e_then, e_else), pos))
        (rec_helper e_if) (rec_helper e_then) (rec_helper e_else)
  | Binop (op, e1, e2) ->
      let op_term =
        Pos.same_pos_as (Desugared.Ast.EOp (Dcalc.Ast.Binop (translate_binop (Pos.unmark op)))) op
      in
      Bindlib.box_apply2
        (fun e1 e2 -> (Desugared.Ast.EApp (op_term, [ e1; e2 ]), pos))
        (rec_helper e1) (rec_helper e2)
  | Unop (op, e) ->
      let op_term =
        Pos.same_pos_as (Desugared.Ast.EOp (Dcalc.Ast.Unop (translate_unop (Pos.unmark op)))) op
      in
      Bindlib.box_apply (fun e -> (Desugared.Ast.EApp (op_term, [ e ]), pos)) (rec_helper e)
  | Literal l ->
      let untyped_term =
        match l with
        | LNumber ((Int i, _), None) -> Desugared.Ast.ELit (Dcalc.Ast.LInt i)
        | LNumber ((Int i, _), Some (Percent, _)) ->
            Desugared.Ast.ELit
              (Dcalc.Ast.LRat Runtime.(decimal_of_integer i /& decimal_of_string "100"))
        | LNumber ((Dec (i, f), _), None) ->
            let digits_f =
              try int_of_float (ceil (float_of_int (Runtime.integer_log2 f) *. log 2.0 /. log 10.0))
              with Invalid_argument _ -> 0
            in
            Desugared.Ast.ELit
              (Dcalc.Ast.LRat
                 Runtime.(
                   decimal_of_integer i
                   +& decimal_of_integer f
                      /& decimal_of_integer (integer_exponentiation (integer_of_int 10) digits_f)))
        | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
            let digits_f =
              try int_of_float (ceil (float_of_int (Runtime.integer_log2 f) *. log 2.0 /. log 10.0))
              with Invalid_argument _ -> 0
            in
            Desugared.Ast.ELit
              (Dcalc.Ast.LRat
                 Runtime.(
                   (decimal_of_integer i
                   +& decimal_of_integer f
                      /& decimal_of_integer (integer_exponentiation (integer_of_int 10) digits_f))
                   /& decimal_of_string "100"))
        | LBool b -> Desugared.Ast.ELit (Dcalc.Ast.LBool b)
        | LMoneyAmount i ->
            Desugared.Ast.ELit
              (Dcalc.Ast.LMoney
                 Runtime.(
                   money_of_cents_integer
                     ((i.money_amount_units *! integer_of_int 100) +! i.money_amount_cents)))
        | LNumber ((Int i, _), Some (Year, _)) ->
            Desugared.Ast.ELit
              (Dcalc.Ast.LDuration (Runtime.duration_of_numbers (Runtime.integer_to_int i) 0 0))
        | LNumber ((Int i, _), Some (Month, _)) ->
            Desugared.Ast.ELit
              (Dcalc.Ast.LDuration (Runtime.duration_of_numbers 0 (Runtime.integer_to_int i) 0))
        | LNumber ((Int i, _), Some (Day, _)) ->
            Desugared.Ast.ELit
              (Dcalc.Ast.LDuration (Runtime.duration_of_numbers 0 0 (Runtime.integer_to_int i)))
        | LNumber ((Dec (_, _), _), Some ((Year | Month | Day), _)) ->
            Errors.raise_spanned_error
              "Impossible to specify decimal amounts of days, months or years" pos
        | LDate date ->
            if Pos.unmark date.literal_date_month > 12 then
              Errors.raise_spanned_error
                "There is an error in this date: the month number is bigger than 12"
                (Pos.get_position date.literal_date_month);
            if Pos.unmark date.literal_date_day > 31 then
              Errors.raise_spanned_error
                "There is an error in this date: the day number is bigger than 31"
                (Pos.get_position date.literal_date_day);
            Desugared.Ast.ELit
              (Dcalc.Ast.LDate
                 (try
                    Runtime.date_of_numbers
                      (Pos.unmark date.literal_date_year)
                      (Pos.unmark date.literal_date_month)
                      (Pos.unmark date.literal_date_day)
                  with Runtime.ImpossibleDate ->
                    Errors.raise_spanned_error
                      "There is an error in this date, it does not correspond to a correct \
                       calendar day"
                      pos))
      in
      Bindlib.box (untyped_term, pos)
  | Ident x -> (
      (* first we check whether this is a local var, then we resort to scope-wide variables *)
      match Desugared.Ast.IdentMap.find_opt x ctxt.local_var_idmap with
      | None -> (
          match Desugared.Ast.IdentMap.find_opt x scope_ctxt.var_idmap with
          | Some uid ->
              (* If the referenced variable has states, then here are the rules to desambiguate. In
                 general, only the last state can be referenced. Except if defining a state of the
                 same variable, then it references the previous state in the chain. *)
              let x_sig = Desugared.Ast.ScopeVarMap.find uid ctxt.var_typs in
              let x_state =
                match x_sig.var_sig_states_list with
                | [] -> None
                | states -> (
                    match inside_definition_of with
                    | Some (Desugared.Ast.ScopeDef.Var (x'_uid, sx'), _)
                      when Desugared.Ast.ScopeVar.compare uid x'_uid = 0 -> (
                        match sx' with
                        | None ->
                            failwith
                              "inconsistent state: inside a definition of a variable with no state \
                               but variable has states"
                        | Some inside_def_state ->
                            if Desugared.Ast.StateName.compare inside_def_state (List.hd states) = 0
                            then
                              Errors.raise_spanned_error
                                "It is impossible to refer to the variable you are defining when \
                                 defining its first state."
                                pos
                            else
                              (* Tricky: we have to retrieve in the list the previous state with
                                 respect to the state that we are defining. *)
                              let correct_state = ref None in
                              ignore
                                (List.fold_left
                                   (fun previous_state state ->
                                     if Desugared.Ast.StateName.compare inside_def_state state = 0
                                     then correct_state := previous_state;
                                     Some state)
                                   None states);
                              !correct_state)
                    | _ ->
                        (* we take the last state in the chain *) Some (List.hd (List.rev states)))
              in
              Bindlib.box (Desugared.Ast.ELocation (ScopeVar ((uid, pos), x_state)), pos)
          | None ->
              Name_resolution.raise_unknown_identifier "for a local or scope-wide variable" (x, pos)
          )
      | Some uid ->
          Desugared.Ast.make_var (uid, pos) (* the whole box thing is to accomodate for this case *)
      )
  | Dotted (e, c, x) -> (
      match Pos.unmark e with
      | Ident y when Name_resolution.is_subscope_uid scope ctxt y ->
          (* In this case, y.x is a subscope variable *)
          let subscope_uid : Scopelang.Ast.SubScopeName.t =
            Name_resolution.get_subscope_uid scope ctxt (Pos.same_pos_as y e)
          in
          let subscope_real_uid : Scopelang.Ast.ScopeName.t =
            Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
          in
          let subscope_var_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
          Bindlib.box
            ( Desugared.Ast.ELocation
                (SubScopeVar (subscope_real_uid, (subscope_uid, pos), (subscope_var_uid, pos))),
              pos )
      | _ -> (
          (* In this case e.x is the struct field x access of expression e *)
          let e = translate_expr scope inside_definition_of ctxt e in
          let x_possible_structs =
            try Desugared.Ast.IdentMap.find (Pos.unmark x) ctxt.field_idmap
            with Not_found ->
              Errors.raise_spanned_error "Unknown subscope or struct field name"
                (Pos.get_position x)
          in
          match c with
          | None ->
              (* No constructor name was specified *)
              if Scopelang.Ast.StructMap.cardinal x_possible_structs > 1 then
                Errors.raise_spanned_error
                  (Format.asprintf
                     "This struct field name is ambiguous, it can belong to %a. Disambiguate it by \
                      prefixing it with the struct name."
                     (Format.pp_print_list
                        ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                        (fun fmt (s_name, _) ->
                          Format.fprintf fmt "%a" Scopelang.Ast.StructName.format_t s_name))
                     (Scopelang.Ast.StructMap.bindings x_possible_structs))
                  (Pos.get_position x)
              else
                let s_uid, f_uid = Scopelang.Ast.StructMap.choose x_possible_structs in
                Bindlib.box_apply (fun e -> (Desugared.Ast.EStructAccess (e, f_uid, s_uid), pos)) e
          | Some c_name -> (
              try
                let c_uid = Desugared.Ast.IdentMap.find (Pos.unmark c_name) ctxt.struct_idmap in
                try
                  let f_uid = Scopelang.Ast.StructMap.find c_uid x_possible_structs in
                  Bindlib.box_apply
                    (fun e -> (Desugared.Ast.EStructAccess (e, f_uid, c_uid), pos))
                    e
                with Not_found ->
                  Errors.raise_spanned_error
                    (Format.asprintf "Struct %s does not contain field %s" (Pos.unmark c_name)
                       (Pos.unmark x))
                    pos
              with Not_found ->
                Errors.raise_spanned_error
                  (Format.asprintf "Struct %s has not been defined before" (Pos.unmark c_name))
                  (Pos.get_position c_name))))
  | FunCall (f, arg) ->
      Bindlib.box_apply2
        (fun f arg -> (Desugared.Ast.EApp (f, [ arg ]), pos))
        (rec_helper f) (rec_helper arg)
  | StructLit (s_name, fields) ->
      let s_uid =
        try Desugared.Ast.IdentMap.find (Pos.unmark s_name) ctxt.struct_idmap
        with Not_found ->
          Errors.raise_spanned_error "This identifier should refer to a struct name"
            (Pos.get_position s_name)
      in
      let s_fields =
        List.fold_left
          (fun s_fields (f_name, f_e) ->
            let f_uid =
              try
                Scopelang.Ast.StructMap.find s_uid
                  (Desugared.Ast.IdentMap.find (Pos.unmark f_name) ctxt.field_idmap)
              with Not_found ->
                Errors.raise_spanned_error
                  (Format.asprintf "This identifier should refer to a field of struct %s"
                     (Pos.unmark s_name))
                  (Pos.get_position f_name)
            in
            (match Scopelang.Ast.StructFieldMap.find_opt f_uid s_fields with
            | None -> ()
            | Some e_field ->
                Errors.raise_multispanned_error
                  (Format.asprintf "The field %a has been defined twice:"
                     Scopelang.Ast.StructFieldName.format_t f_uid)
                  [ (None, Pos.get_position f_e); (None, Pos.get_position (Bindlib.unbox e_field)) ]);
            let f_e = translate_expr scope inside_definition_of ctxt f_e in
            Scopelang.Ast.StructFieldMap.add f_uid f_e s_fields)
          Scopelang.Ast.StructFieldMap.empty fields
      in
      let expected_s_fields = Scopelang.Ast.StructMap.find s_uid ctxt.structs in
      Scopelang.Ast.StructFieldMap.iter
        (fun expected_f _ ->
          if not (Scopelang.Ast.StructFieldMap.mem expected_f s_fields) then
            Errors.raise_spanned_error
              (Format.asprintf "Missing field for structure %a: \"%a\""
                 Scopelang.Ast.StructName.format_t s_uid Scopelang.Ast.StructFieldName.format_t
                 expected_f)
              pos)
        expected_s_fields;

      Bindlib.box_apply
        (fun s_fields -> (Desugared.Ast.EStruct (s_uid, s_fields), pos))
        (LiftStructFieldMap.lift_box s_fields)
  | EnumInject (enum, constructor, payload) -> (
      let possible_c_uids =
        try Desugared.Ast.IdentMap.find (Pos.unmark constructor) ctxt.constructor_idmap
        with Not_found ->
          Errors.raise_spanned_error
            "The name of this constructor has not been defined before, maybe it is a typo?"
            (Pos.get_position constructor)
      in
      match enum with
      | None ->
          if
            (* No constructor name was specified *)
            Scopelang.Ast.EnumMap.cardinal possible_c_uids > 1
          then
            Errors.raise_spanned_error
              (Format.asprintf
                 "This constructor name is ambiguous, it can belong to %a. Desambiguate it by \
                  prefixing it with the enum name."
                 (Format.pp_print_list
                    ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                    (fun fmt (s_name, _) ->
                      Format.fprintf fmt "%a" Scopelang.Ast.EnumName.format_t s_name))
                 (Scopelang.Ast.EnumMap.bindings possible_c_uids))
              (Pos.get_position constructor)
          else
            let e_uid, c_uid = Scopelang.Ast.EnumMap.choose possible_c_uids in
            let payload = Option.map (translate_expr scope inside_definition_of ctxt) payload in
            Bindlib.box_apply
              (fun payload ->
                ( Desugared.Ast.EEnumInj
                    ( (match payload with
                      | Some e' -> e'
                      | None -> (Desugared.Ast.ELit Dcalc.Ast.LUnit, Pos.get_position constructor)),
                      c_uid,
                      e_uid ),
                  pos ))
              (Bindlib.box_opt payload)
      | Some enum -> (
          try
            (* The path has been fully qualified *)
            let e_uid = Desugared.Ast.IdentMap.find (Pos.unmark enum) ctxt.enum_idmap in
            try
              let c_uid = Scopelang.Ast.EnumMap.find e_uid possible_c_uids in
              let payload = Option.map (translate_expr scope inside_definition_of ctxt) payload in
              Bindlib.box_apply
                (fun payload ->
                  ( Desugared.Ast.EEnumInj
                      ( (match payload with
                        | Some e' -> e'
                        | None -> (Desugared.Ast.ELit Dcalc.Ast.LUnit, Pos.get_position constructor)),
                        c_uid,
                        e_uid ),
                    pos ))
                (Bindlib.box_opt payload)
            with Not_found ->
              Errors.raise_spanned_error
                (Format.asprintf "Enum %s does not contain case %s" (Pos.unmark enum)
                   (Pos.unmark constructor))
                pos
          with Not_found ->
            Errors.raise_spanned_error
              (Format.asprintf "Enum %s has not been defined before" (Pos.unmark enum))
              (Pos.get_position enum)))
  | MatchWith (e1, (cases, _cases_pos)) ->
      let e1 = translate_expr scope inside_definition_of ctxt e1 in
      let cases_d, e_uid =
        disambiguate_match_and_build_expression scope inside_definition_of ctxt cases
      in
      Bindlib.box_apply2
        (fun e1 cases_d -> (Desugared.Ast.EMatch (e1, e_uid, cases_d), pos))
        e1
        (LiftEnumConstructorMap.lift_box cases_d)
  | TestMatchCase (e1, pattern) ->
      (match snd (Pos.unmark pattern) with
      | None -> ()
      | Some binding ->
          Errors.print_spanned_warning
            "This binding will be ignored (remove it to suppress warning)"
            (Pos.get_position binding));
      let enum_uid, c_uid =
        disambiguate_constructor ctxt (fst (Pos.unmark pattern)) (Pos.get_position pattern)
      in
      let cases =
        Scopelang.Ast.EnumConstructorMap.mapi
          (fun c_uid' tau ->
            let nop_var = Desugared.Ast.Var.make ("_", pos) in
            Bindlib.unbox
              (Desugared.Ast.make_abs [| nop_var |]
                 (Bindlib.box
                    ( Desugared.Ast.ELit
                        (Dcalc.Ast.LBool (Scopelang.Ast.EnumConstructor.compare c_uid c_uid' = 0)),
                      pos ))
                 pos [ tau ] pos))
          (Scopelang.Ast.EnumMap.find enum_uid ctxt.enums)
      in
      Bindlib.box_apply
        (fun e -> (Desugared.Ast.EMatch (e, enum_uid, cases), pos))
        (translate_expr scope inside_definition_of ctxt e1)
  | ArrayLit es ->
      Bindlib.box_apply
        (fun es -> (Desugared.Ast.EArray es, pos))
        (Bindlib.box_list (List.map rec_helper es))
  | CollectionOp ((((Ast.Filter | Ast.Map) as op'), _pos_op'), param', collection, predicate) ->
      let collection = rec_helper collection in
      let ctxt, param = Name_resolution.add_def_local_var ctxt param' in
      let f_pred =
        Desugared.Ast.make_abs [| param |]
          (translate_expr scope inside_definition_of ctxt predicate)
          pos [ (Scopelang.Ast.TAny, pos) ] pos
      in
      Bindlib.box_apply2
        (fun f_pred collection ->
          ( Desugared.Ast.EApp
              ( ( Desugared.Ast.EOp
                    (match op' with
                    | Ast.Map -> Dcalc.Ast.Binop Dcalc.Ast.Map
                    | Ast.Filter -> Dcalc.Ast.Binop Dcalc.Ast.Filter
                    | _ -> assert false (* should not happen *)),
                  pos ),
                [ f_pred; collection ] ),
            pos ))
        f_pred collection
  | CollectionOp
      ( (Ast.Aggregate (Ast.AggregateArgExtremum (max_or_min, pred_typ, init)), pos_op'),
        param',
        collection,
        predicate ) ->
      let init = rec_helper init in
      let collection = rec_helper collection in
      let ctxt, param = Name_resolution.add_def_local_var ctxt param' in
      let op_kind =
        match pred_typ with
        | Ast.Integer -> Dcalc.Ast.KInt
        | Ast.Decimal -> Dcalc.Ast.KRat
        | Ast.Money -> Dcalc.Ast.KMoney
        | Ast.Duration -> Dcalc.Ast.KDuration
        | Ast.Date -> Dcalc.Ast.KDate
        | _ ->
            Errors.raise_spanned_error
              (Format.asprintf "It is impossible to compute the arg-%s of two values of type %a"
                 (if max_or_min then "max" else "min")
                 Print.format_primitive_typ pred_typ)
              pos
      in
      let cmp_op = if max_or_min then Dcalc.Ast.Gt op_kind else Dcalc.Ast.Lt op_kind in
      let f_pred =
        Desugared.Ast.make_abs [| param |]
          (translate_expr scope inside_definition_of ctxt predicate)
          pos [ (Scopelang.Ast.TAny, pos) ] pos
      in
      let f_pred_var = Desugared.Ast.Var.make ("predicate", Pos.get_position predicate) in
      let f_pred_var_e = Desugared.Ast.make_var (f_pred_var, Pos.get_position predicate) in
      let acc_var = Desugared.Ast.Var.make ("acc", pos) in
      let acc_var_e = Desugared.Ast.make_var (acc_var, pos) in
      let item_var = Desugared.Ast.Var.make ("item", Pos.get_position (Bindlib.unbox collection)) in
      let item_var_e =
        Desugared.Ast.make_var (item_var, Pos.get_position (Bindlib.unbox collection))
      in
      let fold_body =
        Bindlib.box_apply3
          (fun acc_var_e item_var_e f_pred_var_e ->
            ( Desugared.Ast.EIfThenElse
                ( ( Desugared.Ast.EApp
                      ( (Desugared.Ast.EOp (Dcalc.Ast.Binop cmp_op), pos_op'),
                        [
                          (Desugared.Ast.EApp (f_pred_var_e, [ acc_var_e ]), pos);
                          (Desugared.Ast.EApp (f_pred_var_e, [ item_var_e ]), pos);
                        ] ),
                    pos ),
                  acc_var_e,
                  item_var_e ),
              pos ))
          acc_var_e item_var_e f_pred_var_e
      in
      let fold_f =
        Desugared.Ast.make_abs [| acc_var; item_var |] fold_body pos
          [ (Scopelang.Ast.TAny, pos); (Scopelang.Ast.TAny, pos) ]
          pos
      in
      let fold =
        Bindlib.box_apply3
          (fun fold_f collection init ->
            ( Desugared.Ast.EApp
                ( (Desugared.Ast.EOp (Dcalc.Ast.Ternop Dcalc.Ast.Fold), pos),
                  [ fold_f; init; collection ] ),
              pos ))
          fold_f collection init
      in
      Desugared.Ast.make_let_in f_pred_var (Scopelang.Ast.TAny, pos) f_pred fold
  | CollectionOp (op', param', collection, predicate) ->
      let ctxt, param = Name_resolution.add_def_local_var ctxt param' in
      let collection = rec_helper collection in
      let init =
        match Pos.unmark op' with
        | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
            assert false (* should not happen *)
        | Ast.Exists ->
            Bindlib.box (Desugared.Ast.ELit (Dcalc.Ast.LBool false), Pos.get_position op')
        | Ast.Forall -> Bindlib.box (Desugared.Ast.ELit (Dcalc.Ast.LBool true), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Integer) ->
            Bindlib.box
              (Desugared.Ast.ELit (Dcalc.Ast.LInt (Runtime.integer_of_int 0)), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) ->
            Bindlib.box
              ( Desugared.Ast.ELit (Dcalc.Ast.LRat (Runtime.decimal_of_string "0")),
                Pos.get_position op' )
        | Ast.Aggregate (Ast.AggregateSum Ast.Money) ->
            Bindlib.box
              ( Desugared.Ast.ELit
                  (Dcalc.Ast.LMoney (Runtime.money_of_cents_integer (Runtime.integer_of_int 0))),
                Pos.get_position op' )
        | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
            Bindlib.box
              ( Desugared.Ast.ELit (Dcalc.Ast.LDuration (Runtime.duration_of_numbers 0 0 0)),
                Pos.get_position op' )
        | Ast.Aggregate (Ast.AggregateSum t) ->
            Errors.raise_spanned_error
              (Format.asprintf "It is impossible to sum two values of type %a together"
                 Print.format_primitive_typ t)
              pos
        | Ast.Aggregate (Ast.AggregateExtremum (_, _, init)) -> rec_helper init
        | Ast.Aggregate Ast.AggregateCount ->
            Bindlib.box
              (Desugared.Ast.ELit (Dcalc.Ast.LInt (Runtime.integer_of_int 0)), Pos.get_position op')
      in
      let acc_var = Desugared.Ast.Var.make ("acc", Pos.get_position param') in
      let acc = Desugared.Ast.make_var (acc_var, Pos.get_position param') in
      let f_body =
        let make_body (op : Dcalc.Ast.binop) =
          Bindlib.box_apply2
            (fun predicate acc ->
              ( Desugared.Ast.EApp
                  ( (Desugared.Ast.EOp (Dcalc.Ast.Binop op), Pos.get_position op'),
                    [ acc; predicate ] ),
                pos ))
            (translate_expr scope inside_definition_of ctxt predicate)
            acc
        in
        let make_extr_body (cmp_op : Dcalc.Ast.binop) (t : Scopelang.Ast.typ Pos.marked) =
          let tmp_var = Desugared.Ast.Var.make ("tmp", Pos.get_position param') in
          let tmp = Desugared.Ast.make_var (tmp_var, Pos.get_position param') in
          Desugared.Ast.make_let_in tmp_var t
            (translate_expr scope inside_definition_of ctxt predicate)
            (Bindlib.box_apply2
               (fun acc tmp ->
                 ( Desugared.Ast.EIfThenElse
                     ( ( Desugared.Ast.EApp
                           ( (Desugared.Ast.EOp (Dcalc.Ast.Binop cmp_op), Pos.get_position op'),
                             [ acc; tmp ] ),
                         pos ),
                       acc,
                       tmp ),
                   pos ))
               acc tmp)
        in
        match Pos.unmark op' with
        | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
            assert false (* should not happen *)
        | Ast.Exists -> make_body Dcalc.Ast.Or
        | Ast.Forall -> make_body Dcalc.Ast.And
        | Ast.Aggregate (Ast.AggregateSum Ast.Integer) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KInt)
        | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KRat)
        | Ast.Aggregate (Ast.AggregateSum Ast.Money) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KMoney)
        | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
            make_body (Dcalc.Ast.Add Dcalc.Ast.KDuration)
        | Ast.Aggregate (Ast.AggregateSum _) -> assert false (* should not happen *)
        | Ast.Aggregate (Ast.AggregateExtremum (max_or_min, t, _)) ->
            let op_kind, typ =
              match t with
              | Ast.Integer -> (Dcalc.Ast.KInt, (Scopelang.Ast.TLit TInt, pos))
              | Ast.Decimal -> (Dcalc.Ast.KRat, (Scopelang.Ast.TLit TRat, pos))
              | Ast.Money -> (Dcalc.Ast.KMoney, (Scopelang.Ast.TLit TMoney, pos))
              | Ast.Duration -> (Dcalc.Ast.KDuration, (Scopelang.Ast.TLit TDuration, pos))
              | Ast.Date -> (Dcalc.Ast.KDate, (Scopelang.Ast.TLit TDate, pos))
              | _ ->
                  Errors.raise_spanned_error
                    (Format.asprintf "It is impossible to compute the %s of two values of type %a"
                       (if max_or_min then "max" else "min")
                       Print.format_primitive_typ t)
                    pos
            in
            let cmp_op = if max_or_min then Dcalc.Ast.Gt op_kind else Dcalc.Ast.Lt op_kind in
            make_extr_body cmp_op typ
        | Ast.Aggregate Ast.AggregateCount ->
            Bindlib.box_apply2
              (fun predicate acc ->
                ( Desugared.Ast.EIfThenElse
                    ( predicate,
                      ( Desugared.Ast.EApp
                          ( ( Desugared.Ast.EOp (Dcalc.Ast.Binop (Dcalc.Ast.Add Dcalc.Ast.KInt)),
                              Pos.get_position op' ),
                            [
                              acc;
                              ( Desugared.Ast.ELit (Dcalc.Ast.LInt (Runtime.integer_of_int 1)),
                                Pos.get_position predicate );
                            ] ),
                        pos ),
                      acc ),
                  pos ))
              (translate_expr scope inside_definition_of ctxt predicate)
              acc
      in
      let f =
        let make_f (t : Dcalc.Ast.typ_lit) =
          Bindlib.box_apply
            (fun binder ->
              ( Desugared.Ast.EAbs
                  ( (binder, pos),
                    [
                      (Scopelang.Ast.TLit t, Pos.get_position op');
                      (Scopelang.Ast.TAny, pos)
                      (* we put any here because the type of the elements of the arrays is not
                         always the type of the accumulator; for instance in AggregateCount. *);
                    ] ),
                pos ))
            (Bindlib.bind_mvar [| acc_var; param |] f_body)
        in
        match Pos.unmark op' with
        | Ast.Map | Ast.Filter | Ast.Aggregate (Ast.AggregateArgExtremum _) ->
            assert false (* should not happen *)
        | Ast.Exists -> make_f Dcalc.Ast.TBool
        | Ast.Forall -> make_f Dcalc.Ast.TBool
        | Ast.Aggregate (Ast.AggregateSum Ast.Integer)
        | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Integer, _)) ->
            make_f Dcalc.Ast.TInt
        | Ast.Aggregate (Ast.AggregateSum Ast.Decimal)
        | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Decimal, _)) ->
            make_f Dcalc.Ast.TRat
        | Ast.Aggregate (Ast.AggregateSum Ast.Money)
        | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Money, _)) ->
            make_f Dcalc.Ast.TMoney
        | Ast.Aggregate (Ast.AggregateSum Ast.Duration)
        | Ast.Aggregate (Ast.AggregateExtremum (_, Ast.Duration, _)) ->
            make_f Dcalc.Ast.TDuration
        | Ast.Aggregate (Ast.AggregateSum _) | Ast.Aggregate (Ast.AggregateExtremum _) ->
            assert false (* should not happen *)
        | Ast.Aggregate Ast.AggregateCount -> make_f Dcalc.Ast.TInt
      in
      Bindlib.box_apply3
        (fun f collection init ->
          ( Desugared.Ast.EApp
              ((Desugared.Ast.EOp (Dcalc.Ast.Ternop Dcalc.Ast.Fold), pos), [ f; init; collection ]),
            pos ))
        f collection init
  | MemCollection (member, collection) ->
      let param_var = Desugared.Ast.Var.make ("collection_member", pos) in
      let param = Desugared.Ast.make_var (param_var, pos) in
      let collection = rec_helper collection in
      let init = Bindlib.box (Desugared.Ast.ELit (Dcalc.Ast.LBool false), pos) in
      let acc_var = Desugared.Ast.Var.make ("acc", pos) in
      let acc = Desugared.Ast.make_var (acc_var, pos) in
      let f_body =
        Bindlib.box_apply3
          (fun member acc param ->
            ( Desugared.Ast.EApp
                ( (Desugared.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.Or), pos),
                  [
                    ( Desugared.Ast.EApp
                        ((Desugared.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.Eq), pos), [ member; param ]),
                      pos );
                    acc;
                  ] ),
              pos ))
          (translate_expr scope inside_definition_of ctxt member)
          acc param
      in
      let f =
        Bindlib.box_apply
          (fun binder ->
            ( Desugared.Ast.EAbs
                ( (binder, pos),
                  [ (Scopelang.Ast.TLit Dcalc.Ast.TBool, pos); (Scopelang.Ast.TAny, pos) ] ),
              pos ))
          (Bindlib.bind_mvar [| acc_var; param_var |] f_body)
      in
      Bindlib.box_apply3
        (fun f collection init ->
          ( Desugared.Ast.EApp
              ((Desugared.Ast.EOp (Dcalc.Ast.Ternop Dcalc.Ast.Fold), pos), [ f; init; collection ]),
            pos ))
        f collection init
  | Builtin IntToDec -> Bindlib.box (Desugared.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.IntToRat), pos)
  | Builtin Cardinal -> Bindlib.box (Desugared.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.Length), pos)
  | Builtin GetDay -> Bindlib.box (Desugared.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetDay), pos)
  | Builtin GetMonth -> Bindlib.box (Desugared.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetMonth), pos)
  | Builtin GetYear -> Bindlib.box (Desugared.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetYear), pos)

and disambiguate_match_and_build_expression (scope : Scopelang.Ast.ScopeName.t)
    (inside_definition_of : Desugared.Ast.ScopeDef.t Pos.marked option)
    (ctxt : Name_resolution.context) (cases : Ast.match_case Pos.marked list) :
    Desugared.Ast.expr Pos.marked Bindlib.box Scopelang.Ast.EnumConstructorMap.t
    * Scopelang.Ast.EnumName.t =
  let create_var = function
    | None -> (ctxt, (Desugared.Ast.Var.make ("_", Pos.no_pos), Pos.no_pos))
    | Some param ->
        let ctxt, param_var = Name_resolution.add_def_local_var ctxt param in
        (ctxt, (param_var, Pos.get_position param))
  in
  let bind_case_body (c_uid : Dcalc.Ast.EnumConstructor.t) (e_uid : Dcalc.Ast.EnumName.t)
      (ctxt : Name_resolution.context) (param_pos : Pos.t) (case_body : ('a * Pos.t) Bindlib.box)
      (e_binder : (Desugared.Ast.expr, Desugared.Ast.expr * Pos.t) Bindlib.mbinder Bindlib.box) :
      'c Bindlib.box =
    Bindlib.box_apply2
      (fun e_binder case_body ->
        Pos.same_pos_as
          (Desugared.Ast.EAbs
             ( (e_binder, param_pos),
               [
                 Scopelang.Ast.EnumConstructorMap.find c_uid
                   (Scopelang.Ast.EnumMap.find e_uid ctxt.Name_resolution.enums);
               ] ))
          case_body)
      e_binder case_body
  in
  let bind_match_cases (cases_d, e_uid, curr_index) (case, case_pos) =
    match case with
    | Ast.MatchCase case ->
        let constructor, binding = Pos.unmark case.Ast.match_case_pattern in
        let e_uid', c_uid =
          disambiguate_constructor ctxt constructor (Pos.get_position case.Ast.match_case_pattern)
        in
        let e_uid =
          match e_uid with
          | None -> e_uid'
          | Some e_uid ->
              if e_uid = e_uid' then e_uid
              else
                Errors.raise_spanned_error
                  (Format.asprintf
                     "This case matches a constructor of enumeration %a but previous case were \
                      matching constructors of enumeration %a"
                     Scopelang.Ast.EnumName.format_t e_uid Scopelang.Ast.EnumName.format_t e_uid')
                  (Pos.get_position case.Ast.match_case_pattern)
        in
        (match Scopelang.Ast.EnumConstructorMap.find_opt c_uid cases_d with
        | None -> ()
        | Some e_case ->
            Errors.raise_multispanned_error
              (Format.asprintf "The constructor %a has been matched twice:"
                 Scopelang.Ast.EnumConstructor.format_t c_uid)
              [
                (None, Pos.get_position case.match_case_expr);
                (None, Pos.get_position (Bindlib.unbox e_case));
              ]);
        let ctxt, (param_var, param_pos) = create_var binding in
        let case_body = translate_expr scope inside_definition_of ctxt case.Ast.match_case_expr in
        let e_binder = Bindlib.bind_mvar (Array.of_list [ param_var ]) case_body in
        let case_expr = bind_case_body c_uid e_uid ctxt param_pos case_body e_binder in
        (Scopelang.Ast.EnumConstructorMap.add c_uid case_expr cases_d, Some e_uid, curr_index + 1)
    | Ast.WildCard match_case_expr -> (
        let nb_cases = List.length cases in
        let raise_wildcard_not_last_case_err () =
          Errors.raise_multispanned_error "Wildcard must be the last match case"
            [
              (Some "Not ending wildcard:", case_pos);
              (Some "Next reachable case:", curr_index + 1 |> List.nth cases |> Pos.get_position);
            ]
        in
        match e_uid with
        | None ->
            if 1 = nb_cases then
              Errors.raise_spanned_error
                "Couldn't infer the enumeration name from lonely wildcard (wildcard cannot be used \
                 as single match case)"
                case_pos
            else raise_wildcard_not_last_case_err ()
        | Some e_uid ->
            if curr_index < nb_cases - 1 then raise_wildcard_not_last_case_err ();
            let missing_constructors =
              Scopelang.Ast.EnumMap.find e_uid ctxt.Name_resolution.enums
              |> Scopelang.Ast.EnumConstructorMap.filter_map (fun c_uid _ ->
                     match Scopelang.Ast.EnumConstructorMap.find_opt c_uid cases_d with
                     | Some _ -> None
                     | None -> Some c_uid)
            in
            if Scopelang.Ast.EnumConstructorMap.is_empty missing_constructors then
              Errors.print_spanned_warning
                (Format.asprintf
                   "Unreachable match case, all constructors of the enumeration %a are already \
                    specified"
                   Scopelang.Ast.EnumName.format_t e_uid)
                case_pos;
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
            let ctxt, (payload_var, var_pos) = create_var None in
            let case_body = translate_expr scope inside_definition_of ctxt match_case_expr in
            let e_binder = Bindlib.bind_mvar (Array.of_list [ payload_var ]) case_body in

            (* For each missing cases, binds the wildcard payload. *)
            Scopelang.Ast.EnumConstructorMap.fold
              (fun c_uid _ (cases_d, e_uid_opt, curr_index) ->
                let case_expr = bind_case_body c_uid e_uid ctxt var_pos case_body e_binder in
                ( Scopelang.Ast.EnumConstructorMap.add c_uid case_expr cases_d,
                  e_uid_opt,
                  curr_index + 1 ))
              missing_constructors (cases_d, Some e_uid, curr_index))
  in
  let expr, e_name, _ =
    List.fold_left bind_match_cases (Scopelang.Ast.EnumConstructorMap.empty, None, 0) cases
  in
  (expr, Option.get e_name)
  [@@ocamlformat "wrap-comments=false"]

(** {1 Translating scope definitions} *)

(** A scope use can be annotated with a pervasive precondition, in which case this precondition has
    to be appended to the justifications of each definition in the subscope use. This is what this
    function does. *)
let merge_conditions (precond : Desugared.Ast.expr Pos.marked Bindlib.box option)
    (cond : Desugared.Ast.expr Pos.marked Bindlib.box option) (default_pos : Pos.t) :
    Desugared.Ast.expr Pos.marked Bindlib.box =
  match (precond, cond) with
  | Some precond, Some cond ->
      let op_term =
        (Desugared.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.And), Pos.get_position (Bindlib.unbox precond))
      in
      Bindlib.box_apply2
        (fun precond cond ->
          (Desugared.Ast.EApp (op_term, [ precond; cond ]), Pos.get_position precond))
        precond cond
  | Some cond, None | None, Some cond -> cond
  | None, None -> Bindlib.box (Desugared.Ast.ELit (Dcalc.Ast.LBool true), default_pos)

(** Translates a surface definition into condition into a desugared {!type: Desugared.Ast.rule} *)
let process_default (ctxt : Name_resolution.context) (scope : Scopelang.Ast.ScopeName.t)
    (def_key : Desugared.Ast.ScopeDef.t Pos.marked) (rule_id : Desugared.Ast.RuleName.t)
    (param_uid : Desugared.Ast.Var.t Pos.marked option)
    (precond : Desugared.Ast.expr Pos.marked Bindlib.box option)
    (exception_to_rules : Desugared.Ast.RuleSet.t Pos.marked)
    (just : Ast.expression Pos.marked option) (cons : Ast.expression Pos.marked) :
    Desugared.Ast.rule =
  let just =
    match just with
    | Some just -> Some (translate_expr scope (Some def_key) ctxt just)
    | None -> None
  in
  let just = merge_conditions precond just (Pos.get_position def_key) in
  let cons = translate_expr scope (Some def_key) ctxt cons in
  {
    rule_just = just;
    rule_cons = cons;
    rule_parameter =
      (let def_key_typ = Name_resolution.get_def_typ ctxt (Pos.unmark def_key) in
       match (Pos.unmark def_key_typ, param_uid) with
       | Scopelang.Ast.TArrow (t_in, _), Some param_uid -> Some (Pos.unmark param_uid, t_in)
       | Scopelang.Ast.TArrow _, None ->
           Errors.raise_spanned_error
             "This definition has a function type but the parameter is missing"
             (Pos.get_position (Bindlib.unbox cons))
       | _, Some _ ->
           Errors.raise_spanned_error
             "This definition has a parameter but its type is not a function"
             (Pos.get_position (Bindlib.unbox cons))
       | _ -> None);
    rule_exception_to_rules = exception_to_rules;
    rule_id;
  }

(** Wrapper around {!val: process_default} that performs some name disambiguation *)
let process_def (precond : Desugared.Ast.expr Pos.marked Bindlib.box option)
    (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (def : Ast.definition) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope = Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let def_key =
    Name_resolution.get_def_key (Pos.unmark def.definition_name) def.definition_state scope_uid ctxt
      (Pos.get_position def.definition_expr)
  in
  let scope_def_ctxt = Desugared.Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts in
  (* We add to the name resolution context the name of the parameter variable *)
  let param_uid, new_ctxt =
    match def.definition_parameter with
    | None -> (None, ctxt)
    | Some param ->
        let ctxt, param_var = Name_resolution.add_def_local_var ctxt param in
        (Some (Pos.same_pos_as param_var param), ctxt)
  in
  let scope_updated =
    let scope_def = Desugared.Ast.ScopeDefMap.find def_key scope.scope_defs in
    let rule_name = def.definition_id in
    let parent_rules =
      match def.Ast.definition_exception_to with
      | NotAnException -> (Desugared.Ast.RuleSet.empty, Pos.get_position def.Ast.definition_name)
      | UnlabeledException -> (
          match scope_def_ctxt.default_exception_rulename with
          (* This should have been caught previously by check_unlabeled_exception *)
          | None | Some (Name_resolution.Ambiguous _) -> assert false (* should not happen *)
          | Some (Name_resolution.Unique (name, pos)) -> (Desugared.Ast.RuleSet.singleton name, pos)
          )
      | ExceptionToLabel label -> (
          try
            let label_id =
              Desugared.Ast.IdentMap.find (Pos.unmark label) scope_def_ctxt.label_idmap
            in
            ( Desugared.Ast.LabelMap.find label_id scope_def.scope_def_label_groups,
              Pos.get_position def.Ast.definition_name )
          with Not_found ->
            Errors.raise_spanned_error
              (Format.asprintf "Unknown label for the scope variable %a: \"%s\""
                 Desugared.Ast.ScopeDef.format_t def_key (Pos.unmark label))
              (Pos.get_position label))
    in
    let scope_def =
      {
        scope_def with
        scope_def_rules =
          Desugared.Ast.RuleMap.add rule_name
            (process_default new_ctxt scope_uid
               (def_key, Pos.get_position def.definition_name)
               rule_name param_uid precond parent_rules def.definition_condition def.definition_expr)
            scope_def.scope_def_rules;
      }
    in
    { scope with scope_defs = Desugared.Ast.ScopeDefMap.add def_key scope_def scope.scope_defs }
  in
  {
    prgm with
    program_scopes = Scopelang.Ast.ScopeMap.add scope_uid scope_updated prgm.program_scopes;
  }

(** Translates a {!type: Surface.Ast.rule} from the surface language *)
let process_rule (precond : Desugared.Ast.expr Pos.marked Bindlib.box option)
    (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (rule : Ast.rule) : Desugared.Ast.program =
  let def = Ast.rule_to_def rule in
  process_def precond scope ctxt prgm def

(** Translates assertions *)
let process_assert (precond : Desugared.Ast.expr Pos.marked Bindlib.box option)
    (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (ass : Ast.assertion) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope = Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes in
  let ass =
    translate_expr scope_uid None ctxt
      (match ass.Ast.assertion_condition with
      | None -> ass.Ast.assertion_content
      | Some cond ->
          ( Ast.IfThenElse
              (cond, ass.Ast.assertion_content, Pos.same_pos_as (Ast.Literal (Ast.LBool true)) cond),
            Pos.get_position cond ))
  in
  let ass =
    match precond with
    | Some precond ->
        Bindlib.box_apply2
          (fun precond ass ->
            ( Desugared.Ast.EIfThenElse
                (precond, ass, Pos.same_pos_as (Desugared.Ast.ELit (Dcalc.Ast.LBool true)) precond),
              Pos.get_position precond ))
          precond ass
    | None -> ass
  in
  let new_scope = { scope with scope_assertions = ass :: scope.scope_assertions } in
  { prgm with program_scopes = Scopelang.Ast.ScopeMap.add scope_uid new_scope prgm.program_scopes }

(** Translates a surface definition, rule or assertion *)
let process_scope_use_item (precond : Ast.expression Pos.marked option)
    (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (item : Ast.scope_use_item Pos.marked) : Desugared.Ast.program =
  let precond = Option.map (translate_expr scope None ctxt) precond in
  match Pos.unmark item with
  | Ast.Rule rule -> process_rule precond scope ctxt prgm rule
  | Ast.Definition def -> process_def precond scope ctxt prgm def
  | Ast.Assertion ass -> process_assert precond scope ctxt prgm ass
  | _ -> prgm

(** {1 Translating top-level items} *)

(* If this is an unlabeled exception, ensures that it has a unique default definition *)
let check_unlabeled_exception (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (item : Ast.scope_use_item Pos.marked) : unit =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  match Pos.unmark item with
  | Ast.Rule _ | Ast.Definition _ -> (
      let def_key, exception_to =
        match Pos.unmark item with
        | Ast.Rule rule ->
            ( Name_resolution.get_def_key (Pos.unmark rule.rule_name) rule.rule_state scope ctxt
                (Pos.get_position rule.rule_name),
              rule.rule_exception_to )
        | Ast.Definition def ->
            ( Name_resolution.get_def_key (Pos.unmark def.definition_name) def.definition_state
                scope ctxt
                (Pos.get_position def.definition_name),
              def.definition_exception_to )
        | _ -> assert false
        (* should not happen *)
      in
      let scope_def_ctxt = Desugared.Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts in
      match exception_to with
      | Ast.NotAnException | Ast.ExceptionToLabel _ -> ()
      (* If this is an unlabeled exception, we check that it has a unique default definition *)
      | Ast.UnlabeledException -> (
          match scope_def_ctxt.default_exception_rulename with
          | None ->
              Errors.raise_spanned_error "This exception does not have a corresponding definition"
                (Pos.get_position item)
          | Some (Ambiguous pos) ->
              Errors.raise_multispanned_error
                "This exception can refer to several definitions. Try using labels to disambiguate"
                ([ (Some "Ambiguous exception", Pos.get_position item) ]
                @ List.map (fun p -> (Some "Candidate definition", p)) pos)
          | Some (Unique _) -> ()))
  | _ -> ()

(** Translates a surface scope use, which is a bunch of definitions *)
let process_scope_use (ctxt : Name_resolution.context) (prgm : Desugared.Ast.program)
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
  List.fold_left (process_scope_use_item precond scope_uid ctxt) prgm use.scope_use_items

let attribute_to_io (attr : Ast.scope_decl_context_io) : Scopelang.Ast.io =
  {
    Scopelang.Ast.io_output = attr.scope_decl_context_io_output;
    Scopelang.Ast.io_input =
      Pos.map_under_mark
        (fun io ->
          match io with
          | Ast.Input -> Scopelang.Ast.OnlyInput
          | Ast.Internal -> Scopelang.Ast.NoInput
          | Ast.Context -> Scopelang.Ast.Reentrant)
        attr.scope_decl_context_io_input;
  }

(** Main function of this module *)
let desugar_program (ctxt : Name_resolution.context) (prgm : Ast.program) : Desugared.Ast.program =
  let empty_prgm =
    {
      Desugared.Ast.program_structs =
        Scopelang.Ast.StructMap.map Scopelang.Ast.StructFieldMap.bindings
          ctxt.Name_resolution.structs;
      Desugared.Ast.program_enums =
        Scopelang.Ast.EnumMap.map Scopelang.Ast.EnumConstructorMap.bindings
          ctxt.Name_resolution.enums;
      Desugared.Ast.program_scopes =
        Scopelang.Ast.ScopeMap.mapi
          (fun s_uid s_context ->
            {
              Desugared.Ast.scope_vars =
                Desugared.Ast.IdentMap.fold
                  (fun _ v acc ->
                    let v_sig = Desugared.Ast.ScopeVarMap.find v ctxt.var_typs in
                    match v_sig.var_sig_states_list with
                    | [] -> Desugared.Ast.ScopeVarMap.add v Desugared.Ast.WholeVar acc
                    | states -> Desugared.Ast.ScopeVarMap.add v (Desugared.Ast.States states) acc)
                  s_context.Name_resolution.var_idmap Desugared.Ast.ScopeVarMap.empty;
              Desugared.Ast.scope_sub_scopes = s_context.Name_resolution.sub_scopes;
              Desugared.Ast.scope_defs =
                (* Initializing the definitions of all scopes and subscope vars, with no rules yet
                   inside *)
                (let scope_vars_defs =
                   Desugared.Ast.IdentMap.fold
                     (fun _ v acc ->
                       let v_sig = Desugared.Ast.ScopeVarMap.find v ctxt.Name_resolution.var_typs in
                       match v_sig.var_sig_states_list with
                       | [] ->
                           let def_key = Desugared.Ast.ScopeDef.Var (v, None) in
                           Desugared.Ast.ScopeDefMap.add def_key
                             {
                               Desugared.Ast.scope_def_rules = Desugared.Ast.RuleMap.empty;
                               Desugared.Ast.scope_def_typ = v_sig.var_sig_typ;
                               Desugared.Ast.scope_def_label_groups =
                                 Name_resolution.label_groups ctxt s_uid def_key;
                               Desugared.Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
                               Desugared.Ast.scope_def_io = attribute_to_io v_sig.var_sig_io;
                             }
                             acc
                       | states ->
                           fst
                             (List.fold_left
                                (fun (acc, i) state ->
                                  let def_key = Desugared.Ast.ScopeDef.Var (v, Some state) in
                                  ( Desugared.Ast.ScopeDefMap.add def_key
                                      {
                                        Desugared.Ast.scope_def_rules = Desugared.Ast.RuleMap.empty;
                                        Desugared.Ast.scope_def_typ = v_sig.var_sig_typ;
                                        Desugared.Ast.scope_def_label_groups =
                                          Name_resolution.label_groups ctxt s_uid def_key;
                                        Desugared.Ast.scope_def_is_condition =
                                          v_sig.var_sig_is_condition;
                                        Desugared.Ast.scope_def_io =
                                          (* The first state should have the input I/O of the
                                             original variable, and the last state should have the
                                             output I/O of the original variable. All intermediate
                                             states shall have "internal" I/O.*)
                                          (let original_io = attribute_to_io v_sig.var_sig_io in
                                           let io_input =
                                             if i = 0 then original_io.io_input
                                             else
                                               ( Scopelang.Ast.NoInput,
                                                 Pos.get_position
                                                   (Desugared.Ast.StateName.get_info state) )
                                           in
                                           let io_output =
                                             if i = List.length states - 1 then
                                               original_io.io_output
                                             else
                                               ( false,
                                                 Pos.get_position
                                                   (Desugared.Ast.StateName.get_info state) )
                                           in
                                           { io_input; io_output });
                                      }
                                      acc,
                                    i + 1 ))
                                (acc, 0) states))
                     s_context.Name_resolution.var_idmap Desugared.Ast.ScopeDefMap.empty
                 in
                 let scope_and_subscope_vars_defs =
                   Scopelang.Ast.SubScopeMap.fold
                     (fun subscope_name subscope_uid acc ->
                       Desugared.Ast.IdentMap.fold
                         (fun _ v acc ->
                           let v_sig =
                             Desugared.Ast.ScopeVarMap.find v ctxt.Name_resolution.var_typs
                           in
                           let def_key = Desugared.Ast.ScopeDef.SubScopeVar (subscope_name, v) in
                           Desugared.Ast.ScopeDefMap.add def_key
                             {
                               Desugared.Ast.scope_def_rules = Desugared.Ast.RuleMap.empty;
                               Desugared.Ast.scope_def_typ = v_sig.var_sig_typ;
                               Desugared.Ast.scope_def_label_groups =
                                 Name_resolution.label_groups ctxt subscope_uid def_key;
                               Desugared.Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
                               Desugared.Ast.scope_def_io = attribute_to_io v_sig.var_sig_io;
                             }
                             acc)
                         (Scopelang.Ast.ScopeMap.find subscope_uid ctxt.Name_resolution.scopes)
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
  let rec processer_structure (prgm : Desugared.Ast.program) (item : Ast.law_structure) :
      Desugared.Ast.program =
    match item with
    | LawHeading (_, children) ->
        List.fold_left (fun prgm child -> processer_structure prgm child) prgm children
    | CodeBlock (block, _, _) ->
        List.fold_left
          (fun prgm item ->
            match Pos.unmark item with
            | Ast.ScopeUse use -> process_scope_use ctxt prgm use
            | _ -> prgm)
          prgm block
    | LawInclude _ | LawText _ -> prgm
  in
  List.fold_left processer_structure empty_prgm prgm.program_items
