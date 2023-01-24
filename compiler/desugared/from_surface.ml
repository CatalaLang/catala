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

open Catala_utils
module S = Surface.Ast
module SurfacePrint = Surface.Print
open Shared_ast
module Runtime = Runtime_ocaml.Runtime

(** Translation from {!module: Surface.Ast} to {!module: Desugaring.Ast}.

    - Removes syntactic sugars
    - Separate code from legislation *)

(** {1 Translating expressions} *)

(* Resolves the operator kinds into the expected operator operand types.

   This gives only partial typing information, in the case it is enforced using
   the operator suffixes for explicit typing. See {!modules:
   Shared_ast.Operator} for detail. *)

let translate_binop : Surface.Ast.binop -> Pos.t -> Ast.expr boxed =
 fun op pos ->
  let op_expr op tys =
    Expr.eop op (List.map (Marked.mark pos) tys) (Untyped { pos })
  in
  match op with
  | S.And -> op_expr And [TLit TBool; TLit TBool]
  | S.Or -> op_expr Or [TLit TBool; TLit TBool]
  | S.Xor -> op_expr Xor [TLit TBool; TLit TBool]
  | S.Add k ->
    op_expr Add
      (match k with
      | S.KPoly -> [TAny; TAny]
      | S.KInt -> [TLit TInt; TLit TInt]
      | S.KDec -> [TLit TRat; TLit TRat]
      | S.KMoney -> [TLit TMoney; TLit TMoney]
      | S.KDate -> [TLit TDate; TLit TDuration]
      | S.KDuration -> [TLit TDuration; TLit TDuration])
  | S.Sub k ->
    op_expr Sub
      (match k with
      | S.KPoly -> [TAny; TAny]
      | S.KInt -> [TLit TInt; TLit TInt]
      | S.KDec -> [TLit TRat; TLit TRat]
      | S.KMoney -> [TLit TMoney; TLit TMoney]
      | S.KDate -> [TLit TDate; TLit TDate]
      | S.KDuration -> [TLit TDuration; TLit TDuration])
  | S.Mult k ->
    op_expr Mult
      (match k with
      | S.KPoly -> [TAny; TAny]
      | S.KInt -> [TLit TInt; TLit TInt]
      | S.KDec -> [TLit TRat; TLit TRat]
      | S.KMoney -> [TLit TMoney; TLit TRat]
      | S.KDate ->
        Errors.raise_spanned_error pos
          "This operator doesn't exist, dates can't be multiplied"
      | S.KDuration -> [TLit TDuration; TLit TInt])
  | S.Div k ->
    op_expr Div
      (match k with
      | S.KPoly -> [TAny; TAny]
      | S.KInt -> [TLit TInt; TLit TInt]
      | S.KDec -> [TLit TRat; TLit TRat]
      | S.KMoney -> [TLit TMoney; TLit TMoney]
      | S.KDate ->
        Errors.raise_spanned_error pos
          "This operator doesn't exist, dates can't be divided"
      | S.KDuration -> [TLit TDuration; TLit TDuration])
  | S.Lt k | S.Lte k | S.Gt k | S.Gte k ->
    op_expr
      (match op with
      | S.Lt _ -> Lt
      | S.Lte _ -> Lte
      | S.Gt _ -> Gt
      | S.Gte _ -> Gte
      | _ -> assert false)
      (match k with
      | S.KPoly -> [TAny; TAny]
      | S.KInt -> [TLit TInt; TLit TInt]
      | S.KDec -> [TLit TRat; TLit TRat]
      | S.KMoney -> [TLit TMoney; TLit TMoney]
      | S.KDate -> [TLit TDate; TLit TDate]
      | S.KDuration -> [TLit TDuration; TLit TDuration])
  | S.Eq ->
    op_expr Eq [TAny; TAny]
    (* This is a truly polymorphic operator, not an overload *)
  | S.Neq -> assert false (* desugared already *)
  | S.Concat -> op_expr Concat [TArray (TAny, pos); TArray (TAny, pos)]

let translate_unop (op : Surface.Ast.unop) pos : Ast.expr boxed =
  let op_expr op ty = Expr.eop op [Marked.mark pos ty] (Untyped { pos }) in
  match op with
  | S.Not -> op_expr Not (TLit TBool)
  | S.Minus k ->
    op_expr Minus
      (match k with
      | S.KPoly -> TAny
      | S.KInt -> TLit TInt
      | S.KDec -> TLit TRat
      | S.KMoney -> TLit TMoney
      | S.KDate ->
        Errors.raise_spanned_error pos
          "This operator doesn't exist, dates can't be negative"
      | S.KDuration -> TLit TDuration)

let disambiguate_constructor
    (ctxt : Name_resolution.context)
    (constructor : (S.path * S.uident Marked.pos) Marked.pos list)
    (pos : Pos.t) : EnumName.t * EnumConstructor.t =
  let path, constructor =
    match constructor with
    | [c] -> Marked.unmark c
    | _ ->
      Errors.raise_spanned_error pos
        "The deep pattern matching syntactic sugar is not yet supported"
  in
  let possible_c_uids =
    try IdentName.Map.find (Marked.unmark constructor) ctxt.constructor_idmap
    with Not_found ->
      Errors.raise_spanned_error
        (Marked.get_mark constructor)
        "The name of this constructor has not been defined before, maybe it is \
         a typo?"
  in
  match path with
  | [] ->
    if EnumName.Map.cardinal possible_c_uids > 1 then
      Errors.raise_spanned_error
        (Marked.get_mark constructor)
        "This constructor name is ambiguous, it can belong to %a. Disambiguate \
         it by prefixing it with the enum name."
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
           (fun fmt (s_name, _) ->
             Format.fprintf fmt "%a" EnumName.format_t s_name))
        (EnumName.Map.bindings possible_c_uids);
    EnumName.Map.choose possible_c_uids
  | [enum] -> (
    try
      (* The path is fully qualified *)
      let e_uid = Name_resolution.get_enum ctxt enum in
      try
        let c_uid = EnumName.Map.find e_uid possible_c_uids in
        e_uid, c_uid
      with Not_found ->
        Errors.raise_spanned_error pos "Enum %s does not contain case %s"
          (Marked.unmark enum)
          (Marked.unmark constructor)
    with Not_found ->
      Errors.raise_spanned_error (Marked.get_mark enum)
        "Enum %s has not been defined before" (Marked.unmark enum))
  | _ -> Errors.raise_spanned_error pos "Qualified paths are not supported yet"

let int100 = Runtime.integer_of_int 100
let rat100 = Runtime.decimal_of_integer int100

(** The parser allows any combination of logical operators with right
    associativity. We actually want to reject anything that mixes operators
    without parens, so that is handled here. *)
let rec check_formula (op, pos_op) e =
  match Marked.unmark e with
  | S.Binop ((((S.And | S.Or | S.Xor) as op1), pos_op1), e1, e2) ->
    if op = S.Xor || op <> op1 then
      (* Xor is mathematically associative, but without a useful semantics ([a
         xor b xor c] is most likely an error since it's true for [a = b = c =
         true]) *)
      Errors.raise_multispanned_error
        [None, pos_op; None, pos_op1]
        "Please add parentheses to explicit which of these operators should be \
         applied first";
    check_formula (op1, pos_op1) e1;
    check_formula (op1, pos_op1) e2
  | _ -> ()

(** Usage: [translate_expr scope ctxt naked_expr]

    Translates [expr] into its desugared equivalent. [scope] is used to
    disambiguate the scope and subscopes variables than occur in the
    expression, [None] is assumed to mean a toplevel definition *)
let rec translate_expr
    (scope : ScopeName.t option)
    (inside_definition_of : Ast.ScopeDef.t Marked.pos option)
    (ctxt : Name_resolution.context)
    (expr : Surface.Ast.expression) : Ast.expr boxed =
  let scope_vars = match scope with
    | None -> IdentName.Map.empty
    | Some s -> (ScopeName.Map.find s ctxt.scopes).var_idmap
  in
  let rec_helper = translate_expr scope inside_definition_of ctxt in
  let pos = Marked.get_mark expr in
  let emark = Untyped { pos } in
  match Marked.unmark expr with
  | Paren e -> rec_helper e
  | Binop
      ( (Surface.Ast.And, _pos_op),
        ( TestMatchCase (e1_sub, ((constructors, Some binding), pos_pattern)),
          _pos_e1 ),
        e2 ) ->
    (* This sugar corresponds to [e is P x && e'] and should desugar to [match e
       with P x -> e' | _ -> false] *)
    let enum_uid, c_uid =
      disambiguate_constructor ctxt constructors pos_pattern
    in
    let cases =
      EnumConstructor.Map.mapi
        (fun c_uid' tau ->
          if EnumConstructor.compare c_uid c_uid' <> 0 then
            let nop_var = Var.make "_" in
            Expr.make_abs [| nop_var |]
              (Expr.elit (LBool false) emark)
              [tau] pos
          else
            let ctxt, binding_var =
              Name_resolution.add_def_local_var ctxt (Marked.unmark binding)
            in
            let e2 = translate_expr scope inside_definition_of ctxt e2 in
            Expr.make_abs [| binding_var |] e2 [tau] pos)
        (EnumName.Map.find enum_uid ctxt.enums)
    in
    Expr.ematch
      (translate_expr scope inside_definition_of ctxt e1_sub)
      enum_uid cases emark
  | Binop ((((S.And | S.Or | S.Xor), _) as op), e1, e2) ->
    check_formula op e1;
    check_formula op e2;
    let op_term = translate_binop (Marked.unmark op) (Marked.get_mark op) in
    Expr.eapp op_term [rec_helper e1; rec_helper e2] emark
  | IfThenElse (e_if, e_then, e_else) ->
    Expr.eifthenelse (rec_helper e_if) (rec_helper e_then) (rec_helper e_else)
      emark
  | Binop ((S.Neq, posn), e1, e2) ->
    (* Neq is just sugar *)
    rec_helper (Unop ((S.Not, posn), (Binop ((S.Eq, posn), e1, e2), posn)), pos)
  | Binop ((op, pos), e1, e2) ->
    let op_term = translate_binop op pos in
    Expr.eapp op_term [rec_helper e1; rec_helper e2] emark
  | Unop ((op, pos), e) ->
    let op_term = translate_unop op pos in
    Expr.eapp op_term [rec_helper e] emark
  | Literal l ->
    let lit =
      match l with
      | LNumber ((Int i, _), None) -> LInt (Runtime.integer_of_string i)
      | LNumber ((Int i, _), Some (Percent, _)) ->
        LRat Runtime.(Oper.o_div_rat_rat (decimal_of_string i) rat100)
      | LNumber ((Dec (i, f), _), None) ->
        LRat Runtime.(decimal_of_string (i ^ "." ^ f))
      | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
        LRat
          Runtime.(Oper.o_div_rat_rat (decimal_of_string (i ^ "." ^ f)) rat100)
      | LBool b -> LBool b
      | LMoneyAmount i ->
        LMoney
          Runtime.(
            money_of_cents_integer
              (Oper.o_add_int_int
                 (Oper.o_mult_int_int
                    (integer_of_string i.money_amount_units)
                    int100)
                 (integer_of_string i.money_amount_cents)))
      | LNumber ((Int i, _), Some (Year, _)) ->
        LDuration (Runtime.duration_of_numbers (int_of_string i) 0 0)
      | LNumber ((Int i, _), Some (Month, _)) ->
        LDuration (Runtime.duration_of_numbers 0 (int_of_string i) 0)
      | LNumber ((Int i, _), Some (Day, _)) ->
        LDuration (Runtime.duration_of_numbers 0 0 (int_of_string i))
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
        LDate
          (try
             Runtime.date_of_numbers date.literal_date_year
               date.literal_date_month date.literal_date_day
           with Runtime.ImpossibleDate ->
             Errors.raise_spanned_error pos
               "There is an error in this date, it does not correspond to a \
                correct calendar day")
    in
    Expr.elit lit emark
  | Ident ([], (x, pos)) -> (
    (* first we check whether this is a local var, then we resort to scope-wide
       variables, then global variables *)
    match IdentName.Map.find_opt x ctxt.local_var_idmap with
    | Some uid ->
      Expr.make_var uid emark
      (* the whole box thing is to accomodate for this case *)
    | None -> (
      match IdentName.Map.find_opt x scope_vars with
      | Some (ScopeVar uid) ->
        (* If the referenced variable has states, then here are the rules to
           desambiguate. In general, only the last state can be referenced.
           Except if defining a state of the same variable, then it references
           the previous state in the chain. *)
        let x_sig = ScopeVar.Map.find uid ctxt.var_typs in
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
        Expr.elocation (DesugaredScopeVar ((uid, pos), x_state)) emark
      | Some (SubScope _)
      (* Note: allowing access to a global variable with the same name as a
         subscope is disputable, but I see no good reason to forbid it either *)
      | None -> (
          match IdentName.Map.find_opt x ctxt.topdefs with
          | Some v ->
            Expr.elocation (GlobalVar (v, Marked.get_mark (TopdefName.get_info v))) emark
          | None ->
            Name_resolution.raise_unknown_identifier
              "for a local, scope-wide or global variable" (x, pos))
    ))
  | Ident (_path, _x) ->
    Errors.raise_spanned_error pos "Qualified paths are not supported yet"
  | Dotted (e, ((path, x), _ppos)) -> (
    match path, Marked.unmark e with
      | [], Ident ([], (y, _)) when Option.fold scope ~none:false ~some:(fun s -> Name_resolution.is_subscope_uid s ctxt y)
      ->
      (* In this case, y.x is a subscope variable *)
      let subscope_uid, subscope_real_uid =
        match IdentName.Map.find y scope_vars with
        | SubScope (sub, sc) -> sub, sc
        | ScopeVar _ -> assert false
      in
      let subscope_var_uid =
        Name_resolution.get_var_uid subscope_real_uid ctxt x
      in
      Expr.elocation
        (SubScopeVar
           (subscope_real_uid, (subscope_uid, pos), (subscope_var_uid, pos)))
        emark
    | _ ->
      (* In this case e.x is the struct field x access of expression e *)
      let e = translate_expr scope inside_definition_of ctxt e in
      let str =
        match path with
        | [] -> None
        | [c] -> (
          try Some (Name_resolution.get_struct ctxt c)
          with Not_found ->
            Errors.raise_spanned_error (Marked.get_mark c)
              "Structure %s was not declared" (Marked.unmark c))
        | _ ->
          Errors.raise_spanned_error pos "Qualified paths are not supported yet"
      in
      Expr.edstructaccess e (Marked.unmark x) str emark)
  | FunCall (f, arg) -> Expr.eapp (rec_helper f) [rec_helper arg] emark
  | ScopeCall ((([], sc_name), _), fields) ->
    if scope = None then
      Errors.raise_spanned_error pos "Scope calls are not allowed outside of a scope";
    let called_scope = Name_resolution.get_scope ctxt sc_name in
    let scope_def = ScopeName.Map.find called_scope ctxt.scopes in
    let in_struct =
      List.fold_left
        (fun acc (fld_id, e) ->
          let var =
            match
              IdentName.Map.find_opt (Marked.unmark fld_id) scope_def.var_idmap
            with
            | Some (ScopeVar v) -> v
            | Some (SubScope _) | None ->
              Errors.raise_multispanned_error
                [
                  None, Marked.get_mark fld_id;
                  ( Some
                      (Format.asprintf "Scope %a declared here"
                         ScopeName.format_t called_scope),
                    Marked.get_mark (ScopeName.get_info called_scope) );
                ]
                "Scope %a has no input variable %a" ScopeName.format_t
                called_scope Print.lit_style (Marked.unmark fld_id)
          in
          ScopeVar.Map.update var
            (function
              | None -> Some (rec_helper e)
              | Some _ ->
                Errors.raise_spanned_error (Marked.get_mark fld_id)
                  "Duplicate definition of scope input variable '%a'"
                  ScopeVar.format_t var)
            acc)
        ScopeVar.Map.empty fields
    in
    Expr.escopecall called_scope in_struct emark
  | ScopeCall (((_, _sc_name), _), _fields) ->
    Errors.raise_spanned_error pos "Qualified paths are not supported yet"
  | LetIn (x, e1, e2) ->
    let ctxt, v = Name_resolution.add_def_local_var ctxt (Marked.unmark x) in
    let tau = TAny, Marked.get_mark x in
    (* This type will be resolved in Scopelang.Desambiguation *)
    let fn =
      Expr.make_abs [| v |]
        (translate_expr scope inside_definition_of ctxt e2)
        [tau] pos
    in
    Expr.eapp fn [rec_helper e1] emark
  | StructLit ((([], s_name), _), fields) ->
    let s_uid =
      match IdentName.Map.find_opt (Marked.unmark s_name) ctxt.typedefs with
      | Some (Name_resolution.TStruct s_uid) -> s_uid
      | _ ->
        Errors.raise_spanned_error (Marked.get_mark s_name)
          "This identifier should refer to a struct name"
    in

    let s_fields =
      List.fold_left
        (fun s_fields (f_name, f_e) ->
          let f_uid =
            try
              StructName.Map.find s_uid
                (IdentName.Map.find (Marked.unmark f_name) ctxt.field_idmap)
            with Not_found ->
              Errors.raise_spanned_error (Marked.get_mark f_name)
                "This identifier should refer to a field of struct %s"
                (Marked.unmark s_name)
          in
          (match StructField.Map.find_opt f_uid s_fields with
          | None -> ()
          | Some e_field ->
            Errors.raise_multispanned_error
              [None, Marked.get_mark f_e; None, Expr.pos e_field]
              "The field %a has been defined twice:" StructField.format_t f_uid);
          let f_e = translate_expr scope inside_definition_of ctxt f_e in
          StructField.Map.add f_uid f_e s_fields)
        StructField.Map.empty fields
    in
    let expected_s_fields = StructName.Map.find s_uid ctxt.structs in
    StructField.Map.iter
      (fun expected_f _ ->
        if not (StructField.Map.mem expected_f s_fields) then
          Errors.raise_spanned_error pos
            "Missing field for structure %a: \"%a\"" StructName.format_t s_uid
            StructField.format_t expected_f)
      expected_s_fields;

    Expr.estruct s_uid s_fields emark
  | StructLit (((_, _s_name), _), _fields) ->
    Errors.raise_spanned_error pos "Qualified paths are not supported yet"
  | EnumInject (((path, (constructor, pos_constructor)), _), payload) -> (
    let possible_c_uids =
      try IdentName.Map.find constructor ctxt.constructor_idmap
      with Not_found ->
        Errors.raise_spanned_error pos_constructor
          "The name of this constructor has not been defined before, maybe it \
           is a typo?"
    in
    let mark_constructor = Untyped { pos = pos_constructor } in

    match path with
    | [] ->
      if
        (* No constructor name was specified *)
        EnumName.Map.cardinal possible_c_uids > 1
      then
        Errors.raise_spanned_error pos_constructor
          "This constructor name is ambiguous, it can belong to %a. \
           Desambiguate it by prefixing it with the enum name."
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
             (fun fmt (s_name, _) ->
               Format.fprintf fmt "%a" EnumName.format_t s_name))
          (EnumName.Map.bindings possible_c_uids)
      else
        let e_uid, c_uid = EnumName.Map.choose possible_c_uids in
        let payload =
          Option.map (translate_expr scope inside_definition_of ctxt) payload
        in
        Expr.einj
          (match payload with
          | Some e' -> e'
          | None -> Expr.elit LUnit mark_constructor)
          c_uid e_uid emark
    | [enum] -> (
      try
        (* The path has been fully qualified *)
        let e_uid = Name_resolution.get_enum ctxt enum in
        try
          let c_uid = EnumName.Map.find e_uid possible_c_uids in
          let payload =
            Option.map (translate_expr scope inside_definition_of ctxt) payload
          in
          Expr.einj
            (match payload with
            | Some e' -> e'
            | None -> Expr.elit LUnit mark_constructor)
            c_uid e_uid emark
        with Not_found ->
          Errors.raise_spanned_error pos "Enum %s does not contain case %s"
            (Marked.unmark enum) constructor
      with Not_found ->
        Errors.raise_spanned_error (Marked.get_mark enum)
          "Enum %s has not been defined before" (Marked.unmark enum))
    | _ ->
      Errors.raise_spanned_error pos "Qualified paths are not supported yet")
  | MatchWith (e1, (cases, _cases_pos)) ->
    let e1 = translate_expr scope inside_definition_of ctxt e1 in
    let cases_d, e_uid =
      disambiguate_match_and_build_expression scope inside_definition_of ctxt
        cases
    in
    Expr.ematch e1 e_uid cases_d emark
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
      EnumConstructor.Map.mapi
        (fun c_uid' tau ->
          let nop_var = Var.make "_" in
          Expr.make_abs [| nop_var |]
            (Expr.elit (LBool (EnumConstructor.compare c_uid c_uid' = 0)) emark)
            [tau] pos)
        (EnumName.Map.find enum_uid ctxt.enums)
    in
    Expr.ematch
      (translate_expr scope inside_definition_of ctxt e1)
      enum_uid cases emark
  | ArrayLit es -> Expr.earray (List.map rec_helper es) emark
  | CollectionOp (((S.Filter { f } | S.Map { f }) as op), collection) ->
    let collection = rec_helper collection in
    let param, predicate = f in
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param)
    in
    let f_pred =
      Expr.make_abs [| param |]
        (translate_expr scope inside_definition_of ctxt predicate)
        [TAny, pos]
        pos
    in
    Expr.eapp
      (Expr.eop
         (match op with
         | S.Map _ -> Map
         | S.Filter _ -> Filter
         | _ -> assert false)
         [TAny, pos; TAny, pos]
         emark)
      [f_pred; collection] emark
  | CollectionOp
      (S.AggregateArgExtremum { max; default; f = param, predicate }, collection)
    ->
    let default = rec_helper default in
    let pos_dft = Expr.pos default in
    let collection = rec_helper collection in
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param)
    in
    let cmp_op = if max then Op.Gt else Op.Lt in
    let f_pred =
      Expr.make_abs [| param |]
        (translate_expr scope inside_definition_of ctxt predicate)
        [TAny, pos]
        pos
    in
    let param_name = Bindlib.name_of param in
    let v1, v2 = Var.make (param_name ^ "_1"), Var.make (param_name ^ "_2") in
    let x1 = Expr.make_var v1 emark in
    let x2 = Expr.make_var v2 emark in
    let reduce_f =
      (* fun x1 x2 -> cmp_op (pred x1) (pred x2) *)
      (* Note: this computes f_pred twice on every element, but we'd rather not
         rely on returning tuples here *)
      Expr.make_abs [| v1; v2 |]
        (Expr.eifthenelse
           (Expr.eapp
              (Expr.eop cmp_op
                 [TAny, pos_dft; TAny, pos_dft]
                 (Untyped { pos = pos_dft }))
              [Expr.eapp f_pred [x1] emark; Expr.eapp f_pred [x2] emark]
              emark)
           x1 x2 emark)
        [TAny, pos; TAny, pos]
        pos
    in
    Expr.eapp
      (Expr.eop Reduce [TAny, pos; TAny, pos; TAny, pos] emark)
      [reduce_f; default; collection]
      emark
  | CollectionOp
      (((Exists { predicate } | Forall { predicate }) as op), collection) ->
    let collection = rec_helper collection in
    let init, op =
      match op with
      | Exists _ -> false, S.Or
      | Forall _ -> true, S.And
      | _ -> assert false
    in
    let init = Expr.elit (LBool init) emark in
    let param0, predicate = predicate in
    let ctxt, param =
      Name_resolution.add_def_local_var ctxt (Marked.unmark param0)
    in
    let f =
      let acc_var = Var.make "acc" in
      let acc =
        Expr.make_var acc_var (Untyped { pos = Marked.get_mark param0 })
      in
      Expr.eabs
        (Expr.bind [| acc_var; param |]
           (Expr.eapp (translate_binop op pos)
              [acc; translate_expr scope inside_definition_of ctxt predicate]
              emark))
        [TAny, pos; TAny, pos]
        emark
    in
    Expr.eapp
      (Expr.eop Fold [TAny, pos; TAny, pos; TAny, pos] emark)
      [f; init; collection] emark
  | CollectionOp (AggregateExtremum { max; default }, collection) ->
    let collection = rec_helper collection in
    let default = translate_expr scope inside_definition_of ctxt default in
    let op = translate_binop (if max then S.Gt KPoly else S.Lt KPoly) pos in
    let op_f =
      (* fun x1 x2 -> if op x1 x2 then x1 else x2 *)
      let vname = if max then "max" else "min" in
      let v1, v2 = Var.make (vname ^ "1"), Var.make (vname ^ "2") in
      let x1 = Expr.make_var v1 emark in
      let x2 = Expr.make_var v2 emark in
      Expr.make_abs [| v1; v2 |]
        (Expr.eifthenelse (Expr.eapp op [x1; x2] emark) x1 x2 emark)
        [TAny, pos; TAny, pos]
        pos
    in
    Expr.eapp
      (Expr.eop Reduce [TAny, pos; TAny, pos; TAny, pos] emark)
      [op_f; default; collection]
      emark
  | CollectionOp (AggregateSum { typ }, collection) ->
    let collection = rec_helper collection in
    let default_lit =
      let i0 = Runtime.integer_of_int 0 in
      match typ with
      | S.Integer -> LInt i0
      | S.Decimal -> LRat (Runtime.decimal_of_integer i0)
      | S.Money -> LMoney (Runtime.money_of_cents_integer i0)
      | S.Duration -> LDuration (Runtime.duration_of_numbers 0 0 0)
      | t ->
        Errors.raise_spanned_error pos
          "It is impossible to sum values of type %a together"
          SurfacePrint.format_primitive_typ t
    in
    let op_f =
      (* fun x1 x2 -> op x1 x2 *)
      (* we're not allowed pass the operator directly as argument, it must
         appear inside an [EApp] *)
      let v1, v2 = Var.make "sum1", Var.make "sum2" in
      let x1 = Expr.make_var v1 emark in
      let x2 = Expr.make_var v2 emark in
      Expr.make_abs [| v1; v2 |]
        (Expr.eapp (translate_binop (S.Add KPoly) pos) [x1; x2] emark)
        [TAny, pos; TAny, pos]
        pos
    in
    Expr.eapp
      (Expr.eop Reduce [TAny, pos; TAny, pos; TAny, pos] emark)
      [op_f; Expr.elit default_lit emark; collection]
      emark
  | MemCollection (member, collection) ->
    let param_var = Var.make "collection_member" in
    let param = Expr.make_var param_var emark in
    let collection = rec_helper collection in
    let init = Expr.elit (LBool false) emark in
    let acc_var = Var.make "acc" in
    let acc = Expr.make_var acc_var emark in
    let f_body =
      let member = translate_expr scope inside_definition_of ctxt member in
      Expr.eapp
        (Expr.eop Or [TLit TBool, pos; TLit TBool, pos] emark)
        [
          Expr.eapp
            (Expr.eop Eq [TAny, pos; TAny, pos] emark)
            [member; param] emark;
          acc;
        ]
        emark
    in
    let f =
      Expr.eabs
        (Expr.bind [| acc_var; param_var |] f_body)
        [TLit TBool, pos; TAny, pos]
        emark
    in
    Expr.eapp
      (Expr.eop Fold [TAny, pos; TAny, pos; TAny, pos] emark)
      [f; init; collection] emark
  | Builtin ToDecimal -> Expr.eop ToRat [TAny, pos] emark
  | Builtin ToMoney -> Expr.eop ToMoney [TAny, pos] emark
  | Builtin Round -> Expr.eop Round [TAny, pos] emark
  | Builtin Cardinal -> Expr.eop Length [TArray (TAny, pos), pos] emark
  | Builtin GetDay -> Expr.eop GetDay [TLit TDate, pos] emark
  | Builtin GetMonth -> Expr.eop GetMonth [TLit TDate, pos] emark
  | Builtin GetYear -> Expr.eop GetYear [TLit TDate, pos] emark
  | Builtin FirstDayOfMonth -> Expr.eop FirstDayOfMonth [TLit TDate, pos] emark
  | Builtin LastDayOfMonth -> Expr.eop LastDayOfMonth [TLit TDate, pos] emark

and disambiguate_match_and_build_expression
    (scope : ScopeName.t option)
    (inside_definition_of : Ast.ScopeDef.t Marked.pos option)
    (ctxt : Name_resolution.context)
    (cases : Surface.Ast.match_case Marked.pos list) :
    Ast.expr boxed EnumConstructor.Map.t * EnumName.t =
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
      case_body
      e_binder =
    Expr.eabs e_binder
      [
        EnumConstructor.Map.find c_uid
          (EnumName.Map.find e_uid ctxt.Name_resolution.enums);
      ]
      (Marked.get_mark case_body)
  in
  let bind_match_cases (cases_d, e_uid, curr_index) (case, case_pos) =
    match case with
    | Surface.Ast.MatchCase case ->
      let constructor, binding =
        Marked.unmark case.Surface.Ast.match_case_pattern
      in
      let e_uid', c_uid =
        disambiguate_constructor ctxt constructor
          (Marked.get_mark case.Surface.Ast.match_case_pattern)
      in
      let e_uid =
        match e_uid with
        | None -> e_uid'
        | Some e_uid ->
          if e_uid = e_uid' then e_uid
          else
            Errors.raise_spanned_error
              (Marked.get_mark case.Surface.Ast.match_case_pattern)
              "This case matches a constructor of enumeration %a but previous \
               case were matching constructors of enumeration %a"
              EnumName.format_t e_uid EnumName.format_t e_uid'
      in
      (match EnumConstructor.Map.find_opt c_uid cases_d with
      | None -> ()
      | Some e_case ->
        Errors.raise_multispanned_error
          [None, Marked.get_mark case.match_case_expr; None, Expr.pos e_case]
          "The constructor %a has been matched twice:" EnumConstructor.format_t
          c_uid);
      let ctxt, param_var = create_var (Option.map Marked.unmark binding) in
      let case_body =
        translate_expr scope inside_definition_of ctxt
          case.Surface.Ast.match_case_expr
      in
      let e_binder = Expr.bind [| param_var |] case_body in
      let case_expr = bind_case_body c_uid e_uid ctxt case_body e_binder in
      ( EnumConstructor.Map.add c_uid case_expr cases_d,
        Some e_uid,
        curr_index + 1 )
    | Surface.Ast.WildCard match_case_expr -> (
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
          EnumName.Map.find e_uid ctxt.Name_resolution.enums
          |> EnumConstructor.Map.filter_map (fun c_uid _ ->
                 match EnumConstructor.Map.find_opt c_uid cases_d with
                 | Some _ -> None
                 | None -> Some c_uid)
        in
        if EnumConstructor.Map.is_empty missing_constructors then
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
        let e_binder = Expr.bind [| payload_var |] case_body in

        (* For each missing cases, binds the wildcard payload. *)
        EnumConstructor.Map.fold
          (fun c_uid _ (cases_d, e_uid_opt, curr_index) ->
            let case_expr =
              bind_case_body c_uid e_uid ctxt case_body e_binder
            in
            ( EnumConstructor.Map.add c_uid case_expr cases_d,
              e_uid_opt,
              curr_index + 1 ))
          missing_constructors
          (cases_d, Some e_uid, curr_index))
  in
  let naked_expr, e_name, _ =
    List.fold_left bind_match_cases (EnumConstructor.Map.empty, None, 0) cases
  in
  naked_expr, Option.get e_name
  [@@ocamlformat "wrap-comments=false"]

(** {1 Translating scope definitions} *)

(** A scope use can be annotated with a pervasive precondition, in which case
    this precondition has to be appended to the justifications of each
    definition in the subscope use. This is what this function does. *)
let merge_conditions
    (precond : Ast.expr boxed option)
    (cond : Ast.expr boxed option)
    (default_pos : Pos.t) : Ast.expr boxed =
  match precond, cond with
  | Some precond, Some cond ->
    let op_term =
      Expr.eop And
        [TLit TBool, default_pos; TLit TBool, default_pos]
        (Marked.get_mark cond)
    in
    Expr.eapp op_term [precond; cond] (Marked.get_mark cond)
  | Some precond, None -> Marked.unmark precond, Untyped { pos = default_pos }
  | None, Some cond -> cond
  | None, None -> Expr.elit (LBool true) (Untyped { pos = default_pos })

(** Translates a surface definition into condition into a desugared {!type:
    Ast.rule} *)
let process_default
    (ctxt : Name_resolution.context)
    (scope : ScopeName.t)
    (def_key : Ast.ScopeDef.t Marked.pos)
    (rule_id : RuleName.t)
    (param_uid : Ast.expr Var.t Marked.pos option)
    (precond : Ast.expr boxed option)
    (exception_situation : Ast.exception_situation)
    (label_situation : Ast.label_situation)
    (just : Surface.Ast.expression option)
    (cons : Surface.Ast.expression) : Ast.rule =
  let just =
    match just with
    | Some just -> Some (translate_expr (Some scope) (Some def_key) ctxt just)
    | None -> None
  in
  let just = merge_conditions precond just (Marked.get_mark def_key) in
  let cons = translate_expr (Some scope) (Some def_key) ctxt cons in
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
         Errors.raise_spanned_error (Expr.pos cons)
           "This definition has a function type but the parameter is missing"
       | _, Some _ ->
         Errors.raise_spanned_error (Expr.pos cons)
           "This definition has a parameter but its type is not a function"
       | _ -> None);
    rule_exception = exception_situation;
    rule_id;
    rule_label = label_situation;
  }

(** Wrapper around {!val: process_default} that performs some name
    disambiguation *)
let process_def
    (precond : Ast.expr boxed option)
    (scope_uid : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (def : Surface.Ast.definition) : Ast.program =
  let scope : Ast.scope = ScopeName.Map.find scope_uid prgm.program_scopes in
  let scope_ctxt = ScopeName.Map.find scope_uid ctxt.scopes in
  let def_key =
    Name_resolution.get_def_key
      (Marked.unmark def.definition_name)
      def.definition_state scope_uid ctxt
      (Marked.get_mark def.definition_name)
  in
  let scope_def_ctxt =
    Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts
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
    let scope_def = Ast.ScopeDefMap.find def_key scope.scope_defs in
    let rule_name = def.definition_id in
    let label_situation =
      match def.definition_label with
      | Some (label_str, label_pos) ->
        Ast.ExplicitlyLabeled
          (IdentName.Map.find label_str scope_def_ctxt.label_idmap, label_pos)
      | None -> Ast.Unlabeled
    in
    let exception_situation =
      match def.Surface.Ast.definition_exception_to with
      | NotAnException -> Ast.BaseCase
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
            IdentName.Map.find (Marked.unmark label_str)
              scope_def_ctxt.label_idmap
          in
          ExceptionToLabel (label_id, Marked.get_mark label_str)
        with Not_found ->
          Errors.raise_spanned_error
            (Marked.get_mark label_str)
            "Unknown label for the scope variable %a: \"%s\""
            Ast.ScopeDef.format_t def_key (Marked.unmark label_str))
    in
    let scope_def =
      {
        scope_def with
        scope_def_rules =
          RuleName.Map.add rule_name
            (process_default new_ctxt scope_uid
               (def_key, Marked.get_mark def.definition_name)
               rule_name param_uid precond exception_situation label_situation
               def.definition_condition def.definition_expr)
            scope_def.scope_def_rules;
      }
    in
    {
      scope with
      scope_defs = Ast.ScopeDefMap.add def_key scope_def scope.scope_defs;
    }
  in
  {
    prgm with
    program_scopes =
      ScopeName.Map.add scope_uid scope_updated prgm.program_scopes;
  }

(** Translates a {!type: Surface.Ast.rule} from the surface language *)
let process_rule
    (precond : Ast.expr boxed option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (rule : Surface.Ast.rule) : Ast.program =
  let def = Surface.Ast.rule_to_def rule in
  process_def precond scope ctxt prgm def

(** Translates assertions *)
let process_assert
    (precond : Ast.expr boxed option)
    (scope_uid : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (ass : Surface.Ast.assertion) : Ast.program =
  let scope : Ast.scope = ScopeName.Map.find scope_uid prgm.program_scopes in
  let ass =
    translate_expr (Some scope_uid) None ctxt
      (match ass.Surface.Ast.assertion_condition with
      | None -> ass.Surface.Ast.assertion_content
      | Some cond ->
        ( Surface.Ast.IfThenElse
            ( cond,
              ass.Surface.Ast.assertion_content,
              Marked.same_mark_as (Surface.Ast.Literal (Surface.Ast.LBool true))
                cond ),
          Marked.get_mark cond ))
  in
  let ass =
    match precond with
    | Some precond ->
      Expr.eifthenelse precond ass
        (Expr.elit (LBool true) (Marked.get_mark precond))
        (Marked.get_mark precond)
    | None -> ass
  in
  let new_scope =
    { scope with scope_assertions = ass :: scope.scope_assertions }
  in
  {
    prgm with
    program_scopes = ScopeName.Map.add scope_uid new_scope prgm.program_scopes;
  }

(** Translates a surface definition, rule or assertion *)
let process_scope_use_item
    (precond : Surface.Ast.expression option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (item : Surface.Ast.scope_use_item Marked.pos) : Ast.program =
  let precond = Option.map (translate_expr (Some scope) None ctxt) precond in
  match Marked.unmark item with
  | Surface.Ast.Rule rule -> process_rule precond scope ctxt prgm rule
  | Surface.Ast.Definition def -> process_def precond scope ctxt prgm def
  | Surface.Ast.Assertion ass -> process_assert precond scope ctxt prgm ass
  | _ -> prgm

(** {1 Translating top-level items} *)

(* If this is an unlabeled exception, ensures that it has a unique default
   definition *)
let check_unlabeled_exception
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (item : Surface.Ast.scope_use_item Marked.pos) : unit =
  let scope_ctxt = ScopeName.Map.find scope ctxt.scopes in
  match Marked.unmark item with
  | Surface.Ast.Rule _ | Surface.Ast.Definition _ -> (
    let def_key, exception_to =
      match Marked.unmark item with
      | Surface.Ast.Rule rule ->
        ( Name_resolution.get_def_key
            (Marked.unmark rule.rule_name)
            rule.rule_state scope ctxt
            (Marked.get_mark rule.rule_name),
          rule.rule_exception_to )
      | Surface.Ast.Definition def ->
        ( Name_resolution.get_def_key
            (Marked.unmark def.definition_name)
            def.definition_state scope ctxt
            (Marked.get_mark def.definition_name),
          def.definition_exception_to )
      | _ -> assert false
      (* should not happen *)
    in
    let scope_def_ctxt =
      Ast.ScopeDefMap.find def_key scope_ctxt.scope_defs_contexts
    in
    match exception_to with
    | Surface.Ast.NotAnException | Surface.Ast.ExceptionToLabel _ -> ()
    (* If this is an unlabeled exception, we check that it has a unique default
       definition *)
    | Surface.Ast.UnlabeledException -> (
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
    (prgm : Ast.program)
    (use : Surface.Ast.scope_use) : Ast.program =
  let scope_uid = Name_resolution.get_scope ctxt use.scope_use_name in
  (* Make sure the scope exists *)
  let prgm =
    match ScopeName.Map.find_opt scope_uid prgm.program_scopes with
    | Some _ -> prgm
    | None -> assert false
    (* should not happen *)
  in
  let precond = use.scope_use_condition in
  List.iter (check_unlabeled_exception scope_uid ctxt) use.scope_use_items;
  List.fold_left
    (process_scope_use_item precond scope_uid ctxt)
    prgm use.scope_use_items

let process_topdef ctxt prgm def =
  let id = IdentName.Map.find (Marked.unmark def.S.topdef_name) ctxt.Name_resolution.topdefs in
  let ty_pos = Marked.get_mark def.S.topdef_type in
  let translate_typ t =
    (* Todo: better helper function from a more appropriate place *)
    Name_resolution.process_base_typ ctxt (S.Data (Marked.unmark t), Marked.get_mark t)
  in
  let body_type = translate_typ def.S.topdef_type in
  let arg_types = List.map (fun (_, ty) -> translate_typ ty) def.S.topdef_args in
  let expr =
    let ctxt, rv_args =
      List.fold_left (fun (ctxt, rv_args) (v, _ty) ->
          let ctxt, a = Name_resolution.add_def_local_var ctxt (Marked.unmark v) in
          ctxt, a :: rv_args)
        (ctxt, []) def.S.topdef_args
    in
    let body = translate_expr None None ctxt def.S.topdef_expr in
    match def.S.topdef_args with
    | [] -> body
    | args -> (* FIXME: hmm where do we stand on arg tuplification ? *)
      Expr.make_abs (Array.of_list (List.rev rv_args)) body arg_types (Marked.get_mark def.S.topdef_name)
  in
  let typ =
    List.fold_right (fun argty retty -> TArrow (argty, retty), ty_pos) arg_types body_type
  in
  { prgm with Ast.program_globals =
                TopdefName.Map.add id (Expr.unbox expr, typ)
                  prgm.Ast.program_globals }


let attribute_to_io (attr : Surface.Ast.scope_decl_context_io) : Ast.io =
  {
    Ast.io_output = attr.scope_decl_context_io_output;
    Ast.io_input =
      Marked.map_under_mark
        (fun io ->
          match io with
          | Surface.Ast.Input -> Ast.OnlyInput
          | Surface.Ast.Internal -> Ast.NoInput
          | Surface.Ast.Context -> Ast.Reentrant)
        attr.scope_decl_context_io_input;
  }

let init_scope_defs
    (ctxt : Name_resolution.context)
    (scope_idmap : Name_resolution.scope_var_or_subscope IdentName.Map.t) :
    Ast.scope_def Ast.ScopeDefMap.t =
  (* Initializing the definitions of all scopes and subscope vars, with no rules
     yet inside *)
  let add_def _ v scope_def_map =
    match v with
    | Name_resolution.ScopeVar v -> (
      let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
      match v_sig.var_sig_states_list with
      | [] ->
        let def_key = Ast.ScopeDef.Var (v, None) in
        Ast.ScopeDefMap.add def_key
          {
            Ast.scope_def_rules = RuleName.Map.empty;
            Ast.scope_def_typ = v_sig.var_sig_typ;
            Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
            Ast.scope_def_io = attribute_to_io v_sig.var_sig_io;
          }
          scope_def_map
      | states ->
        let scope_def, _ =
          List.fold_left
            (fun (acc, i) state ->
              let def_key = Ast.ScopeDef.Var (v, Some state) in
              let def =
                {
                  Ast.scope_def_rules = RuleName.Map.empty;
                  Ast.scope_def_typ = v_sig.var_sig_typ;
                  Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
                  Ast.scope_def_io =
                    (* The first state should have the input I/O of the original
                       variable, and the last state should have the output I/O
                       of the original variable. All intermediate states shall
                       have "internal" I/O.*)
                    (let original_io = attribute_to_io v_sig.var_sig_io in
                     let io_input =
                       if i = 0 then original_io.io_input
                       else
                         Ast.NoInput, Marked.get_mark (StateName.get_info state)
                     in
                     let io_output =
                       if i = List.length states - 1 then original_io.io_output
                       else false, Marked.get_mark (StateName.get_info state)
                     in
                     { io_input; io_output });
                }
              in
              Ast.ScopeDefMap.add def_key def acc, i + 1)
            (scope_def_map, 0) states
        in
        scope_def)
    | Name_resolution.SubScope (v0, subscope_uid) ->
      let sub_scope_def =
        ScopeName.Map.find subscope_uid ctxt.Name_resolution.scopes
      in
      IdentName.Map.fold
        (fun _ v scope_def_map ->
          match v with
          | Name_resolution.SubScope _ -> scope_def_map
          | Name_resolution.ScopeVar v ->
            (* TODO: shouldn't we ignore internal variables too at this point
               ? *)
            let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
            let def_key =
              Ast.ScopeDef.SubScopeVar
                (v0, v, Marked.get_mark (ScopeVar.get_info v))
            in
            Ast.ScopeDefMap.add def_key
              {
                Ast.scope_def_rules = RuleName.Map.empty;
                Ast.scope_def_typ = v_sig.var_sig_typ;
                Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
                Ast.scope_def_io = attribute_to_io v_sig.var_sig_io;
              }
              scope_def_map)
        sub_scope_def.Name_resolution.var_idmap scope_def_map
  in
  IdentName.Map.fold add_def scope_idmap Ast.ScopeDefMap.empty (* TODO: add topdefs too *)

(** Main function of this module *)
let translate_program
    (ctxt : Name_resolution.context)
    (prgm : Surface.Ast.program) : Ast.program =
  let empty_prgm =
    let program_scopes =
      ScopeName.Map.mapi
        (fun s_uid s_context ->
          let scope_vars =
            IdentName.Map.fold
              (fun _ v acc ->
                match v with
                | Name_resolution.SubScope _ -> acc
                | Name_resolution.ScopeVar v -> (
                  let v_sig = ScopeVar.Map.find v ctxt.var_typs in
                  match v_sig.var_sig_states_list with
                  | [] -> ScopeVar.Map.add v Ast.WholeVar acc
                  | states -> ScopeVar.Map.add v (Ast.States states) acc))
              s_context.Name_resolution.var_idmap ScopeVar.Map.empty
          in
          let scope_sub_scopes =
            IdentName.Map.fold
              (fun _ v acc ->
                match v with
                | Name_resolution.ScopeVar _ -> acc
                | Name_resolution.SubScope (sub_var, sub_scope) ->
                  SubScopeName.Map.add sub_var sub_scope acc)
              s_context.Name_resolution.var_idmap SubScopeName.Map.empty
          in
          {
            Ast.scope_vars;
            scope_sub_scopes;
            scope_defs = init_scope_defs ctxt s_context.var_idmap;
            scope_assertions = [];
            scope_meta_assertions = [];
            scope_uid = s_uid;
          })
        ctxt.Name_resolution.scopes
    in
    {
      Ast.program_ctx =
        {
          ctx_structs = ctxt.Name_resolution.structs;
          ctx_enums = ctxt.Name_resolution.enums;
          ctx_scopes =
            IdentName.Map.fold
              (fun _ def acc ->
                match def with
                | Name_resolution.TScope (scope, scope_out_struct) ->
                  ScopeName.Map.add scope scope_out_struct acc
                | _ -> acc)
              ctxt.Name_resolution.typedefs ScopeName.Map.empty;
          ctx_struct_fields = ctxt.Name_resolution.field_idmap;
        };
      Ast.program_globals = TopdefName.Map.empty;
      Ast.program_scopes;
    }
  in
  let rec processer_structure
      (prgm : Ast.program)
      (item : Surface.Ast.law_structure) : Ast.program =
    match item with
    | LawHeading (_, children) ->
      List.fold_left
        (fun prgm child -> processer_structure prgm child)
        prgm children
    | CodeBlock (block, _, _) ->
      List.fold_left
        (fun prgm item ->
          match Marked.unmark item with
          | Surface.Ast.ScopeUse use -> process_scope_use ctxt prgm use
          | Surface.Ast.Topdef def -> process_topdef ctxt prgm def
          | Surface.Ast.ScopeDecl _ | Surface.Ast.StructDecl _ | Surface.Ast.EnumDecl _ -> prgm)
        prgm block
    | LawInclude _ | LawText _ -> prgm
  in
  List.fold_left processer_structure empty_prgm prgm.program_items
