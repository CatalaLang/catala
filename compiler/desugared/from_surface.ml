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

let translate_binop : S.binop -> Pos.t -> Ast.expr boxed =
 fun op pos ->
  let op_expr op tys =
    Expr.eop op (List.map (Mark.add pos) tys) (Untyped { pos })
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
        Message.raise_spanned_error pos
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
        Message.raise_spanned_error pos
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

let translate_unop (op : S.unop) pos : Ast.expr boxed =
  let op_expr op ty = Expr.eop op [Mark.add pos ty] (Untyped { pos }) in
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
        Message.raise_spanned_error pos
          "This operator doesn't exist, dates can't be negative"
      | S.KDuration -> TLit TDuration)

let raise_error_cons_not_found
    (ctxt : Name_resolution.context)
    (constructor : string Mark.pos) =
  let constructors = Ident.Map.keys ctxt.constructor_idmap in
  let closest_constructors =
    Suggestions.suggestion_minimum_levenshtein_distance_association constructors
      (Mark.remove constructor)
  in
  Message.raise_spanned_error
    ~span_msg:(fun ppf -> Format.fprintf ppf "Here is your code :")
    ~suggestion:closest_constructors (Mark.get constructor)
    "The name of this constructor has not been defined before@ (it's probably \
     a typographical error)."

let rec disambiguate_constructor
    (ctxt : Name_resolution.context)
    (constructor0 : (S.path * S.uident Mark.pos) Mark.pos list)
    (pos : Pos.t) : EnumName.t * EnumConstructor.t =
  let path, constructor =
    match constructor0 with
    | [c] -> Mark.remove c
    | _ ->
      Message.raise_spanned_error pos
        "The deep pattern matching syntactic sugar is not yet supported"
  in
  let possible_c_uids =
    try Ident.Map.find (Mark.remove constructor) ctxt.constructor_idmap
    with Ident.Map.Not_found _ -> raise_error_cons_not_found ctxt constructor
  in
  match path with
  | [] ->
    if EnumName.Map.cardinal possible_c_uids > 1 then
      Message.raise_spanned_error (Mark.get constructor)
        "This constructor name is ambiguous, it can belong to %a. Disambiguate \
         it by prefixing it with the enum name."
        (EnumName.Map.format_keys ~pp_sep:(fun fmt () ->
             Format.pp_print_string fmt " or "))
        possible_c_uids;
    EnumName.Map.choose possible_c_uids
  | [enum] -> (
    (* The path is fully qualified *)
    let e_uid = Name_resolution.get_enum ctxt enum in
    try
      let c_uid = EnumName.Map.find e_uid possible_c_uids in
      e_uid, c_uid
    with EnumName.Map.Not_found _ ->
      Message.raise_spanned_error pos "Enum %s does not contain case %s"
        (Mark.remove enum) (Mark.remove constructor))
  | (modname, mpos) :: path -> (
    let modname = ModuleName.of_string modname in
    match ModuleName.Map.find_opt modname ctxt.modules with
    | None ->
      Message.raise_spanned_error mpos "Module %a not found" ModuleName.format
        modname
    | Some ctxt ->
      let constructor =
        List.map (Mark.map (fun (_, c) -> path, c)) constructor0
      in
      disambiguate_constructor ctxt constructor pos)

let int100 = Runtime.integer_of_int 100
let rat100 = Runtime.decimal_of_integer int100

(** The parser allows any combination of logical operators with right
    associativity. We actually want to reject anything that mixes operators
    without parens, so that is handled here. *)
let rec check_formula (op, pos_op) e =
  match Mark.remove e with
  | S.Binop ((((S.And | S.Or | S.Xor) as op1), pos_op1), e1, e2) ->
    if op = S.Xor || op <> op1 then
      (* Xor is mathematically associative, but without a useful semantics ([a
         xor b xor c] is most likely an error since it's true for [a = b = c =
         true]) *)
      Message.raise_multispanned_error
        [None, pos_op; None, pos_op1]
        "Please add parentheses to explicit which of these operators should be \
         applied first";
    check_formula (op1, pos_op1) e1;
    check_formula (op1, pos_op1) e2
  | _ -> ()

(** Usage: [translate_expr scope ctxt naked_expr]

    Translates [expr] into its desugared equivalent. [scope] is used to
    disambiguate the scope and subscopes variables than occur in the expression,
    [None] is assumed to mean a toplevel definition *)
let rec translate_expr
    (scope : ScopeName.t option)
    (inside_definition_of : Ast.ScopeDef.t Mark.pos option)
    (ctxt : Name_resolution.context)
    (local_vars : Ast.expr Var.t Ident.Map.t)
    (expr : S.expression) : Ast.expr boxed =
  let scope_vars =
    match scope with
    | None -> Ident.Map.empty
    | Some s -> (ScopeName.Map.find s ctxt.scopes).var_idmap
  in
  let rec_helper ?(local_vars = local_vars) e =
    translate_expr scope inside_definition_of ctxt local_vars e
  in
  let pos = Mark.get expr in
  let emark = Untyped { pos } in
  match Mark.remove expr with
  | Paren e -> rec_helper e
  | Binop
      ( (S.And, _pos_op),
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
            let binding_var = Var.make (Mark.remove binding) in
            let local_vars =
              Ident.Map.add (Mark.remove binding) binding_var local_vars
            in
            let e2 = rec_helper ~local_vars e2 in
            Expr.make_abs [| binding_var |] e2 [tau] pos)
        (EnumName.Map.find enum_uid ctxt.enums)
    in
    Expr.ematch ~e:(rec_helper e1_sub) ~name:enum_uid ~cases emark
  | Binop ((((S.And | S.Or | S.Xor), _) as op), e1, e2) ->
    check_formula op e1;
    check_formula op e2;
    let op_term = translate_binop (Mark.remove op) (Mark.get op) in
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
        Message.raise_spanned_error pos
          "Impossible to specify decimal amounts of days, months or years"
      | LDate date ->
        if date.literal_date_month > 12 then
          Message.raise_spanned_error pos
            "There is an error in this date: the month number is bigger than 12";
        if date.literal_date_day > 31 then
          Message.raise_spanned_error pos
            "There is an error in this date: the day number is bigger than 31";
        LDate
          (try
             Runtime.date_of_numbers date.literal_date_year
               date.literal_date_month date.literal_date_day
           with Runtime.ImpossibleDate ->
             Message.raise_spanned_error pos
               "There is an error in this date, it does not correspond to a \
                correct calendar day")
    in
    Expr.elit lit emark
  | Ident ([], (x, pos)) -> (
    (* first we check whether this is a local var, then we resort to scope-wide
       variables, then global variables *)
    match Ident.Map.find_opt x local_vars with
    | Some uid ->
      Expr.make_var uid emark
      (* the whole box thing is to accomodate for this case *)
    | None -> (
      match Ident.Map.find_opt x scope_vars with
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
                  Message.raise_spanned_error pos
                    "It is impossible to refer to the variable you are \
                     defining when defining its first state."
                else
                  (* Tricky: we have to retrieve in the list the previous state
                     with respect to the state that we are defining. *)
                  let rec find_prev_state = function
                    | [] -> None
                    | st0 :: st1 :: _ when StateName.equal inside_def_state st1
                      ->
                      Some st0
                    | _ :: states -> find_prev_state states
                  in
                  find_prev_state states)
            | _ ->
              (* we take the last state in the chain *)
              Some (List.hd (List.rev states)))
        in
        Expr.elocation
          (DesugaredScopeVar { name = uid, pos; state = x_state })
          emark
      | Some (SubScope _)
      (* Note: allowing access to a global variable with the same name as a
         subscope is disputable, but I see no good reason to forbid it either *)
      | None -> (
        match Ident.Map.find_opt x ctxt.topdefs with
        | Some v ->
          Expr.elocation
            (ToplevelVar { name = v, Mark.get (TopdefName.get_info v) })
            emark
        | None ->
          Name_resolution.raise_unknown_identifier
            "for a local, scope-wide or global variable" (x, pos))))
  | Ident (path, name) -> (
    let ctxt = Name_resolution.module_ctx ctxt path in
    match Ident.Map.find_opt (Mark.remove name) ctxt.topdefs with
    | Some v ->
      Expr.elocation
        (ToplevelVar { name = v, Mark.get (TopdefName.get_info v) })
        emark
    | None ->
      Name_resolution.raise_unknown_identifier "for an external variable" name)
  | Dotted (e, ((path, x), _ppos)) -> (
    match path, Mark.remove e with
    | [], Ident ([], (y, _))
      when Option.fold scope ~none:false ~some:(fun s ->
               Name_resolution.is_subscope_uid s ctxt y) ->
      (* In this case, y.x is a subscope variable *)
      let subscope_uid, subscope_real_uid =
        match Ident.Map.find y scope_vars with
        | SubScope (sub, sc) -> sub, sc
        | ScopeVar _ -> assert false
      in
      let subscope_var_uid =
        Name_resolution.get_var_uid subscope_real_uid ctxt x
      in
      Expr.elocation
        (SubScopeVar
           {
             scope = subscope_real_uid;
             alias = subscope_uid, pos;
             var = subscope_var_uid, pos;
           })
        emark
    | _ ->
      (* In this case e.x is the struct field x access of expression e *)
      let e = rec_helper e in
      let rec get_str ctxt = function
        | [] -> None
        | [c] -> Some (Name_resolution.get_struct ctxt c)
        | (modname, mpos) :: path -> (
          let modname = ModuleName.of_string modname in
          match ModuleName.Map.find_opt modname ctxt.modules with
          | None ->
            Message.raise_spanned_error mpos "Module %a not found"
              ModuleName.format modname
          | Some ctxt -> get_str ctxt path)
      in
      Expr.edstructaccess ~e ~field:(Mark.remove x)
        ~name_opt:(get_str ctxt path) emark)
  | FunCall (f, args) ->
    Expr.eapp (rec_helper f) (List.map rec_helper args) emark
  | ScopeCall (((path, id), _), fields) ->
    if scope = None then
      Message.raise_spanned_error pos
        "Scope calls are not allowed outside of a scope";
    let called_scope, scope_def =
      let ctxt = Name_resolution.module_ctx ctxt path in
      let uid = Name_resolution.get_scope ctxt id in
      uid, ScopeName.Map.find uid ctxt.scopes
    in
    let in_struct =
      List.fold_left
        (fun acc (fld_id, e) ->
          let var =
            match
              Ident.Map.find_opt (Mark.remove fld_id) scope_def.var_idmap
            with
            | Some (ScopeVar v) -> v
            | Some (SubScope _) | None ->
              Message.raise_multispanned_error
                [
                  None, Mark.get fld_id;
                  ( Some
                      (Format.asprintf "Scope %a declared here" ScopeName.format
                         called_scope),
                    Mark.get (ScopeName.get_info called_scope) );
                ]
                "Scope %a has no input variable %a" ScopeName.format
                called_scope Print.lit_style (Mark.remove fld_id)
          in
          ScopeVar.Map.update var
            (function
              | None -> Some (rec_helper e)
              | Some _ ->
                Message.raise_spanned_error (Mark.get fld_id)
                  "Duplicate definition of scope input variable '%a'"
                  ScopeVar.format var)
            acc)
        ScopeVar.Map.empty fields
    in
    Expr.escopecall ~scope:called_scope ~args:in_struct emark
  | LetIn (x, e1, e2) ->
    let v = Var.make (Mark.remove x) in
    let local_vars = Ident.Map.add (Mark.remove x) v local_vars in
    let tau = TAny, Mark.get x in
    (* This type will be resolved in Scopelang.Desambiguation *)
    let fn = Expr.make_abs [| v |] (rec_helper ~local_vars e2) [tau] pos in
    Expr.eapp fn [rec_helper e1] emark
  | StructLit ((([], s_name), _), fields) ->
    let s_uid =
      match Ident.Map.find_opt (Mark.remove s_name) ctxt.typedefs with
      | Some (Name_resolution.TStruct s_uid) -> s_uid
      | _ ->
        Message.raise_spanned_error (Mark.get s_name)
          "This identifier should refer to a struct name"
    in

    let s_fields =
      List.fold_left
        (fun s_fields (f_name, f_e) ->
          let f_uid =
            try
              StructName.Map.find s_uid
                (Ident.Map.find (Mark.remove f_name) ctxt.field_idmap)
            with StructName.Map.Not_found _ | Ident.Map.Not_found _ ->
              Message.raise_spanned_error (Mark.get f_name)
                "This identifier should refer to a field of struct %s"
                (Mark.remove s_name)
          in
          (match StructField.Map.find_opt f_uid s_fields with
          | None -> ()
          | Some e_field ->
            Message.raise_multispanned_error
              [None, Mark.get f_e; None, Expr.pos e_field]
              "The field %a has been defined twice:" StructField.format f_uid);
          let f_e = rec_helper f_e in
          StructField.Map.add f_uid f_e s_fields)
        StructField.Map.empty fields
    in
    let expected_s_fields = StructName.Map.find s_uid ctxt.structs in
    StructField.Map.iter
      (fun expected_f _ ->
        if not (StructField.Map.mem expected_f s_fields) then
          Message.raise_spanned_error pos
            "Missing field for structure %a: \"%a\"" StructName.format s_uid
            StructField.format expected_f)
      expected_s_fields;

    Expr.estruct ~name:s_uid ~fields:s_fields emark
  | StructLit (((_, _s_name), _), _fields) ->
    Message.raise_spanned_error pos "Qualified paths are not supported yet"
  | EnumInject (((path, (constructor, pos_constructor)), _), payload) -> (
    let get_possible_c_uids ctxt =
      try Ident.Map.find constructor ctxt.Name_resolution.constructor_idmap
      with Ident.Map.Not_found _ ->
        raise_error_cons_not_found ctxt (constructor, pos_constructor)
    in
    let mark_constructor = Untyped { pos = pos_constructor } in
    match path with
    | [] ->
      let possible_c_uids = get_possible_c_uids ctxt in
      if
        (* No enum name was specified *)
        EnumName.Map.cardinal possible_c_uids > 1
      then
        Message.raise_spanned_error pos_constructor
          "This constructor name is ambiguous, it can belong to %a. \
           Desambiguate it by prefixing it with the enum name."
          (EnumName.Map.format_keys ~pp_sep:(fun fmt () ->
               Format.fprintf fmt " or "))
          possible_c_uids
      else
        let e_uid, c_uid = EnumName.Map.choose possible_c_uids in
        let payload = Option.map rec_helper payload in
        Expr.einj
          ~e:
            (match payload with
            | Some e' -> e'
            | None -> Expr.elit LUnit mark_constructor)
          ~cons:c_uid ~name:e_uid emark
    | path_enum -> (
      let path, enum =
        match List.rev path_enum with
        | enum :: rpath -> List.rev rpath, enum
        | _ -> assert false
      in
      let ctxt = Name_resolution.module_ctx ctxt path in
      let possible_c_uids = get_possible_c_uids ctxt in
      (* The path has been qualified *)
      let e_uid = Name_resolution.get_enum ctxt enum in
      try
        let c_uid = EnumName.Map.find e_uid possible_c_uids in
        let payload = Option.map rec_helper payload in
        Expr.einj
          ~e:
            (match payload with
            | Some e' -> e'
            | None -> Expr.elit LUnit mark_constructor)
          ~cons:c_uid ~name:e_uid emark
      with EnumName.Map.Not_found _ ->
        Message.raise_spanned_error pos "Enum %s does not contain case %s"
          (Mark.remove enum) constructor))
  | MatchWith (e1, (cases, _cases_pos)) ->
    let e1 = rec_helper e1 in
    let cases_d, e_uid =
      disambiguate_match_and_build_expression scope inside_definition_of ctxt
        local_vars cases
    in
    Expr.ematch ~e:e1 ~name:e_uid ~cases:cases_d emark
  | TestMatchCase (e1, pattern) ->
    (match snd (Mark.remove pattern) with
    | None -> ()
    | Some binding ->
      Message.emit_spanned_warning (Mark.get binding)
        "This binding will be ignored (remove it to suppress warning)");
    let enum_uid, c_uid =
      disambiguate_constructor ctxt
        (fst (Mark.remove pattern))
        (Mark.get pattern)
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
    Expr.ematch ~e:(rec_helper e1) ~name:enum_uid ~cases emark
  | ArrayLit es -> Expr.earray (List.map rec_helper es) emark
  | CollectionOp (((S.Filter { f } | S.Map { f }) as op), collection) ->
    let collection = rec_helper collection in
    let param_name, predicate = f in
    let param = Var.make (Mark.remove param_name) in
    let local_vars = Ident.Map.add (Mark.remove param_name) param local_vars in
    let f_pred =
      Expr.make_abs [| param |]
        (rec_helper ~local_vars predicate)
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
      ( S.AggregateArgExtremum { max; default; f = param_name, predicate },
        collection ) ->
    let default = rec_helper default in
    let pos_dft = Expr.pos default in
    let collection = rec_helper collection in
    let param = Var.make (Mark.remove param_name) in
    let local_vars = Ident.Map.add (Mark.remove param_name) param local_vars in
    let cmp_op = if max then Op.Gt else Op.Lt in
    let f_pred =
      Expr.make_abs [| param |]
        (rec_helper ~local_vars predicate)
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
    let param = Var.make (Mark.remove param0) in
    let local_vars = Ident.Map.add (Mark.remove param0) param local_vars in
    let f =
      let acc_var = Var.make "acc" in
      let acc = Expr.make_var acc_var (Untyped { pos = Mark.get param0 }) in
      Expr.eabs
        (Expr.bind [| acc_var; param |]
           (Expr.eapp (translate_binop op pos)
              [acc; rec_helper ~local_vars predicate]
              emark))
        [TAny, pos; TAny, pos]
        emark
    in
    Expr.eapp
      (Expr.eop Fold [TAny, pos; TAny, pos; TAny, pos] emark)
      [f; init; collection] emark
  | CollectionOp (AggregateExtremum { max; default }, collection) ->
    let collection = rec_helper collection in
    let default = rec_helper default in
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
        Message.raise_spanned_error pos
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
      let member = rec_helper member in
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
    (inside_definition_of : Ast.ScopeDef.t Mark.pos option)
    (ctxt : Name_resolution.context)
    (local_vars : Ast.expr Var.t Ident.Map.t)
    (cases : S.match_case Mark.pos list) :
    Ast.expr boxed EnumConstructor.Map.t * EnumName.t =
  let create_var local_vars = function
    | None -> local_vars, Var.make "_"
    | Some param ->
      let param_var = Var.make param in
      Ident.Map.add param param_var local_vars, param_var
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
      (Mark.get case_body)
  in
  let bind_match_cases (cases_d, e_uid, curr_index) (case, case_pos) =
    match case with
    | S.MatchCase case ->
      let constructor, binding = Mark.remove case.S.match_case_pattern in
      let e_uid', c_uid =
        disambiguate_constructor ctxt constructor
          (Mark.get case.S.match_case_pattern)
      in
      let e_uid =
        match e_uid with
        | None -> e_uid'
        | Some e_uid ->
          if e_uid = e_uid' then e_uid
          else
            Message.raise_spanned_error
              (Mark.get case.S.match_case_pattern)
              "This case matches a constructor of enumeration %a but previous \
               case were matching constructors of enumeration %a"
              EnumName.format e_uid EnumName.format e_uid'
      in
      (match EnumConstructor.Map.find_opt c_uid cases_d with
      | None -> ()
      | Some e_case ->
        Message.raise_multispanned_error
          [None, Mark.get case.match_case_expr; None, Expr.pos e_case]
          "The constructor %a has been matched twice:" EnumConstructor.format
          c_uid);
      let local_vars, param_var =
        create_var local_vars (Option.map Mark.remove binding)
      in
      let case_body =
        translate_expr scope inside_definition_of ctxt local_vars
          case.S.match_case_expr
      in
      let e_binder = Expr.bind [| param_var |] case_body in
      let case_expr = bind_case_body c_uid e_uid ctxt case_body e_binder in
      ( EnumConstructor.Map.add c_uid case_expr cases_d,
        Some e_uid,
        curr_index + 1 )
    | S.WildCard match_case_expr -> (
      let nb_cases = List.length cases in
      let raise_wildcard_not_last_case_err () =
        Message.raise_multispanned_error
          [
            Some "Not ending wildcard:", case_pos;
            ( Some "Next reachable case:",
              curr_index + 1 |> List.nth cases |> Mark.get );
          ]
          "Wildcard must be the last match case"
      in
      match e_uid with
      | None ->
        if 1 = nb_cases then
          Message.raise_spanned_error case_pos
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
          Message.emit_spanned_warning case_pos
            "Unreachable match case, all constructors of the enumeration %a \
             are already specified"
            EnumName.format e_uid;
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
        let local_vars, payload_var = create_var local_vars None in
        let case_body =
          translate_expr scope inside_definition_of ctxt local_vars
            match_case_expr
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
        (Mark.get cond)
    in
    Expr.eapp op_term [precond; cond] (Mark.get cond)
  | Some precond, None -> Mark.remove precond, Untyped { pos = default_pos }
  | None, Some cond -> cond
  | None, None -> Expr.elit (LBool true) (Untyped { pos = default_pos })

let rec arglist_eq_check pos_decl pos_def pdecl pdefs =
  match pdecl, pdefs with
  | [], [] -> ()
  | [], (arg, apos) :: _ ->
    Message.raise_multispanned_error
      [Some "Declared here:", pos_decl; Some "Extra argument:", apos]
      "This definition has an extra, undeclared argument '%a'" Print.lit_style
      arg
  | (arg, apos) :: _, [] ->
    Message.raise_multispanned_error
      [
        Some "Argument declared here:", apos;
        Some "Mismatching definition:", pos_def;
      ]
      "This definition is missing argument '%a'" Print.lit_style arg
  | decl :: pdecl, def :: pdefs when Uid.MarkedString.equal decl def ->
    arglist_eq_check pos_decl pos_def pdecl pdefs
  | (decl_arg, decl_apos) :: _, (def_arg, def_apos) :: _ ->
    Message.raise_multispanned_error
      [
        Some "Argument declared here:", decl_apos; Some "Defined here:", def_apos;
      ]
      "Function argument name mismatch between declaration ('%a') and \
       definition ('%a')"
      Print.lit_style decl_arg Print.lit_style def_arg

let process_rule_parameters
    ctxt
    (def_key : Ast.ScopeDef.t Mark.pos)
    (def : S.definition) :
    Ast.expr Var.t Ident.Map.t
    * (Ast.expr Var.t Mark.pos * typ) list Mark.pos option =
  let decl_name, decl_pos = def_key in
  let declared_params = Name_resolution.get_params ctxt decl_name in
  match declared_params, def.S.definition_parameter with
  | None, None -> Ident.Map.empty, None
  | None, Some (_, pos) ->
    Message.raise_multispanned_error
      [
        Some "Declared here without arguments", decl_pos;
        Some "Unexpected arguments appearing here", pos;
      ]
      "Extra arguments in this definition of %a" Ast.ScopeDef.format decl_name
  | Some (_, pos), None ->
    Message.raise_multispanned_error
      [
        Some "Arguments declared here", pos;
        Some "Definition missing the arguments", Mark.get def.S.definition_name;
      ]
      "This definition for %a is missing the arguments" Ast.ScopeDef.format
      decl_name
  | Some (pdecl, pos_decl), Some (pdefs, pos_def) ->
    arglist_eq_check pos_decl pos_def (List.map fst pdecl) pdefs;
    let local_vars, params =
      List.fold_left_map
        (fun local_vars ((lbl, pos), ty) ->
          let v = Var.make lbl in
          let local_vars = Ident.Map.add lbl v local_vars in
          local_vars, ((v, pos), ty))
        Ident.Map.empty pdecl
    in
    local_vars, Some (params, pos_def)

(** Translates a surface definition into condition into a desugared {!type:
    Ast.rule} *)
let process_default
    (ctxt : Name_resolution.context)
    (local_vars : Ast.expr Var.t Ident.Map.t)
    (scope : ScopeName.t)
    (def_key : Ast.ScopeDef.t Mark.pos)
    (rule_id : RuleName.t)
    (params : (Ast.expr Var.t Mark.pos * typ) list Mark.pos option)
    (precond : Ast.expr boxed option)
    (exception_situation : Ast.exception_situation)
    (label_situation : Ast.label_situation)
    (just : S.expression option)
    (cons : S.expression) : Ast.rule =
  let just =
    match just with
    | Some just ->
      Some (translate_expr (Some scope) (Some def_key) ctxt local_vars just)
    | None -> None
  in
  let just = merge_conditions precond just (Mark.get def_key) in
  let cons = translate_expr (Some scope) (Some def_key) ctxt local_vars cons in
  {
    Ast.rule_just = just;
    rule_cons = cons;
    rule_parameter = params;
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
    (def : S.definition) : Ast.program =
  let scope : Ast.scope = ScopeName.Map.find scope_uid prgm.program_scopes in
  let scope_ctxt = ScopeName.Map.find scope_uid ctxt.scopes in
  let def_key =
    Name_resolution.get_def_key
      (Mark.remove def.definition_name)
      def.definition_state scope_uid ctxt
      (Mark.get def.definition_name)
  in
  let scope_def_ctxt =
    Ast.ScopeDef.Map.find def_key scope_ctxt.scope_defs_contexts
  in
  (* We add to the name resolution context the name of the parameter variable *)
  let local_vars, param_uids =
    process_rule_parameters ctxt (Mark.copy def.definition_name def_key) def
  in
  let scope_updated =
    let scope_def = Ast.ScopeDef.Map.find def_key scope.scope_defs in
    let rule_name = def.definition_id in
    let label_situation =
      match def.definition_label with
      | Some (label_str, label_pos) ->
        Ast.ExplicitlyLabeled
          (Ident.Map.find label_str scope_def_ctxt.label_idmap, label_pos)
      | None -> Ast.Unlabeled
    in
    let exception_situation =
      match def.S.definition_exception_to with
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
            Ident.Map.find (Mark.remove label_str) scope_def_ctxt.label_idmap
          in
          ExceptionToLabel (label_id, Mark.get label_str)
        with Ident.Map.Not_found _ ->
          Message.raise_spanned_error (Mark.get label_str)
            "Unknown label for the scope variable %a: \"%s\""
            Ast.ScopeDef.format def_key (Mark.remove label_str))
    in
    let scope_def =
      {
        scope_def with
        scope_def_rules =
          RuleName.Map.add rule_name
            (process_default ctxt local_vars scope_uid
               (def_key, Mark.get def.definition_name)
               rule_name param_uids precond exception_situation label_situation
               def.definition_condition def.definition_expr)
            scope_def.scope_def_rules;
      }
    in
    {
      scope with
      scope_defs = Ast.ScopeDef.Map.add def_key scope_def scope.scope_defs;
    }
  in
  {
    prgm with
    program_scopes =
      ScopeName.Map.add scope_uid scope_updated prgm.program_scopes;
  }

(** Translates a {!type: S.rule} from the surface language *)
let process_rule
    (precond : Ast.expr boxed option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (rule : S.rule) : Ast.program =
  let def = S.rule_to_def rule in
  process_def precond scope ctxt prgm def

(** Translates assertions *)
let process_assert
    (precond : Ast.expr boxed option)
    (scope_uid : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (ass : S.assertion) : Ast.program =
  let scope : Ast.scope = ScopeName.Map.find scope_uid prgm.program_scopes in
  let ass =
    translate_expr (Some scope_uid) None ctxt Ident.Map.empty
      (match ass.S.assertion_condition with
      | None -> ass.S.assertion_content
      | Some cond ->
        ( S.IfThenElse
            ( cond,
              ass.S.assertion_content,
              Mark.copy cond (S.Literal (S.LBool true)) ),
          Mark.get cond ))
  in
  let assertion =
    match precond with
    | Some precond ->
      Expr.eifthenelse precond ass
        (Expr.elit (LBool true) (Mark.get precond))
        (Mark.get precond)
    | None -> ass
  in
  (* The assertion name is not very relevant and should not be used in error
     messages, it is only a reference to designate the assertion instead of its
     expression. *)
  let assertion_name = Ast.AssertionName.fresh ("assert", Expr.pos assertion) in
  let new_scope =
    {
      scope with
      scope_assertions =
        Ast.AssertionName.Map.add assertion_name assertion
          scope.scope_assertions;
    }
  in
  {
    prgm with
    program_scopes = ScopeName.Map.add scope_uid new_scope prgm.program_scopes;
  }

(** Translates a surface definition, rule or assertion *)
let process_scope_use_item
    (precond : S.expression option)
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (item : S.scope_use_item Mark.pos) : Ast.program =
  let precond =
    Option.map (translate_expr (Some scope) None ctxt Ident.Map.empty) precond
  in
  match Mark.remove item with
  | S.Rule rule -> process_rule precond scope ctxt prgm rule
  | S.Definition def -> process_def precond scope ctxt prgm def
  | S.Assertion ass -> process_assert precond scope ctxt prgm ass
  | S.DateRounding (r, _) ->
    let scope_uid = scope in
    let scope : Ast.scope = ScopeName.Map.find scope_uid prgm.program_scopes in
    let r =
      match r with
      | S.Increasing -> Ast.Increasing
      | S.Decreasing -> Ast.Decreasing
    in
    let new_scope =
      match
        List.find_opt
          (fun (scope_opt, _) ->
            scope_opt = Ast.DateRounding Ast.Increasing
            || scope_opt = Ast.DateRounding Ast.Decreasing)
          scope.scope_options
      with
      | Some (_, old_pos) ->
        Message.raise_multispanned_error
          [None, old_pos; None, Mark.get item]
          "You cannot set multiple date rounding modes"
      | None ->
        {
          scope with
          scope_options =
            Mark.copy item (Ast.DateRounding r) :: scope.scope_options;
        }
    in
    {
      prgm with
      program_scopes = ScopeName.Map.add scope_uid new_scope prgm.program_scopes;
    }
  | _ -> prgm

(** {1 Translating top-level items} *)

(* If this is an unlabeled exception, ensures that it has a unique default
   definition *)
let check_unlabeled_exception
    (scope : ScopeName.t)
    (ctxt : Name_resolution.context)
    (item : S.scope_use_item Mark.pos) : unit =
  let scope_ctxt = ScopeName.Map.find scope ctxt.scopes in
  match Mark.remove item with
  | S.Rule _ | S.Definition _ -> (
    let def_key, exception_to =
      match Mark.remove item with
      | S.Rule rule ->
        ( Name_resolution.get_def_key
            (Mark.remove rule.rule_name)
            rule.rule_state scope ctxt (Mark.get rule.rule_name),
          rule.rule_exception_to )
      | S.Definition def ->
        ( Name_resolution.get_def_key
            (Mark.remove def.definition_name)
            def.definition_state scope ctxt
            (Mark.get def.definition_name),
          def.definition_exception_to )
      | _ -> assert false
      (* should not happen *)
    in
    let scope_def_ctxt =
      Ast.ScopeDef.Map.find def_key scope_ctxt.scope_defs_contexts
    in
    match exception_to with
    | S.NotAnException | S.ExceptionToLabel _ -> ()
    (* If this is an unlabeled exception, we check that it has a unique default
       definition *)
    | S.UnlabeledException -> (
      match scope_def_ctxt.default_exception_rulename with
      | None ->
        Message.raise_spanned_error (Mark.get item)
          "This exception does not have a corresponding definition"
      | Some (Ambiguous pos) ->
        Message.raise_multispanned_error
          ([Some "Ambiguous exception", Mark.get item]
          @ List.map (fun p -> Some "Candidate definition", p) pos)
          "This exception can refer to several definitions. Try using labels \
           to disambiguate"
      | Some (Unique _) -> ()))
  | _ -> ()

(** Translates a surface scope use, which is a bunch of definitions *)
let process_scope_use
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (use : S.scope_use) : Ast.program =
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

let process_topdef
    (ctxt : Name_resolution.context)
    (prgm : Ast.program)
    (def : S.top_def) : Ast.program =
  let id =
    Ident.Map.find (Mark.remove def.S.topdef_name) ctxt.Name_resolution.topdefs
  in
  let translate_typ t = Name_resolution.process_type ctxt t in
  let translate_tbase (tbase, m) = translate_typ (Base tbase, m) in
  let typ = translate_typ def.S.topdef_type in
  let expr_opt =
    match def.S.topdef_expr, def.S.topdef_args with
    | None, _ -> None
    | Some e, None ->
      Some (Expr.unbox_closed (translate_expr None None ctxt Ident.Map.empty e))
    | Some e, Some (args, _) ->
      let local_vars, args_tys =
        List.fold_left_map
          (fun local_vars ((lbl, pos), ty) ->
            let v = Var.make lbl in
            let local_vars = Ident.Map.add lbl v local_vars in
            local_vars, ((v, pos), ty))
          Ident.Map.empty args
      in
      let body = translate_expr None None ctxt local_vars e in
      let args, tys = List.split args_tys in
      let e =
        Expr.make_abs
          (Array.of_list (List.map Mark.remove args))
          body
          (List.map translate_tbase tys)
          (Mark.get def.S.topdef_name)
      in
      Some (Expr.unbox_closed e)
  in
  let program_topdefs =
    TopdefName.Map.update id
      (fun def0 ->
        match def0, expr_opt with
        | None, eopt -> Some (eopt, typ)
        | Some (eopt0, ty0), eopt -> (
          let err msg =
            Message.raise_multispanned_error
              [None, Mark.get ty0; None, Mark.get typ]
              (msg ^^ " for %a") TopdefName.format id
          in
          if not (Type.equal ty0 typ) then err "Conflicting type definitions"
          else
            match eopt0, eopt with
            | None, None -> err "Multiple declarations"
            | Some _, Some _ -> err "Multiple definitions"
            | Some e, None -> Some (Some e, typ)
            | None, Some e -> Some (Some e, ty0)))
      prgm.Ast.program_topdefs
  in
  { prgm with Ast.program_topdefs }

let attribute_to_io (attr : S.scope_decl_context_io) : Ast.io =
  {
    Ast.io_output = attr.scope_decl_context_io_output;
    Ast.io_input =
      Mark.map
        (fun io ->
          match io with
          | S.Input -> Runtime.OnlyInput
          | S.Internal -> Runtime.NoInput
          | S.Context -> Runtime.Reentrant)
        attr.scope_decl_context_io_input;
  }

let init_scope_defs
    (ctxt : Name_resolution.context)
    (scope_idmap : Name_resolution.scope_var_or_subscope Ident.Map.t) :
    Ast.scope_def Ast.ScopeDef.Map.t =
  (* Initializing the definitions of all scopes and subscope vars, with no rules
     yet inside *)
  let add_def _ v scope_def_map =
    match v with
    | Name_resolution.ScopeVar v -> (
      let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
      match v_sig.var_sig_states_list with
      | [] ->
        let def_key = Ast.ScopeDef.Var (v, None) in
        Ast.ScopeDef.Map.add def_key
          {
            Ast.scope_def_rules = RuleName.Map.empty;
            Ast.scope_def_typ = v_sig.var_sig_typ;
            Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
            Ast.scope_def_parameters = v_sig.var_sig_parameters;
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
                  Ast.scope_def_parameters = v_sig.var_sig_parameters;
                  Ast.scope_def_io =
                    (* The first state should have the input I/O of the original
                       variable, and the last state should have the output I/O
                       of the original variable. All intermediate states shall
                       have "internal" I/O.*)
                    (let original_io = attribute_to_io v_sig.var_sig_io in
                     let io_input =
                       if i = 0 then original_io.io_input
                       else NoInput, Mark.get (StateName.get_info state)
                     in
                     let io_output =
                       if i = List.length states - 1 then original_io.io_output
                       else false, Mark.get (StateName.get_info state)
                     in
                     { io_input; io_output });
                }
              in
              Ast.ScopeDef.Map.add def_key def acc, i + 1)
            (scope_def_map, 0) states
        in
        scope_def)
    | Name_resolution.SubScope (v0, subscope_uid) ->
      let sub_scope_def = Name_resolution.get_scope_context ctxt subscope_uid in
      let ctxt =
        List.fold_left
          (fun ctx m -> ModuleName.Map.find m ctx.Name_resolution.modules)
          ctxt
          (ScopeName.path subscope_uid)
      in
      Ident.Map.fold
        (fun _ v scope_def_map ->
          match v with
          | Name_resolution.SubScope _ -> scope_def_map
          | Name_resolution.ScopeVar v ->
            (* TODO: shouldn't we ignore internal variables too at this point
               ? *)
            let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
            let def_key =
              Ast.ScopeDef.SubScopeVar (v0, v, Mark.get (ScopeVar.get_info v))
            in
            Ast.ScopeDef.Map.add def_key
              {
                Ast.scope_def_rules = RuleName.Map.empty;
                Ast.scope_def_typ = v_sig.var_sig_typ;
                Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
                Ast.scope_def_parameters = v_sig.var_sig_parameters;
                Ast.scope_def_io = attribute_to_io v_sig.var_sig_io;
              }
              scope_def_map)
        sub_scope_def.Name_resolution.var_idmap scope_def_map
  in
  Ident.Map.fold add_def scope_idmap Ast.ScopeDef.Map.empty

(** Main function of this module *)
let translate_program (ctxt : Name_resolution.context) (surface : S.program) :
    Ast.program =
  let desugared =
    let get_program_scopes ctxt =
      ScopeName.Map.mapi
        (fun s_uid s_context ->
          let scope_vars =
            Ident.Map.fold
              (fun _ v acc ->
                match v with
                | Name_resolution.SubScope _ -> acc
                | Name_resolution.ScopeVar v -> (
                  let v_sig =
                    ScopeVar.Map.find v ctxt.Name_resolution.var_typs
                  in
                  match v_sig.Name_resolution.var_sig_states_list with
                  | [] -> ScopeVar.Map.add v Ast.WholeVar acc
                  | states -> ScopeVar.Map.add v (Ast.States states) acc))
              s_context.Name_resolution.var_idmap ScopeVar.Map.empty
          in
          let scope_sub_scopes =
            Ident.Map.fold
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
            scope_assertions = Ast.AssertionName.Map.empty;
            scope_meta_assertions = [];
            scope_options = [];
            scope_uid = s_uid;
          })
        ctxt.Name_resolution.scopes
    in
    let rec make_ctx ctxt =
      let submodules =
        ModuleName.Map.map make_ctx ctxt.Name_resolution.modules
      in
      {
        Ast.program_ctx =
          {
            (* After name resolution, type definitions (structs and enums) are
               exposed at toplevel for easier lookup *)
            ctx_structs =
              ModuleName.Map.fold
                (fun _ prg acc ->
                  StructName.Map.union
                    (fun _ _ _ -> assert false)
                    acc prg.Ast.program_ctx.ctx_structs)
                submodules ctxt.Name_resolution.structs;
            ctx_enums =
              ModuleName.Map.fold
                (fun _ prg acc ->
                  EnumName.Map.union
                    (fun _ _ _ -> assert false)
                    acc prg.Ast.program_ctx.ctx_enums)
                submodules ctxt.Name_resolution.enums;
            ctx_scopes =
              Ident.Map.fold
                (fun _ def acc ->
                  match def with
                  | Name_resolution.TScope (scope, scope_info) ->
                    ScopeName.Map.add scope scope_info acc
                  | _ -> acc)
                ctxt.Name_resolution.typedefs ScopeName.Map.empty;
            ctx_struct_fields = ctxt.Name_resolution.field_idmap;
            ctx_topdefs = ctxt.Name_resolution.topdef_types;
            ctx_modules =
              ModuleName.Map.map (fun s -> s.Ast.program_ctx) submodules;
          };
        Ast.program_topdefs = TopdefName.Map.empty;
        Ast.program_scopes = get_program_scopes ctxt;
        Ast.program_modules = submodules;
      }
    in
    make_ctx ctxt
  in
  let process_code_block ctxt prgm block =
    List.fold_left
      (fun prgm item ->
        match Mark.remove item with
        | S.ScopeUse use -> process_scope_use ctxt prgm use
        | S.Topdef def -> process_topdef ctxt prgm def
        | S.ScopeDecl _ | S.StructDecl _ | S.EnumDecl _ -> prgm)
      prgm block
  in
  let rec process_structure (prgm : Ast.program) (item : S.law_structure) :
      Ast.program =
    match item with
    | S.LawHeading (_, children) ->
      List.fold_left
        (fun prgm child -> process_structure prgm child)
        prgm children
    | S.CodeBlock (block, _, _) -> process_code_block ctxt prgm block
    | S.LawInclude _ | S.LawText _ -> prgm
  in
  let desugared =
    List.fold_left
      (fun acc (id, intf) ->
        let id = ModuleName.of_string id in
        let modul = ModuleName.Map.find id acc.Ast.program_modules in
        let modul =
          process_code_block (ModuleName.Map.find id ctxt.modules) modul intf
        in
        {
          acc with
          program_modules = ModuleName.Map.add id modul acc.program_modules;
        })
      desugared surface.S.program_modules
  in
  List.fold_left process_structure desugared surface.S.program_items
