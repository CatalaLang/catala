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

let translate_binop :
    S.binop Mark.pos ->
    Pos.t ->
    Ast.expr boxed ->
    Ast.expr boxed ->
    Ast.expr boxed =
 fun (op, op_pos) pos lhs rhs ->
  let op_expr op tys =
    Expr.eappop ~op:(op, op_pos)
      ~tys:(List.map (Mark.add op_pos) tys)
      ~args:[lhs; rhs]
      (Untyped { pos })
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
        Message.error ~pos:op_pos
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
        Message.error ~pos:op_pos
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
  | S.Concat -> op_expr Concat [TArray (TAny, op_pos); TArray (TAny, op_pos)]

let translate_unop ((op, op_pos) : S.unop Mark.pos) pos arg : Ast.expr boxed =
  let op_expr op ty =
    Expr.eappop ~op:(op, op_pos)
      ~tys:[Mark.add op_pos ty]
      ~args:[arg]
      (Untyped { pos })
  in
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
        Message.error ~pos:op_pos
          "This operator doesn't exist, dates can't be negative"
      | S.KDuration -> TLit TDuration)

let raise_error_cons_not_found
    (ctxt : Name_resolution.context)
    (constructor : string Mark.pos) =
  let constructors = Ident.Map.keys ctxt.local.constructor_idmap in
  let closest_constructors =
    Suggestions.suggestion_minimum_levenshtein_distance_association constructors
      (Mark.remove constructor)
  in
  Message.error
    ~pos_msg:(fun ppf -> Format.fprintf ppf "Here is your code :")
    ~pos:(Mark.get constructor) ~suggestion:closest_constructors
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
      Message.error ~pos
        "The deep pattern matching syntactic sugar is not yet supported"
  in
  let possible_c_uids =
    try Ident.Map.find (Mark.remove constructor) ctxt.local.constructor_idmap
    with Ident.Map.Not_found _ -> raise_error_cons_not_found ctxt constructor
  in
  let possible_c_uids =
    (* Eliminate candidates from other modules if there exists some from the
       current one *)
    let current_module =
      EnumName.Map.filter
        (fun struc _ -> EnumName.path struc = [])
        possible_c_uids
    in
    if EnumName.Map.is_empty current_module then possible_c_uids
    else current_module
  in
  match path with
  | [] ->
    if EnumName.Map.cardinal possible_c_uids > 1 then
      Message.error ~pos:(Mark.get constructor)
        "This constructor name is ambiguous, it can belong to@ %a.@ \
         Disambiguate it by prefixing it with the enum name."
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
      Message.error ~pos "Enum %s@ does@ not@ contain@ case@ %s"
        (Mark.remove enum) (Mark.remove constructor))
  | mod_id :: path ->
    let constructor =
      List.map (Mark.map (fun (_, c) -> path, c)) constructor0
    in
    disambiguate_constructor
      (Name_resolution.get_module_ctx ctxt mod_id)
      constructor pos

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
      Message.error
        ~extra_pos:["", pos_op; "", pos_op1]
        "%a" Format.pp_print_text
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
  let rec detuplify_list opos names = function
    (* Where a list is expected (e.g. after [among]), as syntactic sugar, if a
       tuple is found instead we transpose it into a list of tuples *)
    | S.Tuple ls, pos ->
      let m = Untyped { pos } in
      let ls = List.map (detuplify_list opos []) ls in
      let rec zip names = function
        | [] -> assert false
        | [l] -> l
        | l1 :: r ->
          let name1, names =
            match names with name1 :: names -> name1, names | [] -> "x", []
          in
          let rhs = zip names r in
          let rtys, explode =
            match List.length r with
            | 1 -> (TAny, pos), fun e -> [e]
            | size ->
              ( (TTuple (List.map (fun _ -> TAny, pos) r), pos),
                fun e ->
                  List.init size (fun index ->
                      Expr.etupleaccess ~e ~size ~index m) )
          in
          let tys = [TAny, pos; rtys] in
          let f_join =
            let x1 = Var.make name1 in
            let x2 =
              Var.make
                (match names with [] -> "zip" | _ -> String.concat "_" names)
            in
            Expr.make_abs [| x1; x2 |]
              (Expr.make_tuple (Expr.evar x1 m :: explode (Expr.evar x2 m)) m)
              tys pos
          in
          Expr.eappop ~op:(Map2, opos) ~args:[f_join; l1; rhs]
            ~tys:((TAny, pos) :: List.map (fun ty -> TArray ty, pos) tys)
            m
      in
      zip names ls
    | e ->
      (* If the input is not a tuple, we assume it's already a list *)
      rec_helper e
  in
  let pos = Mark.get expr in
  let emark = Untyped { pos } in
  match Mark.remove expr with
  | Paren e -> rec_helper e
  | Binop
      ( (S.And, pos_op),
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
              [tau] pos_op
          else
            let binding_var = Var.make (Mark.remove binding) in
            let local_vars =
              Ident.Map.add (Mark.remove binding) binding_var local_vars
            in
            let e2 = rec_helper ~local_vars e2 in
            Expr.make_abs [| binding_var |] e2 [tau] pos_op)
        (EnumName.Map.find enum_uid ctxt.enums)
    in
    Expr.ematch ~e:(rec_helper e1_sub) ~name:enum_uid ~cases emark
  | Binop ((((S.And | S.Or | S.Xor), _) as op), e1, e2) ->
    check_formula op e1;
    check_formula op e2;
    translate_binop op pos (rec_helper e1) (rec_helper e2)
  | IfThenElse (e_if, e_then, e_else) ->
    Expr.eifthenelse (rec_helper e_if) (rec_helper e_then) (rec_helper e_else)
      emark
  | Binop ((S.Neq, posn), e1, e2) ->
    (* Neq is just sugar *)
    rec_helper (Unop ((S.Not, posn), (Binop ((S.Eq, posn), e1, e2), posn)), pos)
  | Binop (op, e1, e2) -> translate_binop op pos (rec_helper e1) (rec_helper e2)
  | Unop (op, e) -> translate_unop op pos (rec_helper e)
  | Literal l ->
    let lit =
      match l with
      | LNumber ((Int i, _), None) -> LInt (Runtime.integer_of_string i)
      | LNumber ((Int i, _), Some (Percent, _)) ->
        LRat
          Runtime.(
            Oper.o_div_rat_rat (Expr.pos_to_runtime pos) (decimal_of_string i)
              rat100)
      | LNumber ((Dec (i, f), _), None) ->
        LRat Runtime.(decimal_of_string (i ^ "." ^ f))
      | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
        LRat
          Runtime.(
            Oper.o_div_rat_rat (Expr.pos_to_runtime pos)
              (decimal_of_string (i ^ "." ^ f))
              rat100)
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
        Message.error ~pos
          "Impossible to specify decimal amounts of days, months or years"
      | LDate date ->
        if date.literal_date_month > 12 then
          Message.error ~pos
            "There is an error in this date: the month number is bigger than 12";
        if date.literal_date_day > 31 then
          Message.error ~pos
            "There is an error in this date: the day number is bigger than 31";
        LDate
          (try
             Runtime.date_of_numbers date.literal_date_year
               date.literal_date_month date.literal_date_day
           with Failure _ ->
             Message.error ~pos
               "There is an error in this date, it does not correspond to a \
                correct calendar day")
    in
    Expr.elit lit emark
  | Ident ([], (x, pos), state) -> (
    (* first we check whether this is a local var, then we resort to scope-wide
       variables, then global variables *)
    match Ident.Map.find_opt x local_vars, state with
    | Some uid, None ->
      Expr.make_var uid emark
      (* the whole box thing is to accomodate for this case *)
    | Some uid, Some state ->
      Message.error ~pos:(Mark.get state)
        "%a is a local variable, it has no states" Print.var uid
    | None, state -> (
      match Ident.Map.find_opt x scope_vars with
      | Some (ScopeVar uid) ->
        (* If the referenced variable has states, then here are the rules to
           desambiguate. In general, only the last state can be referenced.
           Except if defining a state of the same variable, then it references
           the previous state in the chain. *)
        let x_sig = ScopeVar.Map.find uid ctxt.var_typs in
        let x_state =
          match state, x_sig.var_sig_states_list, inside_definition_of with
          | None, [], _ -> None
          | Some st, [], _ ->
            Message.error ~pos:(Mark.get st)
              "Variable %a does not define states" ScopeVar.format uid
          | st, states, Some (((x'_uid, _), Ast.ScopeDef.Var sx'), _)
            when ScopeVar.equal uid x'_uid -> (
            if st <> None then
              (* TODO *)
              Message.error
                ~pos:(Mark.get (Option.get st))
                "%a" Format.pp_print_text
                "Referring to a previous state of the variable being defined \
                 is not supported at the moment.";
            match sx' with
            | None ->
              Message.error ~internal:true
                "inconsistent state: inside a definition of a variable with no \
                 state but variable has states"
            | Some inside_def_state ->
              if StateName.compare inside_def_state (List.hd states) = 0 then
                Message.error ~pos "%a" Format.pp_print_text
                  "The definition of the initial state of this variable refers \
                   to itself."
              else
                (* Tricky: we have to retrieve in the list the previous state
                   with respect to the state that we are defining. *)
                let rec find_prev_state = function
                  | [] -> None
                  | st0 :: st1 :: _ when StateName.equal inside_def_state st1 ->
                    Some st0
                  | _ :: states -> find_prev_state states
                in
                find_prev_state states)
          | Some st, states, _ -> (
            match
              Ident.Map.find_opt (Mark.remove st) x_sig.var_sig_states_idmap
            with
            | None ->
              Message.error
                ~suggestion:(List.map StateName.to_string states)
                ~extra_pos:
                  [
                    "", Mark.get st;
                    "Variable defined here", Mark.get (ScopeVar.get_info uid);
                  ]
                "Reference to unknown variable state"
            | some -> some)
          | _, states, _ ->
            (* we take the last state in the chain *)
            Some (List.hd (List.rev states))
        in
        Expr.elocation
          (DesugaredScopeVar { name = uid, pos; state = x_state })
          emark
      | Some (SubScope (uid, _, _)) ->
        Expr.elocation
          (DesugaredScopeVar { name = uid, pos; state = None })
          emark
      | None -> (
        match Ident.Map.find_opt x ctxt.local.topdefs with
        | Some v ->
          if state <> None then
            Message.error ~pos
              "Access to intermediate states is only allowed for variables of \
               the current scope";
          Expr.elocation
            (ToplevelVar { name = v, Mark.get (TopdefName.get_info v) })
            emark
        | None ->
          Name_resolution.raise_unknown_identifier
            "for a local, scope-wide or global variable" (x, pos))))
  | Ident (_ :: _, (_, pos), Some _) ->
    Message.error ~pos
      "Access to intermediate states is only allowed for variables of the \
       current scope"
  | Ident (path, name, None) -> (
    let ctxt = Name_resolution.module_ctx ctxt path in
    match Ident.Map.find_opt (Mark.remove name) ctxt.local.topdefs with
    | Some v ->
      Expr.elocation
        (ToplevelVar { name = v, Mark.get (TopdefName.get_info v) })
        emark
    | None ->
      Name_resolution.raise_unknown_identifier "for an external variable" name)
  | Dotted (e, ((path, x), _ppos)) ->
    (* e.x is the struct field x access of expression e *)
    let e = rec_helper e in
    let rec get_str ctxt = function
      | [] -> None
      | [c] -> Some (Name_resolution.get_struct ctxt c)
      | mod_id :: path ->
        get_str (Name_resolution.get_module_ctx ctxt mod_id) path
    in
    Expr.edstructaccess ~e ~field:(Mark.remove x) ~name_opt:(get_str ctxt path)
      emark
  | FunCall ((Builtin b, pos), [arg]) ->
    let op, ty =
      match b with
      | S.ToDecimal -> Op.ToRat, TAny
      | S.ToMoney -> Op.ToMoney, TAny
      | S.Round -> Op.Round, TAny
      | S.Cardinal -> Op.Length, TArray (TAny, pos)
      | S.GetDay -> Op.GetDay, TLit TDate
      | S.GetMonth -> Op.GetMonth, TLit TDate
      | S.GetYear -> Op.GetYear, TLit TDate
      | S.FirstDayOfMonth -> Op.FirstDayOfMonth, TLit TDate
      | S.LastDayOfMonth -> Op.LastDayOfMonth, TLit TDate
    in
    Expr.eappop ~op:(op, pos) ~tys:[ty, pos] ~args:[rec_helper arg] emark
  | S.Builtin _ ->
    Message.error ~pos "Invalid use of built-in: needs one operand"
  | FunCall (f, args) ->
    let args = List.map rec_helper args in
    Expr.eapp ~f:(rec_helper f) ~args ~tys:[] emark
  | ScopeCall (((path, id), _), fields) ->
    if scope = None then
      Message.error ~pos "Scope calls are not allowed outside of a scope";
    let called_scope, scope_def =
      let ctxt = Name_resolution.module_ctx ctxt path in
      let uid = Name_resolution.get_scope ctxt id in
      uid, ScopeName.Map.find uid ctxt.scopes
    in
    let in_struct =
      List.fold_left
        (fun acc ((fld_id : S.scope_var), e) ->
          let subscope_path, var =
            match
              Ident.Map.find_opt
                (Mark.remove (List.hd fld_id))
                scope_def.var_idmap
            with
            | Some (ScopeVar v) -> None, v
            | Some (SubScope (subscope_var, subscope_name, _)) ->
              Some (List.tl fld_id, subscope_name), subscope_var
            | None ->
              Message.error
                ~suggestion:(Ident.Map.keys scope_def.var_idmap)
                ~extra_pos:
                  [
                    "", Mark.get (List.hd fld_id);
                    ( Format.asprintf "Scope %a declared here" ScopeName.format
                        called_scope,
                      Mark.get (ScopeName.get_info called_scope) );
                  ]
                "Scope %a has no input variable %a" ScopeName.format
                called_scope Print.lit_style
                (Mark.remove (List.hd fld_id))
          in
          let rec add_subscope_path_and_expr_to_in_struct
              (current_subscope : ScopeName.t)
              (subscope_path : S.scope_var)
              (var_expr : S.expression) : Ast.expr boxed =
            match subscope_path with
            | hd :: tl -> (
              let current_subscope_ctx =
                ScopeName.Map.find current_subscope ctxt.scopes
              in
              let subscope_var =
                Ident.Map.find_opt (Mark.remove hd)
                  current_subscope_ctx.var_idmap
              in
              match subscope_var with
              | Some (ScopeVar v) -> assert false
              | Some (SubScope (sub_subscope_var, sub_sub_scope_name, _)) ->
                assert false
              | None ->
                Message.error
                  ~suggestion:(Ident.Map.keys current_subscope_ctx.var_idmap)
                  ~extra_pos:
                    [
                      "", Mark.get (List.hd fld_id);
                      ( Format.asprintf "Scope %a declared here"
                          ScopeName.format current_subscope,
                        Mark.get (ScopeName.get_info current_subscope) );
                    ]
                  "Scope %a has no input variable %a" ScopeName.format
                  current_subscope Print.lit_style (Mark.remove hd)
              | _ -> assert false)
            | [] -> rec_helper e
          in
          let merge_subscope_in_structs
              (acc : Ast.expr boxed)
              (new_subscope_input : Ast.expr boxed) : Ast.expr boxed =
            assert false
          in
          ScopeVar.Map.update var
            (fun var_expr ->
              match var_expr, subscope_path with
              | None, None -> Some (rec_helper e)
              | Some _, None ->
                Message.error
                  ~pos:(Mark.get (List.hd fld_id))
                  "Duplicate definition of scope input variable '%a'"
                  ScopeVar.format var
              | None, Some (subscope_path, subscope_name) ->
                Some
                  (add_subscope_path_and_expr_to_in_struct subscope_name
                     subscope_path e)
              | Some subscope_input, Some (subscope_path, subscope_name) ->
                Some
                  (merge_subscope_in_structs subscope_input
                     (add_subscope_path_and_expr_to_in_struct subscope_name
                        subscope_path e)))
            acc)
        ScopeVar.Map.empty fields
    in
    Expr.escopecall ~scope:called_scope ~args:in_struct emark
  | LetIn (xs, e1, e2) ->
    let vs = List.map (fun x -> Var.make (Mark.remove x)) xs in
    let local_vars =
      List.fold_left2
        (fun local_vars x v -> Ident.Map.add (Mark.remove x) v local_vars)
        local_vars xs vs
    in
    let taus = List.map (fun x -> TAny, Mark.get x) xs in
    (* This type will be resolved in Scopelang.Desambiguation *)
    let f =
      Expr.make_abs (Array.of_list vs) (rec_helper ~local_vars e2) taus pos
    in
    Expr.eapp ~f ~args:[rec_helper e1] ~tys:[] emark
  | StructReplace (e, fields) ->
    let fields =
      List.fold_left
        (fun acc (field_id, field_expr) ->
          if Ident.Map.mem (Mark.remove field_id) acc then
            Message.error ~pos:(Mark.get field_expr)
              "Duplicate redefinition of field@ %a" Ident.format
              (Mark.remove field_id);
          Ident.Map.add (Mark.remove field_id) (rec_helper field_expr) acc)
        Ident.Map.empty fields
    in
    Expr.edstructamend ~fields ~e:(rec_helper e) ~name_opt:None emark
  | StructLit (((path, s_name), _), fields) ->
    let ctxt = Name_resolution.module_ctx ctxt path in
    let s_uid =
      match Ident.Map.find_opt (Mark.remove s_name) ctxt.local.typedefs with
      | Some (Name_resolution.TStruct s_uid)
      | Some (Name_resolution.TScope (_, { out_struct_name = s_uid; _ })) ->
        s_uid
      | _ ->
        Message.error ~pos:(Mark.get s_name)
          "This identifier should refer to a struct name"
    in
    let s_fields =
      List.fold_left
        (fun s_fields (f_name, f_e) ->
          let f_uid =
            try
              StructName.Map.find s_uid
                (Ident.Map.find (Mark.remove f_name) ctxt.local.field_idmap)
            with StructName.Map.Not_found _ | Ident.Map.Not_found _ ->
              Message.error ~pos:(Mark.get f_name)
                "This identifier should refer to a field of struct %s"
                (Mark.remove s_name)
          in
          (match StructField.Map.find_opt f_uid s_fields with
          | None -> ()
          | Some e_field ->
            Message.error
              ~extra_pos:["", Mark.get f_e; "", Expr.pos e_field]
              "The field %a has been defined twice:" StructField.format f_uid);
          let f_e = rec_helper f_e in
          StructField.Map.add f_uid f_e s_fields)
        StructField.Map.empty fields
    in
    let expected_s_fields = StructName.Map.find s_uid ctxt.structs in
    if
      StructField.Map.exists
        (fun expected_f _ -> not (StructField.Map.mem expected_f s_fields))
        expected_s_fields
    then
      Message.error ~pos "Missing field(s) for structure %a:@\n%a"
        StructName.format s_uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (expected_f, _) ->
             Format.fprintf fmt "\"%a\"" StructField.format expected_f))
        (StructField.Map.bindings
           (StructField.Map.filter
              (fun expected_f _ ->
                not (StructField.Map.mem expected_f s_fields))
              expected_s_fields));

    Expr.estruct ~name:s_uid ~fields:s_fields emark
  | EnumInject (((path, (constructor, pos_constructor)), _), payload) -> (
    let get_possible_c_uids ctxt =
      try
        let possible =
          Ident.Map.find constructor
            ctxt.Name_resolution.local.constructor_idmap
        in
        (* Eliminate candidates from other modules if there exists some from the
           current one *)
        let current_module =
          EnumName.Map.filter (fun struc _ -> EnumName.path struc = []) possible
        in
        if EnumName.Map.is_empty current_module then possible
        else current_module
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
        Message.error ~pos:pos_constructor
          "This constructor name is ambiguous, it can belong to@ %a.@ \
           Disambiguate it by prefixing it with the enum name."
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
        Message.error ~pos "Enum %s does not contain case %s" (Mark.remove enum)
          constructor))
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
      Message.warning ~pos:(Mark.get binding)
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
  | Tuple es -> Expr.etuple (List.map rec_helper es) emark
  | TupleAccess (e, n) ->
    Expr.etupleaccess ~e:(rec_helper e) ~index:(Mark.remove n - 1) ~size:0 emark
  | CollectionOp ((((S.Filter { f } | S.Map { f }), opos) as op), collection) ->
    let param_names, predicate = f in
    let collection =
      detuplify_list opos (List.map Mark.remove param_names) collection
    in
    let params = List.map (fun n -> Var.make (Mark.remove n)) param_names in
    let local_vars =
      List.fold_left2
        (fun vars n p -> Ident.Map.add (Mark.remove n) p vars)
        local_vars param_names params
    in
    let f_pred =
      Expr.make_abs (Array.of_list params)
        (rec_helper ~local_vars predicate)
        (List.map (fun _ -> TAny, pos) params)
        pos
    in
    let f_pred =
      (* Detuplification (TODO: check if we couldn't fit this in the general
         detuplification later) *)
      match List.length param_names with
      | 1 -> f_pred
      | nb_args ->
        let v =
          Var.make (String.concat "_" (List.map Mark.remove param_names))
        in
        let x = Expr.evar v emark in
        let tys = List.map (fun _ -> TAny, pos) param_names in
        Expr.make_abs [| v |]
          (Expr.make_app f_pred
             (List.init nb_args (fun i ->
                  Expr.etupleaccess ~e:x ~index:i ~size:nb_args emark))
             tys pos)
          [TAny, pos]
          pos
    in
    Expr.eappop
      ~op:
        (match op with
        | S.Map _, pos -> Map, pos
        | S.Filter _, pos -> Filter, pos
        | _ -> assert false)
      ~tys:[TAny, pos; TAny, pos]
      ~args:[f_pred; collection] emark
  | CollectionOp
      ( ( S.AggregateArgExtremum { max; default; f = param_names, predicate },
          opos ),
        collection ) ->
    let default = rec_helper default in
    let pos_dft = Expr.pos default in
    let collection =
      detuplify_list opos (List.map Mark.remove param_names) collection
    in
    let params = List.map (fun n -> Var.make (Mark.remove n)) param_names in
    let local_vars =
      List.fold_left2
        (fun vars n p -> Ident.Map.add (Mark.remove n) p vars)
        local_vars param_names params
    in
    let cmp_op = if max then Op.Gt, opos else Op.Lt, opos in
    let f_pred =
      Expr.make_abs (Array.of_list params)
        (rec_helper ~local_vars predicate)
        [TAny, pos]
        pos
    in
    let add_weight_f =
      let vs = List.map (fun p -> Var.make (Bindlib.name_of p)) params in
      let xs = List.map (fun v -> Expr.evar v emark) vs in
      let x = match xs with [x] -> x | xs -> Expr.etuple xs emark in
      Expr.make_abs (Array.of_list vs)
        (Expr.make_tuple [x; Expr.eapp ~f:f_pred ~args:xs ~tys:[] emark] emark)
        [TAny, pos]
        pos
    in
    let reduce_f =
      (* fun x1 x2 -> if cmp_op (x1.2) (x2.2) cmp *)
      let v1, v2 = Var.make "x1", Var.make "x2" in
      let x1, x2 = Expr.make_var v1 emark, Expr.make_var v2 emark in
      Expr.make_abs [| v1; v2 |]
        (Expr.eifthenelse
           (Expr.eappop ~op:cmp_op
              ~tys:[TAny, pos_dft; TAny, pos_dft]
              ~args:
                [
                  Expr.etupleaccess ~e:x1 ~index:1 ~size:2 emark;
                  Expr.etupleaccess ~e:x2 ~index:1 ~size:2 emark;
                ]
              emark)
           x1 x2 emark)
        [TAny, pos; TAny, pos]
        pos
    in
    let weights_var = Var.make "weights" in
    let default = Expr.make_app add_weight_f [default] [TAny, pos] pos_dft in
    let weighted_result =
      Expr.make_let_in weights_var
        (TArray (TTuple [TAny, pos; TAny, pos], pos), pos)
        (Expr.eappop ~op:(Map, opos)
           ~tys:[TAny, pos; TArray (TAny, pos), pos]
           ~args:[add_weight_f; collection] emark)
        (Expr.eappop ~op:(Reduce, opos)
           ~tys:[TAny, pos; TAny, pos; TAny, pos]
           ~args:[reduce_f; default; Expr.evar weights_var emark]
           emark)
        pos
    in
    Expr.etupleaccess ~e:weighted_result ~index:0 ~size:2 emark
  | CollectionOp
      ((((Exists { predicate } | Forall { predicate }), opos) as op), collection)
    ->
    let collection =
      detuplify_list opos (List.map Mark.remove (fst predicate)) collection
    in
    let init, op =
      match op with
      | Exists _, pos -> false, (S.Or, pos)
      | Forall _, pos -> true, (S.And, pos)
      | _ -> assert false
    in
    let init = Expr.elit (LBool init) emark in
    let params0, predicate = predicate in
    let params = List.map (fun n -> Var.make (Mark.remove n)) params0 in
    let local_vars =
      List.fold_left2
        (fun vars n p -> Ident.Map.add (Mark.remove n) p vars)
        local_vars params0 params
    in
    let f =
      let acc_var = Var.make "acc" in
      let acc =
        Expr.make_var acc_var (Untyped { pos = Mark.get (List.hd params0) })
      in
      Expr.eabs
        (Expr.bind
           (Array.of_list (acc_var :: params))
           (translate_binop op pos acc (rec_helper ~local_vars predicate)))
        [TAny, pos; TAny, pos]
        emark
    in
    Expr.eappop ~op:(Fold, opos)
      ~tys:[TAny, pos; TAny, pos; TAny, pos]
      ~args:[f; init; collection] emark
  | CollectionOp ((AggregateExtremum { max; default }, opos), collection) ->
    let collection = rec_helper collection in
    let default = rec_helper default in
    let op = if max then S.Gt KPoly else S.Lt KPoly in
    let op_f =
      (* fun x1 x2 -> if op x1 x2 then x1 else x2 *)
      let vname = if max then "max" else "min" in
      let v1, v2 = Var.make (vname ^ "1"), Var.make (vname ^ "2") in
      let x1 = Expr.make_var v1 emark in
      let x2 = Expr.make_var v2 emark in
      Expr.make_abs [| v1; v2 |]
        (Expr.eifthenelse (translate_binop (op, pos) pos x1 x2) x1 x2 emark)
        [TAny, pos; TAny, pos]
        pos
    in
    Expr.eappop ~op:(Reduce, opos)
      ~tys:[TAny, pos; TAny, pos; TAny, pos]
      ~args:[op_f; default; collection]
      emark
  | CollectionOp ((AggregateSum { typ }, opos), collection) ->
    let collection = rec_helper collection in
    let default_lit =
      let i0 = Runtime.integer_of_int 0 in
      match typ with
      | S.Integer -> LInt i0
      | S.Decimal -> LRat (Runtime.decimal_of_integer i0)
      | S.Money -> LMoney (Runtime.money_of_cents_integer i0)
      | S.Duration -> LDuration (Runtime.duration_of_numbers 0 0 0)
      | t ->
        Message.error ~pos:opos
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
        (translate_binop (S.Add KPoly, opos) pos x1 x2)
        [TAny, pos; TAny, pos]
        pos
    in
    Expr.eappop ~op:(Reduce, opos)
      ~tys:[TAny, pos; TAny, pos; TAny, pos]
      ~args:[op_f; Expr.elit default_lit emark; collection]
      emark
  | CollectionOp ((Member { element = member }, opos), collection) ->
    let param_var = Var.make "collection_member" in
    let param = Expr.make_var param_var emark in
    let collection = detuplify_list opos ["collection_member"] collection in
    let init = Expr.elit (LBool false) emark in
    let acc_var = Var.make "acc" in
    let acc = Expr.make_var acc_var emark in
    let f_body =
      let member = rec_helper member in
      Expr.eappop ~op:(Or, opos)
        ~tys:[TLit TBool, pos; TLit TBool, pos]
        ~args:
          [
            Expr.eappop ~op:(Eq, opos)
              ~tys:[TAny, pos; TAny, pos]
              ~args:[member; param] emark;
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
    Expr.eappop ~op:(Fold, opos)
      ~tys:[TAny, pos; TAny, pos; TAny, pos]
      ~args:[f; init; collection] emark

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
            Message.error
              ~pos:(Mark.get case.S.match_case_pattern)
              "This case matches a constructor of enumeration@ %a@ but@ \
               previous@ cases@ were@ matching@ constructors@ of@ enumeration@ \
               %a"
              EnumName.format e_uid EnumName.format e_uid'
      in
      (match EnumConstructor.Map.find_opt c_uid cases_d with
      | None -> ()
      | Some e_case ->
        Message.error
          ~extra_pos:["", Mark.get case.match_case_expr; "", Expr.pos e_case]
          "The constructor %a@ has@ been@ matched@ twice:"
          EnumConstructor.format c_uid);
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
        Message.error
          ~extra_pos:
            [
              "Not ending wildcard:", case_pos;
              ( "Next reachable case:",
                curr_index + 1 |> List.nth cases |> Mark.get );
            ]
          "Wildcard must be the last match case"
      in
      match e_uid with
      | None ->
        if 1 = nb_cases then
          Message.error ~pos:case_pos "%a" Format.pp_print_text
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
          Message.warning ~pos:case_pos
            "Unreachable match case, all constructors of the enumeration@ %a@ \
             are@ already@ specified"
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
    Expr.eappop ~op:(And, default_pos)
      ~tys:[TLit TBool, default_pos; TLit TBool, default_pos]
      ~args:[precond; cond] (Mark.get cond)
  | Some precond, None -> Mark.remove precond, Untyped { pos = default_pos }
  | None, Some cond -> cond
  | None, None -> Expr.elit (LBool true) (Untyped { pos = default_pos })

let rec arglist_eq_check pos_decl pos_def pdecl pdefs =
  match pdecl, pdefs with
  | [], [] -> ()
  | [], (arg, apos) :: _ ->
    Message.error
      ~extra_pos:["Declared here:", pos_decl; "Extra argument:", apos]
      "This definition has an extra, undeclared argument '%a'" Print.lit_style
      arg
  | (arg, apos) :: _, [] ->
    Message.error
      ~extra_pos:
        ["Argument declared here:", apos; "Mismatching definition:", pos_def]
      "This definition is missing argument '%a'" Print.lit_style arg
  | decl :: pdecl, def :: pdefs when Uid.MarkedString.equal decl def ->
    arglist_eq_check pos_decl pos_def pdecl pdefs
  | (decl_arg, decl_apos) :: _, (def_arg, def_apos) :: _ ->
    Message.error
      ~extra_pos:
        ["Argument declared here:", decl_apos; "Defined here:", def_apos]
      "Function argument name mismatch between declaration@ ('%a')@ and@ \
       definition@ ('%a')"
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
    Message.error
      ~extra_pos:
        [
          "Declared here without arguments", decl_pos;
          "Unexpected arguments appearing here", pos;
        ]
      "Extra arguments in this definition of@ %a" Ast.ScopeDef.format decl_name
  | Some (_, pos), None ->
    Message.error
      ~extra_pos:
        [
          "Arguments declared here", pos;
          "Definition missing the arguments", Mark.get def.S.definition_name;
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

(** Translates a surface definition into condition into a desugared
    {!type:
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
  let scope : Ast.scope =
    ScopeName.Map.find scope_uid prgm.program_root.module_scopes
  in
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
          Message.error ~pos:(Mark.get label_str)
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
  let module_scopes =
    ScopeName.Map.add scope_uid scope_updated prgm.program_root.module_scopes
  in
  { prgm with program_root = { prgm.program_root with module_scopes } }

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
  let scope : Ast.scope =
    ScopeName.Map.find scope_uid prgm.program_root.module_scopes
  in
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
  let module_scopes =
    ScopeName.Map.add scope_uid new_scope prgm.program_root.module_scopes
  in
  { prgm with program_root = { prgm.program_root with module_scopes } }

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
    let scope : Ast.scope =
      ScopeName.Map.find scope_uid prgm.program_root.module_scopes
    in
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
        Message.error
          ~extra_pos:["", old_pos; "", Mark.get item]
          "You cannot set multiple date rounding modes"
      | None ->
        {
          scope with
          scope_options =
            Mark.copy item (Ast.DateRounding r) :: scope.scope_options;
        }
    in
    let module_scopes =
      ScopeName.Map.add scope_uid new_scope prgm.program_root.module_scopes
    in
    { prgm with program_root = { prgm.program_root with module_scopes } }
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
        Message.error ~pos:(Mark.get item)
          "This exception does not have a corresponding definition"
      | Some (Ambiguous pos) ->
        Message.error ~pos:(Mark.get item)
          ~pos_msg:(fun ppf -> Format.pp_print_text ppf "Ambiguous exception")
          ~extra_pos:(List.map (fun p -> "Candidate definition", p) pos)
          "%a" Format.pp_print_text
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
    match ScopeName.Map.find_opt scope_uid prgm.program_root.module_scopes with
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
    Ident.Map.find
      (Mark.remove def.S.topdef_name)
      ctxt.Name_resolution.local.topdefs
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
      let () =
        match tys with
        | [(Data (S.TTuple _), pos)] ->
          Message.error ~pos
            "Defining arguments of a function as a tuple is not supported, \
             please name the individual arguments"
        | _ -> ()
      in
      let e =
        Expr.make_abs
          (Array.of_list (List.map Mark.remove args))
          body
          (List.map translate_tbase tys)
          (Mark.get def.S.topdef_name)
      in
      Some (Expr.unbox_closed e)
  in
  let module_topdefs =
    TopdefName.Map.update id
      (fun def0 ->
        match def0, expr_opt with
        | None, eopt -> Some (eopt, typ)
        | Some (eopt0, ty0), eopt -> (
          let err msg =
            Message.error
              ~extra_pos:
                [
                  "", Mark.get (TopdefName.get_info id);
                  "", Mark.get def.S.topdef_name;
                ]
              (msg ^^ " for %a") TopdefName.format id
          in
          if not (Type.equal ty0 typ) then err "Conflicting type definitions"
          else
            match eopt0, eopt with
            | None, None -> err "Multiple declarations"
            | Some _, Some _ -> err "Multiple definitions"
            | Some e, None -> Some (Some e, typ)
            | None, Some e -> Some (Some e, ty0)))
      prgm.Ast.program_root.module_topdefs
  in
  { prgm with program_root = { prgm.program_root with module_topdefs } }

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
    (scope_context : Name_resolution.scope_context) :
    Ast.scope_def Ast.ScopeDef.Map.t =
  (* Initializing the definitions of all scopes and subscope vars, with no rules
     yet inside *)
  let add_def _ v scope_def_map =
    let pos =
      match v with
      | ScopeVar v | SubScope (v, _, _) -> Mark.get (ScopeVar.get_info v)
    in
    let new_def v_sig io =
      {
        Ast.scope_def_rules = RuleName.Map.empty;
        Ast.scope_def_typ = v_sig.Name_resolution.var_sig_typ;
        Ast.scope_def_is_condition = v_sig.var_sig_is_condition;
        Ast.scope_def_parameters = v_sig.var_sig_parameters;
        Ast.scope_def_io = io;
      }
    in
    match v with
    | ScopeVar v -> (
      let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
      match v_sig.var_sig_states_list with
      | [] ->
        let def_key = (v, pos), Ast.ScopeDef.Var None in
        Ast.ScopeDef.Map.add def_key
          (new_def v_sig (attribute_to_io v_sig.var_sig_io))
          scope_def_map
      | states ->
        let last_state = List.length states - 1 in
        let scope_def, _ =
          List.fold_left
            (fun (acc, i) state ->
              let def_key = (v, pos), Ast.ScopeDef.Var (Some state) in
              let original_io = attribute_to_io v_sig.var_sig_io in
              (* The first state should have the input I/O of the original
                 variable, and the last state should have the output I/O of the
                 original variable. All intermediate states shall have
                 "internal" I/O.*)
              let io_input =
                if i = 0 then original_io.io_input
                else NoInput, Mark.get (StateName.get_info state)
              in
              let io_output =
                if i = last_state then original_io.io_output
                else false, Mark.get (StateName.get_info state)
              in
              let def = new_def v_sig { io_input; io_output } in
              Ast.ScopeDef.Map.add def_key def acc, i + 1)
            (scope_def_map, 0) states
        in
        scope_def)
    | SubScope (v0, subscope_uid, forward_out) ->
      let sub_scope_def = Name_resolution.get_scope_context ctxt subscope_uid in
      let ctxt =
        List.fold_left
          (fun ctx m ->
            {
              ctxt with
              local = ModuleName.Map.find m ctx.Name_resolution.modules;
            })
          ctxt
          (ScopeName.path subscope_uid)
      in
      let var_def =
        {
          Ast.scope_def_rules = RuleName.Map.empty;
          Ast.scope_def_typ =
            ( TStruct sub_scope_def.scope_out_struct,
              Mark.get (ScopeVar.get_info v0) );
          Ast.scope_def_is_condition = false;
          Ast.scope_def_parameters = None;
          Ast.scope_def_io =
            {
              io_input = NoInput, Mark.get forward_out;
              io_output = forward_out;
            };
        }
      in
      let scope_def_map =
        Ast.ScopeDef.Map.add
          ((v0, pos), Ast.ScopeDef.Var None)
          var_def scope_def_map
      in
      Ident.Map.fold
        (fun _ v scope_def_map ->
          match v with
          | SubScope _ ->
            (* TODO: if we consider "input subscopes" at some point their inputs
               will need to be forwarded here *)
            scope_def_map
          | ScopeVar v ->
            (* TODO: shouldn't we ignore internal variables too at this point
               ? *)
            let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
            let def_key =
              ( (v0, Mark.get (ScopeVar.get_info v)),
                Ast.ScopeDef.SubScopeInput
                  { name = subscope_uid; var_within_origin_scope = v } )
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
  Ident.Map.fold add_def scope_context.var_idmap Ast.ScopeDef.Map.empty

(** Main function of this module *)
let translate_program (ctxt : Name_resolution.context) (surface : S.program) :
    Ast.program =
  let get_scope s_uid =
    let s_context = ScopeName.Map.find s_uid ctxt.scopes in
    let scope_vars =
      Ident.Map.fold
        (fun _ v acc ->
          match v with
          | SubScope _ -> acc
          | ScopeVar v -> (
            let v_sig = ScopeVar.Map.find v ctxt.Name_resolution.var_typs in
            match v_sig.Name_resolution.var_sig_states_list with
            | [] -> ScopeVar.Map.add v Ast.WholeVar acc
            | states -> ScopeVar.Map.add v (Ast.States states) acc))
        s_context.Name_resolution.var_idmap ScopeVar.Map.empty
    in
    let scope_sub_scopes =
      Ident.Map.fold
        (fun _ v acc ->
          match v with
          | ScopeVar _ -> acc
          | SubScope (sub_var, sub_scope, _) ->
            ScopeVar.Map.add sub_var sub_scope acc)
        s_context.Name_resolution.var_idmap ScopeVar.Map.empty
    in
    {
      Ast.scope_vars;
      scope_sub_scopes;
      scope_defs = init_scope_defs ctxt s_context;
      scope_assertions = Ast.AssertionName.Map.empty;
      scope_meta_assertions = [];
      scope_options = [];
      scope_uid = s_uid;
    }
  in
  let get_scopes mctx =
    Ident.Map.fold
      (fun _ tydef acc ->
        match tydef with
        | Name_resolution.TScope (s_uid, _) ->
          ScopeName.Map.add s_uid (get_scope s_uid) acc
        | _ -> acc)
      mctx.Name_resolution.typedefs ScopeName.Map.empty
  in
  let program_modules =
    ModuleName.Map.map
      (fun mctx ->
        {
          Ast.module_scopes = get_scopes mctx;
          Ast.module_topdefs =
            Ident.Map.fold
              (fun _ name acc ->
                TopdefName.Map.add name
                  ( None,
                    TopdefName.Map.find name ctxt.Name_resolution.topdef_types
                  )
                  acc)
              mctx.topdefs TopdefName.Map.empty;
        })
      ctxt.modules
  in
  let program_ctx =
    let open Name_resolution in
    let ctx_scopes mctx acc =
      Ident.Map.fold
        (fun _ tydef acc ->
          match tydef with
          | TScope (s_uid, info) -> ScopeName.Map.add s_uid info acc
          | _ -> acc)
        mctx.Name_resolution.typedefs acc
    in
    let ctx_modules =
      let rec aux mctx =
        Ident.Map.fold
          (fun _ m (M acc) ->
            let sub = aux (ModuleName.Map.find m ctxt.modules) in
            M (ModuleName.Map.add m sub acc))
          mctx.used_modules (M ModuleName.Map.empty)
      in
      aux ctxt.local
    in
    {
      ctx_structs = ctxt.structs;
      ctx_enums = ctxt.enums;
      ctx_scopes =
        ModuleName.Map.fold
          (fun _ -> ctx_scopes)
          ctxt.modules
          (ctx_scopes ctxt.local ScopeName.Map.empty);
      ctx_topdefs = ctxt.topdef_types;
      ctx_struct_fields = ctxt.local.field_idmap;
      ctx_enum_constrs = ctxt.local.constructor_idmap;
      ctx_scope_index =
        Ident.Map.filter_map
          (fun _ -> function
            | Name_resolution.TScope (s, _) -> Some s
            | _ -> None)
          ctxt.local.typedefs;
      ctx_modules;
    }
  in
  let desugared =
    {
      Ast.program_lang = surface.program_lang;
      Ast.program_module_name = surface.Surface.Ast.program_module_name;
      Ast.program_modules;
      Ast.program_ctx;
      Ast.program_root =
        {
          Ast.module_scopes = get_scopes ctxt.Name_resolution.local;
          Ast.module_topdefs = TopdefName.Map.empty;
        };
    }
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
    | S.ModuleDef _ | S.LawInclude _ | S.LawText _ | S.ModuleUse _ -> prgm
  in
  List.fold_left process_structure desugared surface.S.program_items
