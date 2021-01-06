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

(** Translation from {!module: Surface.Ast} to {!module: Desugaring.Ast}.

    - Removes syntactic sugars
    - Separate code from legislation *)

module Pos = Utils.Pos
module Errors = Utils.Errors
module Cli = Utils.Cli

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

let translate_unop (op : Ast.unop) : Dcalc.Ast.unop =
  match op with Not -> Not | Minus l -> Minus (translate_op_kind l)

(** The two modules below help performing operations on map with the {!type: Bindlib.box}. Indeed,
    Catala uses the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library to represent bound
    variables in the AST. In this translation, bound variables are used to represent function
    parameters or pattern macthing bindings. *)

module LiftStructFieldMap = Bindlib.Lift (Scopelang.Ast.StructFieldMap)
module LiftEnumConstructorMap = Bindlib.Lift (Scopelang.Ast.EnumConstructorMap)

(** Usage: [translate_expr scope ctxt expr]

    Translates [expr] into its desugared equivalent. [scope] is used to disambiguate the scope and
    subscopes variables than occur in the expresion *)
let rec translate_expr (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    ((expr, pos) : Ast.expression Pos.marked) : Scopelang.Ast.expr Pos.marked Bindlib.box =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  let rec_helper = translate_expr scope ctxt in
  match expr with
  | IfThenElse (e_if, e_then, e_else) ->
      Bindlib.box_apply3
        (fun e_if e_then e_else -> (Scopelang.Ast.EIfThenElse (e_if, e_then, e_else), pos))
        (rec_helper e_if) (rec_helper e_then) (rec_helper e_else)
  | Binop (op, e1, e2) ->
      let op_term =
        Pos.same_pos_as (Scopelang.Ast.EOp (Dcalc.Ast.Binop (translate_binop (Pos.unmark op)))) op
      in
      Bindlib.box_apply2
        (fun e1 e2 -> (Scopelang.Ast.EApp (op_term, [ e1; e2 ]), pos))
        (rec_helper e1) (rec_helper e2)
  | Unop (op, e) ->
      let op_term =
        Pos.same_pos_as (Scopelang.Ast.EOp (Dcalc.Ast.Unop (translate_unop (Pos.unmark op)))) op
      in
      Bindlib.box_apply (fun e -> (Scopelang.Ast.EApp (op_term, [ e ]), pos)) (rec_helper e)
  | Literal l ->
      let untyped_term =
        match l with
        | Number ((Int i, _), None) -> Scopelang.Ast.ELit (Dcalc.Ast.LInt i)
        | Number ((Int i, _), Some (Percent, _)) ->
            Scopelang.Ast.ELit (Dcalc.Ast.LRat (Q.div (Q.of_bigint i) (Q.of_int 100)))
        | Number ((Dec (i, f), _), None) ->
            let digits_f =
              try int_of_float (ceil (float_of_int (Z.log2 f) *. log 2.0 /. log 10.0))
              with Invalid_argument _ -> 0
            in
            Scopelang.Ast.ELit
              (Dcalc.Ast.LRat
                 Q.(of_bigint i + (of_bigint f / of_bigint (Z.pow (Z.of_int 10) digits_f))))
        | Number ((Dec (i, f), _), Some (Percent, _)) ->
            let digits_f =
              try int_of_float (ceil (float_of_int (Z.log2 f) *. log 2.0 /. log 10.0))
              with Invalid_argument _ -> 0
            in
            Scopelang.Ast.ELit
              (Dcalc.Ast.LRat
                 (Q.div
                    Q.(of_bigint i + (of_bigint f / of_bigint (Z.pow (Z.of_int 10) digits_f)))
                    (Q.of_int 100)))
        | Bool b -> Scopelang.Ast.ELit (Dcalc.Ast.LBool b)
        | MoneyAmount i ->
            Scopelang.Ast.ELit
              (Dcalc.Ast.LMoney Z.((i.money_amount_units * of_int 100) + i.money_amount_cents))
        | Number ((Int i, _), Some (Year, _)) ->
            Scopelang.Ast.ELit (Dcalc.Ast.LDuration Z.(of_int 365 * i))
        | Number ((Int i, _), Some (Month, _)) ->
            Scopelang.Ast.ELit (Dcalc.Ast.LDuration Z.(of_int 30 * i))
        | Number ((Int i, _), Some (Day, _)) -> Scopelang.Ast.ELit (Dcalc.Ast.LDuration i)
        | Number ((Dec (_, _), _), Some ((Year | Month | Day), _)) ->
            Errors.raise_spanned_error
              "Impossible to specify decimal amounts of days, months or years" pos
        | Date date -> (
            let date =
              ODate.Unix.make
                ~year:(Pos.unmark date.literal_date_year)
                ~day:(Pos.unmark date.literal_date_day)
                ~month:
                  ( try ODate.Month.of_int (Pos.unmark date.literal_date_month)
                    with Failure _ ->
                      Errors.raise_spanned_error "Invalid month (should be between 1 and 12)"
                        (Pos.get_position date.literal_date_month) )
                ()
            in
            match ODate.Unix.some_if_valid date with
            | Some date -> Scopelang.Ast.ELit (Dcalc.Ast.LDate date)
            | None -> Errors.raise_spanned_error "Invalid date" pos )
      in
      Bindlib.box (untyped_term, pos)
  | Ident x -> (
      (* first we check whether this is a local var, then we resort to scope-wide variables *)
      match Desugared.Ast.IdentMap.find_opt x ctxt.local_var_idmap with
      | None -> (
          match Desugared.Ast.IdentMap.find_opt x scope_ctxt.var_idmap with
          | Some uid -> Bindlib.box (Scopelang.Ast.ELocation (ScopeVar (uid, pos)), pos)
          | None ->
              Name_resolution.raise_unknown_identifier "for a local or scope-wide variable" (x, pos)
          )
      | Some uid ->
          Scopelang.Ast.make_var (uid, pos) (* the whole box thing is to accomodate for this case *)
      )
  | Dotted (e, x) -> (
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
            ( Scopelang.Ast.ELocation
                (SubScopeVar (subscope_real_uid, (subscope_uid, pos), (subscope_var_uid, pos))),
              pos )
      | _ ->
          (* In this case e.x is the struct field x access of expression e *)
          let e = translate_expr scope ctxt e in
          let x_possible_structs =
            try Desugared.Ast.IdentMap.find (Pos.unmark x) ctxt.field_idmap
            with Not_found ->
              Errors.raise_spanned_error "This identifier should refer to a struct field"
                (Pos.get_position x)
          in
          if Scopelang.Ast.StructMap.cardinal x_possible_structs > 1 then
            Errors.raise_spanned_error
              (Format.asprintf
                 "This struct field name is ambiguous, it can belong to %a. Desambiguate it by \
                  prefixing it with the struct name."
                 (Format.pp_print_list
                    ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                    (fun fmt (s_name, _) ->
                      Format.fprintf fmt "%a" Scopelang.Ast.StructName.format_t s_name))
                 (Scopelang.Ast.StructMap.bindings x_possible_structs))
              (Pos.get_position x)
          else
            let s_uid, f_uid = Scopelang.Ast.StructMap.choose x_possible_structs in
            Bindlib.box_apply (fun e -> (Scopelang.Ast.EStructAccess (e, f_uid, s_uid), pos)) e )
  | FunCall (f, arg) ->
      Bindlib.box_apply2
        (fun f arg -> (Scopelang.Ast.EApp (f, [ arg ]), pos))
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
            ( match Scopelang.Ast.StructFieldMap.find_opt f_uid s_fields with
            | None -> ()
            | Some e_field ->
                Errors.raise_multispanned_error
                  (Format.asprintf "The field %a has been defined twice:"
                     Scopelang.Ast.StructFieldName.format_t f_uid)
                  [ (None, Pos.get_position f_e); (None, Pos.get_position (Bindlib.unbox e_field)) ]
            );
            let f_e = translate_expr scope ctxt f_e in
            Scopelang.Ast.StructFieldMap.add f_uid f_e s_fields)
          Scopelang.Ast.StructFieldMap.empty fields
      in
      Bindlib.box_apply
        (fun s_fields -> (Scopelang.Ast.EStruct (s_uid, s_fields), pos))
        (LiftStructFieldMap.lift_box s_fields)
  | EnumInject (constructor, payload) ->
      let possible_c_uids =
        try Desugared.Ast.IdentMap.find (Pos.unmark constructor) ctxt.constructor_idmap
        with Not_found ->
          Errors.raise_spanned_error
            "The name of this constructor has not been defined before, maybe it is a typo?"
            (Pos.get_position constructor)
      in
      if Scopelang.Ast.EnumMap.cardinal possible_c_uids > 1 then
        Errors.raise_spanned_error
          (Format.asprintf
             "This constuctor name is ambiguous, it can belong to %a. Desambiguate it by prefixing \
              it with the enum name."
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                (fun fmt (s_name, _) ->
                  Format.fprintf fmt "%a" Scopelang.Ast.EnumName.format_t s_name))
             (Scopelang.Ast.EnumMap.bindings possible_c_uids))
          (Pos.get_position constructor)
      else
        let e_uid, c_uid = Scopelang.Ast.EnumMap.choose possible_c_uids in
        let payload = Option.map (translate_expr scope ctxt) payload in
        Bindlib.box_apply
          (fun payload ->
            ( Scopelang.Ast.EEnumInj
                ( ( match payload with
                  | Some e' -> e'
                  | None -> (Scopelang.Ast.ELit Dcalc.Ast.LUnit, Pos.get_position constructor) ),
                  c_uid,
                  e_uid ),
              pos ))
          (Bindlib.box_opt payload)
  | MatchWith (e1, (cases, _cases_pos)) ->
      let e1 = translate_expr scope ctxt e1 in
      let cases_d, e_uid =
        List.fold_left
          (fun (cases_d, e_uid) (case, pos_case) ->
            match Pos.unmark case.Ast.match_case_pattern with
            | [ constructor ], binding ->
                let possible_c_uids =
                  try Desugared.Ast.IdentMap.find (Pos.unmark constructor) ctxt.constructor_idmap
                  with Not_found ->
                    Errors.raise_spanned_error
                      "The name of this constructor has not been defined before, maybe it is a \
                       typo?"
                      (Pos.get_position constructor)
                in
                if e_uid = None && Scopelang.Ast.EnumMap.cardinal possible_c_uids > 1 then
                  Errors.raise_spanned_error
                    (Format.asprintf
                       "This constuctor name is ambiguous, it can belong to %a. Desambiguate it by \
                        prefixing it with the enum name."
                       (Format.pp_print_list
                          ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
                          (fun fmt (s_name, _) ->
                            Format.fprintf fmt "%a" Scopelang.Ast.EnumName.format_t s_name))
                       (Scopelang.Ast.EnumMap.bindings possible_c_uids))
                    (Pos.get_position constructor)
                else
                  let e_uid, c_uid =
                    match e_uid with
                    | Some e_uid -> (
                        ( e_uid,
                          try Scopelang.Ast.EnumMap.find e_uid possible_c_uids
                          with Not_found ->
                            Errors.raise_spanned_error
                              (Format.asprintf "This constructor is not part of the %a enumeration"
                                 Scopelang.Ast.EnumName.format_t e_uid)
                              (Pos.get_position constructor) ) )
                    | None -> Scopelang.Ast.EnumMap.choose possible_c_uids
                  in
                  ( match Scopelang.Ast.EnumConstructorMap.find_opt c_uid cases_d with
                  | None -> ()
                  | Some e_case ->
                      Errors.raise_multispanned_error
                        (Format.asprintf "The constructor %a has been matched twice:"
                           Scopelang.Ast.EnumConstructor.format_t c_uid)
                        [
                          (None, Pos.get_position case.match_case_expr);
                          (None, Pos.get_position (Bindlib.unbox e_case));
                        ] );
                  let ctxt, (param_var, param_pos) =
                    match binding with
                    | None -> (ctxt, (Scopelang.Ast.Var.make ("_", Pos.no_pos), Pos.no_pos))
                    | Some param ->
                        let ctxt, param_var = Name_resolution.add_def_local_var ctxt param in
                        (ctxt, (param_var, Pos.get_position param))
                  in
                  let case_body = translate_expr scope ctxt case.Ast.match_case_expr in
                  let e_binder = Bindlib.bind_mvar (Array.of_list [ param_var ]) case_body in
                  let case_expr =
                    Bindlib.box_apply2
                      (fun e_binder case_body ->
                        Pos.same_pos_as
                          (Scopelang.Ast.EAbs
                             ( param_pos,
                               e_binder,
                               [
                                 Scopelang.Ast.EnumConstructorMap.find c_uid
                                   (Scopelang.Ast.EnumMap.find e_uid ctxt.Name_resolution.enums);
                               ] ))
                          case_body)
                      e_binder case_body
                  in
                  (Scopelang.Ast.EnumConstructorMap.add c_uid case_expr cases_d, Some e_uid)
            | _ :: _, _ ->
                Errors.raise_spanned_error
                  "The deep pattern matching syntactic sugar is not yet supported" pos_case
            | [], _ -> assert false
            (* should not happen *))
          (Scopelang.Ast.EnumConstructorMap.empty, None)
          cases
      in
      Bindlib.box_apply2
        (fun e1 cases_d -> (Scopelang.Ast.EMatch (e1, Option.get e_uid, cases_d), pos))
        e1
        (LiftEnumConstructorMap.lift_box cases_d)
  | ArrayLit es ->
      Bindlib.box_apply
        (fun es -> (Scopelang.Ast.EArray es, pos))
        (Bindlib.box_list (List.map rec_helper es))
  | CollectionOp (op', param', collection, predicate) ->
      let ctxt, param = Name_resolution.add_def_local_var ctxt param' in
      let collection = rec_helper collection in
      let init =
        match Pos.unmark op' with
        | Ast.Exists ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LBool false), Pos.get_position op')
        | Ast.Forall -> Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LBool true), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Integer) ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LInt Z.zero), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LRat Q.zero), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Money) ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LMoney Z.zero), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LDuration Z.zero), Pos.get_position op')
        | Ast.Aggregate (Ast.AggregateExtremum (_, _, init)) -> rec_helper init
        | Ast.Aggregate (Ast.AggregateSum t) ->
            Errors.raise_spanned_error
              (Format.asprintf "It is impossible to sum two values of type %a together"
                 Print.format_primitive_typ t)
              pos
        | Ast.Aggregate Ast.AggregateCount ->
            Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LInt Z.zero), Pos.get_position op')
      in
      let acc_var = Scopelang.Ast.Var.make ("acc", Pos.get_position param') in
      let acc = Scopelang.Ast.make_var (acc_var, Pos.get_position param') in
      let f_body =
        let make_body (op : Dcalc.Ast.binop) =
          Bindlib.box_apply2
            (fun predicate acc ->
              ( Scopelang.Ast.EApp
                  ( (Scopelang.Ast.EOp (Dcalc.Ast.Binop op), Pos.get_position op'),
                    [ acc; predicate ] ),
                pos ))
            (translate_expr scope ctxt predicate)
            acc
        in
        let make_extr_body (cmp_op : Dcalc.Ast.binop) =
          Bindlib.box_apply2
            (fun predicate acc ->
              ( Scopelang.Ast.EIfThenElse
                  ( ( Scopelang.Ast.EApp
                        ( (Scopelang.Ast.EOp (Dcalc.Ast.Binop cmp_op), Pos.get_position op'),
                          [ acc; predicate ] ),
                      pos ),
                    acc,
                    predicate ),
                pos ))
            (translate_expr scope ctxt predicate)
            acc
        in
        match Pos.unmark op' with
        | Ast.Exists -> make_body Dcalc.Ast.Or
        | Ast.Forall -> make_body Dcalc.Ast.And
        | Ast.Aggregate (Ast.AggregateSum Ast.Integer) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KInt)
        | Ast.Aggregate (Ast.AggregateSum Ast.Decimal) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KRat)
        | Ast.Aggregate (Ast.AggregateSum Ast.Money) -> make_body (Dcalc.Ast.Add Dcalc.Ast.KMoney)
        | Ast.Aggregate (Ast.AggregateSum Ast.Duration) ->
            make_body (Dcalc.Ast.Add Dcalc.Ast.KDuration)
        | Ast.Aggregate (Ast.AggregateSum _) -> assert false (* should not happen *)
        | Ast.Aggregate (Ast.AggregateExtremum (max_or_min, t, _)) ->
            let op_kind =
              match t with
              | Ast.Integer -> Dcalc.Ast.KInt
              | Ast.Decimal -> Dcalc.Ast.KRat
              | Ast.Money -> Dcalc.Ast.KMoney
              | Ast.Duration -> Dcalc.Ast.KDuration
              | _ -> assert false
            in
            let cmp_op = if max_or_min then Dcalc.Ast.Gt op_kind else Dcalc.Ast.Lt op_kind in
            make_extr_body cmp_op
        | Ast.Aggregate Ast.AggregateCount ->
            Bindlib.box_apply2
              (fun predicate acc ->
                ( Scopelang.Ast.EIfThenElse
                    ( predicate,
                      ( Scopelang.Ast.EApp
                          ( ( Scopelang.Ast.EOp (Dcalc.Ast.Binop (Dcalc.Ast.Add Dcalc.Ast.KInt)),
                              Pos.get_position op' ),
                            [
                              acc;
                              (Scopelang.Ast.ELit (Dcalc.Ast.LInt Z.one), Pos.get_position predicate);
                            ] ),
                        pos ),
                      acc ),
                  pos ))
              (translate_expr scope ctxt predicate)
              acc
      in
      let f =
        let make_f (t : Dcalc.Ast.typ_lit) =
          Bindlib.box_apply
            (fun binder ->
              ( Scopelang.Ast.EAbs
                  ( pos,
                    binder,
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
          ( Scopelang.Ast.EApp
              ((Scopelang.Ast.EOp (Dcalc.Ast.Ternop Dcalc.Ast.Fold), pos), [ f; init; collection ]),
            pos ))
        f collection init
  | MemCollection (member, collection) ->
      let param_var = Scopelang.Ast.Var.make ("collection_member", pos) in
      let param = Scopelang.Ast.make_var (param_var, pos) in
      let collection = rec_helper collection in
      let init = Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LBool false), pos) in
      let acc_var = Scopelang.Ast.Var.make ("acc", pos) in
      let acc = Scopelang.Ast.make_var (acc_var, pos) in
      let f_body =
        Bindlib.box_apply3
          (fun member acc param ->
            ( Scopelang.Ast.EApp
                ( (Scopelang.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.Or), pos),
                  [
                    ( Scopelang.Ast.EApp
                        ((Scopelang.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.Eq), pos), [ member; param ]),
                      pos );
                    acc;
                  ] ),
              pos ))
          (translate_expr scope ctxt member)
          acc param
      in
      let f =
        Bindlib.box_apply
          (fun binder ->
            ( Scopelang.Ast.EAbs
                ( pos,
                  binder,
                  [ (Scopelang.Ast.TLit Dcalc.Ast.TBool, pos); (Scopelang.Ast.TAny, pos) ] ),
              pos ))
          (Bindlib.bind_mvar [| acc_var; param_var |] f_body)
      in
      Bindlib.box_apply3
        (fun f collection init ->
          ( Scopelang.Ast.EApp
              ((Scopelang.Ast.EOp (Dcalc.Ast.Ternop Dcalc.Ast.Fold), pos), [ f; init; collection ]),
            pos ))
        f collection init
  | Builtin IntToDec -> Bindlib.box (Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.IntToRat), pos)
  | Builtin Cardinal -> Bindlib.box (Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.Length), pos)
  | Builtin GetDay -> Bindlib.box (Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetDay), pos)
  | Builtin GetMonth -> Bindlib.box (Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetMonth), pos)
  | Builtin GetYear -> Bindlib.box (Scopelang.Ast.EOp (Dcalc.Ast.Unop Dcalc.Ast.GetYear), pos)
  | _ ->
      Name_resolution.raise_unsupported_feature "desugaring not implemented for this expression" pos

(** {1 Translating scope definitions} *)

(** A scope use can be annotated with a pervasive precondition, in which case this precondition has
    to be appended to the justifications of each definition in the subscope use. This is what this
    function does. *)
let merge_conditions (precond : Scopelang.Ast.expr Pos.marked Bindlib.box option)
    (cond : Scopelang.Ast.expr Pos.marked Bindlib.box option) (default_pos : Pos.t) :
    Scopelang.Ast.expr Pos.marked Bindlib.box =
  match (precond, cond) with
  | Some precond, Some cond ->
      let op_term =
        (Scopelang.Ast.EOp (Dcalc.Ast.Binop Dcalc.Ast.And), Pos.get_position (Bindlib.unbox precond))
      in
      Bindlib.box_apply2
        (fun precond cond ->
          (Scopelang.Ast.EApp (op_term, [ precond; cond ]), Pos.get_position precond))
        precond cond
  | Some cond, None | None, Some cond -> cond
  | None, None -> Bindlib.box (Scopelang.Ast.ELit (Dcalc.Ast.LBool true), default_pos)

(** Translates a surface definition into condition into a desugared {!type: Desugared.Ast.rule} *)
let process_default (ctxt : Name_resolution.context) (scope : Scopelang.Ast.ScopeName.t)
    (def_key : Desugared.Ast.ScopeDef.t Pos.marked)
    (param_uid : Scopelang.Ast.Var.t Pos.marked option)
    (precond : Scopelang.Ast.expr Pos.marked Bindlib.box option)
    (exception_to_rule : Desugared.Ast.RuleName.t option) (just : Ast.expression Pos.marked option)
    (cons : Ast.expression Pos.marked) : Desugared.Ast.rule =
  let just = match just with Some just -> Some (translate_expr scope ctxt just) | None -> None in
  let just = merge_conditions precond just (Pos.get_position def_key) in
  let cons = translate_expr scope ctxt cons in
  {
    just;
    cons;
    parameter =
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
    exception_to_rule;
  }

(** Wrapper around {!val: process_default} that performs some name disambiguation *)
let process_def (precond : Scopelang.Ast.expr Pos.marked Bindlib.box option)
    (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (def : Ast.definition) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope = Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  let default_pos = Pos.get_position def.definition_expr in
  let def_key =
    match Pos.unmark def.definition_name with
    | [ x ] ->
        let x_uid = Name_resolution.get_var_uid scope_uid ctxt x in
        Desugared.Ast.ScopeDef.Var x_uid
    | [ y; x ] ->
        let subscope_uid : Scopelang.Ast.SubScopeName.t =
          Name_resolution.get_subscope_uid scope_uid ctxt y
        in
        let subscope_real_uid : Scopelang.Ast.ScopeName.t =
          Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
        in
        let x_uid = Name_resolution.get_var_uid subscope_real_uid ctxt x in
        Desugared.Ast.ScopeDef.SubScopeVar (subscope_uid, x_uid)
    | _ -> Errors.raise_spanned_error "Structs are not handled yet" default_pos
  in
  (* We add to the name resolution context the name of the parameter variable *)
  let param_uid, new_ctxt =
    match def.definition_parameter with
    | None -> (None, ctxt)
    | Some param ->
        let ctxt, param_var = Name_resolution.add_def_local_var ctxt param in
        (Some (Pos.same_pos_as param_var param), ctxt)
  in
  let scope_updated =
    let x_def, x_type, is_cond =
      match Desugared.Ast.ScopeDefMap.find_opt def_key scope.scope_defs with
      | Some def -> def
      | None ->
          ( Desugared.Ast.RuleMap.empty,
            Name_resolution.get_def_typ ctxt def_key,
            Name_resolution.is_def_cond ctxt def_key )
    in
    let rule_name =
      match def.Ast.definition_label with
      | None -> None
      | Some label -> Some (Desugared.Ast.IdentMap.find (Pos.unmark label) scope_ctxt.label_idmap)
    in
    let rule_name =
      match rule_name with
      | Some x -> x
      | None ->
          Desugared.Ast.RuleName.fresh
            (Pos.map_under_mark
               (fun qident -> String.concat "." (List.map (fun i -> Pos.unmark i) qident))
               def.definition_name)
    in
    let parent_rule =
      match def.Ast.definition_exception_to with
      | None -> None
      | Some label ->
          Some
            ( try Desugared.Ast.IdentMap.find (Pos.unmark label) scope_ctxt.label_idmap
              with Not_found ->
                Errors.raise_spanned_error
                  (Format.asprintf "Unknown label: \"%s\"" (Pos.unmark label))
                  (Pos.get_position label) )
    in
    let x_def =
      Desugared.Ast.RuleMap.add rule_name
        (process_default new_ctxt scope_uid
           (def_key, Pos.get_position def.definition_name)
           param_uid precond parent_rule def.definition_condition def.definition_expr)
        x_def
    in
    {
      scope with
      scope_defs = Desugared.Ast.ScopeDefMap.add def_key (x_def, x_type, is_cond) scope.scope_defs;
    }
  in
  {
    prgm with
    program_scopes = Scopelang.Ast.ScopeMap.add scope_uid scope_updated prgm.program_scopes;
  }

(** Translates a {!type: Surface.Ast.rule} from the surface language *)
let process_rule (precond : Scopelang.Ast.expr Pos.marked Bindlib.box option)
    (scope : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (rule : Ast.rule) : Desugared.Ast.program =
  let consequence_expr = Ast.Literal (Ast.Bool (Pos.unmark rule.rule_consequence)) in
  let def =
    {
      Ast.definition_label = rule.rule_label;
      Ast.definition_exception_to = rule.rule_exception_to;
      Ast.definition_name = rule.rule_name;
      Ast.definition_parameter = rule.rule_parameter;
      Ast.definition_condition = rule.rule_condition;
      Ast.definition_expr = (consequence_expr, Pos.get_position rule.rule_consequence);
    }
  in
  process_def precond scope ctxt prgm def

(** Translates assertions *)
let process_assert (precond : Scopelang.Ast.expr Pos.marked Bindlib.box option)
    (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : Name_resolution.context)
    (prgm : Desugared.Ast.program) (ass : Ast.assertion) : Desugared.Ast.program =
  let scope : Desugared.Ast.scope = Scopelang.Ast.ScopeMap.find scope_uid prgm.program_scopes in
  let ass =
    translate_expr scope_uid ctxt
      ( match ass.Ast.assertion_condition with
      | None -> ass.Ast.assertion_content
      | Some cond ->
          ( Ast.IfThenElse
              (cond, ass.Ast.assertion_content, Pos.same_pos_as (Ast.Literal (Ast.Bool true)) cond),
            Pos.get_position cond ) )
  in
  let ass =
    match precond with
    | Some precond ->
        Bindlib.box_apply2
          (fun precond ass ->
            ( Scopelang.Ast.EIfThenElse
                (precond, ass, Pos.same_pos_as (Scopelang.Ast.ELit (Dcalc.Ast.LBool true)) precond),
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
  let precond = Option.map (translate_expr scope ctxt) precond in
  match Pos.unmark item with
  | Ast.Rule rule -> process_rule precond scope ctxt prgm rule
  | Ast.Definition def -> process_def precond scope ctxt prgm def
  | Ast.Assertion ass -> process_assert precond scope ctxt prgm ass
  | _ -> prgm

(** {1 Translating top-level items} *)

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
  List.fold_left (process_scope_use_item precond scope_uid ctxt) prgm use.scope_use_items

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
                  (fun _ v acc -> Scopelang.Ast.ScopeVarSet.add v acc)
                  s_context.Name_resolution.var_idmap Scopelang.Ast.ScopeVarSet.empty;
              Desugared.Ast.scope_sub_scopes = s_context.Name_resolution.sub_scopes;
              Desugared.Ast.scope_defs =
                Desugared.Ast.IdentMap.fold
                  (fun _ v acc ->
                    let x, y = Scopelang.Ast.ScopeVarMap.find v ctxt.Name_resolution.var_typs in
                    Desugared.Ast.ScopeDefMap.add (Desugared.Ast.ScopeDef.Var v)
                      (Desugared.Ast.RuleMap.empty, x, y)
                      acc)
                  s_context.Name_resolution.var_idmap Desugared.Ast.ScopeDefMap.empty;
              Desugared.Ast.scope_assertions = [];
              Desugared.Ast.scope_meta_assertions = [];
              Desugared.Ast.scope_uid = s_uid;
            })
          ctxt.Name_resolution.scopes;
    }
  in
  let processer_article_item (prgm : Desugared.Ast.program) (item : Ast.law_article_item) :
      Desugared.Ast.program =
    match item with
    | CodeBlock (block, _) ->
        List.fold_left
          (fun prgm item ->
            match Pos.unmark item with
            | Ast.ScopeUse use -> process_scope_use ctxt prgm use
            | _ -> prgm)
          prgm block
    | _ -> prgm
  in
  let rec processer_structure (prgm : Desugared.Ast.program) (item : Ast.law_structure) :
      Desugared.Ast.program =
    match item with
    | LawHeading (_, children) ->
        List.fold_left (fun prgm child -> processer_structure prgm child) prgm children
    | LawArticle (_, children) ->
        List.fold_left (fun prgm child -> processer_article_item prgm child) prgm children
    | MetadataBlock (b, c) -> processer_article_item prgm (CodeBlock (b, c))
    | IntermediateText _ | LawInclude _ -> prgm
  in

  let processer_item (prgm : Desugared.Ast.program) (item : Ast.program_item) :
      Desugared.Ast.program =
    match item with LawStructure s -> processer_structure prgm s
  in

  List.fold_left processer_item empty_prgm prgm.program_items
