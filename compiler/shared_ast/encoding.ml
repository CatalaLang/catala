(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
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

open Catala_utils
open Definitions
module Runtime = Catala_runtime
open Json_encoding

let bool_encoding : Runtime.runtime_value encoding =
  conv
    (function Runtime.Bool b -> b | _ -> assert false)
    (fun b -> Runtime.Bool b)
    bool

let unit_encoding : Runtime.runtime_value encoding =
  conv
    (function Runtime.Unit -> () | _ -> assert false)
    (fun () -> Runtime.Unit)
    empty

let int_encoding : Runtime.runtime_value encoding =
  union
    [
      case int
        (function Runtime.Integer z -> Some (Z.to_int z) | _ -> None)
        (fun i -> Runtime.Integer (Z.of_int i));
      case string
        (function Runtime.Integer z -> Some (Z.to_string z) | _ -> None)
        (fun s -> Runtime.Integer (Z.of_string s));
    ]

let money_encoding : Runtime.runtime_value encoding =
  let two53 = Z.(shift_left one 53) in
  let z_100 = Z.of_int 100 in
  let q_100 = Q.of_int 100 in
  union
    [
      case int53
        (function
          | Runtime.Money z
            when Z.lt z two53 && Z.gt z (Z.neg two53) && Z.rem z z_100 = Z.zero
            ->
            Some Z.(div z z_100 |> to_int64)
          | _ -> None)
        (fun i -> Runtime.Money Z.(mul (of_int64 i) z_100));
      case float
        (function Runtime.Money z -> Some (Z.to_float z /. 100.) | _ -> None)
        (fun i ->
          let z = Z.of_float (i *. 100.) in
          Runtime.Money z);
      case string
        (function
          | Runtime.Money z ->
            let z = Q.div (Q.of_bigint z) q_100 in
            Some (Q.to_string z)
          | _ -> None)
        (fun s ->
          let q = Q.(of_string s |> mul q_100) in
          Runtime.Money (Q.to_bigint q));
    ]

(* TODO: allow more patterns *)
let rat_encoding : Runtime.runtime_value encoding =
  conv
    (function Runtime.Decimal d -> Q.to_float d | _ -> assert false)
    (fun f -> Runtime.Decimal (Q.of_float f))
    float

let date_encoding : Runtime.runtime_value encoding =
  conv
    (function
      | Runtime.Date d -> Format.asprintf "%a" Dates_calc.format_date d
      | _ -> assert false)
    (fun s -> Runtime.Date (Dates_calc.date_of_string s))
    string

let duration_encoding : Runtime.runtime_value encoding =
  conv
    (function
      | Runtime.Duration d -> Format.asprintf "%a" Dates_calc.format_period d
      | _ -> assert false)
    (fun s -> Runtime.Duration (Dates_calc.period_of_string s))
    string

let make_constant s : Runtime.runtime_value encoding =
  conv
    (function Runtime.Unit -> () | _ -> assert false)
    (fun () -> Unit)
    (constant s)

let generate_lit_encoding (typ_lit : typ_lit) : Runtime.runtime_value encoding =
  match typ_lit with
  | TBool -> bool_encoding
  | TUnit -> unit_encoding
  | TInt -> int_encoding
  | TRat -> rat_encoding
  | TDate -> date_encoding
  | TDuration -> duration_encoding
  | TPos -> unit_encoding
  | TMoney -> money_encoding

let rec generate_encoder (ctx : decl_ctx) (typ : typ) :
    Runtime.runtime_value encoding =
  match Mark.remove typ with
  | TError -> assert false
  | TLit tlit -> generate_lit_encoding tlit
  | TTuple [typ; (TLit TPos, _)] -> generate_encoder ctx typ
  | TTuple tl -> generate_tuple_encoder ctx tl
  | TStruct sname -> generate_struct_encoder ctx sname
  | TEnum ename -> generate_enum_encoder ctx ename
  | TOption typ -> generate_option_encoder ctx typ
  | TArray typ -> generate_array_encoder ctx typ
  | TArrow _ -> Message.error "Cannot encode 'function' types"
  | TDefault _ -> Message.error "Cannot encode 'default' types"
  | TVar _ -> Message.error "Cannot encode 'variable' types"
  | TForAll _ -> Message.error "Cannot encode 'for-all' types"
  | TClosureEnv -> Message.error "Cannot encode 'closure-env' types"

and generate_array_encoder ctx typ : Runtime.runtime_value encoding =
  let open Runtime in
  conv
    (function Array a -> a | _ -> assert false)
    (fun a -> Array a)
    (array (generate_encoder ctx typ))

and generate_option_encoder ctx typ =
  let open Runtime in
  let proj_none = function
    | Enum ("Optional", ("Absent", Unit)) -> Some Unit
    | _ -> None
  in
  let inj_none _ = Enum ("Optional", ("Absent", Unit)) in
  union
    [
      case unit_encoding proj_none inj_none;
      case (make_constant "Absent") proj_none inj_none;
      case
        (obj1 (req "Present" (generate_encoder ctx typ)))
        (function Enum ("Optional", ("Present", x)) -> Some x | _ -> None)
        (fun x -> Enum ("Optional", ("Present", x)));
    ]

and generate_tuple_encoder ctx typl =
  assert (typl <> []);
  let first_tup_enc = tup1 (generate_encoder ctx (List.hd typl)) in
  let add_tuple (encoding : Runtime.runtime_value encoding) typ :
      Runtime.runtime_value encoding =
    let bconv = merge_tups encoding (tup1 (generate_encoder ctx typ)) in
    conv
      (function
        | Runtime.Tuple arr ->
          ( Runtime.Tuple (Array.sub arr 0 (Array.length arr - 1)),
            arr.(Array.length arr - 1) )
        | _ -> assert false)
      (function
        | Runtime.Tuple arr, rval -> Runtime.Tuple (Array.append arr [| rval |])
        | _ -> assert false)
      bconv
  in
  List.fold_left
    (fun e (sf, typ) -> add_tuple e (sf, typ))
    first_tup_enc (List.tl typl)

and generate_struct_encoder (ctx : decl_ctx) (sname : StructName.t) =
  let struc = StructName.Map.find sname ctx.ctx_structs in
  let bdgs = StructField.Map.bindings struc in
  let is_input_scope_struct =
    ScopeName.Map.exists
      (fun _ { in_struct_name; _ } -> StructName.equal sname in_struct_name)
      ctx.ctx_scopes
  in
  let rename_field f =
    let field_s = StructField.to_string f in
    if
      is_input_scope_struct
      && String.ends_with ~suffix:"_in" field_s
      && String.length field_s > 3
    then String.sub field_s 0 (String.length field_s - 3), field_s
    else field_s, field_s
  in
  let empty_struct_enc =
    conv
      (fun _ -> ())
      (fun () -> Runtime.Struct (StructName.to_string sname, []))
      empty
  in
  let add_req_field (encoding : Runtime.runtime_value encoding) (sf, typ) :
      Runtime.runtime_value encoding =
    let field_label, field_s = rename_field sf in
    let bconv =
      merge_objs encoding (obj1 (req field_label (generate_encoder ctx typ)))
    in
    conv
      (function
        | Runtime.Struct (s, lvals) ->
          let rval = List.assoc field_s lvals in
          Runtime.Struct (s, List.remove_assoc field_s lvals), rval
        | _ -> assert false)
      (function
        | Runtime.Struct (s, lvals), rval ->
          Runtime.Struct (s, (field_s, rval) :: lvals)
        | _ -> assert false)
      bconv
  in
  let add_opt_field (encoding : Runtime.runtime_value encoding) (sf, typ) :
      Runtime.runtime_value encoding =
    let field_label, field_s = rename_field sf in
    let bconv =
      merge_objs encoding (obj1 (opt field_label (generate_encoder ctx typ)))
    in
    conv
      (function
        | Runtime.Struct (s, lvals) ->
          let rval =
            List.assoc_opt field_s lvals
            |> Option.map (function
                 | Runtime.Enum ("Optional", ("Present", rval)) -> Some rval
                 | Runtime.Enum ("Optional", ("Absent", Unit)) -> None
                 | _ -> assert false)
            |> Option.join
          in
          Runtime.Struct (s, List.remove_assoc field_s lvals), rval
        | _ -> assert false)
      (function
        | Runtime.Struct (s, lvals), None ->
          Runtime.Struct
            (s, (field_s, Enum ("Optional", ("Absent", Unit))) :: lvals)
        | Runtime.Struct (s, lvals), Some rval ->
          Runtime.Struct
            (s, (field_s, Enum ("Optional", ("Present", rval))) :: lvals)
        | _ -> assert false)
      bconv
  in
  List.fold_left
    (fun e (sf, typ) ->
      match Mark.remove typ with
      | TOption typ | TDefault typ -> add_opt_field e (sf, typ)
      | _ -> add_req_field e (sf, typ))
    empty_struct_enc bdgs

and generate_enum_encoder (ctx : decl_ctx) (ename : EnumName.t) =
  let enum = EnumName.Map.find ename ctx.ctx_enums in
  let bdgs = EnumConstructor.Map.bindings enum in
  let make_constructor_case (cstr, typ) : Runtime.runtime_value case =
    match Mark.remove typ with
    | TLit TUnit ->
      case
        (make_constant (EnumConstructor.to_string cstr))
        (function
          | (Unit : Runtime.runtime_value) ->
            Some
              (Enum
                 ( EnumName.to_string ename,
                   (EnumConstructor.to_string cstr, Unit) ))
          | _ -> None)
        (fun _ -> Unit)
    | _ ->
      case
        (obj1 (req (EnumConstructor.to_string cstr) (generate_encoder ctx typ)))
        (fun v ->
          Some
            (Enum (EnumName.to_string ename, (EnumConstructor.to_string cstr, v))))
        (function Enum (_, (_, v)) -> v | _ -> assert false)
  in
  List.map make_constructor_case bdgs |> union

let make_encoding (ctx : decl_ctx) (typ : typ) : Runtime.runtime_value encoding
    =
  generate_encoder ctx typ

let scope_input_encoding scope ctx typ =
  let scope_s = ScopeName.to_string scope in
  let title = Format.sprintf "Scope %s input" scope_s in
  let description = Format.sprintf "Input structure of scope %s" scope_s in
  let encoding = make_encoding ctx typ in
  def (scope_s ^ "_input") ~title ~description encoding

let scope_output_encoding scope ctx typ =
  let scope_s = ScopeName.to_string scope in
  let title = Format.sprintf "Scope %s output" scope_s in
  let description = Format.sprintf "Output structure of scope %s" scope_s in
  let encoding = make_encoding ctx typ in
  def (scope_s ^ "_output") ~title ~description encoding

module Yojson_repr = Json_encoding.Make (Json_repr.Yojson)

let parse_json e json =
  try Yojson_repr.destruct e json
  with e -> Message.error "%a" (fun fmt -> Json_encoding.print_error fmt) e

let rec convert_to_dcalc
    ctx
    (mark : 'm mark)
    (typ : typ)
    (rval : Runtime.runtime_value) : (dcalc, 'm) boxed_gexpr =
  let mark = Expr.with_ty mark typ in
  let f = convert_to_dcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TUnit, Unit -> Expr.elit LUnit mark
  | TLit TBool, Bool b -> Expr.elit (LBool b) mark
  | TLit TMoney, Money z -> Expr.elit (LMoney z) mark
  | TLit TInt, Integer z -> Expr.elit (LInt z) mark
  | TLit TRat, Decimal q -> Expr.elit (LRat q) mark
  | TLit TDate, Date d -> Expr.elit (LDate d) mark
  | TLit TDuration, Duration d -> Expr.elit (LDuration d) mark
  | TDefault _typ, Enum ("Optional", ("Absent", Unit)) -> Expr.eempty mark
  | TDefault typ, Enum ("Optional", ("Present", rval)) ->
    Expr.epuredefault (f typ rval) mark
  | TOption _typ, Enum ("Optional", ("Absent", Unit)) ->
    Expr.einj ~name:Expr.option_enum ~cons:Expr.none_constr
      ~e:(Expr.elit LUnit mark) mark
  | TOption typ, Enum ("Optional", ("Present", rval)) ->
    Expr.einj ~name:Expr.option_enum ~cons:Expr.some_constr ~e:(f typ rval) mark
  | TEnum ename, Enum (_ename, (cstr, v)) ->
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) -> EnumConstructor.to_string cstr' = cstr)
    in
    Expr.einj ~name:ename ~cons ~e:(f typ_v v) mark
  | TStruct sname, Struct (_sname, ls) ->
    let fields =
      let struc = StructName.Map.find sname ctx.ctx_structs in
      let struc_fields = StructField.Map.bindings struc in
      let lookup_field sf =
        List.find (fun (sf', _) -> StructField.to_string sf' = sf) struc_fields
      in
      List.fold_left
        (fun sfm (sf, v) ->
          let sf, typ = lookup_field sf in
          StructField.Map.add sf (f typ v) sfm)
        StructField.Map.empty ls
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, Array a ->
    Expr.earray (Array.to_list a |> List.map (f typ)) mark
  | TTuple typl, Tuple a ->
    Expr.etuple (Array.to_list a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime_value to dcalc: expected value of type %a, got %a"
      Print.typ typ Runtime.format_value r

let rec convert_to_lcalc
    ctx
    (mark : 'm mark)
    (typ : typ)
    (rval : Runtime.runtime_value) : (lcalc, 'm) boxed_gexpr =
  let mark = Expr.with_ty mark typ in
  let f = convert_to_lcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TPos, Unit -> Expr.epos Pos.void mark
  | TLit TUnit, Unit -> Expr.elit LUnit mark
  | TLit TBool, Bool b -> Expr.elit (LBool b) mark
  | TLit TMoney, Money z -> Expr.elit (LMoney z) mark
  | TLit TInt, Integer z -> Expr.elit (LInt z) mark
  | TLit TRat, Decimal q -> Expr.elit (LRat q) mark
  | TLit TDate, Date d -> Expr.elit (LDate d) mark
  | TLit TDuration, Duration d -> Expr.elit (LDuration d) mark
  | TTuple [typ; (TLit TPos, _)], rval ->
    Expr.etuple [f typ rval; Expr.epos Pos.void mark] mark
  | (TDefault _typ | TOption _typ), Enum ("Optional", ("Absent", Unit)) ->
    Expr.einj ~name:Expr.option_enum ~cons:Expr.none_constr
      ~e:(Expr.elit LUnit mark) mark
  | (TDefault typ | TOption typ), Enum ("Optional", ("Present", rval)) ->
    Expr.einj ~name:Expr.option_enum ~cons:Expr.some_constr ~e:(f typ rval) mark
  | TEnum ename, Enum (_ename, (cstr, v)) ->
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) -> EnumConstructor.to_string cstr' = cstr)
    in
    Expr.einj ~name:ename ~cons ~e:(f typ_v v) mark
  | TStruct sname, Struct (_sname, ls) ->
    let fields =
      let struc = StructName.Map.find sname ctx.ctx_structs in
      let struc_fields = StructField.Map.bindings struc in
      let lookup_field sf =
        List.find (fun (sf', _) -> StructField.to_string sf' = sf) struc_fields
      in
      List.fold_left
        (fun sfm (sf, v) ->
          let sf, typ = lookup_field sf in
          StructField.Map.add sf (f typ v) sfm)
        StructField.Map.empty ls
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, Array a ->
    Expr.earray (Array.to_list a |> List.map (f typ)) mark
  | TTuple typl, Tuple a ->
    Expr.etuple (Array.to_list a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime_value to lcalc: expected value of type %a, got %a"
      Print.typ typ Runtime.format_value r

let rec convert_from_gexpr :
    type a. decl_ctx -> (a, 'm) gexpr -> Runtime.runtime_value =
 fun ctx e ->
  let f = convert_from_gexpr ctx in
  match Mark.remove e with
  | ELit LUnit -> Unit
  | ELit (LBool b) -> Bool b
  | ELit (LMoney m) -> Money m
  | ELit (LInt z) -> Integer z
  | ELit (LRat q) -> Decimal q
  | ELit (LDate d) -> Date d
  | ELit (LDuration d) -> Duration d
  | EEmpty -> Enum ("Optional", ("Absent", Unit))
  | EPureDefault e -> Enum ("Optional", ("Present", f e))
  | EInj { name; cons; e = _ }
    when EnumName.equal Expr.option_enum name
         && EnumConstructor.equal cons Expr.none_constr ->
    Enum ("Optional", ("Absent", Unit))
  | EInj { name; cons; e }
    when EnumName.equal Expr.option_enum name
         && EnumConstructor.equal cons Expr.some_constr ->
    Enum ("Optional", ("Present", f e))
  | EInj { name; cons; e } ->
    Enum (EnumName.to_string name, (EnumConstructor.to_string cons, f e))
  | EStruct { name; fields } ->
    Struct
      ( StructName.to_string name,
        StructField.Map.bindings fields
        |> List.map (fun (sf, e) -> StructField.to_string sf, f e) )
  | EArray el -> Array (List.map f el |> Array.of_list)
  | ETuple [e; (EPos _, _)] -> f e
  | ETuple el -> Tuple (List.map f el |> Array.of_list)
  | _ ->
    Message.error "Failed to convert expression to runtime_value: %a"
      (Print.expr ()) e
