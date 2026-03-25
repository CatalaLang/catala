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
module Val = Runtime.Value
open Json_encoding

let bool_encoding : Val.t encoding =
  conv
    (function
      | Val.V (Bool, v) -> (v : bool)
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of bool while encoding to JSON"
          Val.format v)
    (fun v -> Val.V (Val.Bool, v))
    bool

let unit_encoding : Val.t encoding =
  conv
    (function
      | Val.V (Unit, ()) -> ()
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of unit while encoding to JSON"
          Val.format v)
    (fun () -> Val.V (Unit, ()))
    empty

let try_option f =
  try Some (f ()) with
  | (Sys.Break | Assert_failure _ | Match_failure _) as e -> raise e
  | _ -> None

let int_encoding : Val.t encoding =
  def "integer" ~title:"Catala Integer"
  @@ union
       [
         case int53
           (function
             | Val.V (Integer, z) -> try_option (fun () -> Z.to_int64 z)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of int while encoding to \
                  JSON"
                 Val.format v)
           (fun i -> Val.V (Integer, Z.of_int64 i));
         case string
           (function
             | Val.V (Integer, z) -> Some (Z.to_string z) | _ -> assert false)
           (fun s ->
             try Val.V (Integer, Z.of_string s)
             with _ ->
               raise (Json_encoding.Unexpected ("string", "numeric string")));
       ]

let money_encoding : Val.t encoding =
  def "money" ~title:"Catala Money"
  @@
  let z_100 = Z.of_int 100 in
  let q_100 = Q.of_int 100 in
  union
    [
      case int53
        (function
          | Val.V (Money, z) when Z.rem z z_100 = Z.zero ->
            try_option (fun () -> Z.(div z z_100 |> to_int64))
          | Val.V (Money, _) -> None
          | v ->
            Message.error ~internal:true
              "Unexpected runtime value %a instead of money while encoding to \
               JSON"
              Val.format v)
        (fun i -> Val.V (Money, Z.(mul (of_int64 i) z_100)));
      case float
        (function
          | Val.V (Money, z) -> try_option (fun () -> Z.to_float z /. 100.)
          | _ -> assert false)
        (fun i -> Val.V (Money, Z.of_float (i *. 100.)));
      case string
        (function
          | Val.V (Money, z) ->
            let z = Q.div (Q.of_bigint z) q_100 in
            Some (Q.to_string z)
          | _ -> assert false)
        (fun s ->
          try
            let q = Q.(of_string s |> mul q_100) in
            Val.V (Money, Q.to_bigint q)
          with _ ->
            raise (Json_encoding.Unexpected ("string", "numeric string")));
    ]

let rat_encoding : Val.t encoding =
  def "decimal" ~title:"Catala Decimal"
  @@ union
       [
         case float
           (function
             | Val.V (Decimal, d) -> try_option (fun () -> Q.to_float d)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of decimal while \
                  encoding to JSON"
                 Val.format v)
           (fun f -> Val.V (Decimal, Q.of_float f));
         case int53 (fun _ -> None) (fun f -> Val.V (Decimal, Q.of_int64 f));
         case string
           (function Val.V (Decimal, d) -> Some (Q.to_string d) | _ -> None)
           (fun s ->
             try Val.V (Decimal, Q.of_string s)
             with _ ->
               raise (Json_encoding.Unexpected ("string", "numeric string")));
       ]

let date_encoding : Val.t encoding =
  let date_obj =
    obj3
      (req "year" (ranged_int ~minimum:0 ~maximum:9999 "years"))
      (req "month" (ranged_int ~minimum:1 ~maximum:12 "months"))
      (req "day" (ranged_int ~minimum:1 ~maximum:31 "days"))
  in
  def "date" ~title:"Catala date"
  @@ union
       [
         case
           ~description:
             "Accepts strings with the following format: YYYY-MM-DD, e.g., \
              \"1970-01-31\""
           string
           (function
             | Val.V (Date, d) ->
               Some (Format.asprintf "%a" Dates_calc.format_date d)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of date while encoding \
                  to JSON"
                 Val.format v)
           (fun s -> Val.V (Date, Dates_calc.date_of_string s));
         case
           ~description:
             "Accepts date objects: {\"year\":<int>, \"month\":<int>, \
              \"day\":<int>}"
           date_obj
           (function
             | Val.V (Date, d) -> Some (Dates_calc.date_to_ymd d)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of date while encoding \
                  to JSON"
                 Val.format v)
           (fun (year, month, day) ->
             Val.V (Date, Dates_calc.make_date ~year ~month ~day));
       ]

let duration_encoding : Val.t encoding =
  def "duration" ~title:"Catala duration"
  @@
  let encoding =
    obj3 (dft "years" int 0) (dft "months" int 0) (dft "days" int 0)
    |> conv
         (function
           | Val.V (Duration, d) -> Dates_calc.period_to_ymds d
           | v ->
             Message.error ~internal:true
               "Unexpected runtime value %a instead of duration while encoding \
                to JSON"
               Val.format v)
         (fun (years, months, days) ->
           Val.V (Duration, Dates_calc.make_period ~years ~months ~days))
  in
  encoding

let position_encoding =
  def "position" ~title:"Catala position"
  @@
  let p_encoding = obj2 (req "line" int32) (req "character" int32) in
  let range_encoding = obj2 (req "start" p_encoding) (req "end" p_encoding) in
  obj2 (req "file" string) (req "range" range_encoding)
  |> conv
       (function
         | Val.V (Position, pos) ->
           ( pos.filename,
             ( (Int32.of_int pos.start_line, Int32.of_int pos.start_column),
               (Int32.of_int pos.end_line, Int32.of_int pos.end_column) ) )
         | v ->
           Message.error ~internal:true
             "Unexpected runtime value %a instead of position while encoding \
              to JSON"
             Val.format v)
       (fun (file, ((sl, sc), (el, ec))) ->
         Val.V
           ( Position,
             {
               filename = file;
               start_line = Int32.to_int sl;
               start_column = Int32.to_int sc;
               end_line = Int32.to_int el;
               end_column = Int32.to_int ec;
               law_headings = [];
             } ))

let make_constant s : Val.t encoding =
  conv
    (function
      | Val.V (Unit, ()) -> ()
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of unit while encoding to JSON"
          Val.format v)
    (fun () -> Val.V (Unit, ()))
    (constant s)

let generate_lit_encoding (typ_lit : typ_lit) : Val.t encoding =
  match typ_lit with
  | TBool -> bool_encoding
  | TUnit -> unit_encoding
  | TInt -> int_encoding
  | TRat -> rat_encoding
  | TDate -> date_encoding
  | TDuration -> duration_encoding
  | TMoney -> money_encoding
  | TPos -> position_encoding

let rec generate_encoder (ctx : decl_ctx) (typ : typ) : Val.t encoding =
  match Mark.remove typ with
  | TError -> assert false
  | TLit tlit -> generate_lit_encoding tlit
  | TTuple [typ; (TLit TPos, _)] -> generate_encoder ctx typ
  | TTuple tl -> generate_tuple_encoder ctx tl
  | TStruct sname -> generate_struct_encoder ctx sname
  | TEnum ename -> generate_enum_encoder ctx ename
  | TOption typ -> generate_option_encoder ctx typ
  | TArray typ -> generate_array_encoder ctx typ
  | TArrow _ -> Message.error "Cannot convert functional values from JSON"
  | TDefault _ -> Message.error "Cannot encode 'default' types"
  | TVar _ -> Message.error "Cannot encode 'variable' types"
  | TForAll _ -> Message.error "Cannot encode 'for-all' types"
  | TClosureEnv -> Message.error "Cannot encode 'closure-env' types"
  | TAbstract _ -> Message.error "Cannot encode 'abstract' types"

and generate_array_encoder (ctx : decl_ctx) typ : Val.t encoding =
  let open Val in
  conv
    (function
      | V (Array t, elts) -> Array.map t elts
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of array while encoding to JSON"
          format v)
    (fun a -> V (Array (fun v -> v), a))
    (array (generate_encoder ctx typ))

and generate_option_encoder ctx typ =
  let open Val in
  let proj_none = function
    | V (Enum { name = "Optional"; constr }, v) -> (
      match constr v with _, _, None -> Some (V (Unit, ())) | _ -> None)
    | _ -> None
  in
  let vtyp =
    Enum
      {
        name = "Optional";
        constr =
          (function None -> 0, "Absent", None | Some x -> 1, "Present", Some x);
      }
  in
  let inj_none _ = V (vtyp, None) in
  union
    [
      case unit_encoding proj_none inj_none;
      case (make_constant "Absent") proj_none inj_none;
      case
        (obj1 (req "Present" (generate_encoder ctx typ)))
        (function
          | V (Enum en, v) -> (
            match en.constr v with _, _, Some x -> Some x | _ -> None)
          | _ -> None)
        (fun x -> V (vtyp, Some x));
    ]

and generate_tuple_encoder ctx typl =
  let open Val in
  assert (typl <> []);
  let first_tup_enc = tup1 (generate_encoder ctx (List.hd typl)) in
  let add_tuple (acc : t encoding) typ : t encoding =
    let bconv = merge_tups acc (tup1 (generate_encoder ctx typ)) in
    conv
      (function
        | V (Tuple tf, elts) -> (
          match tf elts with
          | [x1; x2] -> x1, x2
          | arr ->
            let rarr = List.rev arr in
            V (Tuple Fun.id, List.rev (List.tl rarr)), List.hd rarr)
        | v ->
          Message.error ~internal:true
            "Unexpected runtime value %a instead of tuple while encoding to \
             JSON"
            format v)
      (function
        | V (Tuple tf, arr), rval -> V (Tuple Fun.id, tf arr @ [rval])
        | v, rval ->
          (* First element reached *)
          V (Tuple Fun.id, v :: [rval]))
      bconv
  in
  List.fold_left (fun e typ -> add_tuple e typ) first_tup_enc (List.tl typl)

and generate_struct_encoder (ctx : decl_ctx) (sname : StructName.t) =
  let open Val in
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
      (fun () ->
        V
          ( Struct { name = StructName.to_string sname; fields = (fun _ -> []) },
            () ))
      empty
  in
  let add_req_field (encoding : t encoding) (sf, typ) : t encoding =
    let field_label, field_s = rename_field sf in
    let bconv =
      merge_objs encoding (obj1 (req field_label (generate_encoder ctx typ)))
    in
    conv
      (function
        | V (Struct enc, data) as v ->
          let rval = List.assoc field_s (enc.fields data) in
          v, rval
        | v ->
          Message.error ~internal:true
            "Unexpected runtime value %a instead of struct while encoding to \
             JSON"
            format v)
      (function
        | V (Struct enc, data), rval ->
          V
            ( Struct { enc with fields = Fun.id },
              (field_s, rval) :: enc.fields data )
        | _ -> assert false)
      bconv
  in
  let add_opt_field (encoding : t encoding) (sf, typ) : t encoding =
    let field_label, field_s = rename_field sf in
    let bconv =
      merge_objs encoding (obj1 (opt field_label (generate_encoder ctx typ)))
    in
    conv
      (function
        | V (Struct enc, data) as v ->
          let rval =
            List.assoc_opt field_s (enc.fields data)
            |> Option.map (function
              | V (Enum { name = "Optional"; constr }, data) ->
                let _, _, x = constr data in
                x
              | _ -> assert false)
            |> Option.join
          in
          v, rval
        | _ -> assert false)
      (function
        | V (Struct enc, data), None ->
          V
            ( Struct { enc with fields = Fun.id },
              ( field_s,
                V
                  ( Enum
                      {
                        name = "Optional";
                        constr = (fun _ -> 0, "Absent", None);
                      },
                    () ) )
              :: enc.fields data )
        | V (Struct enc, data), Some rval ->
          V
            ( Struct { enc with fields = Fun.id },
              ( field_s,
                V
                  ( Enum
                      {
                        name = "Optional";
                        constr = (fun _ -> 1, "Present", Some rval);
                      },
                    () ) )
              :: enc.fields data )
        | _ -> assert false)
      bconv
  in
  def (Format.asprintf "%a" StructName.format_shortpath sname)
  @@ List.fold_left
       (fun e (sf, typ) ->
         match Mark.remove typ with
         | TOption typ | TDefault typ -> add_opt_field e (sf, typ)
         | _ -> add_req_field e (sf, typ))
       empty_struct_enc bdgs

and generate_enum_encoder (ctx : decl_ctx) (ename : EnumName.t) =
  let open Val in
  let enum = EnumName.Map.find ename ctx.ctx_enums in
  let bdgs = EnumConstructor.Map.bindings enum in
  let ename_s = EnumName.to_string ename in
  let make_constructor_case idx (cstr, typ) : t case =
    let cstr_s = EnumConstructor.to_string cstr in
    match Mark.remove typ with
    | TLit TUnit ->
      case (constant cstr_s)
        (function
          | V (Enum enc, rval) ->
            let _, cstr_s', _ = enc.constr rval in
            if cstr_s = cstr_s' then Some () else None
          | v ->
            Message.error ~internal:true
              "Unexpected runtime value %a instead of enum while encoding to  \
               JSON"
              format v)
        (fun () ->
          V (Enum { name = ename_s; constr = Fun.id }, (idx, cstr_s, None)))
    | _ ->
      case
        (obj1 (req (EnumConstructor.to_string cstr) (generate_encoder ctx typ)))
        (function
          | V (Enum { name; constr }, rval) ->
            let _, cstr_s', v = constr rval in
            if name = ename_s && cstr_s = cstr_s' then v else None
          | _ -> None)
        (fun v ->
          V (Enum { name = ename_s; constr = Fun.id }, (idx, cstr_s, Some v)))
  in
  let enc =
    if List.for_all (fun (_, typ) -> Mark.remove typ = TLit TUnit) bdgs then
      (* This simplifies the JSON schema *)
      string_enum
        (List.mapi
           (fun idx (cstr, _) ->
             let cstr_s = EnumConstructor.to_string cstr in
             ( cstr_s,
               V (Enum { name = ename_s; constr = Fun.id }, (idx, cstr_s, None))
             ))
           bdgs)
    else List.mapi make_constructor_case bdgs |> union
  in
  def (Format.asprintf "%a" EnumName.format_shortpath ename) enc

let make_encoding (ctx : decl_ctx) (typ : typ) = generate_encoder ctx typ

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

let parse_json enc json =
  try Yojson_repr.destruct enc json
  with e ->
    let print_unknown fmt = function
      | Failure msg -> Format.pp_print_string fmt msg
      | e -> Format.pp_print_string fmt (Printexc.to_string e)
    in
    Message.error
      "@[<v 2>Failed to validate JSON:@ %a@]@\n\
       @\n\
       @[<v 2>Expected JSON object of the form:@ %a@]"
      (fun fmt -> Json_encoding.print_error ~print_unknown fmt)
      e Json_schema.pp (Json_encoding.schema enc)

let rec convert_to_dcalc ctx (mark : 'm mark) (typ : typ) (rval : Val.t) :
    (dcalc, 'm) boxed_gexpr =
  let open Val in
  let mark = Expr.with_ty mark typ in
  let f = convert_to_dcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TUnit, V (Val.Unit, _) -> Expr.elit LUnit mark
  | TLit TBool, V (Bool, b) -> Expr.elit (LBool b) mark
  | TLit TMoney, V (Money, z) -> Expr.elit (LMoney z) mark
  | TLit TInt, V (Integer, z) -> Expr.elit (LInt z) mark
  | TLit TRat, V (Decimal, q) -> Expr.elit (LRat q) mark
  | TLit TDate, V (Date, d) -> Expr.elit (LDate d) mark
  | TLit TDuration, V (Duration, d) -> Expr.elit (LDuration d) mark
  | ( TLit TPos,
      V
        ( Position,
          {
            filename;
            start_line;
            start_column;
            end_line;
            end_column;
            law_headings;
          } ) ) ->
    Expr.epos
      Pos.(
        overwrite_law_info
          (from_info filename start_line start_column end_line end_column)
          law_headings)
      mark
  | TDefault typ, V (Enum { name = "Optional"; constr }, v) -> begin
    match constr v with
    | 0, "Absent", None -> Expr.eempty mark
    | 1, "Present", Some rval -> Expr.epuredefault (f typ rval) mark
    | _ -> assert false
  end
  | TOption typ, V (Enum { name = "Optional"; constr }, v) -> begin
    match constr v with
    | 0, "Absent", None ->
      Expr.einj ~name:Expr.option_enum ~cons:Expr.none_constr
        ~e:(Expr.elit LUnit mark) mark
    | 1, "Present", Some rval ->
      Expr.einj ~name:Expr.option_enum ~cons:Expr.some_constr ~e:(f typ rval)
        mark
    | _ -> assert false
  end
  | TEnum ename, V (Enum { name = _; constr }, v) ->
    let _idx, cstr, v = constr v in
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) -> EnumConstructor.to_string cstr' = cstr)
    in
    let e = match v with None -> Expr.elit LUnit mark | Some v -> f typ_v v in
    Expr.einj ~name:ename ~cons ~e mark
  | TStruct sname, V (Struct { name = _; fields }, v) ->
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
        StructField.Map.empty (fields v)
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, V (Array fl, a) ->
    Expr.earray (Array.map fl a |> Array.to_list |> List.map (f typ)) mark
  | TTuple typl, V (Tuple fl, a) ->
    Expr.etuple (fl a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime value to dcalc: expected value of type %a, got %a"
      Print.typ typ format r

let rec convert_to_lcalc ctx (mark : 'm mark) (typ : typ) (rval : Val.t) :
    (lcalc, 'm) boxed_gexpr =
  let open Val in
  let mark = Expr.with_ty mark typ in
  let f = convert_to_lcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TUnit, V (Val.Unit, _) -> Expr.elit LUnit mark
  | TLit TBool, V (Bool, b) -> Expr.elit (LBool b) mark
  | TLit TMoney, V (Money, z) -> Expr.elit (LMoney z) mark
  | TLit TInt, V (Integer, z) -> Expr.elit (LInt z) mark
  | TLit TRat, V (Decimal, q) -> Expr.elit (LRat q) mark
  | TLit TDate, V (Date, d) -> Expr.elit (LDate d) mark
  | TLit TDuration, V (Duration, d) -> Expr.elit (LDuration d) mark
  | ( TLit TPos,
      V
        ( Position,
          {
            filename;
            start_line;
            start_column;
            end_line;
            end_column;
            law_headings;
          } ) ) ->
    Expr.epos
      Pos.(
        overwrite_law_info
          (from_info filename start_line start_column end_line end_column)
          law_headings)
      mark
  | TTuple [typ; (TLit TPos, _)], rval ->
    Expr.etuple [f typ rval; Expr.epos Pos.void mark] mark
  | (TDefault typ | TOption typ), V (Enum { name = "Optional"; constr }, v) ->
    begin
    match constr v with
    | 0, "Absent", None ->
      Expr.einj ~name:Expr.option_enum ~cons:Expr.none_constr
        ~e:(Expr.elit LUnit mark) mark
    | 1, "Present", Some rval ->
      Expr.einj ~name:Expr.option_enum ~cons:Expr.some_constr ~e:(f typ rval)
        mark
    | _ -> assert false
  end
  | TEnum ename, V (Enum { name = _; constr }, v) ->
    let _idx, cstr, v = constr v in
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) -> EnumConstructor.to_string cstr' = cstr)
    in
    let e = match v with None -> Expr.elit LUnit mark | Some v -> f typ_v v in
    Expr.einj ~name:ename ~cons ~e mark
  | TStruct sname, V (Struct { name = _; fields }, v) ->
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
        StructField.Map.empty (fields v)
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, V (Array fl, a) ->
    Expr.earray (Array.map fl a |> Array.to_list |> List.map (f typ)) mark
  | TTuple typl, V (Tuple fl, a) ->
    Expr.etuple (fl a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime value to lcalc: expected value of type %a, got %a"
      Print.typ typ format r

let convert_from_gexpr = Expr.embed_value
