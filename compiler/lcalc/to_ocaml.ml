(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

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
open Shared_ast
open Ast
module D = Dcalc.Ast
module Runtime = Catala_runtime

(** Formatting to a list of formatters *)
let pp dest fmt = Format.kdprintf (fun k -> List.iter k dest) fmt

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       Format.pp_print_string)
    (List.map String.quote uids)

let format_pos ppf pos =
  Format.fprintf ppf
    "@[<hov 1>{filename=%S;@ start_line=%d; start_column=%d;@ end_line=%d; \
     end_column=%d;@ law_headings=%a}@]"
    (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
    (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
    (Pos.get_law_info pos)

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  let pint ppf n =
    if n >= 0 then Format.pp_print_int ppf n else Format.fprintf ppf "(%d)" n
  in
  match Mark.remove l with
  | LBool b -> Print.lit fmt (LBool b)
  | LInt i -> (
    match Z.to_int i with
    | n -> Format.fprintf fmt "integer_of_int %a" pint n
    | exception Z.Overflow ->
      Format.fprintf fmt "integer_of_string@ \"%s\""
        (Runtime.integer_to_string i))
  | LUnit -> Print.lit fmt LUnit
  | LRat (Q.{ num; den } as q) -> (
    match Z.to_int num, Z.to_int den with
    | n, 1 -> Format.fprintf fmt "Q.of_int %a" pint n
    | n, d ->
      Format.fprintf fmt "{ Q.num = Z.of_int %a; den = Z.of_int %a }" pint n
        pint d
    | exception Z.Overflow ->
      Format.fprintf fmt "decimal_of_string \"%s\"" (Q.to_string q))
  | LMoney e -> (
    match Z.to_int e with
    | n -> Format.fprintf fmt "integer_of_int %a" pint n
    | exception Z.Overflow ->
      Format.fprintf fmt "money_of_cents_string@ \"%s\""
        (Runtime.integer_to_string (Runtime.money_to_cents e)))
  | LDate d ->
    let y, m, d = Dates_calc.date_to_ymd d in
    Format.fprintf fmt "date_of_numbers %a %a %a" pint y pint m pint d
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "duration_of_numbers %a %a %a" pint years pint months
      pint days

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list)
    : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%a\"" Uid.MarkedString.format info))
    uids

(* list taken from
   http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:keywords *)
let ocaml_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "Stdlib";
    (* Catala lib elements *)
    "Catala_runtime";
    "Value";
    "Oper";
  ]

let renaming =
  Renaming.program ()
    ~reserved:ocaml_keywords
      (* TODO: add catala runtime built-ins as reserved as well ? *)
    ~skip_constant_binders:true
    ~constant_binder_name:
      (if Global.options.gen_external then None else Some "_")
    ~namespaced_fields:true ~namespaced_constrs:true ~prefix_module:false
    ~modnames_conflict:false

let format_struct_name (fmt : Format.formatter) (v : StructName.t) : unit =
  (match StructName.path v with
  | [] -> ()
  | path ->
    Uid.Path.format fmt path;
    Format.pp_print_char fmt '.');
  assert (
    let n = StructName.base v in
    n = String.capitalize_ascii n);
  Format.pp_print_string fmt (StructName.base v)

let format_to_module_name
    (fmt : Format.formatter)
    (name :
      [< `Ename of EnumName.t
      | `Sname of StructName.t
      | `Aname of AbstractType.t ]) =
  match name with
  | `Ename v -> EnumName.format fmt v
  | `Sname v -> StructName.format fmt v
  | `Aname v -> AbstractType.format fmt v

let format_struct_field_name
    (fmt : Format.formatter)
    ((sname_opt, v) : StructName.t option * StructField.t) : unit =
  Option.iter
    (fun sname ->
      format_to_module_name fmt (`Sname sname);
      Format.pp_print_char fmt '.')
    sname_opt;
  StructField.format fmt v

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit =
  EnumName.format fmt v

let format_enum_cons_name (fmt : Format.formatter) (v : EnumConstructor.t) :
    unit =
  EnumConstructor.format fmt v

let rec format_rtyp ppf ty =
  match Mark.remove ty with
  | TLit TUnit -> Format.pp_print_string ppf "Value.Unit"
  | TLit TBool -> Format.pp_print_string ppf "Value.Bool"
  | TLit TInt -> Format.pp_print_string ppf "Value.Integer"
  | TLit TMoney -> Format.pp_print_string ppf "Value.Money"
  | TLit TRat -> Format.pp_print_string ppf "Value.Decimal"
  | TLit TDate -> Format.pp_print_string ppf "Value.Date"
  | TLit TDuration -> Format.pp_print_string ppf "Value.Duration"
  | TLit TPos -> Format.pp_print_string ppf "Value.Position"
  | TArray ty ->
    Format.fprintf ppf "Value.Array(Value.embed (%a))" format_rtyp ty
  | TTuple tl ->
    let vars = List.mapi (fun i ty -> "x" ^ string_of_int i, ty) tl in
    Format.fprintf ppf "Value.Tuple(fun (@[<hov>%a@]) ->@ [@[<hov>%a@]])"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (fun ppf (v, _) -> Format.pp_print_string ppf v))
      vars
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         (fun ppf (v, ty) ->
           Format.fprintf ppf "Value.embed (%a) %s" format_rtyp ty v))
      vars
  | TStruct name ->
    Format.fprintf ppf "%a.rtype" format_to_module_name (`Sname name)
  | TEnum name ->
    Format.fprintf ppf "%a.rtype" format_to_module_name (`Ename name)
  | TOption ty -> Format.fprintf ppf "Optional.rtype (%a)" format_rtyp ty
  | TAbstract name ->
    Format.fprintf ppf "%a.rtype" format_to_module_name (`Aname name)
  | TArrow _ -> Format.fprintf ppf "Value.Function"
  | TError | TDefault _ | TVar _ | TForAll _ | TClosureEnv ->
    Message.error "Cannot compute comparison on type %a" Print.typ ty

let format_embedding (ppf : Format.formatter) (ty : typ) : unit =
  Format.fprintf ppf "Value.embed (%a)" format_rtyp ty

let typ_needs_parens (e : typ) : bool =
  match Mark.remove e with TArrow _ | TArray _ -> true | _ -> false

let format_typ (fmt : Format.formatter) (typ : typ) : unit =
  let rec aux bctx fmt typ =
    let format_typ_with_parens (fmt : Format.formatter) (t : typ) =
      if typ_needs_parens t then Format.fprintf fmt "(%a)" (aux bctx) t
      else Format.fprintf fmt "%a" (aux bctx) t
    in
    match Mark.remove typ with
    | TLit l -> Format.fprintf fmt "%a" Print.tlit l
    | TTuple [] -> Format.fprintf fmt "unit"
    | TTuple ts ->
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
           format_typ_with_parens)
        ts
    | TStruct s -> Format.fprintf fmt "%a.t" format_to_module_name (`Sname s)
    | TOption t ->
      Format.fprintf fmt "@[<hov 2>(%a)@] %a.t" format_typ_with_parens t
        format_to_module_name (`Ename Expr.option_enum)
    | TDefault t -> aux bctx fmt t
    | TEnum e -> Format.fprintf fmt "%a.t" format_to_module_name (`Ename e)
    | TAbstract e -> Format.fprintf fmt "%a.t" format_to_module_name (`Aname e)
    | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ->@ ")
           format_typ_with_parens)
        (t1 @ [t2])
    | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ_with_parens t1
    | TVar v ->
      let name = Bindlib.name_of v in
      let name =
        if String.starts_with ~prefix:"'" name then
          "a" ^ String.sub name 1 (String.length name - 1)
        else "t" ^ name
      in
      Format.fprintf fmt "'%s" name
    | TForAll tb ->
      (* We suppose here that there aren't multiple parallel binders in the same
         type: in that case two variables could be named the same *)
      let _v, typ, bctx = Bindlib.unmbind_in bctx tb in
      aux bctx fmt typ
    | TClosureEnv -> Format.fprintf fmt "Obj.t(*closure env*)"
    | TError -> assert false
  in
  aux Bindlib.empty_ctxt fmt typ

let format_var_str (fmt : Format.formatter) (v : string) : unit =
  Format.pp_print_string fmt v

let format_var (fmt : Format.formatter) (v : 'm Var.t) : unit =
  format_var_str fmt (Bindlib.name_of v)

let rec needs_parens (e : 'm expr) : bool =
  match Mark.remove e with
  | EApp { f = EAbs { binder; _ }, _; _ } ->
    let _, body = Bindlib.unmbind binder in
    needs_parens body
  | ELit (LBool _ | LUnit)
  | EVar _ | ETuple _
  | EInj { e = ELit LUnit, _; _ }
  | EStruct _ | EPos _ ->
    false
  | _ -> true

let rec format_expr (ctx : decl_ctx) (fmt : Format.formatter) (e : 'm expr) :
    unit =
  let format_expr = format_expr ctx in
  let format_with_parens (fmt : Format.formatter) (e : 'm expr) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else format_expr fmt e
  in
  match Mark.remove e with
  | EVar v -> Format.fprintf fmt "%a" format_var v
  | EExternal { name } -> (
    let path =
      match Mark.remove name with
      | External_value name -> TopdefName.path name
      | External_scope name -> ScopeName.path name
    in
    Uid.Path.format fmt path;
    match Mark.remove name with
    | External_value name -> format_var_str fmt (TopdefName.base name)
    | External_scope name -> format_var_str fmt (ScopeName.base name))
  | ETuple es ->
    Format.fprintf fmt "@[<hov 1>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_with_parens)
      es
  | EStruct { name = s; fields = es } ->
    if StructField.Map.is_empty es then Format.fprintf fmt "()"
    else
      Format.fprintf fmt "{@[<hov 2>%a@]}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (struct_field, e) ->
             Format.fprintf fmt "@[<hov 2>%a =@ %a@]" format_struct_field_name
               (Some s, struct_field) format_with_parens e))
        (StructField.Map.bindings es)
  | EArray es ->
    Format.fprintf fmt "@[<hov 2>[|%a|]@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_with_parens e))
      es
  (* | ETupleAccess { e = ETuple es, _; index; _ } ->
   *   format_expr fmt (List.nth es index) *)
  | ETupleAccess { e; index; size } ->
    Format.fprintf fmt "@[<hv 2>@[<hv 2>let @[<hov>%a@] =@ %a@]@;<1 -2>in x@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt i ->
           Format.pp_print_string fmt (if i = index then "x" else "_")))
      (List.init size Fun.id) format_with_parens e
  | EStructAccess { e; field; name } ->
    Format.fprintf fmt "%a.%a" format_with_parens e format_struct_field_name
      (Some name, field)
  | EInj { e = ELit LUnit, _; cons; name } ->
    Format.fprintf fmt "@[<hov 2>%a.%a@]" format_to_module_name (`Ename name)
      format_enum_cons_name cons
  | EInj { e; cons; name } ->
    Format.fprintf fmt "@[<hov 2>%a.%a@ %a@]" format_to_module_name
      (`Ename name) format_enum_cons_name cons format_with_parens e
  | EMatch { e; cases; name } ->
    let enum = EnumName.Map.find name ctx.ctx_enums in
    Format.fprintf fmt "@[<hv>@[<hv 2>match@ %a@;<1 -2>with@]@ %a| %a@]"
      format_with_parens e Format.pp_print_if_newline ()
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (c, e) ->
           match EnumConstructor.Map.find c enum with
           | TLit TUnit, _ ->
             Format.fprintf fmt "@[<hov>%a.%a %a@]" format_to_module_name
               (`Ename name) format_enum_cons_name c
               (fun fmt e ->
                 match Mark.remove e with
                 | EAbs { binder; _ } ->
                   let _, body = Bindlib.unmbind binder in
                   Format.fprintf fmt "->@ %a" format_with_parens body
                 | _ -> assert false)
               e
           | _ ->
             Format.fprintf fmt "@[<hov>%a.%a %a@]" format_to_module_name
               (`Ename name) format_enum_cons_name c
               (fun fmt e ->
                 match Mark.remove e with
                 | EAbs { binder; _ } ->
                   let xs, body = Bindlib.unmbind binder in
                   Format.fprintf fmt "%a ->@ %a"
                     (Format.pp_print_list
                        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
                        (fun fmt x -> Format.fprintf fmt "%a" format_var x))
                     (Array.to_list xs) format_with_parens body
                 | _ -> assert false)
               e))
      (EnumConstructor.Map.bindings cases)
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.add (Expr.pos e) l)
  | EApp { f = EAbs { binder; pos = _; tys }, _; args; _ } ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) tys in
    let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
    Format.pp_open_vbox fmt 0;
    Format.pp_print_list
      (fun fmt (x, tau, arg) ->
        Format.fprintf fmt
          "@[<hv 2>@[<hv 2>let %a :@ %a =@]@ @[<hov>%a@]@;<1 -2>in@]" format_var
          x format_typ tau format_with_parens arg)
      fmt xs_tau_arg;
    Format.pp_print_cut fmt ();
    (* (match body with
     *  | EApp { f = EAbs _, _; _ }, _ -> *)
    format_expr fmt body;
    Format.pp_close_box fmt ()
    (* | _ -> format_with_parens fmt body) *)
  | EAbs { binder; pos = _; tys } ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) tys in
    Format.fprintf fmt "@[<hov 2>fun@ %a ->@ %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (x, tau) ->
           Format.fprintf fmt "@[<hov 2>(%a:@ %a)@]" format_var x format_typ tau))
      xs_tau format_expr body
  | EApp
      {
        f = EAppOp { op = Log (BeginCall, info), _; args = [f]; _ }, _;
        args;
        _;
      }
    when Global.options.trace <> None ->
    Format.fprintf fmt "(log_begin_call@ %a@ %a)@ %a" format_uid_list info
      format_with_parens f
      (Format.pp_print_list ~pp_sep:Format.pp_print_space format_with_parens)
      args
  | EAppOp { op = Log (VarDef var_def_info, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    Format.fprintf fmt
      "(log_variable_definition@ %a@ {io_input=%s;@ io_output=%b}@ (%a)@ %a)"
      format_uid_list info
      (match var_def_info.log_io_input with
      | NoInput -> "NoInput"
      | OnlyInput -> "OnlyInput"
      | Reentrant -> "Reentrant")
      var_def_info.log_io_output format_embedding
      (var_def_info.log_typ, Pos.void)
      format_with_parens arg1
  | EAppOp { op = Log (PosRecordIfTrueBool, _), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    let pos = Expr.pos e in
    Format.fprintf fmt
      "(log_decision_taken@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) format_with_parens arg1
  | EAppOp { op = Log (EndCall, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    Format.fprintf fmt "(log_end_call@ %a@ %a)" format_uid_list info
      format_with_parens arg1
  | EAppOp { op = Log _, _; args = [arg1]; _ } ->
    Format.fprintf fmt "%a" format_with_parens arg1
  | EApp { f; args; _ } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_with_parens f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EIfThenElse { cond; etrue; efalse } ->
    let rec pr_else = function
      | EIfThenElse { cond; etrue; efalse }, _ ->
        Format.fprintf fmt
          "@[<hv 2>else if@ @[<hov>%a@]@]@ @[<hv 2>then@ @[<hov>%a@]@]@ "
          format_expr cond format_with_parens etrue;
        pr_else efalse
      | efalse ->
        Format.fprintf fmt "@[<hv 2>else@ @[<hov>%a@]@]@]" format_with_parens
          efalse
    in
    Format.fprintf fmt
      "@[<hv>@[<hv 2>if@ @[<hov>%a@]@]@ @[<hv 2>then@ @[<hov>%a@]@]@ "
      format_expr cond format_with_parens etrue;
    pr_else efalse
  | EAppOp { op = ((And | Or) as op), _; args = [e1; e2]; _ } ->
    Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" format_with_parens e1
      (if op = And then "&&" else "||")
      format_with_parens e2
  | EAppOp
      {
        op = ((Map | Map2 | Filter | Fold | Reduce), _) as op;
        args = (ETuple [clos_f; clos_env], mclos) :: args;
        tys;
      } ->
    (* This hack is for handling closure-converted terms when passed to the base
       runtime operators *)
    let m = Mark.get e in
    let e1 =
      ( EApp
          {
            f = clos_f;
            args = [clos_env];
            tys = [TClosureEnv, Expr.mark_pos m];
          },
        mclos )
    in
    format_expr fmt (EAppOp { op; args = e1 :: args; tys }, m)
  | EAppOp { op = Eq, _; args = [a1; a2]; _ }
    when match Expr.ty a1 with
         | TLit (TUnit | TBool | TInt | TMoney | TRat | TDate), _ -> true
         | _ -> false ->
    (* Z and Q support OCaml polymorphic equality *)
    Format.fprintf fmt "@[<hov 2>%a@ = %a@]" format_with_parens a1
      format_with_parens a2
  | EAppOp { op = ((Eq | Lt | Lte | Gt | Gte) as op), _; args = [a1; a2]; _ }
    when match Expr.ty a1 with
         | TLit (TUnit | TBool | TInt | TMoney | TDate), _ -> true
         | _ -> false ->
    (* Z supports OCaml polymorphic comparison, but not Q *)
    Format.fprintf fmt "@[<hov 2>%a@ %s %a@]" format_with_parens a1
      (Print.operator_to_string op)
      format_with_parens a2
  | EAppOp { op = op, pos; args; _ } ->
    Format.fprintf fmt "@[<hov 2>%s@ %t%a@]" (Operator.name op)
      (fun ppf ->
        match op, args with
        | (Eq | Lt | Lte | Gt | Gte), a1 :: _ ->
          Format.fprintf ppf "(%a)@ %a@ " format_rtyp (Expr.ty a1) format_pos
            pos
        | (Map2 | Add_dat_dur _ | Sub_dat_dur _), _ ->
          Format.fprintf ppf "%a@ " format_pos pos
        | ( ( Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int
            | Div_mon_rat | Div_dur_dur ),
            _ :: a :: _ ) ->
          Format.fprintf ppf "%a@ " format_pos (Expr.pos a)
        | _ -> ())
      (Format.pp_print_list ~pp_sep:Format.pp_print_space format_with_parens)
      args
  | EAssert e' ->
    Format.fprintf fmt
      "@[<hov 2>if not@ %a@;\
       <1 -2>then@ @[<hov 2>raise@ (Error@ (%s,@ [%a],@ %t))@]@]"
      format_with_parens e'
      Runtime.(error_to_string AssertionFailed)
      format_pos (Expr.pos e')
      (fun ppf ->
        match
          Pos.get_attr (Expr.pos e) (function
            | ErrorMessage m -> Some m
            | _ -> None)
        with
        | None -> Format.pp_print_string ppf "None"
        | Some m -> Format.fprintf ppf "Some %S" m)
  | EFatalError er ->
    Format.fprintf fmt "raise@ (Error (%a, [%a], %t))" Print.runtime_error er
      format_pos (Expr.pos e) (fun ppf ->
        match
          Pos.get_attr (Expr.pos e) (function
            | ErrorMessage m -> Some m
            | _ -> None)
        with
        | None -> Format.pp_print_string ppf "None"
        | Some m -> Format.fprintf ppf "Some %S" m)
  | EPos p -> format_pos fmt p
  | EBad ->
    Message.error ~internal:true ~pos:(Expr.pos e) "%a" Format.pp_print_text
      "Attempting to compile a EBad node which should have been previously \
       filtered."
  | _ -> .

let format_ctx
    (type_ordering : TypeIdent.t list)
    (ppml : Format.formatter)
    (ppi : Format.formatter)
    (ctx : decl_ctx)
    (modname : ModuleName.t option) : unit =
  let format_struct_decl (struct_name, struct_fields) =
    let ppdef ppf =
      if StructField.Map.is_empty struct_fields then
        Format.fprintf ppf "type t = unit"
      else (
        Format.fprintf ppf "@[<hv 2>type t = {";
        StructField.Map.iter
          (fun struct_field struct_field_type ->
            Format.fprintf ppf "@ @[<hov 2>%a:@ %a;@]" format_struct_field_name
              (None, struct_field) format_typ struct_field_type)
          struct_fields;
        Format.fprintf ppf "@;<1 -2>}@]")
    in
    Format.fprintf ppml "@[<v 2>module %a = struct@," format_to_module_name
      (`Sname struct_name);
    ppdef ppml;
    Format.fprintf ppml "@,@[<hv 2>let rtype = Value.Struct {";
    Format.fprintf ppml "@ name = %S;"
      (StructName.canonical_str modname struct_name);
    Format.fprintf ppml "@ @[<hv 2>fields = fun t -> [";
    StructField.Map.iter
      (fun fld ty ->
        Format.fprintf ppml "@ %S, %a t.%a;"
          (StructField.to_string fld)
          format_embedding ty StructField.format fld)
      struct_fields;
    Format.fprintf ppml "@;<1 -2>]@]";
    Format.fprintf ppml "@;<1 -2>}@]";
    Format.fprintf ppml "@;<1 -2>end@]@,@,";
    if TypeIdent.(Set.mem (Struct struct_name) ctx.ctx_public_types) then (
      Format.fprintf ppi "@[<v 2>module %a : sig@," format_to_module_name
        (`Sname struct_name);
      ppdef ppi;
      Format.fprintf ppi "@,val rtype: t Value.ty";
      Format.fprintf ppi "@;<1 -2>end@]@,@,")
  in
  let format_enum_decl (enum_name, enum_cons) =
    let ppdef ppf =
      Format.fprintf ppf "@[<v 2>type t =";
      EnumConstructor.Map.iter
        (fun constr -> function
          | TLit TUnit, _ ->
            Format.fprintf ppf "@,| @[<hov 2>%a@]" format_enum_cons_name constr
          | ty ->
            Format.fprintf ppf "@,| @[<hov 2>%a of@ %a@]" format_enum_cons_name
              constr format_typ ty)
        enum_cons;
      Format.fprintf ppf "@]"
    in
    Format.fprintf ppml "@[<hv 2>module %a = struct@ " format_to_module_name
      (`Ename enum_name);
    ppdef ppml;
    Format.fprintf ppml "@,@[<hv 2>let rtype = Value.Enum {";
    Format.fprintf ppml "@ name = %S;"
      (EnumName.canonical_str modname enum_name);
    Format.fprintf ppml "@ @[<v 2>constr = function";
    List.iteri
      (fun i (constr, ty) ->
        match ty with
        | TLit TUnit, _ ->
          Format.fprintf ppml "@,| @[<hv 2>%a ->@ %d, %S, None@]"
            format_enum_cons_name constr i
            (EnumConstructor.to_string constr)
        | ty ->
          Format.fprintf ppml
            "@,| @[<hv 2>%a x ->@ @[<hov 2>%d,@ %S,@ Some (%a x)@]@]"
            format_enum_cons_name constr i
            (EnumConstructor.to_string constr)
            format_embedding ty)
      (EnumConstructor.Map.bindings enum_cons);
    Format.fprintf ppml "@]";
    Format.fprintf ppml "@;<1 -2>}@]";
    Format.fprintf ppml "@;<1 -2>end@]@,@,";
    if TypeIdent.(Set.mem (Enum enum_name) ctx.ctx_public_types) then (
      Format.fprintf ppi "@[<v 2>module %a : sig@," format_to_module_name
        (`Ename enum_name);
      ppdef ppi;
      Format.fprintf ppi "@,val rtype: t Value.ty";
      Format.fprintf ppi "@;<1 -2>end@]@,@,")
  in
  let format_abstract_decl name =
    Format.fprintf ppml "@[<v 2>module %a = struct@,type t"
      format_to_module_name (`Aname name);
    if Global.options.trace <> None then
      Format.fprintf ppml "@,let embed (_: t) : runtime_value = Unembeddable";
    Format.fprintf ppml "@;<1 -2>end@]@,@,";
    if TypeIdent.(Set.mem (Abstract name) ctx.ctx_public_types) then (
      Format.fprintf ppi "@[<v 2>module %a : sig@,type t" format_to_module_name
        (`Aname name);
      if Global.options.trace <> None then
        Format.fprintf ppi "@,val embed : t -> runtime_value@,";
      Format.fprintf ppi "@;<1 -2>end@]@,@,")
  in
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | TypeIdent.Enum _ | TypeIdent.Abstract _ -> false
        | TypeIdent.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> TypeIdent.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  List.iter
    (function
      | TypeIdent.Struct s ->
        let def = StructName.Map.find s ctx.ctx_structs in
        if StructName.path s = [] then format_struct_decl (s, def)
      | TypeIdent.Enum e ->
        if EnumName.equal e Expr.option_enum then ()
        else
          let def = EnumName.Map.find e ctx.ctx_enums in
          if EnumName.path e = [] then format_enum_decl (e, def)
      | TypeIdent.Abstract t ->
        if AbstractType.path t = [] then format_abstract_decl t)
    (type_ordering @ scope_structs)

let format_expr ctx fmt e =
  Format.pp_open_vbox fmt 0;
  format_expr ctx fmt e;
  Format.pp_close_box fmt ()

let format_scope_body_expr
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (scope_lets : 'm Ast.expr scope_body_expr) : unit =
  Format.pp_open_vbox fmt 0;
  let last_e =
    BoundList.iter
      ~f:(fun scope_let_var scope_let ->
        Format.fprintf fmt "@[<hv>@[<hov 2>let %a: %a =@ %a@]@ in@]@ "
          format_var scope_let_var format_typ scope_let.scope_let_typ
          (format_expr ctx) scope_let.scope_let_expr)
      scope_lets
  in
  format_expr ctx fmt last_e;
  Format.pp_close_box fmt ()

let format_code_items
    (ctx : decl_ctx)
    (ppml : Format.formatter)
    (ppi : Format.formatter)
    (code_items : 'm Ast.expr code_item_list) : 'm Ast.expr code_export list =
  pp [ppml; ppi] "@[<v>";
  let exports =
    BoundList.iter code_items ~f:(fun var item ->
        match item with
        | Topdef (name, typ, vis, e) ->
          if vis = Public then (
            pp [ppi] "@,(** Toplevel definition %a *)" TopdefName.format name;
            pp [ppi] "@,@[<hov 2>val %a : %a@]@," format_var var format_typ typ);
          Format.fprintf ppml "@,(* Toplevel def %a *)" TopdefName.format name;
          Format.fprintf ppml "@,@[<v 2>@[<hov 2>let %a : %a =@]@ %a@]@,"
            format_var var format_typ typ (format_expr ctx) e
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr =
            Bindlib.unbind body.scope_body_expr
          in
          if body.scope_body_visibility = Public then (
            pp [ppi] "@,(** Scope %a *)" ScopeName.format name;
            pp [ppi] "@,@[<hv 2>val %a :@ @[<hv>%a.t ->@ %a.t@]@]@," format_var
              var format_to_module_name (`Sname body.scope_body_input_struct)
              format_to_module_name (`Sname body.scope_body_output_struct));
          Format.fprintf ppml "@,(* Scope %a *)" ScopeName.format name;
          Format.fprintf ppml
            "@,@[<hv 2>@[<hov 2>let %a :@ %a.t -> %a.t =@ fun %a ->@]@ %a@]@,"
            format_var var format_to_module_name
            (`Sname body.scope_body_input_struct) format_to_module_name
            (`Sname body.scope_body_output_struct) format_var scope_input_var
            (format_scope_body_expr ctx)
            scope_body_expr)
  in
  pp [ppml; ppi] "@]";
  exports

let format_scope_exec_args
    (p : 'm Ast.program)
    filename
    (fmt : Format.formatter)
    exports =
  let tests =
    List.filter_map
      (function KTest scope, e -> Some (scope, e) | _ -> None)
      exports
  in
  if tests = [] then
    Message.warning
      "%a@{<magenta>#[test]@}@ attribute@ above@ their@ declaration."
      Format.pp_print_text
      "No test scope were found: the generated executable won't test any \
       computation. To mark scopes as tests, ensure they don't require inputs, \
       and add the "
  else
    Message.debug "@[<hov 2>Generating entry points for scopes:@ %a@]@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (s, _) ->
           ScopeName.format ppf s))
      tests;
  Format.pp_print_string fmt
    "(** This file has been generated by the Catala compiler, do not edit! *)\n\n";
  Format.pp_print_string fmt "let test_scopes = [\n";
  List.iter
    (fun (scope, _) -> Format.fprintf fmt "  %S;\n" (ScopeName.to_string scope))
    tests;
  Format.pp_print_string fmt "]\n";

  Format.pp_print_string fmt
    {|
let commands =
  Stdlib.List.map (fun c ->
      if Stdlib.List.mem c test_scopes then c else (
        Stdlib.print_endline
          "Specify scopes from the following list (or no argument \
           for running them all):";
        Stdlib.List.iter (fun n -> Stdlib.print_endline ("  - " ^ n))
          test_scopes;
        Stdlib.exit 1
      ))
    (Stdlib.List.tl (Stdlib.Array.to_list Stdlib.Sys.argv))

let commands = if commands = [] then test_scopes else commands

|};
  let modname =
    match p.module_name with
    | Some (n, _) -> ModuleName.to_string n
    | None ->
      String.capitalize_ascii (String.to_id File.(basename filename -.- ""))
  in
  Format.pp_open_vbox fmt 0;
  Format.fprintf fmt "open Catala_runtime@,";
  Format.fprintf fmt "open %s@,@," modname;
  List.iter
    (fun (scope, e) ->
      (* Note: this only checks that execution doesn't raise errors or assert
         failures. Adding a printer for the results could be an idea... *)
      Format.fprintf fmt
        "let () = if Stdlib.List.mem %S commands then (@,\
        \  @[<hv>@[<hov 2>let _ =@ @[<hv>%a@]@]@ in@ Stdlib.print_endline \
         \"\\x1b[32m[RESULT]\\x1b[m Scope %a executed successfully.\"@]@,\
         )@,"
        (ScopeName.to_string scope)
        (format_expr p.decl_ctx) e ScopeName.format scope)
    tests;
  Format.pp_close_box fmt ()

let check_and_reexport_used_modules ppml ppi ~hashf modules =
  List.iter
    (fun (m, intf_id) ->
      pp [ppml]
        "@[<hv 2>let () =@ @[<hov 2>match Catala_runtime.check_module %S \
         \"%a\"@ with@]@,\
         | Ok () -> ()@,\
         @[<hv 2>| Error h -> failwith \"Hash mismatch for module %a, it may \
         need recompiling\"@]@]@,"
        (ModuleName.to_string m)
        (fun ppf h ->
          if intf_id.is_external then
            Format.pp_print_string ppf Hash.external_placeholder
          else Hash.format ppf h)
        (hashf intf_id.hash) ModuleName.format m;
      pp [ppml; ppi] "@[<hv 2>module %a@ = %a@]@," ModuleName.format
        (ModuleName.normalise m) ModuleName.format (ModuleName.normalise m))
    modules;
  if modules <> [] then pp [ppml; ppi] "@,"

let format_module_registration ctx fmt exports modname hash is_external =
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "let () =";
  Format.pp_print_space fmt ();
  Format.pp_open_hvbox fmt 2;
  Format.fprintf fmt "Catala_runtime.register_module \"%a\"" ModuleName.format
    modname;
  Format.pp_print_space fmt ();
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "[ ";
  Format.pp_print_list
    ~pp_sep:(fun fmt () ->
      Format.pp_print_char fmt ';';
      Format.pp_print_cut fmt ())
    (fun fmt (name, e) ->
      Format.fprintf fmt "@[<hov 2>%S,@ Stdlib.Obj.repr %a@]" name
        (format_expr ctx) e)
    fmt
    (List.filter_map
       (function
         | KScope n, e -> Some (ScopeName.to_string n, e)
         | KTopdef n, e -> Some (TopdefName.to_string n, e)
         | KTest _, _ -> None)
       exports);
  Format.pp_close_box fmt ();
  Format.pp_print_char fmt ' ';
  Format.pp_print_string fmt "]";
  Format.pp_print_space fmt ();
  Format.fprintf fmt "\"%a\""
    (fun ppf h ->
      if is_external then Format.pp_print_string ppf Hash.external_placeholder
      else Hash.format ppf h)
    hash;
  Format.pp_close_box fmt ();
  Format.pp_close_box fmt ();
  Format.pp_print_newline fmt ()

let header () =
  let comment =
    if Global.options.gen_external then
      "(* This is a template file following the expected interface and \
       declarations to\n\
      \ * implement the corresponding Catala module.\n\
      \ *\n\
      \ * You should replace all `raise (Error (Impossible))` place-holders \
       with your\n\
      \ * implementation and rename it to remove the \".template\" suffix. *)"
    else
      "(** This file has been generated by the Catala compiler, do not edit! *)"
  in
  comment
  ^ "\n\n\
     [@@@ocaml.warning \"-4-26-27-32-33-34-37-41-42-69\"]\n\n\
     open Catala_runtime\n\n"

let format_program
    output_file
    ppml
    ~(hashf : Hash.t -> Hash.full)
    (p : 'm Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  File.with_secondary_out_channel ~output_file ~ext:"mli"
  @@ fun intf_file ppi ->
  pp [ppml; ppi] "@[<v>";
  pp [ppml; ppi] "%s" (header ());
  if not Global.options.gen_external then
    check_and_reexport_used_modules ppml ppi ~hashf
      (List.map
         (fun (m, intf) -> m, intf.intf_id)
         (ModuleName.Map.bindings p.decl_ctx.ctx_modules));
  format_ctx type_ordering ppml ppi p.decl_ctx (Option.map fst p.module_name);
  let exports = format_code_items p.decl_ctx ppml ppi p.code_items in
  p.module_name
  |> Option.iter (fun (modname, intf_id) ->
      Format.pp_print_cut ppml ();
      format_module_registration p.decl_ctx ppml exports modname
        (hashf intf_id.hash) intf_id.is_external);
  if List.exists (function KTest _, _ -> true | _ -> false) exports then
    File.with_secondary_out_channel ~output_file ~ext:"+main.ml" (fun _ fmt ->
        format_scope_exec_args p
          (Option.value ~default:"-" output_file)
          fmt exports);
  pp [ppml; ppi] "@]";
  if Global.options.gen_external then
    let files = List.filter_map Fun.id [output_file; intf_file] in
    if files <> [] then
      Message.result "Generated template external implementations:@ %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space File.format)
        files
