(*module ExprGen (C : EXPR_PARAM) = struct*)
open Catala_utils
open Shared_ast
open Print

let rec colors =
  let open Ocolor_types in
  blue :: cyan :: green :: yellow :: red :: magenta :: colors

module Precedence = struct
  type op = Xor | And | Or | Comp | Mul | Div | Add | Sub

  type t =
    | Contained
      (* No parens needed, the term has unambiguous beginning and end *)
    | Op of op
    | App (* Function application, right-associative *)
    | Abs (* lambda, *)
    | Dot (* *)

  let oper : type a. a operator Mark.pos -> t = fun op -> 
    match Mark.remove op with
      | Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth
      | Length | Log _ | Minus | Minus_int | Minus_rat | Minus_mon | Minus_dur
      | ToInt | ToInt_rat | ToRat | ToRat_int | ToRat_mon | ToMoney
      | ToMoney_rat | Round | Round_rat | Round_mon ->
        App
      | And -> Op And
      | Or -> Op Or
      | Xor -> Op Xor
      | Eq | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur
      | Eq_dat_dat ->
        Op Comp
      | Lt | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur ->
        Op Comp
      | Lte | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat
      | Lte_dur_dur ->
        Op Comp
      | Gt | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur ->
        Op Comp
      | Gte | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat
      | Gte_dur_dur ->
        Op Comp
      | Add | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
      | Add_dur_dur ->
        Op Add
      | Sub | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat
      | Sub_dat_dur _ | Sub_dur_dur ->
        Op Sub
      | Mult | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat
      | Mult_dur_int ->
        Op Mul
      | Div | Div_int_int | Div_rat_rat | Div_mon_int | Div_mon_rat
      | Div_mon_mon | Div_dur_dur ->
        Op Div
      | HandleExceptions | Map | Map2 | Concat | Filter | Reduce | Fold
      | ToClosureEnv | FromClosureEnv ->
        App
  let expr : type a. (a, 't) gexpr -> t =
   fun e ->
    match Mark.remove e with
    | ELit _ -> Contained (* Todo: unop if < 0 *)
    | EAppOp { op; _ } -> oper op
    | EApp _ -> App
    | EArray _ -> Contained
    | EVar _ -> Contained
    | EExternal _ -> Contained
    | EAbs _ -> Abs
    | EIfThenElse _ -> Contained
    | EStruct _ -> Contained
    | EInj _ -> App
    | EMatch _ -> App
    | ETuple _ -> Contained
    | ETupleAccess _ -> Dot
    | ELocation _ -> Contained
    | EScopeCall _ -> App
    | EDStructAmend _ -> App
    | EDStructAccess _ | EStructAccess _ -> Dot
    | EAssert _ -> App
    | EFatalError _ -> App
    | EDefault _ -> Contained
    | EPureDefault _ -> Contained
    | EEmpty -> Contained
    | EErrorOnEmpty _ -> App
    | EPos _ -> Contained
    | ECustom _ -> Contained
    | EHole _ -> Contained

  let needs_parens ~context ?(rhs = false) value =
    match context, value with
    | _, Contained -> false
    | Dot, Dot -> rhs
    | _, Dot -> false
    | Dot, _ -> true
    | App, App -> not rhs
    | App, Op _ -> true
    | App, Abs -> true
    | Abs, _ -> false
    | Op a, Op b -> (
      match a, b with
      | _, Xor -> true
      | And, And | Or, Or -> false
      | And, Or | Or, And -> true
      | (And | Or | Xor), _ -> false
      | _, (And | Or | Comp) -> true
      | Comp, _ -> false
      | Add, (Add | Sub) -> false
      | Sub, (Add | Sub) -> rhs
      | (Add | Sub), (Mul | Div) -> false
      | (Mul | Div), (Add | Sub) -> true
      | Mul, (Mul | Div) -> false
      | Div, (Mul | Div) -> rhs)
    | Op _, App -> not rhs
    | Op _, _ -> true
    | Contained, _ -> false

  let needs_parens_expr ~context ?(rhs = false) e =
    needs_parens ~context:(expr context) ~rhs (expr e)

  let trace : type a. (a, 't) Trace_ast.t -> t = function 
    | TrExpr _ -> Contained
    | TrLit _ -> Contained (* Todo: unop if < 0 *)
    | TrAppOp { op; _ } -> oper op
    | TrApp _ -> App
    | TrArray _ -> Contained
    | TrVar _ -> Contained
    | TrExternal _ -> Contained
    | TrAbs _ -> Abs
    | TrIfThenElse _ -> Contained
    | TrStruct _ -> Contained
    | TrInj _ -> App
    | TrMatch _ -> App
    | TrTuple _ -> Contained
    | TrTupleAccess _ -> Dot
    | TrStructAccess _ -> Dot
    | TrAssert _ -> App
    | TrFatalError _ -> App
    | TrDefault _ -> Contained
    | TrPureDefault _ -> Contained
    | TrEmpty -> Contained
    | TrErrorOnEmpty _ -> App
    | TrCustom _ -> Contained
    | TrHole _ -> Contained
  let needs_parens_tr ~context ?(rhs = false) tr = 
    needs_parens ~context:(trace context) ~rhs (trace tr)
end

module Trace_precedence = struct
  
end

let with_color f color fmt x =
  (* equivalent to [Format.fprintf fmt "@{<color>%s@}" s] *)
  Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg (C4 color)));
  f fmt x;
  Format.pp_close_stag fmt ()
let pp_color_string = with_color Format.pp_print_string

let typ_needs_parens (ty : typ) : bool =
  match Mark.remove ty with TArrow _ | TArray _ -> true | _ -> false

let rec typ_gen
    (ctx : decl_ctx option)
    ~(colors : Ocolor_types.color4 list)
    (fmt : Format.formatter)
    (ty : typ) : unit =
  let typ = typ_gen ctx in
  let typ_with_parens ~colors (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      typ ~colors:(List.tl colors) fmt t;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else typ ~colors fmt t
  in
  if ctx <> None then attrs fmt (Mark.get ty);
  match Mark.remove ty with
  | TLit l -> tlit fmt l
  | TTuple ts ->
    Format.pp_open_hvbox fmt 2;
    pp_color_string (List.hd colors) fmt "(";
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " op_style ",")
       (typ ~colors:(List.tl colors)))
      fmt ts;
    Format.pp_close_box fmt ();
    pp_color_string (List.hd colors) fmt ")"
  | TStruct s -> (
    match ctx with
    | None -> StructName.format fmt s
    | Some ctx ->
      attrs fmt (Mark.get (StructName.get_info s));
      let fields = StructName.Map.find s ctx.ctx_structs in
      if StructField.Map.is_empty fields then StructName.format fmt s
      else
        Format.fprintf fmt "@[<hv 2>%a %a@,%a@;<0 -2>%a@]" StructName.format s
          (pp_color_string (List.hd colors))
          "{"
          (StructField.Map.format_bindings_i
             ~pp_sep:(fun fmt () ->
               op_style fmt ";";
               Format.pp_print_space fmt ())
             (fun fmt pp_field_name field field_typ ->
               attrs fmt (Mark.get (StructField.get_info field));
               Format.fprintf fmt "@[<hv 2>%t%a@ %a@]" pp_field_name punctuation
                 ":"
                 (typ ~colors:(List.tl colors))
                 field_typ))
          fields
          (pp_color_string (List.hd colors))
          "}")
  | TEnum e -> (
    match ctx with
    | None -> Format.fprintf fmt "@[<hov 2>%a@]" EnumName.format e
    | Some ctx ->
      attrs fmt (Mark.get (EnumName.get_info e));
      let def = EnumName.Map.find e ctx.ctx_enums in
      Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" EnumName.format e punctuation "["
        (EnumConstructor.Map.format_bindings_i
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " punctuation "|")
           (fun fmt pp_case case mty ->
             attrs fmt (Mark.get (EnumConstructor.get_info case));
             Format.fprintf fmt "%t%a@ %a" pp_case punctuation ":" (typ ~colors)
               mty))
        def punctuation "]")
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "option" (typ ~colors) t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" (typ_with_parens ~colors) t1
      op_style "→" (typ ~colors) t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@ %a@ %a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " op_style ",")
         (typ_with_parens ~colors:(List.tl colors)))
      t1
      (pp_color_string (List.hd colors))
      ")" op_style "→"
      (typ ~colors:(List.tl colors))
      t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "list of" (typ ~colors) t1
  | TDefault t1 ->
    punctuation fmt "⟨";
    typ ~colors fmt t1;
    punctuation fmt "⟩"
  | TAny -> base_type fmt "any"
  | TClosureEnv -> base_type fmt "closure_env"

let rec expr_aux :
    type a t.
    Bindlib.ctxt ->
    Ocolor_types.color4 list ->
    Format.formatter ->
    (a, t) gexpr ->
    unit =
  fun bnd_ctx colors fmt e ->

  let exprb bnd_ctx colors e = expr_aux bnd_ctx colors e in
  let exprc colors e = exprb bnd_ctx colors e in
  let expr e = exprc colors e in
  let operator fmt op = operator ~debug:false fmt op in 
  let paren ~rhs ?(colors = colors) expr fmt e1 =
    if Precedence.needs_parens_expr ~rhs ~context:e e1 then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      expr (List.tl colors) fmt e1;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else expr colors fmt e1
  in
  let default_punct = with_color (fun fmt -> Format.pp_print_as fmt 1) in
  let lhs ?(colors = colors) ex = paren ~colors ~rhs:false ex in
  let rhs ex = paren ~rhs:true ex in
    match Mark.remove e with
    | EVar v -> var fmt v
    | EExternal { name } -> external_ref fmt name
    | ETuple es ->
      Format.fprintf fmt "@[<hov 2>%a%a%a@]"
        (pp_color_string (List.hd colors))
        "("
        (Format.pp_print_list
            ~pp_sep:(fun fmt () ->
              Format.fprintf fmt "%a@ " (pp_color_string (List.hd colors)) ",")
            (fun fmt e -> lhs ~colors:(List.tl colors) exprc fmt e))
        es
        (pp_color_string (List.hd colors))
        ")"
    | EArray es ->
      Format.fprintf fmt "@[<hv 2>%a@,@[<hov>%a@]@;<0 -2>%a@]" punctuation "["
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
            (fun fmt e -> lhs exprc fmt e))
        es punctuation "]"
    | ETupleAccess { e; index; _ } ->
      lhs exprc fmt e;
      punctuation fmt ".";
      Format.pp_print_int fmt index
    | ELit l -> lit fmt l
    | EApp { f = EAbs _, _; _ } ->
      let rec pr bnd_ctx colors fmt = function
        | EApp { f = EAbs { binder; pos = _; tys }, _; args; _ }, _ ->
          let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
          let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
          let xs_tau_arg =
            List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args
          in
          Format.pp_print_list
            (fun fmt (x, tau, arg) ->
              Format.fprintf fmt
                "@[<hv 2>@[<hov 4>%a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]" keyword
                "let" var x punctuation ":" (typ_gen None ~colors) tau
                punctuation "=" (exprc colors) arg keyword "in")
            fmt xs_tau_arg;
          Format.pp_print_cut fmt ();
          rhs (pr bnd_ctx) fmt body
        | e -> rhs (exprb bnd_ctx) fmt e
      in
      Format.pp_open_vbox fmt 0;
      pr bnd_ctx colors fmt e;
      Format.pp_close_box fmt ()
    | EAbs { binder; pos = _; tys } ->
      let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
      let expr = exprb bnd_ctx in
      let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
      Format.fprintf fmt "@[<hv 0>%a @[<hv 2>%a@]@ @]%a@ %a" punctuation "λ"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun fmt (x, tau) ->
              match tau with
              | TLit TUnit, _ ->
                punctuation fmt "(";
                punctuation fmt ")"
              | _ ->
                punctuation fmt "(";
                Format.pp_open_hvbox fmt 2;
                var fmt x;
                punctuation fmt ":";
                Format.pp_print_space fmt ();
                typ_gen None ~colors fmt tau;
                Format.pp_close_box fmt ();
                punctuation fmt ")"))
        xs_tau punctuation "→" (rhs expr) body
    | EAppOp { op = ((Map | Filter) as op), _; args = [arg1; arg2]; _ } ->
      Format.fprintf fmt "@[<hv 2>%a %a@ %a@]" operator op (lhs exprc) arg1
        (rhs exprc) arg2
    | EAppOp { op = (Log _ as op), _; args = [arg1]; _ } ->
      Format.fprintf fmt "@[<hv 0>%a@ %a@]" operator op (rhs exprc) arg1
    | EAppOp { op = op0, _; args = [_; _]; _ } ->
      let prec = Precedence.expr e in
      let rec pr colors fmt = function
        (* Flatten sequences of the same associative op *)
        | EAppOp { op = op, _; args = [arg1; arg2]; _ }, _ when op = op0 -> (
          (match prec with
          | Op (And | Or | Mul | Add | Div | Sub) -> lhs pr fmt arg1
          | _ -> lhs exprc fmt arg1);
          Format.pp_print_space fmt ();
          operator fmt op;
          Format.pp_print_char fmt ' ';
          match prec with
          | Op (And | Or | Mul | Add) -> rhs pr fmt arg2
          | _ -> rhs exprc fmt arg2)
        | e -> exprc colors fmt e
      in
      Format.pp_open_hvbox fmt 0;
      pr colors fmt e;
      Format.pp_close_box fmt ()
    | EAppOp { op = op, _; args = [arg1]; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op (rhs exprc) arg1
    | EAppOp { op = op, _; args; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (rhs exprc))
        args
    | EApp { f; args; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" (lhs exprc) f
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (rhs exprc))
        args
    | EIfThenElse _ ->
      let rec pr els fmt = function
        | EIfThenElse { cond; etrue; efalse }, _ ->
          Format.fprintf fmt "@[<hv 2>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]@ %a"
            keyword
            (if els then "else if" else "if")
            expr cond keyword "then" expr etrue (pr true) efalse
        | e ->
          Format.fprintf fmt "@[<hv 2>%a@ %a@]" keyword "else" (rhs exprc) e
      in
      Format.pp_open_hvbox fmt 0;
      pr false fmt e;
      Format.pp_close_box fmt ()
    | EDefault { excepts; just; cons } ->
      if List.length excepts = 0 then
        Format.fprintf fmt "@[<hv 1>%a%a@ %a %a%a@]"
          (default_punct (List.hd colors))
          "⟨"
          (exprc (List.tl colors))
          just
          (default_punct (List.hd colors))
          "⊢"
          (exprc (List.tl colors))
          cons
          (default_punct (List.hd colors))
          "⟩"
      else
        Format.fprintf fmt
          "@[<hv 0>@[<hov 2>%a %a@]@ @[<hov 2>%a %a@ %a %a@] %a@]"
          (default_punct (List.hd colors))
          "⟨"
          (Format.pp_print_list
              ~pp_sep:(fun fmt () ->
                Format.fprintf fmt "%a@ " (default_punct (List.hd colors)) ",")
              (lhs ~colors:(List.tl colors) exprc))
          excepts
          (default_punct (List.hd colors))
          "|"
          (exprc (List.tl colors))
          just
          (default_punct (List.hd colors))
          "⊢"
          (exprc (List.tl colors))
          cons
          (default_punct (List.hd colors))
          "⟩"
    | EPureDefault e ->
      Format.fprintf fmt "%a%a%a"
        (default_punct (List.hd colors))
        "⟨" expr e
        (default_punct (List.hd colors))
        "⟩"
    | EEmpty -> lit_style fmt "∅"
    | EErrorOnEmpty e' ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" op_style "error_empty"
        (rhs exprc) e'
    | EPos p -> Format.fprintf fmt "<%s>" (Pos.to_string_shorter p)
    | EAssert e' ->
      Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation
        "(" (rhs exprc) e' punctuation ")"
    | EFatalError err ->
      Format.fprintf fmt "@[<hov 2>%a@ @{<red>%s@}@]" keyword "error"
        (Runtime.error_to_string err)
    | ELocation loc -> location fmt loc
    | EDStructAccess { e; field; _ } ->
      Format.fprintf fmt "@[<hv 2>%a%a@,%a%a%a@]" (lhs exprc) e punctuation
        "." punctuation "\"" Ident.format field punctuation "\""
    | EDStructAmend { e; fields; _ } ->
      Format.fprintf fmt "@[<hv 2>@[<hov>%a %a@ with@]@ %a@;<1 -2>%a@]"
        punctuation "{" (lhs exprc) e
        (Ident.Map.format_bindings ~pp_sep:Format.pp_print_space
            (fun fmt pp_field_name field_expr ->
              Format.fprintf fmt "@[<hv 2>%t %a@ %a%a@]" pp_field_name
                punctuation "=" (lhs exprc) field_expr punctuation ";"))
        fields punctuation "}"
    | EStruct { name; fields } ->
      if StructField.Map.is_empty fields then (
        punctuation fmt "{";
        StructName.format fmt name;
        punctuation fmt "}")
      else
        Format.fprintf fmt "@[<hv 2>%a %a@ %a@;<1 -2>%a@]" punctuation "{"
          StructName.format name
          (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
              (fun fmt pp_field_name field_expr ->
                Format.fprintf fmt "@[<hv 2>%t %a@ %a%a@]" pp_field_name
                  punctuation "=" (lhs exprc) field_expr punctuation ";"))
          fields punctuation "}"
    | EStructAccess { e; field; _ } ->
      Format.fprintf fmt "@[<hv 2>%a%a@,%a@]" (lhs exprc) e punctuation "."
        StructField.format field
    | EInj { e; cons; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" EnumConstructor.format cons
        (paren ~rhs:false exprc) e
    | EMatch { e; cases; _ } ->
      Format.fprintf fmt "@[<v 0>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]" keyword
        "match" (lhs exprc) e keyword "with"
        (EnumConstructor.Map.format_bindings
            (fun fmt pp_cons_name case_expr ->
              match case_expr with
              | EAbs { binder; tys; _ }, _ ->
                let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
                let expr = exprb bnd_ctx in
                let pp_args fmt =
                  match tys with
                  | [(TLit TUnit, _)] -> ()
                  | _ ->
                    Format.pp_print_seq ~pp_sep:Format.pp_print_space var fmt
                      (Array.to_seq xs);
                    Format.pp_print_space fmt ()
                in
                Format.fprintf fmt "@[<hov 2>%a %t@ %t%a@ %a@]" punctuation "|"
                  pp_cons_name pp_args punctuation "→" (rhs expr) body
              | e ->
                Format.fprintf fmt "@[<hov 2>%a %t@ %a@ %a@]" punctuation "|"
                  pp_cons_name punctuation "→" (rhs exprc) e))
        cases
    | EScopeCall { scope; args } ->
      Format.pp_open_hovbox fmt 2;
      ScopeName.format fmt scope;
      Format.pp_print_space fmt ();
      keyword fmt "of";
      Format.pp_print_space fmt ();
      Format.pp_open_hvbox fmt 2;
      punctuation fmt "{";
      ScopeVar.Map.format_bindings
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
        (fun fmt pp_field_name (_, field_expr) ->
          Format.fprintf fmt "%a%t%a%a@ %a" punctuation "\"" pp_field_name
            punctuation "\"" punctuation "=" (rhs exprc) field_expr)
        fmt args;
      Format.pp_close_box fmt ();
      punctuation fmt "}";
      Format.pp_close_box fmt ()
    | ECustom _ -> Format.pp_print_string fmt "<obj>"
    | EHole _ -> Format.pp_print_string fmt "□"

let expr ppf e = expr_aux Bindlib.empty_ctxt colors ppf e

let print_expr ?(fmt=Message.std_ppf ()) (e : ('a, 'm) gexpr) = Format.fprintf fmt "%a@." expr e

let rec trace_aux :
    type a t.
    Bindlib.ctxt ->
    Ocolor_types.color4 list ->
    Format.formatter ->
    (a, t) Trace_ast.t ->
    unit = 
  fun bnd_ctx colors fmt tr ->
    
  let traceb bnd_ctx colors tr = trace_aux bnd_ctx colors tr in
  let tracec colors tr = traceb bnd_ctx colors tr in
  let trace tr = tracec colors tr in
  let exprb bnd_ctx colors e = expr_aux bnd_ctx colors e in
  (*let exprc colors e = exprb bnd_ctx colors e in*)
  (*let expr e = exprc colors e in*)
  let operator fmt op = operator ~debug:false fmt op in 
  let default_punct = with_color (fun fmt -> Format.pp_print_as fmt 1) in

  let paren (*~rhs*) ?(colors = colors) expr fmt e1 =
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      expr (List.tl colors) fmt e1;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")"
  in
  (*let lhs ?(colors = colors) ex = paren ~colors (*~rhs:false*) ex in*)
  let rhs ex = paren (*~rhs:true*) ex in

  let paren_tr ~rhs ?(colors = colors) trace fmt tr1 =
    if Precedence.needs_parens_tr ~rhs ~context:tr tr1 then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      trace (List.tl colors) fmt tr1;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else trace colors fmt tr1
  in
  let lhs_tr ?(colors = colors) ex = paren_tr ~colors ~rhs:false ex in
  let rhs_tr ex = paren_tr ~rhs:true ex in

    match tr with
    | TrExpr _ -> Format.pp_print_string fmt "□"(*expr_aux bnd_ctx colors fmt e*)
    | TrVar v -> var fmt v
    | TrExternal { name } -> external_ref fmt name
    | TrTuple es ->
      Format.fprintf fmt "@[<hov 2>%a%a%a@]"
        (pp_color_string (List.hd colors))
        "("
        (Format.pp_print_list
            ~pp_sep:(fun fmt () ->
              Format.fprintf fmt "%a@ " (pp_color_string (List.hd colors)) ",")
            (fun fmt tr -> lhs_tr ~colors:(List.tl colors) tracec fmt tr))
        es
        (pp_color_string (List.hd colors))
        ")"
    | TrArray es ->
      Format.fprintf fmt "@[<hv 2>%a@,@[<hov>%a@]@;<0 -2>%a@]" punctuation "["
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
            (fun fmt tr -> lhs_tr tracec fmt tr))
        es punctuation "]"
    | TrTupleAccess { tr; index; _ } ->
      lhs_tr tracec fmt tr;
      punctuation fmt ".";
      Format.pp_print_int fmt index
    | TrLit l -> lit fmt l
    | TrApp { trf = TrAbs _; trv; _ } ->
      let pr bnd_ctx colors fmt = function
        | Trace_ast.TrApp { trf = TrAbs { binder; pos = _; tys }; trargs; _ } ->
          let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
          let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
          let xs_tau_arg =
            List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau trargs
          in
          Format.pp_print_list
            (fun fmt (x, tau, arg) ->
              Format.fprintf fmt
                "@[<hv 2>@[<hov 4>%a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]" keyword
                "let" var x punctuation ":" (typ_gen None ~colors) tau
                punctuation "=" (tracec colors) arg keyword "in")
            fmt xs_tau_arg;
          Format.pp_print_cut fmt ();
          rhs (exprb bnd_ctx) fmt body
        | tr -> rhs_tr (traceb bnd_ctx) fmt tr
      in
      Format.pp_open_vbox fmt 0;
      pr bnd_ctx colors fmt tr;
      Format.pp_print_cut fmt ();
      Format.fprintf fmt "@[<hv 4>%a@ %a@]"
      punctuation "↳" (tracec colors) trv;
      Format.pp_print_cut fmt ();
      Format.pp_close_box fmt ()
    | TrAbs { binder; pos = _; tys } ->
      let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
      let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
      Format.fprintf fmt "@[<hv 0>%a @[<hv 2>%a@]@ @]%a@ %a" punctuation "λ"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun fmt (x, tau) ->
              match tau with
              | TLit TUnit, _ ->
                punctuation fmt "(";
                punctuation fmt ")"
              | _ ->
                punctuation fmt "(";
                Format.pp_open_hvbox fmt 2;
                var fmt x;
                punctuation fmt ":";
                Format.pp_print_space fmt ();
                typ_gen None ~colors fmt tau;
                Format.pp_close_box fmt ();
                punctuation fmt ")"))
        xs_tau punctuation "→" (rhs (exprb bnd_ctx)) body
    | TrAppOp { op = ((Map | Filter) as op), _; trargs = [trarg1; trarg2]; _ } ->
      Format.fprintf fmt "@[<hv 2>%a %a@ %a@]" operator op (lhs_tr tracec) trarg1
        (rhs_tr tracec) trarg2
    | TrAppOp { op = (Log _ as op), _; trargs = [trarg1]; _ } ->
      Format.fprintf fmt "@[<hv 0>%a@ %a@]" operator op (rhs_tr tracec) trarg1
    | TrAppOp { op = op0, _; trargs = [_; _]; _ } ->
      let prec = Precedence.trace tr in
      let rec pr colors fmt = function
        (* Flatten sequences of the same associative op *)
        | Trace_ast.TrAppOp { op = op, _; trargs = [trarg1; trarg2]; _ } when op = op0 -> (
          (match prec with
          | Op (And | Or | Mul | Add | Div | Sub) -> lhs_tr pr fmt trarg1
          | _ -> lhs_tr tracec fmt trarg1);
          Format.pp_print_space fmt ();
          operator fmt op;
          Format.pp_print_char fmt ' ';
          match prec with
          | Op (And | Or | Mul | Add) -> rhs_tr pr fmt trarg2
          | _ -> rhs_tr tracec fmt trarg2)
        | tr -> tracec colors fmt tr
      in
      Format.pp_open_hvbox fmt 0;
      pr colors fmt tr;
      Format.pp_close_box fmt ()
    | TrAppOp { op = op, _; trargs = [trarg1]; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op (rhs_tr tracec) trarg1
    | TrAppOp { op = op, _; trargs; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (rhs_tr tracec))
        trargs
    | TrApp { trf; trargs; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" (lhs_tr tracec) trf
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (rhs_tr tracec))
        trargs
    | TrIfThenElse _ ->
      let rec pr els fmt = function
        | Trace_ast.TrIfThenElse { trcond; trtrue; trfalse } ->
          Format.fprintf fmt "@[<hv 2>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]@ %a"
            keyword
            (if els then "else if" else "if")
            trace trcond keyword "then" trace trtrue (pr true) trfalse
        | tr ->
          Format.fprintf fmt "@[<hv 2>%a@ %a@]" keyword "else" (rhs_tr tracec) tr
      in
      Format.pp_open_hvbox fmt 0;
      pr false fmt tr;
      Format.pp_close_box fmt ()
    | TrDefault { trexcepts; trjust; trcons; _ } ->
      if List.length trexcepts = 0 then
        Format.fprintf fmt "@[<hv 1>%a%a@ %a %a%a@]"
          (default_punct (List.hd colors))
          "⟨"
          (tracec (List.tl colors))
          trjust
          (default_punct (List.hd colors))
          "⊢"
          (tracec (List.tl colors))
          trcons
          (default_punct (List.hd colors))
          "⟩"
      else
        Format.fprintf fmt
          "@[<hv 0>@[<hov 2>%a %a@]@ @[<hov 2>%a %a@ %a %a@] %a@]"
          (default_punct (List.hd colors))
          "⟨"
          (Format.pp_print_list
              ~pp_sep:(fun fmt () ->
                Format.fprintf fmt "%a@ " (default_punct (List.hd colors)) ",")
              (lhs_tr ~colors:(List.tl colors) tracec))
          trexcepts
          (default_punct (List.hd colors))
          "|"
          (tracec (List.tl colors))
          trjust
          (default_punct (List.hd colors))
          "⊢"
          (tracec (List.tl colors))
          trcons
          (default_punct (List.hd colors))
          "⟩"
    | TrPureDefault tr ->
      Format.fprintf fmt "%a%a%a"
        (default_punct (List.hd colors))
        "⟨" trace tr
        (default_punct (List.hd colors))
        "⟩"
    | TrEmpty -> lit_style fmt "∅"
    | TrErrorOnEmpty tr' ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" op_style "error_empty"
        (rhs_tr tracec) tr'
    | TrAssert tr' ->
      Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation
        "(" (rhs_tr tracec) tr' punctuation ")"
    | TrFatalError { err; tr } ->
      Format.fprintf fmt "@[<hv 2>%a@]@.@[<hov 4>%a@ %a@ @{<red>%s@}@]" 
        trace tr punctuation "↳"
        keyword "error" (Runtime.error_to_string err)
    | TrStruct { name; fields } ->
      if StructField.Map.is_empty fields then (
        punctuation fmt "{";
        StructName.format fmt name;
        punctuation fmt "}")
      else
        Format.fprintf fmt "@[<hv 2>%a %a@ %a@;<1 -2>%a@]" punctuation "{"
          StructName.format name
          (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
              (fun fmt pp_field_name field_expr ->
                Format.fprintf fmt "@[<hv 2>%t %a@ %a%a@]" pp_field_name
                  punctuation "=" (lhs_tr tracec) field_expr punctuation ";"))
          fields punctuation "}"
    | TrStructAccess { tr; field; _ } ->
      Format.fprintf fmt "@[<hv 2>%a%a@,%a@]" (lhs_tr tracec) tr punctuation "."
        StructField.format field
    | TrInj { tr; cons; _ } ->
      Format.fprintf fmt "@[<hv 2>%a@ %a@]" EnumConstructor.format cons
        (paren (*~rhs:false*) tracec) tr
    | TrMatch { tr; cases; _ } ->
      Format.fprintf fmt "@[<v 0>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]" keyword
        "match" (lhs_tr tracec) tr keyword "with"
        (EnumConstructor.Map.format_bindings
            (fun fmt pp_cons_name case_expr ->
              match case_expr with
              | Trace_ast.TrAbs { binder; tys; _ } ->
                let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
                let pp_args fmt =
                  match tys with
                  | [(TLit TUnit, _)] -> ()
                  | _ ->
                    Format.pp_print_seq ~pp_sep:Format.pp_print_space var fmt
                      (Array.to_seq xs);
                    Format.pp_print_space fmt ()
                in
                Format.fprintf fmt "@[<hov 2>%a %t@ %t%a@ %a@]" punctuation "|"
                  pp_cons_name pp_args punctuation "→" (rhs (exprb bnd_ctx)) body
              | tr ->
                Format.fprintf fmt "@[<hov 2>%a %t@ %a@ %a@]" punctuation "|"
                  pp_cons_name punctuation "→" (rhs_tr tracec) tr))
        cases
    | TrCustom _ -> Format.pp_print_string fmt "<obj>"
    | TrHole _ -> Format.pp_print_string fmt "□"

let trace ppf tr = trace_aux Bindlib.empty_ctxt colors ppf tr

let print_trace ?(fmt=Message.std_ppf ()) (tr: ('a, 'm) Trace_ast.t) = Format.fprintf fmt "%a@." trace tr