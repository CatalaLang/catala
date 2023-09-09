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

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool b -> Print.lit fmt (LBool b)
  | LInt i ->
    Format.fprintf fmt "integer_of_string@ \"%s\"" (Runtime.integer_to_string i)
  | LUnit -> Print.lit fmt LUnit
  | LRat i -> Format.fprintf fmt "decimal_of_string \"%a\"" Print.lit (LRat i)
  | LMoney e ->
    Format.fprintf fmt "money_of_cents_string@ \"%s\""
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "date_of_numbers (%d) (%d) (%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "duration_of_numbers (%d) (%d) (%d)" years months days

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list)
    : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%a\"" Uid.MarkedString.format info))
    uids

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%s\""
           (Re.replace sanitize_quotes ~f:(fun _ -> "\\\"") info)))
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
    "Runtime";
    "Oper";
  ]

let ocaml_keywords_set = String.Set.of_list ocaml_keywords

let avoid_keywords (s : string) : string =
  if String.Set.mem s ocaml_keywords_set then s ^ "_user" else s
(* Fixme: this could cause clashes if the user program contains both e.g. [new]
   and [new_user] *)

let format_struct_name (fmt : Format.formatter) (v : StructName.t) : unit =
  Format.asprintf "%a" StructName.format v
  |> String.to_ascii
  |> String.to_snake_case
  |> avoid_keywords
  |> Format.fprintf fmt "%s"

let format_to_module_name
    (fmt : Format.formatter)
    (name : [< `Ename of EnumName.t | `Sname of StructName.t ]) =
  (match name with
  | `Ename v -> Format.asprintf "%a" EnumName.format v
  | `Sname v -> Format.asprintf "%a" StructName.format v)
  |> String.to_ascii
  |> avoid_keywords
  |> Format.fprintf fmt "%s"

let format_struct_field_name
    (fmt : Format.formatter)
    ((sname_opt, v) : StructName.t option * StructField.t) : unit =
  (match sname_opt with
  | Some sname ->
    Format.fprintf fmt "%a.%s" format_to_module_name (`Sname sname)
  | None -> Format.fprintf fmt "%s")
    (avoid_keywords
       (String.to_ascii (Format.asprintf "%a" StructField.format v)))

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (String.to_snake_case
          (String.to_ascii (Format.asprintf "%a" EnumName.format v))))

let format_enum_cons_name (fmt : Format.formatter) (v : EnumConstructor.t) :
    unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (String.to_ascii (Format.asprintf "%a" EnumConstructor.format v)))

let rec typ_embedding_name (fmt : Format.formatter) (ty : typ) : unit =
  match Mark.remove ty with
  | TLit TUnit -> Format.fprintf fmt "embed_unit"
  | TLit TBool -> Format.fprintf fmt "embed_bool"
  | TLit TInt -> Format.fprintf fmt "embed_integer"
  | TLit TRat -> Format.fprintf fmt "embed_decimal"
  | TLit TMoney -> Format.fprintf fmt "embed_money"
  | TLit TDate -> Format.fprintf fmt "embed_date"
  | TLit TDuration -> Format.fprintf fmt "embed_duration"
  | TStruct s_name -> Format.fprintf fmt "embed_%a" format_struct_name s_name
  | TEnum e_name -> Format.fprintf fmt "embed_%a" format_enum_name e_name
  | TArray ty -> Format.fprintf fmt "embed_array (%a)" typ_embedding_name ty
  | _ -> Format.fprintf fmt "unembeddable"

let typ_needs_parens (e : typ) : bool =
  match Mark.remove e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : typ) : unit =
  let format_typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Mark.remove typ with
  | TLit l -> Format.fprintf fmt "%a" Print.tlit l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         format_typ_with_parens)
      ts
  | TStruct s -> Format.fprintf fmt "%a.t" format_to_module_name (`Sname s)
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>(%a)@] %a" format_typ_with_parens t
      format_enum_name Expr.option_enum
  | TEnum e -> Format.fprintf fmt "%a.t" format_to_module_name (`Ename e)
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ->@ ")
         format_typ_with_parens)
      (t1 @ [t2])
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ_with_parens t1
  | TAny -> Format.fprintf fmt "_"
  | TClosureEnv -> failwith "unimplemented!"

let format_var_str (fmt : Format.formatter) (v : string) : unit =
  let lowercase_name = String.to_snake_case (String.to_ascii v) in
  let lowercase_name =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.")
      ~subst:(fun _ -> "_dot_")
      lowercase_name
  in
  let lowercase_name = String.to_ascii lowercase_name in
  if
    List.mem lowercase_name ["handle_default"; "handle_default_opt"]
    (* O_O *)
    || String.begins_with_uppercase v
  then Format.pp_print_string fmt lowercase_name
  else if lowercase_name = "_" then Format.pp_print_string fmt lowercase_name
  else Format.fprintf fmt "%s_" lowercase_name

let format_var (fmt : Format.formatter) (v : 'm Var.t) : unit =
  format_var_str fmt (Bindlib.name_of v)

let needs_parens (e : 'm expr) : bool =
  match Mark.remove e with
  | EApp { f = EAbs _, _; _ }
  | ELit (LBool _ | LUnit)
  | EVar _ | ETuple _ | EOp _ ->
    false
  | _ -> true

let format_exception (fmt : Format.formatter) (exc : except Mark.pos) : unit =
  match Mark.remove exc with
  | ConflictError ->
    let pos = Mark.get exc in
    Format.fprintf fmt
      "(ConflictError@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@])"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)
  | EmptyError -> Format.fprintf fmt "EmptyError"
  | Crash -> Format.fprintf fmt "Crash"
  | NoValueProvided ->
    let pos = Mark.get exc in
    Format.fprintf fmt
      "(NoValueProvided@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@])"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)

let rec format_expr (ctx : decl_ctx) (fmt : Format.formatter) (e : 'm expr) :
    unit =
  let format_expr = format_expr ctx in
  let format_with_parens (fmt : Format.formatter) (e : 'm expr) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Mark.remove e with
  | EVar v -> Format.fprintf fmt "%a" format_var v
  | EExternal { name } -> (
    (* FIXME: this is wrong in general !! We assume the idents exposed by the
       module depend only on the original name, while they actually get through
       Bindlib and may have been renamed. A correct implem could use the runtime
       registration used by the interpreter, but that would be distasteful and
       incur a penalty ; or we would need to reproduce the same structure as in
       the original module to ensure that bindlib performs the exact same
       renamings ; or finally we could normalise the names at generation time
       (either at toplevel or in a dedicated submodule ?) *)
    let path =
      match Mark.remove name with
      | External_value name -> TopdefName.path name
      | External_scope name -> ScopeName.path name
    in
    Uid.Path.format fmt path;
    match Mark.remove name with
    | External_value name ->
      format_var_str fmt (Mark.remove (TopdefName.get_info name))
    | External_scope name ->
      format_var_str fmt (Mark.remove (ScopeName.get_info name)))
  | ETuple es ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_with_parens e))
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
  | ETupleAccess { e; index; size } ->
    Format.fprintf fmt "let@ %a@ = %a@ in@ x"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt i ->
           Format.pp_print_string fmt (if i = index then "x" else "_")))
      (List.init size Fun.id) format_with_parens e
  | EStructAccess { e; field; name } ->
    Format.fprintf fmt "%a.%a" format_with_parens e format_struct_field_name
      (Some name, field)
  | EInj { e; cons; name } ->
    Format.fprintf fmt "@[<hov 2>%a.%a@ %a@]" format_to_module_name
      (`Ename name) format_enum_cons_name cons format_with_parens e
  | EMatch { e; cases; name } ->
    Format.fprintf fmt "@[<hv>@[<hov 2>match@ %a@]@ with@\n| %a@]"
      format_with_parens e
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (c, e) ->
           Format.fprintf fmt "@[<hov 2>%a.%a %a@]" format_to_module_name
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
               | _ -> assert false
               (* should not happen *))
             e))
      (EnumConstructor.Map.bindings cases)
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.add (Expr.pos e) l)
  | EApp { f = EAbs { binder; tys }, _; args } ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) tys in
    let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
    Format.fprintf fmt "(%a%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
         (fun fmt (x, tau, arg) ->
           Format.fprintf fmt "@[<hov 2>let@ %a@ :@ %a@ =@ %a@]@ in@\n"
             format_var x format_typ tau format_with_parens arg))
      xs_tau_arg format_with_parens body
  | EAbs { binder; tys } ->
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
        f = EApp { f = EOp { op = Log (BeginCall, info); _ }, _; args = [f] }, _;
        args = [arg];
      }
    when Cli.globals.trace ->
    Format.fprintf fmt "(log_begin_call@ %a@ %a)@ %a" format_uid_list info
      format_with_parens f format_with_parens arg
  | EApp
      { f = EOp { op = Log (VarDef var_def_info, info); _ }, _; args = [arg1] }
    when Cli.globals.trace ->
    Format.fprintf fmt
      "(log_variable_definition@ %a@ {io_input=%s;@ io_output=%b}@ (%a)@ %a)"
      format_uid_list info
      (match var_def_info.log_io_input with
      | NoInput -> "NoInput"
      | OnlyInput -> "OnlyInput"
      | Reentrant -> "Reentrant")
      var_def_info.log_io_output typ_embedding_name
      (var_def_info.log_typ, Pos.no_pos)
      format_with_parens arg1
  | EApp { f = EOp { op = Log (PosRecordIfTrueBool, _); _ }, m; args = [arg1] }
    when Cli.globals.trace ->
    let pos = Expr.mark_pos m in
    Format.fprintf fmt
      "(log_decision_taken@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) format_with_parens arg1
  | EApp { f = EOp { op = Log (EndCall, info); _ }, _; args = [arg1] }
    when Cli.globals.trace ->
    Format.fprintf fmt "(log_end_call@ %a@ %a)" format_uid_list info
      format_with_parens arg1
  | EApp { f = EOp { op = Log _; _ }, _; args = [arg1] } ->
    Format.fprintf fmt "%a" format_with_parens arg1
  | EApp
      {
        f = EOp { op = (HandleDefault | HandleDefaultOpt) as op; _ }, pos;
        args;
      } ->
    Format.fprintf fmt
      "@[<hov 2>%s@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a@]"
      (Print.operator_to_string op)
      (Pos.get_file (Expr.mark_pos pos))
      (Pos.get_start_line (Expr.mark_pos pos))
      (Pos.get_start_column (Expr.mark_pos pos))
      (Pos.get_end_line (Expr.mark_pos pos))
      (Pos.get_end_column (Expr.mark_pos pos))
      format_string_list
      (Pos.get_law_info (Expr.mark_pos pos))
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EApp { f; args } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_with_parens f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EIfThenElse { cond; etrue; efalse } ->
    Format.fprintf fmt
      "@[<hov 2> if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@]"
      format_with_parens cond format_with_parens etrue format_with_parens efalse
  | EOp { op; _ } -> Format.pp_print_string fmt (Operator.name op)
  | EAssert e' ->
    Format.fprintf fmt
      "@[<hov 2>if@ %a@ then@ ()@ else@ raise (AssertionFailed @[<hov \
       2>{filename = \"%s\";@ start_line=%d;@ start_column=%d;@ end_line=%d; \
       end_column=%d;@ law_headings=%a}@])@]"
      format_with_parens e'
      (Pos.get_file (Expr.pos e'))
      (Pos.get_start_line (Expr.pos e'))
      (Pos.get_start_column (Expr.pos e'))
      (Pos.get_end_line (Expr.pos e'))
      (Pos.get_end_column (Expr.pos e'))
      format_string_list
      (Pos.get_law_info (Expr.pos e'))
  | ERaise exc ->
    Format.fprintf fmt "raise@ %a" format_exception (exc, Expr.pos e)
  | ECatch { body; exn; handler } ->
    Format.fprintf fmt
      "@,@[<hv>@[<hov 2>try@ %a@]@ with@]@ @[<hov 2>%a@ ->@ %a@]"
      format_with_parens body format_exception
      (exn, Expr.pos e)
      format_with_parens handler
  | _ -> .

let format_struct_embedding
    (fmt : Format.formatter)
    ((struct_name, struct_fields) : StructName.t * typ StructField.Map.t) =
  if StructField.Map.is_empty struct_fields then
    Format.fprintf fmt "let embed_%a (_: %a.t) : runtime_value = Unit@\n@\n"
      format_struct_name struct_name format_to_module_name (`Sname struct_name)
  else
    Format.fprintf fmt
      "@[<hov 2>let embed_%a (x: %a.t) : runtime_value =@ Struct([\"%a\"],@ \
       @[<hov 2>[%a]@])@]@\n\
       @\n"
      format_struct_name struct_name format_to_module_name (`Sname struct_name)
      StructName.format struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@\n")
         (fun fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "(\"%a\",@ %a@ x.%a)" StructField.format
             struct_field typ_embedding_name struct_field_type
             format_struct_field_name
             (Some struct_name, struct_field)))
      (StructField.Map.bindings struct_fields)

let format_enum_embedding
    (fmt : Format.formatter)
    ((enum_name, enum_cases) : EnumName.t * typ EnumConstructor.Map.t) =
  if EnumConstructor.Map.is_empty enum_cases then
    Format.fprintf fmt "let embed_%a (_: %a.t) : runtime_value = Unit@\n@\n"
      format_to_module_name (`Ename enum_name) format_enum_name enum_name
  else
    Format.fprintf fmt
      "@[<hv 2>@[<hov 2>let embed_%a@ @[<hov 2>(x:@ %a.t)@]@ : runtime_value \
       =@]@ Enum([\"%a\"],@ @[<hov 2>match x with@ %a@])@]@\n\
       @\n"
      format_enum_name enum_name format_to_module_name (`Ename enum_name)
      EnumName.format enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "@[<hov 2>| %a x ->@ (\"%a\", %a x)@]"
             format_enum_cons_name enum_cons EnumConstructor.format enum_cons
             typ_embedding_name enum_cons_type))
      (EnumConstructor.Map.bindings enum_cases)

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    if StructField.Map.is_empty struct_fields then
      Format.fprintf fmt
        "@[<v 2>module %a = struct@\n@[<hov 2>type t = unit@]@]@\nend@\n"
        format_to_module_name (`Sname struct_name)
    else
      Format.fprintf fmt
        "@[<v>@[<v 2>module %a = struct@ @[<hv 2>type t = {@,\
         %a@;\
         <0-2>}@]@]@ end@]@\n"
        format_to_module_name (`Sname struct_name)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "@[<hov 2>%a:@ %a@]" format_struct_field_name
               (None, struct_field) format_typ struct_field_type))
        (StructField.Map.bindings struct_fields);
    if Cli.globals.trace then
      format_struct_embedding fmt (struct_name, struct_fields)
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    Format.fprintf fmt
      "module %a = struct@\n@[<hov 2>@ type t =@\n@[<hov 2>  %a@]@\nend@]@\n"
      format_to_module_name (`Ename enum_name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "@[<hov 2>| %a@ of@ %a@]" format_enum_cons_name
             enum_cons format_typ enum_cons_type))
      (EnumConstructor.Map.bindings enum_cons);
    if Cli.globals.trace then format_enum_embedding fmt (enum_name, enum_cons)
  in
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | Scopelang.Dependency.TVertex.Enum _ -> false
        | Scopelang.Dependency.TVertex.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> Scopelang.Dependency.TVertex.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
        let def = StructName.Map.find s ctx.ctx_structs in
        if StructName.path s = [] then
          Format.fprintf fmt "%a@\n" format_struct_decl (s, def)
      | Scopelang.Dependency.TVertex.Enum e ->
        let def = EnumName.Map.find e ctx.ctx_enums in
        if EnumName.path e = [] then
          Format.fprintf fmt "%a@\n" format_enum_decl (e, def))
    (type_ordering @ scope_structs)

let rename_vars e =
  Expr.(
    unbox
      (rename_vars ~exclude:ocaml_keywords ~reset_context_for_closed_terms:true
         ~skip_constant_binders:true ~constant_binder_name:(Some "_") e))

let format_expr ctx fmt e = format_expr ctx fmt (rename_vars e)

let rec format_scope_body_expr
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (scope_lets : 'm Ast.expr scope_body_expr) : unit =
  match scope_lets with
  | Result e -> format_expr ctx fmt e
  | ScopeLet scope_let ->
    let scope_let_var, scope_let_next =
      Bindlib.unbind scope_let.scope_let_next
    in
    Format.fprintf fmt "@[<hov 2>let %a: %a = %a in@]@\n%a" format_var
      scope_let_var format_typ scope_let.scope_let_typ (format_expr ctx)
      scope_let.scope_let_expr
      (format_scope_body_expr ctx)
      scope_let_next

let format_code_items
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (code_items : 'm Ast.expr code_item_list) : 'm Ast.expr Var.t String.Map.t =
  Scope.fold_left
    ~f:(fun bnd item var ->
      match item with
      | Topdef (name, typ, e) ->
        Format.fprintf fmt "@\n@\n@[<hov 2>let %a : %a =@\n%a@]" format_var var
          format_typ typ (format_expr ctx) e;
        String.Map.add (Format.asprintf "%a" TopdefName.format name) var bnd
      | ScopeDef (name, body) ->
        let scope_input_var, scope_body_expr =
          Bindlib.unbind body.scope_body_expr
        in
        Format.fprintf fmt "@\n@\n@[<hov 2>let %a (%a: %a.t) : %a.t =@\n%a@]"
          format_var var format_var scope_input_var format_to_module_name
          (`Sname body.scope_body_input_struct) format_to_module_name
          (`Sname body.scope_body_output_struct)
          (format_scope_body_expr ctx)
          scope_body_expr;
        String.Map.add (Format.asprintf "%a" ScopeName.format name) var bnd)
    ~init:String.Map.empty code_items

let format_scope_exec
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (bnd : 'm Ast.expr Var.t String.Map.t)
    scope_name
    scope_body =
  let scope_name_str = Format.asprintf "%a" ScopeName.format scope_name in
  let scope_var = String.Map.find scope_name_str bnd in
  let scope_input =
    StructName.Map.find scope_body.scope_body_input_struct ctx.ctx_structs
  in
  if not (StructField.Map.is_empty scope_input) then
    Message.raise_error
      "The scope @{<bold>%s@} defines input variables.@ This is not supported \
       for a main scope at the moment."
      scope_name_str;
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "let () =";
  (* TODO: dump the output using yojson that should be already available from
     the runtime *)
  Format.pp_print_space fmt ();
  format_var fmt scope_var;
  Format.pp_print_space fmt ();
  Format.pp_print_string fmt "()";
  Format.pp_close_box fmt ()

let format_module_registration
    fmt
    (bnd : 'm Ast.expr Var.t String.Map.t)
    modname =
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "let () =";
  Format.pp_print_space fmt ();
  Format.pp_open_hvbox fmt 2;
  Format.fprintf fmt "Runtime_ocaml.Runtime.register_module %S" modname;
  Format.pp_print_space fmt ();
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "[ ";
  Format.pp_print_seq
    ~pp_sep:(fun fmt () ->
      Format.pp_print_char fmt ';';
      Format.pp_print_cut fmt ())
    (fun fmt (id, var) ->
      Format.fprintf fmt "@[<hov 2>%S,@ Obj.repr %a@]" id format_var var)
    fmt (String.Map.to_seq bnd);
  Format.pp_close_box fmt ();
  Format.pp_print_char fmt ' ';
  Format.pp_print_string fmt "]";
  Format.pp_print_space fmt ();
  Format.pp_print_string fmt "\"todo-module-hash\"";
  Format.pp_close_box fmt ();
  Format.pp_close_box fmt ()

let header =
  {ocaml|
(** This file has been generated by the Catala compiler, do not edit! *)

open Runtime_ocaml.Runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]

|ocaml}

let format_program
    (fmt : Format.formatter)
    ?register_module
    ?exec_scope
    (p : 'm Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Format.pp_print_string fmt header;
  format_ctx type_ordering fmt p.decl_ctx;
  let bnd = format_code_items p.decl_ctx fmt p.code_items in
  Format.pp_print_newline fmt ();
  match register_module, exec_scope with
  | Some modname, None -> format_module_registration fmt bnd modname
  | None, Some scope_name ->
    let scope_body = Program.get_scope_body p scope_name in
    format_scope_exec p.decl_ctx fmt bnd scope_name scope_body
  | None, None -> ()
  | Some _, Some _ ->
    Message.raise_error
      "OCaml generation: both module registration and top-level scope \
       execution where required at the same time."
