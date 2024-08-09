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

(** Typing for the default calculus. Because of the error terms, we perform type
    inference using the classical W algorithm with union-find unification. *)

open Catala_utils
module A = Definitions

type flags = { fail_on_any : bool; assume_op_types : bool }

module Any =
  Uid.Make
    (struct
      type info = unit

      let to_string () = "any"
      let format fmt () = Format.fprintf fmt "any"
      let equal () () = true
      let compare () () = 0
      let hash () = Hash.raw `Any
    end)
    (struct
      let style = Ocolor_types.(Fg (C4 hi_magenta))
    end)
    ()

type unionfind_typ = naked_typ Mark.pos UnionFind.elem
(** We do not reuse {!type: A.typ} because we have to include a new [TAny]
    variant. Indeed, error terms can have any type and this has to be captured
    by the type sytem. *)

and naked_typ =
  | TLit of A.typ_lit
  | TArrow of unionfind_typ list * unionfind_typ
  | TTuple of unionfind_typ list
  | TStruct of A.StructName.t
  | TEnum of A.EnumName.t
  | TOption of unionfind_typ
  | TArray of unionfind_typ
  | TDefault of unionfind_typ
  | TAny of Any.t
  | TClosureEnv

let rec typ_to_ast ~(flags : flags) (ty : unionfind_typ) : A.typ =
  let typ_to_ast = typ_to_ast ~flags in
  let ty, pos = UnionFind.get (UnionFind.find ty) in
  match ty with
  | TLit l -> A.TLit l, pos
  | TTuple ts -> A.TTuple (List.map typ_to_ast ts), pos
  | TStruct s -> A.TStruct s, pos
  | TEnum e -> A.TEnum e, pos
  | TOption t -> A.TOption (typ_to_ast t), pos
  | TArrow (t1, t2) -> A.TArrow (List.map typ_to_ast t1, typ_to_ast t2), pos
  | TArray t1 -> A.TArray (typ_to_ast t1), pos
  | TDefault t1 -> A.TDefault (typ_to_ast t1), pos
  | TAny _ ->
    if flags.fail_on_any then
      (* No polymorphism in Catala: type inference should return full types
         without wildcards, and this function is used to recover the types after
         typing. *)
      Message.error ~pos
        "Internal error: typing at this point could not be resolved"
    else A.TAny, pos
  | TClosureEnv -> TClosureEnv, pos

let rec ast_to_typ (ty : A.typ) : unionfind_typ =
  let ty' =
    match Mark.remove ty with
    | A.TLit l -> TLit l
    | A.TArrow (t1, t2) -> TArrow (List.map ast_to_typ t1, ast_to_typ t2)
    | A.TTuple ts -> TTuple (List.map ast_to_typ ts)
    | A.TStruct s -> TStruct s
    | A.TEnum e -> TEnum e
    | A.TOption t -> TOption (ast_to_typ t)
    | A.TArray t -> TArray (ast_to_typ t)
    | A.TDefault t -> TDefault (ast_to_typ t)
    | A.TAny -> TAny (Any.fresh ())
    | A.TClosureEnv -> TClosureEnv
  in
  UnionFind.make (Mark.copy ty ty')

(** {1 Types and unification} *)

let typ_needs_parens (t : unionfind_typ) : bool =
  let t = UnionFind.get (UnionFind.find t) in
  match Mark.remove t with TArrow _ | TArray _ -> true | _ -> false

let with_color f color fmt x =
  (* equivalent to [Format.fprintf fmt "@{<color>%s@}" s] *)
  Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg (C4 color)));
  f fmt x;
  Format.pp_close_stag fmt ()

let pp_color_string = with_color Format.pp_print_string

let rec format_typ
    (ctx : A.decl_ctx)
    ~(colors : Ocolor_types.color4 list)
    (fmt : Format.formatter)
    (naked_typ : unionfind_typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens
      ~colors
      (fmt : Format.formatter)
      (t : unionfind_typ) =
    if typ_needs_parens t then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      format_typ ~colors:(List.tl colors) fmt t;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else Format.fprintf fmt "%a" (format_typ ~colors) t
  in
  let naked_typ = UnionFind.get (UnionFind.find naked_typ) in
  match Mark.remove naked_typ with
  | TLit l -> Format.fprintf fmt "%a" Print.tlit l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         (fun fmt t -> format_typ fmt ~colors:(List.tl colors) t))
      ts
      (pp_color_string (List.hd colors))
      ")"
  | TStruct s -> A.StructName.format fmt s
  | TEnum e -> A.EnumName.format fmt e
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>option %a@]"
      (format_typ_with_parens ~colors:(List.tl colors))
      t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ →@ %a@]"
      (format_typ_with_parens ~colors)
      t1 (format_typ ~colors) t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@ →@ %a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_typ_with_parens ~colors:(List.tl colors)))
      t1
      (pp_color_string (List.hd colors))
      ")" (format_typ ~colors) t2
  | TArray t1 -> (
    match Mark.remove (UnionFind.get (UnionFind.find t1)) with
    | TAny _ when not Global.options.debug -> Format.pp_print_string fmt "list"
    | _ -> Format.fprintf fmt "@[list of@ %a@]" (format_typ ~colors) t1)
  | TDefault t1 ->
    Format.pp_print_as fmt 1 "⟨";
    format_typ ~colors fmt t1;
    Format.pp_print_as fmt 1 "⟩"
  | TAny v ->
    if Global.options.debug then Format.fprintf fmt "<a%d>" (Any.id v)
    else Format.pp_print_string fmt "<any>"
  | TClosureEnv -> Format.fprintf fmt "closure_env"

let rec colors =
  let open Ocolor_types in
  blue :: cyan :: green :: yellow :: red :: magenta :: colors

let dummy_flags = { fail_on_any = false; assume_op_types = false }
let format_typ ctx fmt naked_typ = format_typ ctx ~colors fmt naked_typ

let record_type_error _ctx (A.AnyExpr e) t1 t2 =
  (* We convert union-find types to ast ones otherwise error messages would be
     hindered as union-find side-effects wrongly unify both types. The delayed
     pretty-printing would yield messages such as: 'incompatible types (integer,
     integer)' *)
  let t1_repr = typ_to_ast ~flags:dummy_flags t1 in
  let t2_repr = typ_to_ast ~flags:dummy_flags t2 in
  let e_pos = Expr.pos e in
  let t1_pos = Mark.get t1_repr in
  let t2_pos = Mark.get t2_repr in
  let pp_typ = Print.typ_debug in
  let fmt_pos =
    if e_pos = t1_pos then
      [
        ( (fun ppf ->
            Format.fprintf ppf "@[<hv 2>@[<hov>%a@ %a@]:" Format.pp_print_text
              "This expression has type" pp_typ t1_repr;
            if Global.options.debug then
              Format.fprintf ppf "@ %a@]" Expr.format e
            else Format.pp_close_box ppf ()),
          e_pos );
        ( (fun ppf ->
            Format.fprintf ppf
              "@[<hov>Expected@ type@ %a@ coming@ from@ expression:@]" pp_typ
              t2_repr),
          t2_pos );
      ]
    else
      [
        ( (fun ppf ->
            Format.fprintf ppf "@[<hv 2>@[<hov>%a:@]" Format.pp_print_text
              "While typechecking the following expression";
            if Global.options.debug then
              Format.fprintf ppf "@ %a@]" Expr.format e
            else Format.pp_close_box ppf ()),
          e_pos );
        ( (fun ppf ->
            Format.fprintf ppf "@[<hov>Type@ %a@ is@ coming@ from:@]" pp_typ
              t1_repr),
          t1_pos );
        ( (fun ppf ->
            Format.fprintf ppf "@[<hov>Type@ %a@ is@ coming@ from:@]" pp_typ
              t2_repr),
          t2_pos );
      ]
  in
  Message.delayed_error ~kind:Typing () ~fmt_pos
    "Error during typechecking, incompatible types:@\n\
     @[<v>@{<blue>@<2>%s@} @[<hov>%a@]@,\
     @{<blue>@<2>%s@} @[<hov>%a@]@]" "─➤" pp_typ t1_repr "─➤" pp_typ t2_repr

(** Raises an error if unification cannot be performed. The position annotation
    of the second [unionfind_typ] argument is propagated (unless it is [TAny]). *)
let rec unify
    (ctx : A.decl_ctx)
    (e : ('a, 'm) A.gexpr) (* used for error context *)
    (t1 : unionfind_typ)
    (t2 : unionfind_typ) : unit =
  let unify = unify ctx in
  (* Message.debug "Unifying %a and %a" (format_typ ctx) t1 (format_typ ctx)
     t2; *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let record_type_error () = record_type_error ctx (A.AnyExpr e) t1 t2 in
  let () =
    match Mark.remove t1_repr, Mark.remove t2_repr with
    | TLit tl1, TLit tl2 -> if tl1 <> tl2 then record_type_error ()
    | TArrow (t11, t12), TArrow (t21, t22) -> (
      unify e t12 t22;
      try List.iter2 (unify e) t11 t21
      with Invalid_argument _ -> record_type_error ())
    | TTuple ts1, TTuple ts2 -> (
      try List.iter2 (unify e) ts1 ts2
      with Invalid_argument _ -> record_type_error ())
    | TStruct s1, TStruct s2 ->
      if not (A.StructName.equal s1 s2) then record_type_error ()
    | TEnum e1, TEnum e2 ->
      if not (A.EnumName.equal e1 e2) then record_type_error ()
    | TOption t1, TOption t2 -> unify e t1 t2
    | TArray t1', TArray t2' -> unify e t1' t2'
    | TDefault t1', TDefault t2' -> unify e t1' t2'
    | TClosureEnv, TClosureEnv -> ()
    | TAny _, _ | _, TAny _ -> ()
    | ( ( TLit _ | TArrow _ | TTuple _ | TStruct _ | TEnum _ | TOption _
        | TArray _ | TDefault _ | TClosureEnv ),
        _ ) ->
      record_type_error ()
  in
  ignore
  @@ UnionFind.merge
       (fun t1 t2 -> match Mark.remove t2 with TAny _ -> t1 | _ -> t2)
       t1 t2

let lit_type (lit : A.lit) : naked_typ =
  match lit with
  | LBool _ -> TLit TBool
  | LInt _ -> TLit TInt
  | LRat _ -> TLit TRat
  | LMoney _ -> TLit TMoney
  | LDate _ -> TLit TDate
  | LDuration _ -> TLit TDuration
  | LUnit -> TLit TUnit

(** [op_type] and [resolve_overload] are a bit similar, and work on disjoint
    sets of operators. However, their assumptions are different so we keep the
    functions separate. In particular [resolve_overloads] requires its argument
    types to be known in advance. *)

let polymorphic_op_type (op : Operator.polymorphic A.operator Mark.pos) :
    unionfind_typ =
  let open Operator in
  let pos = Mark.get op in
  let any = lazy (UnionFind.make (TAny (Any.fresh ()), pos)) in
  let any2 = lazy (UnionFind.make (TAny (Any.fresh ()), pos)) in
  let any3 = lazy (UnionFind.make (TAny (Any.fresh ()), pos)) in
  let bt = lazy (UnionFind.make (TLit TBool, pos)) in
  let it = lazy (UnionFind.make (TLit TInt, pos)) in
  let cet = lazy (UnionFind.make (TClosureEnv, pos)) in
  let array a = lazy (UnionFind.make (TArray (Lazy.force a), pos)) in
  let option a = lazy (UnionFind.make (TOption (Lazy.force a), pos)) in
  let ( @-> ) x y =
    lazy (UnionFind.make (TArrow (List.map Lazy.force x, Lazy.force y), pos))
  in
  let ty =
    match Mark.remove op with
    | Fold -> [[any2; any] @-> any2; any2; array any] @-> any2
    | Eq -> [any; any] @-> bt
    | Map -> [[any] @-> any2; array any] @-> array any2
    | Map2 -> [[any; any2] @-> any3; array any; array any2] @-> array any3
    | Filter -> [[any] @-> bt; array any] @-> array any
    | Reduce -> [[any; any] @-> any; any; array any] @-> any
    | Concat -> [array any; array any] @-> array any
    | Log (PosRecordIfTrueBool, _) -> [bt] @-> bt
    | Log _ -> [any] @-> any
    | Length -> [array any] @-> it
    | HandleExceptions -> [array (option any)] @-> option any
    | ToClosureEnv -> [any] @-> cet
    | FromClosureEnv -> [cet] @-> any
  in
  Lazy.force ty

(* Just returns the return type of the operator, assuming the operand types are
   known. Less trict, but useful on monomorphised code where the operators no
   longer have their standard types *)
let polymorphic_op_return_type
    ctx
    e
    (op : Operator.polymorphic A.operator Mark.pos)
    (targs : unionfind_typ list) : unionfind_typ =
  let open Operator in
  let pos = Mark.get op in
  let uf t = UnionFind.make (t, pos) in
  let any _ = uf (TAny (Any.fresh ())) in
  let return_type tf arity =
    let tret = any () in
    unify ctx e tf (UnionFind.make (TArrow (List.init arity any, tret), pos));
    tret
  in
  match Mark.remove op, targs with
  | (Fold | Reduce), [_; tau; _] -> tau
  | Eq, _ -> uf (TLit TBool)
  | Map, [tf; _] -> uf (TArray (return_type tf 1))
  | Map2, [tf; _; _] -> uf (TArray (return_type tf 2))
  | (Filter | Concat), [_; tau] -> tau
  | Log (PosRecordIfTrueBool, _), _ -> uf (TLit TBool)
  | Log _, [tau] -> tau
  | Length, _ -> uf (TLit TInt)
  | HandleExceptions, [_] -> any ()
  | ToClosureEnv, _ -> uf TClosureEnv
  | FromClosureEnv, _ -> any ()
  | op, targs ->
    Message.error ~pos "Mismatched operator arguments: %a@ (%a)"
      (Print.operator ?debug:None)
      op
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (format_typ ctx))
      targs

let resolve_overload_ret_type
    ~flags
    (ctx : A.decl_ctx)
    _e
    (op : Operator.overloaded A.operator Mark.pos)
    tys : unionfind_typ =
  let op_ty =
    Operator.overload_type ctx op (List.map (typ_to_ast ~flags) tys)
  in
  ast_to_typ (Type.arrow_return op_ty)

(** {1 Double-directed typing} *)

module Env = struct
  type 'e t = {
    flags : flags;
    structs : unionfind_typ A.StructField.Map.t A.StructName.Map.t;
    enums : unionfind_typ A.EnumConstructor.Map.t A.EnumName.Map.t;
    vars : ('e, unionfind_typ) Var.Map.t;
    scope_vars : A.typ A.ScopeVar.Map.t;
    scopes : A.typ A.ScopeVar.Map.t A.ScopeName.Map.t;
    scopes_input : A.typ A.ScopeVar.Map.t A.ScopeName.Map.t;
    toplevel_vars : A.typ A.TopdefName.Map.t;
  }

  let empty
      ?(fail_on_any = true)
      ?(assume_op_types = false)
      (decl_ctx : A.decl_ctx) =
    (* We fill the environment initially with the structs and enums
       declarations *)
    {
      flags = { fail_on_any; assume_op_types };
      structs =
        A.StructName.Map.map
          (fun ty -> A.StructField.Map.map ast_to_typ ty)
          decl_ctx.ctx_structs;
      enums =
        A.EnumName.Map.map
          (fun ty -> A.EnumConstructor.Map.map ast_to_typ ty)
          decl_ctx.ctx_enums;
      vars = Var.Map.empty;
      scope_vars = A.ScopeVar.Map.empty;
      scopes = A.ScopeName.Map.empty;
      scopes_input = A.ScopeName.Map.empty;
      toplevel_vars = A.TopdefName.Map.empty;
    }

  let get t v = Var.Map.find_opt v t.vars
  let get_scope_var t sv = A.ScopeVar.Map.find_opt sv t.scope_vars
  let get_toplevel_var t v = A.TopdefName.Map.find_opt v t.toplevel_vars
  let add v tau t = { t with vars = Var.Map.add v tau t.vars }
  let add_var v typ t = add v (ast_to_typ typ) t

  let add_scope_var v typ t =
    { t with scope_vars = A.ScopeVar.Map.add v typ t.scope_vars }

  let add_scope scope_name ~vars ~in_vars t =
    {
      t with
      scopes = A.ScopeName.Map.add scope_name vars t.scopes;
      scopes_input = A.ScopeName.Map.add scope_name in_vars t.scopes_input;
    }

  let add_toplevel_var v typ t =
    { t with toplevel_vars = A.TopdefName.Map.add v typ t.toplevel_vars }

  let open_scope scope_name t =
    let scope_vars =
      A.ScopeVar.Map.disjoint_union t.scope_vars
        (A.ScopeName.Map.find scope_name t.scopes)
    in
    { t with scope_vars }

  let dump ppf env =
    let pp_sep = Format.pp_print_space in
    Format.pp_open_vbox ppf 0;
    (* Format.fprintf ppf "structs: @[<hov>%a@]@,"
     *   (A.StructName.Map.format_keys ~pp_sep) env.structs;
     * Format.fprintf ppf "enums: @[<hov>%a@]@,"
     *   (A.EnumName.Map.format_keys ~pp_sep) env.enums;
     * Format.fprintf ppf "vars: @[<hov>%a@]@,"
     *   (Var.Map.format_keys ~pp_sep) env.vars; *)
    Format.fprintf ppf "scopes: @[<hov>%a@]@,"
      (A.ScopeName.Map.format_keys ~pp_sep)
      env.scopes;
    Format.fprintf ppf "topdefs: @[<hov>%a@]@,"
      (A.TopdefName.Map.format_keys ~pp_sep)
      env.toplevel_vars;
    Format.pp_close_box ppf ()
end

let add_pos e ty = Mark.add (Expr.pos e) ty

let ty : (_, unionfind_typ A.custom) A.marked -> unionfind_typ =
 fun (_, A.Custom { A.custom; _ }) -> custom

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up :
    type a m.
    A.decl_ctx ->
    (a, m) A.gexpr Env.t ->
    (a, m) A.gexpr ->
    (a, unionfind_typ A.custom) A.boxed_gexpr =
 fun ctx env e ->
  typecheck_expr_top_down ctx env
    (UnionFind.make (add_pos e (TAny (Any.fresh ()))))
    e

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down :
    type a m.
    A.decl_ctx ->
    (a, m) A.gexpr Env.t ->
    unionfind_typ ->
    (a, m) A.gexpr ->
    (a, unionfind_typ A.custom) A.boxed_gexpr =
 fun ctx env tau e ->
  (* Message.debug "Propagating type %a for naked_expr :@.@[<hov 2>%a@]"
     (format_typ ctx) tau Expr.format e; *)
  let pos_e = Expr.pos e in
  let flags = env.flags in
  let () =
    (* If there already is a type annotation on the given expr, ensure it
       matches *)
    match Mark.get e with
    | A.Untyped _ | A.Typed { A.ty = A.TAny, _; _ } -> ()
    | A.Typed { A.ty; _ } -> unify ctx e tau (ast_to_typ ty)
    | A.Custom _ -> assert false
  in
  let context_mark = A.Custom { A.custom = tau; pos = pos_e } in
  let mark_with_tau_and_unify uf =
    (* Unify with the supplied type first, and return the mark *)
    unify ctx e uf tau;
    A.Custom { A.custom = uf; pos = pos_e }
  in
  let unionfind ?(pos = e) t = UnionFind.make (add_pos pos t) in
  let ty_mark ty = mark_with_tau_and_unify (unionfind ty) in
  match Mark.remove e with
  | A.ELocation loc ->
    let ty_opt =
      match loc with
      | DesugaredScopeVar { name; _ } | ScopelangScopeVar { name } ->
        Env.get_scope_var env (Mark.remove name)
      | ToplevelVar { name } -> Env.get_toplevel_var env (Mark.remove name)
    in
    let ty =
      match ty_opt with
      | Some ty -> ty
      | None ->
        Message.error ~pos:pos_e "Reference to %a not found" (Print.expr ()) e
    in
    Expr.elocation loc (mark_with_tau_and_unify (ast_to_typ ty))
  | A.EStruct { name; fields } ->
    let mark = ty_mark (TStruct name) in
    let str_ast = A.StructName.Map.find name ctx.A.ctx_structs in
    let str = A.StructName.Map.find name env.structs in
    let _check_fields : unit =
      let missing_fields, extra_fields =
        A.StructField.Map.fold
          (fun fld x (remaining, extra) ->
            if A.StructField.Map.mem fld remaining then
              A.StructField.Map.remove fld remaining, extra
            else remaining, A.StructField.Map.add fld x extra)
          fields
          (str_ast, A.StructField.Map.empty)
      in
      let errs =
        List.map
          (fun (f, ty) ->
            ( Format.asprintf "Missing field %a" A.StructField.format f,
              Mark.get ty ))
          (A.StructField.Map.bindings missing_fields)
        @ List.map
            (fun (f, ef) ->
              let dup = A.StructField.Map.mem f str in
              ( Format.asprintf "%s field %a"
                  (if dup then "Duplicate" else "Unknown")
                  A.StructField.format f,
                Expr.pos ef ))
            (A.StructField.Map.bindings extra_fields)
      in
      if errs <> [] then
        Message.error ~extra_pos:errs
          "Mismatching field definitions for structure %a" A.StructName.format
          name
    in
    let fields =
      A.StructField.Map.mapi
        (fun f_name f_e ->
          let f_ty = A.StructField.Map.find f_name str in
          typecheck_expr_top_down ctx env f_ty f_e)
        fields
    in
    Expr.estruct ~name ~fields mark
  | A.EDStructAmend { name_opt = _; e; fields } ->
    let e = typecheck_expr_top_down ctx env tau e in
    let name =
      match UnionFind.get (ty e) with
      | TStruct name, _ -> name
      | TAny _, _ -> failwith "Disambiguation failure"
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "This expression has type %a, where a structure was expected"
          (format_typ ctx) (ty e)
    in
    let fields = A.Ident.Map.map (typecheck_expr_bottom_up ctx env) fields in
    (* Note: here we identify the structure name, and type the fields
       individually, but without enforcing any consistency constraint between
       the two. This is fine because this construction only appears in
       Desugared, where it is used for disambiguation. In later passes this is
       rewritten into a struct literal, so no need to anticipate name resolution
       and duplicate the checks here. *)
    Expr.edstructamend ~name_opt:(Some name) ~e ~fields context_mark
  | A.EDStructAccess { e = e_struct; name_opt; field } ->
    let t_struct =
      match name_opt with
      | Some name -> unionfind (TStruct name)
      | None -> unionfind (TAny (Any.fresh ()))
    in
    let e_struct' = typecheck_expr_top_down ctx env t_struct e_struct in
    let name =
      match UnionFind.get (ty e_struct') with
      | TStruct name, _ -> name
      | TAny _, _ ->
        Printf.ksprintf failwith
          "Disambiguation failed before reaching field %s" field
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "This is not a structure, cannot access field %s (found type: %a)"
          field (format_typ ctx) (ty e_struct')
    in
    let str =
      try A.StructName.Map.find name env.structs
      with A.StructName.Map.Not_found _ ->
        Message.error ~pos:pos_e "No structure %a found" A.StructName.format
          name
    in
    let field =
      let candidate_structs =
        try A.Ident.Map.find field ctx.ctx_struct_fields
        with A.Ident.Map.Not_found _ -> (
          match
            A.ScopeName.Map.choose_opt
            @@ A.ScopeName.Map.filter
                 (fun _ { A.out_struct_name; _ } ->
                   A.StructName.equal out_struct_name name)
                 ctx.ctx_scopes
          with
          | Some (scope_out, _) ->
            Message.error
              ~fmt_pos:
                [
                  ( (fun ppf ->
                      Format.fprintf ppf
                        "@{<yellow>%s@} is used here as an output" field),
                    Expr.mark_pos context_mark );
                  ( (fun ppf ->
                      Format.fprintf ppf "Scope %a is declared here"
                        A.ScopeName.format scope_out),
                    Mark.get (A.StructName.get_info name) );
                ]
              "Variable @{<yellow>%s@} is not a declared output of scope %a."
              field A.ScopeName.format scope_out
              ~suggestion:
                (Suggestions.sorted_candidates
                   (List.map A.StructField.to_string
                      (A.StructField.Map.keys str))
                   field)
          | None ->
            Message.error
              ~extra_pos:
                [
                  "", Expr.mark_pos context_mark;
                  "Structure definition", Mark.get (A.StructName.get_info name);
                ]
              "Field@ @{<yellow>\"%s\"@}@ does@ not@ belong@ to@ structure@ \
               @{<yellow>\"%a\"@}."
              field A.StructName.format name
              ~suggestion:
                (Suggestions.sorted_candidates
                   (A.Ident.Map.keys ctx.ctx_struct_fields)
                   field))
      in
      try A.StructName.Map.find name candidate_structs
      with A.StructName.Map.Not_found _ ->
        Message.error
          ~pos:(Expr.mark_pos context_mark)
          "Field@ @{<yellow>\"%s\"@}@ does@ not@ belong@ to@ structure@ \
           @{<yellow>\"%a\"@}@ (however, structure@ %a@ defines@ it)@]"
          field A.StructName.format name
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ or@ ")
             (fun fmt s_name ->
               Format.fprintf fmt "@{<yellow>\"%a\"@}" A.StructName.format
                 s_name))
          (A.StructName.Map.keys candidate_structs)
    in
    let fld_ty = A.StructField.Map.find field str in
    let mark = mark_with_tau_and_unify fld_ty in
    Expr.estructaccess ~name ~e:e_struct' ~field mark
  | A.EStructAccess { e = e_struct; name; field } ->
    let fld_ty =
      let str =
        try A.StructName.Map.find name env.structs
        with A.StructName.Map.Not_found _ ->
          Message.error ~pos:pos_e "No structure %a found" A.StructName.format
            name
      in
      try A.StructField.Map.find field str
      with A.StructField.Map.Not_found _ ->
        Message.error ~pos:pos_e
          ~fmt_pos:
            [
              ( (fun ppf ->
                  Format.fprintf ppf "Structure %a declared here"
                    A.StructName.format name),
                Mark.get (A.StructName.get_info name) );
            ]
          "Structure %a doesn't define a field %a" A.StructName.format name
          A.StructField.format field
    in
    let mark = mark_with_tau_and_unify fld_ty in
    let e_struct' =
      typecheck_expr_top_down ctx env (unionfind (TStruct name)) e_struct
    in
    Expr.estructaccess ~e:e_struct' ~field ~name mark
  | A.EInj { name; cons; e = e_enum }
    when Definitions.EnumName.equal name Expr.option_enum ->
    if Definitions.EnumConstructor.equal cons Expr.some_constr then
      let cell_type = unionfind (TAny (Any.fresh ())) in
      let mark = mark_with_tau_and_unify (unionfind (TOption cell_type)) in
      let e_enum' = typecheck_expr_top_down ctx env cell_type e_enum in
      Expr.einj ~name ~cons ~e:e_enum' mark
    else
      (* None constructor *)
      let cell_type = unionfind (TAny (Any.fresh ())) in
      let mark = mark_with_tau_and_unify (unionfind (TOption cell_type)) in
      let e_enum' =
        typecheck_expr_top_down ctx env (unionfind (TLit TUnit)) e_enum
      in
      Expr.einj ~name ~cons ~e:e_enum' mark
  | A.EInj { name; cons; e = e_enum } ->
    let mark = mark_with_tau_and_unify (unionfind (TEnum name)) in
    let e_enum' =
      typecheck_expr_top_down ctx env
        (A.EnumConstructor.Map.find cons (A.EnumName.Map.find name env.enums))
        e_enum
    in
    Expr.einj ~e:e_enum' ~cons ~name mark
  | A.EMatch { e = e1; name; cases }
    when Definitions.EnumName.equal name Expr.option_enum ->
    let cell_type = unionfind ~pos:e1 (TAny (Any.fresh ())) in
    let t_arg = unionfind ~pos:e1 (TOption cell_type) in
    let cases_ty =
      ListLabels.fold_right2
        [Expr.none_constr; Expr.some_constr]
        [unionfind ~pos:e1 (TLit TUnit); cell_type]
        ~f:A.EnumConstructor.Map.add ~init:A.EnumConstructor.Map.empty
    in
    let t_ret = unionfind ~pos:e (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env t_arg e1 in
    let cases =
      A.EnumConstructor.Map.merge
        (fun _ e e_ty ->
          match e, e_ty with
          | Some e, Some e_ty ->
            Some
              (typecheck_expr_top_down ctx env
                 (unionfind ~pos:e (TArrow ([e_ty], t_ret)))
                 e)
          | _ -> assert false)
        cases cases_ty
    in
    Expr.ematch ~e:e1' ~name ~cases mark
  | A.EMatch { e = e1; name; cases } ->
    let cases_ty = A.EnumName.Map.find name ctx.A.ctx_enums in
    let t_ret = unionfind ~pos:e1 (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env (unionfind (TEnum name)) e1 in
    let cases =
      A.EnumConstructor.Map.mapi
        (fun c_name e ->
          let c_ty = A.EnumConstructor.Map.find c_name cases_ty in
          (* For now our constructors are limited to zero or one argument. If
             there is a change to allow for multiple arguments, it might be
             easier to use tuples directly. *)
          let e_ty = unionfind ~pos:e (TArrow ([ast_to_typ c_ty], t_ret)) in
          typecheck_expr_top_down ctx env e_ty e)
        cases
    in
    Expr.ematch ~e:e1' ~name ~cases mark
  | A.EScopeCall { scope; args } ->
    let scope_out_struct =
      (A.ScopeName.Map.find scope ctx.ctx_scopes).out_struct_name
    in
    let mark = mark_with_tau_and_unify (unionfind (TStruct scope_out_struct)) in
    let vars = A.ScopeName.Map.find scope env.scopes_input in
    let args' =
      A.ScopeVar.Map.mapi
        (fun name ->
          typecheck_expr_top_down ctx env
            (ast_to_typ (A.ScopeVar.Map.find name vars)))
        args
    in
    Expr.escopecall ~scope ~args:args' mark
  | A.EVar v ->
    let tau' =
      match Env.get env v with
      | Some t -> t
      | None ->
        Message.error ~pos:pos_e "Variable %s not found in the current context"
          (Bindlib.name_of v)
    in
    Expr.evar (Var.translate v) (mark_with_tau_and_unify tau')
  | A.EExternal { name } ->
    let ty =
      let not_found pr x =
        Message.error ~pos:pos_e
          "Could not resolve the reference to %a.@ Make sure the corresponding \
           module was properly loaded?"
          pr x
      in
      match Mark.remove name with
      | A.External_value name -> (
        try ast_to_typ (A.TopdefName.Map.find name ctx.ctx_topdefs)
        with A.TopdefName.Map.Not_found _ ->
          not_found A.TopdefName.format name)
      | A.External_scope name -> (
        try
          let scope_info = A.ScopeName.Map.find name ctx.ctx_scopes in
          ast_to_typ
            ( TArrow
                ( [TStruct scope_info.in_struct_name, pos_e],
                  (TStruct scope_info.out_struct_name, pos_e) ),
              pos_e )
        with A.ScopeName.Map.Not_found _ -> not_found A.ScopeName.format name)
    in
    Expr.eexternal ~name (mark_with_tau_and_unify ty)
  | A.ELit lit -> Expr.elit lit (ty_mark (lit_type lit))
  | A.ETuple es ->
    let tys = List.map (fun _ -> unionfind (TAny (Any.fresh ()))) es in
    let mark = mark_with_tau_and_unify (unionfind (TTuple tys)) in
    let es' = List.map2 (typecheck_expr_top_down ctx env) tys es in
    Expr.etuple es' mark
  | A.ETupleAccess { e = e1; index; size } ->
    let out_of_bounds size =
      Message.error ~pos:pos_e "Tuple access out of bounds (%d/%d)" index size
    in
    let tuple_ty =
      if size = 0 then (* Unset yet, we resolve it now *)
        TAny (Any.fresh ())
      else if index >= size then out_of_bounds size
      else
        TTuple
          (List.init size (fun n ->
               if n = index then tau
               else unionfind ~pos:e1 (TAny (Any.fresh ()))))
    in
    let tuple_ty = unionfind ~pos:e1 tuple_ty in
    let e1' = typecheck_expr_top_down ctx env tuple_ty e1 in
    let size, mark =
      if size <> 0 then size, context_mark
      else
        match typ_to_ast ~flags tuple_ty with
        | TTuple l, _ -> (
          match List.nth_opt l index with
          | None -> out_of_bounds (List.length l)
          | Some ty -> List.length l, mark_with_tau_and_unify (ast_to_typ ty))
        | TAny, _ -> failwith "Disambiguation failure"
        | ty ->
          Message.error ~pos:(Expr.pos e1)
            "This expression has type@ %a,@ while a tuple was expected"
            (Print.typ ctx) ty
    in
    Expr.etupleaccess ~e:e1' ~index ~size mark
  | A.EAbs { binder; tys = t_args } ->
    if Bindlib.mbinder_arity binder <> List.length t_args then
      Message.error ~pos:(Expr.pos e)
        "function has %d variables but was supplied %d types\n%a"
        (Bindlib.mbinder_arity binder)
        (List.length t_args) Expr.format e
    else
      let tau_args = List.map ast_to_typ t_args in
      let t_ret = unionfind (TAny (Any.fresh ())) in
      let t_func = unionfind (TArrow (tau_args, t_ret)) in
      let mark = mark_with_tau_and_unify t_func in
      let xs, body = Bindlib.unmbind binder in
      let xs' = Array.map Var.translate xs in
      let env =
        List.fold_left2
          (fun env x tau_arg -> Env.add x tau_arg env)
          env (Array.to_list xs) tau_args
      in
      let body' = typecheck_expr_top_down ctx env t_ret body in
      let binder' = Bindlib.bind_mvar xs' (Expr.Box.lift body') in
      Expr.eabs binder' (List.map (typ_to_ast ~flags) tau_args) mark
  | A.EApp { f = e1; args; tys } ->
    (* Here we type the arguments first (in order), to ensure we know the types
       of the arguments if [f] is [EAbs] before disambiguation. This is also the
       right order for the [let-in] form. *)
    let t_args =
      match tys with
      | [] -> List.map (fun _ -> unionfind (TAny (Any.fresh ()))) args
      | tys -> List.map ast_to_typ tys
    in
    let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
    let t_args =
      match t_args, tys with
      | [t], [] -> (
        (* Handles typing before detuplification: if [tys] was not yet set, we
           are allowed to destruct a tuple into multiple arguments (see
           [Scopelang.from_desugared] for the corresponding code
           transformation) *)
        match UnionFind.get t with TTuple tys, _ -> tys | _ -> t_args)
      | _ ->
        if List.length t_args <> List.length args' then
          Message.error ~pos:(Expr.pos e)
            (match e1 with
            | EAbs _, _ -> "This binds %d variables, but %d were provided."
            | _ -> "This function application has %d arguments, but expects %d.")
            (List.length t_args) (List.length args');

        t_args
    in
    let t_func = unionfind ~pos:e1 (TArrow (t_args, tau)) in
    let e1' = typecheck_expr_top_down ctx env t_func e1 in
    Expr.eapp ~f:e1' ~args:args'
      ~tys:(List.map (typ_to_ast ~flags) t_args)
      context_mark
  | A.EAppOp { op; tys; args } ->
    let t_args = List.map ast_to_typ tys in
    let t_func = unionfind (TArrow (t_args, tau)) in
    let args =
      Operator.kind_dispatch (Mark.set pos_e op)
        ~polymorphic:(fun op ->
          if env.flags.assume_op_types then (
            unify ctx e (polymorphic_op_return_type ctx e op t_args) tau;
            List.rev_map (typecheck_expr_bottom_up ctx env) (List.rev args))
          else (
            (* Type the operator first, then right-to-left: polymorphic
               operators are required to allow the resolution of all type
               variables this way *)
            unify ctx e (polymorphic_op_type op) t_func;
            List.rev_map2
              (typecheck_expr_top_down ctx env)
              (List.rev t_args) (List.rev args)))
        ~overloaded:(fun op ->
          (* Typing the arguments first is required to resolve the operator *)
          let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
          unify ctx e tau (resolve_overload_ret_type ~flags ctx e op t_args);
          args')
        ~monomorphic:(fun op ->
          (* Here it doesn't matter but may affect the error messages *)
          unify ctx e (ast_to_typ (Operator.monomorphic_type op)) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
        ~resolved:(fun op ->
          (* This case should not fail *)
          unify ctx e (ast_to_typ (Operator.resolved_type op)) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
    in
    (* All operator applications are monomorphised at this point *)
    let tys = List.map (typ_to_ast ~flags) t_args in
    Expr.eappop ~op ~args ~tys context_mark
  | A.EDefault { excepts; just; cons } ->
    let cons' = typecheck_expr_top_down ctx env tau cons in
    let just' =
      typecheck_expr_top_down ctx env (unionfind ~pos:just (TLit TBool)) just
    in
    let excepts' = List.map (typecheck_expr_top_down ctx env tau) excepts in
    Expr.edefault ~excepts:excepts' ~just:just' ~cons:cons' context_mark
  | A.EPureDefault e1 ->
    let inner_ty = unionfind ~pos:e1 (TAny (Any.fresh ())) in
    let mark =
      mark_with_tau_and_unify (unionfind ~pos:e1 (TDefault inner_ty))
    in
    let e1' = typecheck_expr_top_down ctx env inner_ty e1 in
    Expr.epuredefault e1' mark
  | A.EIfThenElse { cond; etrue = et; efalse = ef } ->
    let et' = typecheck_expr_top_down ctx env tau et in
    let ef' = typecheck_expr_top_down ctx env tau ef in
    let cond' =
      typecheck_expr_top_down ctx env (unionfind ~pos:cond (TLit TBool)) cond
    in
    Expr.eifthenelse cond' et' ef' context_mark
  | A.EAssert e1 ->
    let mark = mark_with_tau_and_unify (unionfind (TLit TUnit)) in
    let e1' =
      typecheck_expr_top_down ctx env (unionfind ~pos:e1 (TLit TBool)) e1
    in
    Expr.eassert e1' mark
  | A.EFatalError err -> Expr.efatalerror err context_mark
  | A.EEmpty ->
    Expr.eempty (ty_mark (TDefault (unionfind (TAny (Any.fresh ())))))
  | A.EErrorOnEmpty e1 ->
    let tau' = unionfind (TDefault tau) in
    let e1' = typecheck_expr_top_down ctx env tau' e1 in
    Expr.eerroronempty e1' context_mark
  | A.EArray es ->
    let cell_type = unionfind (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify (unionfind (TArray cell_type)) in
    let es' = List.map (typecheck_expr_top_down ctx env cell_type) es in
    Expr.earray es' mark
  | A.ECustom { obj; targs; tret } ->
    let mark =
      mark_with_tau_and_unify (ast_to_typ (A.TArrow (targs, tret), Expr.pos e))
    in
    Expr.ecustom obj targs tret mark

(** {1 API} *)

let get_ty_mark ~flags (A.Custom { A.custom = uf; pos }) =
  A.Typed { ty = typ_to_ast ~flags uf; pos }

let expr_raw
    (type a)
    (ctx : A.decl_ctx)
    ?(env = Env.empty ctx)
    ?(typ : A.typ option)
    (e : (a, 'm) A.gexpr) : (a, unionfind_typ A.custom) A.gexpr =
  let fty =
    match typ with
    | None -> typecheck_expr_bottom_up ctx env
    | Some typ -> typecheck_expr_top_down ctx env (ast_to_typ typ)
  in
  Expr.unbox (fty e)

let check_expr ctx ?env ?typ e =
  Expr.map_marks
    ~f:(fun (Custom { pos; _ }) -> A.Untyped { pos })
    (expr_raw ctx ?env ?typ e)

(* Infer the type of an expression *)
let expr ctx ?(env = Env.empty ctx) ?typ e =
  Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) (expr_raw ctx ~env ?typ e)

let scope_body_expr ctx env ty_out body_expr =
  let _env, ret =
    BoundList.fold_map body_expr ~init:env
      ~last:(fun env e ->
        let e' = Expr.unbox (typecheck_expr_top_down ctx env ty_out e) in
        let e' = Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) e' in
        env, Expr.Box.lift e')
      ~f:(fun env var scope ->
        let e0 = scope.A.scope_let_expr in
        let ty_e = ast_to_typ scope.A.scope_let_typ in
        let e = Expr.unbox (typecheck_expr_bottom_up ctx env e0) in
        unify ctx e0 (ty e) ty_e;
        (* We could use [typecheck_expr_top_down] rather than this manual
           unification, but we get better messages with this order of the
           [unify] parameters, which keeps location of the type as defined
           instead of as inferred. *)
        ( Env.add var ty_e env,
          Var.translate var,
          Bindlib.box_apply
            (fun scope_let_expr ->
              {
                scope with
                A.scope_let_typ =
                  (match scope.A.scope_let_typ with
                  | TAny, _ -> typ_to_ast ~flags:env.flags (ty e)
                  | ty -> ty);
                A.scope_let_expr;
              })
            (Expr.Box.lift (Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) e))
        ))
  in
  ret

let scope_body ctx env body =
  let get_pos struct_name = Mark.get (A.StructName.get_info struct_name) in
  let struct_ty struct_name =
    UnionFind.make (Mark.add (get_pos struct_name) (TStruct struct_name))
  in
  let ty_in = struct_ty body.A.scope_body_input_struct in
  let ty_out = struct_ty body.A.scope_body_output_struct in
  let var, e = Bindlib.unbind body.A.scope_body_expr in
  let env = Env.add var ty_in env in
  let e' = scope_body_expr ctx env ty_out e in
  ( Bindlib.box_apply
      (fun scope_body_expr -> { body with scope_body_expr })
      (Bindlib.bind_var (Var.translate var) e'),
    UnionFind.make
      (Mark.add
         (get_pos body.A.scope_body_output_struct)
         (TArrow ([ty_in], ty_out))) )

let scopes ctx env =
  BoundList.fold_map ~init:env
    ~last:(fun ctx () -> ctx, Bindlib.box ())
    ~f:(fun env var item ->
      match item with
      | A.ScopeDef (name, body) ->
        let body_e, ty_scope = scope_body ctx env body in
        ( Env.add var ty_scope env,
          Var.translate var,
          Bindlib.box_apply (fun body -> A.ScopeDef (name, body)) body_e )
      | A.Topdef (name, typ, e) ->
        let e' = expr_raw ctx ~env ~typ e in
        let (A.Custom { custom = uf; _ }) = Mark.get e' in
        let e' = Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) e' in
        ( Env.add var uf env,
          Var.translate var,
          Bindlib.box_apply
            (fun e -> A.Topdef (name, Expr.ty e', e))
            (Expr.Box.lift e') ))

let program ?fail_on_any ?assume_op_types prg =
  let env = Env.empty ?fail_on_any ?assume_op_types prg.A.decl_ctx in
  let new_env, code_items = scopes prg.A.decl_ctx env prg.A.code_items in
  {
    A.lang = prg.lang;
    A.module_name = prg.A.module_name;
    A.code_items = Bindlib.unbox code_items;
    decl_ctx =
      {
        prg.decl_ctx with
        ctx_structs =
          A.StructName.Map.mapi
            (fun s_name fields ->
              A.StructField.Map.mapi
                (fun f_name (t : A.typ) ->
                  match Mark.remove t with
                  | TAny ->
                    typ_to_ast ~flags:env.flags
                      (A.StructField.Map.find f_name
                         (A.StructName.Map.find s_name new_env.structs))
                  | _ -> t)
                fields)
            prg.decl_ctx.ctx_structs;
        ctx_enums =
          A.EnumName.Map.mapi
            (fun e_name cons ->
              A.EnumConstructor.Map.mapi
                (fun cons_name (t : A.typ) ->
                  match Mark.remove t with
                  | TAny ->
                    typ_to_ast ~flags:env.flags
                      (A.EnumConstructor.Map.find cons_name
                         (A.EnumName.Map.find e_name new_env.enums))
                  | _ -> t)
                cons)
            prg.decl_ctx.ctx_enums;
      };
  }

let program ?fail_on_any ?assume_op_types ?(internal_check = false) prg =
  let wrap =
    if internal_check then (fun f ->
      try Message.with_delayed_errors f
      with (Message.CompilerError _ | Message.CompilerErrors _) as exc ->
        let bt = Printexc.get_raw_backtrace () in
        let err =
          match exc with
          | Message.CompilerError err ->
            Message.CompilerError (Message.Content.to_internal_error err)
          | Message.CompilerErrors errs ->
            Message.CompilerErrors
              (List.map Message.Content.to_internal_error errs)
          | _ -> assert false
        in
        Message.debug "Faulty intermediate program:@ %a"
          (Print.program ~debug:true)
          prg;
        Printexc.raise_with_backtrace err bt)
    else fun f -> Message.with_delayed_errors f
  in
  wrap @@ fun () -> program ?fail_on_any ?assume_op_types prg
