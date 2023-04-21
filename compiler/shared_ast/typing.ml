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

module Any =
  Uid.Make
    (struct
      type info = unit

      let to_string _ = "any"
      let format fmt () = Format.fprintf fmt "any"
      let equal _ _ = true
      let compare _ _ = 0
    end)
    ()

type unionfind_typ = naked_typ Marked.pos UnionFind.elem
(** We do not reuse {!type: Shared_ast.typ} because we have to include a new
    [TAny] variant. Indeed, error terms can have any type and this has to be
    captured by the type sytem. *)

and naked_typ =
  | TLit of A.typ_lit
  | TArrow of unionfind_typ list * unionfind_typ
  | TTuple of unionfind_typ list
  | TStruct of A.StructName.t
  | TEnum of A.EnumName.t
  | TOption of unionfind_typ
  | TArray of unionfind_typ
  | TAny of Any.t

let rec typ_to_ast ~leave_unresolved (ty : unionfind_typ) : A.typ =
  let typ_to_ast = typ_to_ast ~leave_unresolved in
  let ty, pos = UnionFind.get (UnionFind.find ty) in
  match ty with
  | TLit l -> A.TLit l, pos
  | TTuple ts -> A.TTuple (List.map typ_to_ast ts), pos
  | TStruct s -> A.TStruct s, pos
  | TEnum e -> A.TEnum e, pos
  | TOption t -> A.TOption (typ_to_ast t), pos
  | TArrow (t1, t2) -> A.TArrow (List.map typ_to_ast t1, typ_to_ast t2), pos
  | TArray t1 -> A.TArray (typ_to_ast t1), pos
  | TAny _ ->
    if leave_unresolved then A.TAny, pos
    else
      (* No polymorphism in Catala: type inference should return full types
         without wildcards, and this function is used to recover the types after
         typing. *)
      Errors.raise_spanned_error pos
        "Internal error: typing at this point could not be resolved"

let rec ast_to_typ (ty : A.typ) : unionfind_typ =
  let ty' =
    match Marked.unmark ty with
    | A.TLit l -> TLit l
    | A.TArrow (t1, t2) -> TArrow (List.map ast_to_typ t1, ast_to_typ t2)
    | A.TTuple ts -> TTuple (List.map ast_to_typ ts)
    | A.TStruct s -> TStruct s
    | A.TEnum e -> TEnum e
    | A.TOption t -> TOption (ast_to_typ t)
    | A.TArray t -> TArray (ast_to_typ t)
    | A.TAny -> TAny (Any.fresh ())
  in
  UnionFind.make (Marked.same_mark_as ty' ty)

(** {1 Types and unification} *)

let typ_needs_parens (t : unionfind_typ) : bool =
  let t = UnionFind.get (UnionFind.find t) in
  match Marked.unmark t with
  | TArrow _ | TArray _ | TOption _ -> true
  | _ -> false

let rec format_typ
    (ctx : A.decl_ctx)
    (fmt : Format.formatter)
    (naked_typ : unionfind_typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : unionfind_typ) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  let naked_typ = UnionFind.get (UnionFind.find naked_typ) in
  match Marked.unmark naked_typ with
  | TLit l -> Format.fprintf fmt "%a" Print.tlit l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         (fun fmt t -> Format.fprintf fmt "%a" format_typ t))
      ts
  | TStruct s -> Format.fprintf fmt "%a" A.StructName.format_t s
  | TEnum e -> Format.fprintf fmt "%a" A.EnumName.format_t e
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>(%a)@ %s@]" format_typ_with_parens t "eoption"
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ →@ %a@]" format_typ_with_parens t1
      format_typ t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>(%a)@ →@ %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_typ_with_parens)
      t1 format_typ t2
  | TArray t1 -> (
    match Marked.unmark (UnionFind.get (UnionFind.find t1)) with
    | TAny _ when not !Cli.debug_flag -> Format.pp_print_string fmt "collection"
    | _ -> Format.fprintf fmt "@[collection@ %a@]" format_typ t1)
  | TAny v ->
    if !Cli.debug_flag then Format.fprintf fmt "<a%d>" (Any.hash v)
    else Format.pp_print_string fmt "<any>"

exception Type_error of A.any_expr * unionfind_typ * unionfind_typ

type mark = { pos : Pos.t; uf : unionfind_typ }

(** Raises an error if unification cannot be performed. The position annotation
    of the second [unionfind_typ] argument is propagated (unless it is [TAny]). *)
let rec unify
    (ctx : A.decl_ctx)
    (e : ('a, 'm A.mark) A.gexpr) (* used for error context *)
    (t1 : unionfind_typ)
    (t2 : unionfind_typ) : unit =
  let unify = unify ctx in
  (* Cli.debug_format "Unifying %a and %a" (format_typ ctx) t1 (format_typ ctx)
     t2; *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let raise_type_error () = raise (Type_error (A.AnyExpr e, t1, t2)) in
  let () =
    match Marked.unmark t1_repr, Marked.unmark t2_repr with
    | TLit tl1, TLit tl2 -> if tl1 <> tl2 then raise_type_error ()
    | TArrow (t11, t12), TArrow (t21, t22) -> (
      unify e t12 t22;
      try List.iter2 (unify e) t11 t21
      with Invalid_argument _ -> raise_type_error ())
    | TTuple ts1, TTuple ts2 -> (
      try List.iter2 (unify e) ts1 ts2
      with Invalid_argument _ -> raise_type_error ())
    | TStruct s1, TStruct s2 ->
      if not (A.StructName.equal s1 s2) then raise_type_error ()
    | TEnum e1, TEnum e2 ->
      if not (A.EnumName.equal e1 e2) then raise_type_error ()
    | TOption t1, TOption t2 -> unify e t1 t2
    | TArray t1', TArray t2' -> unify e t1' t2'
    | TAny _, _ | _, TAny _ -> ()
    | ( ( TLit _ | TArrow _ | TTuple _ | TStruct _ | TEnum _ | TOption _
        | TArray _ ),
        _ ) ->
      raise_type_error ()
  in
  ignore
  @@ UnionFind.merge
       (fun t1 t2 -> match Marked.unmark t2 with TAny _ -> t1 | _ -> t2)
       t1 t2

let handle_type_error ctx e t1 t2 =
  (* TODO: if we get weird error messages, then it means that we should use the
     persistent version of the union-find data structure. *)
  let pos =
    match e with
    | A.AnyExpr e -> (
      match Marked.get_mark e with Untyped { pos } | Typed { pos; _ } -> pos)
  in
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let t1_pos = Marked.get_mark t1_repr in
  let t2_pos = Marked.get_mark t2_repr in
  let unformat_typ typ =
    let buf = Buffer.create 59 in
    let ppf = Format.formatter_of_buffer buf in
    (* set infinite width to disable line cuts *)
    Format.pp_set_margin ppf max_int;
    format_typ ctx ppf typ;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let t1_s fmt () =
    Cli.format_with_style [ANSITerminal.yellow] fmt (unformat_typ t1)
  in
  let t2_s fmt () =
    Cli.format_with_style [ANSITerminal.yellow] fmt (unformat_typ t2)
  in
  Errors.raise_multispanned_error
    [
      ( Some
          (Format.asprintf
             "Error coming from typechecking the following expression:"),
        pos );
      Some (Format.asprintf "Type %a coming from expression:" t1_s ()), t1_pos;
      Some (Format.asprintf "Type %a coming from expression:" t2_s ()), t2_pos;
    ]
    "Error during typechecking, incompatible types:\n%a %a\n%a %a"
    (Cli.format_with_style [ANSITerminal.blue; ANSITerminal.Bold])
    "-->" t1_s ()
    (Cli.format_with_style [ANSITerminal.blue; ANSITerminal.Bold])
    "-->" t2_s ()

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

let polymorphic_op_type (op : Operator.polymorphic A.operator Marked.pos) :
    unionfind_typ =
  let open Operator in
  let pos = Marked.get_mark op in
  let any = lazy (UnionFind.make (TAny (Any.fresh ()), pos)) in
  let any2 = lazy (UnionFind.make (TAny (Any.fresh ()), pos)) in
  let bt = lazy (UnionFind.make (TLit TBool, pos)) in
  let ut = lazy (UnionFind.make (TLit TUnit, pos)) in
  let it = lazy (UnionFind.make (TLit TInt, pos)) in
  let array a = lazy (UnionFind.make (TArray (Lazy.force a), pos)) in
  let option a = lazy (UnionFind.make (TOption (Lazy.force a), pos)) in
  let ( @-> ) x y =
    lazy (UnionFind.make (TArrow (List.map Lazy.force x, Lazy.force y), pos))
  in
  let ty =
    match Marked.unmark op with
    | Fold -> [[any2; any] @-> any2; any2; array any] @-> any2
    | Eq -> [any; any] @-> bt
    | Map -> [[any] @-> any2; array any] @-> array any2
    | Filter -> [[any] @-> bt; array any] @-> array any
    | Reduce -> [[any; any] @-> any; any; array any] @-> any
    | Concat -> [array any; array any] @-> array any
    | Log (PosRecordIfTrueBool, _) -> [bt] @-> bt
    | Log _ -> [any] @-> any
    | Length -> [array any] @-> it
    | HandleDefault -> [array ([ut] @-> any); [ut] @-> bt; [ut] @-> any] @-> any
    | HandleDefaultOpt ->
      [array (option any); [ut] @-> option bt; [ut] @-> option any]
      @-> option any
  in
  Lazy.force ty

let resolve_overload_ret_type
    ~leave_unresolved
    (ctx : A.decl_ctx)
    e
    (op : Operator.overloaded A.operator)
    tys : unionfind_typ =
  let op_ty =
    Operator.overload_type ctx
      (Marked.mark (Expr.pos e) op)
      (List.map (typ_to_ast ~leave_unresolved) tys)
    (* We use [unsafe] because the error is caught below *)
  in
  ast_to_typ (Type.arrow_return op_ty)

(** {1 Double-directed typing} *)

module Env = struct
  type 'e t = {
    structs : unionfind_typ A.StructField.Map.t A.StructName.Map.t;
    enums : unionfind_typ A.EnumConstructor.Map.t A.EnumName.Map.t;
    vars : ('e, unionfind_typ) Var.Map.t;
    scope_vars : A.typ A.ScopeVar.Map.t;
    scopes : A.typ A.ScopeVar.Map.t A.ScopeName.Map.t;
    toplevel_vars : A.typ A.TopdefName.Map.t;
  }

  let empty (decl_ctx : A.decl_ctx) =
    (* We fill the environment initially with the structs and enums
       declarations *)
    {
      structs =
        A.StructName.Map.map
          (A.StructField.Map.map ast_to_typ)
          decl_ctx.ctx_structs;
      enums =
        A.EnumName.Map.map
          (A.EnumConstructor.Map.map ast_to_typ)
          decl_ctx.ctx_enums;
      vars = Var.Map.empty;
      scope_vars = A.ScopeVar.Map.empty;
      scopes = A.ScopeName.Map.empty;
      toplevel_vars = A.TopdefName.Map.empty;
    }

  let get t v = Var.Map.find_opt v t.vars
  let get_scope_var t sv = A.ScopeVar.Map.find_opt sv t.scope_vars
  let get_toplevel_var t v = A.TopdefName.Map.find_opt v t.toplevel_vars

  let get_subscope_out_var t scope var =
    Option.bind (A.ScopeName.Map.find_opt scope t.scopes) (fun vmap ->
        A.ScopeVar.Map.find_opt var vmap)

  let add v tau t = { t with vars = Var.Map.add v tau t.vars }
  let add_var v typ t = add v (ast_to_typ typ) t

  let add_scope_var v typ t =
    { t with scope_vars = A.ScopeVar.Map.add v typ t.scope_vars }

  let add_scope scope_name ~vars t =
    { t with scopes = A.ScopeName.Map.add scope_name vars t.scopes }

  let add_toplevel_var v typ t =
    { t with toplevel_vars = A.TopdefName.Map.add v typ t.toplevel_vars }

  let open_scope scope_name t =
    let scope_vars =
      A.ScopeVar.Map.union
        (fun _ _ -> assert false)
        t.scope_vars
        (A.ScopeName.Map.find scope_name t.scopes)
    in
    { t with scope_vars }
end

let add_pos e ty = Marked.mark (Expr.pos e) ty
let ty (_, { uf; _ }) = uf

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up :
    type a m.
    leave_unresolved:bool ->
    A.decl_ctx ->
    (a, m A.mark) A.gexpr Env.t ->
    (a, m A.mark) A.gexpr ->
    (a, mark) A.boxed_gexpr =
 fun ~leave_unresolved ctx env e ->
  typecheck_expr_top_down ~leave_unresolved ctx env
    (UnionFind.make (add_pos e (TAny (Any.fresh ()))))
    e

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down :
    type a m.
    leave_unresolved:bool ->
    A.decl_ctx ->
    (a, m A.mark) A.gexpr Env.t ->
    unionfind_typ ->
    (a, m A.mark) A.gexpr ->
    (a, mark) A.boxed_gexpr =
 fun ~leave_unresolved ctx env tau e ->
  (* Cli.debug_format "Propagating type %a for naked_expr %a" (format_typ ctx)
     tau (Expr.format ctx) e; *)
  let pos_e = Expr.pos e in
  let () =
    (* If there already is a type annotation on the given expr, ensure it
       matches *)
    match Marked.get_mark e with
    | A.Untyped _ | A.Typed { A.ty = A.TAny, _; _ } -> ()
    | A.Typed { A.ty; _ } -> unify ctx e tau (ast_to_typ ty)
  in
  let context_mark = { uf = tau; pos = pos_e } in
  let mark_with_tau_and_unify uf =
    (* Unify with the supplied type first, and return the mark *)
    unify ctx e uf tau;
    { uf; pos = pos_e }
  in
  let unionfind ?(pos = e) t = UnionFind.make (add_pos pos t) in
  let ty_mark ty = mark_with_tau_and_unify (unionfind ty) in
  match Marked.unmark e with
  | A.ELocation loc ->
    let ty_opt =
      match loc with
      | DesugaredScopeVar (v, _) | ScopelangScopeVar v ->
        Env.get_scope_var env (Marked.unmark v)
      | SubScopeVar (scope, _, v) ->
        Env.get_subscope_out_var env scope (Marked.unmark v)
      | ToplevelVar v -> Env.get_toplevel_var env (Marked.unmark v)
    in
    let ty =
      match ty_opt with
      | Some ty -> ty
      | None ->
        Errors.raise_spanned_error pos_e "Reference to %a not found"
          (Expr.format ctx) e
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
            ( Some (Format.asprintf "Missing field %a" A.StructField.format_t f),
              Marked.get_mark ty ))
          (A.StructField.Map.bindings missing_fields)
        @ List.map
            (fun (f, ef) ->
              let dup = A.StructField.Map.mem f str in
              ( Some
                  (Format.asprintf "%s field %a"
                     (if dup then "Duplicate" else "Unknown")
                     A.StructField.format_t f),
                Expr.pos ef ))
            (A.StructField.Map.bindings extra_fields)
      in
      if errs <> [] then
        Errors.raise_multispanned_error errs
          "Mismatching field definitions for structure %a" A.StructName.format_t
          name
    in
    let fields' =
      A.StructField.Map.mapi
        (fun f_name f_e ->
          let f_ty = A.StructField.Map.find f_name str in
          typecheck_expr_top_down ~leave_unresolved ctx env f_ty f_e)
        fields
    in
    Expr.estruct name fields' mark
  | A.EDStructAccess { e = e_struct; name_opt; field } ->
    let t_struct =
      match name_opt with
      | Some name -> TStruct name
      | None -> TAny (Any.fresh ())
    in
    let e_struct' =
      typecheck_expr_top_down ~leave_unresolved ctx env (unionfind t_struct)
        e_struct
    in
    let name =
      match UnionFind.get (ty e_struct') with
      | TStruct name, _ -> name
      | TAny _, _ ->
        Printf.ksprintf failwith
          "Disambiguation failed before reaching field %s" field
      | _ ->
        Errors.raise_spanned_error (Expr.pos e)
          "This is not a structure, cannot access field %s (%a)" field
          (format_typ ctx) (ty e_struct')
    in
    let fld_ty =
      let str =
        try A.StructName.Map.find name env.structs
        with Not_found ->
          Errors.raise_spanned_error pos_e "No structure %a found"
            A.StructName.format_t name
      in
      let field =
        let candidate_structs =
          try A.IdentName.Map.find field ctx.ctx_struct_fields
          with Not_found ->
            Errors.raise_spanned_error context_mark.pos
              "Field %a does not belong to structure %a (no structure defines \
               it)"
              (Cli.format_with_style [ANSITerminal.yellow])
              ("\"" ^ field ^ "\"")
              (Cli.format_with_style [ANSITerminal.yellow])
              (Format.asprintf "\"%a\"" A.StructName.format_t name)
        in
        try A.StructName.Map.find name candidate_structs
        with Not_found ->
          Errors.raise_spanned_error context_mark.pos
            "Field %a does not belong to structure %a, but to %a"
            (Cli.format_with_style [ANSITerminal.yellow])
            ("\"" ^ field ^ "\"")
            (Cli.format_with_style [ANSITerminal.yellow])
            (Format.asprintf "\"%a\"" A.StructName.format_t name)
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ or@ ")
               (fun fmt s_name ->
                 Format.fprintf fmt "%a"
                   (Cli.format_with_style [ANSITerminal.yellow])
                   (Format.asprintf "\"%a\"" A.StructName.format_t s_name)))
            (List.map fst (A.StructName.Map.bindings candidate_structs))
      in
      A.StructField.Map.find field str
    in
    let mark = mark_with_tau_and_unify fld_ty in
    Expr.edstructaccess e_struct' field (Some name) mark
  | A.EStructAccess { e = e_struct; name; field } ->
    let fld_ty =
      let str =
        try A.StructName.Map.find name env.structs
        with Not_found ->
          Errors.raise_spanned_error pos_e "No structure %a found"
            A.StructName.format_t name
      in
      try A.StructField.Map.find field str
      with Not_found ->
        Errors.raise_multispanned_error
          [
            None, pos_e;
            ( Some "Structure %a declared here",
              Marked.get_mark (A.StructName.get_info name) );
          ]
          "Structure %a doesn't define a field %a" A.StructName.format_t name
          A.StructField.format_t field
    in
    let mark = mark_with_tau_and_unify fld_ty in
    let e_struct' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (unionfind (TStruct name)) e_struct
    in
    Expr.estructaccess e_struct' field name mark
  | A.EInj { name; cons; e = e_enum }
    when Definitions.EnumName.equal name Definitions.option_enum ->
    if Definitions.EnumConstructor.equal cons Definitions.some_constr then
      let cell_type = unionfind (TAny (Any.fresh ())) in
      let mark = mark_with_tau_and_unify (unionfind (TOption cell_type)) in
      let e_enum' =
        typecheck_expr_top_down ~leave_unresolved ctx env cell_type e_enum
      in
      Expr.einj e_enum' cons name mark
    else
      (* None constructor *)
      let cell_type = unionfind (TAny (Any.fresh ())) in
      let mark = mark_with_tau_and_unify (unionfind (TOption cell_type)) in
      let e_enum' =
        typecheck_expr_top_down ~leave_unresolved ctx env
          (unionfind (TLit TUnit)) e_enum
      in
      Expr.einj e_enum' cons name mark
  | A.EInj { name; cons; e = e_enum } ->
    let mark = mark_with_tau_and_unify (unionfind (TEnum name)) in
    let e_enum' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (A.EnumConstructor.Map.find cons (A.EnumName.Map.find name env.enums))
        e_enum
    in
    Expr.einj e_enum' cons name mark
  | A.EMatch { e = e1; name; cases }
    when Definitions.EnumName.compare name Definitions.option_enum = 0 ->
    let cell_type = unionfind ~pos:e1 (TAny (Any.fresh ())) in
    let t_arg = unionfind ~pos:e1 (TOption cell_type) in
    let cases_ty =
      ListLabels.fold_right2
        [A.none_constr; A.some_constr]
        [unionfind ~pos:e1 (TLit TUnit); cell_type]
        ~f:A.EnumConstructor.Map.add ~init:A.EnumConstructor.Map.empty
    in
    let t_ret = unionfind ~pos:e (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ~leave_unresolved ctx env t_arg e1 in
    let cases' =
      A.EnumConstructor.MapLabels.merge cases cases_ty ~f:(fun _ e e_ty ->
          match e, e_ty with
          | Some e, Some e_ty ->
            Some
              (typecheck_expr_top_down ~leave_unresolved ctx env
                 (unionfind ~pos:e (TArrow ([e_ty], t_ret)))
                 e)
          | _ -> assert false)
    in

    Expr.ematch e1' name cases' mark
  | A.EMatch { e = e1; name; cases } ->
    let cases_ty = A.EnumName.Map.find name ctx.A.ctx_enums in
    let t_ret = unionfind ~pos:e1 (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' =
      typecheck_expr_top_down ~leave_unresolved ctx env (unionfind (TEnum name))
        e1
    in
    let cases' =
      A.EnumConstructor.Map.mapi
        (fun c_name e ->
          let c_ty = A.EnumConstructor.Map.find c_name cases_ty in
          (* For now our constructors are limited to zero or one argument. If
             there is a change to allow for multiple arguments, it might be
             easier to use tuples directly. *)
          let e_ty = unionfind ~pos:e (TArrow ([ast_to_typ c_ty], t_ret)) in
          typecheck_expr_top_down ~leave_unresolved ctx env e_ty e)
        cases
    in
    Expr.ematch e1' name cases' mark
  | A.EScopeCall { scope; args } ->
    let scope_out_struct =
      (A.ScopeName.Map.find scope ctx.ctx_scopes).out_struct_name
    in
    let mark = mark_with_tau_and_unify (unionfind (TStruct scope_out_struct)) in
    let vars = A.ScopeName.Map.find scope env.scopes in
    let args' =
      A.ScopeVar.Map.mapi
        (fun name ->
          typecheck_expr_top_down ~leave_unresolved ctx env
            (ast_to_typ (A.ScopeVar.Map.find name vars)))
        args
    in
    Expr.escopecall scope args' mark
  | A.ERaise ex -> Expr.eraise ex context_mark
  | A.ECatch { body; exn; handler } ->
    let body' = typecheck_expr_top_down ~leave_unresolved ctx env tau body in
    let handler' =
      typecheck_expr_top_down ~leave_unresolved ctx env tau handler
    in
    Expr.ecatch body' exn handler' context_mark
  | A.EVar v ->
    let tau' =
      match Env.get env v with
      | Some t -> t
      | None ->
        Errors.raise_spanned_error pos_e
          "Variable %s not found in the current context" (Bindlib.name_of v)
    in
    Expr.evar (Var.translate v) (mark_with_tau_and_unify tau')
  | A.ELit lit -> Expr.elit lit (ty_mark (lit_type lit))
  | A.ETuple es ->
    let tys = List.map (fun _ -> unionfind (TAny (Any.fresh ()))) es in
    let mark = mark_with_tau_and_unify (unionfind (TTuple tys)) in
    let es' =
      List.map2 (typecheck_expr_top_down ~leave_unresolved ctx env) tys es
    in
    Expr.etuple es' mark
  | A.ETupleAccess { e = e1; index; size } ->
    if index >= size then
      Errors.raise_spanned_error (Expr.pos e)
        "Tuple access out of bounds (%d/%d)" index size;
    let tuple_ty =
      TTuple
        (List.init size (fun n ->
             if n = index then tau else unionfind ~pos:e1 (TAny (Any.fresh ()))))
    in
    let e1' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (unionfind ~pos:e1 tuple_ty)
        e1
    in
    Expr.etupleaccess e1' index size context_mark
  | A.EAbs { binder; tys = t_args } ->
    if Bindlib.mbinder_arity binder <> List.length t_args then
      Errors.raise_spanned_error (Expr.pos e)
        "function has %d variables but was supplied %d types"
        (Bindlib.mbinder_arity binder)
        (List.length t_args)
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
      let body' =
        typecheck_expr_top_down ~leave_unresolved ctx env t_ret body
      in
      let binder' = Bindlib.bind_mvar xs' (Expr.Box.lift body') in
      Expr.eabs binder' (List.map (typ_to_ast ~leave_unresolved) tau_args) mark
  | A.EApp { f = (EOp { op; tys }, _) as e1; args } ->
    let t_args = List.map ast_to_typ tys in
    let t_func = unionfind (TArrow (t_args, tau)) in
    let e1', args' =
      Operator.kind_dispatch op
        ~polymorphic:(fun _ ->
          (* Type the operator first, then right-to-left: polymorphic operators
             are required to allow the resolution of all type variables this
             way *)
          let e1' =
            typecheck_expr_top_down ~leave_unresolved ctx env t_func e1
          in
          let args' =
            List.rev_map2
              (typecheck_expr_top_down ~leave_unresolved ctx env)
              (List.rev t_args) (List.rev args)
          in
          e1', args')
        ~overloaded:(fun _ ->
          (* Typing the arguments first is required to resolve the operator *)
          let args' =
            List.map2
              (typecheck_expr_top_down ~leave_unresolved ctx env)
              t_args args
          in
          let e1' =
            typecheck_expr_top_down ~leave_unresolved ctx env t_func e1
          in
          e1', args')
        ~monomorphic:(fun _ ->
          (* Here it doesn't matter but may affect the error messages *)
          let e1' =
            typecheck_expr_top_down ~leave_unresolved ctx env t_func e1
          in
          let args' =
            List.map2
              (typecheck_expr_top_down ~leave_unresolved ctx env)
              t_args args
          in
          e1', args')
        ~resolved:(fun _ ->
          (* This case should not fail *)
          let e1' =
            typecheck_expr_top_down ~leave_unresolved ctx env t_func e1
          in
          let args' =
            List.map2
              (typecheck_expr_top_down ~leave_unresolved ctx env)
              t_args args
          in
          e1', args')
    in
    Expr.eapp e1' args' context_mark
  | A.EApp { f = e1; args } ->
    (* Here we type the arguments first (in order), to ensure we know the types
       of the arguments if [f] is [EAbs] before disambiguation. This is also the
       right order for the [let-in] form. *)
    let t_args = List.map (fun _ -> unionfind (TAny (Any.fresh ()))) args in
    let t_func = unionfind (TArrow (t_args, tau)) in
    let args' =
      List.map2 (typecheck_expr_top_down ~leave_unresolved ctx env) t_args args
    in
    let e1' = typecheck_expr_top_down ~leave_unresolved ctx env t_func e1 in
    Expr.eapp e1' args' context_mark
  | A.EOp { op; tys } ->
    let tys' = List.map ast_to_typ tys in
    let t_ret = unionfind (TAny (Any.fresh ())) in
    let t_func = unionfind (TArrow (tys', t_ret)) in
    unify ctx e t_func tau;
    let tys, mark =
      Operator.kind_dispatch op
        ~polymorphic:(fun op ->
          ( tys,
            mark_with_tau_and_unify (polymorphic_op_type (Marked.mark pos_e op))
          ))
        ~monomorphic:(fun op ->
          let mark =
            mark_with_tau_and_unify
              (ast_to_typ (Operator.monomorphic_type (Marked.mark pos_e op)))
          in
          List.map (typ_to_ast ~leave_unresolved) tys', mark)
        ~overloaded:(fun op ->
          unify ctx e t_ret
            (resolve_overload_ret_type ~leave_unresolved ctx e op tys');
          ( List.map (typ_to_ast ~leave_unresolved) tys',
            { uf = t_func; pos = pos_e } ))
        ~resolved:(fun op ->
          let mark =
            mark_with_tau_and_unify
              (ast_to_typ (Operator.resolved_type (Marked.mark pos_e op)))
          in
          List.map (typ_to_ast ~leave_unresolved) tys', mark)
    in
    Expr.eop op tys mark
  | A.EDefault { excepts; just; cons } ->
    let cons' = typecheck_expr_top_down ~leave_unresolved ctx env tau cons in
    let just' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (unionfind ~pos:just (TLit TBool))
        just
    in
    let excepts' =
      List.map (typecheck_expr_top_down ~leave_unresolved ctx env tau) excepts
    in
    Expr.edefault excepts' just' cons' context_mark
  | A.EIfThenElse { cond; etrue = et; efalse = ef } ->
    let et' = typecheck_expr_top_down ~leave_unresolved ctx env tau et in
    let ef' = typecheck_expr_top_down ~leave_unresolved ctx env tau ef in
    let cond' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (unionfind ~pos:cond (TLit TBool))
        cond
    in
    Expr.eifthenelse cond' et' ef' context_mark
  | A.EAssert e1 ->
    let mark = mark_with_tau_and_unify (unionfind (TLit TUnit)) in
    let e1' =
      typecheck_expr_top_down ~leave_unresolved ctx env
        (unionfind ~pos:e1 (TLit TBool))
        e1
    in
    Expr.eassert e1' mark
  | A.EEmptyError -> Expr.eemptyerror (ty_mark (TAny (Any.fresh ())))
  | A.EErrorOnEmpty e1 ->
    let e1' = typecheck_expr_top_down ~leave_unresolved ctx env tau e1 in
    Expr.eerroronempty e1' context_mark
  | A.EArray es ->
    let cell_type = unionfind (TAny (Any.fresh ())) in
    let mark = mark_with_tau_and_unify (unionfind (TArray cell_type)) in
    let es' =
      List.map (typecheck_expr_top_down ~leave_unresolved ctx env cell_type) es
    in
    Expr.earray es' mark

let wrap ctx f e =
  try f e
  with Type_error (e, ty1, ty2) -> (
    let bt = Printexc.get_raw_backtrace () in
    try handle_type_error ctx e ty1 ty2
    with e -> Printexc.raise_with_backtrace e bt)

let wrap_expr ctx f e =
  (* We need to unbox here, because the typing may otherwise be stored in
     Bindlib closures and not yet applied, and would escape the `try..with` *)
  wrap ctx (fun e -> Expr.unbox (f e)) e

(** {1 API} *)

let get_ty_mark ~leave_unresolved { uf; pos } =
  A.Typed { ty = typ_to_ast ~leave_unresolved uf; pos }

let expr_raw
    (type a)
    ~(leave_unresolved : bool)
    (ctx : A.decl_ctx)
    ?(env = Env.empty ctx)
    ?(typ : A.typ option)
    (e : (a, 'm) A.gexpr) : (a, mark) A.gexpr =
  let fty =
    match typ with
    | None -> typecheck_expr_bottom_up ~leave_unresolved ctx env
    | Some typ ->
      typecheck_expr_top_down ~leave_unresolved ctx env (ast_to_typ typ)
  in
  wrap_expr ctx fty e

let check_expr ~leave_unresolved ctx ?env ?typ e =
  Expr.map_marks
    ~f:(fun { pos; _ } -> A.Untyped { pos })
    (expr_raw ctx ~leave_unresolved ?env ?typ e)

(* Infer the type of an expression *)
let expr ~leave_unresolved ctx ?env ?typ e =
  Expr.map_marks
    ~f:(get_ty_mark ~leave_unresolved)
    (expr_raw ~leave_unresolved ctx ?env ?typ e)

let rec scope_body_expr ~leave_unresolved ctx env ty_out body_expr =
  match body_expr with
  | A.Result e ->
    let e' =
      wrap_expr ctx (typecheck_expr_top_down ~leave_unresolved ctx env ty_out) e
    in
    let e' = Expr.map_marks ~f:(get_ty_mark ~leave_unresolved) e' in
    Bindlib.box_apply (fun e -> A.Result e) (Expr.Box.lift e')
  | A.ScopeLet
      {
        scope_let_kind;
        scope_let_typ;
        scope_let_expr = e0;
        scope_let_next;
        scope_let_pos;
      } ->
    let ty_e = ast_to_typ scope_let_typ in
    let e =
      wrap_expr ctx (typecheck_expr_bottom_up ~leave_unresolved ctx env) e0
    in
    wrap ctx (fun t -> unify ctx e0 (ty e) t) ty_e;
    (* We could use [typecheck_expr_top_down] rather than this manual
       unification, but we get better messages with this order of the [unify]
       parameters, which keeps location of the type as defined instead of as
       inferred. *)
    let var, next = Bindlib.unbind scope_let_next in
    let env = Env.add var ty_e env in
    let next = scope_body_expr ~leave_unresolved ctx env ty_out next in
    let scope_let_next = Bindlib.bind_var (Var.translate var) next in
    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        A.ScopeLet
          {
            scope_let_kind;
            scope_let_typ =
              (match Marked.unmark scope_let_typ with
              | TAny -> typ_to_ast ~leave_unresolved (ty e)
              | _ -> scope_let_typ);
            scope_let_expr;
            scope_let_next;
            scope_let_pos;
          })
      (Expr.Box.lift (Expr.map_marks ~f:(get_ty_mark ~leave_unresolved) e))
      scope_let_next

let scope_body ~leave_unresolved ctx env body =
  let get_pos struct_name =
    Marked.get_mark (A.StructName.get_info struct_name)
  in
  let struct_ty struct_name =
    UnionFind.make (Marked.mark (get_pos struct_name) (TStruct struct_name))
  in
  let ty_in = struct_ty body.A.scope_body_input_struct in
  let ty_out = struct_ty body.A.scope_body_output_struct in
  let var, e = Bindlib.unbind body.A.scope_body_expr in
  let env = Env.add var ty_in env in
  let e' = scope_body_expr ~leave_unresolved ctx env ty_out e in
  ( Bindlib.box_apply
      (fun scope_body_expr -> { body with scope_body_expr })
      (Bindlib.bind_var (Var.translate var) e'),
    UnionFind.make
      (Marked.mark
         (get_pos body.A.scope_body_output_struct)
         (TArrow ([ty_in], ty_out))) )

let rec scopes ~leave_unresolved ctx env = function
  | A.Nil -> Bindlib.box A.Nil
  | A.Cons (item, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    let env, def =
      match item with
      | A.ScopeDef (name, body) ->
        let body_e, ty_scope = scope_body ~leave_unresolved ctx env body in
        ( Env.add var ty_scope env,
          Bindlib.box_apply (fun body -> A.ScopeDef (name, body)) body_e )
      | A.Topdef (name, typ, e) ->
        let e' = expr_raw ~leave_unresolved ctx ~env ~typ e in
        let uf = (Marked.get_mark e').uf in
        let e' = Expr.map_marks ~f:(get_ty_mark ~leave_unresolved) e' in
        ( Env.add var uf env,
          Bindlib.box_apply
            (fun e -> A.Topdef (name, typ, e))
            (Expr.Box.lift e') )
    in
    let next' = scopes ~leave_unresolved ctx env next in
    let next_bind' = Bindlib.bind_var (Var.translate var) next' in
    Bindlib.box_apply2 (fun item next -> A.Cons (item, next)) def next_bind'

let program ~leave_unresolved prg =
  let code_items =
    Bindlib.unbox
      (scopes ~leave_unresolved prg.A.decl_ctx (Env.empty prg.A.decl_ctx)
         prg.A.code_items)
  in
  { prg with code_items }
