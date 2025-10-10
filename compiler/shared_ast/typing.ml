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
open Definitions

type flags = { assume_op_types : bool }

module Env = struct
  type 'e t = {
    flags : flags;
    structs : typ StructField.Map.t StructName.Map.t;
    enums : typ EnumConstructor.Map.t EnumName.Map.t;
    vars : ('e, typ) Var.Map.t;
    scope_vars : typ ScopeVar.Map.t;
    scopes : typ ScopeVar.Map.t ScopeName.Map.t;
    scopes_input : typ ScopeVar.Map.t ScopeName.Map.t;
    toplevel_vars : typ TopdefName.Map.t;
    tvars : Type.t Type.Var.Hashtbl.t;
    printed_errors : unit Pos.Map.t ref;
  }

  let empty ?(assume_op_types = false) (decl_ctx : decl_ctx) =
    (* We fill the environment initially with the structs and enums
       declarations *)
    {
      flags = { assume_op_types };
      structs = decl_ctx.ctx_structs;
      enums = decl_ctx.ctx_enums;
      vars = Var.Map.empty;
      scope_vars = ScopeVar.Map.empty;
      scopes = ScopeName.Map.empty;
      scopes_input = ScopeName.Map.empty;
      toplevel_vars = TopdefName.Map.empty;
      tvars = Type.Var.Hashtbl.create 47;
      printed_errors = ref Pos.Map.empty;
    }

  let get t v = Var.Map.find_opt v t.vars
  let get_scope_var t sv = ScopeVar.Map.find_opt sv t.scope_vars
  let get_toplevel_var t v = TopdefName.Map.find_opt v t.toplevel_vars
  let add_var v tau t = { t with vars = Var.Map.add v tau t.vars }

  let add_scope_var v typ t =
    { t with scope_vars = ScopeVar.Map.add v typ t.scope_vars }

  let add_scope scope_name ~vars ~in_vars t =
    {
      t with
      scopes = ScopeName.Map.add scope_name vars t.scopes;
      scopes_input = ScopeName.Map.add scope_name in_vars t.scopes_input;
    }

  let add_toplevel_var v typ t =
    { t with toplevel_vars = TopdefName.Map.add v typ t.toplevel_vars }

  let open_scope scope_name t =
    let scope_vars =
      ScopeVar.Map.disjoint_union t.scope_vars
        (ScopeName.Map.find scope_name t.scopes)
    in
    { t with scope_vars }

  let dump ppf env =
    let pp_sep = Format.pp_print_space in
    Format.pp_open_vbox ppf 0;
    Format.fprintf ppf "structs: @[<hov>%a@]@,"
      (StructName.Map.format_keys ~pp_sep)
      env.structs;
    Format.fprintf ppf "enums: @[<hov>%a@]@,"
      (EnumName.Map.format_keys ~pp_sep)
      env.enums;
    Format.fprintf ppf "vars: @[<hov>%a@]@,"
      (Var.Map.format_keys ~pp_sep)
      env.vars;
    Format.fprintf ppf "scopes: @[<hov>%a@]@,"
      (ScopeName.Map.format_keys ~pp_sep)
      env.scopes;
    Format.fprintf ppf "topdefs: @[<hov>%a@]@,"
      (TopdefName.Map.format_keys ~pp_sep)
      env.toplevel_vars;
    Format.pp_close_box ppf ()

  let get_tvar env v = Type.Var.Hashtbl.find_opt env.tvars v
  let set_tvar env v ty = Type.Var.Hashtbl.add env.tvars v ty
end

let unification_error env ~pos ?fmt_pos fmt ty1 ty2 =
  if List.exists (fun p -> Pos.Map.mem p !(env.Env.printed_errors)) pos then ()
  else
    let extra_pos =
      match fmt_pos with
      | Some _ -> None
      | None -> Some (List.map (fun p -> "", p) pos)
    in
    Message.delayed_error () ~kind:Typing ?extra_pos ?fmt_pos
      ("Error during typechecking, incompatible types:@\n\
        @[<v>@{<blue>@<2>%s@} @[<hov>%a@]@,\
        @{<blue>@<2>%s@} @[<hov>%a@]"
      ^^ fmt
      ^^ "@]")
      "─➤" Type.format ty1 "─➤" Type.format ty2;
    env.printed_errors :=
      List.fold_left
        (fun acc p -> Pos.Map.add p () acc)
        !(env.Env.printed_errors) pos

let tvar_witness tvset = Type.Var.Set.max_elt tvset
(* anonymous type variables start with "'", so the above is guaranteed to give
   us a user-supplied name if any (quote is before letters in ASCII) *)

(* See [get_ty]. `eqclass` gathers all aliases of the current type ; `seen` is
   other type variables that contain `ty` and is used to detect recursivity. *)
let rec get_ty_aux ?(onfreevar = fun _ -> ()) env pos eqclass seen :
    typ -> typ Bindlib.box = function
  | (TVar v, vpos) as ty -> (
    match Env.get_tvar env v with
    | None ->
      onfreevar v;
      Type.rebox ty
    | Some ty' ->
      if Type.Var.Set.mem v eqclass then
        Type.rebox (TVar (tvar_witness eqclass), vpos)
      else if Type.Var.Set.mem v seen then (
        unification_error env ~pos:[pos; vpos] "@,A type cannot contain itself."
          ty ty';
        Type.rebox ty)
      else get_ty_aux env pos (Type.Var.Set.add v eqclass) seen ty')
  | ty ->
    Type.map
      (get_ty_aux env pos Type.Var.Set.empty (Type.Var.Set.union seen eqclass))
      ty

(* Main function for resolving a type to its expanded, canonical form: this
   expands all known type variables as much as possible. It relies on the
   hashtable maintained in [env] and is at the core of the unification
   procedure.

   This must always be called before exploring a type, or you may be returned
   intermediate type variables. *)
let get_ty env e ty =
  Bindlib.unbox
    (get_ty_aux env (Expr.pos e) Type.Var.Set.empty Type.Var.Set.empty ty)

(* Like [get_ty], but automatically generalises all free type variables found
   remaining: for typing possibly polymorphic functions.

   Precondition: the type must not contain type variables that could be
   constrained somewhere else in the program, obviously. *)
let get_ty_quantified env pos ty =
  let vars = ref Type.Var.Set.empty in
  let bty =
    get_ty_aux
      ~onfreevar:(fun v -> vars := Type.Var.Set.add v !vars)
      env pos Type.Var.Set.empty Type.Var.Set.empty ty
  in
  if Bindlib.is_closed bty then Bindlib.unbox bty
  else
    let vars = Type.Var.Set.filter (fun v -> Bindlib.occur v bty) !vars in
    if Type.Var.Set.is_empty vars then Bindlib.unbox bty
    else
      let bnd =
        Bindlib.bind_mvar (Type.Var.Set.to_seq vars |> Array.of_seq) bty
      in
      TForAll (Bindlib.unbox bnd), Mark.get ty

(** {1 Types and unification} *)

let record_type_error env (AnyExpr e) t1 t2 =
  (* We convert union-find types to ast ones otherwise error messages would be
     hindered as union-find side-effects wrongly unify both types. The delayed
     pretty-printing would yield messages such as: 'incompatible types (integer,
     integer)' *)
  let t1_repr = get_ty env e t1 in
  let t2_repr = get_ty env e t2 in
  let e_pos = Expr.pos e in
  let t1_pos = Mark.get t1_repr in
  let t2_pos = Mark.get t2_repr in
  let pp_typ = Print.typ in
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
  unification_error env ~pos:[e_pos; t1_pos] ~fmt_pos "" t1_repr t2_repr
(* ~pos is used to avoid duplicated messages. Since [t2_pos] generally is the
   position of the type definition, we choose not to include it there to keep
   more messages *)

(** Raises an error if unification cannot be performed. The position annotation
    of the second [typ] argument is propagated (unless it is [TVar]). *)
let rec union
    (env : 'e Env.t)
    (e : ('a, 'm) gexpr as 'e) (* used for error context *)
    (t1 : typ)
    (t2 : typ) : typ =
  (* Message.debug "Unifying %a and %a" Type.format t1 Type.format t2; *)
  let union = union env e in
  let pos2 = Mark.get t2 in
  let record_type_error () = record_type_error env (AnyExpr e) t1 t2 in
  match Mark.remove t1, Mark.remove t2 with
  | TLit tl1, TLit tl2 ->
    if tl1 <> tl2 then record_type_error ();
    t2
  | TArrow (targs1, tret1), TArrow (targs2, tret2) ->
    let tret = union tret1 tret2 in
    let targs =
      try List.map2 union targs1 targs2
      with Invalid_argument _ ->
        record_type_error ();
        targs2
    in
    TArrow (targs, tret), pos2
  | TTuple ts1, TTuple ts2 ->
    let ts =
      try List.map2 union ts1 ts2
      with Invalid_argument _ ->
        record_type_error ();
        ts2
    in
    TTuple ts, pos2
  | TStruct s1, TStruct s2 ->
    if not (StructName.equal s1 s2) then record_type_error ();
    t2
  | TEnum e1, TEnum e2 ->
    if not (EnumName.equal e1 e2) then record_type_error ();
    t2
  | TOption t1', TOption t2' -> TOption (union t1' t2'), pos2
  | TArray t1', TArray t2' -> TArray (union t1' t2'), pos2
  | TDefault t1', TDefault t2' -> TDefault (union t1' t2'), pos2
  | TForAll t1b, TForAll t2b ->
    let _, t1, t2 = Bindlib.unmbind2 t1b t2b in
    union t1 t2
  | TForAll t1b, _ ->
    let _, t1 = Bindlib.unmbind t1b in
    union t1 t2
  | _, TForAll t2b ->
    let _, t2 = Bindlib.unmbind t2b in
    union t1 t2
  | TVar v1, TVar v2 -> (
    if Bindlib.eq_vars v1 v2 then t2
    else
      match Env.get_tvar env v1, Env.get_tvar env v2 with
      | None, None ->
        Env.set_tvar env v1 t2;
        t2
      | Some (TVar v3, _), Some ((TVar v4, _) as t2) when Type.Var.equal v3 v4
        ->
        t2
      | Some (TVar v3, _), _ when Type.Var.equal v2 v3 -> t2
      | None, Some (TVar v3, _) when Type.Var.equal v1 v3 -> t1
      | Some t1, Some t2 ->
        let t = union t1 t2 in
        Env.set_tvar env v1 t;
        Env.set_tvar env v2 t;
        t
      | Some t1, None ->
        if Type.Var.Set.mem v2 (Type.free_vars t1) then
          Message.error ~internal:true ~pos:(Expr.pos e)
            "Recursive type detected: %a(%a) = %a" Type.Var.format v1
            Type.format t1 Type.format t2
        else (
          Env.set_tvar env v2 t1;
          t1)
      | None, Some t2 ->
        if Type.Var.Set.mem v1 (Type.free_vars t2) then
          Message.error ~internal:true ~pos:(Expr.pos e)
            "Recursive type detected: %a(%a) = %a" Type.Var.format v2
            Type.format t2 Type.format t1
        else (
          Env.set_tvar env v1 t2;
          t2))
  | TVar v1, _ ->
    let t =
      match Env.get_tvar env v1 with
      | None -> t2
      | Some t1 ->
        Env.set_tvar env v1 t2;
        union t1 t2
    in
    Env.set_tvar env v1 t;
    t
  | _, TVar v2 ->
    let t =
      match Env.get_tvar env v2 with
      | None -> t1
      | Some t2 ->
        Env.set_tvar env v2 t1;
        union t1 t2
    in
    Env.set_tvar env v2 t;
    t
  | TClosureEnv, TClosureEnv -> t2
  | ( ( TLit _ | TArrow _ | TTuple _ | TStruct _ | TEnum _ | TOption _
      | TArray _ | TDefault _ | TClosureEnv ),
      _ ) ->
    record_type_error ();
    t2

let unify
    (env : 'e Env.t)
    (e : ('a, 'm) gexpr as 'e) (* used for error context *)
    (t1 : typ)
    (t2 : typ) : unit =
  ignore (union env e t1 t2)

let lit_type (lit : lit) : naked_typ =
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

(* This returns the type without quantification, for a given application of the
   operator *)
let polymorphic_op_type (op : Operator.polymorphic operator Mark.pos) : typ =
  let open Operator in
  let pos = Mark.get op in
  let any = lazy (Type.fresh_var pos) in
  let any2 = lazy (Type.fresh_var pos) in
  let any3 = lazy (Type.fresh_var pos) in
  let ut = lazy (TLit TUnit, pos) in
  let bt = lazy (TLit TBool, pos) in
  let it = lazy (TLit TInt, pos) in
  let cet = lazy (TClosureEnv, pos) in
  let array a = lazy (TArray (Lazy.force a), pos) in
  let option a = lazy (TOption (Lazy.force a), pos) in
  let ( @-> ) x y = lazy (TArrow (List.map Lazy.force x, Lazy.force y), pos) in
  let ty =
    match Mark.remove op with
    | Fold -> [[any2; any] @-> any2; any2; array any] @-> any2
    | Eq -> [any; any] @-> bt
    | Map -> [[any] @-> any2; array any] @-> array any2
    | Map2 -> [[any; any2] @-> any3; array any; array any2] @-> array any3
    | Filter -> [[any] @-> bt; array any] @-> array any
    | Reduce -> [[any; any] @-> any; [ut] @-> any; array any] @-> any
    | Concat -> [array any; array any] @-> array any
    | Log (PosRecordIfTrueBool, _) -> [bt] @-> bt
    | Log _ -> [any] @-> any
    | Length -> [array any] @-> it
    | HandleExceptions ->
      let pair a b = lazy (TTuple [Lazy.force a; Lazy.force b], pos) in
      let tpos = lazy (TLit TPos, pos) in
      let texn = option (pair any tpos) in
      [array texn] @-> texn
    | ToClosureEnv -> [any] @-> cet
    | FromClosureEnv -> [cet] @-> any
  in
  Lazy.force ty

(* Just returns the return type of the operator, assuming the operand types are
   known. Less trict, but useful on monomorphised code where the operators no
   longer have their standard types *)
let polymorphic_op_return_type
    env
    e
    (op : Operator.polymorphic operator Mark.pos)
    (targs : typ list) : typ =
  let open Operator in
  let pos = Mark.get op in
  let return_type tf arity =
    let tret = Type.any pos in
    let tfunc = TArrow (List.init arity (fun _ -> Type.any pos), tret), pos in
    unify env e tf tfunc;
    get_ty env e tret
  in
  match Mark.remove op, targs with
  | Fold, [_; tau; _] -> tau
  | Reduce, [tf; _; _] -> return_type tf 2
  | Eq, _ -> TLit TBool, pos
  | Map, [tf; _] -> TArray (return_type tf 1), pos
  | Map2, [tf; _; _] -> TArray (return_type tf 2), pos
  | (Filter | Concat), [_; tau] -> tau
  | Log (PosRecordIfTrueBool, _), _ -> TLit TBool, pos
  | Log _, [tau] -> tau
  | Length, _ -> TLit TInt, pos
  | HandleExceptions, [_] -> Type.any pos
  | ToClosureEnv, _ -> TClosureEnv, pos
  | FromClosureEnv, _ -> Type.any pos
  | op, targs ->
    Message.error ~pos "Mismatched operator arguments: %a@ (%a)"
      (Print.operator ?debug:None)
      op
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         Type.format)
      targs

let resolve_overload_ret_type
    ~flags:_
    _e
    (op : Operator.overloaded operator Mark.pos)
    tys : typ =
  let op_ty = Operator.overload_type op tys in
  Type.arrow_return op_ty

(** {1 Double-directed typing} *)

let expr_ty env ((_, Custom { custom; _ }) as e) = get_ty env e custom

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up :
    type a m.
    decl_ctx ->
    (a, m) gexpr Env.t ->
    (a, m) gexpr ->
    (a, typ custom) boxed_gexpr =
 fun ctx env e ->
  typecheck_expr_top_down ctx env (Type.fresh_var (Expr.pos e)) e

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down :
    type a m.
    decl_ctx ->
    (a, m) gexpr Env.t ->
    typ ->
    (a, m) gexpr ->
    (a, typ custom) boxed_gexpr =
 fun ctx env tau e ->
  (* Message.debug "Propagating type %a for naked_expr :@.@[<hov 2>%a@]"
   *    Type.format (get_ty env e tau) Expr.format e; *)
  let pos_e = Expr.pos e in
  let flags = env.flags in
  let () =
    (* If there already is a type annotation on the given expr, ensure it
       matches (unless it's a type variable) *)
    match Mark.get e with
    | Untyped _ (* | Typed { ty = TVar _, _; _ } *) -> ()
    | Typed { ty; _ } -> unify env e tau ty
    | Custom _ -> assert false
  in
  let context_mark = Custom { custom = tau; pos = pos_e } in
  let mark_with_tau_and_unify ty =
    (* Unify with the supplied type first, and return the mark *)
    Custom { custom = union env e ty tau; pos = pos_e }
  in
  match Mark.remove e with
  | ELocation loc ->
    let ty_opt =
      match loc with
      | DesugaredScopeVar { name; _ } | ScopelangScopeVar { name } ->
        Env.get_scope_var env (Mark.remove name)
      | ToplevelVar { name; _ } -> Env.get_toplevel_var env (Mark.remove name)
    in
    let ty =
      match ty_opt with
      | Some ty -> ty
      | None ->
        Message.error ~pos:pos_e "Reference to %a not found" (Print.expr ()) e
    in
    Expr.elocation loc (mark_with_tau_and_unify ty)
  | EStruct { name; fields } ->
    let mark = mark_with_tau_and_unify (TStruct name, pos_e) in
    let str_ast = StructName.Map.find name ctx.ctx_structs in
    let str = StructName.Map.find name env.structs in
    let _check_fields : unit =
      let missing_fields, extra_fields =
        StructField.Map.fold
          (fun fld x (remaining, extra) ->
            if StructField.Map.mem fld remaining then
              StructField.Map.remove fld remaining, extra
            else remaining, StructField.Map.add fld x extra)
          fields
          (str_ast, StructField.Map.empty)
      in
      let errs =
        List.map
          (fun (f, ty) ->
            Format.asprintf "Missing field %a" StructField.format f, Mark.get ty)
          (StructField.Map.bindings missing_fields)
        @ List.map
            (fun (f, ef) ->
              let dup = StructField.Map.mem f str in
              ( Format.asprintf "%s field %a"
                  (if dup then "Duplicate" else "Unknown")
                  StructField.format f,
                Expr.pos ef ))
            (StructField.Map.bindings extra_fields)
      in
      if errs <> [] then
        Message.error ~extra_pos:errs
          "Mismatching field definitions for structure %a" StructName.format
          name
    in
    let fields =
      StructField.Map.mapi
        (fun f_name f_e ->
          let f_ty = StructField.Map.find f_name str in
          typecheck_expr_top_down ctx env f_ty f_e)
        fields
    in
    Expr.estruct ~name ~fields mark
  | EDStructAmend { name_opt = _; e; fields } ->
    let e = typecheck_expr_top_down ctx env tau e in
    let name =
      match expr_ty env e with
      | TStruct name, _ -> name
      | TForAll _, _ | TVar _, _ -> failwith "Disambiguation failure"
      | ty ->
        Message.error ~pos:(Expr.pos e)
          "This expression has type %a, where a structure was expected"
          Type.format ty
    in
    let fields = Ident.Map.map (typecheck_expr_bottom_up ctx env) fields in
    (* Note: here we identify the structure name, and type the fields
       individually, but without enforcing any consistency constraint between
       the two. This is fine because this construction only appears in
       Desugared, where it is used for disambiguation. In later passes this is
       rewritten into a struct literal, so no need to anticipate name resolution
       and duplicate the checks here. *)
    Expr.edstructamend ~name_opt:(Some name) ~e ~fields context_mark
  | EDStructAccess { e = e_struct; name_opt; field } ->
    let t_struct =
      match name_opt with
      | Some name -> TStruct name, pos_e
      | None -> Type.fresh_var pos_e
    in
    let e_struct' = typecheck_expr_top_down ctx env t_struct e_struct in
    let name_opt =
      match expr_ty env e_struct' with
      | TStruct name, _ -> Some name
      | TForAll _, _ | TVar _, _ -> None
      | ty ->
        Message.error ~pos:(Expr.pos e)
          "This is not a structure, cannot access field @{<magenta>%s@}@ \
           (found type: %a)"
          field Type.format ty
    in
    let name, field =
      let candidate_structs =
        try Ident.Map.find field ctx.ctx_struct_fields
        with Ident.Map.Not_found _ -> (
          match name_opt with
          | None ->
            Message.error
              ~pos:(Expr.mark_pos context_mark)
              "Field@ @{<magenta>%s@}@ does@ not@ belong@ to@ any@ known@ \
               structure"
              field StructName.format
          (* Since we were unable to disambiguate, we can't get any hints at
             this point (but explaining the situation in more detail would
             probably not be helpful) *)
          | Some name -> (
            match
              ScopeName.Map.choose_opt
              @@ ScopeName.Map.filter
                   (fun _ { out_struct_name; _ } ->
                     StructName.equal out_struct_name name)
                   ctx.ctx_scopes
            with
            | Some (scope_out, _) ->
              let str =
                try StructName.Map.find name env.structs
                with StructName.Map.Not_found _ ->
                  Message.error ~pos:pos_e "No structure %a found"
                    StructName.format name
              in
              Message.error
                ~fmt_pos:
                  [
                    ( (fun ppf ->
                        Format.fprintf ppf
                          "@{<magenta>%s@} is used here as an output" field),
                      Expr.mark_pos context_mark );
                    ( (fun ppf ->
                        Format.fprintf ppf "Scope %a is declared here"
                          ScopeName.format scope_out),
                      Mark.get (StructName.get_info name) );
                  ]
                "Variable @{<magenta>%s@} is not a declared output of scope %a."
                field ScopeName.format scope_out
                ~suggestion:
                  (Suggestions.sorted_candidates
                     (List.map StructField.to_string (StructField.Map.keys str))
                     field)
            | None ->
              Message.error
                ~extra_pos:
                  [
                    "", Expr.mark_pos context_mark;
                    "Structure definition", Mark.get (StructName.get_info name);
                  ]
                "Field@ @{<yellow>\"%s\"@}@ does@ not@ belong@ to@ structure@ \
                 @{<yellow>\"%a\"@}."
                field StructName.format name
                ~suggestion:
                  (Suggestions.sorted_candidates
                     (Ident.Map.keys ctx.ctx_struct_fields)
                     field)))
      in
      match name_opt with
      | None ->
        if StructName.Map.cardinal candidate_structs = 1 then
          StructName.Map.choose candidate_structs
        else
          Message.error
            ~pos:(Expr.mark_pos context_mark)
            "@[<v>@[<hov>Ambiguous field access @{<cyan>%s@}:@ the@ parent@ \
             structure@ could@ not@ be@ determined@ at@ this@ point.@ The@ \
             following@ structures@ have@ a@ field@ by@ this@ name:@]@,\
             @[<v>%a@]@,\
             @[<hov>@{<b>Hint@}: explicit the structure the field belongs to \
             using@ x.@{<cyan>StructName@}.@{<magenta>%s@}@ (or@ \
             x.@{<blue>ModuleName@}.@{<cyan>StructName@}.@{<magenta>%s@})@]@]"
            field
            (Format.pp_print_list (fun fmt s_name ->
                 Format.fprintf fmt "- %a" StructName.format s_name))
            (StructName.Map.keys candidate_structs)
            field field
      | Some name -> (
        try name, StructName.Map.find name candidate_structs
        with StructName.Map.Not_found _ ->
          Message.error
            ~pos:(Expr.mark_pos context_mark)
            "Field@ @{<magenta>%s@}@ does@ not@ belong@ to@ structure@ %a@ \
             (however, structure@ %a@ defines@ it).@]"
            field StructName.format name
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ or@ ")
               StructName.format)
            (StructName.Map.keys candidate_structs))
    in
    let str =
      try StructName.Map.find name env.structs
      with StructName.Map.Not_found _ ->
        Message.error ~pos:pos_e "No structure %a found" StructName.format name
    in
    let fld_ty = StructField.Map.find field str in
    let mark = mark_with_tau_and_unify fld_ty in
    Expr.estructaccess ~name ~e:e_struct' ~field mark
  | EStructAccess { e = e_struct; name; field } ->
    let fld_ty =
      let str =
        try StructName.Map.find name env.structs
        with StructName.Map.Not_found _ ->
          Message.error ~pos:pos_e "No structure %a found" StructName.format
            name
      in
      try StructField.Map.find field str
      with StructField.Map.Not_found _ ->
        Message.error ~pos:pos_e
          ~fmt_pos:
            [
              ( (fun ppf ->
                  Format.fprintf ppf "Structure %a declared here"
                    StructName.format name),
                Mark.get (StructName.get_info name) );
            ]
          "Structure %a doesn't define a field %a" StructName.format name
          StructField.format field
    in
    let mark = mark_with_tau_and_unify fld_ty in
    let e_struct' =
      typecheck_expr_top_down ctx env (TStruct name, pos_e) e_struct
    in
    Expr.estructaccess ~e:e_struct' ~field ~name mark
  | EInj { name; cons; e = e_enum }
    when Definitions.EnumName.equal name Expr.option_enum ->
    if Definitions.EnumConstructor.equal cons Expr.some_constr then
      let cell_type = Type.fresh_var (Expr.pos e) in
      let mark = mark_with_tau_and_unify (TOption cell_type, pos_e) in
      let e_enum' = typecheck_expr_top_down ctx env cell_type e_enum in
      Expr.einj ~name ~cons ~e:e_enum' mark
    else
      (* None constructor *)
      let cell_type = Type.fresh_var (Expr.pos e) in
      let mark = mark_with_tau_and_unify (TOption cell_type, pos_e) in
      let e_enum' =
        typecheck_expr_top_down ctx env (TLit TUnit, pos_e) e_enum
      in
      Expr.einj ~name ~cons ~e:e_enum' mark
  | EInj { name; cons; e = e_enum } ->
    let mark = mark_with_tau_and_unify (TEnum name, pos_e) in
    let e_enum' =
      typecheck_expr_top_down ctx env
        (EnumConstructor.Map.find cons (EnumName.Map.find name env.enums))
        e_enum
    in
    Expr.einj ~e:e_enum' ~cons ~name mark
  | EMatch { e = e1; name; cases }
    when Definitions.EnumName.equal name Expr.option_enum ->
    let cell_type = Type.fresh_var (Expr.pos e1) in
    let t_arg = TOption cell_type, Expr.pos e1 in
    let cases_ty =
      EnumConstructor.Map.of_list
        [
          Expr.none_constr, (TLit TUnit, Expr.pos e1);
          Expr.some_constr, cell_type;
        ]
    in
    let t_ret = Type.fresh_var (Expr.pos e) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env t_arg e1 in
    let cases =
      EnumConstructor.Map.mapi
        (fun c_name e ->
          let c_ty = EnumConstructor.Map.find c_name cases_ty in
          let e_ty = TArrow ([c_ty], t_ret), Expr.pos e in
          typecheck_expr_top_down ctx env e_ty e)
        cases
    in
    Expr.ematch ~e:e1' ~name ~cases mark
  | EMatch { e = e1; name; cases } ->
    let cases_ty = EnumName.Map.find name ctx.ctx_enums in
    let t_ret = Type.fresh_var (Expr.pos e1) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env (TEnum name, pos_e) e1 in
    let cases =
      EnumConstructor.Map.mapi
        (fun c_name e ->
          let c_ty = EnumConstructor.Map.find c_name cases_ty in
          (* For now our constructors are limited to zero or one argument. If
             there is a change to allow for multiple arguments, it might be
             easier to use tuples directly. *)
          let e_ty = TArrow ([c_ty], t_ret), Expr.pos e in
          typecheck_expr_top_down ctx env e_ty e)
        cases
    in
    Expr.ematch ~e:e1' ~name ~cases mark
  | EScopeCall { scope; args } ->
    let scope_out_struct =
      (ScopeName.Map.find scope ctx.ctx_scopes).out_struct_name
    in
    let mark = mark_with_tau_and_unify (TStruct scope_out_struct, pos_e) in
    let vars = ScopeName.Map.find scope env.scopes_input in
    let args' =
      ScopeVar.Map.mapi
        (fun name (p, e) ->
          let e' =
            typecheck_expr_top_down ctx env (ScopeVar.Map.find name vars) e
          in
          p, e')
        args
    in
    Expr.escopecall ~scope ~args:args' mark
  | EVar v ->
    let tau' =
      match Env.get env v with
      | Some t -> Type.unquantify t
      | None ->
        Message.error ~pos:pos_e "Variable %s not found in the current context"
          (Bindlib.name_of v)
    in
    Expr.evar (Var.translate v) (mark_with_tau_and_unify tau')
  | EExternal { name } ->
    let ty =
      let not_found pr x =
        Message.error ~pos:pos_e
          "Could not resolve the reference to %a.@ Make sure the corresponding \
           module was properly loaded?"
          pr x
      in
      match Mark.remove name with
      | External_value name -> (
        try
          let atyp, _vis = TopdefName.Map.find name ctx.ctx_topdefs in
          atyp
        with TopdefName.Map.Not_found _ -> not_found TopdefName.format name)
      | External_scope name -> (
        try
          let scope_info = ScopeName.Map.find name ctx.ctx_scopes in
          ( TArrow
              ( [TStruct scope_info.in_struct_name, pos_e],
                (TStruct scope_info.out_struct_name, pos_e) ),
            pos_e )
        with ScopeName.Map.Not_found _ -> not_found ScopeName.format name)
    in
    Expr.eexternal ~name (mark_with_tau_and_unify ty)
  | ELit lit -> Expr.elit lit (mark_with_tau_and_unify (lit_type lit, pos_e))
  | ETuple es ->
    let tys = List.map (fun _ -> Type.fresh_var (Expr.pos e)) es in
    let mark = mark_with_tau_and_unify (TTuple tys, pos_e) in
    let es' = List.map2 (typecheck_expr_top_down ctx env) tys es in
    Expr.etuple es' mark
  | ETupleAccess { e = e1; index; size } ->
    let out_of_bounds size =
      Message.error ~pos:pos_e "Tuple access out of bounds (%d/%d)" index size
    in
    let tuple_ty =
      if size = 0 then
        (* Unset yet, we resolve it now *)
        Type.fresh_var (Expr.pos e1)
      else if index >= size then out_of_bounds size
      else
        ( TTuple
            (List.init size (fun n ->
                 if n = index then tau else Type.fresh_var (Expr.pos e1))),
          Expr.pos e1 )
    in
    let e1' = typecheck_expr_top_down ctx env tuple_ty e1 in
    let size, mark =
      if size <> 0 then size, context_mark
      else
        match get_ty env e tuple_ty with
        | TTuple l, _ -> (
          match List.nth_opt l index with
          | None -> out_of_bounds (List.length l)
          | Some ty -> List.length l, mark_with_tau_and_unify ty)
        | TForAll _, _ | TVar _, _ -> failwith "Disambiguation failure"
        | ty ->
          Message.error ~pos:(Expr.pos e1)
            "This expression has type@ %a,@ while a tuple was expected"
            Type.format ty
    in
    Expr.etupleaccess ~e:e1' ~index ~size mark
  | EAbs { binder; pos; tys = t_args } ->
    (* Polymorphism is only allowed, explicitely, on toplevel definitions : if
       it happens here, the corresponding type variables will already have been
       set. Consequently, we don't quantify Type.fresh_var variables here. *)
    if Bindlib.mbinder_arity binder <> List.length t_args then
      Message.error ~internal:true ~pos:(Expr.pos e)
        "@[<v>function has %d variables but was supplied %d types:@,%a@]"
        (Bindlib.mbinder_arity binder)
        (List.length t_args) Expr.format e;
    let t_ret = Type.fresh_var pos_e in
    let t_func = TArrow (t_args, t_ret), pos_e in
    let mark = mark_with_tau_and_unify t_func in
    let xs, body = Bindlib.unmbind binder in
    let xs' = Array.map Var.translate xs in
    let env =
      List.fold_left2
        (fun env x tau_arg -> Env.add_var x tau_arg env)
        env (Array.to_list xs) t_args
    in
    let body' = typecheck_expr_top_down ctx env t_ret body in
    let binder' = Bindlib.bind_mvar xs' (Expr.Box.lift body') in
    Expr.eabs binder' pos (List.map (get_ty env body) t_args) mark
  | EApp { f = (EAbs _, _) as e1; args; tys = t_args } ->
    (* let-in: there may not be any implicit arguments, and detuplification has
       been done during desugaring. The type of the function body may need to be
       inferred from its arguments *)
    let t_args =
      match t_args with
      | [] -> List.map (fun e -> Type.fresh_var (Expr.pos e)) args
      | _ -> t_args
    in
    let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
    let t_func = TArrow (t_args, tau), Expr.pos e1 in
    let e1' = typecheck_expr_top_down ctx env t_func e1 in
    let tys = List.map2 (get_ty env) args t_args in
    Expr.eapp ~f:e1' ~args:args' ~tys context_mark
  | EApp { f = e1; args; tys } ->
    (* Non-letin application: the arguments may need to be detuplified ; the
       type of the function should be checked for implicit arguments *)
    let e1 = typecheck_expr_bottom_up ctx env e1 in
    let func_ty = expr_ty env e1 in
    let tau_args_all, t_ret =
      match Type.unquantify func_ty with
      | TArrow (tau_args, t_ret), _ -> tau_args, t_ret
      | _ ->
        Message.error ~pos:(Expr.pos e1)
          "This is not a function and can't be applied.@ It has type %a"
          Type.format func_ty
    in
    if tys <> [] then List.iter2 (unify env e) tys tau_args_all;
    let tau_args_implicit, tau_args =
      List.partition
        (fun ty -> Pos.has_attr (Mark.get ty) ImplicitPosArg)
        (if tys = [] then tau_args_all else tys)
    in
    let args' = List.map (typecheck_expr_bottom_up ctx env) args in
    let ty_args = List.map (expr_ty env) args' in
    let args_tys =
      (* Handles typing before detuplification *)
      match args, ty_args, tau_args with
      | [e], [(TTuple tys, _)], _ :: _ :: _ -> List.map (fun ty -> e, ty) tys
      | _ -> List.combine args ty_args
    in
    let tys =
      try
        List.map2 (fun (arg, ty) tau -> union env arg ty tau) args_tys tau_args
      with Invalid_argument _ ->
        Message.error ~pos:(Expr.pos e)
          "This function application has %d arguments, but expects %d."
          (List.length ty_args) (List.length tau_args)
    in
    let tys =
      let rec insert_implicit tau_args_all tys =
        match tau_args_all, tys with
        | tau :: tau_args_all, tys
          when Pos.has_attr (Mark.get tau) ImplicitPosArg ->
          tau :: insert_implicit tau_args_all tys
        | _ :: tau_args_all, ty :: tys -> ty :: insert_implicit tau_args_all tys
        | [], [] -> []
        | _ -> assert false
      in
      if tau_args_implicit = [] then tys else insert_implicit tau_args_all tys
    in
    Expr.eapp ~f:e1 ~args:args' ~tys (mark_with_tau_and_unify t_ret)
  | EAppOp { op; tys = t_args; args } ->
    let t_func = TArrow (t_args, tau), pos_e in
    let args =
      Operator.kind_dispatch (Mark.set pos_e op)
        ~polymorphic:(fun op ->
          if env.flags.assume_op_types then (
            unify env e (polymorphic_op_return_type env e op t_args) tau;
            List.rev_map (typecheck_expr_bottom_up ctx env) (List.rev args))
          else (
            (* Type the operator first, then right-to-left: polymorphic
               operators are required to allow the resolution of all type
               variables this way *)
            unify env e (polymorphic_op_type op) t_func;
            (* List.rev_map(2) applies the side effects in order *)
            let args =
              List.rev_map2
                (typecheck_expr_top_down ctx env)
                (List.rev t_args) (List.rev args)
            in
            (* Equality is actually not truly polymorphic, it needs expansion,
               so add a check here for now *)
            (match op, args with
            | (Eq, _), a :: _ ->
              if not (Type.fully_known (expr_ty env a)) then
                Message.delayed_error () ~kind:Typing ~pos:(Mark.get op) "%a"
                  Format.pp_print_text
                  "Equality cannot be resolved at this point: the type of the \
                   operands is not fully known."
            | _ -> ());
            args))
        ~overloaded:(fun op ->
          (* Typing the arguments first is required to resolve the operator *)
          let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
          unify env e tau
            (resolve_overload_ret_type ~flags e op
               (List.map2 (get_ty env) args t_args));
          args')
        ~monomorphic:(fun op ->
          (* Here it doesn't matter but may affect the error messages *)
          unify env e (Operator.monomorphic_type op) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
        ~resolved:(fun op ->
          (* This case should not fail *)
          unify env e (Operator.resolved_type op) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
    in
    (* All operator applications are monomorphised at this point *)
    let tys = List.map2 (get_ty env) args t_args in
    Expr.eappop ~op ~args ~tys context_mark
  | EDefault { excepts; just; cons } ->
    let cons' = typecheck_expr_top_down ctx env tau cons in
    let just' =
      typecheck_expr_top_down ctx env (TLit TBool, Expr.pos just) just
    in
    let excepts' = List.map (typecheck_expr_top_down ctx env tau) excepts in
    Expr.edefault ~excepts:excepts' ~just:just' ~cons:cons' context_mark
  | EPureDefault e1 ->
    let inner_ty = Type.fresh_var (Expr.pos e1) in
    let mark = mark_with_tau_and_unify (TDefault inner_ty, Expr.pos e1) in
    let e1' = typecheck_expr_top_down ctx env inner_ty e1 in
    Expr.epuredefault e1' mark
  | EIfThenElse { cond; etrue = et; efalse = ef } ->
    let et' = typecheck_expr_top_down ctx env tau et in
    let ef' = typecheck_expr_top_down ctx env tau ef in
    let cond' =
      typecheck_expr_top_down ctx env (TLit TBool, Expr.pos cond) cond
    in
    Expr.eifthenelse cond' et' ef' context_mark
  | EAssert e1 ->
    let mark = mark_with_tau_and_unify (TLit TUnit, pos_e) in
    let e1' = typecheck_expr_top_down ctx env (TLit TBool, Expr.pos e1) e1 in
    Expr.eassert e1' mark
  | EFatalError err -> Expr.efatalerror err context_mark
  | EPos p -> Expr.epos p (mark_with_tau_and_unify (TLit TPos, pos_e))
  | EEmpty ->
    Expr.eempty
      (mark_with_tau_and_unify (TDefault (Type.fresh_var (Expr.pos e)), pos_e))
  | EErrorOnEmpty e1 ->
    let tau' = TDefault tau, pos_e in
    let e1' = typecheck_expr_top_down ctx env tau' e1 in
    Expr.eerroronempty e1' context_mark
  | EArray es ->
    let cell_type = Type.fresh_var (Expr.pos e) in
    let mark = mark_with_tau_and_unify (TArray cell_type, pos_e) in
    let es' = List.map (typecheck_expr_top_down ctx env cell_type) es in
    Expr.earray es' mark
  | ECustom { obj; targs; tret } ->
    let mark = mark_with_tau_and_unify (TArrow (targs, tret), Expr.pos e) in
    Expr.ecustom obj targs tret mark

(** {1 API} *)

let get_ty_mark env (Custom { custom = ty; pos }) =
  Typed { ty = get_ty_quantified env pos ty; pos }

let expr_raw
    (type a)
    (ctx : decl_ctx)
    ?(env = Env.empty ctx)
    ?(typ : typ option)
    (e : (a, 'm) gexpr) : (a, typ custom) gexpr =
  let fty =
    match typ with
    | None -> typecheck_expr_bottom_up ctx env
    | Some typ -> typecheck_expr_top_down ctx env typ
  in
  Expr.unbox (fty e)

let check_expr ctx ?env ?typ e =
  Expr.map_marks
    ~f:(fun (Custom { pos; _ }) -> Untyped { pos })
    (expr_raw ctx ?env ?typ e)

(* Infer the type of an expression *)
let expr ctx ?(env = Env.empty ctx) ?typ e =
  match typ with
  | Some (TForAll bnd, tpos) ->
    (* polymorphic function case *)
    let tvars, typ = Bindlib.unmbind bnd in
    let e' = typecheck_expr_top_down ctx env typ e in
    let _tvars =
      Array.fold_left
        (fun acc tv ->
          let acc = Type.Var.Set.add tv acc in
          match get_ty env e (TVar tv, tpos) with
          | TVar tv', _ when Type.Var.equal tv tv' -> acc
          | (TForAll _, _) as ty when Type.is_universal ty -> acc
          | TVar tv', pos ->
            if Type.Var.Set.mem tv' acc || Array.mem tv' tvars then
              Message.delayed_error () ~kind:Typing ~pos:(Expr.pos e')
                ~extra_pos:["", pos]
                "@[<hv>@[<hov>This function has type@ @]%a@ @[<hov>which \
                 requires that@ %a = %a,@]@ @[<hov>while@ they@ are@ both@ \
                 specified@ as@ \"@{<cyan>anything@}\".@]@]@ You may want to \
                 give them the same explicit name@ (\"@{<cyan>anything of type \
                 t@}\")"
                Type.format (expr_ty env e') Type.format (TVar tv, tpos)
                Type.format (TVar tv', tpos);
            Type.Var.Set.add tv' acc
          | ty ->
            Message.delayed_error () ~kind:Typing ~pos:(Mark.get ty)
              "In this function definition, the type@ %a@ is@ specified@ as@ \
               @{<cyan>anything@},@ but it appears to only work for@ %a@ here"
              Type.format (TVar tv, tpos) Type.format ty;
            acc)
        Type.Var.Set.empty tvars
    in
    (* TODO: cleanup the used type vars from env ? *)
    let e' = Expr.map_marks ~f:(get_ty_mark env) (Expr.unbox e') in
    let typ = TForAll bnd, tpos in
    Expr.set_ty typ e'
  | _ -> Expr.map_marks ~f:(get_ty_mark env) (expr_raw ctx ~env ?typ e)

let scope_body_expr ctx env ty_out body_expr =
  let _env, ret =
    BoundList.fold_map body_expr ~init:env
      ~last:(fun env e ->
        let e' = Expr.unbox (typecheck_expr_top_down ctx env ty_out e) in
        let e' = Expr.map_marks ~f:(get_ty_mark env) e' in
        env, Expr.Box.lift e')
      ~f:(fun env var scope ->
        let e0 = scope.scope_let_expr in
        let ty_e = scope.scope_let_typ in
        let e = Expr.unbox (typecheck_expr_bottom_up ctx env e0) in
        unify env e0 (expr_ty env e) ty_e;
        (* We could use [typecheck_expr_top_down] rather than this manual
           unification, but we get better messages with this order of the
           [unify] parameters, which keeps location of the type as defined
           instead of as inferred. *)
        ( Env.add_var var scope.scope_let_typ env,
          Var.translate var,
          Bindlib.box_apply
            (fun scope_let_expr ->
              {
                scope with
                scope_let_typ =
                  (match scope.scope_let_typ with
                  | TForAll _, _ -> expr_ty env e
                  | ty -> ty);
                scope_let_expr;
              })
            (Expr.Box.lift (Expr.map_marks ~f:(get_ty_mark env) e)) ))
  in
  ret

let scope_body ctx env body =
  let get_pos struct_name = Mark.get (StructName.get_info struct_name) in
  let struct_ty struct_name =
    Mark.add (get_pos struct_name) (TStruct struct_name)
  in
  let ty_in = struct_ty body.scope_body_input_struct in
  let ty_out = struct_ty body.scope_body_output_struct in
  let var, e = Bindlib.unbind body.scope_body_expr in
  let env = Env.add_var var ty_in env in
  let e' = scope_body_expr ctx env ty_out e in
  ( Bindlib.box_apply
      (fun scope_body_expr -> { body with scope_body_expr })
      (Bindlib.bind_var (Var.translate var) e'),
    Mark.add (get_pos body.scope_body_output_struct) (TArrow ([ty_in], ty_out))
  )

let scopes ctx env =
  BoundList.fold_map ~init:env
    ~last:(fun env el ->
      ( env,
        Scope.map_exports
          (fun e -> Expr.map_marks ~f:(get_ty_mark env) (expr_raw ctx ~env e))
          el ))
    ~f:(fun env var item ->
      match item with
      | ScopeDef (name, body) ->
        let body_e, ty_scope = scope_body ctx env body in
        ( Env.add_var var ty_scope env,
          Var.translate var,
          Bindlib.box_apply (fun body -> ScopeDef (name, body)) body_e )
      | Topdef (name, typ, vis, e) ->
        let e' = expr ctx ~env ~typ e in
        ( Env.add_var var typ env,
          Var.translate var,
          Bindlib.box_apply
            (fun e -> Topdef (name, typ, vis, e))
            (Expr.Box.lift e') ))

let program ?assume_op_types prg =
  let env = Env.empty ?assume_op_types prg.decl_ctx in
  let new_env, code_items = scopes prg.decl_ctx env prg.code_items in
  {
    lang = prg.lang;
    module_name = prg.module_name;
    code_items = Bindlib.unbox code_items;
    decl_ctx =
      {
        prg.decl_ctx with
        ctx_structs =
          StructName.Map.mapi
            (fun s_name fields ->
              StructField.Map.mapi
                (fun f_name (t : typ) ->
                  match Mark.remove t with
                  | TForAll _ ->
                    StructField.Map.find f_name
                      (StructName.Map.find s_name new_env.structs)
                  | _ -> t)
                fields)
            prg.decl_ctx.ctx_structs;
        ctx_enums =
          EnumName.Map.mapi
            (fun e_name cons ->
              EnumConstructor.Map.mapi
                (fun cons_name (t : typ) ->
                  match Mark.remove t with
                  | TForAll _ ->
                    EnumConstructor.Map.find cons_name
                      (EnumName.Map.find e_name new_env.enums)
                  | _ -> t)
                cons)
            prg.decl_ctx.ctx_enums;
      };
  }

let program ?assume_op_types ?(internal_check = false) prg =
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
        Message.debug "@[<v>Faulty intermediate program:@,%a@]"
          (Print.program ~debug:true)
          prg;
        Printexc.raise_with_backtrace err bt)
    else fun f -> Message.with_delayed_errors f
  in
  wrap @@ fun () -> program ?assume_op_types prg
