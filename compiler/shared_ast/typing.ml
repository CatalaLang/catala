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

type flags = { fail_on_any : bool; assume_op_types : bool }

type unionfind = typ UnionFind.elem

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
    tvars : unionfind Type.Var.Map.t;
  }

  let empty
      ?(fail_on_any = true)
      ?(assume_op_types = false)
      (decl_ctx : decl_ctx) =
    (* We fill the environment initially with the structs and enums
       declarations *)
    {
      flags = { fail_on_any; assume_op_types };
      structs = decl_ctx.ctx_structs;
      enums = decl_ctx.ctx_enums;
      vars = Var.Map.empty;
      scope_vars = ScopeVar.Map.empty;
      scopes = ScopeName.Map.empty;
      scopes_input = ScopeName.Map.empty;
      toplevel_vars = TopdefName.Map.empty;
      tvars = Type.Var.Map.empty;
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
    (* Format.fprintf ppf "structs: @[<hov>%a@]@,"
     *   (StructName.Map.format_keys ~pp_sep) env.structs;
     * Format.fprintf ppf "enums: @[<hov>%a@]@,"
     *   (EnumName.Map.format_keys ~pp_sep) env.enums;
     * Format.fprintf ppf "vars: @[<hov>%a@]@,"
     *   (Var.Map.format_keys ~pp_sep) env.vars; *)
    Format.fprintf ppf "scopes: @[<hov>%a@]@,"
      (ScopeName.Map.format_keys ~pp_sep)
      env.scopes;
    Format.fprintf ppf "topdefs: @[<hov>%a@]@,"
      (TopdefName.Map.format_keys ~pp_sep)
      env.toplevel_vars;
    Format.pp_close_box ppf ()

  let get_unionfind env v = Type.Var.Map.find_opt v env.tvars

  let unionfind env v pos =
    match get_unionfind env v with
    | Some uf -> env, uf
    | None ->
      let uf = UnionFind.make (TVar v, pos) in
      { env with tvars = Type.Var.Map.add v uf env.tvars },
      uf

  let get_ty env v pos =
    match get_unionfind env v with
    | Some uf -> UnionFind.get (UnionFind.find uf)
    | None -> TVar v, pos
end

(** Possible results of the resolution of a type variable *)
type tvar_resolution =
  | Value of { uf: unionfind; typ: typ }
  (** The variable has been unified with the given type *)
  | Free of { uf: unionfind }
  (** The variable resolves to itself *)
  | Unbound
  (** The variable isn't in the environment, normally this means an universally bound variable *)

let tvar_resolve env v =
  match Env.get_unionfind env v with
  | None -> Unbound
  | Some uf ->
    if UnionFind.is_representative uf then Free { uf }
    else Value { uf; typ = UnionFind.get (UnionFind.find uf) }

let get_ty env ty =
  let rec aux = function
    | TVar v, _ ->
      (match tvar_resolve env v with
       | Value { typ; _ } -> aux typ
       | Free _ | Unbound -> Type.rebox ty)
    | ty -> Type.map aux ty
  in
  Bindlib.unbox (aux ty)

(*  match Mark.remove t with
 *   | TUnionFind (T uf) -> get_ty (UnionFind.get (UnionFind.find uf))
 *   | _ -> t
 * 
 * let make_uf (ty: naked_typ Bindlib.box) (pos: Pos.t) : typ Bindlib.box =
 *   Bindlib.box_apply (fun t -> TUnionFind (T (UnionFind.make (t, pos))), pos) ty
 * 
 * let make_uf_box (ty: naked_typ) (pos: Pos.t): typ Bindlib.box =
 *   Bindlib.box_apply (fun t -> TUnionFind (T (UnionFind.make (t, pos))), pos)
 *     (Bindlib.box ty) *)


let appty ~(pos:Pos.t) fty ty =
  Bindlib.box_apply (fun t -> fty t, pos) ty

(* let any (pos: Pos.t) : typ = TUnionFind (T (UnionFind.make (TVar (TVar.fresh ()), pos))), pos *)

let typ_needs_parens t =
  match Mark.remove t with TArrow _ | TArray _ | TAny _ -> true | _ -> false

let with_color f color fmt x =
  (* equivalent to [Format.fprintf fmt "@{<color>%s@}" s] *)
  Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg (C4 color)));
  f fmt x;
  Format.pp_close_stag fmt ()

let pp_color_string = with_color Format.pp_print_string

let rec format_typ
    ~(bctx : Bindlib.ctxt)
    ~(colors : Ocolor_types.color4 list)
    (fmt : Format.formatter)
    (ty : typ): unit  =
  let format_typ_with_parens
      ~bctx
      ~colors
      (fmt : Format.formatter)
      (ty : typ) =
    if typ_needs_parens ty then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      format_typ ~bctx ~colors:(List.tl colors) fmt ty;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else Format.fprintf fmt "%a" (format_typ ~bctx ~colors) ty
  in
  match Mark.remove ty with
  | TLit l -> Format.fprintf fmt "%a" Print.tlit l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         (fun fmt t -> format_typ ~bctx fmt ~colors:(List.tl colors) t))
      ts
      (pp_color_string (List.hd colors))
      ")"
  | TStruct s -> StructName.format fmt s
  | TEnum e -> EnumName.format fmt e
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>option %a@]"
      (format_typ_with_parens ~bctx~colors:(List.tl colors))
      t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ →@ %a@]"
      (format_typ_with_parens ~bctx ~colors)
      t1 (format_typ ~bctx ~colors) t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@ →@ %a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_typ_with_parens ~bctx ~colors:(List.tl colors)))
      t1
      (pp_color_string (List.hd colors))
      ")" (format_typ ~bctx ~colors) t2
  | TArray t1 -> (
    match Mark.remove t1 with
    | TAny _ when not Global.options.debug -> Format.pp_print_string fmt "list"
    | _ -> Format.fprintf fmt "@[list of@ %a@]" (format_typ ~bctx ~colors) t1)
  | TDefault t1 ->
    Format.pp_print_as fmt 1 "⟨";
    format_typ ~bctx ~colors fmt t1;
    Format.pp_print_as fmt 1 "⟩"
  | TVar v -> Type.Var.format fmt v
  | TAny tb ->
    let vars, t, bctx = Bindlib.unmbind_in bctx tb in
    Format.fprintf fmt "@[<h>%a@]%a"
      (Format.pp_print_list (fun fmt v -> Format.fprintf fmt "∀%a." Type.Var.format v))
      (Array.to_list vars)
      (format_typ_with_parens ~bctx ~colors) t
  | TClosureEnv -> Format.fprintf fmt "closure_env"

let rec colors =
  let open Ocolor_types in
  blue :: cyan :: green :: yellow :: red :: magenta :: colors

let dummy_flags = { fail_on_any = false; assume_op_types = false }

(* let rec var_occurs v = function
 *   | TVar v1, _ -> TVar.equal v v1
 *   | TAny tb, _ ->
 *     let _, ty = Bindlib.unmbind tb in var_occurs v (get_ty ty)
 *   | TUnionFind (T uf), _ ->
 *     var_occurs v (UnionFind.get uf) ||
 *     (not (UnionFind.is_representative uf) &&
 *      var_occurs v (UnionFind.get (UnionFind.find uf)) )
 *   | ty ->
 *     shallow_fold
 *       (fun ty acc -> acc || var_occurs v (get_ty ty))
 *       ty false *)

(* let rec typ_to_ast ~flags (ty : typ) : typ =
 *   match ty with
 *   | TUnionFind (T uf), _ ->
 *     let ty = UnionFind.get (UnionFind.find uf) in
 *     typ_to_ast ~flags ty
 *   | TVar v, pos ->
 *     TVar (TVar.translate v), pos
 *   | TAny bnd, _ ->
 *     let _, ty = Bindlib.unmbind bnd in
 *     typ_to_ast ~flags ty
 *   | (TLit _ | TStruct _ | TEnum _ | TClosureEnv), _ as ty -> ty
 *   | TTuple tl, m -> TTuple (List.map (typ_to_ast ~flags) tl), m
 *   | TOption ty, m -> TOption (typ_to_ast ~flags ty), m
 *   | TArray ty, m -> TArray (typ_to_ast ~flags ty), m
 *   | TDefault ty, m -> TDefault (typ_to_ast ~flags ty), m
 *   | TArrow (tl, ty), m -> TArrow (List.map (typ_to_ast ~flags) tl, typ_to_ast ~flags ty), m
 *   (* let rec aux vars ty =
 *    *   let ty, pos = get_ty ty in
 *    *   let ( @& ) f bt = Bindlib.box_apply (fun t -> f t, pos) bt in
 *    *   match ty with
 *    *   | TLit l -> vars, Bindlib.box (TLit l, pos)
 *    *   | TTuple ts ->
 *    *     let vars, ts = List.fold_left_map aux vars ts in
 *    *     vars, (fun ts -> TTuple ts) @& Bindlib.box_list ts
 *    *   | TStruct s -> vars, Bindlib.box (TStruct s, pos)
 *    *   | TEnum e -> vars, Bindlib.box (TEnum e, pos)
 *    *   | TOption t ->
 *    *     let vars, t = aux vars t in
 *    *     vars, (fun t -> TOption t) @& t
 *    *   | TArrow (t1, t2) ->
 *    *     let vars, t1 = List.fold_left_map aux vars t1 in
 *    *     let vars, t2 = aux vars t2 in
 *    *     ( vars,
 *    *       Bindlib.box_apply2
 *    *         (fun t1 t2 -> TArrow (t1, t2), pos)
 *    *         (Bindlib.box_list t1) t2 )
 *    *   | TArray t1 ->
 *    *     let vars, t1 = aux vars t1 in
 *    *     vars, (fun t1 -> TArray t1) @& t1
 *    *   | TDefault t1 ->
 *    *     let vars, t1 = aux vars t1 in
 *    *     vars, (fun t1 -> TDefault t1) @& t1
 *    *   | TVar v ->
 *    *     let vars, var =
 *    *       match TVar.Map.find_opt v vars with
 *    *       | Some var -> vars, var
 *    *       | None ->
 *    *         let var = Type.Var.fresh () in
 *    *         TVar.Map.add v var vars, var
 *    *     in
 *    *     vars, Fun.id @& (Bindlib.box_var var)
 *    *   | TAny tb ->
 *    *     let vs, t = Bindlib.unmbind tb in
 *    *     let vars, vs2 = Array.fold_left_map (fun vars v1 ->
 *    *         let v2 = Type.Var.fresh () in
 *    *         TVar.Map.add v1 v2 vars, v2)
 *    *         vars vs
 *    *     in
 *    *     let vars, t = aux vars t in
 *    *     let vars = Array.fold_left (fun vars v -> TVar.Map.remove v vars) vars vs in
 *    *     let tb = Bindlib.bind_mvar vs2 t in
 *    *     vars, (fun tb -> TAny tb) @& tb
 *    *   | TClosureEnv -> vars, Bindlib.box (TClosureEnv, pos)
 *    * in
 *    * (* let pos = Mark.get (get_ty ty) in *)
 *    * let vars, bty = aux TVar.Map.empty ty in
 *    * if not (TVar.Map.is_empty vars) then
 *    *   Message.warning "Remaining free vars after conversion to AST!?@ %a"
 *    *     format_typ ty;
 *    * Bindlib.unbox bty *) *)

(* Wraps all `TVar` inside a `TUnionFind`. The crucial part is that, in the returned type, every instance of a given var is in the same unionfind element.  *)
(* let ast_to_typ_aux ?(tvars=Type.Var.Map.empty) (ty : typ) : unionfind Type.Var.Map.t * typ =
 *   let vars = ref tvars in
 *   let rec aux: typ -> typ = function
 *     | TVar v, pos ->
 *       let v1 = TVar.translate v in
 *       let uf =
 *         try Type.Var.Map.find v !vars
 *         with Type.Var.Map.Not_found _ ->
 *           let uf = UnionFind.make (TVar v1, pos) in
 *           vars := Type.Var.Map.add v uf !vars;
 *           uf
 *       in
 *       TUnionFind (T uf), pos
 *     | TAny bnd, _ ->
 *       let tvars, ty = Bindlib.unmbind bnd in
 *       let ret = aux ty in
 *       vars := Array.fold_left (fun acc v -> Type.Var.Map.remove v acc) !vars tvars;
 *       ret
 *     | (TLit _ | TStruct _ | TEnum _ | TClosureEnv), _ as ty -> ty
 *     | TTuple tl, m -> TTuple (List.map aux tl), m
 *     | TOption ty, m -> TOption (aux ty), m
 *     | TArray ty, m -> TArray (aux ty), m
 *     | TDefault ty, m -> TDefault (aux ty), m
 *     | TArrow (tl, ty), m -> TArrow (List.map aux tl, aux ty), m
 *   in
 *   !vars, aux ty
 * 
 * let ast_to_typ ty = snd (ast_to_typ_aux ty)
 * 
 * let ast_to_typ_list tl =
 *   let _, ty = List.fold_left_map (fun tvars ty -> ast_to_typ_aux ~tvars ty) Type.Var.Map.empty tl
 *   in ty *)

(*
  let rec aux vars (ty, pos) =
    match ty with
    | TLit l -> vars, make_uf_box (TLit l) pos
    | TTuple ts ->
      let vars, ts = List.fold_left_map aux vars ts in
      vars, appty ~pos (fun ts -> TTuple ts) (Bindlib.box_list ts)
    | TStruct s -> vars, make_uf_box (TStruct s) pos
    | TEnum e -> vars, make_uf_box (TEnum e) pos
    | TOption t ->
      let vars, t = aux vars t in
      vars, appty ~pos (fun t -> TOption t) t
    | TArrow (t1, t2) ->
      let vars, t1 = List.fold_left_map aux vars t1 in
      let vars, t2 = aux vars t2 in
      ( vars,
        Bindlib.box_apply2
          (fun t1 t2 -> UnionFind.make (TArrow (t1, t2), pos))
          (Bindlib.box_list t1) t2 )
    | TArray t1 ->
      let vars, t1 = aux vars t1 in
      vars, appty ~pos (fun t1 -> TArray t1) t1
    | TDefault t1 ->
      let vars, t1 = aux vars t1 in
      vars, appty ~pos (fun t1 -> TDefault t1) t1
    | TVar v ->
      let vars, var =
        match Type.Var.Map.find_opt v vars with
        | Some var -> vars, var
        | None ->
          let var = TVar.fresh () in
          Type.Var.Map.add v var vars, var
      in
      vars, make_uf (Bindlib.box_var var) pos
    | TAny tb ->
      let vs, t = Bindlib.unmbind tb in
      let vars, vs2 = Array.fold_left_map (fun vars v1 ->
          let v2 = TVar.fresh () in
          Type.Var.Map.add v1 v2 vars, v2)
          vars vs
      in
      let vars, t = aux vars t in
      let vars = Array.fold_left (fun vars v -> Type.Var.Map.remove v vars) vars vs in
      let tb = Bindlib.bind_mvar vs2 t in
      vars, appty ~pos (fun tb -> TAny tb) tb
    | TClosureEnv -> vars, make_uf_box TClosureEnv pos
  in
  (* let pos = Mark.get (get_ty ty) in *)
  let vars, bty = aux Type.Var.Map.empty ty in
  if not (Type.Var.Map.is_empty vars) then
    Message.warning "Remaining free vars after conversion from AST...@ %a"
      Type.format ty;
  Bindlib.unbox bty
*)


  (* let rec aux vars = function
   *   | TVar v, _ -> Type.Var.Map.find v vars
   *   | TAny tb, m ->
   *     let var, ty = Bindlib.unbind tb in
   *     let any = UnionFind.make (TAny (Var.fresh ()), m) in
   *     let vars = Type.Var.Map.add var any vars in
   *     aux vars ty
   *   | ty, m ->
   *     let ty =
   *       match ty with
   *       | TLit l -> TLit l
   *       | TArrow (t1, t2) -> TArrow (List.map (aux vars) t1, aux vars t2)
   *       | TTuple ts -> TTuple (List.map (aux vars) ts)
   *       | TStruct s -> TStruct s
   *       | TEnum e -> TEnum e
   *       | TOption t -> TOption (aux vars t)
   *       | TArray t -> TArray (aux vars t)
   *       | TDefault t -> TDefault (aux vars t)
   *       | TClosureEnv -> TClosureEnv
   *       | TVar _ | TAny _ -> assert false
   *     in
   *     UnionFind.make (ty, m)
   * in
   * aux vars ty *)

(** {1 Types and unification} *)

let record_type_error env (AnyExpr e) t1 t2 =
  (* We convert union-find types to ast ones otherwise error messages would be
     hindered as union-find side-effects wrongly unify both types. The delayed
     pretty-printing would yield messages such as: 'incompatible types (integer,
     integer)' *)
  let t1_repr = get_ty env t1 in
  let t2_repr = get_ty env t2 in
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
  Message.delayed_error ~kind:Typing () ~fmt_pos
    "Error during typechecking, incompatible types:@\n\
     @[<v>@{<blue>@<2>%s@} @[<hov>%a@]@,\
     @{<blue>@<2>%s@} @[<hov>%a@]@]" "─➤" pp_typ t1_repr "─➤" pp_typ t2_repr

(** Raises an error if unification cannot be performed. The position annotation
    of the second [unionfind_typ] argument is propagated (unless it is [TAny]). *)
let rec unify
    (env : 'e Env.t)
    (e : ('a, 'm) gexpr as 'e) (* used for error context *)
    (t1 : typ)
    (t2 : typ) : typ =
  (* Message.debug "Unifying %a and %a" (format_typ ctx) t1 (format_typ ctx)
     t2; *)
  let unify = unify env e in
  let pos2 = Mark.get t2 in
  let record_type_error () = record_type_error env (AnyExpr e) t1 t2 in
  match Mark.remove t1, Mark.remove t2 with
(*
  | TUnionFind (T uf1), TUnionFind (T uf2) ->
    let ty1 = UnionFind.get (UnionFind.find uf1) in
    let ty2 = UnionFind.get (UnionFind.find uf2) in
    let ty = unify ty1 ty2 in
    let uf = UnionFind.union uf1 uf2 in
    UnionFind.set uf ty;
    TUnionFind (T uf), Mark.get ty
  | TUnionFind (T uf), _ ->
    let ty = unify (UnionFind.get (UnionFind.find uf)) t2 in
    UnionFind.set uf ty;
    TUnionFind (T uf), Mark.get ty
  | _, TUnionFind (T uf) ->
    let ty = unify t1 (UnionFind.get (UnionFind.find uf)) in
    UnionFind.set uf ty;
    TUnionFind (T uf), Mark.get ty
*)
  | TLit tl1, TLit tl2 -> if tl1 <> tl2 then record_type_error (); t2
  | TArrow (targs1, tret1), TArrow (targs2, tret2) ->
    let tret = unify tret1 tret2 in
    let targs =
      try List.map2 unify targs1 targs2
      with Invalid_argument _ -> record_type_error (); targs2
    in
    TArrow (targs, tret), pos2
  | TTuple ts1, TTuple ts2 ->
    let ts =
      try List.map2 unify ts1 ts2
      with Invalid_argument _ -> record_type_error (); ts2
    in
    TTuple ts, pos2
  | TStruct s1, TStruct s2 ->
    if not (StructName.equal s1 s2) then record_type_error ();
    t2
  | TEnum e1, TEnum e2 ->
    if not (EnumName.equal e1 e2) then record_type_error ();
    t2
  | TOption t1', TOption t2' -> TOption (unify t1' t2'), pos2
  | TArray t1', TArray t2' -> TArray (unify t1' t2'), pos2
  | TDefault t1', TDefault t2' -> TDefault (unify t1' t2'), pos2
  | TVar v1, TVar v2 ->
    if Bindlib.eq_vars v1 v2 then t2 else
    (match tvar_resolve env v1, tvar_resolve env v2 with
     | Unbound, _ | _, Unbound -> record_type_error (); t2
     | Free { uf = uf1 }, Free { uf = uf2 } ->
       UnionFind.set (UnionFind.union uf1 uf2) t2;
       t2
     | Value { uf = uf1; typ = typ1 }, Value { uf = uf2; typ = typ2 } ->
       let uf = UnionFind.union uf1 uf2 in
       let typ = unify typ1 typ2 in
       UnionFind.set uf typ;
       typ
     | Free { uf = uf1 }, Value { uf = uf2; typ }
     | Value { uf = uf1; typ }, Free { uf = uf2 } ->
       ignore (UnionFind.union uf1 uf2);
       typ)
  | TVar v, _ ->
    (match tvar_resolve env v with
     | Unbound -> record_type_error (); t2
     | Free { uf } -> UnionFind.set uf t2; t2
     | Value { uf; typ } ->
       let typ = unify typ t2 in
       UnionFind.set uf typ;
       typ)
  | _, TVar v ->
    (match tvar_resolve env v with
     | Unbound -> record_type_error (); t1
     | Free { uf } -> UnionFind.set uf t1; t1
     | Value { uf; typ } ->
       let typ = unify t1 typ in
       UnionFind.set uf typ;
       typ)
  | TClosureEnv, TClosureEnv -> t2
  | TAny t1b, _ ->
    let _, t1 = Bindlib.unmbind t1b in
    unify t1 t2
  | _, TAny t2b ->
    let _, t2 = Bindlib.unmbind t2b in
    unify t1 t2
  | ( ( TLit _ | TArrow _ | TTuple _ | TStruct _ | TEnum _ | TOption _
      | TArray _ | TDefault _ | TClosureEnv ),
      _ ) ->
    record_type_error (); t2

let unify'
    (env: 'e Env.t)
    (e : ('a, 'm) gexpr as 'e) (* used for error context *)
    (t1 : typ)
    (t2 : typ) : unit =
  ignore (unify env e t1 t2)


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

let polymorphic_op_type (op : Operator.polymorphic operator Mark.pos) :
    typ =
  let open Operator in
  let pos = Mark.get op in
  let v1, v2, v3 = Type.Var.fresh (), Type.Var.fresh (), Type.Var.fresh () in
  let vars = [v1; v2; v3] in
  let any = lazy (Bindlib.box_var v1) in
  let any2 = lazy (Bindlib.box_var v2) in
  let any3 = lazy (Bindlib.box_var v3) in
  let ut = lazy (TLit TUnit) in
  let bt = lazy (TLit TBool) in
  let it = lazy (TLit TInt) in
  let cet = lazy TClosureEnv in
  let array a = lazy (appty ~pos (fun t -> TArray t) (Lazy.force a)) in
  let option a = lazy (
    appty ~pos (fun t -> TOption t) (Lazy.force a)
  ) in
  let ( @-> ) x y =
    lazy (
      Bindlib.box_apply2 (fun args ret -> TArrow (args, ret), pos)
        (Bindlib.box_list (List.map Lazy.force x))
        (Lazy.force y)
    )
  in
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
      let pair a b =
        lazy (
          Bindlib.box_apply2 (fun a b -> TTuple [a; b], pos)
            (Lazy.force a) (Lazy.force b)
        )
      in
      let tpos = lazy (make_uf_box (TLit TPos) pos) in
      let texn = option (pair any tpos) in
      [array texn] @-> texn
    | ToClosureEnv -> [any] @-> cet
    | FromClosureEnv -> [cet] @-> any
  in
  let ty = Lazy.force ty in
  let vars =
    List.filter (fun v -> Bindlib.occur v ty) vars
    |> Array.of_list
  in
  appty ~pos (fun bnd -> TAny bnd) (Bindlib.bind_mvar vars ty)
  |> Bindlib.unbox

(* Just returns the return type of the operator, assuming the operand types are
   known. Less trict, but useful on monomorphised code where the operators no
   longer have their standard types *)
let polymorphic_op_return_type
    ctx
    e
    (op : Operator.polymorphic operator Mark.pos)
    (targs : typ list) : typ =
  let open Operator in
  let pos = Mark.get op in
  let return_type tf arity =
    let tret = any pos in
    let tfunc = TArrow (List.init arity (fun _ -> any pos), tret), pos in
    unify' ctx e tf tfunc;
    tret
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
  | HandleExceptions, [_] -> any pos
  | ToClosureEnv, _ -> TClosureEnv, pos
  | FromClosureEnv, _ -> any pos
  | op, targs ->
    Message.error ~pos "Mismatched operator arguments: %a@ (%a)"
      (Print.operator ?debug:None)
      op
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         format_typ)
      targs

let resolve_overload_ret_type
    ~flags
    (ctx : decl_ctx)
    _e
    (op : Operator.overloaded operator Mark.pos)
    tys : typ =
  let op_ty =
    Operator.overload_type ctx op (List.map (typ_to_ast ~flags) tys)
  in
  ast_to_typ (Type.arrow_return op_ty)

(** {1 Double-directed typing} *)

let add_pos e ty = Mark.add (Expr.pos e) ty

let ty : (_, typ custom) marked -> typ =
  fun (_, Custom { custom; _ }) -> custom

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up :
    type a m.
    decl_ctx ->
    (a, m) gexpr Env.t ->
    (a, m) gexpr ->
    (a, typ custom) boxed_gexpr =
 fun ctx env e ->
  typecheck_expr_top_down ctx env
    (any (Expr.pos e))
    e

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down :
    type a m.
    decl_ctx ->
    (a, m) gexpr Env.t ->
    typ ->
    (a, m) gexpr ->
    (a, typ custom) boxed_gexpr =
 fun ctx env tau e ->
  Message.debug "Propagating type %a for naked_expr :@.@[<hov 2>%a@]"
     format_typ tau Expr.format e;
  let pos_e = Expr.pos e in
  let flags = env.flags in
  let () =
    (* If there already is a type annotation on the given expr, ensure it
       matches *)
    match Mark.get e with
    | Untyped _ -> ()
    | Typed { ty; _ } ->
      let ty = ast_to_typ ty in
      unify' ctx e tau ty
    | Custom _ -> assert false
  in
  let context_mark = Custom { custom = tau; pos = pos_e } in
  let mark_with_tau_and_unify ty =
    (* Unify with the supplied type first, and return the mark *)
    unify' ctx e ty tau;
    Custom { custom = ty; pos = pos_e }
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
    Expr.elocation loc (mark_with_tau_and_unify (ast_to_typ ty))
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
            ( Format.asprintf "Missing field %a" StructField.format f,
              Mark.get ty ))
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
      match get_ty (ty e) with
      | TStruct name, _ -> name
      | TAny _, _ | TVar _, _ -> failwith "Disambiguation failure"
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "This expression has type %a, where a structure was expected"
          format_typ (ty e)
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
      | None -> any pos_e
    in
    let e_struct' = typecheck_expr_top_down ctx env t_struct e_struct in
    let name_opt =
      match get_ty (ty e_struct') with
      | TStruct name, _ -> Some name
      | TAny _, _ | TVar _, _ -> None
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "This is not a structure, cannot access field @{<magenta>%s@}@ \
           (found type: %a)"
          field format_typ (ty e_struct')
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
                     (List.map StructField.to_string
                        (StructField.Map.keys str))
                     field)
            | None ->
              Message.error
                ~extra_pos:
                  [
                    "", Expr.mark_pos context_mark;
                    ( "Structure definition",
                      Mark.get (StructName.get_info name) );
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
        Message.error ~pos:pos_e "No structure %a found" StructName.format
          name
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
      let cell_type = any (Expr.pos e) in
      let mark = mark_with_tau_and_unify (TOption cell_type, pos_e) in
      let e_enum' = typecheck_expr_top_down ctx env cell_type e_enum in
      Expr.einj ~name ~cons ~e:e_enum' mark
    else
      (* None constructor *)
      let cell_type = any (Expr.pos e) in
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
    let cell_type = any (Expr.pos e1) in
    let t_arg = TOption cell_type, Expr.pos e1 in
    let cases_ty =
      ListLabels.fold_right2
        [Expr.none_constr; Expr.some_constr]
        [TLit TUnit, Expr.pos e1; cell_type]
        ~f:EnumConstructor.Map.add ~init:EnumConstructor.Map.empty
    in
    let t_ret = any (Expr.pos e) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env t_arg e1 in
    let cases =
      EnumConstructor.Map.merge
        (fun _ e e_ty ->
          match e, e_ty with
          | Some e, Some e_ty ->
            Some
              (typecheck_expr_top_down ctx env
                 (TArrow ([e_ty], t_ret), Expr.pos e)
                 e)
          | _ -> assert false)
        cases cases_ty
    in
    Expr.ematch ~e:e1' ~name ~cases mark
  | EMatch { e = e1; name; cases } ->
    let cases_ty = EnumName.Map.find name ctx.ctx_enums in
    let t_ret = any (Expr.pos e1) in
    let mark = mark_with_tau_and_unify t_ret in
    let e1' = typecheck_expr_top_down ctx env (TEnum name, pos_e) e1 in
    let cases =
      EnumConstructor.Map.mapi
        (fun c_name e ->
          let c_ty = EnumConstructor.Map.find c_name cases_ty in
          (* For now our constructors are limited to zero or one argument. If
             there is a change to allow for multiple arguments, it might be
             easier to use tuples directly. *)
          let e_ty = TArrow ([ast_to_typ c_ty], t_ret), Expr.pos e in
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
            typecheck_expr_top_down ctx env
              (ast_to_typ (ScopeVar.Map.find name vars))
              e
          in
          p, e')
        args
    in
    Expr.escopecall ~scope ~args:args' mark
  | EVar v ->
    let tau' =
      match Env.get env v with
      | Some t -> ast_to_typ t
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
          ast_to_typ atyp
        with TopdefName.Map.Not_found _ ->
          not_found TopdefName.format name)
      | External_scope name -> (
        try
          let scope_info = ScopeName.Map.find name ctx.ctx_scopes in
          ast_to_typ
            ( TArrow
                ( [TStruct scope_info.in_struct_name, pos_e],
                  (TStruct scope_info.out_struct_name, pos_e) ),
              pos_e )
        with ScopeName.Map.Not_found _ -> not_found ScopeName.format name)
    in
    Expr.eexternal ~name (mark_with_tau_and_unify ty)
  | ELit lit -> Expr.elit lit (mark_with_tau_and_unify (lit_type lit, pos_e))
  | ETuple es ->
    let tys = List.map (fun _ -> any (Expr.pos e)) es in
    let mark = mark_with_tau_and_unify (TTuple tys, pos_e) in
    let es' = List.map2 (typecheck_expr_top_down ctx env) tys es in
    Expr.etuple es' mark
  | ETupleAccess { e = e1; index; size } ->
    let out_of_bounds size =
      Message.error ~pos:pos_e "Tuple access out of bounds (%d/%d)" index size
    in
    let tuple_ty =
      if size = 0 then (* Unset yet, we resolve it now *)
        any (Expr.pos e1)
      else if index >= size then out_of_bounds size
      else
        TTuple
          (List.init size (fun n ->
               if n = index then tau
               else any (Expr.pos e1))),
        Expr.pos e1
    in
    let e1' = typecheck_expr_top_down ctx env tuple_ty e1 in
    let size, mark =
      if size <> 0 then size, context_mark
      else
        match get_ty tuple_ty with
        | TTuple l, _ -> (
          match List.nth_opt l index with
          | None -> out_of_bounds (List.length l)
          | Some ty -> List.length l, mark_with_tau_and_unify ty)
        | TAny _, _ | TVar _, _ -> failwith "Disambiguation failure"
        | _ ->
          Message.error ~pos:(Expr.pos e1)
            "This expression has type@ %a,@ while a tuple was expected"
            format_typ tuple_ty
    in
    Expr.etupleaccess ~e:e1' ~index ~size mark
  | EAbs { binder; pos; tys = t_args } ->
    (* Polymorphism is only allowed, explicitely, on toplevel definitions :
       if it happens here, the corresponding type variables will already have been set.
       Consequently, we don't quantify any variables here.
    *)
    let _, t_args = Bindlib.unmbind t_args in (* <<= UNIFY with tau *)
    if Bindlib.mbinder_arity binder <> List.length t_args then
      Message.error ~pos:(Expr.pos e)
        "function has %d variables but was supplied %d types\n%a"
        (Bindlib.mbinder_arity binder)
        (List.length t_args) Expr.format e;
    let tau_args = List.map ast_to_typ t_args in (* wrong *)
    let t_ret = any pos_e in
    let t_func = TArrow (tau_args, t_ret), pos_e in
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
    Expr.eabs binder' pos Bindlib.(unbox (bind_mvar [||] (box (List.map (typ_to_ast ~flags) tau_args)))) mark
  | EApp { f = e1; args; tys } ->
    (* Here we type the arguments first (in order), to ensure we know the types
       of the arguments if [f] is [EAbs] before disambiguation. This is also the
       right order for the [let-in] form. *)
    let t_args =
      match tys with
      | [] -> List.map (fun _ -> any (Expr.pos e)) args
      | tys -> List.map ast_to_typ tys
    in
    let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
    Message.debug "LETIN? %a" Expr.format e1;
    Message.debug "args <<< @[<hv>%a@]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") format_typ) t_args;
    let t_args =
      match t_args, tys with
      | [t], [] -> (
        (* Handles typing before detuplification: if [tys] was not yet set, we
           are allowed to destruct a tuple into multiple arguments (see
           [Scopelang.from_desugared] for the corresponding code
           transformation) *)
        match get_ty t with TTuple tys, _ -> tys | _ -> t_args)
      | _ ->
        if List.length t_args <> List.length args' then
          Message.error ~pos:(Expr.pos e)
            (match e1 with
            | EAbs _, _ -> "This binds %d variables, but %d were provided."
            | _ -> "This function application has %d arguments, but expects %d.")
            (List.length t_args) (List.length args');

        t_args
    in
    let t_func = TArrow (t_args, tau), Expr.pos e1 in
    let e1' = typecheck_expr_top_down ctx env t_func e1 in
    Expr.eapp ~f:e1' ~args:args'
      ~tys:(List.map (typ_to_ast ~flags) t_args)
      context_mark
  | EAppOp { op; tys; args } ->
    let t_args = List.map ast_to_typ tys in
    let t_func = TArrow (t_args, tau), pos_e in
    let args =
      Operator.kind_dispatch (Mark.set pos_e op)
        ~polymorphic:(fun op ->
          if env.flags.assume_op_types then (
            unify' ctx e (polymorphic_op_return_type ctx e op t_args) tau;
            List.rev_map (typecheck_expr_bottom_up ctx env) (List.rev args))
          else (
            (* Type the operator first, then right-to-left: polymorphic
               operators are required to allow the resolution of all type
               variables this way *)
            unify' ctx e (polymorphic_op_type op) t_func;
            (* List.rev_map(2) applies the side effects in order *)
            List.rev_map2
              (typecheck_expr_top_down ctx env)
              (List.rev t_args) (List.rev args)))
        ~overloaded:(fun op ->
          (* Typing the arguments first is required to resolve the operator *)
          let args' = List.map2 (typecheck_expr_top_down ctx env) t_args args in
          unify' ctx e tau (resolve_overload_ret_type ~flags ctx e op t_args);
          args')
        ~monomorphic:(fun op ->
          (* Here it doesn't matter but may affect the error messages *)
          unify' ctx e (ast_to_typ (Operator.monomorphic_type op)) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
        ~resolved:(fun op ->
          (* This case should not fail *)
          unify' ctx e (ast_to_typ (Operator.resolved_type op)) t_func;
          List.map2 (typecheck_expr_top_down ctx env) t_args args)
    in
    (* All operator applications are monomorphised at this point *)
    let tys = List.map (typ_to_ast ~flags) t_args in
    Expr.eappop ~op ~args ~tys context_mark
  | EDefault { excepts; just; cons } ->
    let cons' = typecheck_expr_top_down ctx env tau cons in
    let just' =
      typecheck_expr_top_down ctx env (TLit TBool, Expr.pos just) just
    in
    let excepts' = List.map (typecheck_expr_top_down ctx env tau) excepts in
    Expr.edefault ~excepts:excepts' ~just:just' ~cons:cons' context_mark
  | EPureDefault e1 ->
    let inner_ty = any (Expr.pos e1) in
    let mark =
      mark_with_tau_and_unify (TDefault inner_ty, Expr.pos e1)
    in
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
    let e1' =
      typecheck_expr_top_down ctx env (TLit TBool, Expr.pos e1) e1
    in
    Expr.eassert e1' mark
  | EFatalError err -> Expr.efatalerror err context_mark
  | EPos p -> Expr.epos p (mark_with_tau_and_unify (TLit TPos, pos_e))
  | EEmpty ->
    Expr.eempty (mark_with_tau_and_unify (TDefault (any (Expr.pos e)), pos_e))
  | EErrorOnEmpty e1 ->
    let tau' = TDefault tau, pos_e in
    let e1' = typecheck_expr_top_down ctx env tau' e1 in
    Expr.eerroronempty e1' context_mark
  | EArray es ->
    let cell_type = any (Expr.pos e) in
    let mark = mark_with_tau_and_unify (TArray cell_type, pos_e) in
    let es' = List.map (typecheck_expr_top_down ctx env cell_type) es in
    Expr.earray es' mark
  | ECustom { obj; targs; tret } ->
    let mark =
      mark_with_tau_and_unify (ast_to_typ (TArrow (targs, tret), Expr.pos e))
    in
    Expr.ecustom obj targs tret mark

(** {1 API} *)

let get_ty_mark ~flags (Custom { custom = uf; pos }) =
  Typed { ty = typ_to_ast ~flags uf; pos }

let expr_raw
    (type a)
    (ctx : decl_ctx)
    ?(env = Env.empty ctx)
    ?(typ : typ option)
    (e : (a, 'm) gexpr) : (a, typ custom) gexpr =
  let fty =
    match typ with
    | None -> typecheck_expr_bottom_up ctx env
    | Some typ -> typecheck_expr_top_down ctx env (ast_to_typ typ)
  in
  Expr.unbox (fty e)

let check_expr ctx ?env ?typ e =
  Expr.map_marks
    ~f:(fun (Custom { pos; _ }) -> Untyped { pos })
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
        let e0 = scope.scope_let_expr in
        let ty_e = ast_to_typ scope.scope_let_typ in
        let e = Expr.unbox (typecheck_expr_bottom_up ctx env e0) in
        unify' ctx e0 (ty e) ty_e;
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
                  | TAny _, _ -> typ_to_ast ~flags:env.flags (ty e)
                  | ty -> ty);
                scope_let_expr;
              })
            (Expr.Box.lift (Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) e))
        ))
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
  let e' = scope_body_expr ctx env (ast_to_typ ty_out) e in
  ( Bindlib.box_apply
      (fun scope_body_expr -> { body with scope_body_expr })
      (Bindlib.bind_var (Var.translate var) e'),
      (Mark.add
         (get_pos body.scope_body_output_struct)
         (TArrow ([ty_in], ty_out))) )

let scopes ctx env =
  BoundList.fold_map ~init:env
    ~last:(fun env el ->
      ( env,
        Scope.map_exports
          (fun e ->
            Expr.map_marks
              ~f:(get_ty_mark ~flags:env.flags)
              (expr_raw ctx ~env e))
          el ))
    ~f:(fun env var item ->
      match item with
      | ScopeDef (name, body) ->
        let body_e, ty_scope = scope_body ctx env body in
        ( Env.add_var var ty_scope env,
          Var.translate var,
          Bindlib.box_apply (fun body -> ScopeDef (name, body)) body_e )
      | Topdef (name, (TAny bnd, tpos), vis, e) ->
        (* polymorphic function case *)
        let tvars, typ = Bindlib.unmbind bnd in
        (* let tvars_map =
         *   Array.fold_left (fun acc va ->
         *       let vt = Type.Var.translate va in
         *       Type.Var.Map.add va (UnionFind.make (TVar vt, tpos)) acc)
         *     Type.Var.Map.empty tvars
         * in *)
        let tvars_map, typ = ast_to_typ_aux typ in
        let e' = typecheck_expr_top_down ctx env typ e in
        let typ = ty e' in
        Type.Var.Map.iter (fun v uf ->
            if not (Type.equal ~unionfind:(fun (T a) (T b) -> UnionFind.eq a b)
                      UnionFind.(get (find uf))
                      (TVar (Type.Var.translate v), tpos))
            then Message.error ~pos:tpos "Not as polymorphic as expected"
            (* FIXME: delayed and better message *)
          ) tvars_map;
        let tbinder =
          typ_to_ast ~flags:env.flags typ |> Type.rebox |> Bindlib.bind_mvar tvars |> Bindlib.unbox
        in
        let typ = TAny tbinder, (Mark.get typ) in
        let e' = Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) (Expr.unbox e') in
        ( Env.add_var var typ env,
          Var.translate var,
          Bindlib.box_apply
            (fun e -> Topdef (name, Expr.ty e', vis, e))
            (Expr.Box.lift e') )
      | Topdef (name, typ, vis, e) ->
        let e' = expr_raw ctx ~env ~typ e in
        let e' = Expr.map_marks ~f:(get_ty_mark ~flags:env.flags) e' in
        ( Env.add_var var typ env,
          Var.translate var,
          Bindlib.box_apply
            (fun e -> Topdef (name, Expr.ty e', vis, e))
            (Expr.Box.lift e') ))

let program ?fail_on_any ?assume_op_types prg =
  let env = Env.empty ?fail_on_any ?assume_op_types prg.decl_ctx in
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
                  | TAny _ ->
                    typ_to_ast ~flags:env.flags
                      (StructField.Map.find f_name
                         (StructName.Map.find s_name new_env.structs))
                  | _ -> t)
                fields)
            prg.decl_ctx.ctx_structs;
        ctx_enums =
          EnumName.Map.mapi
            (fun e_name cons ->
              EnumConstructor.Map.mapi
                (fun cons_name (t : typ) ->
                  match Mark.remove t with
                  | TAny _ ->
                    typ_to_ast ~flags:env.flags
                      (EnumConstructor.Map.find cons_name
                         (EnumName.Map.find e_name new_env.enums))
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
