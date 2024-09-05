(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

module DefaultBindlibCtxRename : Bindlib.Renaming = struct
  (* This code is a copy-paste from Bindlib, they forgot to expose the default
     implementation ! *)
  type ctxt = int String.Map.t

  let empty_ctxt = String.Map.empty

  let split_name : string -> string * int =
   fun name ->
    let len = String.length name in
    (* [i] is the index of the first first character of the suffix. *)
    let i =
      let is_digit c = '0' <= c && c <= '9' in
      let first_digit = ref len in
      let first_non_0 = ref len in
      while !first_digit > 0 && is_digit name.[!first_digit - 1] do
        decr first_digit;
        if name.[!first_digit] <> '0' then first_non_0 := !first_digit
      done;
      !first_non_0
    in
    if
      i = len || not (i >= 2 && name.[i - 1] = '_' && name.[i - 2] = '_')
      (* The || clause is a Catala addition *)
    then name, 0
    else String.sub name 0 (i - 2), int_of_string (String.sub name i (len - i))

  let get_suffix : string -> int -> ctxt -> int * ctxt =
   fun name suffix ctxt ->
    let n = try String.Map.find name ctxt with String.Map.Not_found _ -> -1 in
    let suffix = if suffix > n then suffix else n + 1 in
    suffix, String.Map.add name suffix ctxt

  let merge_name : string -> int -> string =
   fun prefix suffix ->
    if suffix > 0 then
      prefix ^ "__" ^ string_of_int suffix (* The "__" is a Catala addition *)
    else prefix

  let new_name : string -> ctxt -> string * ctxt =
   fun name ctxt ->
    let prefix, suffix = split_name name in
    let suffix, ctxt = get_suffix prefix suffix ctxt in
    merge_name prefix suffix, ctxt

  let reserve_name : string -> ctxt -> ctxt =
   fun name ctxt ->
    let prefix, suffix = split_name name in
    try
      let n = String.Map.find prefix ctxt in
      if suffix <= n then ctxt else String.Map.add prefix suffix ctxt
    with String.Map.Not_found _ -> String.Map.add prefix suffix ctxt

  let reset_context_for_closed_terms = false
  let skip_constant_binders = false
  let constant_binder_name = None
end

module type BindlibCtxt = module type of Bindlib.Ctxt (DefaultBindlibCtxRename)

type config = {
  reserved : string list;
  sanitize_varname : string -> string;
  skip_constant_binders : bool;
  constant_binder_name : string option;
}

type context = {
  bindCtx : (module BindlibCtxt);
  bcontext : DefaultBindlibCtxRename.ctxt;
  vars : string -> string;
  scopes : ScopeName.t -> ScopeName.t;
  topdefs : TopdefName.t -> TopdefName.t;
  structs : StructName.t -> StructName.t;
  fields : StructField.t -> StructField.t;
  enums : EnumName.t -> EnumName.t;
  constrs : EnumConstructor.t -> EnumConstructor.t;
}

let default_config =
  {
    reserved = [];
    sanitize_varname = Fun.id;
    skip_constant_binders = true;
    constant_binder_name = None;
  }

let patch_binder_name fname b =
  let name = fname (Bindlib.binder_name b) in
  let occurs = Bindlib.binder_occur b in
  let rank = Bindlib.binder_rank b in
  let mkfree v = EVar v in
  let subst = Bindlib.subst b in
  Bindlib.raw_binder name occurs rank mkfree subst

let patch_mbinder_names fname b =
  let names = Array.map fname (Bindlib.mbinder_names b) in
  let occurs = Bindlib.mbinder_occurs b in
  let rank = Bindlib.mbinder_rank b in
  let mkfree v = EVar v in
  let msubst = Bindlib.msubst b in
  Bindlib.raw_mbinder names occurs rank mkfree msubst

let unbind_in ctx ?fname b =
  let module BindCtx = (val ctx.bindCtx) in
  let b = match fname with Some fn -> patch_binder_name fn b | None -> b in
  let v, e, bcontext = BindCtx.unbind_in ctx.bcontext b in
  v, e, { ctx with bcontext }

let unmbind_in ctx ?fname b =
  let module BindCtx = (val ctx.bindCtx) in
  let b = match fname with Some fn -> patch_mbinder_names fn b | None -> b in
  let vs, e, bcontext = BindCtx.unmbind_in ctx.bcontext b in
  vs, e, { ctx with bcontext }

let set_rewriters ?scopes ?topdefs ?structs ?fields ?enums ?constrs ctx =
  (fun ?(scopes = ctx.scopes) ?(topdefs = ctx.topdefs) ?(structs = ctx.structs)
       ?(fields = ctx.fields) ?(enums = ctx.enums) ?(constrs = ctx.constrs) () ->
    { ctx with scopes; topdefs; structs; fields; enums; constrs })
    ?scopes ?topdefs ?structs ?fields ?enums ?constrs ()

let new_id ctx name =
  let module BindCtx = (val ctx.bindCtx) in
  let var, bcontext =
    BindCtx.new_var_in ctx.bcontext (fun _ -> assert false) name
  in
  Bindlib.name_of var, { ctx with bcontext }

let reserve_name ctx name =
  { ctx with bcontext = DefaultBindlibCtxRename.reserve_name name ctx.bcontext }

let get_ctx cfg =
  let module BindCtx = Bindlib.Ctxt (struct
    include DefaultBindlibCtxRename

    let skip_constant_binders = cfg.skip_constant_binders
    let constant_binder_name = cfg.constant_binder_name
  end) in
  {
    bindCtx = (module BindCtx);
    bcontext =
      List.fold_left
        (fun ctx name -> DefaultBindlibCtxRename.reserve_name name ctx)
        BindCtx.empty_ctxt cfg.reserved;
    vars = cfg.sanitize_varname;
    scopes = Fun.id;
    topdefs = Fun.id;
    structs = Fun.id;
    fields = Fun.id;
    enums = Fun.id;
    constrs = Fun.id;
  }

let rec typ ctx = function
  | TStruct n, m -> TStruct (ctx.structs n), m
  | TEnum n, m -> TEnum (ctx.enums n), m
  | ty -> Type.map (typ ctx) ty

(* {2 Handling expressions} *)

let rec expr : type k. context -> (k, 'm) gexpr -> (k, 'm) gexpr boxed =
 fun ctx e ->
  let fm m = Expr.map_ty (typ ctx) m in
  match e with
  | EExternal { name = External_scope s, pos }, m ->
    Expr.eexternal ~name:(External_scope (ctx.scopes s), pos) (fm m)
  | EExternal { name = External_value d, pos }, m ->
    Expr.eexternal ~name:(External_value (ctx.topdefs d), pos) (fm m)
  | EAbs { binder; tys }, m ->
    let vars, body, ctx = unmbind_in ctx ~fname:ctx.vars binder in
    let body = expr ctx body in
    let binder = Expr.bind vars body in
    Expr.eabs binder (List.map (typ ctx) tys) (fm m)
  | EStruct { name; fields }, m ->
    Expr.estruct ~name:(ctx.structs name)
      ~fields:
        (StructField.Map.fold
           (fun fld e -> StructField.Map.add (ctx.fields fld) (expr ctx e))
           fields StructField.Map.empty)
      (fm m)
  | EStructAccess { name; field; e }, m ->
    Expr.estructaccess ~name:(ctx.structs name) ~field:(ctx.fields field)
      ~e:(expr ctx e) (fm m)
  | EInj { name; e; cons }, m ->
    Expr.einj ~name:(ctx.enums name) ~cons:(ctx.constrs cons) ~e:(expr ctx e)
      (fm m)
  | EMatch { name; e; cases }, m ->
    Expr.ematch ~name:(ctx.enums name)
      ~cases:
        (EnumConstructor.Map.fold
           (fun cons e ->
             EnumConstructor.Map.add (ctx.constrs cons) (expr ctx e))
           cases EnumConstructor.Map.empty)
      ~e:(expr ctx e) (fm m)
  | e -> Expr.map ~typ:(typ ctx) ~f:(expr ctx) ~op:Fun.id e

let scope_name ctx s = ctx.scopes s
let topdef_name ctx s = ctx.topdefs s
let struct_name ctx s = ctx.structs s
let enum_name ctx e = ctx.enums e

(* {2 Handling scopes} *)

(** Maps carrying around a naming context, enriched at each [unbind] *)
let rec boundlist_map_ctx ~f ~fname ~last ~ctx = function
  | Last l -> Bindlib.box_apply (fun l -> Last l) (last ctx l)
  | Cons (item, next_bind) ->
    let item = f ctx item in
    let var, next, ctx = unbind_in ctx ~fname next_bind in
    let next = boundlist_map_ctx ~f ~fname ~last ~ctx next in
    let next_bind = Bindlib.bind_var var next in
    Bindlib.box_apply2
      (fun item next_bind -> Cons (item, next_bind))
      item next_bind

let rename_vars_in_lets ctx scope_body_expr =
  boundlist_map_ctx scope_body_expr ~ctx ~fname:String.to_snake_case
    ~last:(fun ctx e -> Expr.Box.lift (expr ctx e))
    ~f:(fun ctx scope_let ->
      Bindlib.box_apply
        (fun scope_let_expr ->
          {
            scope_let with
            scope_let_expr;
            scope_let_typ = typ ctx scope_let.scope_let_typ;
          })
        (Expr.Box.lift (expr ctx scope_let.scope_let_expr)))

let code_items ctx fty (items : 'e code_item_list) =
  let rec aux ctx = function
    | Last l ->
      let l =
        Bindlib.box_list
          (List.map
             (function EVar v -> Bindlib.box_var v | _ -> assert false)
             l)
      in
      Bindlib.box_apply (fun l -> Last l) l, ctx
    | Cons (ScopeDef (name, body), next_bind) ->
      let scope_body =
        let scope_input_var, scope_lets, ctx =
          unbind_in ctx ~fname:String.to_snake_case body.scope_body_expr
        in
        let scope_lets = rename_vars_in_lets ctx scope_lets in
        let scope_body_expr = Bindlib.bind_var scope_input_var scope_lets in
        Bindlib.box_apply
          (fun scope_body_expr ->
            {
              scope_body_input_struct =
                struct_name ctx body.scope_body_input_struct;
              scope_body_output_struct =
                struct_name ctx body.scope_body_output_struct;
              scope_body_expr;
              scope_body_visibility = body.scope_body_visibility;
            })
          scope_body_expr
      in
      let scope_var, next, ctx =
        match body.scope_body_visibility with
        | Public ->
          (* The scope name is already registered in the bcontext *)
          let name, _ = ScopeName.get_info (scope_name ctx name) in
          let v = Bindlib.new_var (fun v -> EVar v) name in
          let next = Bindlib.subst next_bind (EVar v) in
          v, next, ctx
        | Private ->
          (* Otherwise, it is treated as a normal variable *)
          unbind_in ctx ~fname:ctx.vars next_bind
      in
      let next_body, ctx = aux ctx next in
      let next_bind = Bindlib.bind_var scope_var next_body in
      ( Bindlib.box_apply2
          (fun body next_bind -> Cons (ScopeDef (name, body), next_bind))
          scope_body next_bind,
        ctx )
    | Cons (Topdef (name, ty, visibility, e), next_bind) ->
      let e = expr ctx e in
      let ty = fty ty in
      let topdef_var, next, ctx =
        match visibility with
        | Public ->
          (* The topef name is already registered in the bcontext *)
          let name, _ = TopdefName.get_info (topdef_name ctx name) in
          let v = Bindlib.new_var (fun v -> EVar v) name in
          let next = Bindlib.subst next_bind (EVar v) in
          v, next, ctx
        | Private ->
          (* Otherwise, it is treated as a normal variable *)
          unbind_in ctx ~fname:ctx.vars next_bind
      in
      let next_body, ctx = aux ctx next in
      let next_bind = Bindlib.bind_var topdef_var next_body in
      ( Bindlib.box_apply2
          (fun e next_bind ->
            Cons (Topdef (name, ty, visibility, e), next_bind))
          (Expr.Box.lift e) next_bind,
        ctx )
  in
  let items, ctx = aux ctx items in
  Bindlib.unbox items, ctx

module PathMap = Map.Make (Uid.Path)

(* Intermediate structure used by function [Renaming.program] *)
type type_renaming_ctx = {
  path_ctx : context PathMap.t;
  structs_map : StructName.t StructName.Map.t;
  fields_map : StructField.t StructField.Map.t;
  enums_map : EnumName.t EnumName.Map.t;
  constrs_map : EnumConstructor.t EnumConstructor.Map.t;
  ctx_structs : struct_ctx;
  ctx_enums : enum_ctx;
  namespaced_fields_constrs : bool;
  f_struct : string -> string;
  f_field : string -> string;
  f_enum : string -> string;
  f_constr : string -> string;
}

let process_type_ident
    (decl_ctx : decl_ctx)
    ctx0
    type_ident
    (tctx : type_renaming_ctx) =
  match type_ident with
  | TypeIdent.Struct name ->
    let fields = StructName.Map.find name decl_ctx.ctx_structs in
    let path = StructName.path name in
    let str, pos = StructName.get_info name in
    let path_ctx, ctx =
      try tctx.path_ctx, PathMap.find path tctx.path_ctx
      with PathMap.Not_found _ -> PathMap.add path ctx0 tctx.path_ctx, ctx0
    in
    let id, ctx = new_id ctx (tctx.f_struct str) in
    let new_name = StructName.fresh path (id, pos) in
    let ctx1, fields_map, ctx_fields =
      StructField.Map.fold
        (fun name ty (ctx, fields_map, ctx_fields) ->
          let str, pos = StructField.get_info name in
          let id, ctx = new_id ctx (tctx.f_field str) in
          let new_name = StructField.fresh (id, pos) in
          ( ctx,
            StructField.Map.add name new_name fields_map,
            StructField.Map.add new_name ty ctx_fields ))
        fields
        ( (if tctx.namespaced_fields_constrs then ctx0 else ctx),
          tctx.fields_map,
          StructField.Map.empty )
    in
    let ctx = if tctx.namespaced_fields_constrs then ctx else ctx1 in
    {
      tctx with
      path_ctx = PathMap.add path ctx path_ctx;
      structs_map = StructName.Map.add name new_name tctx.structs_map;
      fields_map;
      ctx_structs = StructName.Map.add new_name ctx_fields tctx.ctx_structs;
    }
  | TypeIdent.Enum name when EnumName.equal name Expr.option_enum ->
    (* The option type shouldn't be renamed, it has special handling in
       backends. FIXME: could the fact that it's special be detected differently
       from id comparison ? Structure maybe, or a more specific construct ? *)
    let constrs = EnumName.Map.find name decl_ctx.ctx_enums in
    let ctx = PathMap.find [] tctx.path_ctx in
    let ctx1, constrs_map =
      EnumConstructor.Map.fold
        (fun name _ (ctx, constrs_map) ->
          let str, _ = EnumConstructor.get_info name in
          let ctx = reserve_name ctx str in
          ctx, EnumConstructor.Map.add name name constrs_map)
        constrs
        ( (if tctx.namespaced_fields_constrs then ctx0 else ctx),
          tctx.constrs_map )
    in
    let ctx = if tctx.namespaced_fields_constrs then ctx else ctx1 in
    {
      tctx with
      path_ctx = PathMap.add [] ctx tctx.path_ctx;
      enums_map = EnumName.Map.add name name tctx.enums_map;
      constrs_map;
      ctx_enums = EnumName.Map.add name Expr.option_enum_config tctx.ctx_enums;
    }
  | TypeIdent.Enum name ->
    let constrs = EnumName.Map.find name decl_ctx.ctx_enums in
    let path = EnumName.path name in
    let str, pos = EnumName.get_info name in
    let path_ctx, ctx =
      try tctx.path_ctx, PathMap.find path tctx.path_ctx
      with PathMap.Not_found _ -> PathMap.add path ctx0 tctx.path_ctx, ctx0
    in
    let id, ctx = new_id ctx (tctx.f_enum str) in
    let new_name = EnumName.fresh path (id, pos) in
    let ctx1, constrs_map, ctx_constrs =
      EnumConstructor.Map.fold
        (fun name ty (ctx, constrs_map, ctx_constrs) ->
          let str, pos = EnumConstructor.get_info name in
          let id, ctx = new_id ctx (tctx.f_constr str) in
          let new_name = EnumConstructor.fresh (id, pos) in
          ( ctx,
            EnumConstructor.Map.add name new_name constrs_map,
            EnumConstructor.Map.add new_name ty ctx_constrs ))
        constrs
        ( (if tctx.namespaced_fields_constrs then ctx0 else ctx),
          tctx.constrs_map,
          EnumConstructor.Map.empty )
    in
    let ctx = if tctx.namespaced_fields_constrs then ctx else ctx1 in
    {
      tctx with
      path_ctx = PathMap.add path ctx path_ctx;
      enums_map = EnumName.Map.add name new_name tctx.enums_map;
      constrs_map;
      ctx_enums = EnumName.Map.add new_name ctx_constrs tctx.ctx_enums;
    }

let cap s = String.to_ascii s |> String.capitalize_ascii
let uncap s = String.to_ascii s |> String.uncapitalize_ascii

(* Todo? - handle separate namespaces ? (e.g. allow a field and var to have the
   same name for backends that support it) - register module names as reserved
   names *)
let program
    ~reserved
    ~skip_constant_binders
    ~constant_binder_name
    ~namespaced_fields_constrs
    ?(f_var = String.to_snake_case)
    ?(f_struct = cap)
    ?(f_field = uncap)
    ?(f_enum = cap)
    ?(f_constr = cap)
    p =
  let cfg =
    {
      reserved;
      sanitize_varname = f_var;
      skip_constant_binders;
      constant_binder_name;
    }
  in
  let ctx = get_ctx cfg in
  (* Each module needs its separate ctx since resolution is qualified ; and name
     resolution in a given module must be processed consistently independently
     on the current context. *)
  let type_renaming_ctx =
    {
      path_ctx = PathMap.singleton [] ctx;
      structs_map = StructName.Map.empty;
      fields_map = StructField.Map.empty;
      enums_map = EnumName.Map.empty;
      constrs_map = EnumConstructor.Map.empty;
      ctx_structs = StructName.Map.empty;
      ctx_enums = EnumName.Map.empty;
      namespaced_fields_constrs;
      f_struct;
      f_field;
      f_enum;
      f_constr;
    }
  in
  let type_renaming_ctx =
    (* We first run the renaming on public idents alone, to be sure it is not
       affected by private definitions *)
    TypeIdent.Set.fold
      (process_type_ident p.decl_ctx ctx)
      p.decl_ctx.ctx_public_types type_renaming_ctx
    (* Warning: the folding order matters here, if a module contains e.g. two
       fields with the same name. This fold relies on UIDs, and is thus
       dependent on the definition order. Another possibility would be to fold
       lexicographically, but the result would be "less intuitive" *)
  in
  let path_ctx = type_renaming_ctx.path_ctx in
  let path_ctx, scopes_map =
    ScopeName.Map.fold
      (fun name info (path_ctx, scopes_map) ->
        let path = ScopeName.path name in
        if info.visibility = Private then
          (* Private scopes / topdefs in the root module will be renamed through
             the variables binding them in the code_items. It's important that
             they don't affect the renaming of public items *)
          path_ctx, scopes_map
        else
          (* Public items need to be renamed deterministically ; in particular,
             when coming from other modules, they are referred to through their
             uids *)
          let str, pos = ScopeName.get_info name in
          let path_ctx, ctx =
            try path_ctx, PathMap.find path path_ctx
            with PathMap.Not_found _ -> PathMap.add path ctx path_ctx, ctx
          in
          let id, ctx = new_id ctx (f_var str) in
          let new_name = ScopeName.fresh path (id, pos) in
          ( PathMap.add path ctx path_ctx,
            ScopeName.Map.add name new_name scopes_map ))
      p.decl_ctx.ctx_scopes
      (path_ctx, ScopeName.Map.empty)
  in
  let path_ctx, topdefs_map, ctx_topdefs =
    TopdefName.Map.fold
      (fun name (typ, visibility) (path_ctx, topdefs_map, ctx_topdefs) ->
        let path = TopdefName.path name in
        if visibility = Private then
          (* Private scopes / topdefs in the root module will be renamed through
             the variables binding them in the code_items. *)
          ( path_ctx,
            topdefs_map,
            TopdefName.Map.add name (typ, visibility) ctx_topdefs )
          (* [typ] is rewritten later on *)
        else
          let str, pos = TopdefName.get_info name in
          let path_ctx, ctx =
            try path_ctx, PathMap.find path path_ctx
            with PathMap.Not_found _ -> PathMap.add path ctx path_ctx, ctx
          in
          let id, ctx = new_id ctx (f_var str) in
          let new_name = TopdefName.fresh path (id, pos) in
          ( PathMap.add path ctx path_ctx,
            TopdefName.Map.add name new_name topdefs_map,
            TopdefName.Map.add new_name (typ, visibility) ctx_topdefs ))
      p.decl_ctx.ctx_topdefs
      (path_ctx, TopdefName.Map.empty, TopdefName.Map.empty)
  in
  (* At this point, all public idents have been deterministically mapped. We
     proceed with the remaining typedefs *)
  let type_renaming_ctx =
    let remaining_type_ids =
      TypeIdent.Set.diff
        (StructName.Map.fold
           (fun s _ -> TypeIdent.Set.add (Struct s))
           p.decl_ctx.ctx_structs
        @@ EnumName.Map.fold
             (fun e _ -> TypeIdent.Set.add (Enum e))
             p.decl_ctx.ctx_enums TypeIdent.Set.empty)
        p.decl_ctx.ctx_public_types
    in
    TypeIdent.Set.fold
      (process_type_ident p.decl_ctx ctx)
      remaining_type_ids
      { type_renaming_ctx with path_ctx }
  in
  (* And update the scope infos; the types in the topdefs are taken care of by
     the generic rewrite of [decl_ctx] *)
  let ctx_scopes =
    ScopeName.Map.fold
      (fun name info ctx_scopes ->
        let name =
          try ScopeName.Map.find name scopes_map
          with ScopeName.Map.Not_found _ -> name
        in
        let info =
          {
            in_struct_name =
              StructName.Map.find info.in_struct_name
                type_renaming_ctx.structs_map;
            out_struct_name =
              StructName.Map.find info.out_struct_name
                type_renaming_ctx.structs_map;
            out_struct_fields =
              ScopeVar.Map.map
                (fun fld ->
                  StructField.Map.find fld type_renaming_ctx.fields_map)
                info.out_struct_fields;
            visibility = info.visibility;
          }
        in
        ScopeName.Map.add name info ctx_scopes)
      p.decl_ctx.ctx_scopes ScopeName.Map.empty
  in
  (* Note: another possibility would be to process the scope info along with the
     renamings, but in a first pass for public items, and a second for private
     ones. This would fail if e.g. a public scope depends on a private struct,
     which could actually be a feature. *)
  let ctx = PathMap.find [] path_ctx in
  let ctx =
    set_rewriters ctx
      ~scopes:(fun n ->
        Option.value ~default:n @@ ScopeName.Map.find_opt n scopes_map)
      ~topdefs:(fun n ->
        Option.value ~default:n @@ TopdefName.Map.find_opt n topdefs_map)
      ~structs:(fun n -> StructName.Map.find n type_renaming_ctx.structs_map)
      ~fields:(fun n -> StructField.Map.find n type_renaming_ctx.fields_map)
      ~enums:(fun n -> EnumName.Map.find n type_renaming_ctx.enums_map)
      ~constrs:(fun n ->
        EnumConstructor.Map.find n type_renaming_ctx.constrs_map)
  in
  let decl_ctx =
    {
      p.decl_ctx with
      ctx_enums = type_renaming_ctx.ctx_enums;
      ctx_structs = type_renaming_ctx.ctx_structs;
      ctx_scopes;
      ctx_topdefs;
    }
  in
  let decl_ctx = Program.map_decl_ctx ~f:(typ ctx) decl_ctx in
  let code_items, ctx = code_items ctx (typ ctx) p.code_items in
  { p with decl_ctx; code_items }, ctx

(* This first-class module wrapping is here to allow a polymorphic renaming
   function to be passed around *)

module type Renaming = sig
  val apply : 'e program -> 'e program * context
end

type t = (module Renaming)

let apply (module R : Renaming) = R.apply

let program
    ~reserved
    ~skip_constant_binders
    ~constant_binder_name
    ~namespaced_fields_constrs
    ?f_var
    ?f_struct
    ?f_field
    ?f_enum
    ?f_constr
    () =
  let module M = struct
    let apply p =
      program ~reserved ~skip_constant_binders ~constant_binder_name
        ~namespaced_fields_constrs ?f_var ?f_struct ?f_field ?f_enum ?f_constr p
  end in
  (module M : Renaming)

let default =
  program () ~reserved:default_config.reserved
    ~skip_constant_binders:default_config.skip_constant_binders
    ~constant_binder_name:default_config.constant_binder_name ~f_var:Fun.id
    ~f_struct:Fun.id ~f_field:Fun.id ~f_enum:Fun.id ~f_constr:Fun.id
    ~namespaced_fields_constrs:true
