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
    if i = len then name, 0
    else String.sub name 0 i, int_of_string (String.sub name i (len - i))

  let get_suffix : string -> int -> ctxt -> int * ctxt =
   fun name suffix ctxt ->
    let n = try String.Map.find name ctxt with String.Map.Not_found _ -> -1 in
    let suffix = if suffix > n then suffix else n + 1 in
    suffix, String.Map.add name suffix ctxt

  let merge_name : string -> int -> string =
   fun prefix suffix ->
    if suffix > 0 then prefix ^ string_of_int suffix else prefix

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
  reset_context_for_closed_terms : bool;
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

let unbind_in ctx ?fname b =
  let module BindCtx = (val ctx.bindCtx) in
  match fname with
  | Some fn ->
    let name = fn (Bindlib.binder_name b) in
    let v, bcontext = BindCtx.new_var_in ctx.bcontext (fun v -> EVar v) name in
    let e = Bindlib.subst b (EVar v) in
    v, e, { ctx with bcontext }
  | None ->
    let v, e, bcontext = BindCtx.unbind_in ctx.bcontext b in
    v, e, { ctx with bcontext }

let unmbind_in ctx ?fname b =
  let module BindCtx = (val ctx.bindCtx) in
  match fname with
  | Some fn ->
    let names = Array.map fn (Bindlib.mbinder_names b) in
    let rvs, bcontext =
      Array.fold_left
        (fun (rvs, bcontext) n ->
          let v, bcontext = BindCtx.new_var_in bcontext (fun v -> EVar v) n in
          v :: rvs, bcontext)
        ([], ctx.bcontext) names
    in
    let vs = Array.of_list (List.rev rvs) in
    let e = Bindlib.msubst b (Array.map (fun v -> EVar v) vs) in
    vs, e, { ctx with bcontext }
  | None ->
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

    let reset_context_for_closed_terms = cfg.reset_context_for_closed_terms
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
 fun ctx -> function
  | EExternal { name = External_scope s, pos }, m ->
    Expr.eexternal ~name:(External_scope (ctx.scopes s), pos) m
  | EExternal { name = External_value d, pos }, m ->
    Expr.eexternal ~name:(External_value (ctx.topdefs d), pos) m
  | EAbs { binder; tys }, m ->
    let vars, body, ctx = unmbind_in ctx ~fname:ctx.vars binder in
    let body = expr ctx body in
    let binder = Expr.bind vars body in
    Expr.eabs binder (List.map (typ ctx) tys) m
  | EStruct { name; fields }, m ->
    Expr.estruct ~name:(ctx.structs name)
      ~fields:
        (StructField.Map.fold
           (fun fld e -> StructField.Map.add (ctx.fields fld) (expr ctx e))
           fields StructField.Map.empty)
      m
  | EStructAccess { name; field; e }, m ->
    Expr.estructaccess ~name:(ctx.structs name) ~field:(ctx.fields field)
      ~e:(expr ctx e) m
  | EInj { name; e; cons }, m ->
    Expr.einj ~name:(ctx.enums name) ~cons:(ctx.constrs cons) ~e:(expr ctx e) m
  | EMatch { name; e; cases }, m ->
    Expr.ematch ~name:(ctx.enums name)
      ~cases:
        (EnumConstructor.Map.fold
           (fun cons e ->
             EnumConstructor.Map.add (ctx.constrs cons) (expr ctx e))
           cases EnumConstructor.Map.empty)
      ~e:(expr ctx e) m
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

let code_items ctx (scopes : 'e code_item_list) =
  let f ctx = function
    | ScopeDef (name, body) ->
      let name = scope_name ctx name in
      let scope_input_var, scope_lets, ctx =
        unbind_in ctx ~fname:String.to_snake_case body.scope_body_expr
      in
      let scope_lets = rename_vars_in_lets ctx scope_lets in
      let scope_body_expr = Bindlib.bind_var scope_input_var scope_lets in
      Bindlib.box_apply
        (fun scope_body_expr ->
          let body =
            {
              scope_body_input_struct =
                struct_name ctx body.scope_body_input_struct;
              scope_body_output_struct =
                struct_name ctx body.scope_body_output_struct;
              scope_body_expr;
            }
          in
          ScopeDef (name, body))
        scope_body_expr
    | Topdef (name, ty, e) ->
      Bindlib.box_apply
        (fun e -> Topdef (name, typ ctx ty, e))
        (Expr.Box.lift (expr ctx e))
  in
  Bindlib.unbox
  @@ boundlist_map_ctx ~ctx ~f ~fname:String.to_snake_case
       ~last:(fun _ctx -> Bindlib.box)
       scopes

let cap s = String.to_ascii s |> String.capitalize_ascii
let uncap s = String.to_ascii s |> String.uncapitalize_ascii

(* Todo? - handle separate namespaces ? (e.g. allow a field and var to have the
   same name for backends that support it) - register module names as reserved
   names *)
let program
    ~reserved
    ~reset_context_for_closed_terms
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
      reset_context_for_closed_terms;
      skip_constant_binders;
      constant_binder_name;
    }
  in
  let ctx = get_ctx cfg in
  (* Each module needs its separate ctx since resolution is qualified ; and name
     resolution in a given module must be processed consistently independently
     on the current context. *)
  let ctx0 = ctx in
  let module PathMap = Map.Make (Uid.Path) in
  let pctxmap = PathMap.singleton [] ctx in
  let pctxmap, structs_map, fields_map, ctx_structs =
    (* Warning: the folding order matters here, if a module contains e.g. two
       fields with the same name. This fold relies on UIDs, and is thus
       dependent on the definition order. Another possibility would be to fold
       lexicographically, but the result would be "less intuitive" *)
    StructName.Map.fold
      (fun name fields (pctxmap, structs_map, fields_map, ctx_structs) ->
        let path = StructName.path name in
        let str, pos = StructName.get_info name in
        let pctxmap, ctx =
          try pctxmap, PathMap.find path pctxmap
          with PathMap.Not_found _ -> PathMap.add path ctx pctxmap, ctx
        in
        let id, ctx = new_id ctx (f_struct str) in
        let new_name = StructName.fresh path (id, pos) in
        let ctx1, fields_map, ctx_fields =
          StructField.Map.fold
            (fun name ty (ctx, fields_map, ctx_fields) ->
              let str, pos = StructField.get_info name in
              let id, ctx = new_id ctx (f_field str) in
              let new_name = StructField.fresh (id, pos) in
              ( ctx,
                StructField.Map.add name new_name fields_map,
                StructField.Map.add new_name ty ctx_fields ))
            fields
            ( (if namespaced_fields_constrs then ctx0 else ctx),
              fields_map,
              StructField.Map.empty )
        in
        let ctx = if namespaced_fields_constrs then ctx else ctx1 in
        ( PathMap.add path ctx pctxmap,
          StructName.Map.add name new_name structs_map,
          fields_map,
          StructName.Map.add new_name ctx_fields ctx_structs ))
      p.decl_ctx.ctx_structs
      ( pctxmap,
        StructName.Map.empty,
        StructField.Map.empty,
        StructName.Map.empty )
  in
  let pctxmap, enums_map, constrs_map, ctx_enums =
    EnumName.Map.fold
      (fun name constrs (pctxmap, enums_map, constrs_map, ctx_enums) ->
        if EnumName.equal name Expr.option_enum then
          (* The option type shouldn't be renamed, it has special handling in
             backends. FIXME: could the fact that it's special be detected
             differently from id comparison ? Structure maybe, or a more
             specific construct ? *)
          let ctx1, constrs_map =
            EnumConstructor.Map.fold
              (fun name _ (ctx, constrs_map) ->
                 let str, _ = EnumConstructor.get_info name in
                 let ctx = reserve_name ctx str in
                 ( ctx,
                   EnumConstructor.Map.add name name constrs_map ))
              constrs
              ( (if namespaced_fields_constrs then ctx0 else ctx),
                constrs_map )
          in
          let ctx = if namespaced_fields_constrs then ctx else ctx1 in
          ( PathMap.add [] ctx pctxmap,
            EnumName.Map.add name name enums_map,
            constrs_map,
            EnumName.Map.add name Expr.option_enum_config ctx_enums )
        else
          let path = EnumName.path name in
          let str, pos = EnumName.get_info name in
          let pctxmap, ctx =
            try pctxmap, PathMap.find path pctxmap
            with Not_found -> PathMap.add path ctx pctxmap, ctx
          in
          let id, ctx = new_id ctx (f_enum str) in
          let new_name = EnumName.fresh path (id, pos) in
          let ctx1, constrs_map, ctx_constrs =
            EnumConstructor.Map.fold
              (fun name ty (ctx, constrs_map, ctx_constrs) ->
                let str, pos = EnumConstructor.get_info name in
                let id, ctx = new_id ctx (f_constr str) in
                let new_name = EnumConstructor.fresh (id, pos) in
                ( ctx,
                  EnumConstructor.Map.add name new_name constrs_map,
                  EnumConstructor.Map.add new_name ty ctx_constrs ))
              constrs
              ( (if namespaced_fields_constrs then ctx0 else ctx),
                constrs_map,
                EnumConstructor.Map.empty )
          in
          let ctx = if namespaced_fields_constrs then ctx else ctx1 in
          ( PathMap.add path ctx pctxmap,
            EnumName.Map.add name new_name enums_map,
            constrs_map,
            EnumName.Map.add new_name ctx_constrs ctx_enums ))
      p.decl_ctx.ctx_enums
      ( pctxmap,
        EnumName.Map.empty,
        EnumConstructor.Map.empty,
        EnumName.Map.empty )
  in
  let pctxmap, scopes_map, ctx_scopes =
    ScopeName.Map.fold
      (fun name info (pctxmap, scopes_map, ctx_scopes) ->
        let info =
          {
            in_struct_name = StructName.Map.find info.in_struct_name structs_map;
            out_struct_name =
              StructName.Map.find info.out_struct_name structs_map;
            out_struct_fields =
              ScopeVar.Map.map
                (fun fld -> StructField.Map.find fld fields_map)
                info.out_struct_fields;
          }
        in
        let path = ScopeName.path name in
        if path = [] then
          (* Scopes / topdefs in the root module will be renamed through the
             variables binding them in the code_items *)
          ( pctxmap,
            ScopeName.Map.add name name scopes_map,
            ScopeName.Map.add name info ctx_scopes )
        else
          let str, pos = ScopeName.get_info name in
          let pctxmap, ctx =
            try pctxmap, PathMap.find path pctxmap
            with Not_found -> PathMap.add path ctx pctxmap, ctx
          in
          let id, ctx = new_id ctx (f_var str) in
          let new_name = ScopeName.fresh path (id, pos) in
          ( PathMap.add path ctx pctxmap,
            ScopeName.Map.add name new_name scopes_map,
            ScopeName.Map.add new_name info ctx_scopes ))
      p.decl_ctx.ctx_scopes
      (pctxmap, ScopeName.Map.empty, ScopeName.Map.empty)
  in
  let pctxmap, topdefs_map, ctx_topdefs =
    TopdefName.Map.fold
      (fun name typ (pctxmap, topdefs_map, ctx_topdefs) ->
        let path = TopdefName.path name in
        if path = [] then
          (* Topdefs / topdefs in the root module will be renamed through the
             variables binding them in the code_items *)
          ( pctxmap,
            TopdefName.Map.add name name topdefs_map,
            TopdefName.Map.add name typ ctx_topdefs )
          (* [typ] is rewritten later on *)
        else
          let str, pos = TopdefName.get_info name in
          let pctxmap, ctx =
            try pctxmap, PathMap.find path pctxmap
            with Not_found -> PathMap.add path ctx pctxmap, ctx
          in
          let id, ctx = new_id ctx (f_var str) in
          let new_name = TopdefName.fresh path (id, pos) in
          ( PathMap.add path ctx pctxmap,
            TopdefName.Map.add name new_name topdefs_map,
            TopdefName.Map.add new_name typ ctx_topdefs ))
      p.decl_ctx.ctx_topdefs
      (pctxmap, TopdefName.Map.empty, TopdefName.Map.empty)
  in
  let ctx = PathMap.find [] pctxmap in
  let ctx =
    set_rewriters ctx
      ~scopes:(fun n -> ScopeName.Map.find n scopes_map)
      ~topdefs:(fun n -> TopdefName.Map.find n topdefs_map)
      ~structs:(fun n -> StructName.Map.find n structs_map)
      ~fields:(fun n -> StructField.Map.find n fields_map)
      ~enums:(fun n -> EnumName.Map.find n enums_map)
      ~constrs:(fun n -> EnumConstructor.Map.find n constrs_map)
  in
  let decl_ctx =
    { p.decl_ctx with ctx_enums; ctx_structs; ctx_scopes; ctx_topdefs }
  in
  let decl_ctx = Program.map_decl_ctx ~f:(typ ctx) decl_ctx in
  let code_items = code_items ctx p.code_items in
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
    ~reset_context_for_closed_terms
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
      program ~reserved ~reset_context_for_closed_terms ~skip_constant_binders
        ~constant_binder_name ~namespaced_fields_constrs ?f_var ?f_struct
        ?f_field ?f_enum ?f_constr p
  end in
  (module M : Renaming)
