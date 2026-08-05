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
open Shared_ast

type size_mark = (int * typed mark) custom

type 'a split_ctx = {
  split_threshold : int;
  decl_ctx : decl_ctx;
  topdefs : (lcalc, size_mark) Shared_ast__Definitions.gexpr Var.Set.t;
}

(** Bottom-up fold mapping typed marks to size marks, i.e., each node's mark
    will contain its sub-term (approximated) size. *)
let add_size (type a) : (a, typed) gexpr -> (a, size_mark) boxed_gexpr =
 fun e ->
  let location_size = 6 in
  let op_size : a operator -> int = function
    (* Traces in backends outputs a [begin] corresponding to the tag
       and its location and an [end] node with its value. *)
    | Tag (ScopeCall _) -> 3 + location_size + 2
    | Tag (FunCall _) -> 3 + location_size + 2
    | Tag (ScopeVarDef _) -> 2 + 2 + 2 + location_size + 2
    | Tag (LocalVarDef _) -> 3 + location_size + 2
    | Tag (LocalTupDef { names }) -> 2 + List.length names + location_size + 2
    | Tag BranchingCondition -> 2 + location_size + 2
    | Tag (Branching None) -> 2 + location_size + 2
    | Tag (Branching (Some _)) -> 3 + location_size + 2
    | Tag Assertion -> 2 + location_size + 2
    | Tag (Exception { label = None; _ }) -> 3 + location_size + 2
    | Tag (Exception { label = Some _; _ }) -> 4 + location_size + 2
    | DebugPrint _ | Sort _ | Add_dat_dur _ | Sub_dat_dur _ -> 2
    | _ -> 1
  in
  let rec add_size (e : (a, typed) gexpr) : int * (a, size_mark) boxed_gexpr =
    let (Typed { pos; ty = _ } as m) = Mark.get e in
    let mk_mark n : size_mark mark = Custom { pos; custom = n, m } in
    match Mark.remove e with
    | EVar _ | EExternal _ | ELit _ | EEmpty | ECustom _ | EBad | EFatalError _
      ->
      let new_m = mk_mark 1 in
      1, Expr.map_marks ~f:(fun _ -> new_m) e
    | ELocation _ | EPos _ ->
      let new_m = mk_mark location_size in
      location_size, Expr.map_marks ~f:(fun _ -> new_m) e
    | EFatalError_pos { error; pos_expr } ->
      let size_pos_expr, pos_expr = add_size pos_expr in
      let size = 2 + size_pos_expr in
      size, Expr.efatalerror_pos ~error ~pos_expr (mk_mark size)
    | ETuple args ->
      let size_args, args = List.map add_size args |> List.split in
      let size = List.fold_left ( + ) 1 size_args in
      let e' = Expr.etuple args (mk_mark size) in
      size, e'
    | EArray args ->
      let size_args, args = List.map add_size args |> List.split in
      let size = List.fold_left ( + ) 1 size_args in
      size, Expr.earray args (mk_mark size)
    | ETupleAccess { e; index; size = size_acs } ->
      let size_e, e = add_size e in
      let size = size_e + 1 in
      size, Expr.etupleaccess ~e ~index ~size:size_acs (mk_mark size)
    | EInj { e = sube; name; cons } ->
      let size_e, e = add_size sube in
      let size = size_e + 1 in
      size, Expr.einj ~e ~name ~cons (mk_mark size)
    | EAssert sube ->
      let size_e, sube = add_size sube in
      let size = size_e + 1 in
      size, Expr.eassert sube (mk_mark size)
    | EErrorOnEmpty sube ->
      let size_e, sube = add_size sube in
      let size = size_e + 1 in
      size, Expr.eerroronempty sube (mk_mark size)
    | EPureDefault sube ->
      let size_e, sube = add_size sube in
      let size = size_e + 1 in
      size, Expr.epuredefault sube (mk_mark size)
    | EApp { f; args; tys } ->
      let size_args, args = List.map add_size args |> List.split in
      let size_f, f = add_size f in
      let size = List.fold_left ( + ) 1 size_args + size_f + 1 in
      size, Expr.eapp ~f ~args ~tys (mk_mark size)
    | EAppOp { args; op; tys } ->
      let size_args, args = List.map add_size args |> List.split in
      let size_op = op_size (Mark.remove op) in
      let size = List.fold_left ( + ) (1 + size_op) size_args in
      size, Expr.eappop ~args ~op ~tys (mk_mark size)
    | EAbs { binder; pos; tys } ->
      let vars, body = Bindlib.unmbind binder in
      let size_body, body = add_size body in
      let vars = Array.map Var.translate vars in
      let binder = Expr.bind vars body in
      let size = Array.length vars + 1 + size_body in
      size, Expr.eabs binder pos tys (mk_mark size)
    | EIfThenElse { cond; etrue; efalse } ->
      let size_cond, cond = add_size cond in
      let size_etrue, etrue = add_size etrue in
      let size_efalse, efalse = add_size efalse in
      let size = 1 + size_cond + size_etrue + size_efalse in
      size, Expr.eifthenelse cond etrue efalse (mk_mark size)
    | EDefault { excepts; just; cons } ->
      let size_excepts, excepts =
        List.map add_size excepts
        |> List.split
        |> fun (a, b) -> List.fold_left ( + ) 0 a, b
      in
      let size_just, just = add_size just in
      let size_cons, cons = add_size cons in
      let size = size_excepts + size_just + size_cons + 1 in
      size, Expr.edefault ~excepts ~just ~cons (mk_mark size)
    | EStruct { name; fields } ->
      let size_fields, fields =
        let size = ref 0 in
        let r =
          StructField.Map.map
            (fun e ->
              let size_e, e = add_size e in
              size := !size + 1 + size_e;
              e)
            fields
        in
        !size, r
      in
      let size = 1 + size_fields in
      size, Expr.estruct ~name ~fields (mk_mark size)
    | EDStructAmend { e; fields; name_opt } ->
      let size_fields, fields =
        let size = ref 0 in
        let r =
          MarkedIdent.Map.map
            (fun e ->
              let size_e, e = add_size e in
              size := !size + 1 + size_e;
              e)
            fields
        in
        !size, r
      in
      let size_e, e = add_size e in
      let size = 1 + size_e + size_fields in
      size, Expr.edstructamend ~e ~fields ~name_opt (mk_mark size)
    | EDStructAccess { e; name_opt; field } ->
      let size_e, e = add_size e in
      let size = 1 + size_e in
      size, Expr.edstructaccess ~e ~name_opt ~field (mk_mark size)
    | EStructAccess { e; name; field } ->
      let size_e, e = add_size e in
      let size = 1 + size_e in
      size, Expr.estructaccess ~e ~name ~field (mk_mark size)
    | EMatch { e; cases; name } ->
      let size_cases, cases =
        let size = ref 0 in
        let r =
          EnumConstructor.Map.map
            (fun e ->
              let size_e, e = add_size e in
              size := !size + 1 + size_e;
              e)
            cases
        in
        !size, r
      in
      let size_e, e = add_size e in
      let size = size_cases + size_e in
      size, Expr.ematch ~e ~cases ~name (mk_mark size)
    | EScopeCall { args; scope } ->
      let size_args, args =
        let size = ref 0 in
        let r =
          ScopeVar.Map.map
            (fun (x, e) ->
              let size_e, e = add_size e in
              size := !size + 1 + size_e;
              x, e)
            args
        in
        !size, r
      in
      let size = 1 + size_args in
      size, Expr.escopecall ~scope ~args (mk_mark size)
  in
  snd (add_size e)

let remove_size e =
  Expr.map_marks
    ~f:(function Custom { custom = _size, ty_m; pos = _ } -> ty_m)
    e

let get_size (e : (_, size_mark mark) Mark.ed) =
  let (Custom { custom = size, _m; _ }) = Mark.get e in
  size

let size_mark_ty
    (Custom { custom = _size, Typed { ty; _ }; _ } : size_mark mark) =
  ty

let update_size_mark_ty (Custom { custom = size, Typed m; pos }) ty =
  Custom { custom = size, Typed { m with ty }; pos }

let update_mark_size
    new_size
    (Custom { custom = _size, m; pos } : size_mark mark) : size_mark mark =
  Custom { custom = new_size, m; pos }

(** Convert the given expression into a topdef parametrized by its free
    variables and a call to this function with the appropriate arguments. *)
let split_expression (ctx : 'a split_ctx) (e : (_, size_mark) gexpr) =
  let fv : (lcalc, size_mark) gexpr Var.t = Var.make "compute_chunk" in
  let free_vars =
    Expr.free_vars_marked e
    |> Var.Map.filter (fun v _m ->
        not (Var.Set.mem (Var.translate v) ctx.topdefs))
    |> Var.Map.bindings
  in
  let fname = TopdefName.fresh [] (Bindlib.name_of fv, Pos.void) in
  let orig_mark = Mark.get e in
  let (free_vars_e : _ boxed_gexpr list), free_vars_typs =
    List.map (fun (v, m) -> Expr.evar v m, size_mark_ty m) free_vars
    |> List.split
  in
  let f = Expr.evar fv (update_mark_size 1 orig_mark) in
  let ret_ty = Type.arrow_return (size_mark_ty orig_mark) in
  let free_vars_v = Array.of_list (List.map fst free_vars) in
  let topdef_ty = TArrow (free_vars_typs, ret_ty), Pos.void in
  let topdef_mark =
    update_size_mark_ty orig_mark topdef_ty
    |> update_mark_size (1 + List.length free_vars_e + get_size e)
  in
  let binder = Expr.bind free_vars_v (Expr.rebox e) in
  let call_mark = update_mark_size (2 + List.length free_vars) orig_mark in
  let ecall = Expr.eapp ~f ~args:free_vars_e ~tys:free_vars_typs call_mark in
  let topdef_abs =
    Expr.eabs binder (List.map Expr.pos free_vars) free_vars_typs topdef_mark
  in
  let topdef_e =
    fname, topdef_ty, Private, remove_size (Expr.unbox topdef_abs)
  in
  let decl_ctx =
    {
      ctx.decl_ctx with
      ctx_topdefs =
        TopdefName.Map.add fname (topdef_ty, Private) ctx.decl_ctx.ctx_topdefs;
    }
  in
  let fv = Var.translate fv in
  let ctx : 'a split_ctx =
    { ctx with decl_ctx; topdefs = Var.Set.add fv ctx.topdefs }
  in
  (fv, topdef_e, ctx), ecall

(** Partition the given array in two: the result is an array concatenation with
    the left argument being a call to a topdef building a sub-array with
    elements that fits in the [split_threshold] argument. The right argument is
    the remaining array left unchanged. This function also returns the generated
    topdef. *)
let split_array (ctx : 'a split_ctx) (e : (_, size_mark) gexpr) =
  let rec split_until_full (curr_size, acc) = function
    | [] -> assert false
    | h :: t ->
      let new_size = curr_size + get_size h in
      if new_size > ctx.split_threshold then (List.rev acc, h :: t), curr_size
      else split_until_full (new_size, h :: acc) t
  in
  let orig_mark = Mark.get e in
  let elts =
    match Mark.remove e with EArray elts -> elts | _ -> assert false
  in
  let (l, r), left_size = split_until_full (0, []) elts in
  let fv = Var.make "compute_subarray" in
  let free_l_vars =
    List.fold_left
      (fun m e ->
        Var.Map.union (fun _ l _ -> Some l) m (Expr.free_vars_marked e))
      Var.Map.empty l
    |> Var.Map.filter (fun v _ ->
        (* Assumes topdefs are reachable *)
        not (Var.Set.mem (Var.translate v) ctx.topdefs))
    |> Var.Map.bindings
  in
  let fname = TopdefName.fresh [] (Bindlib.name_of fv, Pos.void) in
  let l_mark = update_mark_size (1 + left_size) orig_mark in
  let right_size = get_size e - left_size - 1 in
  let r_mark = update_mark_size (1 + right_size) orig_mark in
  let call_mark = update_mark_size (1 + List.length free_l_vars) orig_mark in
  let free_vars_e, free_vars_typs =
    List.map (fun (v, m) -> Expr.evar v m, size_mark_ty m) free_l_vars
    |> List.split
  in
  let f = Expr.evar fv (update_mark_size 1 orig_mark) in
  let ecall = Expr.eapp ~f ~args:free_vars_e ~tys:free_vars_typs call_mark in
  let topdef_ty = TArrow (free_vars_typs, size_mark_ty orig_mark), Pos.void in
  let binder =
    Expr.bind
      (Array.of_list (List.map fst free_l_vars))
      (Expr.earray (List.map Expr.rebox l) l_mark)
  in
  let topdef_mark = update_size_mark_ty orig_mark topdef_ty in
  let topdef_abs =
    Expr.eabs binder (List.map Expr.pos free_l_vars) free_vars_typs topdef_mark
  in
  let topdef_e =
    fname, topdef_ty, Private, remove_size (Expr.unbox topdef_abs)
  in
  let decl_ctx =
    {
      ctx.decl_ctx with
      ctx_topdefs =
        TopdefName.Map.add fname (topdef_ty, Private) ctx.decl_ctx.ctx_topdefs;
    }
  in
  let ctx =
    { ctx with decl_ctx; topdefs = Var.Set.add (Var.translate fv) ctx.topdefs }
  in
  let right_array = Expr.earray (List.map Expr.rebox r) r_mark in
  let concat_mark =
    update_mark_size (2 + get_size ecall + get_size right_array) orig_mark
  in
  let array_concat =
    Expr.eappop ~op:(Op.Concat, Pos.void) ~args:[ecall; right_array]
      ~tys:[size_mark_ty l_mark; size_mark_ty r_mark]
      concat_mark
  in
  (fv, topdef_e, ctx), array_concat

let is_small_enough ctx e = get_size e <= ctx.split_threshold
let is_too_large ctx e = not (is_small_enough ctx e)

(** Handles non-splittable arguments, e.g., call arguments, tuple, etc. The
    heuristic is: if one of the argument is too large (w.r.t to the
    [split_threshold]), we split it, otherwise, we split the largest element. *)
let rec handle_args ctx args =
  let r, rev_args =
    List.fold_left
      (function
        | (Some _ as r), rev_args -> fun e -> r, Expr.rebox e :: rev_args
        | None, rev_args ->
          fun e ->
            if is_too_large ctx e then
              let r, e = find_split_candidate ctx e in
              r, e :: rev_args
            else None, Expr.rebox e :: rev_args)
      (None, []) args
  in
  match r with
  | Some _ as r -> r, List.rev rev_args
  | None ->
    (* We cannot split args => reduce the largest arg *)
    let sizes = List.mapi (fun i arg -> i, get_size arg) args in
    let i_max, _ =
      List.fold_left
        (fun ((_, max_size) as acc) (i, size) ->
          if max_size < size then i, size else acc)
        (-1, 0) sizes
    in
    let all_r, args =
      List.mapi
        (fun i arg ->
          if i = i_max then
            let r, ecall = find_split_candidate ctx arg in
            r, ecall
          else None, Expr.(rebox arg))
        args
      |> List.split
    in
    let r =
      List.fold_left
        (function None -> fun r -> r | acc -> fun _ -> acc)
        None all_r
    in
    r, args

(** Handles arrays: if one of the element is too large (w.r.t to the
    [split_threshold]), we split it. Otherwise, we divide it using
    [split_array]. *)
and handle_arrays (ctx : 'a split_ctx) e =
  let rec find_and_rewrite_too_large_elt acc = function
    | [] -> None
    | h :: t ->
      if is_too_large ctx h then
        let r, f = find_split_candidate ctx h in
        Some (r, List.rev_append (f :: acc) (List.map Expr.rebox t))
      else find_and_rewrite_too_large_elt (Expr.rebox h :: acc) t
  in
  match Mark.remove e with
  | EArray l -> (
    match find_and_rewrite_too_large_elt [] l with
    | Some (r, args) ->
      let size_elts = List.map get_size args |> List.fold_left ( + ) 0 in
      r, Expr.earray args (update_mark_size (1 + size_elts) (Mark.get e))
    | None ->
      let (fv, topdef_e, ctx), array_concat = split_array ctx e in
      Some (fv, topdef_e, ctx), array_concat)
  | _ -> assert false

(** Iterates over the AST to look for a good split candidate, i.e., an ast node
    small enough to be split into a topdef. The AST is dynamically resized
    depending on the hoisted expression avoiding extra AST traversals. *)
and find_split_candidate (ctx : _ split_ctx) (e : (_, size_mark) gexpr) =
  let is_abs = function EAbs _, _ -> true | _ -> false in
  if is_small_enough ctx e && not (is_abs e)
  (* We void splitting on abstraction. It yields weird code
     construction. *)
  then
    let (fv, topdef, ctx), ecall = split_expression ctx e in
    Some (fv, topdef, ctx), ecall
  else
    let m = Mark.get e in
    let add_to_mark_size node_size new_e m =
      update_mark_size (node_size + get_size new_e) m
    in
    let add_all_to_mark_size node_size new_args =
      let size_args = List.fold_left ( + ) 0 (List.map get_size new_args) in
      update_mark_size (node_size + size_args) m
    in
    (* Current node is too large *)
    match Mark.remove e with
    | EBad | EPos _ | ELocation _ | ELit _ | EVar _ | EExternal _
    | EFatalError_pos _ ->
      None, Expr.rebox e
    | EApp { f; args; tys } ->
      let size_args = List.fold_left (fun s arg -> get_size arg + s) 0 args in
      if get_size f >= size_args then
        let r, f = find_split_candidate ctx f in
        ( r,
          Expr.eapp ~f ~args:(List.map Expr.rebox args) ~tys
            (add_to_mark_size (1 + size_args) f m) )
      else
        let r, args = handle_args ctx args in
        ( r,
          Expr.eapp ~f:(Expr.rebox f) ~args ~tys
            (add_all_to_mark_size (1 + get_size f) args) )
    | EAppOp { op; tys; args } ->
      let r, args = handle_args ctx args in
      r, Expr.eappop ~op ~args ~tys (add_all_to_mark_size 2 args)
    | EArray _elts ->
      let r, e = handle_arrays ctx e in
      (* Mark already resized in [handle_arrays] *)
      r, e
    | ETuple args ->
      let r, args = handle_args ctx args in
      r, Expr.etuple args (add_all_to_mark_size 1 args)
    | EAbs { binder; pos; tys } ->
      let vars, body = Bindlib.unmbind binder in
      let r, body = find_split_candidate ctx body in
      let binder = Expr.bind vars body in
      ( r,
        Expr.eabs binder pos tys
          (add_to_mark_size (1 + Array.length vars) body m) )
    | EIfThenElse { cond; etrue; efalse } -> (
      match handle_args ctx [cond; etrue; efalse] with
      | ret, ([cond; etrue; efalse] as l) ->
        ret, Expr.eifthenelse cond etrue efalse (add_all_to_mark_size 1 l)
      | _ -> assert false)
    | ETupleAccess { e; index; size } ->
      let r, e = find_split_candidate ctx e in
      r, Expr.etupleaccess ~e ~index ~size (add_to_mark_size 1 e m)
    | EInj { name; cons; e } ->
      let r, e = find_split_candidate ctx e in
      r, Expr.einj ~name ~cons ~e (add_to_mark_size 1 e m)
    | EStruct { name; fields } ->
      let fields, l_e = StructField.Map.bindings fields |> List.split in
      let r, l_e = handle_args ctx l_e in
      let fields = List.combine fields l_e |> StructField.Map.of_list in
      let bdgs = StructField.Map.bindings fields in
      ( r,
        Expr.estruct ~name ~fields
          (add_all_to_mark_size (1 + List.length bdgs) (List.map snd bdgs)) )
    | EStructAccess { name; field; e } ->
      let r, e = find_split_candidate ctx e in
      r, Expr.estructaccess ~name ~field ~e (add_to_mark_size 1 e m)
    | EMatch { name; e; cases } ->
      let size_cases =
        EnumConstructor.Map.fold (fun _ arg s -> get_size arg + s) cases 0
      in
      let r, e =
        if get_size e >= size_cases then
          let r, e = find_split_candidate ctx e in
          let cases = EnumConstructor.Map.map Expr.rebox cases in
          let bdgs = EnumConstructor.Map.bindings cases in
          ( r,
            Expr.ematch ~name ~e ~cases
              (add_all_to_mark_size
                 (1 + get_size e + List.length bdgs)
                 (List.map snd bdgs)) )
        else
          let constrs, l_e = EnumConstructor.Map.bindings cases |> List.split in
          let r, l_e = handle_args ctx l_e in
          let cases = List.combine constrs l_e |> EnumConstructor.Map.of_list in
          ( r,
            Expr.ematch ~name ~e:(Expr.rebox e) ~cases
              (add_all_to_mark_size (1 + get_size e + List.length l_e) l_e) )
      in
      r, e

(** Fixpoint expression split. The returns the new expression and the hoisted
    topdefs. *)
let rec split_expression ctx rev_topdefs (e : (_, size_mark) gexpr) =
  if is_too_large ctx e then (
    let curr_size = get_size e in
    let r, e = find_split_candidate ctx e in
    let new_size = get_size e in
    if curr_size <= new_size then
      Message.error ~internal:true
        "Split expression is not smaller than the original expression";
    match r with
    | None -> Message.error ~internal:true "Could not split expression"
    | Some (fv, (fname, topdef_ty, vis, topdef_abs), ctx) ->
      let new_acc = (fv, fname, topdef_ty, vis, topdef_abs) :: rev_topdefs in
      let b =
        Bindlib.box_apply (split_expression ctx new_acc) (Expr.Box.lift e)
      in
      Bindlib.unbox b)
  else ctx, rev_topdefs, e

(* See the mli *)
let split_program ~threshold (p : 'm program) : 'm program =
  let ctx =
    {
      split_threshold = threshold;
      decl_ctx = p.decl_ctx;
      topdefs = Var.Set.empty;
    }
  in
  let rec split_code_items ctx = function
    | Last exports ->
      let exports : _ code_export Bindlib.box list =
        List.map
          (fun (ex, e) ->
            Bindlib.box_apply (fun e -> ex, e) Expr.(Box.lift (rebox e)))
          exports
      in
      ctx, Bindlib.box_apply (fun x -> Last x) (Bindlib.box_list exports)
    | Cons (ScopeDef (sname, body), next_bind) ->
      let scope_var, scope_body_expr = Bindlib.unbind body.scope_body_expr in
      let (ctx, rev_topdefs), new_scope_body_expr =
        BoundList.fold_map ~init:(ctx, [])
          ~last:(fun (ctx, rev_topdefs) last_e ->
            (ctx, rev_topdefs), Expr.Box.lift (Expr.rebox last_e))
          ~f:(fun (ctx, rev_topdefs) var let_e ->
            let e = Expr.unbox (add_size let_e.scope_let_expr) in
            let new_ctx, new_rev_topdefs, new_scope_let_expr =
              split_expression ctx rev_topdefs e
            in
            ( (new_ctx, new_rev_topdefs),
              var,
              Bindlib.box_apply
                (fun scope_let_expr -> { let_e with scope_let_expr })
                (Expr.Box.lift (remove_size new_scope_let_expr)) ))
          scope_body_expr
      in
      let prefix_topdefs :
          (_ Bindlib.var * (_, typed) gexpr code_item Bindlib.box) list =
        List.rev_map
          (fun (fv, fname, topdef_ty, vis, topdef_abs) ->
            ( Var.translate fv,
              Bindlib.box_apply
                (fun topdef_abs -> Topdef (fname, topdef_ty, vis, topdef_abs))
                (Expr.Box.lift topdef_abs) ))
          rev_topdefs
      in
      let new_scope_body_expr =
        Bindlib.bind_var scope_var new_scope_body_expr
      in
      let fv, next = Bindlib.unbind next_bind in
      let new_item :
          (lcalc, typed) gexpr Var.t
          * (lcalc, typed) gexpr code_item Bindlib.box =
        ( Var.translate fv,
          Bindlib.box_apply
            (fun new_scope_body_expr ->
              ScopeDef
                (sname, { body with scope_body_expr = new_scope_body_expr }))
            new_scope_body_expr )
      in
      let ctx, next = split_code_items ctx next in
      let prefix_boundlist =
        List.fold_right
          (fun (v, topdef) acc -> BoundList.cons (Var.translate v) topdef acc)
          (prefix_topdefs @ [new_item])
          next
      in
      ctx, prefix_boundlist
    | Cons (Topdef (name, typ, vis, e), next_bind) ->
      let ctx, rev_topdefs, new_e =
        split_expression ctx [] (Expr.unbox (add_size e))
      in
      let prefix_topdefs : (_ Bindlib.var * _ gexpr code_item Bindlib.box) list
          =
        List.rev_map
          (fun (fv, fname, topdef_ty, vis, topdef_abs) ->
            ( Var.translate fv,
              Bindlib.box_apply
                (fun topdef_abs -> Topdef (fname, topdef_ty, vis, topdef_abs))
                (Expr.Box.lift topdef_abs) ))
          rev_topdefs
      in
      let fv, next = Bindlib.unbind next_bind in
      let new_item =
        ( fv,
          Bindlib.box_apply
            (fun new_e -> Topdef (name, typ, vis, new_e))
            (Expr.Box.lift (remove_size new_e)) )
      in
      let ctx =
        { ctx with topdefs = Var.Set.add (Var.translate fv) ctx.topdefs }
      in
      let ctx, next = split_code_items ctx next in
      let prefix_boundlist =
        List.fold_right
          (fun (v, topdef) acc -> BoundList.cons v topdef acc)
          (prefix_topdefs @ [new_item])
          next
      in
      ctx, prefix_boundlist
  in
  let ctx, code_items = split_code_items ctx p.code_items in
  { p with decl_ctx = ctx.decl_ctx; code_items = Bindlib.unbox code_items }
