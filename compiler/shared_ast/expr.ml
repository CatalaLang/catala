(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

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

(** Functions handling the types of [shared_ast] *)

(* Basic block constructors *)

module Box = struct
  module B = Bindlib

  let app0 x mark = B.box x, mark
  let app1 (xb, m) f mark = B.box_apply (fun x -> f (x, m)) xb, mark

  let app2 (xb1, m1) (xb2, m2) f mark =
    B.box_apply2 (fun x1 x2 -> f (x1, m1) (x2, m2)) xb1 xb2, mark

  let app3 (xb1, m1) (xb2, m2) (xb3, m3) f mark =
    ( B.box_apply3 (fun x1 x2 x3 -> f (x1, m1) (x2, m2) (x3, m3)) xb1 xb2 xb3,
      mark )

  let appn xmbl f mark =
    let xbl, ml = List.split xmbl in
    B.box_apply (fun xl -> f (List.combine xl ml)) (B.box_list xbl), mark

  let app1n (xb0, m0) xmbl f mark =
    let xbl, ml = List.split xmbl in
    ( B.box_apply2
        (fun x0 xl -> f (x0, m0) (List.combine xl ml))
        xb0 (B.box_list xbl),
      mark )

  let app2n (xb0, m0) (xb1, m1) xmbl f mark =
    let xbl, ml = List.split xmbl in
    ( B.box_apply3
        (fun x0 x1 xl -> f (x0, m0) (x1, m1) (List.combine xl ml))
        xb0 xb1 (B.box_list xbl),
      mark )

  let lift : ('a, 't) boxed_gexpr -> ('a, 't) gexpr B.box =
   fun em -> B.box_apply (fun e -> Mark.add (Mark.get em) e) (Mark.remove em)

  module LiftIdentMap = Bindlib.Lift (Ident.Map)

  let lift_identmap = LiftIdentMap.lift_box

  module LiftStruct = Bindlib.Lift (StructField.Map)

  let lift_struct = LiftStruct.lift_box

  module LiftEnum = Bindlib.Lift (EnumConstructor.Map)

  let lift_enum = LiftEnum.lift_box

  module LiftScopeVars = Bindlib.Lift (ScopeVar.Map)

  let lift_scope_vars = LiftScopeVars.lift_box

  let rec lift_scope_call_args (args : ('a, 'm) boxed_gexpr scope_call_args) :
      ('a, 'm) gexpr scope_call_args Bindlib.box =
    lift_scope_vars
      (ScopeVar.Map.map
         (fun (arg : ('a, 'm) boxed_gexpr scope_call_arg) ->
           match arg with
           | ScopeVarArg e ->
             Bindlib.box_apply (fun e -> ScopeVarArg e) (lift e)
           | SubScopeVarArg (sub_scope_uid, sub_args) ->
             Bindlib.box_apply
               (fun sub_args -> SubScopeVarArg (sub_scope_uid, sub_args))
               (lift_scope_call_args sub_args))
         args)

  module Ren = struct
    module Set = Set.Make (String)

    type ctxt = Set.t

    let skip_constant_binders = true
    let reset_context_for_closed_terms = true
    let constant_binder_name = None
    let empty_ctxt = Set.empty
    let reserve_name n s = Set.add n s
    let new_name n s = n, Set.add n s
  end

  module Ctx = Bindlib.Ctxt (Ren)

  let fv b = Ren.Set.elements (Ctx.free_vars b)

  let assert_closed b =
    match fv b with
    | [] -> ()
    | [h] ->
      Message.error ~internal:true
        "The boxed term is not closed the variable %s is free in the global \
         context"
        h
    | l ->
      Message.error ~internal:true
        "The boxed term is not closed the variables %a is free in the global \
         context"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           Format.pp_print_string)
        l
end

let bind vars e = Bindlib.bind_mvar vars (Box.lift e)

let subst binder vars =
  Bindlib.msubst binder (Array.of_list (List.map Mark.remove vars))

let evar v mark = Mark.add mark (Bindlib.box_var v)
let eexternal ~name mark = Mark.add mark (Bindlib.box (EExternal { name }))
let etuple args = Box.appn args @@ fun args -> ETuple args

let etupleaccess ~e ~index ~size =
  assert (size = 0 || index < size);
  Box.app1 e @@ fun e -> ETupleAccess { e; index; size }

let earray args = Box.appn args @@ fun args -> EArray args
let elit l mark = Mark.add mark (Bindlib.box (ELit l))

let eabs binder tys mark =
  Bindlib.box_apply (fun binder -> EAbs { binder; tys }) binder, mark

let eapp ~f ~args ~tys = Box.app1n f args @@ fun f args -> EApp { f; args; tys }
let eassert e1 = Box.app1 e1 @@ fun e1 -> EAssert e1
let efatalerror e1 = Box.app0 @@ EFatalError e1

let eappop ~op ~args ~tys =
  Box.appn args @@ fun args -> EAppOp { op; args; tys }

let edefault ~excepts ~just ~cons =
  Box.app2n just cons excepts
  @@ fun just cons excepts -> EDefault { excepts; just; cons }

let epuredefault e = Box.app1 e @@ fun e1 -> EPureDefault e1

let eifthenelse cond etrue efalse =
  Box.app3 cond etrue efalse
  @@ fun cond etrue efalse -> EIfThenElse { cond; etrue; efalse }

let eerroronempty e1 = Box.app1 e1 @@ fun e1 -> EErrorOnEmpty e1
let eempty mark = Mark.add mark (Bindlib.box EEmpty)

let ecustom obj targs tret mark =
  Mark.add mark (Bindlib.box (ECustom { obj; targs; tret }))

let elocation loc = Box.app0 @@ ELocation loc

let estruct ~name ~(fields : ('a, 't) boxed_gexpr StructField.Map.t) mark =
  Mark.add mark
  @@ Bindlib.box_apply
       (fun fields -> EStruct { name; fields })
       (Box.lift_struct (StructField.Map.map Box.lift fields))

let edstructamend ~name_opt ~e ~(fields : ('a, 't) boxed_gexpr Ident.Map.t) mark
    =
  Mark.add mark
  @@ Bindlib.box_apply2
       (fun e fields -> EDStructAmend { name_opt; e; fields })
       (Box.lift e)
       (Box.lift_identmap (Ident.Map.map Box.lift fields))

let edstructaccess ~name_opt ~field ~e =
  Box.app1 e @@ fun e -> EDStructAccess { name_opt; field; e }

let estructaccess ~name ~field ~e =
  Box.app1 e @@ fun e -> EStructAccess { name; field; e }

let einj ~name ~cons ~e = Box.app1 e @@ fun e -> EInj { name; cons; e }

let ematch ~name ~e ~cases mark =
  Mark.add mark
  @@ Bindlib.box_apply2
       (fun e cases -> EMatch { name; e; cases })
       (Box.lift e)
       (Box.lift_enum (EnumConstructor.Map.map Box.lift cases))

let escopecall ~scope ~(args : ('a, 'm) boxed_gexpr scope_call_args) mark =
  Mark.add mark
  @@ Bindlib.box_apply
       (fun args -> EScopeCall { scope; args })
       (Box.lift_scope_call_args args)

(* - Manipulation of marks - *)

let no_mark : type m. m mark -> m mark = function
  | Untyped _ -> Untyped { pos = Pos.no_pos }
  | Typed _ -> Typed { pos = Pos.no_pos; ty = Mark.add Pos.no_pos TAny }
  | Custom { custom; pos = _ } -> Custom { pos = Pos.no_pos; custom }

let mark_pos (type m) (m : m mark) : Pos.t =
  match m with Untyped { pos } | Typed { pos; _ } | Custom { pos; _ } -> pos

let pos (type m) (x : ('a, m) marked) : Pos.t = mark_pos (Mark.get x)
let ty (_, m) : typ = match m with Typed { ty; _ } -> ty

let set_ty (type m) (ty : typ) (x : ('a, m) marked) : ('a, typed) marked =
  Mark.add
    (match Mark.get x with
    | Untyped { pos } -> Typed { pos; ty }
    | Typed m -> Typed { m with ty }
    | Custom { pos; _ } -> Typed { pos; ty })
    (Mark.remove x)

let map_mark (type m) (pos_f : Pos.t -> Pos.t) (ty_f : typ -> typ) (m : m mark)
    : m mark =
  match m with
  | Untyped { pos } -> Untyped { pos = pos_f pos }
  | Typed { pos; ty } -> Typed { pos = pos_f pos; ty = ty_f ty }
  | Custom { pos; custom } -> Custom { pos = pos_f pos; custom }

let map_mark2
    (type m)
    (pos_f : Pos.t -> Pos.t -> Pos.t)
    (ty_f : typed -> typed -> typ)
    (m1 : m mark)
    (m2 : m mark) : m mark =
  match m1, m2 with
  | Untyped m1, Untyped m2 -> Untyped { pos = pos_f m1.pos m2.pos }
  | Typed m1, Typed m2 -> Typed { pos = pos_f m1.pos m2.pos; ty = ty_f m1 m2 }
  | Custom _, Custom _ -> invalid_arg "map_mark2"

let fold_marks
    (type m)
    (pos_f : Pos.t list -> Pos.t)
    (ty_f : typed list -> typ)
    (ms : m mark list) : m mark =
  match ms with
  | [] -> invalid_arg "Dcalc.Ast.fold_mark"
  | Untyped _ :: _ as ms ->
    Untyped { pos = pos_f (List.map (function Untyped { pos } -> pos) ms) }
  | Typed _ :: _ as ms ->
    Typed
      {
        pos = pos_f (List.map (function Typed { pos; _ } -> pos) ms);
        ty = ty_f (List.map (function Typed m -> m) ms);
      }
  | Custom _ :: _ -> invalid_arg "map_mark2"

let with_pos (type m) (pos : Pos.t) (m : m mark) : m mark =
  map_mark (fun _ -> pos) (fun ty -> ty) m

let map_ty (type m) (ty_f : typ -> typ) (m : m mark) : m mark =
  map_mark (fun pos -> pos) ty_f m

let with_ty (type m) (m : m mark) ?pos (ty : typ) : m mark =
  map_mark (fun default -> Option.value pos ~default) (fun _ -> ty) m

let maybe_ty (type m) ?(typ = TAny) (m : m mark) : typ =
  match m with
  | Untyped { pos } | Custom { pos; _ } -> Mark.add pos typ
  | Typed { ty; _ } -> ty

let untyped = Untyped { pos = Pos.no_pos }
let typed = Typed { pos = Pos.no_pos; ty = TLit TUnit, Pos.no_pos }

(* - Predefined types (option) - *)

let option_enum = EnumName.fresh [] ("Eoption", Pos.no_pos)

(* Warning: order of these definitions is important, binary injection assumes
   that None is first *)
let none_constr = EnumConstructor.fresh ("ENone", Pos.no_pos)
let some_constr = EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config =
  EnumConstructor.Map.of_list
    [none_constr, (TLit TUnit, Pos.no_pos); some_constr, (TAny, Pos.no_pos)]

let pos_to_runtime pos =
  {
    Runtime.filename = Pos.get_file pos;
    start_line = Pos.get_start_line pos;
    start_column = Pos.get_start_column pos;
    end_line = Pos.get_end_line pos;
    end_column = Pos.get_end_column pos;
    law_headings = Pos.get_law_info pos;
  }

let runtime_to_pos rpos =
  let pos =
    let open Runtime in
    Pos.from_info rpos.filename rpos.start_line rpos.start_column rpos.end_line
      rpos.end_column
  in
  Pos.overwrite_law_info pos rpos.law_headings

(* - Traversal functions - *)

(* shallow map *)
let map
    (type a b)
    ?(typ : typ -> typ = Fun.id)
    ?op:(fop =
        (fun _ -> invalid_arg "Expr.map"
          : a Operator.t Mark.pos -> b Operator.t Mark.pos))
    ~(f : (a, 'm1) gexpr -> (b, 'm2) boxed_gexpr)
    (e : ((a, b, 'm1) base_gexpr, 'm2) marked) : (b, 'm2) boxed_gexpr =
  let m = map_ty typ (Mark.get e) in
  match Mark.remove e with
  | ELit l -> elit l m
  | EApp { f = e1; args; tys } ->
    eapp ~f:(f e1) ~args:(List.map f args) ~tys:(List.map typ tys) m
  | EAppOp { op; tys; args } ->
    eappop ~op:(fop op) ~tys:(List.map typ tys) ~args:(List.map f args) m
  | EArray args -> earray (List.map f args) m
  | EVar v -> evar (Var.translate v) m
  | EExternal { name } -> eexternal ~name m
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let body = f body in
    let binder = bind (Array.map Var.translate vars) body in
    let tys = List.map typ tys in
    eabs binder tys m
  | EIfThenElse { cond; etrue; efalse } ->
    eifthenelse (f cond) (f etrue) (f efalse) m
  | ETuple args -> etuple (List.map f args) m
  | ETupleAccess { e; index; size } -> etupleaccess ~e:(f e) ~index ~size m
  | EInj { name; cons; e } -> einj ~name ~cons ~e:(f e) m
  | EAssert e1 -> eassert (f e1) m
  | EFatalError e1 -> efatalerror e1 m
  | EDefault { excepts; just; cons } ->
    edefault ~excepts:(List.map f excepts) ~just:(f just) ~cons:(f cons) m
  | EPureDefault e1 -> epuredefault (f e1) m
  | EEmpty -> eempty m
  | EErrorOnEmpty e1 -> eerroronempty (f e1) m
  | ELocation loc -> elocation loc m
  | EStruct { name; fields } ->
    let fields = StructField.Map.map f fields in
    estruct ~name ~fields m
  | EDStructAmend { name_opt; e; fields } ->
    let fields = Ident.Map.map f fields in
    edstructamend ~name_opt ~e:(f e) ~fields m
  | EDStructAccess { name_opt; field; e } ->
    edstructaccess ~name_opt ~field ~e:(f e) m
  | EStructAccess { name; field; e } -> estructaccess ~name ~field ~e:(f e) m
  | EMatch { name; e; cases } ->
    let cases = EnumConstructor.Map.map f cases in
    ematch ~name ~e:(f e) ~cases m
  | EScopeCall { scope; args } ->
    let rec map_scope_call_args args =
      ScopeVar.Map.map
        (fun arg ->
          match arg with
          | ScopeVarArg e -> ScopeVarArg (f e)
          | SubScopeVarArg (sub_scope_uid, sub_args) ->
            SubScopeVarArg (sub_scope_uid, map_scope_call_args sub_args))
        args
    in
    let args = map_scope_call_args args in
    escopecall ~scope ~args m
  | ECustom { obj; targs; tret } ->
    ecustom obj (List.map typ targs) (typ tret) m

let rec map_top_down ~f e = map ~f:(map_top_down ~f) ~op:Fun.id (f e)
let map_marks ~f e = map_top_down ~f:(Mark.map_mark f) e

(* Folds the given function on the direct children of the given expression. *)
let shallow_fold
    (type a)
    (f : (a, 'm) gexpr -> 'acc -> 'acc)
    (e : (a, 'm) gexpr)
    (acc : 'acc) : 'acc =
  let lfold x acc = List.fold_left (fun acc x -> f x acc) acc x in
  match Mark.remove e with
  | ELit _ | EVar _ | EFatalError _ | EExternal _ | ELocation _ | EEmpty -> acc
  | EApp { f = e; args; _ } -> acc |> f e |> lfold args
  | EAppOp { args; _ } -> acc |> lfold args
  | EArray args -> acc |> lfold args
  | EAbs { binder; tys = _ } ->
    let _, body = Bindlib.unmbind binder in
    acc |> f body
  | EIfThenElse { cond; etrue; efalse } -> acc |> f cond |> f etrue |> f efalse
  | ETuple args -> acc |> lfold args
  | ETupleAccess { e; _ } -> acc |> f e
  | EInj { e; _ } -> acc |> f e
  | EAssert e -> acc |> f e
  | EDefault { excepts; just; cons } -> acc |> lfold excepts |> f just |> f cons
  | EPureDefault e -> acc |> f e
  | EErrorOnEmpty e -> acc |> f e
  | EStruct { fields; _ } -> acc |> StructField.Map.fold (fun _ -> f) fields
  | EDStructAmend { e; fields; _ } ->
    acc |> f e |> Ident.Map.fold (fun _ -> f) fields
  | EDStructAccess { e; _ } -> acc |> f e
  | EStructAccess { e; _ } -> acc |> f e
  | EMatch { e; cases; _ } ->
    acc |> f e |> EnumConstructor.Map.fold (fun _ -> f) cases
  | EScopeCall { args; _ } ->
    let rec fold_scope_call_args args =
      ScopeVar.Map.fold
        (fun _ arg ->
          match arg with
          | ScopeVarArg e -> f e
          | SubScopeVarArg (_, sub_args) -> fold_scope_call_args sub_args)
        args
    in
    acc |> fold_scope_call_args args
  | ECustom _ -> acc

(* Like [map], but also allows to gather a result bottom-up. *)
let map_gather
    (type a)
    ~(acc : 'acc)
    ~(join : 'acc -> 'acc -> 'acc)
    ~(f : (a, 'm1) gexpr -> 'acc * (a, 'm2) boxed_gexpr)
    (e : ((a, 'm1) naked_gexpr, 'm2) marked) : 'acc * (a, 'm2) boxed_gexpr =
  let m = Mark.get e in
  let lfoldmap es =
    let acc, r_es =
      List.fold_left
        (fun (acc, es) e ->
          let acc1, e = f e in
          join acc acc1, e :: es)
        (acc, []) es
    in
    acc, List.rev r_es
  in
  match Mark.remove e with
  | ELit l -> acc, elit l m
  | EApp { f = e1; args; tys } ->
    let acc1, f = f e1 in
    let acc2, args = lfoldmap args in
    join acc1 acc2, eapp ~f ~args ~tys m
  | EAppOp { op; args; tys } ->
    let acc, args = lfoldmap args in
    acc, eappop ~op ~args ~tys m
  | EArray args ->
    let acc, args = lfoldmap args in
    acc, earray args m
  | EVar v -> acc, evar (Var.translate v) m
  | EExternal { name } -> acc, eexternal ~name m
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let acc, body = f body in
    let binder = bind (Array.map Var.translate vars) body in
    acc, eabs binder tys m
  | EIfThenElse { cond; etrue; efalse } ->
    let acc1, cond = f cond in
    let acc2, etrue = f etrue in
    let acc3, efalse = f efalse in
    join (join acc1 acc2) acc3, eifthenelse cond etrue efalse m
  | ETuple args ->
    let acc, args = lfoldmap args in
    acc, etuple args m
  | ETupleAccess { e; index; size } ->
    let acc, e = f e in
    acc, etupleaccess ~e ~index ~size m
  | EInj { name; cons; e } ->
    let acc, e = f e in
    acc, einj ~name ~cons ~e m
  | EAssert e ->
    let acc, e = f e in
    acc, eassert e m
  | EFatalError e -> acc, efatalerror e m
  | EDefault { excepts; just; cons } ->
    let acc1, excepts = lfoldmap excepts in
    let acc2, just = f just in
    let acc3, cons = f cons in
    join (join acc1 acc2) acc3, edefault ~excepts ~just ~cons m
  | EPureDefault e ->
    let acc, e = f e in
    acc, epuredefault e m
  | EEmpty -> acc, eempty m
  | EErrorOnEmpty e ->
    let acc, e = f e in
    acc, eerroronempty e m
  | ELocation loc -> acc, elocation loc m
  | EStruct { name; fields } ->
    let acc, fields =
      StructField.Map.fold
        (fun cons e (acc, fields) ->
          let acc1, e = f e in
          join acc acc1, StructField.Map.add cons e fields)
        fields
        (acc, StructField.Map.empty)
    in
    acc, estruct ~name ~fields m
  | EDStructAmend { name_opt; e; fields } ->
    let acc, e = f e in
    let acc, fields =
      Ident.Map.fold
        (fun cons e (acc, fields) ->
          let acc1, e = f e in
          join acc acc1, Ident.Map.add cons e fields)
        fields (acc, Ident.Map.empty)
    in
    acc, edstructamend ~name_opt ~e ~fields m
  | EDStructAccess { name_opt; field; e } ->
    let acc, e = f e in
    acc, edstructaccess ~name_opt ~field ~e m
  | EStructAccess { name; field; e } ->
    let acc, e = f e in
    acc, estructaccess ~name ~field ~e m
  | EMatch { name; e; cases } ->
    let acc, e = f e in
    let acc, cases =
      EnumConstructor.Map.fold
        (fun cons e (acc, cases) ->
          let acc1, e = f e in
          join acc acc1, EnumConstructor.Map.add cons e cases)
        cases
        (acc, EnumConstructor.Map.empty)
    in
    acc, ematch ~name ~e ~cases m
  | EScopeCall { scope; args } ->
    let rec map_gather_scope_call_args args acc =
      ScopeVar.Map.fold
        (fun var arg (acc, args) ->
          match arg with
          | ScopeVarArg e ->
            let acc1, e = f e in
            join acc acc1, ScopeVar.Map.add var (ScopeVarArg e) args
          | SubScopeVarArg (sub_scope_uid, sub_args) ->
            let acc1, sub_args = map_gather_scope_call_args sub_args acc in
            ( join acc acc1,
              ScopeVar.Map.add var
                (SubScopeVarArg (sub_scope_uid, sub_args))
                args ))
        args (acc, ScopeVar.Map.empty)
    in
    let acc, args = map_gather_scope_call_args args acc in
    acc, escopecall ~scope ~args m
  | ECustom { obj; targs; tret } -> acc, ecustom obj targs tret m

(* - *)

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let rec rebox (e : ('a any, 't) gexpr) = map ~f:rebox ~op:Fun.id e

let box e = Mark.map Bindlib.box e
let unbox (e, m) = Bindlib.unbox e, m

let unbox_closed e =
  Box.assert_closed (fst e);
  unbox e

let untype e = map_marks ~f:(fun m -> Untyped { pos = mark_pos m }) e

(* Tests *)

let is_value (type a) (e : (a, _) gexpr) =
  match Mark.remove e with
  | ELit _ | EAbs _ | ECustom _ | EExternal _ -> true
  | _ -> false

let equal_lit (l1 : lit) (l2 : lit) =
  let open Runtime.Oper in
  match l1, l2 with
  | LBool b1, LBool b2 -> not (o_xor b1 b2)
  | LInt n1, LInt n2 -> o_eq_int_int n1 n2
  | LRat r1, LRat r2 -> o_eq_rat_rat r1 r2
  | LMoney m1, LMoney m2 -> o_eq_mon_mon m1 m2
  | LUnit, LUnit -> true
  | LDate d1, LDate d2 -> o_eq_dat_dat d1 d2
  | LDuration d1, LDuration d2 -> (
    try o_eq_dur_dur (pos_to_runtime Pos.no_pos) d1 d2
    with Runtime.(Error (UncomparableDurations, _)) -> false)
  | (LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _), _ ->
    false

let compare_lit (l1 : lit) (l2 : lit) =
  let open Runtime.Oper in
  match l1, l2 with
  | LBool b1, LBool b2 -> Bool.compare b1 b2
  | LInt n1, LInt n2 ->
    if o_lt_int_int n1 n2 then -1 else if o_eq_int_int n1 n2 then 0 else 1
  | LRat r1, LRat r2 ->
    if o_lt_rat_rat r1 r2 then -1 else if o_eq_rat_rat r1 r2 then 0 else 1
  | LMoney m1, LMoney m2 ->
    if o_lt_mon_mon m1 m2 then -1 else if o_eq_mon_mon m1 m2 then 0 else 1
  | LUnit, LUnit -> 0
  | LDate d1, LDate d2 ->
    if o_lt_dat_dat d1 d2 then -1 else if o_eq_dat_dat d1 d2 then 0 else 1
  | LDuration d1, LDuration d2 -> (
    (* Duration comparison in the runtime may fail, so rely on a basic
       lexicographic comparison instead *)
    let y1, m1, d1 = Runtime.duration_to_years_months_days d1 in
    let y2, m2, d2 = Runtime.duration_to_years_months_days d2 in
    match compare y1 y2 with
    | 0 -> ( match compare m1 m2 with 0 -> compare d1 d2 | n -> n)
    | n -> n)
  | LBool _, _ -> -1
  | _, LBool _ -> 1
  | LInt _, _ -> -1
  | _, LInt _ -> 1
  | LRat _, _ -> -1
  | _, LRat _ -> 1
  | LMoney _, _ -> -1
  | _, LMoney _ -> 1
  | LUnit, _ -> -1
  | _, LUnit -> 1
  | LDate _, _ -> -1
  | _, LDate _ -> 1
  | LDuration _, _ -> .
  | _, LDuration _ -> .

let compare_location
    (type a)
    (x : a glocation Mark.pos)
    (y : a glocation Mark.pos) =
  match Mark.remove x, Mark.remove y with
  | ( DesugaredScopeVar { name = vx; state = sx },
      DesugaredScopeVar { name = vy; state = sy } ) -> (
    match Mark.compare ScopeVar.compare vx vy with
    | 0 -> Option.compare StateName.compare sx sy
    | n -> n)
  | ScopelangScopeVar { name = vx, _ }, ScopelangScopeVar { name = vy, _ } ->
    ScopeVar.compare vx vy
  | ToplevelVar { name = vx, _ }, ToplevelVar { name = vy, _ } ->
    TopdefName.compare vx vy
  | DesugaredScopeVar _, _ -> -1
  | _, DesugaredScopeVar _ -> 1
  | ScopelangScopeVar _, _ -> -1
  | _, ScopelangScopeVar _ -> 1
  | ToplevelVar _, _ -> .
  | _, ToplevelVar _ -> .

let equal_location a b = compare_location a b = 0
let equal_error er1 er2 = er1 = er2
let compare_error er1 er2 = Stdlib.compare er1 er2

let equal_external_ref ref1 ref2 =
  match ref1, ref2 with
  | External_value v1, External_value v2 -> TopdefName.equal v1 v2
  | External_scope s1, External_scope s2 -> ScopeName.equal s1 s2
  | (External_value _ | External_scope _), _ -> false

let compare_external_ref ref1 ref2 =
  match ref1, ref2 with
  | External_value v1, External_value v2 -> TopdefName.compare v1 v2
  | External_scope s1, External_scope s2 -> ScopeName.compare s1 s2
  | External_value _, _ -> -1
  | _, External_value _ -> 1
  | External_scope _, _ -> .
  | _, External_scope _ -> .

(* weird indentation; see
   https://github.com/ocaml-ppx/ocamlformat/issues/2143 *)
let rec equal_list : 'a. ('a, 't) gexpr list -> ('a, 't) gexpr list -> bool =
 fun es1 es2 -> List.equal equal es1 es2

and equal : type a. (a, 't) gexpr -> (a, 't) gexpr -> bool =
 fun e1 e2 ->
  match Mark.remove e1, Mark.remove e2 with
  | EVar v1, EVar v2 -> Bindlib.eq_vars v1 v2
  | EExternal { name = n1 }, EExternal { name = n2 } ->
    Mark.equal equal_external_ref n1 n2
  | ETuple es1, ETuple es2 -> equal_list es1 es2
  | ( ETupleAccess { e = e1; index = id1; size = s1 },
      ETupleAccess { e = e2; index = id2; size = s2 } ) ->
    s1 = s2 && equal e1 e2 && id1 = id2
  | EArray es1, EArray es2 -> equal_list es1 es2
  | ELit l1, ELit l2 -> l1 = l2
  | EAbs { binder = b1; tys = tys1 }, EAbs { binder = b2; tys = tys2 } ->
    Type.equal_list tys1 tys2 && Bindlib.eq_mbinder equal b1 b2
  | ( EApp { f = e1; args = args1; tys = tys1 },
      EApp { f = e2; args = args2; tys = tys2 } ) ->
    equal e1 e2 && equal_list args1 args2 && Type.equal_list tys1 tys2
  | ( EAppOp { op = op1; args = args1; tys = tys1 },
      EAppOp { op = op2; args = args2; tys = tys2 } ) ->
    Mark.equal Operator.equal op1 op2
    && equal_list args1 args2
    && Type.equal_list tys1 tys2
  | EAssert e1, EAssert e2 -> equal e1 e2
  | EFatalError e1, EFatalError e2 -> equal_error e1 e2
  | ( EDefault { excepts = exc1; just = def1; cons = cons1 },
      EDefault { excepts = exc2; just = def2; cons = cons2 } ) ->
    equal def1 def2 && equal cons1 cons2 && equal_list exc1 exc2
  | EPureDefault e1, EPureDefault e2 -> equal e1 e2
  | ( EIfThenElse { cond = if1; etrue = then1; efalse = else1 },
      EIfThenElse { cond = if2; etrue = then2; efalse = else2 } ) ->
    equal if1 if2 && equal then1 then2 && equal else1 else2
  | EEmpty, EEmpty -> true
  | EErrorOnEmpty e1, EErrorOnEmpty e2 -> equal e1 e2
  | ELocation l1, ELocation l2 ->
    equal_location (Mark.add Pos.no_pos l1) (Mark.add Pos.no_pos l2)
  | ( EStruct { name = s1; fields = fields1 },
      EStruct { name = s2; fields = fields2 } ) ->
    StructName.equal s1 s2 && StructField.Map.equal equal fields1 fields2
  | ( EDStructAmend { name_opt = s1; e = e1; fields = fields1 },
      EDStructAmend { name_opt = s2; e = e2; fields = fields2 } ) ->
    Option.equal StructName.equal s1 s2
    && equal e1 e2
    && Ident.Map.equal equal fields1 fields2
  | ( EDStructAccess { e = e1; field = f1; name_opt = s1 },
      EDStructAccess { e = e2; field = f2; name_opt = s2 } ) ->
    Option.equal StructName.equal s1 s2 && Ident.equal f1 f2 && equal e1 e2
  | ( EStructAccess { e = e1; field = f1; name = s1 },
      EStructAccess { e = e2; field = f2; name = s2 } ) ->
    StructName.equal s1 s2 && StructField.equal f1 f2 && equal e1 e2
  | EInj { e = e1; cons = c1; name = n1 }, EInj { e = e2; cons = c2; name = n2 }
    ->
    EnumName.equal n1 n2 && EnumConstructor.equal c1 c2 && equal e1 e2
  | ( EMatch { e = e1; name = n1; cases = cases1 },
      EMatch { e = e2; name = n2; cases = cases2 } ) ->
    EnumName.equal n1 n2
    && equal e1 e2
    && EnumConstructor.Map.equal equal cases1 cases2
  | ( EScopeCall { scope = s1; args = fields1 },
      EScopeCall { scope = s2; args = fields2 } ) ->
    let rec scope_call_args_equal fields1 fields2 =
      ScopeVar.Map.equal
        (fun field1 field2 ->
          match field1, field2 with
          | ScopeVarArg e1, ScopeVarArg e2 -> equal e1 e2
          | ( SubScopeVarArg (sub_scope_uid1, sub_fields1),
              SubScopeVarArg (sub_scope_uid2, sub_fields2) ) ->
            ScopeName.equal sub_scope_uid1 sub_scope_uid2
            && scope_call_args_equal sub_fields1 sub_fields2
          | _ -> false)
        fields1 fields2
    in
    ScopeName.equal s1 s2 && scope_call_args_equal fields1 fields2
  | ( ECustom { obj = obj1; targs = targs1; tret = tret1 },
      ECustom { obj = obj2; targs = targs2; tret = tret2 } ) ->
    Type.equal_list targs1 targs2 && Type.equal tret1 tret2 && obj1 == obj2
  | ( ( EVar _ | EExternal _ | ETuple _ | ETupleAccess _ | EArray _ | ELit _
      | EAbs _ | EApp _ | EAppOp _ | EAssert _ | EFatalError _ | EDefault _
      | EPureDefault _ | EIfThenElse _ | EEmpty | EErrorOnEmpty _ | ELocation _
      | EStruct _ | EDStructAmend _ | EDStructAccess _ | EStructAccess _
      | EInj _ | EMatch _ | EScopeCall _ | ECustom _ ),
      _ ) ->
    false

let rec compare : type a. (a, _) gexpr -> (a, _) gexpr -> int =
 fun e1 e2 ->
  (* Infix operator to chain comparisons lexicographically. *)
  let ( @@< ) cmp1 cmpf = match cmp1 with 0 -> cmpf () | n -> n in
  (* OCamlformat doesn't know to keep consistency in match cases so disabled
     locally for readability *)
  match[@ocamlformat "disable"] Mark.remove e1, Mark.remove e2 with
  | ELit l1, ELit l2 ->
    compare_lit l1 l2
  | EApp {f=f1; args=args1; tys=tys1}, EApp {f=f2; args=args2; tys=tys2} ->
    compare f1 f2 @@< fun () ->
    List.compare compare args1 args2 @@< fun () ->
    List.compare Type.compare tys1 tys2
  | EAppOp {op=op1; args=args1; tys=tys1}, EAppOp {op=op2; args=args2; tys=tys2} ->
    Mark.compare Operator.compare op1 op2 @@< fun () ->
    List.compare compare args1 args2 @@< fun () ->
    List.compare Type.compare tys1 tys2
  | EArray a1, EArray a2 ->
    List.compare compare a1 a2
  | EVar v1, EVar v2 ->
    Bindlib.compare_vars v1 v2
  | EExternal { name = n1 }, EExternal { name = n2 } ->
    Mark.compare compare_external_ref n1 n2
  | EAbs {binder=binder1; tys=typs1},
    EAbs {binder=binder2; tys=typs2} ->
    List.compare Type.compare typs1 typs2 @@< fun () ->
    let _, e1, e2 = Bindlib.unmbind2 binder1 binder2 in
    compare e1 e2
  | EIfThenElse {cond=i1; etrue=t1; efalse=e1},
    EIfThenElse {cond=i2; etrue=t2; efalse=e2} ->
    compare i1 i2 @@< fun () ->
    compare t1 t2 @@< fun () ->
    compare e1 e2
  | ELocation l1, ELocation l2 ->
    compare_location (Mark.add Pos.no_pos l1) (Mark.add Pos.no_pos l2)
  | EStruct {name=name1; fields=field_map1 },
    EStruct {name=name2; fields=field_map2 } ->
    StructName.compare name1 name2 @@< fun () ->
    StructField.Map.compare compare field_map1 field_map2
  | EDStructAmend {name_opt=n1; e=e1; fields=field_map1},
    EDStructAmend {name_opt=n2; e=e2; fields=field_map2} ->
    compare e1 e2 @@< fun () ->
    Ident.Map.compare compare field_map1 field_map2 @@< fun () ->
    Option.compare StructName.compare n1 n2
  | EDStructAccess {e=e1; field=field_name1; name_opt=struct_name1},
    EDStructAccess {e=e2; field=field_name2; name_opt=struct_name2} ->
    compare e1 e2 @@< fun () ->
    Ident.compare field_name1 field_name2 @@< fun () ->
    Option.compare StructName.compare struct_name1 struct_name2
  | EStructAccess {e=e1; field=field_name1; name=struct_name1 },
    EStructAccess {e=e2; field=field_name2; name=struct_name2 } ->
    compare e1 e2 @@< fun () ->
    StructField.compare field_name1 field_name2 @@< fun () ->
    StructName.compare struct_name1 struct_name2
  | EMatch {e=e1; name=name1; cases=emap1 },
    EMatch {e=e2; name=name2; cases=emap2 } ->
    EnumName.compare name1 name2 @@< fun () ->
    compare e1 e2 @@< fun () ->
    EnumConstructor.Map.compare compare emap1 emap2
  | EScopeCall {scope=name1; args=field_map1},
    EScopeCall {scope=name2; args=field_map2} ->
    ScopeName.compare name1 name2 @@< fun () ->
      let rec scope_call_args_compare fields1 fields2 =
        ScopeVar.Map.compare
          (fun field1 field2 ->
            match field1, field2 with
            | ScopeVarArg e1, ScopeVarArg e2 -> compare e1 e2
            | SubScopeVarArg (sub_scope_uid1, sub_fields1), SubScopeVarArg (sub_scope_uid2, sub_fields2) ->
              ScopeName.compare sub_scope_uid1 sub_scope_uid2 @@< fun () ->
              scope_call_args_compare sub_fields1 sub_fields2
            | ScopeVarArg _, SubScopeVarArg _ -> -1 | SubScopeVarArg _, ScopeVarArg _ -> 1)
          fields1 fields2
      in
    scope_call_args_compare field_map1 field_map2
  | ETuple es1, ETuple es2 ->
    List.compare compare es1 es2
  | ETupleAccess {e=e1; index=n1; size=s1},
    ETupleAccess {e=e2; index=n2; size=s2} ->
    Int.compare s1 s2 @@< fun () ->
    Int.compare n1 n2 @@< fun () ->
    compare e1 e2
  | EInj {e=e1; name=name1; cons=cons1 },
    EInj {e=e2; name=name2; cons=cons2 } ->
    EnumName.compare name1 name2 @@< fun () ->
    EnumConstructor.compare cons1 cons2 @@< fun () ->
    compare e1 e2
  | EAssert e1, EAssert e2 ->
    compare e1 e2
  | EFatalError e1, EFatalError e2 ->
    compare_error e1 e2
  | EDefault {excepts=exs1; just=just1; cons=cons1},
    EDefault {excepts=exs2; just=just2; cons=cons2} ->
    compare just1 just2 @@< fun () ->
    compare cons1 cons2 @@< fun () ->
    List.compare compare exs1 exs2
  | EPureDefault e1, EPureDefault e2 ->
    compare e1 e2
  | EEmpty, EEmpty -> 0
  | EErrorOnEmpty e1, EErrorOnEmpty e2 ->
    compare e1 e2
  | ECustom _, _ | _, ECustom _ ->
    (* fixme: ideally this would be forbidden by typing *)
    invalid_arg "Custom block comparison"
  | ELit _, _ -> -1 | _, ELit _ -> 1
  | EApp _, _ -> -1 | _, EApp _ -> 1
  | EAppOp _, _ -> -1 | _, EAppOp _ -> 1
  | EArray _, _ -> -1 | _, EArray _ -> 1
  | EVar _, _ -> -1 | _, EVar _ -> 1
  | EExternal _, _ -> -1 | _, EExternal _ -> 1
  | EAbs _, _ -> -1 | _, EAbs _ -> 1
  | EIfThenElse _, _ -> -1 | _, EIfThenElse _ -> 1
  | ELocation _, _ -> -1 | _, ELocation _ -> 1
  | EStruct _, _ -> -1 | _, EStruct _ -> 1
  | EDStructAmend _, _ -> -1 | _, EDStructAmend _ -> 1
  | EDStructAccess _, _ -> -1 | _, EDStructAccess _ -> 1
  | EStructAccess _, _ -> -1 | _, EStructAccess _ -> 1
  | EMatch _, _ -> -1 | _, EMatch _ -> 1
  | EScopeCall _, _ -> -1 | _, EScopeCall _ -> 1
  | ETuple _, _ -> -1 | _, ETuple _ -> 1
  | ETupleAccess _, _ -> -1 | _, ETupleAccess _ -> 1
  | EInj _, _ -> -1 | _, EInj _ -> 1
  | EAssert _, _ -> -1 | _, EAssert _ -> 1
  | EFatalError _, _ -> -1 | _, EFatalError _ -> 1
  | EDefault _, _ -> -1 | _, EDefault _ -> 1
  | EPureDefault _, _ -> -1 | _, EPureDefault _ -> 1
  | EEmpty , _ -> -1 | _, EEmpty  -> 1
  | EErrorOnEmpty _, _ -> . | _, EErrorOnEmpty _ -> .

let rec free_vars : ('a, 't) gexpr -> ('a, 't) gexpr Var.Set.t = function
  | EVar v, _ -> Var.Set.singleton v
  | EAbs { binder; _ }, _ ->
    let vs, body = Bindlib.unmbind binder in
    Array.fold_right Var.Set.remove vs (free_vars body)
  | e -> shallow_fold (fun e -> Var.Set.union (free_vars e)) e Var.Set.empty
(* Could also be done with [rebox] followed by [Bindlib.free_vars], if that
   returned more than a context *)

(* This function is first defined in [Print], only for dependency reasons *)
let skip_wrappers : type a. (a, 'm) gexpr -> (a, 'm) gexpr = Print.skip_wrappers

let remove_logging_calls e =
  let rec f e =
    let e, m = map ~f ~op:Fun.id e in
    ( Bindlib.box_apply
        (function
          | EAppOp { op = Log _, _; args = [(arg, _)]; _ } -> arg | e -> e)
        e,
      m )
  in
  f e

module DefaultBindlibCtxRename = struct
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
end

let rename_vars
    ?(exclude = ([] : string list))
    ?(reset_context_for_closed_terms = false)
    ?(skip_constant_binders = false)
    ?(constant_binder_name = None)
    e =
  let module BindCtx = Bindlib.Ctxt (struct
    include DefaultBindlibCtxRename

    let reset_context_for_closed_terms = reset_context_for_closed_terms
    let skip_constant_binders = skip_constant_binders
    let constant_binder_name = constant_binder_name
  end) in
  let rec aux : type a. BindCtx.ctxt -> (a, 't) gexpr -> (a, 't) gexpr boxed =
   fun ctx e ->
    match e with
    | EAbs { binder; tys }, m ->
      let vars, body, ctx = BindCtx.unmbind_in ctx binder in
      let body = aux ctx body in
      let binder = bind vars body in
      eabs binder tys m
    | e -> map ~f:(aux ctx) ~op:Fun.id e
  in
  let ctx =
    List.fold_left
      (fun ctx name -> DefaultBindlibCtxRename.reserve_name name ctx)
      BindCtx.empty_ctxt exclude
  in
  aux ctx e

let format ppf e = Print.expr ~debug:false () ppf e

let rec size : type a. (a, 't) gexpr -> int =
 fun e ->
  match Mark.remove e with
  | EVar _ | EExternal _ | ELit _ | EEmpty | ECustom _ -> 1
  | ETuple args -> List.fold_left (fun acc arg -> acc + size arg) 1 args
  | EArray args -> List.fold_left (fun acc arg -> acc + size arg) 1 args
  | ETupleAccess { e; _ } -> size e + 1
  | EInj { e; _ } -> size e + 1
  | EAssert e -> size e + 1
  | EFatalError _ -> 1
  | EErrorOnEmpty e -> size e + 1
  | EPureDefault e -> size e + 1
  | EApp { f; args; _ } ->
    List.fold_left (fun acc arg -> acc + size arg) (1 + size f) args
  | EAppOp { args; _ } -> List.fold_left (fun acc arg -> acc + size arg) 2 args
  | EAbs { binder; _ } ->
    let _, body = Bindlib.unmbind binder in
    1 + size body
  | EIfThenElse { cond; etrue; efalse } ->
    1 + size cond + size etrue + size efalse
  | EDefault { excepts; just; cons } ->
    List.fold_left
      (fun acc except -> acc + size except)
      (1 + size just + size cons)
      excepts
  | ELocation _ -> 1
  | EStruct { fields; _ } ->
    StructField.Map.fold (fun _ e acc -> acc + 1 + size e) fields 0
  | EDStructAmend { e; fields; _ } ->
    1 + size e + Ident.Map.fold (fun _ e acc -> acc + 1 + size e) fields 0
  | EDStructAccess { e; _ } -> 1 + size e
  | EStructAccess { e; _ } -> 1 + size e
  | EMatch { e; cases; _ } ->
    EnumConstructor.Map.fold (fun _ e acc -> acc + 1 + size e) cases (size e)
  | EScopeCall { args; _ } ->
    let rec scope_call_args_size args acc =
      ScopeVar.Map.fold
        (fun _ arg acc ->
          acc
          + 1
          +
          match arg with
          | ScopeVarArg e -> size e
          | SubScopeVarArg (_, sub_args) -> scope_call_args_size sub_args acc)
        args acc
    in
    scope_call_args_size args 1

(* - Expression building helpers - *)

let make_var v mark = evar v mark

let make_abs xs e taus pos =
  let mark =
    map_mark
      (fun _ -> pos)
      (fun ety -> Mark.add pos (TArrow (taus, ety)))
      (Mark.get e)
  in
  eabs (bind xs e) taus mark

let make_tuple el m0 =
  match el with
  | [] -> etuple [] (with_ty m0 (TTuple [], mark_pos m0))
  | el ->
    let m =
      fold_marks
        (fun posl -> List.hd posl)
        (fun ml -> TTuple (List.map (fun t -> t.ty) ml), (List.hd ml).pos)
        (List.map (fun e -> Mark.get e) el)
    in
    etuple el m

let make_tupleaccess e index size pos =
  let m =
    map_mark
      (fun _ -> pos)
      (function
        | TTuple tl, _ -> (
          try List.nth tl index
          with Failure _ ->
            Message.error ~internal:true "Trying to build invalid tuple access")
        | TAny, pos -> TAny, pos
        | ty ->
          Message.error ~internal:true "Unexpected non-tuple type annotation %a"
            Print.typ_debug ty)
      (Mark.get e)
  in
  etupleaccess ~e ~index ~size m

let make_app f args tys pos =
  let mark =
    fold_marks
      (fun _ -> pos)
      (function
        | [] -> assert false
        | fty :: argtys -> (
          match Mark.remove fty.ty with
          | TArrow (tx', tr) ->
            assert (Type.unifiable_list tx' (List.map (fun x -> x.ty) argtys));
            tr
          | TAny -> fty.ty
          | _ ->
            Message.error ~internal:true
              "wrong type: found %a while expecting either an Arrow or Any"
              Print.typ_debug fty.ty))
      (List.map Mark.get (f :: args))
  in
  eapp ~f ~args ~tys mark

let make_erroronempty e =
  let mark =
    map_mark
      (fun pos -> pos)
      (function
        | TDefault ty, _ -> ty
        | TAny, pos -> TAny, pos
        | ty ->
          Message.error ~internal:true
            "wrong type: found %a while expecting a TDefault on@;<1 2>%a"
            Print.typ_debug ty format (unbox e))
      (Mark.get e)
  in
  eerroronempty e mark

let thunk_term term =
  let silent = Var.make "_" in
  let pos = mark_pos (Mark.get term) in
  make_abs [| silent |] term [TLit TUnit, pos] pos

let empty_thunked_term mark = thunk_term (Bindlib.box EEmpty, mark)

let unthunk_term_nobox = function
  | EAbs { binder; tys = [(TLit TUnit, _)] }, _ ->
    let _v, e = Bindlib.unmbind binder in
    e
  | _ -> invalid_arg "unthunk_term_nobox"

let make_let_in x tau e1 e2 mpos =
  make_app (make_abs [| x |] e2 [tau] mpos) [e1] [tau] (pos e2)

let make_multiple_let_in xs taus e1s e2 mpos =
  make_app (make_abs xs e2 taus mpos) e1s taus (pos e2)

let make_puredefault e =
  let mark =
    map_mark (fun pos -> pos) (fun ty -> TDefault ty, Mark.get ty) (Mark.get e)
  in
  epuredefault e mark

let fun_id ?(var_name : string = "x") mark : ('a any, 'm) boxed_gexpr =
  let x = Var.make var_name in
  make_abs [| x |] (evar x mark) [TAny, mark_pos mark] (mark_pos mark)
