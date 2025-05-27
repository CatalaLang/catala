open Catala_utils
open Shared_ast

(*let slice_trace (trace_with_holes : ('a, 'm) Trace_ast.t) : 'a program = assert false*)

let get_fields _ = assert false

let rec join_expr : 
  type d t. 
  (d, t) gexpr -> 
  (d, t) gexpr -> 
  (d, t) gexpr =
fun e1 e2 -> 
  let m = Mark.get e1 in
  match Mark.remove e1, Mark.remove e2 with
  | _, EHole _ -> e1
  | EHole _, _ -> e2
  | ELit l1, ELit l2 when l1 == l2 -> e1
  | EEmpty, EEmpty -> e1
  | EFatalError err1, EFatalError err2 when err1 = err2 -> e1
  | EAbs { binder = b1; pos; tys }, EAbs { binder = b2; pos=_; tys=_} ->
    let vars1, e1 = Bindlib.unmbind b1 in
    let vars2, e2 = Bindlib.unmbind b2 in
    if List.fold_left2 
      (fun b v1 v2 -> b && Bindlib.eq_vars v1 v2) 
      true 
      (Array.to_list vars1) 
      (Array.to_list vars2)
    then
      let e = join_expr e1 e2 in 
      let binder = 
        Bindlib.bind_mvar vars1 (Bindlib.box e) 
        |> Bindlib.unbox 
      in
      Mark.add m (EAbs { binder; pos; tys })
    else
      Message.error "The two functions cannot be joined because the arguments are not the same"
  | ECustom { obj = o1; targs = _; tret = _ }, ECustom { obj = o2; targs = _; tret = _ } 
    when o1 = o2 -> e1 
  | EVar x1, EVar x2 when x1 = x2 -> e1 
  | EExternal { name = n1 }, EExternal { name = n2 } when n1 = n2 -> e1
  | EApp { f = f1; args = a1; tys }, EApp { f = f2; args = a2; tys = _} ->
    Mark.add m (EApp { f = join_expr f1 f2; args = List.map2 join_expr a1 a2; tys })
  | EAppOp { op = op1; args = a1; tys }, EAppOp { op = op2; args = a2; tys = _ } when op1 = op2->
    Mark.add m (EAppOp {op = op1; args = List.map2 join_expr a1 a2; tys })
  | EArray e1, EArray e2 -> Mark.add m (EArray (List.map2 join_expr e1 e2))
  | EIfThenElse { cond = c1; etrue = t1; efalse = f1 }, EIfThenElse { cond = c2; etrue = t2; efalse = f2 } ->
    Mark.add m (EIfThenElse { cond = join_expr c1 c2; etrue = join_expr t1 t2; efalse = join_expr f1 f2 })
  | EStruct { name = n1; fields = f1 }, EStruct { name = n2; fields = f2 } when n1 = n2 ->
    let fields = StructField.Map.mapi (fun f e -> join_expr e (StructField.Map.find f f2)) f1 in 
    Mark.add m (EStruct { name = n1; fields })
  | EStructAccess { name = n1; e = e1; field = f1 }, EStructAccess { name = n2; e = e2; field = f2 } 
    when n1 = n2 && f1 = f2 ->
      Mark.add m (EStructAccess { name = n1; e = join_expr e1 e2; field = f1 })
  | EInj { name = n1; e = e1; cons = c1 }, EInj { name = n2; e = e2; cons = c2 }
    when n1 = n2 && c1 = c2 ->
      Mark.add m (EInj { name = n1; e = join_expr e1 e2; cons = c1 })
  | EMatch { name = n1; e = e1; cases = c1 }, EMatch { name = n2; e = e2; cases = c2 } when n1 = n2 ->
    let cases = EnumConstructor.Map.mapi (fun c e -> join_expr e (EnumConstructor.Map.find c c2)) c1 in 
    Mark.add m (EMatch { name = n1; e = join_expr e1 e2; cases})
  | ETuple e1, ETuple e2 -> Mark.add m (ETuple (List.map2 join_expr e1 e2))
  | ETupleAccess { e = e1; index = i1; size = s1}, ETupleAccess { e = e2; index = i2; size = s2}
    when i1 = i2 && s1 = s2 ->
      Mark.add m (ETupleAccess { e = join_expr e1 e2; index = i1; size = s1 })
  | EAssert e1, EAssert e2 -> Mark.add m (EAssert (join_expr e1 e2))
  | EDefault { excepts = x1; just = j1; cons = c1 }, EDefault { excepts = x2; just = j2; cons = c2 } ->
    Mark.add m (EDefault { excepts = List.map2 join_expr x1 x2; just = join_expr j1 j2; cons = join_expr c1 c2 })
  | EPureDefault e1, EPureDefault e2 -> Mark.add m (EPureDefault (join_expr e1 e2))
  | EErrorOnEmpty e1, EErrorOnEmpty e2 -> Mark.add m (EErrorOnEmpty (join_expr e1 e2))
  | _ -> Message.error "The two expressions cannot be joined"
let tany = (TAny, Pos.void)

let unevaluate :
  type d t.
  decl_ctx ->
  Global.backend_lang ->
  (d dcalc_slicing, t) gexpr ->
  (d dcalc_slicing, t) Trace_ast.t ->
  (d dcalc_slicing, t) gexpr =
fun ctx lang value trace -> 
  let rec unevaluate_aux :
    type d t.
    (d dcalc_slicing, t) gexpr ->
    (d dcalc_slicing, t) Trace_ast.t ->
    ((d dcalc_slicing, t) gexpr, (d dcalc_slicing, t) gexpr) Var.Map.t * (d dcalc_slicing, t) gexpr =
  fun v trace ->
    let m = Mark.get v in
    match Mark.remove v, trace with
    | EHole _, _ -> Var.Map.empty, v
    | _, TrExpr _ -> Message.error "This case should not happen, TrExpr cannot be unevaluated"
    | ELit l1, TrLit l2 when l1 == l2 -> Var.Map.empty, v
    | EEmpty, TrEmpty -> Var.Map.empty, v
    (*| ECustom { obj=o1; targs=ta1; tret=tr1 }, 
      TrCustom { obj=o2; targs=ta2; tret=tr2 }
      when o1 == o2 && ta1 == ta2 && tr1 == tr2 -> v*)
    (*| EAbs { binder = b1; pos = p1; tys = t1 }, 
      TrAbs { binder = b2; pos = p2; tys = t2 }
      when b1 = b2 && p1 = p2 && t1 = t2 -> Var.Map.empty, v*)
    | EAbs _, TrAbs _-> Var.Map.empty, v
    | _, TrVar x -> Var.Map.singleton x v, Mark.add m (EVar x)
    | _, TrExternal { name } -> Var.Map.empty, Mark.add m (EExternal { name })
    | _, TrApp { trf = TrAbs {binder; pos; tys} as trf; trargs; tys=tys2; trv } -> 
      let local_ctx, e = unevaluate_aux v trv in
      let vars_arr, _ = Bindlib.unmbind binder in
      let vars = Array.to_list vars_arr in
      let values = List.map (fun v -> Var.Map.find v local_ctx) vars in
      let lctx2, e2 = List.split(List.map2 unevaluate_aux values trargs) in
      let binder2 = 
         Bindlib.bind_mvar vars_arr (Bindlib.box e) 
        |> Bindlib.unbox
      in
      let lctx1, e1 = unevaluate_aux (Mark.add m (EAbs { binder = binder2 ; pos ; tys})) trf in
      (List.fold_left (Var.Map.union (fun _ v1 v2 -> Some (join_expr v1 v2)) )lctx1 lctx2), Mark.add m (EApp {f = e1; args = e2; tys = tys2})
    | _, TrAppOp { op; trargs; tys; trv } -> assert false
    | _, TrStructAccess { name; tr; field } -> assert false
    | EStruct { name = n1; fields = f1 }, TrStruct { name = n2; fields = f2 } 
      when n1 = n2 -> assert false
    | ETuple v, TrTuple tr when List.length v = List.length tr -> assert false
    | _, TrTupleAccess { tr; index; size } -> 
      let arr = List.init size (fun i -> if i = index then v else Mark.add m (EHole tany)) in
      let e = Mark.add m (ETuple arr) in 
      let local_ctx, e' = unevaluate_aux e tr in 
      local_ctx, Mark.add m (ETupleAccess { e = e'; index; size })
    | EInj { name = n1; e; cons = c1 }, TrInj { name = n2; tr; cons = c2 } -> assert false
    | _, TrMatch { name; tr; cases } -> assert false
    | _, TrIfThenElse { trcond; trtrue; trfalse } -> assert false
    | EArray v, TrArray tr -> assert false
    | ELit LUnit, TrAssert tr -> assert false
    | _, TrErrorOnEmpty tr -> assert false
    | _, TrPureDefault tr -> assert false
    | _, TrDefault { trexcepts; trjust; trcons } -> assert false
    | EFatalError err1, TrFatalError { err = err2; tr } when err1 = err2 -> assert false
    | _ -> Message.error "The trace does not match the value"
  in snd (unevaluate_aux value trace)
