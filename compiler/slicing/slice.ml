open Catala_utils
open Shared_ast

(*let slice_trace (trace_with_holes : ('a, 'm) Trace_ast.t) : 'a program = assert false*)

let get_fields (ctx:decl_ctx) name = StructName.Map.find name ctx.ctx_structs

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
    if Array.for_all2 Bindlib.eq_vars vars1 vars2
    then
      let e = join_expr e1 e2 in 
      let binder = Bindlib.unbox (Bindlib.bind_mvar vars1 (Bindlib.box e)) in
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

let join_ctx ctx1 ctx2 = (Var.Map.union (fun _ v1 v2 -> Some (join_expr v1 v2))) ctx1 ctx2  

let tany = (TAny, Pos.void)

let rec change_op_type_for_slicing : type d. d operator Mark.pos -> slicing_features operator Mark.pos =
fun _ -> assert false

let change_expr_type_for_slicing : type d t. ?ctx:((d, t) gexpr, (slicing_features, t) gexpr Var.t) Var.Map.t -> (d, t) gexpr -> (slicing_features, t) gexpr =
fun ?(ctx=Var.Map.empty) e -> 
  let rec aux : ((d, t) gexpr, (slicing_features, t) gexpr Var.t) Var.Map.t -> (d, t) gexpr -> (slicing_features, t) gexpr =
  fun lctx e ->
  let m = Mark.get e in
  let e' = match Mark.remove e with
    | ELit l -> ELit l
    | EApp { f; args; tys } -> 
      EApp { 
        f = aux lctx f; 
        args = List.map (aux lctx) args;
        tys 
      }
    | EAppOp { op; args; tys } ->
      EAppOp {
        op = change_op_type_for_slicing op;
        args = List.map (aux lctx) args;
        tys
      }
    | EArray arr -> EArray (List.map (aux lctx) arr)
    | EVar x -> EVar (Var.Map.find x lctx)
    | EAbs { binder; pos; tys } ->
      (*
        binder : (('a, 'a, 'm) base_gexpr, ('a, 'm) gexpr) Bindlib.mbinder;
        pos : Pos.t list;
        tys : typ list;
      }
        ->*) 
      (* We need to mbind the new vars*)
      let vars, e = Bindlib.unmbind binder in 
      let new_vars = Array.map Var.make (Bindlib.names_of vars) in
      let new_ctx = List.fold_left2 
        (fun ctx v v' -> Var.Map.add v v' ctx) 
        lctx 
        (Array.to_list vars) 
        (Array.to_list new_vars) 
      in
      let new_e = aux new_ctx e in
      let new_binder = Bindlib.unbox (Bindlib.bind_mvar new_vars (Bindlib.box new_e)) in 
      EAbs { binder = new_binder; pos; tys }
    | EIfThenElse { cond; etrue; efalse } ->
      EIfThenElse{
        cond = aux lctx cond;
        etrue = aux lctx etrue;
        efalse = aux lctx efalse;
      }
    | EStruct { name; fields } -> EStruct { name; fields = StructField.Map.map (aux lctx) fields }
    | EInj { name; e ; cons } -> EInj { name; e = aux lctx e; cons }
    | EMatch { name; e; cases } ->
      EMatch {
        name;
        e = aux lctx e;
        cases = EnumConstructor.Map.map (aux lctx) cases;
      }
    | ETuple tpl -> ETuple (List.map (aux lctx) tpl)
    | ETupleAccess { e; index; size } -> ETupleAccess { e = aux lctx e; index; size }
    | EStructAccess { name; e; field } -> EStructAccess { name; e = aux lctx e; field }
    | EExternal { name } -> EExternal { name }
    | EAssert e -> EAssert (aux lctx e)
    | EFatalError err -> EFatalError err
    | EPos pos -> EPos pos
    | EDefault { excepts; just; cons} ->
      EDefault { 
        excepts = List.map (aux lctx) excepts;
        just = aux lctx just;
        cons = aux lctx cons
      }
    | EPureDefault e -> EPureDefault (aux lctx e)
    | EEmpty -> EEmpty
    | EErrorOnEmpty e -> EErrorOnEmpty (aux lctx e)
    | EHole typ -> EHole typ
    | _ -> Message.error "Unable to correct type to slice this expression"
  in
  Mark.add m e'
in aux ctx e

let change_trace_type_for_slicing : type d t. (d, t) Trace_ast.t -> (slicing_features, t) Trace_ast.t =
fun tr ->
  let rec aux : ((d, t) gexpr, (slicing_features, t) gexpr Var.t) Var.Map.t -> (d, t) Trace_ast.t -> (slicing_features, t) Trace_ast.t =
  fun lctx tr -> match tr with
    | TrLit l -> TrLit l
    | TrApp { trf; trargs; tys; trv } -> 
      TrApp { 
        trf = aux lctx trf; 
        trargs = List.map (aux lctx) trargs;
        tys;
        trv = aux lctx trv 
      }
    | TrAppOp { op; trargs; tys; vargs } ->
      TrAppOp {
        op = change_op_type_for_slicing op;
        trargs = List.map (aux lctx) trargs;
        tys;
        vargs = List.map (change_expr_type_for_slicing ~ctx:lctx) vargs
      }
    | TrArray arr -> TrArray (List.map (aux lctx) arr)
    | TrVar x -> TrVar (Var.Map.find x lctx)
    | TrAbs { binder; pos; tys } ->
      let vars, e = Bindlib.unmbind binder in 
      let new_vars = Array.map Var.make (Bindlib.names_of vars) in
      let new_ctx = List.fold_left2 
        (fun ctx v v' -> Var.Map.add v v' ctx) 
        lctx 
        (Array.to_list vars) 
        (Array.to_list new_vars) 
      in
      let new_e = change_expr_type_for_slicing ~ctx:new_ctx e in
      let new_binder = Bindlib.unbox (Bindlib.bind_mvar new_vars (Bindlib.box new_e)) in 
      TrAbs { binder = new_binder; pos; tys }
    | TrIfThenElse { trcond; trtrue; trfalse } ->
      TrIfThenElse{
        trcond = aux lctx trcond;
        trtrue = aux lctx trtrue;
        trfalse = aux lctx trfalse;
      }
    | TrStruct { name; fields } -> TrStruct { name; fields = StructField.Map.map (aux lctx) fields }
    | TrInj { name; tr; cons } -> TrInj { name; tr = aux lctx tr; cons }
    | TrMatch { name; tr; cases } ->
      TrMatch {
        name;
        tr = aux lctx tr;
        cases = EnumConstructor.Map.map (aux lctx) cases;
      }
    | TrTuple tpl -> TrTuple (List.map (aux lctx) tpl)
    | TrTupleAccess { tr; index; size } -> TrTupleAccess { tr = aux lctx tr; index; size }
    | TrStructAccess { name; tr; field } -> TrStructAccess { name; tr = aux lctx tr; field }
    | TrExternal { name } -> TrExternal { name }
    | TrAssert e -> TrAssert (aux lctx e)
    | TrFatalError { err; tr } -> TrFatalError { err; tr = aux lctx tr }
    | TrDefault { trexcepts; trjust; trcons; vexcepts} ->
      TrDefault { 
        trexcepts = List.map (aux lctx) trexcepts;
        trjust = aux lctx trjust;
        trcons = aux lctx trcons;
        vexcepts = List.map (change_expr_type_for_slicing ~ctx:lctx) vexcepts;
      }
    | TrPureDefault e -> TrPureDefault (aux lctx e)
    | TrEmpty -> TrEmpty
    | TrErrorOnEmpty e -> TrErrorOnEmpty (aux lctx e)
    | TrHole typ -> TrHole typ
    | _ -> Message.error "Unable to correct type to slice this trace"
  in aux Var.Map.empty tr


let unevaluate :
  type t.
  decl_ctx ->
  (slicing_features, t) gexpr ->
  (slicing_features, t) Trace_ast.t ->
  (slicing_features, t) gexpr =
fun ctx value trace -> 
  let rec unevaluate_aux :
    (slicing_features, t) gexpr ->
    (slicing_features, t) Trace_ast.t ->
    ((slicing_features, t) gexpr, (slicing_features, t) gexpr) Var.Map.t * (slicing_features, t) gexpr =
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
    | EAbs { binder = _; pos = _; tys = t1 }, TrAbs { binder = _; pos = _; tys = t2 }
      when t1 = t2 -> Var.Map.empty, v
    | _, TrVar x -> Var.Map.singleton x v, Mark.add m (EVar x)
    | _, TrExternal { name } -> Var.Map.empty, Mark.add m (EExternal { name })
    | _, TrApp { trf = TrAbs { binder; pos; tys } as trf; trargs; tys=tys2; trv } -> 
      let local_ctx, e = unevaluate_aux v trv in
      let vars_arr, _ = Bindlib.unmbind binder in
      let vars = Array.to_list vars_arr in
      let values = List.map (fun v -> Var.Map.find v local_ctx) vars in
      let lctx2, e2 = List.split(List.map2 unevaluate_aux values trargs) in
      let binder2 = Bindlib.unbox (Bindlib.bind_mvar vars_arr (Bindlib.box e)) in
      let lctx1, e1 = unevaluate_aux (Mark.add m (EAbs { binder = binder2 ; pos ; tys})) trf in
      (List.fold_left join_ctx lctx1 lctx2), Mark.add m (EApp {f = e1; args = e2; tys = tys2})
    | _, TrAppOp { op; trargs; tys; vargs } -> (* may need to verify that op(vargs) = v *)
      (* Could certainely reduce the expression again by watching all the operators more closely *) 
      (* For instance lenght function does not need to know the content of an array *)
      (* Or multiplication by 0 could make the other integer a hole *)
      let lctxs, args = List.split (List.map2 unevaluate_aux vargs trargs) in 
      let local_ctx = List.fold_left join_ctx Var.Map.empty lctxs in 
      local_ctx, Mark.add m (EAppOp { op; args; tys })
    | _, TrStructAccess { name; tr; field } -> 
      let fields_typ = get_fields ctx name in 
      let fields = StructField.Map.mapi (fun f ty -> if f = field then v else (Mark.add m (EHole ty))) fields_typ in
      let estruct = Mark.add m (EStruct { name; fields }) in
      let local_ctx, e = unevaluate_aux estruct tr in
      local_ctx, Mark.add m (EStructAccess { name; e; field })
    | EStruct { name = n1; fields = efields }, TrStruct { name = n2; fields = trfields } 
      when n1 = n2 ->
        let fields_with_ctx = StructField.Map.mapi (fun f e -> unevaluate_aux e (StructField.Map.find f trfields)) efields in
        let local_ctx = StructField.Map.fold (fun _ (ctx,_) lctx -> join_ctx lctx ctx) fields_with_ctx Var.Map.empty in
        let fields = StructField.Map.map snd fields_with_ctx in
        local_ctx, Mark.add m (EStruct { name = n1; fields })
    | ETuple v, TrTuple tr when List.length v = List.length tr -> 
      let local_ctxs, es = List.split(List.map2 unevaluate_aux v tr) in 
      let local_ctx = List.fold_left join_ctx Var.Map.empty local_ctxs in 
      local_ctx, Mark.add m (ETuple es)
    | _, TrTupleAccess { tr; index; size } -> 
      let arr = List.init size (fun i -> if i = index then v else Mark.add m (EHole tany)) in
      let e = Mark.add m (ETuple arr) in 
      let local_ctx, e' = unevaluate_aux e tr in 
      local_ctx, Mark.add m (ETupleAccess { e = e'; index; size })
    | EInj { name = n1; e = v; cons = c1 }, TrInj { name = n2; tr; cons = c2 } 
      when n1 = n2 && c1 = c2 ->
        let local_ctx, e = unevaluate_aux v tr in 
        local_ctx, Mark.add m (EInj { name = n1; e; cons = c1})
    | _, TrMatch { name; tr; cases } -> 
      let interesting_cases = EnumConstructor.Map.filter 
        (fun _ t -> match t with (Trace_ast.TrExpr _ |TrHole _) -> false | _ -> true ) cases in
      (match EnumConstructor.Map.cardinal interesting_cases with
      | 0 -> Message.error "No case found to unevaluate the value in the match sequence"
      | 1 -> 
        let cons, trc = EnumConstructor.Map.choose interesting_cases in 
        let lctxc, ec' = unevaluate_aux v trc in 
        (match Mark.remove ec' with
        | EApp { f = ec; args = [v']; tys = _} -> 
          let lctx, e = unevaluate_aux (Mark.add m (EInj { name; e = v'; cons })) tr in 
          let cases_expr = EnumConstructor.Map.mapi 
            (fun c _ -> if c = cons then ec else (Mark.add m (EHole tany))) 
            cases  
          in
          (join_ctx lctx lctxc), (Mark.add m (EMatch { name; e; cases = cases_expr})) 
        | _ -> Message.error "Should not happen if well typed"
        )
      | _ -> Message.error "Could not determine the case the value was computed from in the match sequence"
      )
    | _, TrIfThenElse { trcond; trtrue; trfalse } -> (
      match trtrue, trfalse with 
        | _, (TrExpr _ |TrHole _)-> (*take the true branch*)(
          let lctxc, cond = unevaluate_aux (Mark.add m (ELit(LBool true))) trcond in
          let lctxt, etrue = unevaluate_aux v trtrue in
          (join_ctx lctxc lctxt), Mark.add m (EIfThenElse { cond; etrue; efalse = Mark.add m (EHole tany)})
          )
        | (TrExpr _ |TrHole _), _ -> (*take the false branch*)(
          let lctxc, cond = unevaluate_aux (Mark.add m (ELit(LBool false))) trcond in
          let lctxf, efalse = unevaluate_aux v trfalse in
          (join_ctx lctxc lctxf), Mark.add m (EIfThenElse { cond; etrue = Mark.add m (EHole tany); efalse})
          )
        | _ -> Message.error "Could not identify whether the result of the condition was true or false" 
      )
    | EArray v, TrArray tr when List.length v = List.length tr ->
      let local_ctxs, es = List.split(List.map2 unevaluate_aux v tr) in 
      let local_ctx = List.fold_left join_ctx Var.Map.empty local_ctxs in 
      local_ctx, Mark.add m (EArray es)
    | ELit LUnit, TrAssert tr -> 
      let local_ctx, e = unevaluate_aux (Mark.add m (ELit (LBool true))) tr in 
      local_ctx, Mark.add m (EAssert e)
    | v', TrErrorOnEmpty tr when v' <> EEmpty -> 
      let local_ctx, e = unevaluate_aux v tr in 
      local_ctx, Mark.add m (EErrorOnEmpty e)
    | _, TrPureDefault tr ->
      let local_ctx, e = unevaluate_aux v tr in 
      local_ctx, Mark.add m (EPureDefault e)
    | _, TrDefault { trexcepts; vexcepts; trjust; trcons } -> (
      match trjust, trcons with
      | (TrExpr _ |TrHole _), _ -> (* The result is obtained from one of the exceptions *)
        let lctxs,excepts = List.split(List.map2 unevaluate_aux vexcepts trexcepts) in
        let local_ctx = List.fold_left join_ctx Var.Map.empty lctxs in 
        local_ctx, Mark.add m (EDefault { excepts; just = Mark.add m (EHole tany); cons = Mark.add m (EHole tany)})
      | _, (TrExpr _ |TrHole _) -> (* The result is obtained from the false justification *)
        let eempty = List.init (List.length trexcepts) (fun _ -> Mark.add m EEmpty) in
        let lctxe, excepts = List.split (List.map2 unevaluate_aux eempty trexcepts) in 
        let lctxj, just = unevaluate_aux (Mark.add m (ELit(LBool false))) trjust in
        let local_ctx = List.fold_left join_ctx lctxj lctxe in
        local_ctx, (Mark.add m (EDefault { excepts; just; cons = Mark.add m (EHole tany) }))
      | _, _ -> (* The result is obtained from the consequence *)
        let eempty = List.init (List.length trexcepts) (fun _ -> Mark.add m EEmpty) in
        let lctxe, excepts = List.split (List.map2 unevaluate_aux eempty trexcepts) in 
        let lctxj, just = unevaluate_aux (Mark.add m (ELit(LBool true))) trjust in
        let lctxc, cons = unevaluate_aux v trcons in
        let local_ctx = List.fold_left join_ctx lctxc (lctxj::lctxe) in
        local_ctx, (Mark.add m (EDefault { excepts; just; cons }))
    )
    | EFatalError err1, TrFatalError { err = err2; tr } when err1 = err2 -> (
      match err1, tr with
      | AssertionFailed, TrAssert tr ->
        let local_ctx, e = unevaluate_aux (Mark.add m (ELit(LBool false))) tr in
        local_ctx, Mark.add m (EAssert e)
      | NoValue, TrErrorOnEmpty tr -> 
        let local_ctx, e = unevaluate_aux (Mark.add m EEmpty) tr in 
        local_ctx, Mark.add m (EErrorOnEmpty e)
      | Conflict, TrDefault { trexcepts; vexcepts; trjust = _; trcons = _ } -> 
        let lctxs,excepts = List.split(List.map2
          (fun v tr -> if Mark.remove v == EEmpty then Var.Map.empty, Mark.add m (EHole tany) else unevaluate_aux v tr)
          vexcepts
          trexcepts
        ) in
        let local_ctx = List.fold_left join_ctx Var.Map.empty lctxs in 
        local_ctx, Mark.add m (EDefault { excepts; just = Mark.add m (EHole tany); cons = Mark.add m (EHole tany)})
      | _ -> Message.error "This error in the execution could not be handled by the unevaluation function"
    )
    | _ -> Message.error "The trace does not match the value"
  in snd (unevaluate_aux value trace)
