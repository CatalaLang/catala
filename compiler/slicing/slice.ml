open Catala_utils
open Shared_ast

let get_fields (ctx:decl_ctx) name = StructName.Map.find name ctx.ctx_structs

let bind vars expr = Bindlib.unbox (Expr.bind vars (Expr.rebox expr))

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
  | ELit l1, ELit l2 when l1 = l2 -> e1
  | EEmpty, EEmpty -> e1
  | EFatalError err1, EFatalError err2 when err1 = err2 -> e1
  | EAbs { binder = b1; pos; tys }, EAbs { binder = b2; pos=_; tys=_} ->
    let vars1, e1 = Bindlib.unmbind b1 in
    let vars2, e2 = Bindlib.unmbind b2 in
    if Array.for_all2 Bindlib.eq_vars vars1 vars2
    then
      let e = join_expr e1 e2 in
      Mark.add m (EAbs { binder = bind vars1 e; pos; tys })
    else
      Message.error "The two functions cannot be joined because the arguments are not the same"
  | ECustom { obj = o1; targs = _; tret = _ }, ECustom { obj = o2; targs = _; tret = _ } 
    when o1 = o2 -> e1 
  | EVar x1, EVar x2 when Bindlib.eq_vars x1 x2 -> e1 
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

let print_ctx ctx =  List.iter (fun cpl -> let x,e = cpl in (print_string (Bindlib.name_of x); print_string " : "; Print_trace.print_expr e; print_string ", ")) (Var.Map.bindings ctx); print_newline()

let unevaluate :
  type c t.
  decl_ctx ->
  ((yes, c, yes) slicing_interpr_kind, t) gexpr ->
  ((yes, c, yes) slicing_interpr_kind, t) Trace_ast.t ->
  ((yes, c, yes) slicing_interpr_kind, t) gexpr =
fun ctx value trace -> 
  let rec unevaluate_aux :
    ((yes, c, yes) slicing_interpr_kind, t) gexpr ->
    ((yes, c, yes) slicing_interpr_kind, t) Trace_ast.t ->
    (((yes, c, yes) slicing_interpr_kind, t) gexpr, ((yes, c, yes) slicing_interpr_kind, t) gexpr) Var.Map.t * ((yes, c, yes) slicing_interpr_kind, t) gexpr =
  fun v trace ->
    let m = Mark.get v in
    match Mark.remove v, trace with
    | EHole _, _ -> Var.Map.empty, v
    | _, TrExpr _ -> Message.error "This case should not happen, TrExpr cannot be unevaluated"
    | ELit l1, TrLit l2 when l1 = l2 -> Var.Map.empty, v
    | EEmpty, TrEmpty -> Var.Map.empty, v
    (*| ECustom { obj=o1; targs=ta1; tret=tr1 }, 
      TrCustom { obj=o2; targs=ta2; tret=tr2 }
      when o1 = o2 && ta1 = ta2 && tr1 = tr2 -> v*)
    | EAbs _, TrAbs _ -> Var.Map.empty, v
    | _, TrVar x -> Var.Map.singleton x v, Mark.add m (EVar x)
    | _, TrExternal { name } -> Var.Map.empty, Mark.add m (EExternal { name })
    | _, TrApp { trf = TrAbs { binder = _; pos; tys } as trf; trargs; tys=tys2; vars; trv } ->
      let local_ctx, e = unevaluate_aux v trv in
      let values = List.map 
        (fun v -> match Var.Map.find_opt v local_ctx with 
          | Some value -> value 
          (*if the variable is not in the context, then it was not used in the body hence we can assign it a hole*)
          | None -> Mark.add m (EHole tany) 
        ) 
        (Array.to_list vars)
      in
      let lctx2, e2 = List.split(List.map2 unevaluate_aux values trargs) in
      let lctx1, e1 = unevaluate_aux (Mark.add m (EAbs { binder = bind vars e; pos; tys})) trf in
      (List.fold_left join_ctx local_ctx (lctx1::lctx2)), Mark.add m (EApp {f = e1; args = e2; tys = tys2})
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
          (fun v tr -> if Mark.remove v = EEmpty then Var.Map.empty, Mark.add m (EHole tany) else unevaluate_aux v tr)
          vexcepts
          trexcepts
        ) in
        let local_ctx = List.fold_left join_ctx Var.Map.empty lctxs in 
        local_ctx, Mark.add m (EDefault { excepts; just = Mark.add m (EHole tany); cons = Mark.add m (EHole tany)})
      | _ -> Message.error "This error in the execution could not be handled by the unevaluation function"
    )
    | _ -> Message.error "The trace does not match the value"
  in snd (unevaluate_aux value trace)

let rec list_fold_right4 f l1 l2 l3 l4 accu =
  match (l1, l2, l3, l4) with
    ([], [], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3, a4::l4) -> f a1 a2 a3 a4 (list_fold_right4 f l1 l2 l3 l4 accu)
  | _ -> assert false

let del_useless_declarations e =
  let rec f :
      type d c.
      ((d, c, yes) slicing_interpr_kind, 't) gexpr -> 
      ((d, c, yes) slicing_interpr_kind, 't) gexpr boxed
      = function
    | EApp { f = EAbs { binder; pos; tys }, mf; args; _ }, m -> (
      let vars, body = Bindlib.unmbind binder in
      let body = Expr.map ~f body in
      let useful_vars, pos, tys, args = list_fold_right4
        (fun var p t e (vars, pos, tys, args) -> match Mark.remove e with 
          | EHole _ -> (vars, pos, tys, args) 
          | _ -> (var::vars, p::pos, t::tys, (Expr.map ~f e)::args) )
        (Array.to_list vars) pos tys args ([], [], [], [])
      in
      if useful_vars <> [] then
        let binder = Expr.bind (Array.of_list useful_vars) body in
        Expr.eapp ~f:(Expr.eabs binder pos tys mf) ~args ~tys m
      else 
        body
    )
    | (EHole _, _) as e -> Expr.map ~f e
    | (ECustom _, _) as e -> Expr.map ~f e
    | EAppOp { op; args; tys }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmpty, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | (EPos _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | EFatalError _ | ELit _ | EApp _ | EArray _ | EVar _
        | EExternal _ | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _
        | EInj _ | EStruct _ | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  Expr.unbox (f e)


let slice :
type c t.
  decl_ctx ->
  ((yes, c, yes) slicing_interpr_kind, t) gexpr ->
  ((yes, c, yes) slicing_interpr_kind, t) Trace_ast.t ->
  ((yes, c, yes) slicing_interpr_kind, t) gexpr =
fun ctx value trace -> 
  let expr = unevaluate ctx value trace in
  del_useless_declarations expr