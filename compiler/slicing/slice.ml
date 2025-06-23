open Catala_utils
open Shared_ast

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
      let body = f body in
      let useful_vars, pos, tys, args = list_fold_right4
        (fun var p t e (vars, pos, tys, args) -> match Mark.remove e with 
          | EHole _ -> (vars, pos, tys, args) 
          | _ -> (var::vars, p::pos, t::tys, (f e)::args) )
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

let get_fields (ctx:decl_ctx) name = StructName.Map.find name ctx.ctx_structs

let bind vars expr = Bindlib.unbox (Expr.bind vars (Expr.rebox expr))

let rec join_expr : type d t. (d, t) gexpr -> (d, t) gexpr -> (d, t) gexpr =
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
    if Array.length vars1 = Array.length vars2
    then
      let e = join_expr e1 e2 in
      Mark.add m (EAbs { binder = bind vars1 e; pos; tys })
    else
      Message.error "The two functions cannot be joined because the arguments are not the same"
  | ECustom { obj = o1; targs = _; tret = _ }, ECustom { obj = o2; targs = _; tret = _ } 
    when o1 = o2 -> e1 
  | EVar _, EVar _ (*when Bindlib.eq_vars x1 x2*) -> e1 
  | EExternal { name = n1 }, EExternal { name = n2 } when n1 = n2 -> e1
  | EApp { f = f1; args = a1; tys }, EApp { f = f2; args = a2; tys = _} ->
    Mark.add m (EApp { f = join_expr f1 f2; args = join_expr_list a1 a2; tys })
  | EAppOp { op = op1; args = a1; tys }, EAppOp { op = op2; args = a2; tys = _ } when op1 = op2->
    Mark.add m (EAppOp {op = op1; args = join_expr_list a1 a2; tys })
  | EArray e1, EArray e2 -> Mark.add m (EArray (join_expr_list e1 e2))
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
  | ETuple e1, ETuple e2 -> Mark.add m (ETuple (join_expr_list e1 e2))
  | ETupleAccess { e = e1; index = i1; size = s1}, ETupleAccess { e = e2; index = i2; size = s2}
    when i1 = i2 && s1 = s2 ->
      Mark.add m (ETupleAccess { e = join_expr e1 e2; index = i1; size = s1 })
  | EAssert e1, EAssert e2 -> Mark.add m (EAssert (join_expr e1 e2))
  | EDefault { excepts = x1; just = j1; cons = c1 }, EDefault { excepts = x2; just = j2; cons = c2 } ->
    Mark.add m (EDefault { excepts = join_expr_list x1 x2; just = join_expr j1 j2; cons = join_expr c1 c2 })
  | EPureDefault e1, EPureDefault e2 -> Mark.add m (EPureDefault (join_expr e1 e2))
  | EErrorOnEmpty e1, EErrorOnEmpty e2 -> Mark.add m (EErrorOnEmpty (join_expr e1 e2))
  | _ -> 
    Message.error "@[<v 2>The two expressions cannot be joined@ Expr1 : %a@ Expr2 : %a" 
    Format_trace.expr e1 Format_trace.expr e2

and join_expr_list : type d t. (d, t) gexpr list -> (d, t) gexpr list -> (d, t) gexpr list =
  fun l1 l2 -> List.map2 join_expr l1 l2

let join_ctx ctx1 ctx2 = (Var.Map.union (fun _ v1 v2 -> Some (join_expr v1 v2))) ctx1 ctx2 

let join_ctx_list ctx_list = List.fold_left join_ctx Var.Map.empty ctx_list

let hole = EHole (TAny, Pos.void)
let mark_hole m = Mark.add m hole

let unevaluate :
  type t.
  decl_ctx ->
  ((yes, yes, yes) slicing_interpr_kind, t) gexpr ->
  ((yes, yes, yes) slicing_interpr_kind, t) Trace_ast.t ->
  ((yes, yes, yes) slicing_interpr_kind, t) gexpr =
fun ctx value trace -> 
  let rec unevaluate_aux :
    ((yes, yes, yes) slicing_interpr_kind, t) gexpr ->
    ((yes, yes, yes) slicing_interpr_kind, t) Trace_ast.t ->
    (((yes, yes, yes) slicing_interpr_kind, t) gexpr, ((yes, yes, yes) slicing_interpr_kind, t) gexpr) Var.Map.t * 
    ((yes, yes, yes) slicing_interpr_kind, t) gexpr =
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
    | EAbs { binder = substituted_binder; pos; tys }, TrAbs { binder = original_binder ; _} -> 
      (* There may be variables in the body of the abstraction that have been substituted 
         so to unevaluate the body properly, we have to return the original binder and the context of what substitutions occured *)
      (*
      Message.log "Function : ";
      Format_trace.print_expr v;
      Message.log "Trace Function :";
      Format_trace.print_trace trace;
      Format.print_newline();

      let vars, substituted_body, original_body = Bindlib.unmbind2 substituted_binder original_binder in 
      *)
      
      Var.Map.empty, v
    | _, TrVar {var = x; _} -> Var.Map.singleton x v, Mark.add m (EVar x)
    | _, TrExternal { name } -> Var.Map.empty, Mark.add m (EExternal { name })
    | _, TrApp { trf; trargs; tys; vars; trv } ->
      let local_ctx, e = unevaluate_aux v trv in
      let values = List.map 
        (fun v -> match Var.Map.find_opt v local_ctx with 
          | Some value -> value 
          (*if the variable is not in the context, then it was not used in the body hence we can assign it a hole*)
          | None -> mark_hole m 
        ) 
        (Array.to_list vars)
      in
      let lctx2, e2 = unevaluate_list values trargs in
      let empty_pos = List.init (List.length tys) (fun _ -> Pos.void) in
      let lctx1, e1 = unevaluate_aux (Mark.add m (EAbs { binder = bind vars e; pos =  empty_pos; tys})) trf in
      
      (*match trf with
        | TrAbs { binder = _; pos; tys } -> unevaluate_aux (Mark.add m (EAbs { binder = bind vars e; pos; tys})) trf
        | TrVar { var = _; value = (EAbs { binder = _; pos; tys }, m)} -> 
          unevaluate_aux (Mark.add m (EAbs { binder = bind vars e; pos; tys})) trf
        | _ -> 
          Message.error "@[<v 2> The left term is not a function in the application @ Expr : %a@ Trace : %a@]" 
          Format_trace.expr v Format_trace.trace trace
      in*)
      join_ctx_list (local_ctx::lctx1::lctx2), Mark.add m (EApp {f = e1; args = e2; tys})
    | _, TrAppOp { op = Length, _ as op; trargs; tys; vargs = [(EArray vs, m)]; _ } -> 
      (* The content of the Array is not relevant so we replace each element by a hole *)
      let vargs = [Mark.add m (EArray (List.map (fun v -> mark_hole (Mark.get v)) vs))] in
      unevaluate_op op tys vargs trargs m 
    | EArray vs, TrAppOp { op = Map, _ as op; trargs; tys; vargs = [EAbs _,mf; (EArray _, ma)]; traux } -> 
      (* We slice the expression further *)
      let _, sliced_apps = unevaluate_list vs traux in
      let f, sliced_vs = List.fold_right 
        (fun app (joined_f, vs) -> match app with 
          | EApp {f; args = [v]; _}, _ -> (join_expr f joined_f), v::vs
          | EHole _, _ -> joined_f, mark_hole ma :: vs
          | _ -> assert false
        )
        sliced_apps (mark_hole mf, [])
      in
      unevaluate_op op tys [f; (EArray sliced_vs, ma)] trargs m 
    | EArray vs, TrAppOp { op = Map2, _ as op; trargs; tys; vargs = [EAbs _,mf; (EArray _, m1); (EArray _, m2)]; traux } ->
      let _, sliced_apps = unevaluate_list vs traux in
      let f, sliced_vs1, sliced_vs2 = List.fold_right 
        (fun app (joined_f, vs1, vs2) -> match app with 
          | EApp {f; args = [v1; v2]; _}, _ -> (join_expr f joined_f), v1::vs1, v2::vs2
          | EHole _, _ -> joined_f, mark_hole m1 :: vs1, mark_hole m2 ::vs2
          | _ -> assert false
        )
        sliced_apps
        (mark_hole mf, [], [])
      in
      unevaluate_op op tys [f; (EArray sliced_vs1, m1); (EArray sliced_vs2, m2)] trargs m
    | _ , TrAppOp { op = Reduce, _ as op; trargs; tys; vargs = [_,mf; EAbs _, md; (EArray [], ma) as arr]; traux = [tr]} ->
      let _, sliced_app = unevaluate_aux v tr in
      let sliced_default, sliced_arr = match sliced_app with
        | EApp {f; args = [ELit LUnit, _]; _}, _ -> f, arr
        | EHole _, _ -> mark_hole md, mark_hole ma
        | _ -> assert false
      in 
      unevaluate_op op tys [mark_hole mf; sliced_default; sliced_arr] trargs m 
    | _, TrAppOp { op = Reduce, _ as op; trargs; tys; vargs = [EAbs _, mf; _,md; (EArray (_::_), ma) ]; traux} ->
      let f, v0, sliced_vs = List.fold_left (
        fun (joined_f, acc, vs) tr ->
          match snd (unevaluate_aux acc tr) with
          | EApp {f; args = [acc0; v]; _}, _ -> join_expr joined_f f, acc0, (v::vs)
          | EHole _, m -> joined_f, mark_hole m, ((mark_hole m)::vs)
          | _ -> assert false
        )
        (v, mark_hole mf, []) traux
      in
      unevaluate_op op tys [f; mark_hole md; (EArray (v0::sliced_vs), ma)] trargs m 
    | EArray vs, TrAppOp { op = Filter, _ as op; trargs; tys; vargs = [EAbs _,mf; (EArray _),ma]; traux} -> 
      (* This case is particular because we have to slice each element of the array depending on 
        how they behave with the filter function and then join the values with the sliced result *)
      (* In this case, traux contains alternatively the result of the filter function on an element and the trace of the application *)
      let rec split_list l1 l2 = function
        | [] -> List.rev l1, List.rev l2
        | t1::t2::q -> split_list (t1::l1) (t2::l2) q
        | _ -> assert false
      in
      let bools, trbs = split_list [] [] traux in
      let bools = List.map 
        (fun tb -> match tb with | Trace_ast.TrLit (LBool b) -> b | _ -> assert false) 
        bools 
      in
      let _, sliced_apps = unevaluate_list (List.map (fun b -> ELit(LBool b),m) bools) trbs in
      let f, sliced_arr = List.fold_right 
        (fun app (joined_f, vs) -> match app with 
          | EApp {f; args = [v]; _}, _ -> (join_expr f joined_f), v::vs
          | EHole _, _ -> joined_f, mark_hole ma :: vs
          | _ -> assert false
        )
        sliced_apps (mark_hole mf, [])
      in
      let rec join_relevant_values bools raw filtered = 
        match bools, raw, filtered with
        | [], [], [] -> []
        | false::bs, r::rs, _ -> r::(join_relevant_values bs rs filtered)
        | true::bs, r::rs, f::fs -> (join_expr r f) :: (join_relevant_values bs rs fs)
        | _ -> assert false
      in 
      let sliced_vs = join_relevant_values bools sliced_arr vs in
      unevaluate_op op tys [f; (EArray sliced_vs, ma)] trargs m
    | _, TrAppOp { op = Fold,_ as op; trargs; tys; vargs = [EAbs _, mf; _; (EArray _, ma)]; traux} ->
      let f, v0, sliced_vs = List.fold_left (
        fun (joined_f, acc, vs) tr ->
          match snd (unevaluate_aux acc tr) with
          | EApp {f; args = [acc0; v]; _}, _ -> join_expr joined_f f, acc0, (v::vs)
          | EHole _, m -> joined_f, mark_hole m, ((mark_hole m)::vs)
          | _ -> assert false
        )
        (v, mark_hole mf, []) traux
      in
      unevaluate_op op tys [f; v0; (EArray sliced_vs, ma)] trargs m 
    | _, TrAppOp { op; trargs; tys; vargs; _ } -> 
      unevaluate_op op tys vargs trargs m 
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
    | ETuple vs, TrTuple trs when List.length vs = List.length trs -> 
      let local_ctxs, es = unevaluate_list vs trs in 
      join_ctx_list local_ctxs, Mark.add m (ETuple es)
    | _, TrTupleAccess { tr; index; size } -> 
      let arr = List.init size (fun i -> if i = index then v else mark_hole m) in
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
            (fun c _ -> if c = cons then ec else (mark_hole m)) 
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
          (join_ctx lctxc lctxt), Mark.add m (EIfThenElse { cond; etrue; efalse = mark_hole m})
          )
        | (TrExpr _ |TrHole _), _ -> (*take the false branch*)(
          let lctxc, cond = unevaluate_aux (Mark.add m (ELit(LBool false))) trcond in
          let lctxf, efalse = unevaluate_aux v trfalse in
          (join_ctx lctxc lctxf), Mark.add m (EIfThenElse { cond; etrue = mark_hole m; efalse})
          )
        | _ -> Message.error "Could not identify whether the result of the condition was true or false" 
      )
    | EArray vs, TrArray trs when List.length vs = List.length trs ->
      let local_ctxs, es = unevaluate_list vs trs in 
      join_ctx_list local_ctxs, Mark.add m (EArray es)
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
        let lctxs,excepts = unevaluate_list vexcepts trexcepts in
        let local_ctx = join_ctx_list lctxs in 
        local_ctx, Mark.add m (EDefault { excepts; just = mark_hole m; cons = mark_hole m})
      | _, (TrExpr _ |TrHole _) -> (* The result is obtained from the false justification *)
        let eempty = List.init (List.length trexcepts) (fun _ -> Mark.add m EEmpty) in
        let lctxe, excepts = unevaluate_list eempty trexcepts in 
        let lctxj, just = unevaluate_aux (Mark.add m (ELit(LBool false))) trjust in
        let local_ctx = join_ctx_list (lctxj::lctxe) in
        local_ctx, (Mark.add m (EDefault { excepts; just; cons = mark_hole m }))
      | _, _ -> (* The result is obtained from the consequence *)
        let eempty = List.init (List.length trexcepts) (fun _ -> Mark.add m EEmpty) in
        let lctxe, excepts = unevaluate_list eempty trexcepts in 
        let lctxj, just = unevaluate_aux (Mark.add m (ELit(LBool true))) trjust in
        let lctxc, cons = unevaluate_aux v trcons in
        let local_ctx = join_ctx_list (lctxc::lctxj::lctxe) in
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
          (fun v tr -> if Mark.remove v = EEmpty then Var.Map.empty, mark_hole m else unevaluate_aux v tr)
          vexcepts
          trexcepts
        ) in
        let local_ctx = join_ctx_list lctxs in 
        local_ctx, Mark.add m (EDefault { excepts; just = mark_hole m; cons = mark_hole m})
      | _ -> Message.error "This error in the execution could not be handled by the unevaluation function"
    )
    | _ -> Message.error "@[<v 2>The trace does not match the value@ Expr : %a@ Trace : %a@]" Format_trace.expr v Format_trace.trace trace

  and unevaluate_list v_list trace_list = List.split (List.map2 unevaluate_aux v_list trace_list)
  
  and unevaluate_op op tys vargs trargs m = let lctxs, args = unevaluate_list vargs trargs in 
      join_ctx_list lctxs, Mark.add m (EAppOp { op; args; tys })

  in del_useless_declarations (snd (unevaluate_aux value trace))

let print_slicing_things expr value trace sliced_expr = 
  Message.log "Input program :";
  Format.print_newline();
  Format_trace.print_expr expr;
  Format.print_newline();
  Message.log "Result :";
  Format.print_newline();
  Format_trace.print_expr value;
  Format.print_newline();
  Message.log "Trace :";
  Format.print_newline();
  Format_trace.print_trace trace;
  Message.log "Sliced program :";
  Format.print_newline(); 
  Format_trace.print_expr sliced_expr;
  Format.print_newline()

let slice 
  ?(debug = false)
  (p : (dcalc, 'm) gexpr program)
  (s : ScopeName.t) =
  Message.with_delayed_errors (fun () ->
    let ctx = p.decl_ctx in
    let e = Expr.unbox (Program.to_expr p s) in
    match Interpreter.evaluate_expr p.decl_ctx p.lang (Interpreter.addcustom e) with
    | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e ->
      begin
      (* Get the term to interpret *)
      let application_term = Scope.empty_input_struct_dcalc ctx s_in mark_e in
      let to_interpret =
        Expr.make_app (Expr.box e) [application_term]
          [TStruct s_in, Expr.pos e]
          (Expr.pos e)
      in 
      let e = (Expr.unbox to_interpret) in
      if debug then (
        Message.log "Input program :";
        Format.print_newline();
        Format_trace.print_expr e;
      );
      (* Evaluate the expression with trace *)
      let v, tr = Interpret.evaluate_expr_safe ctx p.lang e in
      if debug then (
        Message.log "Result :";
        Format.print_newline();
        Format_trace.print_expr v;
        Format.print_newline();
        Message.log "Trace :";
        Format.print_newline();
        Format_trace.print_trace tr;
      );
      (* Unevaluate the value with the trace to get a sliced version of the expression *)
      let sliced_e = unevaluate p.decl_ctx (Interpret.addholes v) tr in
      if debug then (
        Message.log "Sliced program :";
        Format.print_newline(); 
        Format_trace.print_expr sliced_e;
        Format.print_newline();
      );

      match Mark.remove v with
      | EStruct _ -> v,sliced_e
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "The interpretation of a program should always yield a struct \
            corresponding to the scope variables"
      end
    | _ ->
      Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
        "The interpreter can only interpret terms starting with functions \
          having thunked arguments")

let test 
  ?(debug = false)
  (p : (dcalc, 'm) gexpr program)
  (s : ScopeName.t) =
  let v, sliced_e = slice ~debug p s in
  let value, trace = Interpret.evaluate_expr_safe p.decl_ctx p.lang (Interpret.delholes sliced_e) in
  if debug then (
    Message.log "Result from sliced program :";
    Format.print_newline();
    Format_trace.print_expr value;
    Format.print_newline();
    Message.log "Trace of sliced program :";
    Format.print_newline();
    Format_trace.print_trace trace;
  );
  Expr.equal v value