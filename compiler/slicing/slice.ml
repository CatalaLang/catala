open Catala_utils
open Shared_ast

(*let slice_trace (trace_with_holes : ('a, 'm) Trace_ast.t) : 'a program = assert false*)

let get_fields _ = assert false
(*
let rec unevaluate ctx v (trace : ('a, 'm) Trace_ast.t) = 
  let m = Mark.get v in
  match Mark.remove v, trace with
  (*| EHole t, _ -> v*)
  | _, TrExpr _ -> Message.error "This case should not happen, TrExpr cannot be unevaluated"
  (* Constants *)
  | ELit l1, TrLit l2 when l1 == l2 -> v
  | EEmpty, TrEmpty -> v
  | EAbs { binder = b1; pos = p1; tys = t1 }, 
    TrAbs { binder = b2; pos = p2; tys = t2 }
    when b1 == b2 && p1 == p2 && t1 == t2 -> v
  | ECustom { obj=o1; targs=ta1; tret=tr1 }, 
    TrCustom { obj=o2; targs=ta2; tret=tr2 }
    when o1 == o2 && ta1 == ta2 && tr1 == tr2 -> v
  (*Others*)
  | _, TrVar x -> Mark.add m (EVar x)  
  | _, TrExternal { name } -> Mark.add m (EExternal { name })
  (*| v, TrApp { trf; trargs; tys; trv } -> 
    (match trf with
      | TrAbs { binder; pos; tys } ->
        let e = unevaluate v trv in
        let 
        let es = List.map2 unevaluate 
    )
  *)
  (*| v, TrAppOpp*)
  | _, TrStructAccess { name; tr; field } ->
    (* Context is needed to make sure we unevaluate a valid struct *)
    let fields = get_fields name in
    let values = List.init (List.length fields) (EHole TAny) in
    let fields_map = StructField.Map.of_list (List.combine fields values) in 
    let fields_map = StructField.Map.update field (fun _ -> Some v) fields_map in
    let e = unevaluate (EStruct { name; fields = fields_map }) tr in
    Mark.add m (EStructAccess { name; e; field})
  | _ -> .
*)
let rec unevaluate :
  type d t.
  decl_ctx ->
  Global.backend_lang ->
  ((d, yes) interpr_kind, t) gexpr ->
  ((d, yes) interpr_kind, t) Trace_ast.t ->
  ((d, yes) interpr_kind, t) Var.Map.t * ((d, yes) interpr_kind, t) gexpr =
  fun ctx lang value trace -> assert false
