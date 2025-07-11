open Catala_utils
open Shared_ast
open Trace_ast


let hole = EHole (TAny, Pos.void)
let mark_hole m = Mark.add m hole

(* Typing shenanigan to handle hole terms in the AST type. *)
let addholes e =
  let rec f :
      type d c h.
      ((d, c, h) slicing_interpr_kind, 't) gexpr -> 
      ((d, c, yes) slicing_interpr_kind, 't) gexpr boxed
  = function
    | (ECustom _, _) as e -> Expr.map ~f e
    | EAppOp { op; tys; args }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmpty, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | (EPos _, _) as e -> Expr.map ~f e
    | EFatalError Runtime.Unreachable, m -> Expr.map ~f @@ mark_hole m
    | ( ( EAssert _ | EFatalError _ | ELit _ | EApp _ | EArray _ | EVar _
        | EExternal _ | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _
        | EInj _ | EStruct _ | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | (EHole _, _) as e -> Expr.map ~f e
    | _ -> .
  in
  let open struct
    external id :
      (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr -> 
        (('d, 'c, yes) slicing_interpr_kind, 't) gexpr
      = "%identity"
  end in
  if false then Expr.unbox (f e)
    (* We keep the implementation as a typing proof, but bypass the AST
       traversal for performance. Note that it's not completely 1-1 since the
       traversal would do a reboxing of all bound variables *)
  else id e

let delholes e =
  let rec f :
      type d c h.
      ((d, c, h) slicing_interpr_kind, 't) gexpr -> 
      ((d, c, no) slicing_interpr_kind, 't) gexpr boxed
  = function
    | EHole _, m -> Expr.efatalerror Unreachable m
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

(* Trace constructors *)
let trexpr e = TrExpr e

let trlit l = TrLit l

let trapp ~trf ~trargs ~tys ~vars ~trv =
  TrApp { trf; trargs; tys; vars; trv }

let trappcustom ~trcustom ~custom ~trargs ~vargs ~tys ~v =
  TrAppCustom { trcustom; custom; trargs; tys; vargs; v }

let trappop ~op ~trargs ~tys ~vargs ~traux =
  TrAppOp { op; trargs; tys; vargs; traux }

let trarray ts = TrArray ts

let trvar ~var ~value = TrVar { var; value }

let trabs ~binder ~pos ~tys = TrAbs { binder; pos; tys }

let trcontextclosure ~context ~tr = 
  if context = Var.Map.empty then tr 
  else TrContextClosure { context; tr }

let trifthenelse ~trcond ~trtrue ~trfalse =
  TrIfThenElse { trcond; trtrue; trfalse }

let trstruct ~name ~fields =
  TrStruct { name; fields }

let trinj ~name ~tr ~cons =
  TrInj { name; tr; cons }

let trmatch ~name ~tr ~cases =
  TrMatch { name; tr; cases }

let trtuple ts = TrTuple ts

let trtupleaccess ~tr ~index ~size =
  TrTupleAccess { tr; index; size }

let trstructaccess ~name ~tr ~field =
  TrStructAccess { name; tr; field }

let trexternal ~name = TrExternal { name }

let trassert t = TrAssert t

let trfatalerror ~err ~tr = TrFatalError {err; tr}

let trdefault ~trexcepts ~vexcepts ~trjust ~trcons =
  TrDefault { trexcepts; vexcepts; trjust; trcons }

let trpuredefault t = TrPureDefault t

let trempty = TrEmpty

let trerroronempty t = TrErrorOnEmpty t

let trcustom ~obj ~targs ~tret = TrCustom { obj; targs; tret }

let trhole ty = TrHole ty

let tranyhole = TrHole (TAny, Pos.void)

let substitute_bounded_vars :
  type c d h.
  (((d, c, h) slicing_interpr_kind, 't) gexpr, 
  ((d, c, h) slicing_interpr_kind, 't) gexpr) Var.Map.t -> 
  ((d, c, h) slicing_interpr_kind, 't) gexpr ->
  (((d, c, h) slicing_interpr_kind, 't) gexpr, 
  ((d, c, h) slicing_interpr_kind, 't) gexpr) Var.Map.t *
  ((d, c, h) slicing_interpr_kind, 't) gexpr =
fun ctx_closure e ->
  let join = Var.Map.union (fun _ v _ -> Some v) in
  let expr_map_gather ~f e = 
    Expr.map_gather ~acc:Var.Map.empty ~join ~f e in
  let rec f :
      ((d, c, h) slicing_interpr_kind, 't) gexpr -> 
      (((d, c, h) slicing_interpr_kind, 't) gexpr, 
      ((d, c, h) slicing_interpr_kind, 't) gexpr) Var.Map.t *
      ((d, c, h) slicing_interpr_kind, 't) gexpr boxed
    = function
    | EVar x, m -> (
      match Var.Map.find_opt x ctx_closure with
        | Some v -> 
          let acc, v' = f v in
          Var.Map.add x v acc, v'
        | None -> Var.Map.empty, Expr.evar x m
      )
    | EHole _, _ as e -> expr_map_gather ~f e
    | (ECustom _, _) as e -> expr_map_gather ~f e
    | EAppOp { op; args; tys }, m ->
      let accs, args = List.split(List.map f args) in
      List.fold_left join Var.Map.empty accs, Expr.eappop ~tys ~args ~op m
    | (EDefault _, _) as e ->  expr_map_gather ~f e
    | (EPureDefault _, _) as e ->  expr_map_gather ~f e
    | (EEmpty, _) as e ->  expr_map_gather ~f e
    | (EErrorOnEmpty _, _) as e ->  expr_map_gather ~f e
    | (EPos _, _) as e ->  expr_map_gather ~f e
    | ( ( EAssert _ | EFatalError _ | ELit _ | EApp _ | EArray _ 
        | EExternal _ | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _
        | EInj _ | EStruct _ | EStructAccess _ | EMatch _ ),
        _ ) as e ->
       expr_map_gather ~f e
    | _ -> .
  in
  let ctx, e = f e in
  ctx, Expr.unbox e

let rec is_sub_expr: type d t. (d, t) gexpr -> (d, t) gexpr -> bool =
fun e1 e2 -> match Mark.remove e1, Mark.remove e2 with 
  | EHole _, _ -> true
  | _, EHole _ -> false
  | ELit l1, ELit l2 -> l1 = l2
  | EEmpty, EEmpty -> true
  | EFatalError err1, EFatalError err2 -> err1 = err2
  | EAbs {binder = b1; _}, EAbs {binder = b2; _} -> 
    let _, e1, e2 = Bindlib.unmbind2 b1 b2 in is_sub_expr e1 e2
  | ECustom {obj = o1; _}, ECustom {obj = o2; _} -> o1 = o2
  | EVar x1, EVar x2 -> Bindlib.eq_vars x1 x2
  | EExternal _, EExternal _ -> Expr.equal e1 e2
  | EApp { f = f1; args = a1; _ }, EApp { f = f2; args = a2; _ } ->
    List.for_all2 is_sub_expr (f1::a1) (f2::a2)
  | EAppOp { op = op1; args = a1; _ }, EAppOp { op = op2; args = a2; _ } ->
    Mark.equal Operator.equal op1 op2 && List.for_all2 is_sub_expr a1 a2
  | EArray e1, EArray e2 -> List.for_all2 is_sub_expr e1 e2
  | EIfThenElse { cond = c1; etrue = t1; efalse = f1 }, 
    EIfThenElse { cond = c2; etrue = t2; efalse = f2 } ->
    List.for_all2 is_sub_expr [c1;t1;f1] [c2;t2;f2]
  | EStruct { name = n1; fields = f1 }, EStruct { name = n2; fields = f2 } ->
    (* The name StructField.Map.equal is misleading, 
       it is just a forall2 for StructField.Map *)
    StructName.equal n1 n2 && StructField.Map.equal is_sub_expr f1 f2
  | EStructAccess { name = n1; e = e1; field = f1 }, 
    EStructAccess { name = n2; e = e2; field = f2 } ->
    StructName.equal n1 n2 && StructField.equal f1 f2 && is_sub_expr e1 e2
  | EInj { name = n1; e = e1; cons = c1 }, 
    EInj { name = n2; e = e2; cons = c2 } ->
    EnumName.equal n1 n2 && EnumConstructor.equal c1 c2 && is_sub_expr e1 e2
  | EMatch { name = n1; e = e1; cases = c1 }, 
    EMatch { name = n2; e = e2; cases = c2 } ->
    (* The name EnumConstructor.Map.equal is misleading,
       it is just a forall2 for EnumConstructor.Map *)
    EnumName.equal n1 n2 && is_sub_expr e1 e2 && 
    EnumConstructor.Map.equal is_sub_expr c1 c2
  | ETuple e1, ETuple e2 -> List.for_all2 is_sub_expr e1 e2
  | ETupleAccess { e = e1; index = i1; size = s1}, 
    ETupleAccess { e = e2; index = i2; size = s2} ->
    i1 = i2 && s1 = s2 && is_sub_expr e1 e2
  | EAssert e1, EAssert e2 -> is_sub_expr e1 e2
  | EDefault { excepts = x1; just = j1; cons = c1 }, 
    EDefault { excepts = x2; just = j2; cons = c2 } ->
    List.for_all2 is_sub_expr (c1::j1::x1) (c2::j2::x2)
  | EPureDefault e1, EPureDefault e2 -> is_sub_expr e1 e2
  | EErrorOnEmpty e1, EErrorOnEmpty e2 -> is_sub_expr e1 e2
  | _ -> false

(* Helpers for Result *)

let ( let* ) = Result.bind

let ok map v tr = Ok (map,v,tr)

let error err m trace = Error (err, m, trace)

let map_error_trace trace_wrapper r = 
  Result.map_error (fun (err, m, tr) -> err, m, trace_wrapper err tr) r

let union_map m1 m2 = Var.Map.union (fun _ v _ -> Some v) m1 m2

let map_result_with_trace f lst =
  let rec aux accmap accv acctr rev_prefix = function
    | [] -> Ok (accmap, List.rev accv, List.rev acctr)
    | x :: xs ->
      match f x with
      | Ok (map, v, tr) -> aux (union_map accmap map) 
        (v :: accv) (tr :: acctr) (x :: rev_prefix) xs
      | Error (err, m, trfail) ->
          let rev_prefix_tr = List.map trexpr rev_prefix in
          let rest_tr = List.map trexpr xs in
          let full_tr = List.rev_append rev_prefix_tr (trfail::rest_tr) in
          error err m full_tr
  in
  aux Var.Map.empty [] [] [] lst

let map_result_with_trace2 f l1 l2 =
  let rec aux accmap accv acctr rev_prefix = function
    | [], [] -> Ok (accmap, List.rev accv, List.rev acctr)
    | x1 :: xs1, x2 :: xs2 -> (
      match f x1 x2 with
      | Ok (map, v, tr) -> aux (union_map accmap map) 
        (v :: accv) (tr :: acctr) ((x1, x2) :: rev_prefix) (xs1, xs2)
      | Error (err, m, trfail) ->
        let rev_prefix_tr = List.map (fun _ -> tranyhole) rev_prefix in
        let rest_tr = List.map (fun _ -> tranyhole) xs1 in
        let full_tr
         = List.rev_append rev_prefix_tr (trfail :: rest_tr) in
        error err m full_tr
    )
    | _ -> raise (Invalid_argument "map_result_with_trace2")
  in
  aux Var.Map.empty [] [] [] (l1, l2)

let fold_result_with_trace f acc0 lst =
  let rec aux accmap acc tracc = function
    | [] -> Ok (accmap, acc, List.rev tracc)
    | x :: xs ->
        match f acc x with
        | Ok (map, acc', tr) -> 
          aux (union_map accmap map) acc' (tr :: tracc) xs
        | Error (err, m, trfail) ->
            let rest_tr = List.map (fun _ -> tranyhole) xs in
            let full_tr = List.rev_append tracc (trfail::rest_tr) in
            error err m full_tr
  in
  aux Var.Map.empty acc0 [] lst

let filter_result_with_trace f lst =
  let rec aux accmap accv acctr = function
    | [] -> Ok (accmap, List.rev accv, List.rev acctr)
    | x :: xs ->
      match f x with
      | Ok (map, (ELit (LBool b),_), tr_call) ->
        let acctr' = (trlit (LBool b) :: tr_call :: acctr) in
        let accv' = if b then x :: accv else accv in
        aux (union_map accmap map) accv' acctr' xs
      | Error (err, m, trfail) ->
        let rev_prefix_tr = List.map (fun _ -> tranyhole) acctr in
        let rest_tr = List.init (List.length xs * 2) (fun _ -> tranyhole) in
        let full_tr = List.rev_append rev_prefix_tr (trfail :: rest_tr) in
        error err m full_tr
      | _ -> Message.error
        "fold_filter_result : This predicate evaluated to something else\ 
         than a boolean (should not happen if the term was well-typed)"
  in
  aux Var.Map.empty [] [] lst



