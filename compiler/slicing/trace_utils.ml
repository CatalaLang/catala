open Catala_utils
open Shared_ast
open Trace_ast

(* Typing shenanigan to add hole terms to the AST type. *)
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
      (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr -> (('d, 'c, yes) slicing_interpr_kind, 't) gexpr
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
let trhole ty = TrHole ty

let tranyhole = TrHole (TAny, Pos.void)

let trexpr e = TrExpr (addholes e)

let trlit l = TrLit l

let trapp ~trf ~trargs ~tys ~vars ~trv =
  TrApp { trf; trargs; tys; vars; trv }

let trappcustom ~trcustom ~custom ~trargs ~vargs ~tys ~v =
  TrAppCustom { trcustom; custom = addholes custom; trargs; tys; vargs= List.map addholes vargs; v=addholes v }

let trappop ~op ~trargs ~tys ~vargs ~traux =
  TrAppOp { op = Operator.translate op; trargs; tys; vargs= List.map addholes vargs; traux }

let trarray ts = TrArray ts

let trvar ~var ~value = TrVar { var; value }

let trabs ~binder ~pos ~tys = TrAbs { binder; pos; tys }

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

let trfatalerror err = TrFatalError err

let trdefault ~trexcepts ~vexcepts ~trjust ~trcons =
  TrDefault { trexcepts; vexcepts = (List.map addholes vexcepts); trjust; trcons }

let trpuredefault t = TrPureDefault t

let trempty = TrEmpty

let trerroronempty t = TrErrorOnEmpty t

let trcustom ~obj ~targs ~tret =
  TrCustom { obj; targs; tret }

(* Helpers for Result *)

let ( let* ) = Result.bind

let ok e tr = Ok (e,tr)

let error err m trace = Error (err, m, trace)

let map_error_trace trace_wrapper r = 
  Result.map_error (fun (err, m, tr) -> err, m, trace_wrapper tr) r

let map_result_with_trace f lst =
  let rec aux accv acctr rev_prefix = function
    | [] -> Ok (List.rev accv, List.rev acctr)
    | x :: xs ->
      match f x with
      | Ok (v, tr) -> aux (v :: accv) (tr :: acctr) (x :: rev_prefix) xs
      | Error (err, m, trfail) ->
          let rev_prefix_trace = List.map trexpr rev_prefix in
          let rest_trace = List.map trexpr xs in
          let full_trace = List.rev_append rev_prefix_trace (trfail::rest_trace) in
          error err m full_trace
  in
  aux [] [] [] lst

let map_result_with_trace2 f l1 l2 =
  let rec aux accv acctr rev_prefix = function
    | [], [] -> Ok (List.rev accv, List.rev acctr)
    | x1 :: xs1, x2 :: xs2 -> (
      match f x1 x2 with
      | Ok (v, tr) -> aux (v :: accv) (tr :: acctr) ((x1, x2) :: rev_prefix) (xs1, xs2)
      | Error (err, m, trfail) ->
        let trace_before = List.map (fun _ -> tranyhole) rev_prefix in
        let trace_after = List.map (fun _ -> tranyhole) xs1 in
        let full_trace = List.rev_append trace_before (trfail :: trace_after) in
        error err m full_trace
    )
    | _ -> Message.error "map_result_with_trace2: input lists must have the same length"
  in
  aux [] [] [] (l1, l2)

let fold_result_with_trace f acc0 lst =
  let rec aux acc tracc = function
    | [] -> Ok (acc, List.rev tracc)
    | x :: xs ->
        match f acc x with
        | Ok (acc', tr) -> aux acc' (tr :: tracc) xs
        | Error (err, m, trfail) ->
            let trace_after = List.map (fun _ -> tranyhole) xs in
            let full_trace = List.rev_append tracc (trfail::trace_after) in
            error err m full_trace
  in
  aux acc0 [] lst

let filter_result_with_trace f lst =
  let rec aux accv acctr = function
    | [] -> Ok (List.rev accv, List.rev acctr)
    | x :: xs ->
      match f x with
      | Ok ((ELit (LBool b),_), tr_call) ->
        let acctr' = (trlit (LBool b) :: tr_call :: acctr) in
        let accv' = if b then x :: accv else accv in
        aux accv' acctr' xs
      | Error (err, m, trfail) ->
        let rev_prefix_trace = List.map (fun _ -> tranyhole) acctr in
        let rest_trace = List.init (List.length xs * 2) (fun _ -> tranyhole) in
        let full_trace = List.rev_append rev_prefix_trace (trfail :: rest_trace) in
        error err m full_trace
      | _ -> Message.error
        "fold_filter_result : This predicate evaluated to something else than a boolean \
          (should not happen if the term was well-typed)"
  in
  aux [] [] lst



