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
let trexpr e = TrExpr e

let trlit l = TrLit l

let trapp ~trf ~trargs ~tys ~vars ~trv =
  TrApp { trf; trargs; tys; vars; trv }

let trappop ~op ~trargs ~tys ~vargs ~traux =
  TrAppOp { op; trargs; tys; vargs; traux }

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

let trfatalerror ~err ~tr = TrFatalError { err; tr }

let trdefault ~trexcepts ~vexcepts ~trjust ~trcons =
  TrDefault { trexcepts; vexcepts; trjust; trcons }

let trpuredefault t = TrPureDefault t

let trempty = TrEmpty

let trerror_on_empty t = TrErrorOnEmpty t

let trcustom ~obj ~targs ~tret =
  TrCustom { obj; targs; tret }

let trhole ty = TrHole ty
