open Catala_utils
open Shared_ast

type ('a, 'm) t =
  | TrExpr : ('a, 'm) gexpr -> ('a, 'm) t
  | TrLit : lit -> ('a, 'm) t
  | TrApp : {
      trf : ('a, 'm) t;
      trargs : ('a, 'm) t list;
      tys : typ list;
      vars : ('a, 'm) gexpr Var.t array;
      trv : ('a, 'm) t
    }
      -> ('a, 'm) t
  | TrAppOp : {
      op : 'a operator Mark.pos;
      trargs : ('a, 'm) t list;
      tys : typ list;
      vargs : ('a, 'm) gexpr list
    }
      -> ('a, 'm) t
  | TrArray : ('a, 'm) t list -> ('a, 'm) t
  | TrVar : ('a, 'm) naked_gexpr Bindlib.var -> ('a, 'm) t
  | TrAbs : {
      binder : (('a, 'a, 'm) base_gexpr, ('a, 'm) gexpr) Bindlib.mbinder;
      pos : Pos.t list;
      tys : typ list;
    }
      -> ('a, 'm) t
  | TrIfThenElse : {
      trcond : ('a, 'm) t;
      trtrue : ('a, 'm) t;
      trfalse : ('a, 'm) t;
    }
      -> ('a, 'm) t
  | TrStruct : {
      name : StructName.t;
      fields : ('a, 'm) t StructField.Map.t;
    }
      -> ('a, 'm) t
  | TrInj : {
      name : EnumName.t;
      tr : ('a, 'm) t;
      cons : EnumConstructor.t;
    }
      -> ('a, 'm) t
  | TrMatch : {
      name : EnumName.t;
      tr : ('a, 'm) t;
      cases : ('a, 'm) t EnumConstructor.Map.t;
    }
      -> ('a, 'm) t
  | TrTuple : ('a, 'm) t list -> ('a, 'm) t
  | TrTupleAccess : {
      tr : ('a, 'm) t;
      index : int;
      size : int;
    }
      -> ('a, 'm) t
  (* Early stages *)
  | TrStructAccess : {
      name : StructName.t;
      tr : ('a, 'm) t;
      field : StructField.t;
    }
      -> ('a, 'm) t
      (** Resolved struct/enums, after name resolution in [desugared] *)
  (* Lambda-like *)
  | TrExternal : {
      name : external_ref Mark.pos;
    }
      -> ('a, 'm) t
  | TrAssert : ('a, 'm) t -> ('a, 'm) t
  | TrFatalError : {
      err : Runtime.error;
      tr : ('a, 'm) t
    }
      -> ('a, 'm) t
  (* Default terms *)
  | TrDefault : {
      trexcepts : ('a, 'm) t list;
      vexcepts : ('a, 'm) gexpr list;
      trjust : ('a, 'm) t;
      trcons : ('a, 'm) t;
    }
      -> ('a, 'm) t
  | TrPureDefault : ('a, 'm) t -> ('a, 'm) t
  | TrEmpty : ('a, 'm) t
  | TrErrorOnEmpty : ('a, 'm) t -> ('a, 'm) t
  (* Only used during evaluation *)
  | TrCustom : {
      obj : Obj.t;
      targs : typ list;
      tret : typ;
    }
      -> ('a, 'm) t
      (** A function of the given type, as a runtime OCaml object. The specified
          types for arguments and result must be the Catala types corresponding
          to the runtime types of the function. *)
  | TrHole : typ -> ('a, 'm) t