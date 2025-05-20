open Catala_utils
open Shared_ast

type 'a t =
  (* Constructors common to all ASTs *)
  | TrExpr : ('a, 'm) gexpr -> 'a t
  | TrLit : lit -> 'a t
  | TrApp : {
      trf : 'a t;
      trargs : 'a t list;
          (** length may be 1 even if arity > 1 in desugared. scopelang performs
              detuplification, so length = arity afterwards *)
      tys : typ list;  (** Set to [[]] before disambiguation *)
      trv : 'a t
    }
      -> 'a t
  | TrAppOp : {
      op : 'a operator Mark.pos;
      trargs : 'a t list;
      tys : typ list;
      trv : 'a t
    }
      -> 'a t
  | TrArray : 'a t list -> 'a t
  | TrVar : ('a, 'm) naked_gexpr Bindlib.var -> 'a t
  | TrAbs : {
      binder : (('a, 'a, 'm) base_gexpr, ('a, 'm) gexpr) Bindlib.mbinder;
      pos : Pos.t list;
      tys : typ list;
    }
      -> 'a t
  | TrIfThenElse : {
      trcond : 'a t;
      trtrue : 'a t;
      trfalse : 'a t;
    }
      -> 'a t
  | TrStruct : {
      name : StructName.t;
      fields : 'a t StructField.Map.t;
    }
      -> 'a t
  | TrInj : {
      name : EnumName.t;
      tr : 'a t;
      cons : EnumConstructor.t;
    }
      -> 'a t
  | TrMatch : {
      name : EnumName.t;
      tr : 'a t;
      cases : 'a t EnumConstructor.Map.t;
    }
      -> 'a t
  | TrTuple : 'a t list -> 'a t
  | TrTupleAccess : {
      tr : 'a t;
      index : int;
      size : int;
    }
      -> 'a t
  (* Early stages *)
  | TrStructAccess : {
      name : StructName.t;
      tr : 'a t;
      field : StructField.t;
    }
      -> 'a t
      (** Resolved struct/enums, after name resolution in [desugared] *)
  (* Lambda-like *)
  | TrExternal : {
      name : external_ref Mark.pos;
    }
      -> 'a t
  | TrAssert : 'a t -> 'a t
  | TrFatalError : {
      err : Runtime.error;
      tr : 'a t
    }
      -> 'a t
  (* Default terms *)
  | TrDefault : {
      trexcepts : 'a t list;
      trjust : 'a t;
      trcons : 'a t;
    }
      -> 'a t
  | TrPureDefault : 'a t -> 'a t
  (** "return" of a pure term, so that it can be typed as [default] *)
  | TrEmpty : 'a t
  | TrErrorOnEmpty : 'a t -> 'a t
  (* Only used during evaluation *)
  | TrCustom : {
      obj : Obj.t;
      targs : typ list;
      tret : typ;
    }
      -> 'a t
      (** A function of the given type, as a runtime OCaml object. The specified
          types for arguments and result must be the Catala types corresponding
          to the runtime types of the function. *)
  | TrHole : typ -> 'a t