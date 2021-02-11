module D = Dcalc.Ast
type lit =
    LBool of bool
  | LInt of Z.t
  | LRat of Q.t
  | LMoney of Z.t
  | LUnit
  | LDate of D.date
  | LDuration of D.duration
type except = ConflictError | EmptyError | Crash
type expr =
    EVar of expr Bindlib.var Utils.Pos.marked
  | ETuple of expr Utils.Pos.marked list * D.StructName.t option
  | ETupleAccess of expr Utils.Pos.marked * int * D.StructName.t option *
      D.typ Utils.Pos.marked list
  | EInj of expr Utils.Pos.marked * int * D.EnumName.t *
      D.typ Utils.Pos.marked list
  | EMatch of expr Utils.Pos.marked * expr Utils.Pos.marked list *
      D.EnumName.t
  | EArray of expr Utils.Pos.marked list
  | ELit of lit
  | EAbs of Utils.Pos.t * (expr, expr Utils.Pos.marked) Bindlib.mbinder *
      D.typ Utils.Pos.marked list
  | EApp of expr Utils.Pos.marked * expr Utils.Pos.marked list
  | EAssert of expr Utils.Pos.marked
  | EOp of D.operator
  | EIfThenElse of expr Utils.Pos.marked * expr Utils.Pos.marked *
      expr Utils.Pos.marked
  | ERaise of except
  | ECatch of expr Utils.Pos.marked * except * expr Utils.Pos.marked
module Var :
  sig
    type t = expr Bindlib.var
    val make : string Utils.Pos.marked -> t
    val compare : 'a Bindlib.var -> 'b Bindlib.var -> int
  end
module VarMap :
  sig
    type key = Var.t
    type 'a t = 'a Stdlib__map.Make(Var).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type vars = expr Bindlib.mvar
val make_var : Var.t Utils.Pos.marked -> expr Utils.Pos.marked Bindlib.box
val make_abs :
  vars ->
  expr Utils.Pos.marked Bindlib.box ->
  Utils.Pos.t ->
  D.typ Utils.Pos.marked list ->
  Utils.Pos.t -> expr Utils.Pos.marked Bindlib.box
val make_app :
  expr Utils.Pos.marked Bindlib.box ->
  expr Utils.Pos.marked Bindlib.box list ->
  Utils.Pos.t -> expr Utils.Pos.marked Bindlib.box
val make_let_in :
  Var.t ->
  D.typ Utils.Pos.marked ->
  expr Utils.Pos.marked Bindlib.box ->
  expr Utils.Pos.marked Bindlib.box -> expr Utils.Pos.marked Bindlib.box
type binder = (expr, expr Utils.Pos.marked) Bindlib.binder
type program = {
  decl_ctx : D.decl_ctx;
  scopes : (Var.t * expr Utils.Pos.marked) list;
}
