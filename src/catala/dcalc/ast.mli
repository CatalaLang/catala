module ScopeName :
  sig
    type t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
module StructName :
  sig
    type t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
module StructFieldName :
  sig
    type t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
module StructMap :
  sig
    type key = StructName.t
    type +'a t
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
module EnumName :
  sig
    type t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
module EnumConstructor :
  sig
    type t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
  end
module EnumMap :
  sig
    type key = EnumName.t
    type +'a t
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
type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration
type typ =
    TLit of typ_lit
  | TTuple of typ Utils.Pos.marked list * StructName.t option
  | TEnum of typ Utils.Pos.marked list * EnumName.t
  | TArrow of typ Utils.Pos.marked * typ Utils.Pos.marked
  | TArray of typ Utils.Pos.marked
  | TAny
type date = CalendarLib.Date.t
type duration = CalendarLib.Date.Period.t
type lit =
    LBool of bool
  | LEmptyError
  | LInt of Z.t
  | LRat of Q.t
  | LMoney of Z.t
  | LUnit
  | LDate of date
  | LDuration of duration
type op_kind = KInt | KRat | KMoney | KDate | KDuration
type ternop = Fold
type binop =
    And
  | Or
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
  | Map
  | Filter
type log_entry = VarDef | BeginCall | EndCall | PosRecordIfTrueBool
type unop =
    Not
  | Minus of op_kind
  | ErrorOnEmpty
  | Log of log_entry * Utils.Uid.MarkedString.info list
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear
type operator = Ternop of ternop | Binop of binop | Unop of unop
type expr =
    EVar of expr Bindlib.var Utils.Pos.marked
  | ETuple of expr Utils.Pos.marked list * StructName.t option
  | ETupleAccess of expr Utils.Pos.marked * int * StructName.t option *
      typ Utils.Pos.marked list
  | EInj of expr Utils.Pos.marked * int * EnumName.t *
      typ Utils.Pos.marked list
  | EMatch of expr Utils.Pos.marked * expr Utils.Pos.marked list * EnumName.t
  | EArray of expr Utils.Pos.marked list
  | ELit of lit
  | EAbs of Utils.Pos.t * (expr, expr Utils.Pos.marked) Bindlib.mbinder *
      typ Utils.Pos.marked list
  | EApp of expr Utils.Pos.marked * expr Utils.Pos.marked list
  | EAssert of expr Utils.Pos.marked
  | EOp of operator
  | EDefault of expr Utils.Pos.marked list * expr Utils.Pos.marked *
      expr Utils.Pos.marked
  | EIfThenElse of expr Utils.Pos.marked * expr Utils.Pos.marked *
      expr Utils.Pos.marked
type struct_ctx = (StructFieldName.t * typ Utils.Pos.marked) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Utils.Pos.marked) list EnumMap.t
type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx; }
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
  typ Utils.Pos.marked list ->
  Utils.Pos.t -> expr Utils.Pos.marked Bindlib.box
val make_app :
  expr Utils.Pos.marked Bindlib.box ->
  expr Utils.Pos.marked Bindlib.box list ->
  Utils.Pos.t -> expr Utils.Pos.marked Bindlib.box
val make_let_in :
  Var.t ->
  typ Utils.Pos.marked ->
  expr Utils.Pos.marked Bindlib.box ->
  expr Utils.Pos.marked Bindlib.box -> expr Utils.Pos.marked Bindlib.box
val make_multiple_let_in :
  Var.t array ->
  typ Utils.Pos.marked list ->
  expr Utils.Pos.marked list Bindlib.box ->
  expr Utils.Pos.marked Bindlib.box -> expr Utils.Pos.marked Bindlib.box
type binder = (expr, expr Utils.Pos.marked) Bindlib.binder
type program = {
  decl_ctx : decl_ctx;
  scopes : (Var.t * expr Utils.Pos.marked) list;
}
