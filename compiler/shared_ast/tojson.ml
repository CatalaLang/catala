module Runtime = Runtime_ocaml.Runtime
module Ast = Definitions

type typ_lit = Definitions.typ_lit =
  | TBool
  | TUnit
  | TInt
  | TRat
  | TMoney
  | TDate
  | TDuration
[@@deriving yojson]

type typ =
  | TLit of typ_lit
  | TTuple of typ list
  | TStruct of string
  | TEnum of string
  | TOption of typ
  | TArrow of typ * typ
  | TArray of typ
  | TAny
[@@deriving yojson]

open Catala_utils

let info_to_string u = Uid.MarkedString.to_string u

let rec ser_typ typ =
  match Marked.unmark typ with
  | Ast.TLit l -> TLit l
  | Ast.TTuple l -> TTuple (List.map ser_typ l)
  | Ast.TStruct l -> TStruct (Ast.StructName.get_info l |> info_to_string)
  | Ast.TEnum l -> TEnum (Ast.EnumName.get_info l |> info_to_string)
  | Ast.TOption l -> TOption (ser_typ l)
  | Ast.TArrow (l1, l2) -> TArrow (ser_typ l1, ser_typ l2)
  | Ast.TArray l -> TArray (ser_typ l)
  | Ast.TAny -> TAny

type ctx

let find_scope_name : ctx -> string -> Ast.ScopeName.t = assert false
let find_struct_name : ctx -> string -> Ast.StructName.t = assert false
let find_struct_field : ctx -> string -> Ast.StructField.t = assert false
let find_enum_name : ctx -> string -> Ast.EnumName.t = assert false

let find_enum_constructor : ctx -> string -> Ast.EnumConstructor.t =
  assert false

let find_rule_name : ctx -> string -> Ast.RuleName.t = assert false
let find_label_name : ctx -> string -> Ast.LabelName.t = assert false
let find_scope_var : ctx -> string -> Ast.ScopeVar.t = assert false
let find_sub_scope_name : ctx -> string -> Ast.SubScopeName.t = assert false
let find_state_name : ctx -> string -> Ast.StateName.t = assert false

let rec unser_typ ctx typ =
  ( (match typ with
    | TLit l -> Ast.TLit l
    | TTuple l -> Ast.TTuple (List.map (unser_typ ctx) l)
    | TStruct l -> Ast.TStruct (find_struct_name ctx l)
    | TEnum l -> Ast.TEnum (find_enum_name ctx l)
    | TOption l -> Ast.TOption (unser_typ ctx l)
    | TArrow (l1, l2) -> Ast.TArrow (unser_typ ctx l1, unser_typ ctx l2)
    | TArray l -> Ast.TArray (unser_typ ctx l)
    | TAny -> Ast.TAny),
    Pos.no_pos )

type date = Runtime.date [@@deriving yojson]
type duration = Runtime.duration [@@deriving yojson]

type log_entry = VarDef of typ | BeginCall | EndCall | PosRecordIfTrueBool
[@@deriving yojson]

type op =
  | Not
  | GetDay
  | GetMonth
  | GetYear
  | FirstDayOfMonth
  | LastDayOfMonth
  | Length
  | Log of log_entry * string list
  | And
  | Or
  | Xor
  | Eq
  | Map
  | Concat
  | Filter
  | Reduce
  | Minus_int
  | Minus_rat
  | Minus_mon
  | Minus_dur
  | ToRat_int
  | ToRat_mon
  | ToMoney_rat
  | Round_rat
  | Round_mon
  | Add_int_int
  | Add_rat_rat
  | Add_mon_mon
  | Add_dat_dur
  | Add_dur_dur
  | Sub_int_int
  | Sub_rat_rat
  | Sub_mon_mon
  | Sub_dat_dat
  | Sub_dat_dur
  | Sub_dur_dur
  | Mult_int_int
  | Mult_rat_rat
  | Mult_mon_rat
  | Mult_dur_int
  | Div_int_int
  | Div_rat_rat
  | Div_mon_rat
  | Div_mon_mon
  | Lt_int_int
  | Lt_rat_rat
  | Lt_mon_mon
  | Lt_dat_dat
  | Lt_dur_dur
  | Lte_int_int
  | Lte_rat_rat
  | Lte_mon_mon
  | Lte_dat_dat
  | Lte_dur_dur
  | Gt_int_int
  | Gt_rat_rat
  | Gt_mon_mon
  | Gt_dat_dat
  | Gt_dur_dur
  | Gte_int_int
  | Gte_rat_rat
  | Gte_mon_mon
  | Gte_dat_dat
  | Gte_dur_dur
  | Eq_int_int
  | Eq_rat_rat
  | Eq_mon_mon
  | Eq_dur_dur
  | Eq_dat_dat
  | Fold
[@@deriving yojson]

type except = ConflictError | EmptyError | NoValueProvided | Crash
[@@deriving yojson]

type glit =
  | LBool : bool -> glit
  | LEmptyError : glit
  | LInt : Runtime.integer -> glit
  | LRat : Runtime.decimal -> glit
  | LMoney : Runtime.money -> glit
  | LUnit : glit
  | LDate : date -> glit
  | LDuration : duration -> glit
[@@deriving yojson]

type glocation =
  | DesugaredScopeVar : string * string option -> glocation
  | ScopelangScopeVar : string -> glocation
  | SubScopeVar : string * string * string -> glocation
[@@deriving yojson]

type gexpr =
  | ELit of glit
  | EApp : { f : gexpr; args : gexpr list } -> gexpr
  | EOp : { op : op; tys : typ list } -> gexpr
  | EArray : gexpr list -> gexpr
  | EVar : string -> gexpr
  | EAbs : { binder : string list * gexpr; tys : typ list } -> gexpr
  | EIfThenElse : { cond : gexpr; etrue : gexpr; efalse : gexpr } -> gexpr
  | EStruct : { name : string; fields : (string * gexpr) list } -> gexpr
  | EInj : { name : string; e : gexpr; cons : string } -> gexpr
  | EMatch : {
      name : string;
      e : gexpr;
      cases : (string * gexpr) list;
    }
      -> gexpr
  (* Early stages *)
  | ELocation : glocation -> gexpr
  | EScopeCall : { scope : string; args : (string * gexpr) list } -> gexpr
  | EDStructAccess : {
      name_opt : string option;
      e : gexpr;
      field : string;
    }
      -> gexpr  (** [desugared] has ambiguous struct fields *)
  | EStructAccess : { name : string; e : gexpr; field : string } -> gexpr
      (** Resolved struct/enums, after [desugared] *)
  (* Lambda-like *)
  | EAssert : gexpr -> gexpr
  (* Default terms *)
  | EDefault : { excepts : gexpr list; just : gexpr; cons : gexpr } -> gexpr
  | EErrorOnEmpty : gexpr -> gexpr
  (* Lambda calculus with exceptions *)
  | ETuple : gexpr list -> gexpr
  | ETupleAccess : { e : gexpr; index : int; size : int } -> gexpr
  | ERaise : except -> gexpr
  | ECatch : { body : gexpr; exn : except; handler : gexpr } -> gexpr
[@@deriving yojson]

type scope_let_kind =
  | DestructuringInputStruct  (** [let x = input.field]*)
  | ScopeVarDefinition  (** [let x = error_on_empty e]*)
  | SubScopeVarDefinition
      (** [let s.x = fun _ -> e] or [let s.x = error_on_empty e] for input-only
          subscope variables. *)
  | CallingSubScope  (** [let result = s ({ x = s.x; y = s.x; ...}) ]*)
  | DestructuringSubScopeResults  (** [let s.x = result.x ]**)
  | Assertion  (** [let _ = assert e]*)
[@@deriving yojson]

type scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ;
  scope_let_expr : gexpr;
  scope_let_next : string * scope_body_expr;
}

and scope_body_expr = Result of gexpr | ScopeLet of scope_let
[@@deriving yojson]

type scope_body = {
  scope_body_input_struct : string;
  scope_body_output_struct : string;
  scope_body_expr : string * scope_body_expr;
}
[@@deriving yojson]

type scope_def = {
  scope_name : string;
  scope_body : scope_body;
  scope_next : string * scopes;
}

and scopes = Nil | ScopeDef of scope_def [@@deriving yojson]

type struct_ctx = (string * (string * typ) list) list [@@deriving yojson]
type enum_ctx = (string * (string * typ) list) list [@@deriving yojson]

type scope_out_struct = {
  out_struct_name : string;
  out_struct_fields : (string * string) list;
}
[@@deriving yojson]

type decl_ctx = {
  ctx_enums : enum_ctx;
  ctx_structs : struct_ctx;
  ctx_struct_fields : (string * (string * string) list) list;
      (** needed for disambiguation (desugared -> scope) *)
  ctx_scopes : (string * scope_out_struct) list;
}
[@@deriving yojson]

type program = { decl_ctx : decl_ctx; scopes : scopes } [@@deriving yojson]
