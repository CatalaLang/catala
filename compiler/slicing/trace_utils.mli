open Shared_ast

(* Generic hole *)
val hole : ('a, < holes : yes; .. >, 'b) base_gexpr

val mark_hole :
  'a ->
  (('b, < holes : yes; .. >, 'c) base_gexpr, 'a)
  Catala_utils.Mark.ed

(* Typing shenanigan to handle hole terms in the AST type. *)
val addholes :
  (('a, 'b, 'c) slicing_interpr_kind, 't) gexpr ->
  (('a, 'b, yes) slicing_interpr_kind, 't)
  Shared_ast__Definitions.gexpr
  (** Modify the typing of the expression to enable holes *)

val delholes :
  (('a, 'b, 'c) slicing_interpr_kind, 't) gexpr ->
  (('a, 'b, no) slicing_interpr_kind, 't)
  Shared_ast__Definitions.gexpr
  (** Modify the typing of the expression to disable holes *)

(* Trace AST constructor *)
val trexpr : ('a, 'b) gexpr -> ('a, 'b) Trace_ast.t

val trlit : lit -> ('a, 'b) Trace_ast.t

val trapp :
  trf:('a, 'b) Trace_ast.t ->
  trargs:('a, 'b) Trace_ast.t list ->
  tys:typ list ->
  vars:('a, 'b) gexpr Var.t array ->
  trv:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trappcustom :
  trcustom:('a, 'b) Trace_ast.t ->
  custom:('a, 'b) gexpr ->
  trargs:('a, 'b) Trace_ast.t list ->
  vargs:('a, 'b) gexpr list ->
  tys:typ list ->
  v:('a, 'b) gexpr -> ('a, 'b) Trace_ast.t

val trappop :
  op:'a operator Catala_utils.Mark.pos ->
  trargs:('a, 'b) Trace_ast.t list ->
  tys:typ list ->
  vargs:('a, 'b) gexpr list ->
  traux:('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trarray : ('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trvar :
  var:('a, 'b) naked_gexpr Bindlib.var ->
  value:('a, 'b) gexpr -> ('a, 'b) Trace_ast.t

val trabs :
  binder:(('a, 'a, 'b) base_gexpr, ('a, 'b) gexpr)
         Bindlib.mbinder ->
  pos:Catala_utils.Pos.t list ->
  tys:typ list -> ('a, 'b) Trace_ast.t

val trcontextclosure :
  context:(('a, 'b) gexpr, ('a, 'b) gexpr)
          Var.Map.t ->
  tr:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trifthenelse :
  trcond:('a, 'b) Trace_ast.t ->
  trtrue:('a, 'b) Trace_ast.t ->
  trfalse:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trstruct :
  name:StructName.t ->
  fields:('a, 'b) Trace_ast.t StructField.Map.t ->
  ('a, 'b) Trace_ast.t

val trinj :
  name:EnumName.t ->
  tr:('a, 'b) Trace_ast.t ->
  cons:EnumConstructor.t -> ('a, 'b) Trace_ast.t

val trmatch :
  name:EnumName.t ->
  tr:('a, 'b) Trace_ast.t ->
  cases:('a, 'b) Trace_ast.t EnumConstructor.Map.t ->
  ('a, 'b) Trace_ast.t

val trtuple : ('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trtupleaccess :
  tr:('a, 'b) Trace_ast.t -> index:int -> size:int -> ('a, 'b) Trace_ast.t

val trstructaccess :
  name:StructName.t ->
  tr:('a, 'b) Trace_ast.t ->
  field:StructField.t -> ('a, 'b) Trace_ast.t

val trexternal :
  name:external_ref Catala_utils.Mark.pos -> ('a, 'b) Trace_ast.t

val trassert : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trfatalerror :
  err:Runtime_ocaml.Runtime.error ->
  tr:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trdefault :
  trexcepts:('a, 'b) Trace_ast.t list ->
  vexcepts:('a, 'b) gexpr list ->
  trjust:('a, 'b) Trace_ast.t ->
  trcons:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trpuredefault : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trempty : ('a, 'b) Trace_ast.t

val trerroronempty : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trcustom :
  obj:Obj.t ->
  targs:typ list -> tret:typ -> ('a, 'b) Trace_ast.t

val trhole : typ -> ('a, 'b) Trace_ast.t

val tranyhole : ('a, 'b) Trace_ast.t

val substitute_bound_vars_with_ctx:
  ((('d, 'c, 'h) slicing_interpr_kind, 't) gexpr,
   (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr)
  Var.Map.t ->
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr ->
  ((('d, 'c, 'h) slicing_interpr_kind, 't) gexpr,
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr)
  Var.Map.t *
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr 

val substitute_bound_vars :
  ((('d, 'c, 'h) slicing_interpr_kind, 't) gexpr,
   (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr)
  Var.Map.t ->
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr ->
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr
  (** Substitutes all variables in the expression by their value in the context *)

val min_context :
  ((('d, 'c, 'h) slicing_interpr_kind, 't) gexpr,
   (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr)
  Var.Map.t ->
  (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr ->
  ((('d, 'c, 'h) slicing_interpr_kind, 't) gexpr,
   (('d, 'c, 'h) slicing_interpr_kind, 't) gexpr)
  Var.Map.t
  (** Return the submap of the context containing only the free variables of the expression *)

val is_sub_expr :
  ('d, 't) gexpr -> ('d, 't) gexpr -> bool
  (** Test whether the first argument is a sub expression of the second one*)
