(* Generic hole *)
val hole : ('a, < holes : Shared_ast.yes; .. >, 'b) Shared_ast.base_gexpr

val mark_hole :
  'a ->
  (('b, < holes : Shared_ast.yes; .. >, 'c) Shared_ast.base_gexpr, 'a)
  Catala_utils.Mark.ed

(* Typing shenanigan to handle hole terms in the AST type. *)
val addholes :
  (('a, 'b, 'c) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr ->
  (('a, 'b, Shared_ast.yes) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast__Definitions.gexpr
  (** Modify the typing of the expression to enable holes *)

val delholes :
  (('a, 'b, 'c) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr ->
  (('a, 'b, Shared_ast.no) Shared_ast.slicing_interpr_kind, 't)
  Shared_ast__Definitions.gexpr
  (** Modify the typing of the expression to disable holes *)

(* Trace AST constructor *)
val trexpr : ('a, 'b) Shared_ast.gexpr -> ('a, 'b) Trace_ast.t

val trlit : Shared_ast.lit -> ('a, 'b) Trace_ast.t

val trapp :
  trf:('a, 'b) Trace_ast.t ->
  trargs:('a, 'b) Trace_ast.t list ->
  tys:Shared_ast.typ list ->
  vars:('a, 'b) Shared_ast.gexpr Shared_ast.Var.t array ->
  trv:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trappcustom :
  trcustom:('a, 'b) Trace_ast.t ->
  custom:('a, 'b) Shared_ast.gexpr ->
  trargs:('a, 'b) Trace_ast.t list ->
  vargs:('a, 'b) Shared_ast.gexpr list ->
  tys:Shared_ast.typ list ->
  v:('a, 'b) Shared_ast.gexpr -> ('a, 'b) Trace_ast.t

val trappop :
  op:'a Shared_ast.operator Catala_utils.Mark.pos ->
  trargs:('a, 'b) Trace_ast.t list ->
  tys:Shared_ast.typ list ->
  vargs:('a, 'b) Shared_ast.gexpr list ->
  traux:('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trarray : ('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trvar :
  var:('a, 'b) Shared_ast.naked_gexpr Bindlib.var ->
  value:('a, 'b) Shared_ast.gexpr -> ('a, 'b) Trace_ast.t

val trabs :
  binder:(('a, 'a, 'b) Shared_ast.base_gexpr, ('a, 'b) Shared_ast.gexpr)
         Bindlib.mbinder ->
  pos:Catala_utils.Pos.t list ->
  tys:Shared_ast.typ list -> ('a, 'b) Trace_ast.t

val trcontextclosure :
  context:(('a, 'b) Shared_ast.gexpr, ('a, 'b) Shared_ast.gexpr)
          Shared_ast.Var.Map.t ->
  tr:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trifthenelse :
  trcond:('a, 'b) Trace_ast.t ->
  trtrue:('a, 'b) Trace_ast.t ->
  trfalse:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trstruct :
  name:Shared_ast.StructName.t ->
  fields:('a, 'b) Trace_ast.t Shared_ast.StructField.Map.t ->
  ('a, 'b) Trace_ast.t

val trinj :
  name:Shared_ast.EnumName.t ->
  tr:('a, 'b) Trace_ast.t ->
  cons:Shared_ast.EnumConstructor.t -> ('a, 'b) Trace_ast.t

val trmatch :
  name:Shared_ast.EnumName.t ->
  tr:('a, 'b) Trace_ast.t ->
  cases:('a, 'b) Trace_ast.t Shared_ast.EnumConstructor.Map.t ->
  ('a, 'b) Trace_ast.t

val trtuple : ('a, 'b) Trace_ast.t list -> ('a, 'b) Trace_ast.t

val trtupleaccess :
  tr:('a, 'b) Trace_ast.t -> index:int -> size:int -> ('a, 'b) Trace_ast.t

val trstructaccess :
  name:Shared_ast.StructName.t ->
  tr:('a, 'b) Trace_ast.t ->
  field:Shared_ast.StructField.t -> ('a, 'b) Trace_ast.t

val trexternal :
  name:Shared_ast.external_ref Catala_utils.Mark.pos -> ('a, 'b) Trace_ast.t

val trassert : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trfatalerror :
  err:Runtime_ocaml.Runtime.error ->
  tr:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trdefault :
  trexcepts:('a, 'b) Trace_ast.t list ->
  vexcepts:('a, 'b) Shared_ast.gexpr list ->
  trjust:('a, 'b) Trace_ast.t ->
  trcons:('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trpuredefault : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trempty : ('a, 'b) Trace_ast.t

val trerroronempty : ('a, 'b) Trace_ast.t -> ('a, 'b) Trace_ast.t

val trcustom :
  obj:Obj.t ->
  targs:Shared_ast.typ list -> tret:Shared_ast.typ -> ('a, 'b) Trace_ast.t

val trhole : Shared_ast.typ -> ('a, 'b) Trace_ast.t

val tranyhole : ('a, 'b) Trace_ast.t

val substitute_bounded_vars :
  ((('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr,
   (('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr)
  Shared_ast.Var.Map.t ->
  (('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr ->
  ((('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr,
   (('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr)
  Shared_ast.Var.Map.t *
  (('d, 'c, 'h) Shared_ast.slicing_interpr_kind, 't) Shared_ast.gexpr
  (** Substitutes all variables in the expression by their value in the 
      context and return the submap containing all the substituted variables
      with the substituted expression *)

val is_sub_expr :
  ('d, 't) Shared_ast.gexpr -> ('d, 't) Shared_ast.gexpr -> bool
  (** Test whether the first argument is a sub expression of the second one*)
