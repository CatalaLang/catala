(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Functions handling the expressions of [shared_ast] *)

open Catala_utils
open Definitions

(** {2 Boxed constructors} *)

val box : ('a, 'm) gexpr -> ('a, 'm) boxed_gexpr
(** Box the expression from the outside *)

val unbox : ('a, 'm) boxed_gexpr -> ('a, 'm) gexpr
(** For closed expressions, similar to [Bindlib.unbox] *)

val unbox_closed : ('a, 'm) boxed_gexpr -> ('a, 'm) gexpr
(** Similar to [unbox], but with an added assertion check on the expression
    being closed *)

val rebox : ('a any, 'm) gexpr -> ('a, 'm) boxed_gexpr
(** Rebuild the whole term, re-binding all variables and exposing free variables *)

val evar : ('a, 'm) gexpr Var.t -> 'm mark -> ('a, 'm) boxed_gexpr

val eexternal :
  path:path ->
  name:external_ref Mark.pos ->
  'm mark ->
  (< explicitScopes : no ; .. >, 'm) boxed_gexpr

val bind :
  ('a, 'm) gexpr Var.t array ->
  ('a, 'm) boxed_gexpr ->
  (('a, 'm) naked_gexpr, ('a, 'm) gexpr) Bindlib.mbinder Bindlib.box

val subst :
  (('a, 'm) naked_gexpr, ('a, 'm) gexpr) Bindlib.mbinder ->
  ('a, 'm) gexpr list ->
  ('a, 'm) gexpr

val etuple : ('a, 'm) boxed_gexpr list -> 'm mark -> ('a any, 'm) boxed_gexpr

val etupleaccess :
  ('a, 'm) boxed_gexpr -> int -> int -> 'm mark -> ('a any, 'm) boxed_gexpr

val earray : ('a, 'm) boxed_gexpr list -> 'm mark -> ('a any, 'm) boxed_gexpr
val elit : lit -> 'm mark -> ('a any, 'm) boxed_gexpr

val eabs :
  (('a, 'm) naked_gexpr, ('a, 'm) gexpr) Bindlib.mbinder Bindlib.box ->
  typ list ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val eapp :
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr list ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val eassert :
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< assertions : yes ; .. > as 'a), 'm) boxed_gexpr

val eop : 'a operator -> typ list -> 'm mark -> ('a any, 'm) boxed_gexpr

val edefault :
  ('a, 'm) boxed_gexpr list ->
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< defaultTerms : yes ; .. > as 'a), 'm) boxed_gexpr

val eifthenelse :
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val eemptyerror :
  'm mark -> ((< defaultTerms : yes ; .. > as 'a), 'm) boxed_gexpr

val eerroronempty :
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< defaultTerms : yes ; .. > as 'a), 'm) boxed_gexpr

val ecatch :
  ('a, 'm) boxed_gexpr ->
  except ->
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< exceptions : yes ; .. > as 'a), 'm) boxed_gexpr

val eraise : except -> 'm mark -> (< exceptions : yes ; .. >, 'm) boxed_gexpr
val elocation : 'a glocation -> 'm mark -> ((< .. > as 'a), 'm) boxed_gexpr

val estruct :
  name:StructName.t ->
  fields:('a, 'm) boxed_gexpr StructField.Map.t ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val edstructaccess :
  path:path ->
  name_opt:StructName.t option ->
  field:Ident.t ->
  e:('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< syntacticNames : yes ; .. > as 'a), 'm) boxed_gexpr

val estructaccess :
  name:StructName.t ->
  field:StructField.t ->
  e:('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< resolvedNames : yes ; .. > as 'a), 'm) boxed_gexpr

val einj :
  name:EnumName.t ->
  cons:EnumConstructor.t ->
  e:('a, 'm) boxed_gexpr ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val ematch :
  name:EnumName.t ->
  e:('a, 'm) boxed_gexpr ->
  cases:('a, 'm) boxed_gexpr EnumConstructor.Map.t ->
  'm mark ->
  ('a any, 'm) boxed_gexpr

val escopecall :
  path:path ->
  scope:ScopeName.t ->
  args:('a, 'm) boxed_gexpr ScopeVar.Map.t ->
  'm mark ->
  ((< explicitScopes : yes ; .. > as 'a), 'm) boxed_gexpr

val ecustom :
  Obj.t ->
  Type.t list ->
  Type.t ->
  'm mark ->
  (< custom : Definitions.yes ; .. >, 'm) boxed_gexpr

val fun_id : ?var_name:string -> 'm mark -> ('a any, 'm) boxed_gexpr

(** {2 Manipulation of marks} *)

val no_mark : 'm mark -> 'm mark
(** Returns an empty mark, using the argument as type witness. Note that the
    custom part is kept on [Custom] marks *)

val mark_pos : 'm mark -> Pos.t
val with_pos : Pos.t -> 'm mark -> 'm mark

val with_ty : 'm mark -> ?pos:Pos.t -> typ -> 'm mark
(** Adds the given type information only on typed marks *)

val map_ty : (typ -> typ) -> 'm mark -> 'm mark
(** Identity on untyped marks*)

val map_mark : (Pos.t -> Pos.t) -> (typ -> typ) -> 'm mark -> 'm mark

val map_mark2 :
  (Pos.t -> Pos.t -> Pos.t) ->
  (typed -> typed -> typ) ->
  'm mark ->
  'm mark ->
  'm mark
(** @raise Invalid_arg on custom marks*)

val fold_marks :
  (Pos.t list -> Pos.t) -> (typed list -> typ) -> 'm mark list -> 'm mark
(** @raise Invalid_arg on custom marks*)

val maybe_ty : ?typ:naked_typ -> 'm mark -> typ
(** Returns the corresponding type on a typed expr, or [typ] (defaulting to
    [TAny]) at the current position on an untyped one *)

(** {2 Predefined types} *)

val option_enum : EnumName.t
val none_constr : EnumConstructor.t
val some_constr : EnumConstructor.t
val option_enum_config : typ EnumConstructor.Map.t

(** Manipulation of marked expressions *)

val pos : ('a, 'm) marked -> Pos.t
val ty : ('e, typed) marked -> typ
val set_ty : typ -> ('a, 'm) marked -> ('a, typed) marked
val untype : ('a, 'm) gexpr -> ('a, untyped) boxed_gexpr

(** {2 Traversal functions} *)

val map :
  f:(('a, 'm1) gexpr -> ('b, 'm2) boxed_gexpr) ->
  (('a, 'b, 'm1) base_gexpr, 'm2) marked ->
  ('b, 'm2) boxed_gexpr
(** Shallow mapping on expressions (non recursive): applies the given function
    to all sub-terms of the given expression, and rebuilds the node.

    This function makes it very concise to transform only certain nodes of the
    AST. For instance, if you want to remove all errors on empty, you can write

    {[
      let remove_error_empty e =
        let rec f e =
          match Mark.remove e with
          | EErrorOnEmpty e1 -> Expr.map ~f e1
          | _ -> Expr.map ~f e
        in
        f e
    ]}

    This can even be used to translate between different kinds of ASTs: see
    [Lcalc.Compile_without_exceptions] for an example. The structure is like
    this:

    {[
      let rec translate = function
        | SpecificCase e -> TargetCase (translate e)
        | (All | Other | Common | Cases) as e -> Expr.map ~f:translate e
    ]}

    The [e] parameter passed to [map] here needs to have only the common cases
    in its shallow type, but can still contain any node from the starting AST
    deeper inside: this is where the second type parameter to [base_gexpr]
    becomes useful. *)

val map_top_down :
  f:(('a, 'm1) gexpr -> (('a, 'm1) naked_gexpr, 'm2) marked) ->
  ('a, 'm1) gexpr ->
  ('a, 'm2) boxed_gexpr
(** Recursively applies [f] to the nodes of the expression tree. The type
    returned by [f] is hybrid since the mark at top-level has been rewritten,
    but not yet the marks in the subtrees. *)

val map_marks :
  f:('m1 mark -> 'm2 mark) -> ('a, 'm1) gexpr -> ('a, 'm2) boxed_gexpr

val shallow_fold :
  (('a, 'm) gexpr -> 'acc -> 'acc) -> ('a, 'm) gexpr -> 'acc -> 'acc
(** Applies a function on all sub-terms of the given expression. Does not
    recurse. It opens binders unless you avoid sending binders to the function
    like the example below. Useful as helper for recursive calls within
    traversal functions. This can be used to compute free variables with e.g.:

    {[
      let rec free_vars = function
        | EVar v, _ -> Var.Set.singleton v
        | EAbs { binder; _ }, _ ->
          let vs, body = Bindlib.unmbind binder in
          Array.fold_right Var.Set.remove vs (free_vars body)
        | e ->
          shallow_fold (fun e -> Var.Set.union (free_vars e)) e Var.Set.empty
    ]} *)

val map_gather :
  acc:'acc ->
  join:('acc -> 'acc -> 'acc) ->
  f:(('a, 'm1) gexpr -> 'acc * ('a, 'm2) boxed_gexpr) ->
  (('a, 'm1) naked_gexpr, 'm2) marked ->
  'acc * ('a, 'm2) boxed_gexpr
(** Shallow mapping similar to [map], but additionally allows to gather an
    accumulator bottom-up. [acc] is the accumulator value returned on terminal
    nodes, and [join] is used to merge accumulators from the different sub-terms
    of an expression. [acc] is assumed to be a neutral element for [join].
    Typically used with a set of variables used in the rewrite:

    {[
      let rec rewrite e =
        match Mark.remove e with
        | Specific_case -> Var.Set.singleton x, some_rewrite_fun e
        | _ ->
          Expr.map_gather ~acc:Var.Set.empty ~join:Var.Set.union ~f:rewrite e
    ]}

    See [Lcalc.closure_conversion] for a real-world example. *)

(** {2 Expression building helpers} *)

val make_var : ('a, 'm) gexpr Var.t -> 'm mark -> ('a, 'm) boxed_gexpr

val make_abs :
  ('a, 'm) gexpr Var.vars ->
  ('a, 'm) boxed_gexpr ->
  typ list ->
  Pos.t ->
  ('a any, 'm) boxed_gexpr

val make_app :
  ('a any, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr list ->
  Pos.t ->
  ('a any, 'm) boxed_gexpr

val empty_thunked_term :
  'm mark -> (< defaultTerms : yes ; .. >, 'm) boxed_gexpr

val thunk_term : ('a any, 'b) boxed_gexpr -> 'b mark -> ('a, 'b) boxed_gexpr
val unthunk_term_nobox : ('a any, 'm) gexpr -> 'm mark -> ('a, 'm) gexpr

val make_let_in :
  ('a, 'm) gexpr Var.t ->
  typ ->
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr ->
  Pos.t ->
  ('a any, 'm) boxed_gexpr

val make_multiple_let_in :
  ('a, 'm) gexpr Var.vars ->
  typ list ->
  ('a, 'm) boxed_gexpr list ->
  ('a, 'm) boxed_gexpr ->
  Pos.t ->
  ('a any, 'm) boxed_gexpr

val make_default :
  ('a, 'm) boxed_gexpr list ->
  ('a, 'm) boxed_gexpr ->
  ('a, 'm) boxed_gexpr ->
  'm mark ->
  ((< polymorphic : yes ; defaultTerms : yes ; .. > as 'a), 'm) boxed_gexpr
(** [make_default ?pos exceptions just cons] builds a term semantically
    equivalent to [<exceptions | just :- cons>] (the [EDefault] constructor)
    while avoiding redundant nested constructions. The position is extracted
    from [just] by default.

    Note that some simplifications take place here, even though all of them
    return an [EDefault] term:

    - [<ex | true :- def>], when [def] is a default term [<j :- c>] without
      exceptions, is collapsed into [<ex | def>]
    - [<ex | false :- _>], when [ex] is a single exception of the form
      [EDefault], is rewritten as [ex] *)

val make_tuple :
  ('a any, 'm) boxed_gexpr list -> 'm mark -> ('a, 'm) boxed_gexpr
(** Builds a tuple; the mark argument is only used as witness and for position
    when building 0-uples *)

(** {2 Transformations} *)

val skip_wrappers : ('a, 'm) gexpr -> ('a, 'm) gexpr
(** Removes surface logging calls and [EErrorOnEmpty] nodes. Shallow function *)

val remove_logging_calls :
  ((< polymorphic : yes ; .. > as 'a), 'm) gexpr -> ('a, 'm) boxed_gexpr
(** Removes all calls to [Log] unary operators in the AST, replacing them by
    their argument. *)

val rename_vars :
  ?exclude:string list ->
  ?reset_context_for_closed_terms:bool ->
  ?skip_constant_binders:bool ->
  ?constant_binder_name:string option ->
  ('a, 'm) gexpr ->
  ('a, 'm) boxed_gexpr
(** Disambiguates all variable names in [e]. [exclude] will blacklist the given
    names (useful for keywords or built-in names) ; the other flags behave as
    defined in the bindlib documentation for module type [Rename] *)

val format : Format.formatter -> ('a, 'm) gexpr -> unit
(** Simple printing without debug, use [Print.expr ()] instead to follow the
    command-line debug setting *)

(** {2 Analysis and tests} *)

val equal_lit : lit -> lit -> bool
val compare_lit : lit -> lit -> int
val equal_path : path -> path -> bool
val compare_path : path -> path -> int
val equal_location : 'a glocation Mark.pos -> 'a glocation Mark.pos -> bool
val compare_location : 'a glocation Mark.pos -> 'a glocation Mark.pos -> int
val equal_except : except -> except -> bool
val compare_except : except -> except -> int

val equal : ('a, 'm) gexpr -> ('a, 'm) gexpr -> bool
(** Determines if two expressions are equal, omitting their position information *)

val compare : ('a, 'm) gexpr -> ('a, 'm) gexpr -> int
(** Standard comparison function, suitable for e.g. [Set.Make]. Ignores position
    information *)

val is_value : ('a any, 'm) gexpr -> bool
val free_vars : ('a any, 'm) gexpr -> ('a, 'm) gexpr Var.Set.t

val size : ('a, 'm) gexpr -> int
(** Used by the optimizer to know when to stop *)

(** {2 Low-level handling of boxed expressions} *)
module Box : sig
  (** This module contains helper functions for Bindlib, and wrappers to use
      boxed expressions.

      We use the [boxed_expr = naked_expr box marked] type throughout, rather
      than the more straightforward [expr box = naked_expr marked box], because
      the latter would force us to resolve the box every time we need to recover
      the annotation, which happens often. It's more efficient and convenient to
      add the annotation outside of the box, and delay its injection (using
      [lift]) to when the parent term gets built. *)

  val lift : ('a, 'm) boxed_gexpr -> ('a, 'm) gexpr Bindlib.box
  (** Inject the annotation within the box, to use e.g. when a [gexpr box] is
      required for building parent terms *)

  val app0 : ('a, 'm) naked_gexpr -> 'm mark -> ('a, 'm) boxed_gexpr
  (** The [app*] functions allow building boxed expressions using
      [Bindlib.apply_box] and its variants, while correctly handling the
      expression annotations. Note that the function provided as argument should
      return a [naked_gexpr] and the expression annotation (['t]) is provided as
      a separate argument. *)

  val app1 :
    ('a, 'm1) boxed_gexpr ->
    (('a, 'm1) gexpr -> ('a, 'm2) naked_gexpr) ->
    'm2 mark ->
    ('a, 'm2) boxed_gexpr

  val app2 :
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr ->
    (('a, 'm) gexpr -> ('a, 'm) gexpr -> ('a, 'm) naked_gexpr) ->
    'm mark ->
    ('a, 'm) boxed_gexpr

  val app3 :
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr ->
    (('a, 'm) gexpr -> ('a, 'm) gexpr -> ('a, 'm) gexpr -> ('a, 'm) naked_gexpr) ->
    'm mark ->
    ('a, 'm) boxed_gexpr

  val appn :
    ('a, 'm) boxed_gexpr list ->
    (('a, 'm) gexpr list -> ('a, 'm) naked_gexpr) ->
    'm mark ->
    ('a, 'm) boxed_gexpr

  val app1n :
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr list ->
    (('a, 'm) gexpr -> ('a, 'm) gexpr list -> ('a, 'm) naked_gexpr) ->
    'm mark ->
    ('a, 'm) boxed_gexpr

  val app2n :
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr ->
    ('a, 'm) boxed_gexpr list ->
    (('a, 'm) gexpr ->
    ('a, 'm) gexpr ->
    ('a, 'm) gexpr list ->
    ('a, 'm) naked_gexpr) ->
    'm mark ->
    ('a, 'm) boxed_gexpr

  val fv : 'b Bindlib.box -> string list
  (** [fv] return the list of free variables from a boxed term. *)

  val assert_closed : 'b Bindlib.box -> unit
  (** [assert_closed b] check there is no free variables in then [b] boxed term.
      It raises an internal error if it not the case, printing all free
      variables. *)
end
