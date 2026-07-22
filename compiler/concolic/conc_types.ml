open Shared_ast
open Symb_expr
open Path_constraint

type s_expr = SymbExpr.z3_expr

type _conc_info = {
  symb_expr : SymbExpr.t;
  constraints : PathConstraint.naked_path;
  ty : typ option;
}

type conc_info = _conc_info custom

(* This is DCalc with possibly genericErrors and customs *)
type ('c, 'e) conc_interpr_kind =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : yes
  ; genericErrors : 'e
  ; custom : 'c >

type conc_src_kind = (yes, no) conc_interpr_kind
type conc_dest_kind = (yes, yes) conc_interpr_kind

type conc_expr = (conc_src_kind, conc_info) gexpr
(** A concolic expression is a concrete DCalc expression that carries its
    symbolic representation and the constraints necessary to compute it. Upon
    initialization, [symb_expr] is [None], except for inputs whose [symb_expr]
    is a symbol. Then [symb_expr] is set by evaluation, except for inputs which
    stay the same. The expression can have [EGenericError] and [ECustom]. *)

type conc_result = (conc_dest_kind, conc_info) gexpr
(** A concolic result expression is the same as a concolic expression but it can
    be an error *)

type conc_naked_expr = (conc_src_kind, conc_info) naked_gexpr
type conc_naked_result = (conc_dest_kind, conc_info) naked_gexpr
type conc_boxed_expr = (conc_src_kind, conc_info) boxed_gexpr
