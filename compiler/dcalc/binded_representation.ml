open Utils
module D = Ast

(** Alternative representation of the Dcalc Ast. It is currently used in the transformation without
    exceptions. We make heavy use of bindlib, binding each scope-let-variable and each scope
    explicitly. *)

(** In [Ast], [Ast.scope_lets] is defined as a list of kind, var, and boxed expression. This
    representation binds using bindlib the tail of the list with the variable defined in the let. *)
type scope_lets =
  | Result of D.expr Pos.marked
  | ScopeLet of {
      scope_let_kind : D.scope_let_kind;
      scope_let_typ : D.typ Pos.marked;
      scope_let_expr : D.expr Pos.marked;
      scope_let_next : (D.expr, scope_lets) Bindlib.binder;
      scope_let_pos : Pos.t;
    }

type scope_body = {
  scope_body_input_struct : D.StructName.t;
  scope_body_output_struct : D.StructName.t;
  scope_body_result : (D.expr, scope_lets) Bindlib.binder;
}
(** As a consequence, the scope_body contains only a result and input/output signature, as the other
    elements are stored inside the scope_let. The binder present is the argument of type
    [scope_body_input_struct]. *)

(* finally, we do the same transformation for the whole program for the kinded lets. This permit us to use bindlib variables for scopes names. *)
type scopes =
  | Nil
  | ScopeDef of {
      scope_name : D.ScopeName.t;
      scope_body : scope_body;
      scope_next : (D.expr, scopes) Bindlib.binder;
    }

let union = D.VarMap.union (fun _ _ _ -> Some ())

(** free variables. For each construction, we define two free variables functions. The first one
    generates [unit D.VarMap.t], since there is no [D.VarSet.t]. And the second returns a list. The
    second one is better from pretty printing in debug. *)

let rec fv_scope_lets scope_lets =
  match scope_lets with
  | Result e -> D.fv e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
      let v, body = Bindlib.unbind next in
      union (D.fv e) (D.VarMap.remove v (fv_scope_lets body))

let fv_scope_body { scope_body_result = binder; _ } =
  let v, body = Bindlib.unbind binder in
  D.VarMap.remove v (fv_scope_lets body)

let rec fv_scopes scopes =
  match scopes with
  | Nil -> D.VarMap.empty
  | ScopeDef { scope_body = body; scope_next = next; _ } ->
      let v, next = Bindlib.unbind next in

      union (D.VarMap.remove v (fv_scopes next)) (fv_scope_body body)

let _free_vars_scope_lets scope_lets = fv_scope_lets scope_lets |> D.VarMap.bindings |> List.map fst

let _free_vars_scope_body scope_body = fv_scope_body scope_body |> D.VarMap.bindings |> List.map fst

let free_vars_scopes scopes = fv_scopes scopes |> D.VarMap.bindings |> List.map fst

(** Actual transformation for scopes. It simply *)
let bind_scope_lets (acc : scope_lets Bindlib.box) (scope_let : D.scope_let) :
    scope_lets Bindlib.box =
  let pos = snd scope_let.D.scope_let_var in

  Cli.debug_print
  @@ Format.asprintf "binding let %a. Variable occurs = %b" Print.format_var
       (fst scope_let.D.scope_let_var)
       (Bindlib.occur (fst scope_let.D.scope_let_var) acc);

  let binder = Bindlib.bind_var (fst scope_let.D.scope_let_var) acc in
  Bindlib.box_apply2
    (fun expr binder ->
      Cli.debug_print
      @@ Format.asprintf "free variables in expression: %a"
           (Format.pp_print_list Print.format_var)
           (D.free_vars expr);
      ScopeLet
        {
          scope_let_kind = scope_let.D.scope_let_kind;
          scope_let_typ = scope_let.D.scope_let_typ;
          scope_let_expr = expr;
          scope_let_next = binder;
          scope_let_pos = pos;
        })
    scope_let.D.scope_let_expr binder

let bind_scope_body (body : D.scope_body) : scope_body Bindlib.box =
  (* it is a fold_right and not a fold_left. *)
  let body_result =
    ListLabels.fold_right body.D.scope_body_lets
      ~init:(Bindlib.box_apply (fun e -> Result e) body.D.scope_body_result)
      ~f:(Fun.flip bind_scope_lets)
  in

  Cli.debug_print @@ Format.asprintf "binding arg %a" Print.format_var body.D.scope_body_arg;
  let scope_body_result = Bindlib.bind_var body.D.scope_body_arg body_result in

  Cli.debug_print
  @@ Format.asprintf "isfinal term is closed: %b" (Bindlib.is_closed scope_body_result);
  Bindlib.box_apply
    (fun scope_body_result ->
      Cli.debug_print
      @@ Format.asprintf "rank of the final term: %i" (Bindlib.binder_rank scope_body_result);
      {
        scope_body_output_struct = body.D.scope_body_output_struct;
        scope_body_input_struct = body.D.scope_body_input_struct;
        scope_body_result;
      })
    scope_body_result

let bind_scope
    ((scope_name, scope_var, scope_body) : D.ScopeName.t * D.expr Bindlib.var * D.scope_body)
    (acc : scopes Bindlib.box) : scopes Bindlib.box =
  Bindlib.box_apply2
    (fun scope_body scope_next -> ScopeDef { scope_name; scope_body; scope_next })
    (bind_scope_body scope_body) (Bindlib.bind_var scope_var acc)

let bind_scopes (scopes : (D.ScopeName.t * D.expr Bindlib.var * D.scope_body) list) :
    scopes Bindlib.box =
  let result = ListLabels.fold_right scopes ~init:(Bindlib.box Nil) ~f:bind_scope in

  Cli.debug_print
  @@ Format.asprintf "free variable in the program : [%a]"
       (Format.pp_print_list Print.format_var)
       (free_vars_scopes (Bindlib.unbox result));

  result
