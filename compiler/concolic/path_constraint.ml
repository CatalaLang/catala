open Catala_utils
open Shared_ast
open Symb_expr

module PathConstraint = struct
  type s_expr = SymbExpr.z3_expr
  type reentrant = { symb : SymbExpr.reentrant; is_empty : bool }
  type pc_expr = Pc_z3 of s_expr | Pc_reentrant of reentrant

  (* path constraint cannot be empty (this looks like a GADT but it would be
     overkill I think) *)
  type naked_pc = { expr : pc_expr; pos : Pos.t; branch : bool }
  type naked_path = naked_pc list

  let mk_z3 (expr : SymbExpr.t) (pos : Pos.t) (branch : bool) : naked_pc =
    let expr =
      match expr with
      | Symb_z3 e -> Pc_z3 e
      | _ ->
        invalid_arg "[PathConstraint.mk_z3] expects a z3 symbolic expression"
    in
    { expr; pos; branch }

  let mk_reentrant
      (expr : SymbExpr.t)
      (dummy_const : s_expr)
      (pos : Pos.t)
      (branch : bool) : naked_pc option =
    let expr : pc_expr option =
      match expr with
      | Symb_reentrant r -> Some (Pc_reentrant { symb = r; is_empty = branch })
      | Symb_z3 s when Z3.Expr.equal s dummy_const ->
        (* If the symbolic expression is the dummy, it means that the default
           being evaluated is in a scope called by the scope under analysis.
           Thus the context variable is not an input variable of the concolic
           engine, and its evaluation should not generate a path constraint. *)
        None
      | _ ->
        Message.raise_spanned_error pos
          "[PathConstraint.mk_reentrant] expects reentrant symbolic expression \
           or dummy const but got %a"
          SymbExpr.formatter expr
    in
    Option.bind expr (fun expr -> Some { expr; pos; branch })

  type annotated_pc =
    | Negated of naked_pc
        (** the path constraint that has been negated to generate a new input *)
    | Done of naked_pc
        (** a path node that has been explored should, and whose constraint
            should not be negated *)
    | Normal of naked_pc  (** all other constraints *)

  type annotated_path = annotated_pc list

  (** Computation path logic *)

  (* Two path constraint expressions are equal if they are of the same kind, and
     if either their Z3 expressions are equal or their variable name and
     "emptyness" are equal depending on that kind. *)
  let pc_expr_equal e e' : bool =
    match e, e' with
    | Pc_z3 e1, Pc_z3 e2 -> Z3.Expr.equal e1 e2
    | Pc_reentrant e1, Pc_reentrant e2 ->
      StructField.equal e1.symb.name e2.symb.name && e1.is_empty = e2.is_empty
    | _, _ -> false

  (** Two path constraints are equal if their expressions are equal, and they
      are marked with the same branch information. Position is not taken into
      account as it is used only in printing and not in computations *)
  let path_constraint_equal c c' : bool =
    pc_expr_equal c.expr c'.expr && c.branch = c'.branch

  (** Compare the path of the previous evaluation and the path of the current
      evaluation. If a constraint was previously marked as Done or Normal, then
      check that it stayed the same. If it was previously marked as Negated,
      thus if it was negated before the two evaluations, then check that the
      concrete value was indeed negated and mark it Done. If there are new
      constraints after the last one, add them as Normal. Crash in other cases. *)
  let rec compare_paths (path_prev : annotated_path) (path_new : naked_path) :
      annotated_path =
    match path_prev, path_new with
    | [], [] -> []
    | [], c' :: p' ->
      Normal c' :: compare_paths [] p' (* the new path can be longer *)
    | _ :: _, [] -> failwith "[compare_paths] old path is longer than new path"
    | Normal c :: p, c' :: p' ->
      if path_constraint_equal c c' then Normal c :: compare_paths p p'
      else
        failwith
          "[compare_paths] a constraint that should not change has changed"
    | Negated c :: p, c' :: p' ->
      if c.branch <> c'.branch then
        (* the branch has been successfully negated and is now done *)
        (* TODO we should have a way to know if c and c' are the same except for
           their [branch] *)
        Done c' :: compare_paths p p'
      else
        failwith "[compare_paths] the negated condition lead to the same path"
    | Done c :: p, c' :: p' ->
      if c = c' then Done c :: compare_paths p p'
      else
        failwith
          "[compare_paths] a done constraint that should not change has changed"

  (** Remove Done paths until a Normal (not yet negated) constraint is found,
      then mark this branch as Negated. This function shall be called on an
      output of [compare_paths], and thus no Negated constraint should appear in
      its input. *)
  let rec make_expected_path (path : annotated_path) : annotated_path =
    match path with
    | [] -> []
    | Normal c :: p -> Negated c :: p
    | Done _ :: p -> make_expected_path p
    | Negated _ :: _ ->
      failwith
        "[make_expected_path] found a negated constraint, which should not \
         happen"

  (* TODO use formatter for those *)
  let string_of_pc_expr (e : pc_expr) : string =
    match e with
    | Pc_z3 e -> Z3.Expr.to_string e
    | Pc_reentrant { symb = { name; _ }; is_empty } ->
      (if is_empty then "Empty(" else "NotEmpty(")
      ^ Mark.remove (StructField.get_info name)
      ^ ")"

  let string_of_path_constraint (pc : naked_pc) : string =
    string_of_pc_expr pc.expr
    ^
    if Cli.globals.debug then
      "@" ^ Pos.to_string_short pc.pos ^ " {" ^ string_of_bool pc.branch ^ "}"
    else ""

  let print_path_constraints (pcs : naked_path) : unit =
    let pp_sep fmt () = Format.fprintf fmt "\n" in
    Message.emit_debug "Path constraints after evaluation:\n%a"
      (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (List.map string_of_path_constraint pcs)

  let print_annotated_path_constraints constraints : unit =
    if constraints = [] then Message.emit_debug "No constraints"
    else begin
      let pp_sep fmt () = Format.fprintf fmt "\n" in
      let aux apc =
        match apc with
        | Normal pc -> "       " ^ string_of_path_constraint pc
        | Done pc -> "DONE   " ^ string_of_path_constraint pc
        | Negated pc -> "NEGATE " ^ string_of_path_constraint pc
      in
      Message.emit_debug "Trying new path constraints:\n%a"
        (Format.pp_print_list ~pp_sep Format.pp_print_string)
        (List.map aux constraints)
    end
end
