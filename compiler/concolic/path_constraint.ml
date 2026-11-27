open Catala_utils
open Shared_ast
open Symb_expr

type s_expr = SymbExpr.z3_expr
type soft_id = string
type soft = { symb : s_expr; weight : int; id : soft_id }
type reentrant = { symb : SymbExpr.reentrant; is_empty : bool }

type pc_expr =
  | Pc_z3 of s_expr
  | Pc_soft of soft
  | Pc_reentrant of reentrant
  | Pc_incomplete

module Set = Set.Make (struct
  type t = pc_expr

  let compare l r =
    match l, r with
    | Pc_z3 s, Pc_z3 s' -> Z3.Expr.compare s s'
    | Pc_soft { symb; _ }, Pc_soft { symb = symb'; _ } ->
      Z3.Expr.compare symb symb'
    | ( Pc_reentrant { symb; is_empty },
        Pc_reentrant { symb = symb'; is_empty = is_empty' } ) ->
      if is_empty = is_empty' then SymbExpr.compare_reentrant symb symb'
      else Stdlib.compare is_empty is_empty'
    | Pc_incomplete, Pc_incomplete -> 0
    | _ -> Stdlib.compare l r
end)

type naked_pc = { expr : pc_expr; pos : Pos.t; branch : bool }
type 'a path = { path : ('a * naked_pc) list; pc_set : Set.t }
type naked_path = unit path

type annotation =
  | Negated
      (** the path constraint that has been negated to generate a new input *)
  | Done
      (** a path node that has been explored should, and whose constraint should
          not be negated *)
  | Normal

type annotated_pc = annotation * naked_pc
type annotated_path = annotation path
type 'a incremental_action = IncrPush of 'a | IncrPop of 'a
type incremental_annotated_pc = (annotation * naked_pc) incremental_action
type incremental_pc_expr = pc_expr incremental_action

let format_pc_expr (fmt : Format.formatter) (e : pc_expr) : unit =
  let open Format in
  match e with
  | Pc_z3 e -> SymbExpr.format_sexpr fmt e
  | Pc_soft { symb; weight; id } ->
    fprintf fmt "Soft(%a, %d, %s)" SymbExpr.format_sexpr symb weight id
  | Pc_reentrant { symb = { name; _ }; is_empty } ->
    fprintf fmt "%s(%s)"
      (if is_empty then "Empty" else "NotEmpty")
      (Mark.remove (StructField.get_info name))
  | Pc_incomplete -> pp_print_string fmt "Incomplete"

let format_pc_debug_info (fmt : Format.formatter) (pc : naked_pc) : unit =
  if Global.options.debug then
    (Pos.format_loc_text ()) fmt (Pos.overwrite_law_info pc.pos [])

let format_pc (fmt : Format.formatter) pc : unit =
  Format.fprintf fmt "@[<v 2>%a@ %a@]" format_pc_expr pc.expr
    format_pc_debug_info pc

let format_naked_path (fmt : Format.formatter) (pcs : naked_path) : unit =
  Format.pp_print_list format_pc fmt (List.map snd pcs.path)

let format_annotated_pc (fmt : Format.formatter) (annot, pc) : unit =
  let annot_s =
    match annot with Normal -> "" | Done -> "DONE" | Negated -> "NEGATED"
  in
  Format.fprintf fmt "%7s %a" annot_s format_pc pc

let format_annotated_path (fmt : Format.formatter) (pcs : annotated_path) : unit
    =
  if pcs.path = [] then Format.pp_print_string fmt "[No constraints]"
  else Format.pp_print_list format_annotated_pc fmt pcs.path

let empty : unit path = { path = []; pc_set = Set.empty }
let empty_annotated : annotated_path = { path = []; pc_set = Set.empty }

let mk_z3 (expr : SymbExpr.t) (pos : Pos.t) (branch : bool) : naked_pc =
  let expr =
    match expr with
    | Symb_z3 e -> Pc_z3 e
    | Symb_incomplete -> Pc_incomplete
    | _ ->
      invalid_arg
        "[PathConstraint.mk_z3] expects a z3 symbolic expression (or \
         incomplete)"
  in
  { expr; pos; branch }

let default_id = ref 0

let fresh_id () =
  incr default_id;
  !default_id

let id_of_string (id : string option) =
  match id with Some s -> s | None -> "id!" ^ string_of_int (fresh_id ())

let mk_soft
    (expr : SymbExpr.t)
    (weight : int)
    (id : soft_id option)
    (pos : Pos.t)
    (branch : bool) : naked_pc =
  let expr =
    match expr with
    | Symb_z3 e -> Pc_soft { symb = e; weight; id = id_of_string id }
    | _ ->
      invalid_arg "[PathConstraint.mk_soft] expects a z3 symbolic expression"
  in
  { expr; pos; branch }

let mk_reentrant
    (expr : SymbExpr.t)
    (reentrant_const : s_expr)
    (pos : Pos.t)
    (branch : bool) : naked_pc option =
  let expr : pc_expr option =
    match expr with
    | Symb_reentrant r -> Some (Pc_reentrant { symb = r; is_empty = branch })
    | Symb_z3 s when Z3.Expr.equal s reentrant_const ->
      (* If the symbolic expression is the dummy, it means that the default
           being evaluated is in a scope called by the scope under analysis.
           Thus the context variable is not an input variable of the concolic
           engine, and its evaluation should not generate a path constraint. *)
      None
    | _ ->
      Message.error ~pos
        "[PathConstraint.mk_reentrant] expects reentrant symbolic expression \
         or dummy const but got %a"
        SymbExpr.format expr
  in
  Option.bind expr (fun expr -> Some { expr; pos; branch })

let is_incomplete (pc : naked_pc) : bool =
  match pc.expr with Pc_incomplete -> true | _ -> false

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

(** Two path constraints are equal if their expressions are equal, and they are
    marked with the same branch information. Position is not taken into account
    as it is used only in printing and not in computations *)
let path_constraint_equal c c' : bool =
  pc_expr_equal c.expr c'.expr && c.branch = c'.branch

let add : type a. a path -> a * naked_pc -> a path =
 fun pc (m, c) ->
  if Set.mem c.expr pc.pc_set then pc
  else (
    Message.debug "adding %a" format_pc c;
    Message.debug "%s" Printexc.(get_callstack 10 |> raw_backtrace_to_string);
    { path = (m, c) :: pc.path; pc_set = Set.add c.expr pc.pc_set })

let singleton c = add empty ((), c)

let add_all (pc : naked_path) (cl : naked_pc list) =
  (* preserve the order => rev *)
  List.fold_left add pc (List.rev_map (fun c -> (), c) cl)

let filter p (pc : 'a path) =
  let l, r = List.partition (fun (_, c) -> p c) pc.path in
  {
    path = l;
    pc_set = List.fold_left (fun s (_, r) -> Set.remove r.expr s) pc.pc_set r;
  }

let exists p pc = List.exists (fun (_, c) -> p c) pc.path
let length pc = List.length pc.path
let is_empty p = p.path = []

let append (new_pc : 'a path) (pc : 'a path) =
  let new_constraints =
    List.filter_map
      (fun (_, c) -> if Set.mem c.expr pc.pc_set then None else Some c)
      new_pc.path
  in
  add_all pc new_constraints

let rev_append (new_pc : 'a path) (pc : 'a path) =
  let new_constraints =
    List.filter (fun (_, c) -> not (Set.mem c.expr pc.pc_set)) new_pc.path
  in
  List.fold_left add pc new_constraints

let constraints (p : 'a path) = p.path

(** Compare the path of the previous evaluation and the path of the current
    evaluation. If a constraint was previously marked as Done or Normal, then
    check that it stayed the same. If it was previously marked as Negated, thus
    if it was negated before the two evaluations, then check that the concrete
    value was indeed negated and mark it Done. If there are new constraints
    after the last one, add them as Normal. Crash in other cases. *)
let compare_paths (expected_path : annotated_path) (evaluated_path : naked_path)
    : annotated_path * incremental_annotated_pc list =
  (* ordered by recency: first path constraint is the most recent =>
     reverse for pair-wise common prefix check *)
  let expected_path_l = List.rev expected_path.path in
  let evaluated_path_l = List.rev_map snd evaluated_path.path in
  let rec loop acc = function
    | [], [] -> acc, [] (* to be rev'ed later *)
    | [], p ->
      (* new path can be longer *)
      let acc = List.rev_append (List.map (fun c -> Normal, c) p) acc in
      let diff = List.map (fun c -> IncrPush (Normal, c)) p in
      acc, diff
    | _ :: _, [] ->
      Message.error "[compare_paths] old path is longer than new path"
    | (Normal, c) :: p, c' :: p' ->
      if not (path_constraint_equal c c') then
        Message.error
          "[compare_paths] a constraint that should not change has changed";
      loop ((Normal, c) :: acc) (p, p')
    | (Negated, c) :: p, c' :: p' ->
      if c.branch = c'.branch then
        Message.error
          "@[<v 2>[compare_paths] the negated condition lead to the same \
           path:@ %a @ @ vs.@ @ %a@]"
          format_pc c format_pc c';
      (* the branch has been successfully negated and is now done *)
      (* TODO we should have a way to know if c and c' are the same except for
           their [branch] *)
      loop ((Done, c') :: acc) (p, p')
    | (Done, c) :: p, c' :: p' ->
      (* To check: physical equality test should be okay *)
      if c <> c' then begin
        Message.error
          "[compare_paths] a done constraint that should not change has changed"
      end;
      loop ((Done, c) :: acc) (p, p')
  in
  let new_annotated_path, diff = loop [] (expected_path_l, evaluated_path_l) in
  ( (* implicit reverse => not needed?! *)
    List.fold_left add
      { path = []; pc_set = Set.empty }
      (List.rev new_annotated_path),
    diff )

(** Remove Done paths until a Normal (not yet negated) constraint is found, then
    mark this branch as Negated. This function shall be called on an output of
    [compare_paths], and thus no Negated constraint should appear in its input.
*)
let make_expected_path (p : annotated_path) :
    annotated_path * incremental_annotated_pc list =
  let rec loop (acc, diff) = function
    | [] -> [], []
    | (Normal, c) :: p ->
      ( List.rev_append ((Negated, c) :: acc) p,
        List.rev
          (List.rev [IncrPop (Normal, c); IncrPush (Negated, c)] @ List.rev diff)
      )
    | (Done, c) :: p -> loop (acc, IncrPop (Done, c) :: diff) p
    | (Negated, _) :: _ ->
      Message.error
        "[make_expected_path] found a negated constraint, which should not \
         happen"
  in
  let new_path, diff = loop ([], []) p.path in
  { p with path = new_path }, diff

let add pc c = add pc ((), c)

let pop p =
  match p.path with
  | [] -> None
  | ((_, c) as h) :: t ->
    Some (h, { path = t; pc_set = Set.remove c.expr p.pc_set })
