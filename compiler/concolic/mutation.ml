open Shared_ast
open Catala_utils

let init = function
  | None ->
    Message.result "[mutation] random seed";
    Random.self_init ()
  | Some n ->
    Message.result "[mutation] seed %n" n;
    Random.init n

let choose_in_list l n =
  if l = [] || n = 0 then []
  else
    let len = List.length l in
    List.init n (fun _ -> List.nth l (Random.int len))

let random p =
  let f = Random.float 1.0 in
  (* Message.result "random %.2f %.2f" f p; *)
  f < p

type ('e, 'c, 't) mutation_type =
  ((yes, 'c, yes) interpr_kind, 't) gexpr boxed ->
  ((yes, 'c, yes) interpr_kind, 't) gexpr boxed

let remove_excepts_n = ref 0

let remove_excepts (p : float) : ('e, 'c, 't) mutation_type =
 fun e ->
  match Expr.unbox e with
  | EDefault { excepts; just; cons }, m -> begin
    let len = List.length excepts in
    let excepts =
      List.filter_map
        (fun e -> if random p then Some (Expr.rebox e) else None)
        excepts
    in
    let just = Expr.rebox just in
    let cons = Expr.rebox cons in
    let diff = len - List.length excepts in
    if Global.options.debug then
      Message.debug "[mutation] Removing %n/%n excepts" diff len;
    remove_excepts_n := !remove_excepts_n + diff;
    Expr.edefault ~excepts ~just ~cons m
  end
  | _ -> e

let duplicate_excepts_n = ref 0

let duplicate_excepts : ('e, 'c, 't) mutation_type =
 fun e ->
  match Expr.unbox e with
  | EDefault { excepts; just; cons }, m -> begin
    let excepts =
      if excepts = [] then excepts
      else
        let exc = choose_in_list excepts 1 |> List.hd in
        if Global.options.debug then
          Message.debug "[mutation] Duplicating except %a" (Print.expr ()) exc;
        incr duplicate_excepts_n;
        exc :: excepts
    in
    let excepts = List.map Expr.rebox excepts in
    let just = Expr.rebox just in
    let cons = Expr.rebox cons in
    Expr.edefault ~excepts ~just ~cons m
  end
  | _ -> e

let negate_justs_n = ref 0

let negate_justs : ('e, 'c, 't) mutation_type =
 fun e ->
  match Expr.unbox e with
  | EDefault { excepts; just; cons }, m -> begin
    let excepts = List.map Expr.rebox excepts in
    let just = Expr.rebox just in
    let not_just =
      Expr.eappop
        ~op:(Operator.Not, Expr.mark_pos m)
        ~args:[just]
        ~tys:[TLit TBool, Expr.mark_pos m]
        m
    in
    let cons = Expr.rebox cons in
    if Global.options.debug then Message.debug "[mutation] Negating just";
    incr negate_justs_n;
    Expr.edefault ~excepts ~just:not_just ~cons m
  end
  | _ -> e

let apply_mutations
    (type e c)
    (mutations : ((e, c, 't) mutation_type * float) list)
    expr =
  let op = Fun.id in
  let rec f :
      ((yes, c, yes) interpr_kind, 't) gexpr ->
      ((yes, c, yes) interpr_kind, 't) gexpr boxed = function
    | (EDefault { excepts; just; cons }, m) as e ->
      if Global.options.debug then
        Message.debug "[mutation] looking at expression %a (%n)" (Print.expr ())
          e (List.length excepts);
      let excepts = List.map f excepts in
      let just = f just in
      let cons = f cons in
      List.fold_left
        (fun acc (mutation, p) ->
          if random p then begin
            if Global.options.debug then
              Message.debug "[mutation] mutating expression %a" (Print.expr ())
                e;
            mutation acc
          end
          else acc)
        (Expr.edefault ~excepts ~just ~cons m)
        mutations
    | _ as e -> Expr.map ~op ~f e
  in
  f expr

type ast_stats_t = {
  mutable defaults : int;
  mutable defaults_with_excepts : int;
  mutable excepts_sizes : int list;
  mutable ifs : int;
  mutable asserts : int;
  mutable matches : int;
  mutable match_max_arms : int;
}

let pprint_ast_stats (fmt : Format.formatter) (s : ast_stats_t) =
  let list_median l =
    let sl = List.sort Stdlib.compare l in
    List.nth sl (List.length sl / 2)
  in
  let list_mean l =
    float_of_int (List.fold_left ( + ) 0 l) /. float_of_int (List.length l)
  in
  let open Format in
  fprintf fmt "AST Stats:@\n@[<v 2>  ";
  let f = fprintf fmt "%s: %n@," in
  f "defaults" s.defaults;
  f "defaults with excepts" s.defaults_with_excepts;
  f "total excepts" (List.fold_left ( + ) 0 s.excepts_sizes);
  f "max excepts in a default" (List.fold_left max (-1) s.excepts_sizes);
  fprintf fmt "excepts: %a@,"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       Format.pp_print_int)
    (List.sort Stdlib.compare s.excepts_sizes);
  f "median number of excepts (*excl. empty defaults*)"
    (list_median (List.filter (fun s -> s > 0) s.excepts_sizes));
  fprintf fmt "mean number of excepts (*excl. empty defaults*): %.2f@,"
    (list_mean (List.filter (fun s -> s > 0) s.excepts_sizes));
  f "if-then-else" s.ifs;
  f "asserts" s.asserts;
  f "match" s.matches;
  f "max arms on a match" s.match_max_arms;
  fprintf fmt "@]"

let get_stats expr =
  let stats =
    {
      defaults = 0;
      defaults_with_excepts = 0;
      excepts_sizes = [];
      ifs = 0;
      asserts = 0;
      matches = 0;
      match_max_arms = 0;
    }
  in
  let op = Fun.id in
  let rec f :
      ((yes, 'c, yes) interpr_kind, 't) gexpr ->
      ((yes, 'c, yes) interpr_kind, 't) gexpr boxed = function
    | (EIfThenElse _, _) as e ->
      stats.ifs <- stats.ifs + 1;
      Expr.map ~op ~f e
    | (EAssert _, _) as e ->
      stats.asserts <- stats.asserts + 1;
      Expr.map ~op ~f e
    | (EMatch { cases; _ }, _) as e ->
      stats.matches <- stats.matches + 1;
      stats.match_max_arms <-
        max stats.match_max_arms (EnumConstructor.Map.keys cases |> List.length);
      Expr.map ~op ~f e
    | (EDefault { excepts; _ }, _) as e ->
      let len = List.length excepts in
      stats.excepts_sizes <- len :: stats.excepts_sizes;
      stats.defaults <- stats.defaults + 1;
      if excepts <> [] then
        stats.defaults_with_excepts <- stats.defaults_with_excepts + 1;
      Expr.map ~op ~f e
    | _ as e -> Expr.map ~op ~f e
  in
  let _ = f expr in
  stats

let mutate_default_at_index
    (type e c)
    (mutation : (e, c, 't) mutation_type)
    (only_with_excepts : bool)
    (goal : int)
    expr =
  let op = Fun.id in
  let i = ref 0 in
  let rec f :
      ((yes, c, yes) interpr_kind, 't) gexpr ->
      ((yes, c, yes) interpr_kind, 't) gexpr boxed = function
    | (EDefault { excepts; just; cons }, m) as e ->
      if excepts <> [] || not only_with_excepts then incr i;
      if !i = goal then begin
        if Global.options.debug then
          Message.debug "[mutation] looking at expression %a (%n)"
            (Print.expr ()) e (List.length excepts);
        let excepts = List.map Expr.rebox excepts in
        let just = Expr.rebox just in
        let cons = Expr.rebox cons in
        mutation (Expr.edefault ~excepts ~just ~cons m)
      end
      else Expr.map ~op ~f e
    | _ as e -> Expr.map ~op ~f e
  in
  f expr

let create_one_conflict (expr : ((yes, 'c, yes) interpr_kind, 't) gexpr) :
    ((yes, 'c, yes) interpr_kind, 't) gexpr boxed =
  let stats = get_stats expr in
  let goal = Random.int stats.defaults_with_excepts + 1 in
  mutate_default_at_index duplicate_excepts true goal expr
