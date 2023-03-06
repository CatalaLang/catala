(* λx.x *)
type term =
  | Var of term Bindlib.var
  | Abs of (term, term) Bindlib.binder
  | App of term * term

let rec eval : term -> term =
 fun t ->
  match t with
  | App (f, a) -> begin
    match eval f with Abs b -> eval (Bindlib.subst b a) | _ -> t
  end
  | _ -> t

module Expr = struct
  open Bindlib

  let mkfree : term var -> term = fun x -> Var x

  let rec size : term -> int =
   fun t ->
    match t with
    | Var _ -> 0
    | Abs b ->
      let _, t = unbind b in
      1 + size t
    | App (t, u) -> 1 + size t + size u

  let var : term var -> term box = fun x -> box_var x

  let abs_raw : (term, term) binder box -> term box =
   fun b -> box_apply (fun b -> Abs b) b

  let abs : term var -> term box -> term box = fun x t -> abs_raw (bind_var x t)

  let app : term box -> term box -> term box =
   fun t u -> box_apply2 (fun t u -> App (t, u)) t u

  let rec box_term : term -> term box =
   fun t ->
    match t with
    | Var x -> var x
    | Abs b -> abs_raw (box_binder box_term b)
    | App (t, u) -> app (box_term t) (box_term u)
end

module Var = struct
  module Generic = struct
    type t = RVar : 'e Bindlib.var -> t

    let t v = RVar v
    let get (RVar v) = Bindlib.copy_var v (fun x -> Var x) (Bindlib.name_of v)
    let compare (RVar x) (RVar y) = Bindlib.compare_vars x y
    let eq (RVar x) (RVar y) = Bindlib.eq_vars x y
  end

  module Map = struct
    open Generic
    open Map.Make (Generic)

    type nonrec ('e, 'x) t = 'x t

    let empty = empty
    let singleton v x = singleton (t v) x
    let add v x m = add (t v) x m
    let update v f m = update (t v) f m
    let find v m = find (t v) m
    let find_opt v m = find_opt (t v) m
    let bindings m = bindings m |> List.map (fun (v, x) -> get v, x)
    let mem x m = mem (t x) m
    let union f m1 m2 = union (fun v x1 x2 -> f (get v) x1 x2) m1 m2
    let fold f m acc = fold (fun v x acc -> f (get v) x acc) m acc
  end
end

(* λx.x *)
let id : term =
  let open Expr in
  let x = Bindlib.new_var mkfree "x" in
  Bindlib.unbox (abs x (var x))

(* λx.λy.x *)
let fst : term =
  let open Expr in
  let x = Bindlib.new_var mkfree "x" in
  let y = Bindlib.new_var mkfree "x" in
  Bindlib.unbox (abs x (abs y (var y)))

(* λx.(x) x) (boxed) *)
let delta : term Bindlib.box =
  let open Expr in
  let x = Bindlib.new_var mkfree "x" in
  abs x (app (var x) (var x))

(* (λx.(x) x) λx.(x) x *)
let omega : term =
  let open Expr in
  Bindlib.unbox (app delta delta)

(* λx.(x) x) *)
let delta : term =
  let open Expr in
  Bindlib.unbox delta

let rec to_string : Bindlib.ctxt -> term -> string =
 fun ctxt t ->
  match t with
  | Var x -> Bindlib.name_of x
  | Abs b ->
    let x, t, ctxt = Bindlib.unbind_in ctxt b in
    "λ" ^ Bindlib.name_of x ^ "." ^ to_string ctxt t
  | App (t, u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u

let to_string : term -> string =
 fun t -> to_string (Bindlib.free_vars (Expr.box_term t)) t

module P = struct
  type term = Var of string | Abs of string * term | App of term * term
  [@@deriving yojson]
end

let rec term_of_parseterm (ctx : ctx) (t : P.term) : term box =
  let fix =
    let ctx' = ctx in
    let fix ?(ctx : ctx option) =
      match ctx with
      | None -> term_of_parseterm ctx'
      | Some ctx -> term_of_parseterm ctx
    in
    fix
  in
  match t with
  | P.Var x -> var (find_var ctx x)
  | P.Abs (x, t) ->
    let ctx = add_var ctx x in
    abs (find_var ctx x) (fix ~ctx t)
  | P.App (t1, t2) -> app (fix t1) (fix t2)

let term_of_parseterm t : term = unbox (term_of_parseterm empty t)

let rec parseterm_of_term (ctxt : ctxt) (t : term) : P.term =
  match t with
  | Var x -> P.Var (name_of x)
  | Abs b ->
    let x, t, ctxt = unbind_in ctxt b in
    P.Abs (name_of x, parseterm_of_term ctxt t)
  | App (t1, t2) -> P.App (parseterm_of_term ctxt t1, parseterm_of_term ctxt t2)

let parseterm_of_term (t : term) : P.term =
  parseterm_of_term (free_vars (box_term t)) t

let _ =
  ListLabels.iter
    [id; fst; delta; omega; eval id]
    ~f:(fun t ->
      Format.printf "%d %s %a\n" (size t) (to_string t)
        (Yojson.Safe.pretty_print ~std:true)
        (P.yojson_of_term (parseterm_of_term t)))

let _ =
  assert (
    ListLabels.for_all [id; fst; delta; omega] ~f:(fun t ->
        let t' = parseterm_of_term t in
        parseterm_of_term (term_of_parseterm t') = t'))

let trans ctx e = assert false
