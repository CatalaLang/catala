(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Typing for the default calculus. Because of the error terms, we perform type
    inference using the classical W algorithm with union-find unification. *)

open Utils
module A = Ast
open A.Infer

(** {1 Types and unification} *)

let typ_needs_parens (t : typ Marked.pos UnionFind.elem) : bool =
  let t = UnionFind.get (UnionFind.find t) in
  match Marked.unmark t with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ
    (ctx : Ast.decl_ctx)
    (fmt : Format.formatter)
    (typ : typ Marked.pos UnionFind.elem) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens
      (fmt : Format.formatter)
      (t : typ Marked.pos UnionFind.elem) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  let typ = UnionFind.get (UnionFind.find typ) in
  match Marked.unmark typ with
  | TLit l -> Format.fprintf fmt "%a" Print.format_tlit l
  | TTuple (ts, None) ->
    Format.fprintf fmt "@[<hov 2>(%a)]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         (fun fmt t -> Format.fprintf fmt "%a" format_typ t))
      ts
  | TTuple (_ts, Some s) -> Format.fprintf fmt "%a" Ast.StructName.format_t s
  | TEnum (_ts, e) -> Format.fprintf fmt "%a" Ast.EnumName.format_t e
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a →@ %a@]" format_typ_with_parens t1
      format_typ t2
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ t1
  | TAny d -> Format.fprintf fmt "any[%d]" (Any.hash d)

exception
  Type_error of
    A.untyped A.marked_expr
    * typ Marked.pos UnionFind.elem
    * typ Marked.pos UnionFind.elem

(** Raises an error if unification cannot be performed *)
let rec unify
    (ctx : Ast.decl_ctx)
    (e : 'm A.marked_expr) (* used for error context *)
    (t1 : typ Marked.pos UnionFind.elem)
    (t2 : typ Marked.pos UnionFind.elem) : unit =
  let unify = unify ctx in
  (* Cli.debug_format "Unifying %a and %a" (format_typ ctx) t1 (format_typ ctx)
     t2; *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let raise_type_error () =
    raise (Type_error (Bindlib.unbox (A.untype_expr e), t1, t2))
  in
  let repr =
    match Marked.unmark t1_repr, Marked.unmark t2_repr with
    | TLit tl1, TLit tl2 when tl1 = tl2 -> None
    | TArrow (t11, t12), TArrow (t21, t22) ->
      unify e t11 t21;
      unify e t12 t22;
      None
    | TTuple (ts1, s1), TTuple (ts2, s2) ->
      if s1 = s2 && List.length ts1 = List.length ts2 then begin
        List.iter2 (unify e) ts1 ts2;
        None
      end
      else raise_type_error ()
    | TEnum (ts1, e1), TEnum (ts2, e2) ->
      if e1 = e2 && List.length ts1 = List.length ts2 then begin
        List.iter2 (unify e) ts1 ts2;
        None
      end
      else raise_type_error ()
    | TArray t1', TArray t2' ->
      unify e t1' t2';
      None
    | TAny _, TAny _ -> None
    | TAny _, _ -> Some t2_repr
    | _, TAny _ -> Some t1_repr
    | _ -> raise_type_error ()
  in
  let t_union = UnionFind.union t1 t2 in
  match repr with None -> () | Some t_repr -> UnionFind.set t_union t_repr

let handle_type_error ctx e t1 t2 =
  (* TODO: if we get weird error messages, then it means that we should use the
     persistent version of the union-find data structure. *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let t1_pos = Marked.get_mark t1_repr in
  let t2_pos = Marked.get_mark t2_repr in
  let unformat_typ typ =
    let buf = Buffer.create 59 in
    let ppf = Format.formatter_of_buffer buf in
    (* set infinite width to disable line cuts *)
    Format.pp_set_margin ppf max_int;
    format_typ ctx ppf typ;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let t1_s fmt () =
    Cli.format_with_style [ANSITerminal.yellow] fmt (unformat_typ t1)
  in
  let t2_s fmt () =
    Cli.format_with_style [ANSITerminal.yellow] fmt (unformat_typ t2)
  in
  Errors.raise_multispanned_error
    [
      ( Some
          (Format.asprintf
             "Error coming from typechecking the following expression:"),
        A.pos e );
      Some (Format.asprintf "Type %a coming from expression:" t1_s ()), t1_pos;
      Some (Format.asprintf "Type %a coming from expression:" t2_s ()), t2_pos;
    ]
    "Error during typechecking, incompatible types:\n%a %a\n%a %a"
    (Cli.format_with_style [ANSITerminal.blue; ANSITerminal.Bold])
    "-->" t1_s ()
    (Cli.format_with_style [ANSITerminal.blue; ANSITerminal.Bold])
    "-->" t2_s ()

(** Operators have a single type, instead of being polymorphic with constraints.
    This allows us to have a simpler type system, while we argue the syntactic
    burden of operator annotations helps the programmer visualize the type flow
    in the code. *)
let op_type (op : A.operator Marked.pos) : typ Marked.pos UnionFind.elem =
  let pos = Marked.get_mark op in
  let bt = UnionFind.make (TLit TBool, pos) in
  let it = UnionFind.make (TLit TInt, pos) in
  let rt = UnionFind.make (TLit TRat, pos) in
  let mt = UnionFind.make (TLit TMoney, pos) in
  let dut = UnionFind.make (TLit TDuration, pos) in
  let dat = UnionFind.make (TLit TDate, pos) in
  let any = UnionFind.make (TAny (Any.fresh ()), pos) in
  let array_any = UnionFind.make (TArray any, pos) in
  let any2 = UnionFind.make (TAny (Any.fresh ()), pos) in
  let array_any2 = UnionFind.make (TArray any2, pos) in
  let arr x y = UnionFind.make (TArrow (x, y), pos) in
  match Marked.unmark op with
  | A.Ternop A.Fold ->
    arr (arr any2 (arr any any2)) (arr any2 (arr array_any any2))
  | A.Binop (A.And | A.Or | A.Xor) -> arr bt (arr bt bt)
  | A.Binop (A.Add KInt | A.Sub KInt | A.Mult KInt | A.Div KInt) ->
    arr it (arr it it)
  | A.Binop (A.Add KRat | A.Sub KRat | A.Mult KRat | A.Div KRat) ->
    arr rt (arr rt rt)
  | A.Binop (A.Add KMoney | A.Sub KMoney) -> arr mt (arr mt mt)
  | A.Binop (A.Add KDuration | A.Sub KDuration) -> arr dut (arr dut dut)
  | A.Binop (A.Sub KDate) -> arr dat (arr dat dut)
  | A.Binop (A.Add KDate) -> arr dat (arr dut dat)
  | A.Binop (A.Div KDuration) -> arr dut (arr dut rt)
  | A.Binop (A.Mult KDuration) -> arr dut (arr it dut)
  | A.Binop (A.Div KMoney) -> arr mt (arr mt rt)
  | A.Binop (A.Mult KMoney) -> arr mt (arr rt mt)
  | A.Binop (A.Lt KInt | A.Lte KInt | A.Gt KInt | A.Gte KInt) ->
    arr it (arr it bt)
  | A.Binop (A.Lt KRat | A.Lte KRat | A.Gt KRat | A.Gte KRat) ->
    arr rt (arr rt bt)
  | A.Binop (A.Lt KMoney | A.Lte KMoney | A.Gt KMoney | A.Gte KMoney) ->
    arr mt (arr mt bt)
  | A.Binop (A.Lt KDate | A.Lte KDate | A.Gt KDate | A.Gte KDate) ->
    arr dat (arr dat bt)
  | A.Binop (A.Lt KDuration | A.Lte KDuration | A.Gt KDuration | A.Gte KDuration)
    ->
    arr dut (arr dut bt)
  | A.Binop (A.Eq | A.Neq) -> arr any (arr any bt)
  | A.Binop A.Map -> arr (arr any any2) (arr array_any array_any2)
  | A.Binop A.Filter -> arr (arr any bt) (arr array_any array_any)
  | A.Binop A.Concat -> arr array_any (arr array_any array_any)
  | A.Unop (A.Minus KInt) -> arr it it
  | A.Unop (A.Minus KRat) -> arr rt rt
  | A.Unop (A.Minus KMoney) -> arr mt mt
  | A.Unop (A.Minus KDuration) -> arr dut dut
  | A.Unop A.Not -> arr bt bt
  | A.Unop (A.Log (A.PosRecordIfTrueBool, _)) -> arr bt bt
  | A.Unop (A.Log _) -> arr any any
  | A.Unop A.Length -> arr array_any it
  | A.Unop A.GetDay -> arr dat it
  | A.Unop A.GetMonth -> arr dat it
  | A.Unop A.GetYear -> arr dat it
  | A.Unop A.RoundMoney -> arr mt mt
  | A.Unop A.RoundDecimal -> arr rt rt
  | A.Unop A.IntToRat -> arr it rt
  | Binop (Mult KDate) | Binop (Div KDate) | Unop (Minus KDate) ->
    Errors.raise_spanned_error pos "This operator is not available!"

(** {1 Double-directed typing} *)

type env = typ Marked.pos UnionFind.elem A.VarMap.t

let translate_var v = Bindlib.copy_var v (fun x -> A.EVar x) (Bindlib.name_of v)
let add_pos e ty = Marked.mark (A.pos e) ty
let ty (_, A.Inferring { A.uf; _ }) = uf
let ( let+ ) x f = Bindlib.box_apply f x
let ( and+ ) x1 x2 = Bindlib.box_pair x1 x2

(* Maps a boxing function on a list, returning a boxed list *)
let bmap (f : 'a -> 'b Bindlib.box) (es : 'a list) : 'b list Bindlib.box =
  List.fold_right
    (fun e acc ->
      let+ e' = f e and+ acc in
      e' :: acc)
    es (Bindlib.box [])

(* Likewise, but with a function of two arguments on two lists of identical
   lengths *)
let bmap2 (f : 'a -> 'b -> 'c Bindlib.box) (es : 'a list) (xs : 'b list) :
    'c list Bindlib.box =
  List.fold_right2
    (fun e x acc ->
      let+ e' = f e x and+ acc in
      e' :: acc)
    es xs (Bindlib.box [])

let box_ty e = Bindlib.unbox (Bindlib.box_apply ty e)

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up
    (ctx : Ast.decl_ctx)
    (env : env)
    (e : 'm A.marked_expr) : A.inferring A.marked_expr Bindlib.box =
  (* Cli.debug_format "Looking for type of %a" (Print.format_expr ~debug:true
     ctx) e; *)
  let pos_e = A.pos e in
  let mark (e : A.inferring A.expr) uf =
    Marked.mark (A.Inferring { A.uf; pos = pos_e }) e
  in
  let unionfind_make ?(pos = e) t = UnionFind.make (add_pos pos t) in
  let mark_with_uf e1 ?pos ty = mark e1 (unionfind_make ?pos ty) in
  match Marked.unmark e with
  | A.EVar v -> begin
    match A.VarMap.find_opt (A.Var.t v) env with
    | Some t ->
      let+ v' = Bindlib.box_var (translate_var v) in
      mark v' t
    | None ->
      Errors.raise_spanned_error (A.pos e)
        "Variable %s not found in the current context." (Bindlib.name_of v)
  end
  | A.ELit (LBool _) as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TBool)
  | A.ELit (LInt _) as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TInt)
  | A.ELit (LRat _) as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TRat)
  | A.ELit (LMoney _) as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TMoney)
  | A.ELit (LDate _) as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TDate)
  | A.ELit (LDuration _) as e1 ->
    Bindlib.box @@ mark_with_uf e1 (TLit TDuration)
  | A.ELit LUnit as e1 -> Bindlib.box @@ mark_with_uf e1 (TLit TUnit)
  | A.ELit LEmptyError as e1 ->
    Bindlib.box @@ mark_with_uf e1 (TAny (Any.fresh ()))
  | A.ETuple (es, s) ->
    let+ es = bmap (typecheck_expr_bottom_up ctx env) es in
    mark_with_uf (ETuple (es, s)) (TTuple (List.map ty es, s))
  | A.ETupleAccess (e1, n, s, typs) -> begin
    let utyps = List.map ast_to_typ typs in
    let+ e1 =
      typecheck_expr_top_down ctx env (unionfind_make (TTuple (utyps, s))) e1
    in
    match List.nth_opt utyps n with
    | Some t' -> mark (ETupleAccess (e1, n, s, typs)) t'
    | None ->
      Errors.raise_spanned_error (A.pos e1)
        "Expression should have a tuple type with at least %d elements but \
         only has %d"
        n (List.length typs)
  end
  | A.EInj (e1, n, e_name, ts) ->
    let ts' = List.map ast_to_typ ts in
    let ts_n =
      match List.nth_opt ts' n with
      | Some ts_n -> ts_n
      | None ->
        Errors.raise_spanned_error (A.pos e)
          "Expression should have a sum type with at least %d cases but only \
           has %d"
          n (List.length ts')
    in
    let+ e1' = typecheck_expr_top_down ctx env ts_n e1 in
    mark_with_uf (A.EInj (e1', n, e_name, ts)) (TEnum (ts', e_name))
  | A.EMatch (e1, es, e_name) ->
    let enum_cases =
      List.map (fun e' -> unionfind_make ~pos:e' (TAny (Any.fresh ()))) es
    in
    let t_e1 = UnionFind.make (add_pos e1 (TEnum (enum_cases, e_name))) in
    let t_ret = unionfind_make ~pos:e (TAny (Any.fresh ())) in
    let+ e1' = typecheck_expr_top_down ctx env t_e1 e1
    and+ es' =
      bmap2
        (fun es' enum_t ->
          typecheck_expr_top_down ctx env
            (unionfind_make ~pos:es' (TArrow (enum_t, t_ret)))
            es')
        es enum_cases
    in
    mark (EMatch (e1', es', e_name)) t_ret
  | A.EAbs (binder, taus) ->
    if Bindlib.mbinder_arity binder <> List.length taus then
      Errors.raise_spanned_error (A.pos e)
        "function has %d variables but was supplied %d types"
        (Bindlib.mbinder_arity binder)
        (List.length taus)
    else
      let xs, body = Bindlib.unmbind binder in
      let xs' = Array.map translate_var xs in
      let xstaus = List.mapi (fun i tau -> xs'.(i), ast_to_typ tau) taus in
      let env =
        List.fold_left
          (fun env (x, tau) -> A.VarMap.add (A.Var.t x) tau env)
          env xstaus
      in
      let body' = typecheck_expr_bottom_up ctx env body in
      let t_func =
        List.fold_right
          (fun (_, t_arg) acc -> unionfind_make (TArrow (t_arg, acc)))
          xstaus (box_ty body')
      in
      let+ binder' = Bindlib.bind_mvar xs' body' in
      mark (EAbs (binder', taus)) t_func
  | A.EApp (e1, args) ->
    let args' = bmap (typecheck_expr_bottom_up ctx env) args in
    let t_ret = unionfind_make (TAny (Any.fresh ())) in
    let t_func =
      List.fold_right
        (fun ty_arg acc -> unionfind_make (TArrow (ty_arg, acc)))
        (Bindlib.unbox (Bindlib.box_apply (List.map ty) args'))
        t_ret
    in
    let+ e1' = typecheck_expr_bottom_up ctx env e1 and+ args' in
    unify ctx e (ty e1') t_func;
    mark (EApp (e1', args')) t_ret
  | A.EOp op as e1 -> Bindlib.box @@ mark e1 (op_type (Marked.mark pos_e op))
  | A.EDefault (excepts, just, cons) ->
    let just' =
      typecheck_expr_top_down ctx env
        (unionfind_make ~pos:just (TLit TBool))
        just
    in
    let cons' = typecheck_expr_bottom_up ctx env cons in
    let tau = box_ty cons' in
    let+ just'
    and+ cons'
    and+ excepts' =
      bmap (fun except -> typecheck_expr_top_down ctx env tau except) excepts
    in
    mark (A.EDefault (excepts', just', cons')) tau
  | A.EIfThenElse (cond, et, ef) ->
    let cond' =
      typecheck_expr_top_down ctx env
        (unionfind_make ~pos:cond (TLit TBool))
        cond
    in
    let et' = typecheck_expr_bottom_up ctx env et in
    let tau = box_ty et' in
    let+ cond' and+ et' and+ ef' = typecheck_expr_top_down ctx env tau ef in
    mark (A.EIfThenElse (cond', et', ef')) tau
  | A.EAssert e1 ->
    let+ e1' =
      typecheck_expr_top_down ctx env (unionfind_make ~pos:e1 (TLit TBool)) e1
    in
    mark_with_uf (A.EAssert e1') ~pos:e1 (TLit TUnit)
  | A.ErrorOnEmpty e1 ->
    let+ e1' = typecheck_expr_bottom_up ctx env e1 in
    mark (A.ErrorOnEmpty e1') (ty e1')
  | A.EArray es ->
    let cell_type = unionfind_make (TAny (Any.fresh ())) in
    let+ es' =
      bmap
        (fun e1 ->
          let e1' = typecheck_expr_bottom_up ctx env e1 in
          unify ctx e1 cell_type (box_ty e1');
          e1')
        es
    in
    mark_with_uf (A.EArray es') (TArray cell_type)

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down
    (ctx : Ast.decl_ctx)
    (env : env)
    (tau : typ Marked.pos UnionFind.elem)
    (e : 'm A.marked_expr) : A.inferring A.marked_expr Bindlib.box =
  (* Cli.debug_format "Propagating type %a for expr %a" (format_typ ctx) tau
     (Print.format_expr ctx) e; *)
  let pos_e = A.pos e in
  let mark e = Marked.mark (A.Inferring { uf = tau; pos = pos_e }) e in
  let unify_and_mark (e : A.inferring A.expr) tau' =
    let e = Marked.mark (A.Inferring { uf = tau'; pos = pos_e }) e in
    unify ctx (Bindlib.unbox (A.untype_expr e)) tau tau';
    e
  in
  let unionfind_make ?(pos = e) t = UnionFind.make (add_pos pos t) in
  match Marked.unmark e with
  | A.EVar v -> begin
    match A.VarMap.find_opt (A.Var.t v) env with
    | Some tau' ->
      let+ v' = Bindlib.box_var (translate_var v) in
      unify_and_mark v' tau'
    | None ->
      Errors.raise_spanned_error (A.pos e)
        "Variable %s not found in the current context" (Bindlib.name_of v)
  end
  | A.ELit (LBool _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TBool))
  | A.ELit (LInt _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TInt))
  | A.ELit (LRat _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TRat))
  | A.ELit (LMoney _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TMoney))
  | A.ELit (LDate _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TDate))
  | A.ELit (LDuration _) as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TDuration))
  | A.ELit LUnit as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TLit TUnit))
  | A.ELit LEmptyError as e1 ->
    Bindlib.box @@ unify_and_mark e1 (unionfind_make (TAny (Any.fresh ())))
  | A.ETuple (es, s) ->
    let+ es' = bmap (typecheck_expr_bottom_up ctx env) es in
    unify_and_mark
      (A.ETuple (es', s))
      (unionfind_make (TTuple (List.map ty es', s)))
  | A.ETupleAccess (e1, n, s, typs) -> begin
    let typs' = List.map ast_to_typ typs in
    let+ e1' =
      typecheck_expr_top_down ctx env (unionfind_make (TTuple (typs', s))) e1
    in
    match List.nth_opt typs' n with
    | Some t1n -> unify_and_mark (A.ETupleAccess (e1', n, s, typs)) t1n
    | None ->
      Errors.raise_spanned_error (Ast.pos e1)
        "Expression should have a tuple type with at least %d elements but \
         only has %d"
        n (List.length typs)
  end
  | A.EInj (e1, n, e_name, ts) ->
    let ts' = List.map ast_to_typ ts in
    let ts_n =
      match List.nth_opt ts' n with
      | Some ts_n -> ts_n
      | None ->
        Errors.raise_spanned_error (A.pos e)
          "Expression should have a sum type with at least %d cases but only \
           has %d"
          n (List.length ts)
    in
    let+ e1' = typecheck_expr_top_down ctx env ts_n e1 in
    unify_and_mark
      (A.EInj (e1', n, e_name, ts))
      (unionfind_make (TEnum (ts', e_name)))
  | A.EMatch (e1, es, e_name) ->
    let enum_cases =
      List.map (fun e' -> unionfind_make ~pos:e' (TAny (Any.fresh ()))) es
    in
    let e1' =
      typecheck_expr_top_down ctx env
        (unionfind_make ~pos:e1 (TEnum (enum_cases, e_name)))
        e1
    in
    let t_ret = unionfind_make ~pos:e (TAny (Any.fresh ())) in
    let+ e1'
    and+ es' =
      bmap2
        (fun es' enum_t ->
          typecheck_expr_top_down ctx env
            (unionfind_make ~pos:es' (TArrow (enum_t, t_ret)))
            es')
        es enum_cases
    in
    unify_and_mark (EMatch (e1', es', e_name)) t_ret
  | A.EAbs (binder, t_args) ->
    if Bindlib.mbinder_arity binder <> List.length t_args then
      Errors.raise_spanned_error (A.pos e)
        "function has %d variables but was supplied %d types"
        (Bindlib.mbinder_arity binder)
        (List.length t_args)
    else
      let xs, body = Bindlib.unmbind binder in
      let xs' = Array.map translate_var xs in
      let xstaus =
        List.map2 (fun x t_arg -> x, ast_to_typ t_arg) (Array.to_list xs) t_args
      in
      let env =
        List.fold_left
          (fun env (x, t_arg) -> A.VarMap.add (A.Var.t x) t_arg env)
          env xstaus
      in
      let body' = typecheck_expr_bottom_up ctx env body in
      let t_func =
        List.fold_right
          (fun (_, t_arg) acc -> unionfind_make (TArrow (t_arg, acc)))
          xstaus (box_ty body')
      in
      let+ binder' = Bindlib.bind_mvar xs' body' in
      unify_and_mark (EAbs (binder', t_args)) t_func
  | A.EApp (e1, args) ->
    let+ args' = bmap (typecheck_expr_bottom_up ctx env) args
    and+ e1' = typecheck_expr_bottom_up ctx env e1 in
    let t_func =
      List.fold_right
        (fun arg acc -> unionfind_make (TArrow (ty arg, acc)))
        args' tau
    in
    unify ctx e (ty e1') t_func;
    unify_and_mark (EApp (e1', args')) tau
  | A.EOp op as e1 ->
    let op_typ = op_type (add_pos e op) in
    Bindlib.box (unify_and_mark e1 op_typ)
  | A.EDefault (excepts, just, cons) ->
    let+ just' =
      typecheck_expr_top_down ctx env
        (unionfind_make ~pos:just (TLit TBool))
        just
    and+ cons' = typecheck_expr_top_down ctx env tau cons
    and+ excepts' = bmap (typecheck_expr_top_down ctx env tau) excepts in
    mark (A.EDefault (excepts', just', cons'))
  | A.EIfThenElse (cond, et, ef) ->
    let+ cond' =
      typecheck_expr_top_down ctx env
        (unionfind_make ~pos:cond (TLit TBool))
        cond
    and+ et' = typecheck_expr_top_down ctx env tau et
    and+ ef' = typecheck_expr_top_down ctx env tau ef in
    mark (A.EIfThenElse (cond', et', ef'))
  | A.EAssert e1 ->
    let+ e1' =
      typecheck_expr_top_down ctx env (unionfind_make ~pos:e1 (TLit TBool)) e1
    in
    unify_and_mark (EAssert e1') (unionfind_make ~pos:e1 (TLit TUnit))
  | A.ErrorOnEmpty e1 ->
    let+ e1' = typecheck_expr_top_down ctx env tau e1 in
    mark (A.ErrorOnEmpty e1')
  | A.EArray es ->
    let cell_type = unionfind_make (TAny (Any.fresh ())) in
    let+ es' =
      bmap
        (fun e1 ->
          let e1' = typecheck_expr_bottom_up ctx env e1 in
          unify ctx e cell_type (box_ty e1');
          e1')
        es
    in
    unify_and_mark (A.EArray es') (unionfind_make (TArray cell_type))

let wrap ctx f e =
  try f e
  with Type_error (e, ty1, ty2) -> (
    let bt = Printexc.get_raw_backtrace () in
    try handle_type_error ctx e ty1 ty2
    with e -> Printexc.raise_with_backtrace e bt)

(** {1 API} *)

let get_ty_mark (A.Inferring { uf; pos }) =
  A.Typed { ty = A.Infer.typ_to_ast uf; pos }

(* Infer the type of an expression *)
let infer_types (ctx : Ast.decl_ctx) (e : 'm A.marked_expr) :
    Ast.typed Ast.marked_expr Bindlib.box =
  A.map_expr_marks ~f:get_ty_mark
  @@ Bindlib.unbox
  @@ wrap ctx (typecheck_expr_bottom_up ctx A.VarMap.empty) e

let infer_type (type m) ctx (e : m A.marked_expr) =
  match Marked.get_mark e with
  | A.Typed { ty; _ } -> ty
  | A.Inferring { uf; _ } -> typ_to_ast uf
  | A.Untyped _ -> A.ty (Bindlib.unbox (infer_types ctx e))

(** Typechecks an expression given an expected type *)
let check_type
    (ctx : Ast.decl_ctx)
    (e : 'm A.marked_expr)
    (tau : A.typ Marked.pos) =
  (* todo: consider using the already inferred type if ['m] = [typed] *)
  ignore
  @@ wrap ctx (typecheck_expr_top_down ctx A.VarMap.empty (ast_to_typ tau)) e

let infer_types_program prg =
  let ctx = prg.A.decl_ctx in
  let rec process_scopes env = function
    | A.Nil -> Bindlib.box A.Nil
    | A.ScopeDef
        {
          scope_next;
          scope_name;
          scope_body =
            {
              scope_body_input_struct = s_in;
              scope_body_output_struct = s_out;
              scope_body_expr = body;
            };
        } ->
      let scope_pos = Marked.get_mark (A.ScopeName.get_info scope_name) in
      let struct_ty struct_name =
        let struc = A.StructMap.find struct_name ctx.A.ctx_structs in
        ast_to_typ
          (Marked.mark scope_pos
             (A.TTuple (List.map snd struc, Some struct_name)))
      in
      let ty_in = struct_ty s_in in
      let ty_out = struct_ty s_out in
      let ty_scope =
        UnionFind.make (Marked.mark scope_pos (TArrow (ty_in, ty_out)))
      in
      let rec process_scope_body_expr env = function
        | A.Result e ->
          let e' = typecheck_expr_bottom_up ctx env e in
          Bindlib.box_apply
            (fun e ->
              unify ctx e (ty e) ty_out;
              A.Result e)
            e'
        | A.ScopeLet
            {
              scope_let_kind;
              scope_let_typ;
              scope_let_expr = e;
              scope_let_next;
              scope_let_pos;
            } ->
          let ty_e = ast_to_typ scope_let_typ in
          let e = typecheck_expr_bottom_up ctx env e in
          let var, next = Bindlib.unbind scope_let_next in
          let env = A.VarMap.add (A.Var.t var) ty_e env in
          let next = process_scope_body_expr env next in
          let scope_let_next = Bindlib.bind_var (translate_var var) next in
          Bindlib.box_apply2
            (fun scope_let_expr scope_let_next ->
              unify ctx scope_let_expr (ty scope_let_expr) ty_e;
              A.ScopeLet
                {
                  scope_let_kind;
                  scope_let_typ;
                  scope_let_expr;
                  scope_let_next;
                  scope_let_pos;
                })
            e scope_let_next
      in
      let scope_body_expr =
        let var, e = Bindlib.unbind body in
        let env = A.VarMap.add (A.Var.t var) ty_in env in
        let e' = process_scope_body_expr env e in
        let e' =
          Bindlib.box_apply
            (fun e ->
              Bindlib.unbox
              @@ A.map_exprs_in_scope_lets ~varf:translate_var
                   ~f:
                     (A.map_expr_top_down ~f:(fun e ->
                          Marked.(mark (get_ty_mark (get_mark e)) (unmark e))))
                   e)
            e'
        in
        Bindlib.bind_var (translate_var var) e'
      in
      let scope_next =
        let scope_var, next = Bindlib.unbind scope_next in
        let env = A.VarMap.add (A.Var.t scope_var) ty_scope env in
        let next' = process_scopes env next in
        Bindlib.bind_var (translate_var scope_var) next'
      in
      Bindlib.box_apply2
        (fun scope_body_expr scope_next ->
          A.ScopeDef
            {
              scope_next;
              scope_name;
              scope_body =
                {
                  scope_body_input_struct = s_in;
                  scope_body_output_struct = s_out;
                  scope_body_expr;
                };
            })
        scope_body_expr scope_next
  in
  let scopes = wrap ctx (process_scopes A.VarMap.empty) prg.scopes in
  Bindlib.box_apply (fun scopes -> { A.decl_ctx = ctx; scopes }) scopes
  |> Bindlib.unbox
