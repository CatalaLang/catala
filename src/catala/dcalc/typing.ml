(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Typing for the default calculus. Because of the error terms, we perform type inference using the
    classical W algorithm with union-find unification. *)

open Utils
module A = Ast

(** {1 Types and unification} *)

module Any =
  Utils.Uid.Make
    (struct
      type info = unit

      let format_info fmt () = Format.fprintf fmt "any"
    end)
    ()

(** We do not reuse {!type: Dcalc.Ast.typ} because we have to include a new [TAny] variant. Indeed,
    error terms can have any type and this has to be captured by the type sytem. *)
type typ =
  | TLit of A.typ_lit
  | TArrow of typ Pos.marked UnionFind.elem * typ Pos.marked UnionFind.elem
  | TTuple of typ Pos.marked UnionFind.elem list * Ast.StructName.t option
  | TEnum of typ Pos.marked UnionFind.elem list * Ast.EnumName.t
  | TArray of typ Pos.marked UnionFind.elem
  | TAny of Any.t

let typ_needs_parens (t : typ Pos.marked UnionFind.elem) : bool =
  let t = UnionFind.get (UnionFind.find t) in
  match Pos.unmark t with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ (ctx : Ast.decl_ctx) (fmt : Format.formatter)
    (typ : typ Pos.marked UnionFind.elem) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : typ Pos.marked UnionFind.elem) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  let typ = UnionFind.get (UnionFind.find typ) in
  match Pos.unmark typ with
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
      Format.fprintf fmt "@[<hov 2>%a →@ %a@]" format_typ_with_parens t1 format_typ t2
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ t1
  | TAny d -> Format.fprintf fmt "any[%d]" (Any.hash d)

(** Raises an error if unification cannot be performed *)
let rec unify (ctx : Ast.decl_ctx) (t1 : typ Pos.marked UnionFind.elem)
    (t2 : typ Pos.marked UnionFind.elem) : unit =
  let unify = unify ctx in
  (* Cli.debug_print (Format.asprintf "Unifying %a and %a" (format_typ ctx) t1 (format_typ ctx) t2); *)
  let t1_repr = UnionFind.get (UnionFind.find t1) in
  let t2_repr = UnionFind.get (UnionFind.find t2) in
  let raise_type_error (t1_pos : Pos.t) (t2_pos : Pos.t) : 'a =
    (* TODO: if we get weird error messages, then it means that we should use the persistent version
       of the union-find data structure. *)
    let t1_s =
      Cli.print_with_style [ ANSITerminal.yellow ] "%s"
        (Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
           ~subst:(fun _ -> " ")
           (Format.asprintf "%a" (format_typ ctx) t1))
    in
    let t2_s =
      Cli.print_with_style [ ANSITerminal.yellow ] "%s"
        (Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
           ~subst:(fun _ -> " ")
           (Format.asprintf "%a" (format_typ ctx) t2))
    in
    Errors.raise_multispanned_error
      (Format.asprintf "Error during typechecking, incompatible types:\n%s %s\n%s %s"
         (Cli.print_with_style [ ANSITerminal.blue; ANSITerminal.Bold ] "-->")
         t1_s
         (Cli.print_with_style [ ANSITerminal.blue; ANSITerminal.Bold ] "-->")
         t2_s)
      [
        (Some (Format.asprintf "Type %s coming from expression:" t1_s), t1_pos);
        (Some (Format.asprintf "Type %s coming from expression:" t2_s), t2_pos);
      ]
  in
  let repr =
    match (t1_repr, t2_repr) with
    | (TLit tl1, _), (TLit tl2, _) when tl1 = tl2 -> None
    | (TArrow (t11, t12), _), (TArrow (t21, t22), _) ->
        unify t11 t21;
        unify t12 t22;
        None
    | (TTuple (ts1, s1), t1_pos), (TTuple (ts2, s2), t2_pos) ->
        if s1 = s2 && List.length ts1 = List.length ts2 then begin
          List.iter2 unify ts1 ts2;
          None
        end
        else raise_type_error t1_pos t2_pos
    | (TEnum (ts1, e1), t1_pos), (TEnum (ts2, e2), t2_pos) ->
        if e1 = e2 && List.length ts1 = List.length ts2 then begin
          List.iter2 unify ts1 ts2;
          None
        end
        else raise_type_error t1_pos t2_pos
    | (TArray t1', _), (TArray t2', _) ->
        unify t1' t2';
        None
    | (TAny _, _), (TAny _, _) -> None
    | (TAny _, _), t_repr | t_repr, (TAny _, _) -> Some t_repr
    | (_, t1_pos), (_, t2_pos) -> raise_type_error t1_pos t2_pos
  in
  let t_union = UnionFind.union t1 t2 in
  match repr with None -> () | Some t_repr -> UnionFind.set t_union t_repr

(** Operators have a single type, instead of being polymorphic with constraints. This allows us to
    have a simpler type system, while we argue the syntactic burden of operator annotations helps
    the programmer visualize the type flow in the code. *)
let op_type (op : A.operator Pos.marked) : typ Pos.marked UnionFind.elem =
  let pos = Pos.get_position op in
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
  match Pos.unmark op with
  | A.Ternop A.Fold -> arr (arr any2 (arr any any2)) (arr any2 (arr array_any any2))
  | A.Binop (A.And | A.Or | A.Xor) -> arr bt (arr bt bt)
  | A.Binop (A.Add KInt | A.Sub KInt | A.Mult KInt | A.Div KInt) -> arr it (arr it it)
  | A.Binop (A.Add KRat | A.Sub KRat | A.Mult KRat | A.Div KRat) -> arr rt (arr rt rt)
  | A.Binop (A.Add KMoney | A.Sub KMoney) -> arr mt (arr mt mt)
  | A.Binop (A.Add KDuration | A.Sub KDuration) -> arr dut (arr dut dut)
  | A.Binop (A.Sub KDate) -> arr dat (arr dat dut)
  | A.Binop (A.Add KDate) -> arr dat (arr dut dat)
  | A.Binop (A.Div KMoney) -> arr mt (arr mt rt)
  | A.Binop (A.Mult KMoney) -> arr mt (arr rt mt)
  | A.Binop (A.Lt KInt | A.Lte KInt | A.Gt KInt | A.Gte KInt) -> arr it (arr it bt)
  | A.Binop (A.Lt KRat | A.Lte KRat | A.Gt KRat | A.Gte KRat) -> arr rt (arr rt bt)
  | A.Binop (A.Lt KMoney | A.Lte KMoney | A.Gt KMoney | A.Gte KMoney) -> arr mt (arr mt bt)
  | A.Binop (A.Lt KDate | A.Lte KDate | A.Gt KDate | A.Gte KDate) -> arr dat (arr dat bt)
  | A.Binop (A.Lt KDuration | A.Lte KDuration | A.Gt KDuration | A.Gte KDuration) ->
      arr dut (arr dut bt)
  | A.Binop (A.Eq | A.Neq) -> arr any (arr any bt)
  | A.Binop A.Map -> arr (arr any any2) (arr array_any array_any2)
  | A.Binop A.Filter -> arr (arr any bt) (arr array_any array_any)
  | A.Unop (A.Minus KInt) -> arr it it
  | A.Unop (A.Minus KRat) -> arr rt rt
  | A.Unop (A.Minus KMoney) -> arr mt mt
  | A.Unop (A.Minus KDuration) -> arr dut dut
  | A.Unop A.Not -> arr bt bt
  | A.Unop A.ErrorOnEmpty -> arr any any
  | A.Unop (A.Log (A.PosRecordIfTrueBool, _)) -> arr bt bt
  | A.Unop (A.Log _) -> arr any any
  | A.Unop A.Length -> arr array_any it
  | A.Unop A.GetDay -> arr dat it
  | A.Unop A.GetMonth -> arr dat it
  | A.Unop A.GetYear -> arr dat it
  | A.Unop A.IntToRat -> arr it rt
  | Binop (Mult (KDate | KDuration)) | Binop (Div (KDate | KDuration)) | Unop (Minus KDate) ->
      Errors.raise_spanned_error "This operator is not available!" pos

let rec ast_to_typ (ty : A.typ) : typ =
  match ty with
  | A.TLit l -> TLit l
  | A.TArrow (t1, t2) ->
      TArrow
        ( UnionFind.make (Pos.map_under_mark ast_to_typ t1),
          UnionFind.make (Pos.map_under_mark ast_to_typ t2) )
  | A.TTuple (ts, s) ->
      TTuple (List.map (fun t -> UnionFind.make (Pos.map_under_mark ast_to_typ t)) ts, s)
  | A.TEnum (ts, e) ->
      TEnum (List.map (fun t -> UnionFind.make (Pos.map_under_mark ast_to_typ t)) ts, e)
  | A.TArray t -> TArray (UnionFind.make (Pos.map_under_mark ast_to_typ t))
  | A.TAny -> TAny (Any.fresh ())

let rec typ_to_ast (ty : typ Pos.marked UnionFind.elem) : A.typ Pos.marked =
  Pos.map_under_mark
    (fun ty ->
      match ty with
      | TLit l -> A.TLit l
      | TTuple (ts, s) -> A.TTuple (List.map typ_to_ast ts, s)
      | TEnum (ts, e) -> A.TEnum (List.map typ_to_ast ts, e)
      | TArrow (t1, t2) -> A.TArrow (typ_to_ast t1, typ_to_ast t2)
      | TAny _ -> A.TAny
      | TArray t1 -> A.TArray (typ_to_ast t1))
    (UnionFind.get (UnionFind.find ty))

(** {1 Double-directed typing} *)

type env = typ Pos.marked UnionFind.elem A.VarMap.t

(** Infers the most permissive type from an expression *)
let rec typecheck_expr_bottom_up (ctx : Ast.decl_ctx) (env : env) (e : A.expr Pos.marked) :
    typ Pos.marked UnionFind.elem =
  (* Cli.debug_print (Format.asprintf "Looking for type of %a" (Print.format_expr ctx) e); *)
  try
    let out =
      match Pos.unmark e with
      | EVar v -> (
          match A.VarMap.find_opt (Pos.unmark v) env with
          | Some t -> t
          | None ->
              Errors.raise_spanned_error "Variable not found in the current context"
                (Pos.get_position e))
      | ELit (LBool _) -> UnionFind.make (Pos.same_pos_as (TLit TBool) e)
      | ELit (LInt _) -> UnionFind.make (Pos.same_pos_as (TLit TInt) e)
      | ELit (LRat _) -> UnionFind.make (Pos.same_pos_as (TLit TRat) e)
      | ELit (LMoney _) -> UnionFind.make (Pos.same_pos_as (TLit TMoney) e)
      | ELit (LDate _) -> UnionFind.make (Pos.same_pos_as (TLit TDate) e)
      | ELit (LDuration _) -> UnionFind.make (Pos.same_pos_as (TLit TDuration) e)
      | ELit LUnit -> UnionFind.make (Pos.same_pos_as (TLit TUnit) e)
      | ELit LEmptyError -> UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e)
      | ETuple (es, s) ->
          let ts = List.map (typecheck_expr_bottom_up ctx env) es in
          UnionFind.make (Pos.same_pos_as (TTuple (ts, s)) e)
      | ETupleAccess (e1, n, s, typs) -> (
          let typs =
            List.map (fun typ -> UnionFind.make (Pos.map_under_mark ast_to_typ typ)) typs
          in
          typecheck_expr_top_down ctx env e1 (UnionFind.make (TTuple (typs, s), Pos.get_position e));
          match List.nth_opt typs n with
          | Some t' -> t'
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf
                   "Expression should have a tuple type with at least %d elements but only has %d" n
                   (List.length typs))
                (Pos.get_position e1))
      | EInj (e1, n, e_name, ts) ->
          let ts = List.map (fun t -> UnionFind.make (Pos.map_under_mark ast_to_typ t)) ts in
          let ts_n =
            match List.nth_opt ts n with
            | Some ts_n -> ts_n
            | None ->
                Errors.raise_spanned_error
                  (Format.asprintf
                     "Expression should have a sum type with at least %d cases but only has %d" n
                     (List.length ts))
                  (Pos.get_position e)
          in
          typecheck_expr_top_down ctx env e1 ts_n;
          UnionFind.make (Pos.same_pos_as (TEnum (ts, e_name)) e)
      | EMatch (e1, es, e_name) ->
          let enum_cases =
            List.map (fun e' -> UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e')) es
          in
          let t_e1 = UnionFind.make (Pos.same_pos_as (TEnum (enum_cases, e_name)) e1) in
          typecheck_expr_top_down ctx env e1 t_e1;
          let t_ret = UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e) in
          List.iteri
            (fun i es' ->
              let enum_t = List.nth enum_cases i in
              let t_es' = UnionFind.make (Pos.same_pos_as (TArrow (enum_t, t_ret)) es') in
              typecheck_expr_top_down ctx env es' t_es')
            es;
          t_ret
      | EAbs (pos_binder, binder, taus) ->
          let xs, body = Bindlib.unmbind binder in
          if Array.length xs = List.length taus then
            let xstaus =
              List.map2
                (fun x tau ->
                  (x, UnionFind.make (ast_to_typ (Pos.unmark tau), Pos.get_position tau)))
                (Array.to_list xs) taus
            in
            let env = List.fold_left (fun env (x, tau) -> A.VarMap.add x tau env) env xstaus in
            List.fold_right
              (fun (_, t_arg) (acc : typ Pos.marked UnionFind.elem) ->
                UnionFind.make (TArrow (t_arg, acc), pos_binder))
              xstaus
              (typecheck_expr_bottom_up ctx env body)
          else
            Errors.raise_spanned_error
              (Format.asprintf "function has %d variables but was supplied %d types"
                 (Array.length xs) (List.length taus))
              pos_binder
      | EApp (e1, args) ->
          let t_args = List.map (typecheck_expr_bottom_up ctx env) args in
          let t_ret = UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e) in
          let t_app =
            List.fold_right
              (fun t_arg acc -> UnionFind.make (Pos.same_pos_as (TArrow (t_arg, acc)) e))
              t_args t_ret
          in
          typecheck_expr_top_down ctx env e1 t_app;
          t_ret
      | EOp op -> op_type (Pos.same_pos_as op e)
      | EDefault (excepts, just, cons) ->
          typecheck_expr_top_down ctx env just (UnionFind.make (Pos.same_pos_as (TLit TBool) just));
          let tcons = typecheck_expr_bottom_up ctx env cons in
          List.iter (fun except -> typecheck_expr_top_down ctx env except tcons) excepts;
          tcons
      | EIfThenElse (cond, et, ef) ->
          typecheck_expr_top_down ctx env cond (UnionFind.make (Pos.same_pos_as (TLit TBool) cond));
          let tt = typecheck_expr_bottom_up ctx env et in
          typecheck_expr_top_down ctx env ef tt;
          tt
      | EAssert e' ->
          typecheck_expr_top_down ctx env e' (UnionFind.make (Pos.same_pos_as (TLit TBool) e'));
          UnionFind.make (Pos.same_pos_as (TLit TUnit) e')
      | EArray es ->
          let cell_type = UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e) in
          List.iter
            (fun e' ->
              let t_e' = typecheck_expr_bottom_up ctx env e' in
              unify ctx cell_type t_e')
            es;
          UnionFind.make (Pos.same_pos_as (TArray cell_type) e)
    in
    (* Cli.debug_print (Format.asprintf "Found type of %a: %a" (Print.format_expr ctx) e (format_typ
       ctx) out); *)
    out
  with Errors.StructuredError (msg, err_pos) when List.length err_pos = 2 ->
    raise
      (Errors.StructuredError
         ( msg,
           (Some "Error coming from typechecking the following expression:", Pos.get_position e)
           :: err_pos ))

(** Checks whether the expression can be typed with the provided type *)
and typecheck_expr_top_down (ctx : Ast.decl_ctx) (env : env) (e : A.expr Pos.marked)
    (tau : typ Pos.marked UnionFind.elem) : unit =
  (* Cli.debug_print (Format.asprintf "Typechecking %a : %a" (Print.format_expr ctx) e (format_typ
     ctx) tau); *)
  try
    match Pos.unmark e with
    | EVar v -> (
        match A.VarMap.find_opt (Pos.unmark v) env with
        | Some tau' -> ignore (unify ctx tau tau')
        | None ->
            Errors.raise_spanned_error "Variable not found in the current context"
              (Pos.get_position e))
    | ELit (LBool _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TBool) e))
    | ELit (LInt _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TInt) e))
    | ELit (LRat _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TRat) e))
    | ELit (LMoney _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TMoney) e))
    | ELit (LDate _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TDate) e))
    | ELit (LDuration _) -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TDuration) e))
    | ELit LUnit -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TUnit) e))
    | ELit LEmptyError -> unify ctx tau (UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e))
    | ETuple (es, s) ->
        let t_es =
          UnionFind.make
            (Pos.same_pos_as (TTuple (List.map (typecheck_expr_bottom_up ctx env) es, s)) e)
        in
        unify ctx tau t_es
    | ETupleAccess (e1, n, s, typs) -> (
        let typs = List.map (fun typ -> UnionFind.make (Pos.map_under_mark ast_to_typ typ)) typs in
        typecheck_expr_top_down ctx env e1 (UnionFind.make (TTuple (typs, s), Pos.get_position e));
        match List.nth_opt typs n with
        | Some t1n -> unify ctx t1n tau
        | None ->
            Errors.raise_spanned_error
              (Format.asprintf
                 "Expression should have a tuple type with at least %d elements but only has %d" n
                 (List.length typs))
              (Pos.get_position e1))
    | EInj (e1, n, e_name, ts) ->
        let ts = List.map (fun t -> UnionFind.make (Pos.map_under_mark ast_to_typ t)) ts in
        let ts_n =
          match List.nth_opt ts n with
          | Some ts_n -> ts_n
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf
                   "Expression should have a sum type with at least %d cases but only has %d" n
                   (List.length ts))
                (Pos.get_position e)
        in
        typecheck_expr_top_down ctx env e1 ts_n;
        unify ctx (UnionFind.make (Pos.same_pos_as (TEnum (ts, e_name)) e)) tau
    | EMatch (e1, es, e_name) ->
        let enum_cases =
          List.map (fun e' -> UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e')) es
        in
        let t_e1 = UnionFind.make (Pos.same_pos_as (TEnum (enum_cases, e_name)) e1) in
        typecheck_expr_top_down ctx env e1 t_e1;
        let t_ret = UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e) in
        List.iteri
          (fun i es' ->
            let enum_t = List.nth enum_cases i in
            let t_es' = UnionFind.make (Pos.same_pos_as (TArrow (enum_t, t_ret)) es') in
            typecheck_expr_top_down ctx env es' t_es')
          es;
        unify ctx tau t_ret
    | EAbs (pos_binder, binder, t_args) ->
        let xs, body = Bindlib.unmbind binder in
        if Array.length xs = List.length t_args then
          let xstaus =
            List.map2
              (fun x t_arg -> (x, UnionFind.make (Pos.map_under_mark ast_to_typ t_arg)))
              (Array.to_list xs) t_args
          in
          let env = List.fold_left (fun env (x, t_arg) -> A.VarMap.add x t_arg env) env xstaus in
          let t_out = typecheck_expr_bottom_up ctx env body in
          let t_func =
            List.fold_right
              (fun (_, t_arg) acc -> UnionFind.make (Pos.same_pos_as (TArrow (t_arg, acc)) e))
              xstaus t_out
          in
          unify ctx t_func tau
        else
          Errors.raise_spanned_error
            (Format.asprintf "function has %d variables but was supplied %d types" (Array.length xs)
               (List.length t_args))
            pos_binder
    | EApp (e1, args) ->
        let t_args = List.map (typecheck_expr_bottom_up ctx env) args in
        let te1 = typecheck_expr_bottom_up ctx env e1 in
        let t_func =
          List.fold_right
            (fun t_arg acc -> UnionFind.make (Pos.same_pos_as (TArrow (t_arg, acc)) e))
            t_args tau
        in
        unify ctx te1 t_func
    | EOp op ->
        let op_typ = op_type (Pos.same_pos_as op e) in
        unify ctx op_typ tau
    | EDefault (excepts, just, cons) ->
        typecheck_expr_top_down ctx env just (UnionFind.make (Pos.same_pos_as (TLit TBool) just));
        typecheck_expr_top_down ctx env cons tau;
        List.iter (fun except -> typecheck_expr_top_down ctx env except tau) excepts
    | EIfThenElse (cond, et, ef) ->
        typecheck_expr_top_down ctx env cond (UnionFind.make (Pos.same_pos_as (TLit TBool) cond));
        typecheck_expr_top_down ctx env et tau;
        typecheck_expr_top_down ctx env ef tau
    | EAssert e' ->
        typecheck_expr_top_down ctx env e' (UnionFind.make (Pos.same_pos_as (TLit TBool) e'));
        unify ctx tau (UnionFind.make (Pos.same_pos_as (TLit TUnit) e'))
    | EArray es ->
        let cell_type = UnionFind.make (Pos.same_pos_as (TAny (Any.fresh ())) e) in
        List.iter
          (fun e' ->
            let t_e' = typecheck_expr_bottom_up ctx env e' in
            unify ctx cell_type t_e')
          es;
        unify ctx tau (UnionFind.make (Pos.same_pos_as (TArray cell_type) e))
  with Errors.StructuredError (msg, err_pos) when List.length err_pos = 2 ->
    raise
      (Errors.StructuredError
         ( msg,
           (Some "Error coming from typechecking the following expression:", Pos.get_position e)
           :: err_pos ))

(** {1 API} *)

(* Infer the type of an expression *)
let infer_type (ctx : Ast.decl_ctx) (e : A.expr Pos.marked) : A.typ Pos.marked =
  let ty = typecheck_expr_bottom_up ctx A.VarMap.empty e in
  typ_to_ast ty

(** Typechecks an expression given an expected type *)
let check_type (ctx : Ast.decl_ctx) (e : A.expr Pos.marked) (tau : A.typ Pos.marked) =
  typecheck_expr_top_down ctx A.VarMap.empty e (UnionFind.make (Pos.map_under_mark ast_to_typ tau))
