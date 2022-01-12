(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Dcalc
open Ast
open Z3

module StringMap : Map.S with type key = String.t = Map.Make (String)

type context = {
  ctx_z3 : Z3.context;
  (* The Z3 context, used to create symbols and expressions *)
  ctx_decl : decl_ctx;
  (* The declaration context from the Catala program, containing information to precisely pretty
     print Catala expressions *)
  ctx_var : typ Pos.marked VarMap.t;
  (* A map from Catala variables to their types, needed to create Z3 expressions of the right
     sort *)
  ctx_funcdecl : FuncDecl.func_decl VarMap.t;
  (* A map from Catala function names (represented as variables) to Z3 function declarations, used
     to only define once functions in Z3 queries *)
  ctx_z3vars : Var.t StringMap.t;
      (* A map from strings, corresponding to Z3 symbol names, to the Catala variable they
         represent. Used when to pretty-print Z3 models when a counterexample is generated *)
}
(** The context contains all the required information to encode a VC represented as a Catala term to
    Z3. The fields [ctx_decl] and [ctx_var] are computed before starting the translation to Z3, and
    are thus unmodified throughout the translation. The [ctx_z3] context is an OCaml abstraction on
    top of an underlying C++ imperative implementation, it is therefore only created once.
    Unfortunately, the maps [ctx_funcdecl] and [ctx_z3vars] are computed dynamically during the
    translation requiring us to pass the context around in a functional way **)

(** [add_funcdecl] adds the mapping between the Catala variable [v] and the Z3 function declaration
    [fd] to the context **)
let add_funcdecl (v : Var.t) (fd : FuncDecl.func_decl) (ctx : context) : context =
  { ctx with ctx_funcdecl = VarMap.add v fd ctx.ctx_funcdecl }

(** [add_z3var] adds the mapping between [name] and the Catala variable [v] to the context **)
let add_z3var (name : string) (v : Var.t) (ctx : context) : context =
  { ctx with ctx_z3vars = StringMap.add name v ctx.ctx_z3vars }

(** [unique_name] returns the full, unique name corresponding to variable [v], as given by Bindlib **)
let unique_name (v : Var.t) : string =
  Format.asprintf "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

(** [print_model] pretty prints a Z3 model, used to exhibit counter examples where verification
    conditions are not satisfied **)
let print_model (model : Model.model) : string =
  let decls = Model.get_decls model in
  List.fold_left
    (fun acc d ->
      match Model.get_const_interp model d with
      (* TODO: Better handling of this case *)
      | None -> failwith "[Z3 model]: A variable does not have an associated Z3 solution"
      (* Prints "name : value\n" *)
      | Some e ->
          if FuncDecl.get_arity d = 0 then
            (* Constant case *)
            (* TODO: Needs a better parsing of the value once we have more than integer types *)
            "  " ^ Symbol.to_string (FuncDecl.get_name d) ^ " : " ^ Expr.to_string e ^ "\n" ^ acc
          else failwith "[Z3 model]: Printing of functions is not yet supported")
    "" decls

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Sort.sort =
  match t with
  | TBool -> Boolean.mk_sort ctx.ctx_z3
  | TUnit -> failwith "[Z3 encoding] TUnit type not supported"
  | TInt -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> failwith "[Z3 encoding] TRat type not supported"
  | TMoney -> failwith "[Z3 encoding] TMoney type not supported"
  | TDate -> failwith "[Z3 encoding] TDate type not supported"
  | TDuration -> failwith "[Z3 encoding] TDuration type not supported"

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let translate_typ (ctx : context) (t : typ) : Sort.sort =
  match t with
  | TLit t -> translate_typ_lit ctx t
  | TTuple _ -> failwith "[Z3 encoding] TTuple type not supported"
  | TEnum _ -> failwith "[Z3 encoding] TEnum type not supported"
  | TArrow _ -> failwith "[Z3 encoding] TArrow type not supported"
  | TArray _ -> failwith "[Z3 encoding] TArray type not supported"
  | TAny -> failwith "[Z3 encoding] TAny type not supported"

(** [translate_lit] returns the Z3 expression as a literal corresponding to [lit] **)
let translate_lit (ctx : context) (l : lit) : Expr.expr =
  match l with
  | LBool b -> if b then Boolean.mk_true ctx.ctx_z3 else Boolean.mk_false ctx.ctx_z3
  | LEmptyError -> failwith "[Z3 encoding] LEmptyError literals not supported"
  | LInt n -> Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (Runtime.integer_to_int n)
  | LRat _ -> failwith "[Z3 encoding] LRat literals not supported"
  | LMoney _ -> failwith "[Z3 encoding] LMoney literals not supported"
  | LUnit -> failwith "[Z3 encoding] LUnit literals not supported"
  | LDate _ -> failwith "[Z3 encoding] LDate literals not supported"
  | LDuration _ -> failwith "[Z3 encoding] LDuration literals not supported"

(** [find_or_create_funcdecl] attempts to retrieve the Z3 function declaration corresponding to the
    variable [v]. If no such function declaration exists yet, we construct it and add it to the
    context, thus requiring to return a new context *)
let find_or_create_funcdecl (ctx : context) (v : Var.t) : context * FuncDecl.func_decl =
  match VarMap.find_opt v ctx.ctx_funcdecl with
  | Some fd -> (ctx, fd)
  | None -> (
      (* Retrieves the Catala type of the function [v] *)
      let f_ty = VarMap.find v ctx.ctx_var in
      match Pos.unmark f_ty with
      | TArrow (t1, t2) ->
          let z3_t1 = translate_typ ctx (Pos.unmark t1) in
          let z3_t2 = translate_typ ctx (Pos.unmark t2) in
          let name = unique_name v in
          let fd = FuncDecl.mk_func_decl_s ctx.ctx_z3 name [ z3_t1 ] z3_t2 in
          let ctx = add_funcdecl v fd ctx in
          let ctx = add_z3var name v ctx in
          (ctx, fd)
      | _ ->
          failwith
            "[Z3 Encoding] Ill-formed VC, a function application does not have a function type")

(** [translate_op] returns the Z3 expression corresponding to the application of [op] to the
    arguments [args] **)
let rec translate_op (ctx : context) (op : operator) (args : expr Pos.marked list) :
    context * Expr.expr =
  match op with
  | Ternop _top ->
      let _e1, _e2, _e3 =
        match args with
        | [ e1; e2; e3 ] -> (e1, e2, e3)
        | _ ->
            failwith
              (Format.asprintf "[Z3 encoding] Ill-formed ternary operator application: %a"
                 (Print.format_expr ctx.ctx_decl)
                 (EApp ((EOp op, Pos.no_pos), args), Pos.no_pos))
      in

      failwith "[Z3 encoding] ternary operator application not supported"
  | Binop bop -> (
      let ctx, e1, e2 =
        match args with
        | [ e1; e2 ] ->
            let ctx, e1 = translate_expr ctx e1 in
            let ctx, e2 = translate_expr ctx e2 in
            (ctx, e1, e2)
        | _ ->
            failwith
              (Format.asprintf "[Z3 encoding] Ill-formed binary operator application: %a"
                 (Print.format_expr ctx.ctx_decl)
                 (EApp ((EOp op, Pos.no_pos), args), Pos.no_pos))
      in

      match bop with
      | And -> (ctx, Boolean.mk_and ctx.ctx_z3 [ e1; e2 ])
      | Or -> (ctx, Boolean.mk_or ctx.ctx_z3 [ e1; e2 ])
      | Xor -> (ctx, Boolean.mk_xor ctx.ctx_z3 e1 e2)
      | Add KInt -> (ctx, Arithmetic.mk_add ctx.ctx_z3 [ e1; e2 ])
      | Add _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Add not supported"
      | Sub KInt -> (ctx, Arithmetic.mk_sub ctx.ctx_z3 [ e1; e2 ])
      | Sub _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Sub not supported"
      | Mult KInt -> (ctx, Arithmetic.mk_mul ctx.ctx_z3 [ e1; e2 ])
      | Mult _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Mult not supported"
      | Div KInt -> (ctx, Arithmetic.mk_div ctx.ctx_z3 e1 e2)
      | Div _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Div not supported"
      | Lt KInt -> (ctx, Arithmetic.mk_lt ctx.ctx_z3 e1 e2)
      | Lt _ -> failwith "[Z3 encoding] application of non-integer binary operator Lt not supported"
      | Lte KInt -> (ctx, Arithmetic.mk_le ctx.ctx_z3 e1 e2)
      | Lte _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Lte not supported"
      | Gt KInt -> (ctx, Arithmetic.mk_gt ctx.ctx_z3 e1 e2)
      | Gt _ -> failwith "[Z3 encoding] application of non-integer binary operator Gt not supported"
      | Gte KInt -> (ctx, Arithmetic.mk_ge ctx.ctx_z3 e1 e2)
      | Gte _ ->
          failwith "[Z3 encoding] application of non-integer binary operator Gte not supported"
      | Eq -> (ctx, Boolean.mk_eq ctx.ctx_z3 e1 e2)
      | Neq -> (ctx, Boolean.mk_not ctx.ctx_z3 (Boolean.mk_eq ctx.ctx_z3 e1 e2))
      | Map -> failwith "[Z3 encoding] application of binary operator Map not supported"
      | Concat -> failwith "[Z3 encoding] application of binary operator Concat not supported"
      | Filter -> failwith "[Z3 encoding] application of binary operator Filter not supported")
  | Unop uop -> (
      let ctx, e1 =
        match args with
        | [ e1 ] -> translate_expr ctx e1
        (* TODO: Print term for error message *)
        | _ -> failwith "[Z3 encoding] Ill-formed unary operator application"
      in

      match uop with
      | Not -> (ctx, Boolean.mk_not ctx.ctx_z3 e1)
      | Minus _ -> failwith "[Z3 encoding] application of unary operator Minus not supported"
      (* Omitting the log from the VC *)
      | Log _ -> (ctx, e1)
      | Length -> failwith "[Z3 encoding] application of unary operator Length not supported"
      | IntToRat -> failwith "[Z3 encoding] application of unary operator IntToRat not supported"
      | GetDay -> failwith "[Z3 encoding] application of unary operator GetDay not supported"
      | GetMonth -> failwith "[Z3 encoding] application of unary operator GetMonth not supported"
      | GetYear -> failwith "[Z3 encoding] application of unary operator GetYear not supported")

(** [translate_expr] translate the expression [vc] to its corresponding Z3 expression **)
and translate_expr (ctx : context) (vc : expr Pos.marked) : context * Expr.expr =
  match Pos.unmark vc with
  | EVar v ->
      let v = Pos.unmark v in
      let t = VarMap.find v ctx.ctx_var in
      let name = unique_name v in
      let ctx = add_z3var name v ctx in
      (ctx, Expr.mk_const_s ctx.ctx_z3 name (translate_typ ctx (Pos.unmark t)))
  | ETuple _ -> failwith "[Z3 encoding] ETuple unsupported"
  | ETupleAccess _ -> failwith "[Z3 encoding] ETupleAccess unsupported"
  | EInj _ -> failwith "[Z3 encoding] EInj unsupported"
  | EMatch _ -> failwith "[Z3 encoding] EMatch unsupported"
  | EArray _ -> failwith "[Z3 encoding] EArray unsupported"
  | ELit l -> (ctx, translate_lit ctx l)
  | EAbs _ -> failwith "[Z3 encoding] EAbs unsupported"
  | EApp (head, args) -> (
      match Pos.unmark head with
      | EOp op -> translate_op ctx op args
      | EVar v ->
          let ctx, fd = find_or_create_funcdecl ctx (Pos.unmark v) in
          (* Fold_right to preserve the order of the arguments: The head argument is appended at the
             head *)
          let ctx, z3_args =
            List.fold_right
              (fun arg (ctx, acc) ->
                let ctx, z3_arg = translate_expr ctx arg in
                (ctx, z3_arg :: acc))
              args (ctx, [])
          in
          (ctx, Expr.mk_app ctx.ctx_z3 fd z3_args)
      | _ ->
          failwith
            "[Z3 encoding] EApp node: Catala function calls should only include operators or \
             function names")
  | EAssert _ -> failwith "[Z3 encoding] EAssert unsupported"
  | EOp _ -> failwith "[Z3 encoding] EOp unsupported"
  | EDefault _ -> failwith "[Z3 encoding] EDefault unsupported"
  | EIfThenElse (e_if, e_then, e_else) ->
      (* Encode this as (e_if ==> e_then) /\ (not e_if ==> e_else) *)
      let ctx, z3_if = translate_expr ctx e_if in
      let ctx, z3_then = translate_expr ctx e_then in
      let ctx, z3_else = translate_expr ctx e_else in
      ( ctx,
        Boolean.mk_and ctx.ctx_z3
          [
            Boolean.mk_implies ctx.ctx_z3 z3_if z3_then;
            Boolean.mk_implies ctx.ctx_z3 (Boolean.mk_not ctx.ctx_z3 z3_if) z3_else;
          ] )
  | ErrorOnEmpty _ -> failwith "[Z3 encoding] ErrorOnEmpty unsupported"

type vc_encoding_result = Success of context * Expr.expr | Fail of string

let print_positive_result (vc : Conditions.verification_condition) : string =
  match vc.Conditions.vc_kind with
  | Conditions.NoEmptyError ->
      Format.asprintf "%s: this variable never returns an empty error"
        (Cli.print_with_style [ ANSITerminal.yellow ] "%s.%s"
           (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
           (Bindlib.name_of (Pos.unmark vc.vc_variable)))
  | Conditions.NoOverlappingExceptions ->
      Format.asprintf "%s: no two exceptions to ever overlap for this variable"
        (Cli.print_with_style [ ANSITerminal.yellow ] "%s.%s"
           (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
           (Bindlib.name_of (Pos.unmark vc.vc_variable)))

let print_negative_result (vc : Conditions.verification_condition) : string =
  match vc.Conditions.vc_kind with
  | Conditions.NoEmptyError ->
      Format.asprintf "%s: this variable might return an empty error\n%s"
        (Cli.print_with_style [ ANSITerminal.yellow ] "%s.%s"
           (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
           (Bindlib.name_of (Pos.unmark vc.vc_variable)))
        (Pos.retrieve_loc_text (Pos.get_position vc.vc_variable))
  | Conditions.NoOverlappingExceptions ->
      Format.asprintf "%s: at least two exceptions overlap for this variable\n%s"
        (Cli.print_with_style [ ANSITerminal.yellow ] "%s.%s"
           (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
           (Bindlib.name_of (Pos.unmark vc.vc_variable)))
        (Pos.retrieve_loc_text (Pos.get_position vc.vc_variable))

(** [encode_and_check_vc] spawns a new Z3 solver and tries to solve the expression [vc] **)
let encode_and_check_vc (decl_ctx : decl_ctx) (z3_ctx : Z3.context)
    (vc : Conditions.verification_condition * vc_encoding_result) : unit =
  let vc, z3_vc = vc in

  Cli.debug_print
    (Format.asprintf "For this variable:\n%s\n"
       (Pos.retrieve_loc_text (Pos.get_position vc.Conditions.vc_guard)));
  Cli.debug_print
    (Format.asprintf "This verification condition was generated for %s:@\n%a"
       (Cli.print_with_style [ ANSITerminal.yellow ] "%s"
          (match vc.vc_kind with
          | Conditions.NoEmptyError -> "the variable definition never to return an empty error"
          | NoOverlappingExceptions -> "no two exceptions to ever overlap"))
       (Dcalc.Print.format_expr decl_ctx)
       vc.vc_guard);

  match z3_vc with
  | Success (_, z3_vc) ->
      Cli.debug_print
        (Format.asprintf "The translation to Z3 is the following:@\n%s" (Expr.to_string z3_vc));

      let solver = Solver.mk_solver z3_ctx None in

      Solver.add solver [ Boolean.mk_not z3_ctx z3_vc ];

      if Solver.check solver [] = UNSATISFIABLE then Cli.result_print (print_positive_result vc)
      else (
        (* TODO: Print model as error message for Catala debugging purposes *)
        Cli.error_print (print_negative_result vc);
        match Solver.get_model solver with
        | None -> Cli.error_print "Z3 did not manage to generate a counterexample"
        | Some model ->
            Cli.error_print
              (Format.asprintf "Z3 generated the following counterexample:\n%s" (print_model model)))
  | Fail msg -> Cli.error_print (Format.asprintf "The translation to Z3 failed:@\n%s" msg)

(** [solve_vc] is the main entry point of this module. It takes a list of expressions [vcs]
    corresponding to verification conditions that must be discharged by Z3, and attempts to solve
    them **)
let solve_vc (prgm : program) (decl_ctx : decl_ctx) (vcs : Conditions.verification_condition list) :
    unit =
  Cli.debug_print (Format.asprintf "Running Z3 version %s" Version.to_string);

  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let z3_ctx = mk_context cfg in

  let z3_vcs =
    List.map
      (fun vc ->
        ( vc,
          try
            let ctx, z3_vc =
              translate_expr
                {
                  ctx_z3 = z3_ctx;
                  ctx_decl = decl_ctx;
                  ctx_var =
                    VarMap.union
                      (fun _ _ _ ->
                        failwith "[Z3 encoding]: A Variable cannot be both free and bound")
                      (variable_types prgm) vc.Conditions.vc_free_vars_typ;
                  ctx_funcdecl = VarMap.empty;
                  ctx_z3vars = StringMap.empty;
                }
                (Bindlib.unbox (Dcalc.Optimizations.remove_all_logs vc.Conditions.vc_guard))
            in
            Success (ctx, z3_vc)
          with Failure msg -> Fail msg ))
      vcs
  in

  List.iter (encode_and_check_vc decl_ctx z3_ctx) z3_vcs
