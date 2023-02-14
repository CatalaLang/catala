(* Structural invariant: *)

open Shared_ast
open Ast
open Catala_utils

type invariant_expr = typed expr -> bool option

(* Structural invariant: no default can have as type A -> B *)
let invariant_default_no_arrow e =
  match Marked.unmark e with
  | EDefault _ -> begin
    match Marked.unmark (Expr.ty e) with
    | TArrow _ -> Some false
    | _ -> Some true
  end
  | _ -> None

(* Structural invariant: no partial evaluation *)
let invariant_no_partial_evaluation e =
  match Marked.unmark e with
  | EApp _ -> begin
    match Marked.unmark (Expr.ty e) with
    | TArrow _ -> Some false
    | _ -> Some true
  end
  | _ -> None

(* Structural invariant: no function can return an function *)
let invariant_no_return_a_function e =
  match Marked.unmark e with
  | EAbs _ -> begin
    match Marked.unmark (Expr.ty e) with
    | TArrow (_, (TArrow _, _)) -> Some false
    | _ -> Some true
  end
  | _ -> None

let check_invariant
    ?(name : string option)
    (inv : invariant_expr)
    (p : typed program) =
  let result = ref true in
  let p' =
    Program.map_exprs p ~varf:Fun.id ~f:(fun e ->
        let rec f e =
          let r =
            match inv e with
            | None -> true
            | Some false ->
              Errors.format_spanned_warning (Expr.pos e)
                "Internal Error: Invalid structural invariant %a. The \
                 expression with type  %a. Full term: %a"
                Format.(pp_print_option (fun fmt -> Format.fprintf fmt "(%s)"))
                name (Print.typ p.decl_ctx) (Expr.ty e) (Print.expr p.decl_ctx)
                e;
              false
            | Some true ->
              (* Cli.result_format "Structural invariant verified %a"
                 Format.(pp_print_option (fun fmt -> Format.fprintf fmt "(%s)"))
                 name; *)
              true
          in
          Expr.map_gather e ~acc:r ~join:( && ) ~f
        in

        let res, e' = f e in
        result := res && !result;
        e')
  in
  assert (Bindlib.free_vars p' = Bindlib.empty_ctxt);
  if not !result then
    Errors.raise_internal_error
      "Structural invariant not valid! See above for more informations."
