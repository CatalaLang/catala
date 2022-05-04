(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
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

open Utils
open Ast

let needs_parens (_e : expr Pos.marked) : bool = false

let format_local_name (fmt : Format.formatter) (v : LocalName.t) : unit =
  Format.fprintf fmt "%a_%s" LocalName.format_t v
    (string_of_int (LocalName.hash v))

let rec format_expr
    (decl_ctx : Dcalc.Ast.decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (e : expr Pos.marked) : unit =
  let format_expr = format_expr decl_ctx ~debug in
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then
      Format.fprintf fmt "%a%a%a" Dcalc.Print.format_punctuation "(" format_expr
        e Dcalc.Print.format_punctuation ")"
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_local_name v
  | EFunc v -> Format.fprintf fmt "%a" TopLevelName.format_t v
  | EStruct (es, s) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" Dcalc.Ast.StructName.format_t s
      Dcalc.Print.format_punctuation "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (e, struct_field) ->
           Format.fprintf fmt "%a%a%a%a %a" Dcalc.Print.format_punctuation "\""
             Dcalc.Ast.StructFieldName.format_t struct_field
             Dcalc.Print.format_punctuation "\"" Dcalc.Print.format_punctuation
             ":" format_expr e))
      (List.combine es
         (List.map fst (Dcalc.Ast.StructMap.find s decl_ctx.ctx_structs)))
      Dcalc.Print.format_punctuation "}"
  | EArray es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" Dcalc.Print.format_punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
      es Dcalc.Print.format_punctuation "]"
  | EStructFieldAccess (e1, field, s) ->
    Format.fprintf fmt "%a%a%a%a%a" format_expr e1
      Dcalc.Print.format_punctuation "." Dcalc.Print.format_punctuation "\""
      Dcalc.Ast.StructFieldName.format_t
      (fst
         (List.find
            (fun (field', _) ->
              Dcalc.Ast.StructFieldName.compare field' field = 0)
            (Dcalc.Ast.StructMap.find s decl_ctx.ctx_structs)))
      Dcalc.Print.format_punctuation "\""
  | EInj (e, case, enum) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Dcalc.Print.format_enum_constructor
      (fst
         (List.find
            (fun (case', _) -> Dcalc.Ast.EnumConstructor.compare case' case = 0)
            (Dcalc.Ast.EnumMap.find enum decl_ctx.ctx_enums)))
      format_expr e
  | ELit l ->
    Format.fprintf fmt "%a" Lcalc.Print.format_lit (Pos.same_pos_as l e)
  | EApp
      ( (EOp (Binop ((Dcalc.Ast.Map | Dcalc.Ast.Filter) as op)), _),
        [ arg1; arg2 ] ) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" Dcalc.Print.format_binop
      (op, Pos.no_pos) format_with_parens arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1
      Dcalc.Print.format_binop (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop (Log _)), _), [ arg1 ]) when not debug ->
    Format.fprintf fmt "%a" format_with_parens arg1
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Dcalc.Print.format_unop
      (op, Pos.no_pos) format_with_parens arg1
  | EApp (f, args) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_expr f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EOp (Ternop op) ->
    Format.fprintf fmt "%a" Dcalc.Print.format_ternop (op, Pos.no_pos)
  | EOp (Binop op) ->
    Format.fprintf fmt "%a" Dcalc.Print.format_binop (op, Pos.no_pos)
  | EOp (Unop op) ->
    Format.fprintf fmt "%a" Dcalc.Print.format_unop (op, Pos.no_pos)

let rec format_statement
    (decl_ctx : Dcalc.Ast.decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (stmt : stmt Pos.marked) : unit =
  if debug then () else ();
  match Pos.unmark stmt with
  | SInnerFuncDef (name, func) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]"
      Dcalc.Print.format_keyword "let" LocalName.format_t (Pos.unmark name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt ((name, _), typ) ->
           Format.fprintf fmt "%a%a %a@ %a%a" Dcalc.Print.format_punctuation "("
             LocalName.format_t name Dcalc.Print.format_punctuation ":"
             (Dcalc.Print.format_typ decl_ctx)
             typ Dcalc.Print.format_punctuation ")"))
      func.func_params Dcalc.Print.format_punctuation "="
      (format_block decl_ctx ~debug)
      func.func_body
  | SLocalDecl (name, typ) ->
    Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]" Dcalc.Print.format_keyword
      "decl" LocalName.format_t (Pos.unmark name) Dcalc.Print.format_punctuation
      ":"
      (Dcalc.Print.format_typ decl_ctx)
      typ
  | SLocalDef (name, expr) ->
    Format.fprintf fmt "@[<hov 2>%a %a@ %a@]" LocalName.format_t
      (Pos.unmark name) Dcalc.Print.format_punctuation "="
      (format_expr decl_ctx ~debug)
      expr
  | STryExcept (b_try, except, b_with) ->
    Format.fprintf fmt "@[<v 2>%a%a@ %a@]@\n@[<v 2>%a %a%a@ %a@]"
      Dcalc.Print.format_keyword "try" Dcalc.Print.format_punctuation ":"
      (format_block decl_ctx ~debug)
      b_try Dcalc.Print.format_keyword "with" Lcalc.Print.format_exception
      except Dcalc.Print.format_punctuation ":"
      (format_block decl_ctx ~debug)
      b_with
  | SRaise except ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Dcalc.Print.format_keyword "raise"
      Lcalc.Print.format_exception except
  | SIfThenElse (e_if, b_true, b_false) ->
    Format.fprintf fmt "@[<v 2>%a @[<hov 2>%a@]%a@ %a@ @]@[<v 2>%a%a@ %a@]"
      Dcalc.Print.format_keyword "if"
      (format_expr decl_ctx ~debug)
      e_if Dcalc.Print.format_punctuation ":"
      (format_block decl_ctx ~debug)
      b_true Dcalc.Print.format_keyword "else" Dcalc.Print.format_punctuation
      ":"
      (format_block decl_ctx ~debug)
      b_false
  | SReturn ret ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Dcalc.Print.format_keyword "return"
      (format_expr decl_ctx ~debug)
      (ret, Pos.get_position stmt)
  | SAssert expr ->
    Format.fprintf fmt "@[<hov 2>%a %a@]" Dcalc.Print.format_keyword "assert"
      (format_expr decl_ctx ~debug)
      (expr, Pos.get_position stmt)
  | SSwitch (e_switch, enum, arms) ->
    Format.fprintf fmt "@[<v 0>%a @[<hov 2>%a@]%a@]%a"
      Dcalc.Print.format_keyword "switch"
      (format_expr decl_ctx ~debug)
      e_switch Dcalc.Print.format_punctuation ":"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt ((case, _), (arm_block, payload_name)) ->
           Format.fprintf fmt "%a %a%a@ %a @[<v 2>%a@ %a@]"
             Dcalc.Print.format_punctuation "|"
             Dcalc.Print.format_enum_constructor case
             Dcalc.Print.format_punctuation ":" LocalName.format_t payload_name
             Dcalc.Print.format_punctuation "→"
             (format_block decl_ctx ~debug)
             arm_block))
      (List.combine (Dcalc.Ast.EnumMap.find enum decl_ctx.ctx_enums) arms)

and format_block
    (decl_ctx : Dcalc.Ast.decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (block : block) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () ->
      Format.fprintf fmt "%a@ " Dcalc.Print.format_punctuation ";")
    (format_statement decl_ctx ~debug)
    fmt block

let format_scope
    (decl_ctx : Dcalc.Ast.decl_ctx)
    ?(debug : bool = false)
    (fmt : Format.formatter)
    (body : scope_body) : unit =
  if debug then () else ();
  Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]"
    Dcalc.Print.format_keyword "let" TopLevelName.format_t body.scope_body_var
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
       (fun fmt ((name, _), typ) ->
         Format.fprintf fmt "%a%a %a@ %a%a" Dcalc.Print.format_punctuation "("
           LocalName.format_t name Dcalc.Print.format_punctuation ":"
           (Dcalc.Print.format_typ decl_ctx)
           typ Dcalc.Print.format_punctuation ")"))
    body.scope_body_func.func_params Dcalc.Print.format_punctuation "="
    (format_block decl_ctx ~debug)
    body.scope_body_func.func_body
