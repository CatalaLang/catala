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

open Utils
open Ast

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with EAbs _ -> true | _ -> false

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let format_location (fmt : Format.formatter) (l : location) : unit =
  match l with
  | ScopeVar v -> Format.fprintf fmt "%a" ScopeVar.format_t (Pos.unmark v)
  | SubScopeVar (_, subindex, subvar) ->
      Format.fprintf fmt "%a.%a" SubScopeName.format_t (Pos.unmark subindex)
        ScopeVar.format_t (Pos.unmark subvar)

let typ_needs_parens (e : typ Pos.marked) : bool =
  match Pos.unmark e with TArrow _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : typ Pos.marked) : unit =
  let format_typ_with_parens (fmt : Format.formatter) (t : typ Pos.marked) =
    if typ_needs_parens t then
      Format.fprintf fmt "%a%a%a" Dcalc.Print.format_punctuation "(" format_typ
        t Dcalc.Print.format_punctuation ")"
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit l -> Dcalc.Print.format_tlit fmt l
  | TStruct s -> Format.fprintf fmt "%a" Ast.StructName.format_t s
  | TEnum e -> Format.fprintf fmt "%a" Ast.EnumName.format_t e
  | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a %a@ %a@]" format_typ_with_parens t1
        Dcalc.Print.format_operator "→" format_typ t2
  | TArray t1 ->
      Format.fprintf fmt "@[%a@ %a@]" format_typ (Pos.same_pos_as t1 typ)
        Dcalc.Print.format_base_type "array"
  | TAny -> Format.fprintf fmt "any"

let rec format_expr (fmt : Format.formatter) (e : expr Pos.marked) : unit =
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | ELocation l -> Format.fprintf fmt "%a" format_location l
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
  | ELit l ->
      Format.fprintf fmt "%a" Dcalc.Print.format_lit (Pos.same_pos_as l e)
  | EStruct (name, fields) ->
      Format.fprintf fmt " @[<hov 2>%a@ %a@ %a@ %a@]" Ast.StructName.format_t
        name Dcalc.Print.format_punctuation "{"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () ->
             Format.fprintf fmt "%a@ " Dcalc.Print.format_punctuation ";")
           (fun fmt (field_name, field_expr) ->
             Format.fprintf fmt "%a%a%a%a@ %a" Dcalc.Print.format_punctuation
               "\"" Ast.StructFieldName.format_t field_name
               Dcalc.Print.format_punctuation "\""
               Dcalc.Print.format_punctuation "=" format_expr field_expr))
        (Ast.StructFieldMap.bindings fields)
        Dcalc.Print.format_punctuation "}"
  | EStructAccess (e1, field, _) ->
      Format.fprintf fmt "%a%a%a%a%a" format_expr e1
        Dcalc.Print.format_punctuation "." Dcalc.Print.format_punctuation "\""
        Ast.StructFieldName.format_t field Dcalc.Print.format_punctuation "\""
  | EEnumInj (e1, cons, _) ->
      Format.fprintf fmt "%a@ %a" Ast.EnumConstructor.format_t cons format_expr
        e1
  | EMatch (e1, _, cases) ->
      Format.fprintf fmt "@[<hov 0>%a@ @[<hov 2>%a@]@ %a@ %a@]"
        Dcalc.Print.format_keyword "match" format_expr e1
        Dcalc.Print.format_keyword "with"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun fmt (cons_name, case_expr) ->
             Format.fprintf fmt "@[<hov 2>%a %a@ %a@ %a@]"
               Dcalc.Print.format_punctuation "|"
               Dcalc.Print.format_enum_constructor cons_name
               Dcalc.Print.format_punctuation "→" format_expr case_expr))
        (Ast.EnumConstructorMap.bindings cases)
  | EApp ((EAbs ((binder, _), taus), _), args) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      let xs_tau_arg =
        List.map2 (fun (x, tau) arg -> (x, tau, arg)) xs_tau args
      in
      Format.fprintf fmt "@[%a%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           (fun fmt (x, tau, arg) ->
             Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@ %a@\n@]"
               Dcalc.Print.format_keyword "let" format_var x
               Dcalc.Print.format_punctuation ":" format_typ tau
               Dcalc.Print.format_punctuation "=" format_expr arg
               Dcalc.Print.format_keyword "in"))
        xs_tau_arg format_expr body
  | EAbs ((binder, _), taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]"
        Dcalc.Print.format_punctuation "λ"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           (fun fmt (x, tau) ->
             Format.fprintf fmt "@[%a%a%a@ %a%a@]"
               Dcalc.Print.format_punctuation "(" format_var x
               Dcalc.Print.format_punctuation ":" format_typ tau
               Dcalc.Print.format_punctuation ")"))
        xs_tau Dcalc.Print.format_punctuation "→" format_expr body
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[%a@ %a@ %a@]" format_with_parens arg1
        Dcalc.Print.format_binop (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[%a@ %a@]" Dcalc.Print.format_unop (op, Pos.no_pos)
        format_with_parens arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[%a@ %a@]" format_expr f
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@]"
        Dcalc.Print.format_keyword "if" format_expr e1
        Dcalc.Print.format_keyword "then" format_expr e2
        Dcalc.Print.format_keyword "else" format_expr e3
  | EOp (Ternop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_ternop (op, Pos.no_pos)
  | EOp (Binop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_binop (op, Pos.no_pos)
  | EOp (Unop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_unop (op, Pos.no_pos)
  | EDefault (excepts, just, cons) ->
      if List.length excepts = 0 then
        Format.fprintf fmt "@[%a%a %a@ %a%a@]" Dcalc.Print.format_punctuation
          "⟨" format_expr just Dcalc.Print.format_punctuation "⊢" format_expr
          cons Dcalc.Print.format_punctuation "⟩"
      else
        Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a %a@ %a%a@]"
          Dcalc.Print.format_punctuation "⟨"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
             format_expr)
          excepts Dcalc.Print.format_punctuation "|" format_expr just
          Dcalc.Print.format_punctuation "⊢" format_expr cons
          Dcalc.Print.format_punctuation "⟩"
  | ErrorOnEmpty e' ->
      Format.fprintf fmt "error_empty@ %a" format_with_parens e'
  | EArray es ->
      Format.fprintf fmt "%a%a%a" Dcalc.Print.format_punctuation "["
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Dcalc.Print.format_punctuation fmt ";")
           (fun fmt e -> Format.fprintf fmt "@[%a@]" format_expr e))
        es Dcalc.Print.format_punctuation "]"

let format_struct
    (fmt : Format.formatter)
    ((name, fields) : StructName.t * (StructFieldName.t * typ Pos.marked) list)
    : unit =
  Format.fprintf fmt "%a %a %a %a@\n@[<hov 2>  %a@]@\n%a"
    Dcalc.Print.format_keyword "type" StructName.format_t name
    Dcalc.Print.format_punctuation "=" Dcalc.Print.format_punctuation "{"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a%a %a" StructFieldName.format_t field_name
           Dcalc.Print.format_punctuation ":" format_typ typ))
    fields Dcalc.Print.format_punctuation "}"

let format_enum
    (fmt : Format.formatter)
    ((name, cases) : EnumName.t * (EnumConstructor.t * typ Pos.marked) list) :
    unit =
  Format.fprintf fmt "%a %a %a @\n@[<hov 2>  %a@]" Dcalc.Print.format_keyword
    "type" EnumName.format_t name Dcalc.Print.format_punctuation "="
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a %a%a %a" Dcalc.Print.format_punctuation "|"
           EnumConstructor.format_t field_name Dcalc.Print.format_punctuation
           ":" format_typ typ))
    cases

let format_scope
    (fmt : Format.formatter) ((name, decl) : ScopeName.t * scope_decl) : unit =
  Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]"
    Dcalc.Print.format_keyword "let" Dcalc.Print.format_keyword "scope"
    ScopeName.format_t name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
       (fun fmt (scope_var, (typ, vis)) ->
         Format.fprintf fmt "%a%a%a %a%a%a%a%a" Dcalc.Print.format_punctuation
           "(" ScopeVar.format_t scope_var Dcalc.Print.format_punctuation ":"
           format_typ typ Dcalc.Print.format_punctuation "|"
           Dcalc.Print.format_keyword
           (match Pos.unmark vis.io_input with
           | NoInput -> "internal"
           | OnlyInput -> "input"
           | Reentrant -> "context")
           (if Pos.unmark vis.io_output then fun fmt () ->
            Format.fprintf fmt "%a@,%a" Dcalc.Print.format_punctuation "|"
              Dcalc.Print.format_keyword "output"
           else fun fmt () -> Format.fprintf fmt "@<0>")
           () Dcalc.Print.format_punctuation ")"))
    (ScopeVarMap.bindings decl.scope_sig)
    Dcalc.Print.format_punctuation "="
    (Format.pp_print_list
       ~pp_sep:(fun fmt () ->
         Format.fprintf fmt "%a@ " Dcalc.Print.format_punctuation ";")
       (fun fmt rule ->
         match rule with
         | Definition (loc, typ, _, e) ->
             Format.fprintf fmt "@[<hov 2>%a %a %a %a %a@ %a@]"
               Dcalc.Print.format_keyword "let" format_location (Pos.unmark loc)
               Dcalc.Print.format_punctuation ":" format_typ typ
               Dcalc.Print.format_punctuation "="
               (fun fmt e ->
                 match Pos.unmark loc with
                 | SubScopeVar _ -> format_expr fmt e
                 | ScopeVar v -> (
                     match
                       Pos.unmark
                         (snd (ScopeVarMap.find (Pos.unmark v) decl.scope_sig))
                           .io_input
                     with
                     | Reentrant ->
                         Format.fprintf fmt "%a@ %a" Dcalc.Print.format_operator
                           "reentrant or by default" format_expr e
                     | _ -> Format.fprintf fmt "%a" format_expr e))
               e
         | Assertion e ->
             Format.fprintf fmt "%a %a" Dcalc.Print.format_keyword "assert"
               format_expr e
         | Call (scope_name, subscope_name) ->
             Format.fprintf fmt "%a %a%a%a%a" Dcalc.Print.format_keyword "call"
               ScopeName.format_t scope_name Dcalc.Print.format_punctuation "["
               SubScopeName.format_t subscope_name
               Dcalc.Print.format_punctuation "]"))
    decl.scope_decl_rules

let format_program (fmt : Format.formatter) (p : program) : unit =
  Format.fprintf fmt "%a%a%a%a%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       format_struct)
    (StructMap.bindings p.program_structs)
    (fun fmt () ->
      if StructMap.is_empty p.program_structs then Format.fprintf fmt ""
      else Format.fprintf fmt "\n\n")
    ()
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       format_enum)
    (EnumMap.bindings p.program_enums)
    (fun fmt () ->
      if EnumMap.is_empty p.program_enums then Format.fprintf fmt ""
      else Format.fprintf fmt "\n\n")
    ()
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\n")
       format_scope)
    (ScopeMap.bindings p.program_scopes)
