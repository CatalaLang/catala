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
[@@@warning "-32-27-33"]

open Utils
open Ast
open Lcalc.Backends
module D = Dcalc.Ast
module L = Lcalc.Ast

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from
       https://docs.soliditylang.org/en/v0.8.7/cheatsheet.html?highlight=keywords#reserved-keywords *)
    | "after" | "alias" | "apply" | "auto" | "byte" | "case" | "copyof" | "default" | "define"
    | "final" | "implements" | "in" | "inline" | "let" | "macro" | "match" | "mutable" | "null"
    | "of" | "partial" | "promise" | "reference" | "relocatable" | "sealed" | "sizeof" | "static"
    | "supports" | "switch" | "typedef" | "typeof" | "var" ->
        true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : Dcalc.Ast.StructName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_lowercase (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructName.format_t v))))

let format_struct_field_name (fmt : Format.formatter) (v : Dcalc.Ast.StructFieldName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v)))

let format_enum_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_lowercase (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumName.format_t v))))

let format_enum_cons_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumConstructor.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumConstructor.format_t v)))

let typ_needs_parens (e : Dcalc.Ast.typ Pos.marked) : bool =
  match Pos.unmark e with TArray _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Pos.marked) : unit =
  let format_typ = format_typ in
  let format_func_param_typ (fmt : Format.formatter) (t : Dcalc.Ast.typ Pos.marked) =
    match Pos.unmark t with
    | TLit TUnit -> Format.fprintf fmt ""
    | TArrow _ -> Format.fprintf fmt "%a" format_typ t
    | _ -> Format.fprintf fmt "%a calldata" format_typ t
  in
  let format_typ_with_parens (fmt : Format.formatter) (t : Dcalc.Ast.typ Pos.marked) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit TUnit -> Format.fprintf fmt ""
  | TLit TMoney -> Format.fprintf fmt "money"
  | TLit TInt -> Format.fprintf fmt "integer"
  | TLit TRat -> Format.fprintf fmt "decimal"
  | TLit TDate -> Format.fprintf fmt "date"
  | TLit TDuration -> Format.fprintf fmt "duration"
  | TLit TBool -> Format.fprintf fmt "bool"
  | TTuple (ts, None) ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt t -> Format.fprintf fmt "%a" format_typ_with_parens t))
        ts
  | TTuple (_, Some s) -> Format.fprintf fmt "%a" format_struct_name s
  | TEnum (_, e) -> Format.fprintf fmt "%a" format_enum_name e
  | TArrow (t1, t2) ->
      Format.fprintf fmt "function (%a) returns (%a)" format_func_param_typ t1 format_func_param_typ
        t2
  | TArray t1 -> Format.fprintf fmt "%a[]" format_typ_with_parens t1
  | TAny -> Format.fprintf fmt "Any"

let format_ctx (type_ordering : Scopelang.Dependency.TVertex.t list) (fmt : Format.formatter)
    (ctx : D.decl_ctx) : unit =
  let format_op_eq fmt ((typ : Dcalc.Ast.typ Pos.marked), (v : Dcalc.Ast.StructFieldName.t)) =
    match Pos.unmark typ with
    | TArrow (_, _) ->
        Format.fprintf fmt "(a.%a == b.%a)" format_struct_field_name v format_struct_field_name v
    | _ -> Format.fprintf fmt "eq(a.%a, b.%a)" format_struct_field_name v format_struct_field_name v
  in
  let format_struct_decl fmt (struct_name, struct_fields) =
    if List.length struct_fields = 0 then failwith "no fields in the struct"
    else
      Format.fprintf fmt
        "struct %a {@\n\
         %a@\n\
         }@\n\
         @\n\
         function eq(%a calldata a, %a calldata b)@\n\
         \tpure@\n\
         \treturns (bool)@\n\
         {@\n\
         \treturn(@\n\
         %a@\n\
         \t);@\n\
         }"
        format_struct_name struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun _fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "\t%a %a;" format_typ struct_field_type format_struct_field_name
               struct_field))
        struct_fields format_struct_name struct_name format_struct_name struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " && @\n")
           (fun _fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "\t\t%a" format_op_eq (struct_field_type, struct_field)))
        struct_fields
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if List.length enum_cons = 0 then failwith "no constructors in the enum"
    else
      Format.fprintf fmt "enum %a {@\n%a@\n}" format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@\n")
           (fun _fmt (enum_cons, enum_cons_type) ->
             Format.fprintf fmt "\t%a" format_enum_cons_name enum_cons))
        enum_cons
  in
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | Scopelang.Dependency.TVertex.Enum _ -> false
        | Scopelang.Dependency.TVertex.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> Scopelang.Dependency.TVertex.Struct s)
      (Dcalc.Ast.StructMap.bindings
         (Dcalc.Ast.StructMap.filter (fun s _ -> not (is_in_type_ordering s)) ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
          Format.fprintf fmt "%a@\n@\n" format_struct_decl
            (s, Dcalc.Ast.StructMap.find s ctx.Dcalc.Ast.ctx_structs)
      | Scopelang.Dependency.TVertex.Enum e ->
          Format.fprintf fmt "%a@\n@\n" format_enum_decl
            (e, Dcalc.Ast.EnumMap.find e ctx.Dcalc.Ast.ctx_enums))
    (type_ordering @ scope_structs)

let format_program (fmt : Format.formatter) (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Cli.style_flag := false;
  Format.fprintf fmt
    "// This file has been generated by the Catala compiler, do not edit!@\n\
     @\n\
     pragma solidity ^0.8.7;\n\
    \     @\n\
     struct unit { bool __empty; }@\n\
     @\n\
     struct duration { int256 value; }@\n\
     function eq(duration calldata a, duration calldata b) pure returns (bool) { return(a.value == \
     b.value); }@\n\
     function ne(duration calldata a, duration calldata b) pure returns (bool) { return(!eq(a, \
     b)); }@\n\
     @\n\
     struct money { integer value; }@\n\
     function eq(money calldata a, money calldata b) pure returns (bool) { return(eq(a.value, \
     b.value)); }@\n\
     function ne(money calldata a, money calldata b) pure returns (bool) { return(!eq(a, b)); }@\n\
     @\n\
     struct integer { int256 value; }@\n\
     function eq(integer calldata a, integer calldata b) pure returns (bool) { return(a.value == \
     b.value); }@\n\
     function ne(integer calldata a, integer calldata b) pure returns (bool) { return(!eq(a, b)); }@\n\
     @\n\
     struct decimal { int256 value; }@\n\
     function eq(decimal calldata a, decimal calldata b) pure returns (bool) { return(a.value == \
     b.value); }@\n\
     function ne(decimal calldata a, decimal calldata b) pure returns (bool) { return(!eq(a, b)); }@\n\
     @\n\
     struct date { bool __empty; }@\n\
     @\n\
     %a@\n\
     @\n"
    (format_ctx type_ordering) p.decl_ctx
