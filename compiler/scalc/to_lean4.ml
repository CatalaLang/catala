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

open Catala_utils
open Shared_ast
open Ast

(* Context for pretty-printing *)
type ctx = { decl_ctx : decl_ctx }

(* Lean4 keywords to avoid *)
let lean4_keywords =
  [
    "def";
    "theorem";
    "axiom";
    "inductive";
    "structure";
    "class";
    "instance";
    "where";
    "let";
    "in";
    "fun";
    "match";
    "with";
    "do";
    "if";
    "then";
    "else";
    "for";
    "return";
    "try";
    "catch";
    "finally";
    "import";
    "open";
    "namespace";
    "section";
    "variable";
    "universe";
    "mutual";
    "private";
    "protected";
    "partial";
    "unsafe";
    "noncomputable";
    "opaque";
    "abbrev";
    "deriving";
  ]

(* Renaming configuration for Lean4 *)
let renaming =
  Renaming.program ()
    ~reserved:lean4_keywords
    ~skip_constant_binders:false
    ~constant_binder_name:None
    ~namespaced_fields:true
    ~namespaced_constrs:true
    ~prefix_module:false
    ~modnames_conflict:false
    ~f_var:String.to_snake_case
    ~f_struct:String.to_camel_case
    ~f_enum:String.to_camel_case

(* Helper to check if type needs parentheses *)
let typ_needs_parens (t : typ) : bool =
  match Mark.remove t with
  | TArrow _ | TArray _ -> true
  | _ -> false

(* Format Catala types to Lean4 types *)
let rec format_typ (ctx : ctx) (fmt : Format.formatter) (typ : typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then
      Format.fprintf fmt "(%a)" format_typ t
    else
      Format.fprintf fmt "%a" format_typ t
  in
  match Mark.remove typ with
  | TLit TUnit -> Format.pp_print_string fmt "Unit"
  | TLit TBool -> Format.pp_print_string fmt "Bool"
  | TLit TInt -> Format.pp_print_string fmt "Int"
  | TLit TRat -> Format.pp_print_string fmt "Rat" (* Rational numbers *)
  | TLit TMoney -> Format.pp_print_string fmt "Money"
  | TLit TDate -> Format.pp_print_string fmt "Date"
  | TLit TDuration -> Format.pp_print_string fmt "Duration"
  | TLit TPos -> Format.pp_print_string fmt "SourcePosition"
  | TTuple ts ->
    (* Lean4 uses × for product types *)
    Format.fprintf fmt "@[<hov>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " × ")
         format_typ_with_parens)
      ts
  | TStruct s -> StructName.format fmt s
  | TEnum e -> EnumName.format fmt e
  | TOption some_typ ->
    (* Lean4's Option type *)
    Format.fprintf fmt "Option %a" format_typ_with_parens some_typ
  | TArrow (t1, t2) ->
    (* Function types *)
    Format.fprintf fmt "%a → %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " → ")
         format_typ_with_parens)
      t1 format_typ_with_parens t2
  | TArray t1 ->
    (* Lists in Lean4 *)
    Format.fprintf fmt "List %a" format_typ_with_parens t1
  | TDefault t -> format_typ fmt t
  | TVar _ -> Format.pp_print_string fmt "α" (* Type variable *)
  | TForAll tb ->
    let _v, t = Bindlib.unmbind tb in
    format_typ fmt t
  | TClosureEnv -> Format.pp_print_string fmt "ClosureEnv" (* Placeholder *)

(* Format struct declaration *)
let format_struct_decl (ctx : ctx) (fmt : Format.formatter)
    (struct_name, struct_fields) : unit =
  let fields = StructField.Map.bindings struct_fields in
  if StructField.Map.is_empty struct_fields then
    (* Empty struct - use a single unit field *)
    Format.fprintf fmt "@[<v 2>structure %a where@,  dummy : Unit@]"
      StructName.format struct_name
  else
    Format.fprintf fmt "@[<v 2>structure %a where@,%a@]"
      StructName.format struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (field_name, field_type) ->
           Format.fprintf fmt "  %a : %a"
             StructField.format field_name
             (format_typ ctx) field_type))
      fields

(* Format enum declaration *)
let format_enum_decl (ctx : ctx) (fmt : Format.formatter)
    (enum_name, enum_cons) : unit =
  if EnumConstructor.Map.is_empty enum_cons then
    failwith "Cannot translate empty enum"
  else
    Format.fprintf fmt "@[<v 2>inductive %a where@,%a@]"
      EnumName.format enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (cons_name, cons_type) ->
           Format.fprintf fmt "  | %a : %a → %a"
             EnumConstructor.format cons_name
             (format_typ ctx) cons_type
             EnumName.format enum_name))
      (EnumConstructor.Map.bindings enum_cons)

(* Format type declarations (structs and enums) *)
let format_ctx (type_ordering : TypeIdent.t list) (fmt : Format.formatter)
    (ctx : ctx) : unit =
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | TypeIdent.Enum _ -> false
        | TypeIdent.Struct s' -> StructName.equal s s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> TypeIdent.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.decl_ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | TypeIdent.Struct s ->
        if StructName.path s = [] then
          Format.fprintf fmt "%a@\n@\n"
            (format_struct_decl ctx)
            (s, StructName.Map.find s ctx.decl_ctx.ctx_structs)
      | TypeIdent.Enum e ->
        if EnumName.path e = [] && not (EnumName.equal e Expr.option_enum) then
          Format.fprintf fmt "%a@\n@\n"
            (format_enum_decl ctx)
            (e, EnumName.Map.find e ctx.decl_ctx.ctx_enums))
    (type_ordering @ scope_structs)

(* Format a scope as a stub *)
let format_scope_stub (fmt : Format.formatter) (name : string) : unit =
  Format.fprintf fmt "-- Scope %s (not yet implemented)@\ndef scope_%s : Unit := ()@\n@\n" name name

(* Format code items - currently just stubs *)
let format_code_item (fmt : Format.formatter) = function
  | SVar { var; _ } ->
    Format.fprintf fmt "-- Variable %a (not yet implemented)@\n" VarName.format var
  | SFunc { var; _ } ->
    Format.fprintf fmt "-- Function %a (not yet implemented)@\n" FuncName.format var
  | SScope { scope_body_name; _ } ->
    format_scope_stub fmt (Format.asprintf "%a" ScopeName.format scope_body_name)

(* Format test scopes *)
let format_tests (fmt : Format.formatter) (p : Ast.program) : unit =
  let _, tests = p.tests in
  if tests <> [] then begin
    Format.fprintf fmt "-- Test scopes@\n";
    List.iter (fun (scope_name, _block) ->
      Format.fprintf fmt "-- Test for scope %a@\n" ScopeName.format scope_name;
      Format.fprintf fmt "def test_%a := sorry -- test not implemented@\n@\n"
        ScopeName.format scope_name
    ) tests
  end

(* Main program formatter *)
let format_program
    (output_file : File.t option)
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  (* Suppress unused variable warning *)
  let _ = output_file in
  
  (* Create context *)
  let ctx = { decl_ctx = p.ctx.decl_ctx } in
  
  Format.pp_open_vbox fmt 0;
  
  (* File header *)
  Format.fprintf fmt "-- This file has been generated by the Catala compiler, do not edit!@\n@\n";
  Format.fprintf fmt "-- Lean4 runtime imports@\n";
  Format.fprintf fmt "-- TODO: Add runtime library imports@\n@\n";
  
  (* Type declarations (structs and enums) *)
  Format.fprintf fmt "-- Type declarations@\n";
  format_ctx type_ordering fmt ctx;
  Format.fprintf fmt "@\n";
  
  (* Program definitions *)
  Format.fprintf fmt "-- Program definitions@\n";
  List.iter (format_code_item fmt) p.code_items;
  Format.fprintf fmt "@\n";
  
  (* Test scopes *)
  format_tests fmt p;
  
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ()

