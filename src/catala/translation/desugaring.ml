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

(* This file intends to desugar parsing/ast.ml into ir/ir.ml *)

exception Desugaring of string

let constructor_desugaring (prgm : Ir.program) (constructor : Ast.constructor) : Ir.constructor=
  match Ir.StructMap.filter (fun name v -> Ir.Struct.raw name = constructor) prgm.structs with
    | ( name_with_id, _ ) :: _ -> Ir.Struct name_with_id
    | [] -> 
      match Ir.EnumMap.filter (fun name v -> Ir.Enum.raw name = constructor) prgm.enums with
        | ( name_with_id, _ ) :: _ -> Ir.Enum name_with_id
        | [] -> raise (Desugaring ("Unbound constructor name : " ^ constructor))

let typ_desugaring (prgm : Ir.program) (typ : Ast.typ) =
  let primitive_desugaring = function
    | Ast.Integer -> Ir.Integer
    | Ast.Decimal -> Ir.Decimal
    | Ast.Boolean -> Ir.Boolean
    | Ast.Money   -> Ir.Money
    | Ast.Text    -> Ir.Text
    | Ast.Date    -> Ir.Date
    | Ast.Named name -> Ir.Named (constructor_desugaring prgm name)
  in

  let rec base_typ_data_desugaring = function
    | Ast.Primitive prim_typ -> Ir.TPrim (primitive_desugaring prim_typ)
    | Ast.Collection base_typ -> Ir.TVec (base_typ_desugaring (Pos.unmark base_typ))
    | Ast.Optional base_typ -> Ir.TOption (base_typ_desugaring (Pos.unmark base_typ))
  in

  let base_typ_desugaring = function
    | Ast.Condition -> Ir.Condition
    | Ast.Data base_typ_data -> Ir.Data (base_typ_data_desugaring base_typ_data)
  in

  match typ with
    | Ast.Base base_typ -> Ir.Base (base_typ_desugaring base_typ)
    | Ast.Func {arg_typ;return_typ} -> Ir.Func
      {
        arg_typ = Pos.map_under_mark base_typ_desugaring arg_typ;
        return_typ = Pos.map_under_mark base_typ_desugaring arg_typ
      }

(*
let struct_decl_desugaring ({structs, enums, scopes, struct_fields, enum_cases} : Ir.program) (struct_decl : Ast.struct_decl) =
  let name_with_pos = struct_decl.struct_decl_name in
  let name = Pos.unmarked name_with_pos in
  if StructMap.exists (fun wid v -> Ir.Struct.raw wid = name) structs then
    (* Raise some error here *)
    {structs; enums; scopes; struct_fields; enum_cases}
  else (
    (* Produce a new name *)
    let struct_name_with_id = Ir.Struct.fresh name in
    (* Process struct fields *)
    let process_struct_field 
      (struct_fields, field_list) 
      {struct_decl_field_name, struct_decl_field_typ} 
    =  
      let field_name = Pos.unmark struct_decl_field_name in
      let field_name_pos = Pos.get_position struct_decl_field_name in
      let field_typ = Pos.unmark struct_decl_field_typ in
      let field_typ_pos = Pos.get_position struct_decl_field_typ in

      if StructFieldMap.exists (fun wid v -> Ir.StructField.raw wid = field_name) struct_fields then (
        (* Raise some error here *)
        (struct_fields, field_list)
      ) else (
        let field_name_with_id = Ir.StructField.fresh field_name in
        let struct_fields = Ir.StructFieldMap.add field_name_with_id struct_name_with_id struct_fields in
        let ir_field_typ
        (struct_fields,
          :: field_list
        )

    in



    let ir_struct_fields = List.fold_left struct_fields (
    ) struct_decl.struct_decl_fields

    let name_with_id = Ir.Struct.fresh name in
    let structs = Ir.StructMap.add name_with_id ir_struct_decl in
    { structs; enums; scopes }
  )
  

let enum_decl_desugaring (prgm : Ir.program) (enum_decl : Ast.enum_decl) =
  let 

let scope_decl_desugaring (prgm : Ir.program) (_field_decl : Ast.field_decl) = prgm

let scope_use_desugaring (prgm : Ir.program) (_field_use : Ast.field_use) = prgm

let code_item_desugaring (prgm : Ir.program) (code_item : Ast.code_item) =
  match code_item with
  | Ast.ScopeUse field_use -> field_use_translation prgm field_use
  | Ast.ScopeDecl field_decl -> field_decl_translation prgm field_decl
  | Ast.StructDecl struct_decl -> struct_decl_translation prgm struct_decl
  | Ast.EnumDecl enum_decl -> enum_decl_translation prgm enum_decl

let code_block_desugaring : Ir.program -> Ast.code_block -> Ir.program =
  List.fold_left (fun prgm item -> code_item_translation prgm (Pos.unmark item))

let program_item_desugaring (prgm : Ir.program) (item : Ast.program_item) =
  match item with
  | Ast.MetadataBlock (block, _) | Ast.CodeBlock (block, _) -> code_block_translation prgm block
  | _ -> prgm

let desugaring (prgm : Ast.program) =
  let empty_prgm : Ir.program =
    { enums = Ir.EnumMap.empty; fields = Ir.FieldMap.empty; structs = Ir.StructMap.empty }
  in
  List.fold_left program_item_translation empty_prgm prgm.program_items
*)
