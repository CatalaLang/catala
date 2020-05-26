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

(* This file intends to desugar parsing/ast.ml ir/ir.ml *)

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
    let desugar_struct_fields {struct_decl_field_name, struct_decl_field_typ} =

      if StructFieldMap.exists (fun wid v -> Ir.StructField.raw wid = field_name) struct_fields then (
        (* Raise some error here *)
      ) else 
        Ir.StructFieldMap.add (Ir.StructField.fresh field_name) struct_name_with_id
    in

    let desugar_struct_field field =
       

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
