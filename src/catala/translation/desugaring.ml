(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the license *)

exception Desugaring of string

let typ_desugaring (context : Ir.context) (typ : Ast.typ) : Ir.typ =
  let prim_typ_desugaring = function
    | Ast.Integer -> Ir.Integer
    | Ast.Decimal -> Ir.Decimal
    | Ast.Boolean -> Ir.Boolean
    | Ast.Money -> Ir.Money
    | Ast.Text -> Ir.Text
    | Ast.Date -> Ir.Date
    | Ast.Named str -> (
        match Ir.StringMap.find_opt str context.string_to_uid with
        | Some uid -> Ir.Named uid
        | None -> raise (Desugaring ("Unbound constructor " ^ str)) )
  in

  let rec base_typ_data_desugaring = function
    | Ast.Primitive prim_typ -> Ir.TPrim (prim_typ_desugaring prim_typ)
    | Ast.Collection typ_data -> Ir.TVec (base_typ_data_desugaring (Pos.unmark typ_data))
    | Ast.Optional typ_data -> Ir.TOption (base_typ_data_desugaring (Pos.unmark typ_data))
  in

  let base_typ_desugaring = function
    | Ast.Condition -> Ir.Condition
    | Ast.Data typ_data -> Ir.Data (base_typ_data_desugaring typ_data)
  in

  let func_typ_desugaring ({ arg_typ; return_typ } : Ast.func_typ) : Ir.func_typ =
    {
      arg_typ = Pos.map_under_mark base_typ_desugaring arg_typ;
      return_typ = Pos.map_under_mark base_typ_desugaring return_typ;
    }
  in

  match typ with
  | Ast.Base base_typ -> Ir.Base (base_typ_desugaring base_typ)
  | Ast.Func func_typ -> Ir.Func (func_typ_desugaring func_typ)
