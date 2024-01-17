(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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

open Shared_ast
open Ast
open Catala_utils

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.no_pos) (y, Pos.no_pos)
  let format fmt x = Print.typ_debug fmt (x, Pos.no_pos)
end)

type option_instance = {
  name : EnumName.t;
  some_cons : EnumConstructor.t;
  some_typ : naked_typ;
  none_cons : EnumConstructor.t;
}

type tuple_instance = {
  name : StructName.t;
  fields : (StructField.t * naked_typ) list;
}

type monomorphized_instances = {
  (* The keys are the types inside the [TOption] (before monomorphization). *)
  options : option_instance TypMap.t;
  (* The keys are the [TTuple] types themselves (before monomorphization). *)
  tuples : tuple_instance TypMap.t;
}

let collect_monomorphized_instances (prg : typed program) :
    monomorphized_instances =
  let option_instances_counter = ref 0 in
  let tuple_instances_counter = ref 0 in
  let rec collect_typ acc typ =
    match Mark.remove typ with
    | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> acc
    | TTuple args ->
      let new_acc =
        {
          acc with
          tuples =
            TypMap.update (Mark.remove typ)
              (fun monomorphized_name ->
                match monomorphized_name with
                | Some e -> Some e
                | None ->
                  incr tuple_instances_counter;
                  Some
                    {
                      fields =
                        List.mapi
                          (fun i arg ->
                            ( StructField.fresh
                                ("elt_" ^ string_of_int i, Pos.no_pos),
                              Mark.remove arg ))
                          args;
                      name =
                        StructName.fresh []
                          ( "tuple_" ^ string_of_int !option_instances_counter,
                            Pos.no_pos );
                    })
              acc.tuples;
        }
      in
      List.fold_left collect_typ new_acc args
    | TArray t | TDefault t -> collect_typ acc t
    | TArrow (args, ret) ->
      List.fold_left collect_typ (collect_typ acc ret) args
    | TOption t ->
      let new_acc =
        {
          acc with
          options =
            TypMap.update (Mark.remove t)
              (fun monomorphized_name ->
                match monomorphized_name with
                | Some e -> Some e
                | None ->
                  incr option_instances_counter;
                  Some
                    {
                      some_cons =
                        EnumConstructor.fresh
                          ( "Some_" ^ string_of_int !option_instances_counter,
                            Pos.no_pos );
                      none_cons =
                        EnumConstructor.fresh
                          ( "None_" ^ string_of_int !option_instances_counter,
                            Pos.no_pos );
                      some_typ = Mark.remove t;
                      name =
                        EnumName.fresh []
                          ( "option_" ^ string_of_int !option_instances_counter,
                            Pos.no_pos );
                    })
              acc.options;
        }
      in
      collect_typ new_acc t
  in
  let rec collect_expr acc e =
    let acc = collect_typ acc (Expr.maybe_ty (Mark.get e)) in
    Expr.shallow_fold (fun e acc -> collect_expr acc e) e acc
  in
  let acc =
    Scope.fold_left
      ~init:{ options = TypMap.empty; tuples = TypMap.empty }
      ~f:(fun acc item _ ->
        match item with
        | Topdef (_, typ, e) -> collect_typ (collect_expr acc e) typ
        | ScopeDef (_, body) ->
          let _, body = Bindlib.unbind body.scope_body_expr in
          Scope.fold_left_lets ~init:acc
            ~f:(fun acc { scope_let_typ; scope_let_expr; _ } _ ->
              collect_typ (collect_expr acc scope_let_expr) scope_let_typ)
            body)
      prg.code_items
  in
  EnumName.Map.fold
    (fun _ constructors acc ->
      EnumConstructor.Map.fold
        (fun _ t acc -> collect_typ acc t)
        constructors acc)
    prg.decl_ctx.ctx_enums
    (StructName.Map.fold
       (fun _ fields acc ->
         StructField.Map.fold (fun _ t acc -> collect_typ acc t) fields acc)
       prg.decl_ctx.ctx_structs acc)

let rec monomorphize_typ
    (monomorphized_instances : monomorphized_instances)
    (typ : typ) : typ =
  match Mark.remove typ with
  | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> typ
  | TArray t1 ->
    TArray (monomorphize_typ monomorphized_instances t1), Mark.get typ
  | TDefault t1 ->
    TDefault (monomorphize_typ monomorphized_instances t1), Mark.get typ
  | TArrow (t1s, t2) ->
    ( TArrow
        ( List.map (monomorphize_typ monomorphized_instances) t1s,
          monomorphize_typ monomorphized_instances t2 ),
      Mark.get typ )
  | TTuple _ ->
    ( TStruct (TypMap.find (Mark.remove typ) monomorphized_instances.tuples).name,
      Mark.get typ )
  | TOption t1 ->
    ( TEnum (TypMap.find (Mark.remove t1) monomorphized_instances.options).name,
      Mark.get typ )

(* We output a typed expr but the types in the output are wrong, it should be
   untyped and re-typed later. *)
let rec monomorphize_expr
    (monomorphized_instances : monomorphized_instances)
    (e : typed expr) : typed expr boxed =
  let typ = Expr.maybe_ty (Mark.get e) in
  match Mark.remove e with
  | ETuple args ->
    let new_args = List.map (monomorphize_expr monomorphized_instances) args in
    let tuple_instance =
      TypMap.find (Mark.remove typ) monomorphized_instances.tuples
    in
    let fields =
      StructField.Map.of_list
      @@ List.map2
           (fun new_arg (tuple_field, _) -> tuple_field, new_arg)
           new_args tuple_instance.fields
    in
    Expr.estruct ~name:tuple_instance.name ~fields (Mark.get e)
  | ETupleAccess { e = e1; index; _ } ->
    let tuple_instance =
      TypMap.find
        (Mark.remove (Expr.maybe_ty (Mark.get e1)))
        monomorphized_instances.tuples
    in
    let new_e1 = monomorphize_expr monomorphized_instances e1 in
    Expr.estructaccess ~name:tuple_instance.name
      ~field:(fst (List.nth tuple_instance.fields index))
      ~e:new_e1 (Mark.get e)
  | EMatch { name; e = e1; cases } when EnumName.equal name Expr.option_enum ->
    let new_e1 = monomorphize_expr monomorphized_instances e1 in
    let new_cases =
      EnumConstructor.Map.bindings
        (EnumConstructor.Map.map
           (monomorphize_expr monomorphized_instances)
           cases)
    in
    let option_instance =
      TypMap.find
        (match Mark.remove (Expr.maybe_ty (Mark.get e1)) with
        | TOption t -> Mark.remove t
        | _ -> failwith "should not happen")
        monomorphized_instances.options
    in
    let new_cases =
      match new_cases with
      | [(n1, e1); (n2, e2)] -> (
        match
          ( Mark.remove (EnumConstructor.get_info n1),
            Mark.remove (EnumConstructor.get_info n2) )
        with
        | "ESome", "ENone" ->
          [option_instance.some_cons, e1; option_instance.none_cons, e2]
        | "ENone", "ESome" ->
          [option_instance.some_cons, e2; option_instance.none_cons, e1]
        | _ -> failwith "should not happen")
      | _ -> failwith "should not happen"
    in
    let new_cases = EnumConstructor.Map.of_list new_cases in
    Expr.ematch ~name:option_instance.name ~e:new_e1 ~cases:new_cases
      (Mark.get e)
  | EInj { name; e = e1; cons } when EnumName.equal name Expr.option_enum ->
    let option_instance =
      TypMap.find
        (Mark.remove (Expr.maybe_ty (Mark.get e1)))
        monomorphized_instances.options
    in
    let new_e1 = monomorphize_expr monomorphized_instances e1 in
    let new_cons =
      match Mark.remove (EnumConstructor.get_info cons) with
      | "ESome" -> option_instance.some_cons
      | "ENone" -> option_instance.none_cons
      | __ -> failwith "should not happen"
    in
    Expr.einj ~name:option_instance.name ~e:new_e1 ~cons:new_cons (Mark.get e)
    (* We do not forget to tweak types stored directly in the AST in the nodes
       of kind [EAbs], [EApp] and [EAppOp]. *)
  | EAbs { binder; tys } ->
    let new_tys = List.map (monomorphize_typ monomorphized_instances) tys in
    let vars, body = Bindlib.unmbind binder in
    let new_body = monomorphize_expr monomorphized_instances body in
    Expr.make_abs vars new_body new_tys (Expr.pos e)
  | EApp { f; args; tys } ->
    let new_f = monomorphize_expr monomorphized_instances f in
    let new_args = List.map (monomorphize_expr monomorphized_instances) args in
    let new_tys = List.map (monomorphize_typ monomorphized_instances) tys in
    Expr.eapp ~f:new_f ~args:new_args ~tys:new_tys (Mark.get e)
  | EAppOp { op; args; tys } ->
    let new_args = List.map (monomorphize_expr monomorphized_instances) args in
    let new_tys = List.map (monomorphize_typ monomorphized_instances) tys in
    Expr.eappop ~op ~args:new_args ~tys:new_tys (Mark.get e)
  | _ -> Expr.map ~f:(monomorphize_expr monomorphized_instances) e

let program (prg : typed program) :
    untyped program * Scopelang.Dependency.TVertex.t list =
  let monomorphized_instances = collect_monomorphized_instances prg in
  (* First we augment the [decl_ctx] with the monomorphized instances *)
  let prg =
    {
      prg with
      decl_ctx =
        {
          prg.decl_ctx with
          ctx_enums =
            TypMap.fold
              (fun _ (option_instance : option_instance) (ctx_enums : enum_ctx) ->
                EnumName.Map.add option_instance.name
                  (EnumConstructor.Map.add option_instance.none_cons
                     (TLit TUnit, Pos.no_pos)
                     (EnumConstructor.Map.singleton option_instance.some_cons
                        (option_instance.some_typ, Pos.no_pos)))
                  ctx_enums)
              monomorphized_instances.options prg.decl_ctx.ctx_enums;
          ctx_structs =
            TypMap.fold
              (fun _ (tuple_instance : tuple_instance)
                   (ctx_structs : struct_ctx) ->
                StructName.Map.add tuple_instance.name
                  (List.fold_left
                     (fun acc (field, typ) ->
                       StructField.Map.add field (typ, Pos.no_pos) acc)
                     StructField.Map.empty tuple_instance.fields)
                  ctx_structs)
              monomorphized_instances.tuples prg.decl_ctx.ctx_structs;
        };
    }
  in
  (* And we remove the polymorphic option type *)
  let prg =
    {
      prg with
      decl_ctx =
        {
          prg.decl_ctx with
          ctx_enums =
            EnumName.Map.remove Expr.option_enum prg.decl_ctx.ctx_enums;
        };
    }
  in
  (* Then we replace all hardcoded types and expressions with the monomorphized
     instances *)
  let prg =
    {
      prg with
      decl_ctx =
        {
          prg.decl_ctx with
          ctx_enums =
            EnumName.Map.map
              (EnumConstructor.Map.map
                 (monomorphize_typ monomorphized_instances))
              prg.decl_ctx.ctx_enums;
          ctx_structs =
            StructName.Map.map
              (StructField.Map.map (monomorphize_typ monomorphized_instances))
              prg.decl_ctx.ctx_structs;
        };
    }
  in
  let prg =
    Bindlib.unbox
    @@ Bindlib.box_apply
         (fun code_items -> { prg with code_items })
         (Scope.map
            ~f:(fun code_item ->
              match code_item with
              | Topdef (name, typ, e) -> Bindlib.box (Topdef (name, typ, e))
              | ScopeDef (name, body) ->
                let s_var, scope_body = Bindlib.unbind body.scope_body_expr in
                Bindlib.box_apply
                  (fun scope_body_expr ->
                    ScopeDef (name, { body with scope_body_expr }))
                  (Bindlib.bind_var s_var
                     (Scope.map_exprs_in_lets ~varf:Fun.id
                        ~transform_types:
                          (monomorphize_typ monomorphized_instances)
                        ~f:(monomorphize_expr monomorphized_instances)
                        scope_body)))
            ~varf:Fun.id prg.code_items)
  in
  let prg = Program.untype prg in
  ( prg,
    Scopelang.Dependency.check_type_cycles prg.decl_ctx.ctx_structs
      prg.decl_ctx.ctx_enums )
