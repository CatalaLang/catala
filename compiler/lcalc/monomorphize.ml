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

type array_instance = {
  name : StructName.t;
  len_field : StructField.t;
  content_field : StructField.t;
  content_typ : naked_typ;
}

type monomorphized_instances = {
  (* The keys are the types inside the [TOption] (before monomorphization). *)
  options : option_instance Type.Map.t;
  (* The keys are the [TTuple] types themselves (before monomorphization). *)
  tuples : tuple_instance Type.Map.t;
  (* The keys are the types inside the [TArray] (before monomorphization). *)
  arrays : array_instance Type.Map.t;
}

let collect_monomorphized_instances (prg : typed program) :
    monomorphized_instances =
  let option_instances_counter = ref 0 in
  let tuple_instances_counter = ref 0 in
  let array_instances_counter = ref 0 in
  let rec collect_typ acc typ =
    match Mark.remove typ with
    | TTuple args when List.for_all (fun t -> Mark.remove t <> TAny) args ->
      let new_acc =
        {
          acc with
          tuples =
            Type.Map.update typ
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
    | TArray t ->
      let new_acc =
        {
          acc with
          arrays =
            Type.Map.update t
              (fun monomorphized_name ->
                match monomorphized_name with
                | Some e -> Some e
                | None ->
                  incr array_instances_counter;
                  Some
                    {
                      len_field = StructField.fresh ("length", Pos.no_pos);
                      content_field = StructField.fresh ("content", Pos.no_pos);
                      content_typ = Mark.remove t;
                      name =
                        StructName.fresh []
                          ( "array_" ^ string_of_int !array_instances_counter,
                            Pos.no_pos );
                    })
              acc.arrays;
        }
      in
      collect_typ new_acc t
    | TDefault t -> collect_typ acc t
    | TArrow (args, ret) ->
      List.fold_left collect_typ (collect_typ acc ret) args
    | TOption t when Mark.remove t <> TAny ->
      let new_acc =
        {
          acc with
          options =
            Type.Map.update t
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
    | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> acc
    | TOption _ | TTuple _ ->
      raise
        (Message.CompilerError
           (Message.Content.add_position
              (Message.Content.to_internal_error
                 (Message.Content.of_message (fun fmt ->
                      Format.fprintf fmt
                        "Some types in tuples or option have not been resolved \
                         by the typechecking before monomorphization.")))
              (Mark.get typ)))
  in
  let rec collect_expr e acc =
    Expr.shallow_fold collect_expr e (collect_typ acc (Expr.ty e))
  in
  let acc =
    Scope.fold_left
      ~init:
        { options = Type.Map.empty; tuples = Type.Map.empty; arrays = Type.Map.empty }
      ~f:(fun acc item _ ->
        match item with
        | Topdef (_, typ, e) -> collect_typ (collect_expr e acc) typ
        | ScopeDef (_, body) ->
          let _, body = Bindlib.unbind body.scope_body_expr in
          Scope.fold_left_lets ~init:acc
            ~f:(fun acc { scope_let_typ; scope_let_expr; _ } _ ->
              collect_typ (collect_expr scope_let_expr acc) scope_let_typ)
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
    ( TStruct (Type.Map.find t1 monomorphized_instances.arrays).name,
      Mark.get typ )
  | TDefault t1 ->
    TDefault (monomorphize_typ monomorphized_instances t1), Mark.get typ
  | TArrow (t1s, t2) ->
    ( TArrow
        ( List.map (monomorphize_typ monomorphized_instances) t1s,
          monomorphize_typ monomorphized_instances t2 ),
      Mark.get typ )
  | TTuple _ ->
    ( TStruct (Type.Map.find typ monomorphized_instances.tuples).name,
      Mark.get typ )
  | TOption t1 ->
    ( TEnum (Type.Map.find t1 monomorphized_instances.options).name,
      Mark.get typ )

let is_some c =
  EnumConstructor.equal Expr.some_constr c ||
  (assert (EnumConstructor.equal Expr.none_constr c); false)

(* We output a typed expr but the types in the output are wrong, it should be
   untyped and re-typed later. *)
let rec monomorphize_expr
    (monomorphized_instances : monomorphized_instances)
    (e : typed expr) : typed expr boxed =
  let typ = Expr.ty e in
  match Mark.remove e with
  | ETuple args ->
    let new_args = List.map (monomorphize_expr monomorphized_instances) args in
    let tuple_instance =
      Type.Map.find typ monomorphized_instances.tuples
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
      Type.Map.find
        (Expr.ty e1)
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
      Type.Map.find
        (match Mark.remove (Expr.ty e1) with
        | TOption t -> t
        | _ -> failwith "should not happen")
        monomorphized_instances.options
    in
    let new_cases =
      match new_cases with
      | [(n1, e1); (n2, e2)] -> (
          let is_some c =
            EnumConstructor.equal Expr.some_constr c ||
            (assert (EnumConstructor.equal Expr.none_constr c); false)
          in
          match is_some n1, is_some n2 with
          | true, false ->
            [option_instance.some_cons, e1; option_instance.none_cons, e2]
          | false, true ->
            [option_instance.some_cons, e2; option_instance.none_cons, e1]
          | _ -> failwith "should not happen")
      | _ -> failwith "should not happen"
    in
    let new_cases = EnumConstructor.Map.of_list new_cases in
    Expr.ematch ~name:option_instance.name ~e:new_e1 ~cases:new_cases
      (Mark.get e)
  | EInj { name; e = e1; cons } when EnumName.equal name Expr.option_enum ->
    let option_instance =
      Type.Map.find
        (match Mark.remove (Expr.ty e) with
        | TOption t -> t
        | _ -> failwith "should not happen")
        monomorphized_instances.options
    in
    let new_e1 = monomorphize_expr monomorphized_instances e1 in
    let new_cons =
      if is_some cons
      then option_instance.some_cons
      else option_instance.none_cons
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
  | EArray args ->
    let new_args = List.map (monomorphize_expr monomorphized_instances) args in
    let array_instance =
      Type.Map.find
        (match Mark.remove (Expr.ty e) with
        | TArray t -> t
        | _ -> failwith "should not happen")
        monomorphized_instances.arrays
    in
    Expr.estruct ~name:array_instance.name
      ~fields:
        (StructField.Map.add array_instance.content_field
           (Expr.earray new_args (Mark.get e))
           (StructField.Map.singleton array_instance.len_field
              (Expr.elit
                 (LInt (Runtime.integer_of_int (List.length args)))
                 (Mark.get e))))
      (Mark.get e)
  | _ -> Expr.map ~f:(monomorphize_expr monomorphized_instances) e

let program (prg : typed program) :
    untyped program * Scopelang.Dependency.TVertex.t list =
  let monomorphized_instances = collect_monomorphized_instances prg in
  (* First we remove the polymorphic option type *)
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
  (* Then we augment the [decl_ctx] with the monomorphized instances *)
  let prg =
    {
      prg with
      decl_ctx =
        {
          prg.decl_ctx with
          ctx_enums =
            Type.Map.fold
              (fun _ (option_instance : option_instance) (ctx_enums : enum_ctx) ->
                EnumName.Map.add option_instance.name
                  (EnumConstructor.Map.add option_instance.none_cons
                     (TLit TUnit, Pos.no_pos)
                     (EnumConstructor.Map.singleton option_instance.some_cons
                        (monomorphize_typ monomorphized_instances
                           (option_instance.some_typ, Pos.no_pos))))
                  ctx_enums)
              monomorphized_instances.options prg.decl_ctx.ctx_enums;
          ctx_structs =
            Type.Map.fold
              (fun _ (tuple_instance : tuple_instance)
                   (ctx_structs : struct_ctx) ->
                StructName.Map.add tuple_instance.name
                  (List.fold_left
                     (fun acc (field, typ) ->
                       StructField.Map.add field
                         (monomorphize_typ monomorphized_instances
                            (typ, Pos.no_pos))
                         acc)
                     StructField.Map.empty tuple_instance.fields)
                  ctx_structs)
              monomorphized_instances.tuples
              (Type.Map.fold
                 (fun _ (array_instance : array_instance)
                      (ctx_structs : struct_ctx) ->
                   StructName.Map.add array_instance.name
                     (StructField.Map.add array_instance.content_field
                        ( TArray
                            (monomorphize_typ monomorphized_instances
                               (array_instance.content_typ, Pos.no_pos)),
                          Pos.no_pos )
                        (StructField.Map.singleton array_instance.len_field
                           (TLit TInt, Pos.no_pos)))
                     ctx_structs)
                 monomorphized_instances.arrays prg.decl_ctx.ctx_structs);
        };
    }
  in
  let code_items =
    Bindlib.unbox @@
    Scope.map
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
      ~varf:Fun.id prg.code_items
  in
  let prg = Program.untype { prg with code_items } in
  ( prg,
    Scopelang.Dependency.check_type_cycles prg.decl_ctx.ctx_structs
      prg.decl_ctx.ctx_enums )
