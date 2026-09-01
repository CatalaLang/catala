(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy
   of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations
   under the License. *)

(** JSON export of the [dcalc] intermediate representation.

    Intended for machine consumption, e.g. differential testing of the Catala
    runtime against external oracles. Every node is a JSON object with a
    "tag" field naming the constructor and further fields for its children. *)

open Catala_utils
open Shared_ast

let lit_to_json : lit -> Yojson.Safe.t =
  function
  | LBool b -> `Bool b
  | LInt i -> `String (Catala_runtime.integer_to_string i)
  | LUnit -> `String "()"
  | LRat r -> `String (Catala_runtime.decimal_to_string ~max_prec_digits:20 r)
  | LMoney m -> `String (Catala_runtime.money_to_string m)
  | LDate d -> `String (Catala_runtime.date_to_string d)
  | LDuration d -> `String (Catala_runtime.duration_to_string d)

let rec expr_to_json : type a m. Bindlib.ctxt -> (a, m) gexpr -> Yojson.Safe.t =
 fun bctx e ->
  match Mark.remove e with
  | ELit l -> `Assoc [("tag", `String "lit"); ("value", lit_to_json l)]
  | EVar v ->
    `Assoc [("tag", `String "var"); ("name", `String (Bindlib.name_of v))]
  | EApp { f; args; _ } ->
    `Assoc
      [ ("tag", `String "app");
        ("f", expr_to_json bctx f);
        ("args", `List (List.map (expr_to_json bctx) args)) ]
  | EAppOp { op; args; _ } ->
    `Assoc
      [ ("tag", `String "op");
        ("op", `String (Print.operator_to_string (Mark.remove op)));
        ("args", `List (List.map (expr_to_json bctx) args)) ]
  | EAbs { binder; _ } ->
    let xs, body, bctx' = Bindlib.unmbind_in bctx binder in
    let params =
      Array.to_seq xs
      |> Seq.map (fun v -> `String (Bindlib.name_of v))
      |> List.of_seq
    in
    `Assoc
      [ ("tag", `String "abs"); ("params", `List params);
        ("body", expr_to_json bctx' body) ]
  | EIfThenElse { cond; etrue; efalse } ->
    `Assoc
      [ ("tag", `String "if");
        ("cond", expr_to_json bctx cond);
        ("then", expr_to_json bctx etrue);
        ("else", expr_to_json bctx efalse) ]
  | EStruct { name; fields } ->
    `Assoc
      [ ("tag", `String "struct");
        ("name", `String (StructName.to_string name));
        ("fields",
         `Assoc
           (List.map
              (fun (f, ev) -> (StructField.to_string f, expr_to_json bctx ev))
              (StructField.Map.bindings fields))) ]
  | EStructAccess { e; field; _ } ->
    `Assoc
      [ ("tag", `String "struct_access");
        ("e", expr_to_json bctx e);
        ("field", `String (StructField.to_string field)) ]
  | EInj { cons; e; _ } ->
    `Assoc
      [ ("tag", `String "inj");
        ("cons", `String (EnumConstructor.to_string cons));
        ("e", expr_to_json bctx e) ]
  | EMatch { e; cases; _ } ->
    `Assoc
      [ ("tag", `String "match");
        ("e", expr_to_json bctx e);
        ("cases",
         `Assoc
           (List.map
              (fun (c, ce) -> (EnumConstructor.to_string c, expr_to_json bctx ce))
              (EnumConstructor.Map.bindings cases))) ]
  | ETuple es ->
    `Assoc
      [ ("tag", `String "tuple");
        ("items", `List (List.map (expr_to_json bctx) es)) ]
  | ETupleAccess { e; index; _ } ->
    `Assoc
      [ ("tag", `String "tuple_access");
        ("e", expr_to_json bctx e);
        ("index", `Int index) ]
  | EDefault { excepts; just; cons } ->
    `Assoc
      [ ("tag", `String "default");
        ("excepts", `List (List.map (expr_to_json bctx) excepts));
        ("just", expr_to_json bctx just);
        ("cons", expr_to_json bctx cons) ]
  | EPureDefault e1 ->
    `Assoc [("tag", `String "pure_default"); ("e", expr_to_json bctx e1)]
  | EEmpty -> `Assoc [("tag", `String "empty")]
  | EErrorOnEmpty e1 ->
    `Assoc [("tag", `String "error_on_empty"); ("e", expr_to_json bctx e1)]
  | EFatalError err ->
    `Assoc
      [ ("tag", `String "fatal_error");
        ("error", `String (Catala_runtime.error_to_string err)) ]
  | EPos p ->
    `Assoc [("tag", `String "pos"); ("pos", `String (Pos.to_string p))]
  | EExternal _ | EScopeCall _ | EDStructAmend _ | EDStructAccess _
  | ELocation _ | EAssert _ | EArray _ | ECustom _ | EFatalError_pos _
  | EBad ->
    (* Not expected in dcalc; kept for exhaustiveness *)
    `Assoc
      [ ("tag", `String "unsupported");
        ("detail",
         `String (Format.asprintf "%a" (Print.expr ~debug:true ()) e)) ]

let scope_let_kind_to_json = function
  | DestructuringInputStruct -> `String "get"
  | ScopeVarDefinition -> `String "set"
  | CallingSubScope -> `String "call"
  | DestructuringSubScopeResults -> `String "sub_get"
  | Assertion -> `String "assert"

let typ_to_string ty = Format.asprintf "%a" Print.typ ty

let scope_body_to_json body =
  let {
    scope_body_input_struct;
    scope_body_output_struct;
    scope_body_expr = body;
    scope_body_visibility = _vis;
  } =
    body
  in
  let x, body = Bindlib.unbind body in
  ignore x;
  let lets = ref [] in
  let last =
    BoundList.iter
      ~f:(fun x sl ->
        lets :=
          `Assoc
            [ ("kind", scope_let_kind_to_json sl.scope_let_kind);
              ("var", `String (Bindlib.name_of x));
              ("typ", `String (typ_to_string sl.scope_let_typ));
              ("expr", expr_to_json Bindlib.empty_ctxt sl.scope_let_expr) ]
          :: !lets)
      body
  in
  `Assoc
    [ ("input_struct", `String (StructName.to_string scope_body_input_struct));
      ("output_struct", `String (StructName.to_string scope_body_output_struct));
      ("lets", `List (List.rev !lets));
      ("return", expr_to_json Bindlib.empty_ctxt last) ]

let program_to_json prg =
  let scopes_ref = ref [] in
  ignore @@ BoundList.iter
      ~f:(fun _v item ->
        match item with
        | ScopeDef (sn, body) ->
          scopes_ref :=
            `Assoc
              [ ("scope", `String (ScopeName.to_string sn));
                ("body", scope_body_to_json body) ]
            :: !scopes_ref
        | Topdef _ -> ())
      prg.code_items;
  `Assoc [("program", `List (List.rev !scopes_ref))]
