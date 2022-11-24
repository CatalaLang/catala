(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr> Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Builds a context that allows for mapping each name to a precise uid, taking
    lexical scopes into account *)

open Catala_utils
open Shared_ast

(** {1 Name resolution context} *)

type ident = string

module IdentMap : Map.S with type key = String.t = Map.Make (String)

type unique_rulename =
  | Ambiguous of Pos.t list
  | Unique of RuleName.t Marked.pos

type scope_def_context = {
  default_exception_rulename : unique_rulename option;
  label_idmap : LabelName.t IdentMap.t;
}

type scope_var_or_subscope =
  | ScopeVar of ScopeVar.t
  | SubScope of SubScopeName.t * ScopeName.t

type scope_context = {
  var_idmap : scope_var_or_subscope IdentMap.t;
      (** All variables, including scope variables and subscopes *)
  scope_defs_contexts : scope_def_context Ast.ScopeDefMap.t;
      (** What is the default rule to refer to for unnamed exceptions, if any *)
  sub_scopes : ScopeName.Set.t;
      (** Other scopes referred to by this scope. Used for dependency analysis *)
}
(** Inside a scope, we distinguish between the variables and the subscopes. *)

type struct_context = typ StructField.Map.t
(** Types of the fields of a struct *)

type enum_context = typ EnumConstructor.Map.t
(** Types of the payloads of the cases of an enum *)

type var_sig = {
  var_sig_typ : typ;
  var_sig_is_condition : bool;
  var_sig_io : Surface.Ast.scope_decl_context_io;
  var_sig_states_idmap : StateName.t IdentMap.t;
  var_sig_states_list : StateName.t list;
}

(** Capitalised type names share a namespace on the user side, but may
    correspond to only one of the following *)
type typedef =
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TScope of ScopeName.t * scope_out_struct
      (** Implicitly defined output struct *)

type context = {
  local_var_idmap : Ast.expr Var.t IdentMap.t;
      (** Inside a definition, local variables can be introduced by functions
          arguments or pattern matching *)
  typedefs : typedef IdentMap.t;
      (** Gathers the names of the scopes, structs and enums *)
  field_idmap : StructField.t StructName.Map.t IdentMap.t;
      (** The names of the struct fields. Names of fields can be shared between
          different structs *)
  constructor_idmap : EnumConstructor.t EnumName.Map.t IdentMap.t;
      (** The names of the enum constructors. Constructor names can be shared
          between different enums *)
  scopes : scope_context ScopeName.Map.t;  (** For each scope, its context *)
  structs : struct_context StructName.Map.t;
      (** For each struct, its context *)
  enums : enum_context EnumName.Map.t;  (** For each enum, its context *)
  var_typs : var_sig ScopeVar.Map.t;
      (** The signatures of each scope variable declared *)
}
(** Main context used throughout {!module: Surface.Desugaring} *)

(** {1 Helpers} *)

(** Temporary function raising an error message saying that a feature is not
    supported yet *)
let raise_unsupported_feature (msg : string) (pos : Pos.t) =
  Errors.raise_spanned_error pos "Unsupported feature: %s" msg

(** Function to call whenever an identifier used somewhere has not been declared
    in the program previously *)
let raise_unknown_identifier (msg : string) (ident : ident Marked.pos) =
  Errors.raise_spanned_error (Marked.get_mark ident)
    "\"%s\": unknown identifier %s"
    (Cli.with_style [ANSITerminal.yellow] "%s" (Marked.unmark ident))
    msg

(** Gets the type associated to an uid *)
let get_var_typ (ctxt : context) (uid : ScopeVar.t) : typ =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_typ

let is_var_cond (ctxt : context) (uid : ScopeVar.t) : bool =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_is_condition

let get_var_io (ctxt : context) (uid : ScopeVar.t) :
    Surface.Ast.scope_decl_context_io =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_io

(** Get the variable uid inside the scope given in argument *)
let get_var_uid
    (scope_uid : ScopeName.t)
    (ctxt : context)
    ((x, pos) : ident Marked.pos) : ScopeVar.t =
  let scope = ScopeName.Map.find scope_uid ctxt.scopes in
  match IdentMap.find_opt x scope.var_idmap with
  | Some (ScopeVar uid) -> uid
  | _ ->
    raise_unknown_identifier
      (Format.asprintf "for a variable of scope %a" ScopeName.format_t scope_uid)
      (x, pos)

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid
    (scope_uid : ScopeName.t)
    (ctxt : context)
    ((y, pos) : ident Marked.pos) : SubScopeName.t =
  let scope = ScopeName.Map.find scope_uid ctxt.scopes in
  match IdentMap.find_opt y scope.var_idmap with
  | Some (SubScope (sub_uid, _sub_id)) -> sub_uid
  | _ -> raise_unknown_identifier "for a subscope of this scope" (y, pos)

(** [is_subscope_uid scope_uid ctxt y] returns true if [y] belongs to the
    subscopes of [scope_uid]. *)
let is_subscope_uid (scope_uid : ScopeName.t) (ctxt : context) (y : ident) :
    bool =
  let scope = ScopeName.Map.find scope_uid ctxt.scopes in
  match IdentMap.find_opt y scope.var_idmap with
  | Some (SubScope _) -> true
  | _ -> false

(** Checks if the var_uid belongs to the scope scope_uid *)
let belongs_to (ctxt : context) (uid : ScopeVar.t) (scope_uid : ScopeName.t) :
    bool =
  let scope = ScopeName.Map.find scope_uid ctxt.scopes in
  IdentMap.exists
    (fun _ -> function
      | ScopeVar var_uid -> ScopeVar.equal uid var_uid
      | _ -> false)
    scope.var_idmap

(** Retrieves the type of a scope definition from the context *)
let get_def_typ (ctxt : context) (def : Ast.ScopeDef.t) : typ =
  match def with
  | Ast.ScopeDef.SubScopeVar (_, x, _)
  (* we don't need to look at the subscope prefix because [x] is already the uid
     referring back to the original subscope *)
  | Ast.ScopeDef.Var (x, _) ->
    get_var_typ ctxt x

let is_def_cond (ctxt : context) (def : Ast.ScopeDef.t) : bool =
  match def with
  | Ast.ScopeDef.SubScopeVar (_, x, _)
  (* we don't need to look at the subscope prefix because [x] is already the uid
     referring back to the original subscope *)
  | Ast.ScopeDef.Var (x, _) ->
    is_var_cond ctxt x

let get_enum ctxt id =
  match IdentMap.find (Marked.unmark id) ctxt.typedefs with
  | TEnum id -> id
  | TStruct sid ->
    Errors.raise_multispanned_error
      [
        None, Marked.get_mark id;
        Some "Structure defined at", Marked.get_mark (StructName.get_info sid);
      ]
      "Expecting an enum, but found a structure"
  | TScope (sid, _) ->
    Errors.raise_multispanned_error
      [
        None, Marked.get_mark id;
        Some "Scope defined at", Marked.get_mark (ScopeName.get_info sid);
      ]
      "Expecting an enum, but found a scope"
  | exception Not_found ->
    Errors.raise_spanned_error (Marked.get_mark id) "No enum named %s found"
      (Marked.unmark id)

let get_struct ctxt id =
  match IdentMap.find (Marked.unmark id) ctxt.typedefs with
  | TStruct id | TScope (_, { out_struct_name = id; _ }) -> id
  | TEnum eid ->
    Errors.raise_multispanned_error
      [
        None, Marked.get_mark id;
        Some "Enum defined at", Marked.get_mark (EnumName.get_info eid);
      ]
      "Expecting an struct, but found an enum"
  | exception Not_found ->
    Errors.raise_spanned_error (Marked.get_mark id) "No struct named %s found"
      (Marked.unmark id)

let get_scope ctxt id =
  match IdentMap.find (Marked.unmark id) ctxt.typedefs with
  | TScope (id, _) -> id
  | TEnum eid ->
    Errors.raise_multispanned_error
      [
        None, Marked.get_mark id;
        Some "Enum defined at", Marked.get_mark (EnumName.get_info eid);
      ]
      "Expecting an scope, but found an enum"
  | TStruct sid ->
    Errors.raise_multispanned_error
      [
        None, Marked.get_mark id;
        Some "Structure defined at", Marked.get_mark (StructName.get_info sid);
      ]
      "Expecting an scope, but found a structure"
  | exception Not_found ->
    Errors.raise_spanned_error (Marked.get_mark id) "No scope named %s found"
      (Marked.unmark id)

(** {1 Declarations pass} *)

(** Process a subscope declaration *)
let process_subscope_decl
    (scope : ScopeName.t)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl_context_scope) : context =
  let name, name_pos = decl.scope_decl_context_scope_name in
  let subscope, s_pos = decl.scope_decl_context_scope_sub_scope in
  let scope_ctxt = ScopeName.Map.find scope ctxt.scopes in
  match IdentMap.find_opt subscope scope_ctxt.var_idmap with
  | Some use ->
    let info =
      match use with
      | ScopeVar v -> ScopeVar.get_info v
      | SubScope (ssc, _) -> SubScopeName.get_info ssc
    in
    Errors.raise_multispanned_error
      [Some "first use", Marked.get_mark info; Some "second use", s_pos]
      "Subscope name \"%a\" already used"
      (Cli.format_with_style [ANSITerminal.yellow])
      subscope
  | None ->
    let sub_scope_uid = SubScopeName.fresh (name, name_pos) in
    let original_subscope_uid =
      get_scope ctxt decl.scope_decl_context_scope_sub_scope
    in
    let scope_ctxt =
      {
        scope_ctxt with
        var_idmap =
          IdentMap.add name
            (SubScope (sub_scope_uid, original_subscope_uid))
            scope_ctxt.var_idmap;
        sub_scopes =
          ScopeName.Set.add original_subscope_uid scope_ctxt.sub_scopes;
      }
    in
    { ctxt with scopes = ScopeName.Map.add scope scope_ctxt ctxt.scopes }

let is_type_cond ((typ, _) : Surface.Ast.typ) =
  match typ with
  | Surface.Ast.Base Surface.Ast.Condition
  | Surface.Ast.Func { arg_typ = _; return_typ = Surface.Ast.Condition, _ } ->
    true
  | _ -> false

(** Process a basic type (all types except function types) *)
let rec process_base_typ
    (ctxt : context)
    ((typ, typ_pos) : Surface.Ast.base_typ Marked.pos) : typ =
  match typ with
  | Surface.Ast.Condition -> TLit TBool, typ_pos
  | Surface.Ast.Data (Surface.Ast.Collection t) ->
    ( TArray
        (process_base_typ ctxt
           (Surface.Ast.Data (Marked.unmark t), Marked.get_mark t)),
      typ_pos )
  | Surface.Ast.Data (Surface.Ast.Primitive prim) -> (
    match prim with
    | Surface.Ast.Integer -> TLit TInt, typ_pos
    | Surface.Ast.Decimal -> TLit TRat, typ_pos
    | Surface.Ast.Money -> TLit TMoney, typ_pos
    | Surface.Ast.Duration -> TLit TDuration, typ_pos
    | Surface.Ast.Date -> TLit TDate, typ_pos
    | Surface.Ast.Boolean -> TLit TBool, typ_pos
    | Surface.Ast.Text -> raise_unsupported_feature "text type" typ_pos
    | Surface.Ast.Named ident -> (
      match IdentMap.find_opt ident ctxt.typedefs with
      | Some (TStruct s_uid) -> TStruct s_uid, typ_pos
      | Some (TEnum e_uid) -> TEnum e_uid, typ_pos
      | Some (TScope (_, scope_str)) ->
        TStruct scope_str.out_struct_name, typ_pos
      | None ->
        Errors.raise_spanned_error typ_pos
          "Unknown type \"%a\", not a struct or enum previously declared"
          (Cli.format_with_style [ANSITerminal.yellow])
          ident))

(** Process a type (function or not) *)
let process_type (ctxt : context) ((naked_typ, typ_pos) : Surface.Ast.typ) : typ
    =
  match naked_typ with
  | Surface.Ast.Base base_typ -> process_base_typ ctxt (base_typ, typ_pos)
  | Surface.Ast.Func { arg_typ; return_typ } ->
    ( TArrow (process_base_typ ctxt arg_typ, process_base_typ ctxt return_typ),
      typ_pos )

(** Process data declaration *)
let process_data_decl
    (scope : ScopeName.t)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl_context_data) : context =
  (* First check the type of the context data *)
  let data_typ = process_type ctxt decl.scope_decl_context_item_typ in
  let is_cond = is_type_cond decl.scope_decl_context_item_typ in
  let name, pos = decl.scope_decl_context_item_name in
  let scope_ctxt = ScopeName.Map.find scope ctxt.scopes in
  match IdentMap.find_opt name scope_ctxt.var_idmap with
  | Some use ->
    let info =
      match use with
      | ScopeVar v -> ScopeVar.get_info v
      | SubScope (ssc, _) -> SubScopeName.get_info ssc
    in
    Errors.raise_multispanned_error
      [Some "First use:", Marked.get_mark info; Some "Second use:", pos]
      "Variable name \"%a\" already used"
      (Cli.format_with_style [ANSITerminal.yellow])
      name
  | None ->
    let uid = ScopeVar.fresh (name, pos) in
    let scope_ctxt =
      {
        scope_ctxt with
        var_idmap = IdentMap.add name (ScopeVar uid) scope_ctxt.var_idmap;
      }
    in
    let states_idmap, states_list =
      List.fold_right
        (fun state_id (states_idmap, states_list) ->
          let state_uid = StateName.fresh state_id in
          ( IdentMap.add (Marked.unmark state_id) state_uid states_idmap,
            state_uid :: states_list ))
        decl.scope_decl_context_item_states (IdentMap.empty, [])
    in
    {
      ctxt with
      scopes = ScopeName.Map.add scope scope_ctxt ctxt.scopes;
      var_typs =
        ScopeVar.Map.add uid
          {
            var_sig_typ = data_typ;
            var_sig_is_condition = is_cond;
            var_sig_io = decl.scope_decl_context_item_attribute;
            var_sig_states_idmap = states_idmap;
            var_sig_states_list = states_list;
          }
          ctxt.var_typs;
    }

(** Adds a binding to the context *)
let add_def_local_var (ctxt : context) (name : ident) : context * Ast.expr Var.t
    =
  let local_var_uid = Var.make name in
  let ctxt =
    {
      ctxt with
      local_var_idmap = IdentMap.add name local_var_uid ctxt.local_var_idmap;
    }
  in
  ctxt, local_var_uid

(** Process a struct declaration *)
let process_struct_decl (ctxt : context) (sdecl : Surface.Ast.struct_decl) :
    context =
  let s_uid = get_struct ctxt sdecl.struct_decl_name in
  if sdecl.struct_decl_fields = [] then
    Errors.raise_spanned_error
      (Marked.get_mark sdecl.struct_decl_name)
      "The struct %s does not have any fields; give it some for Catala to be \
       able to accept it."
      (Marked.unmark sdecl.struct_decl_name);
  List.fold_left
    (fun ctxt (fdecl, _) ->
      let f_uid = StructField.fresh fdecl.Surface.Ast.struct_decl_field_name in
      let ctxt =
        {
          ctxt with
          field_idmap =
            IdentMap.update
              (Marked.unmark fdecl.Surface.Ast.struct_decl_field_name)
              (fun uids ->
                match uids with
                | None -> Some (StructName.Map.singleton s_uid f_uid)
                | Some uids -> Some (StructName.Map.add s_uid f_uid uids))
              ctxt.field_idmap;
        }
      in
      {
        ctxt with
        structs =
          StructName.Map.update s_uid
            (fun fields ->
              match fields with
              | None ->
                Some
                  (StructField.Map.singleton f_uid
                     (process_type ctxt fdecl.Surface.Ast.struct_decl_field_typ))
              | Some fields ->
                Some
                  (StructField.Map.add f_uid
                     (process_type ctxt fdecl.Surface.Ast.struct_decl_field_typ)
                     fields))
            ctxt.structs;
      })
    ctxt sdecl.struct_decl_fields

(** Process an enum declaration *)
let process_enum_decl (ctxt : context) (edecl : Surface.Ast.enum_decl) : context
    =
  let e_uid = get_enum ctxt edecl.enum_decl_name in
  if List.length edecl.enum_decl_cases = 0 then
    Errors.raise_spanned_error
      (Marked.get_mark edecl.enum_decl_name)
      "The enum %s does not have any cases; give it some for Catala to be able \
       to accept it."
      (Marked.unmark edecl.enum_decl_name);
  List.fold_left
    (fun ctxt (cdecl, cdecl_pos) ->
      let c_uid = EnumConstructor.fresh cdecl.Surface.Ast.enum_decl_case_name in
      let ctxt =
        {
          ctxt with
          constructor_idmap =
            IdentMap.update
              (Marked.unmark cdecl.Surface.Ast.enum_decl_case_name)
              (fun uids ->
                match uids with
                | None -> Some (EnumName.Map.singleton e_uid c_uid)
                | Some uids -> Some (EnumName.Map.add e_uid c_uid uids))
              ctxt.constructor_idmap;
        }
      in
      {
        ctxt with
        enums =
          EnumName.Map.update e_uid
            (fun cases ->
              let typ =
                match cdecl.Surface.Ast.enum_decl_case_typ with
                | None -> TLit TUnit, cdecl_pos
                | Some typ -> process_type ctxt typ
              in
              match cases with
              | None -> Some (EnumConstructor.Map.singleton c_uid typ)
              | Some fields -> Some (EnumConstructor.Map.add c_uid typ fields))
            ctxt.enums;
      })
    ctxt edecl.enum_decl_cases

(** Process an item declaration *)
let process_item_decl
    (scope : ScopeName.t)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl_context_item) : context =
  match decl with
  | Surface.Ast.ContextData data_decl -> process_data_decl scope ctxt data_decl
  | Surface.Ast.ContextScope sub_decl ->
    process_subscope_decl scope ctxt sub_decl

(** Process a scope declaration *)
let process_scope_decl (ctxt : context) (decl : Surface.Ast.scope_decl) :
    context =
  let scope_uid = get_scope ctxt decl.scope_decl_name in
  let ctxt =
    List.fold_left
      (fun ctxt item -> process_item_decl scope_uid ctxt (Marked.unmark item))
      ctxt decl.scope_decl_context
  in
  (* Add an implicit struct def for the scope output type *)
  let output_fields =
    List.fold_right
      (fun item acc ->
        match Marked.unmark item with
        | Surface.Ast.ContextData
            ({
               scope_decl_context_item_attribute =
                 { scope_decl_context_io_output = true, _; _ };
               _;
             } as data) ->
          Marked.mark (Marked.get_mark item)
            {
              Surface.Ast.struct_decl_field_name =
                data.scope_decl_context_item_name;
              Surface.Ast.struct_decl_field_typ =
                data.scope_decl_context_item_typ;
            }
          :: acc
        | _ -> acc)
      decl.scope_decl_context []
  in
  if output_fields = [] then
    (* we allow scopes without output variables, and still define their (empty)
       output struct for convenience *)
    {
      ctxt with
      structs =
        StructName.Map.add
          (get_struct ctxt decl.scope_decl_name)
          StructField.Map.empty ctxt.structs;
    }
  else
    let ctxt =
      process_struct_decl ctxt
        {
          struct_decl_name = decl.scope_decl_name;
          struct_decl_fields = output_fields;
        }
    in
    let out_struct_fields =
      let sco = ScopeName.Map.find scope_uid ctxt.scopes in
      let str = get_struct ctxt decl.scope_decl_name in
      IdentMap.fold
        (fun id var svmap ->
          match var with
          | SubScope _ -> svmap
          | ScopeVar v -> (
            try
              let field =
                StructName.Map.find str (IdentMap.find id ctxt.field_idmap)
              in
              ScopeVar.Map.add v field svmap
            with Not_found -> svmap))
        sco.var_idmap ScopeVar.Map.empty
    in
    let typedefs =
      IdentMap.update
        (Marked.unmark decl.scope_decl_name)
        (function
          | Some (TScope (scope, { out_struct_name; _ })) ->
            Some (TScope (scope, { out_struct_name; out_struct_fields }))
          | _ -> assert false)
        ctxt.typedefs
    in
    { ctxt with typedefs }

let typedef_info = function
  | TStruct t -> StructName.get_info t
  | TEnum t -> EnumName.get_info t
  | TScope (s, _) -> ScopeName.get_info s

(** Process the names of all declaration items *)
let process_name_item (ctxt : context) (item : Surface.Ast.code_item Marked.pos)
    : context =
  let raise_already_defined_error (use : Uid.MarkedString.info) name pos msg =
    Errors.raise_multispanned_error
      [
        Some "First definition:", Marked.get_mark use;
        Some "Second definition:", pos;
      ]
      "%s name \"%a\" already defined" msg
      (Cli.format_with_style [ANSITerminal.yellow])
      name
  in
  match Marked.unmark item with
  | ScopeDecl decl ->
    let name, pos = decl.scope_decl_name in
    (* Checks if the name is already used *)
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "scope")
      (IdentMap.find_opt name ctxt.typedefs);
    let scope_uid = ScopeName.fresh (name, pos) in
    let out_struct_uid = StructName.fresh (name, pos) in
    {
      ctxt with
      typedefs =
        IdentMap.add name
          (TScope
             ( scope_uid,
               {
                 out_struct_name = out_struct_uid;
                 out_struct_fields = ScopeVar.Map.empty;
               } ))
          ctxt.typedefs;
      scopes =
        ScopeName.Map.add scope_uid
          {
            var_idmap = IdentMap.empty;
            scope_defs_contexts = Ast.ScopeDefMap.empty;
            sub_scopes = ScopeName.Set.empty;
          }
          ctxt.scopes;
    }
  | StructDecl sdecl ->
    let name, pos = sdecl.struct_decl_name in
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "struct")
      (IdentMap.find_opt name ctxt.typedefs);
    let s_uid = StructName.fresh sdecl.struct_decl_name in
    {
      ctxt with
      typedefs =
        IdentMap.add
          (Marked.unmark sdecl.struct_decl_name)
          (TStruct s_uid) ctxt.typedefs;
    }
  | EnumDecl edecl ->
    let name, pos = edecl.enum_decl_name in
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "enum")
      (IdentMap.find_opt name ctxt.typedefs);
    let e_uid = EnumName.fresh edecl.enum_decl_name in
    {
      ctxt with
      typedefs =
        IdentMap.add
          (Marked.unmark edecl.enum_decl_name)
          (TEnum e_uid) ctxt.typedefs;
    }
  | ScopeUse _ -> ctxt

(** Process a code item that is a declaration *)
let process_decl_item (ctxt : context) (item : Surface.Ast.code_item Marked.pos)
    : context =
  match Marked.unmark item with
  | ScopeDecl decl -> process_scope_decl ctxt decl
  | StructDecl sdecl -> process_struct_decl ctxt sdecl
  | EnumDecl edecl -> process_enum_decl ctxt edecl
  | ScopeUse _ -> ctxt

(** Process a code block *)
let process_code_block
    (ctxt : context)
    (block : Surface.Ast.code_block)
    (process_item : context -> Surface.Ast.code_item Marked.pos -> context) :
    context =
  List.fold_left (fun ctxt decl -> process_item ctxt decl) ctxt block

(** Process a law structure, only considering the code blocks *)
let rec process_law_structure
    (ctxt : context)
    (s : Surface.Ast.law_structure)
    (process_item : context -> Surface.Ast.code_item Marked.pos -> context) :
    context =
  match s with
  | Surface.Ast.LawHeading (_, children) ->
    List.fold_left
      (fun ctxt child -> process_law_structure ctxt child process_item)
      ctxt children
  | Surface.Ast.CodeBlock (block, _, _) ->
    process_code_block ctxt block process_item
  | Surface.Ast.LawInclude _ | Surface.Ast.LawText _ -> ctxt

(** {1 Scope uses pass} *)

let get_def_key
    (name : Surface.Ast.qident)
    (state : Surface.Ast.ident Marked.pos option)
    (scope_uid : ScopeName.t)
    (ctxt : context)
    (pos : Pos.t) : Ast.ScopeDef.t =
  let scope_ctxt = ScopeName.Map.find scope_uid ctxt.scopes in
  match name with
  | [x] ->
    let x_uid = get_var_uid scope_uid ctxt x in
    let var_sig = ScopeVar.Map.find x_uid ctxt.var_typs in
    Ast.ScopeDef.Var
      ( x_uid,
        match state with
        | Some state -> (
          try
            Some
              (IdentMap.find (Marked.unmark state) var_sig.var_sig_states_idmap)
          with Not_found ->
            Errors.raise_multispanned_error
              [
                None, Marked.get_mark state;
                ( Some "Variable declaration:",
                  Marked.get_mark (ScopeVar.get_info x_uid) );
              ]
              "This identifier is not a state declared for variable %a."
              ScopeVar.format_t x_uid)
        | None ->
          if not (IdentMap.is_empty var_sig.var_sig_states_idmap) then
            Errors.raise_multispanned_error
              [
                None, Marked.get_mark x;
                ( Some "Variable declaration:",
                  Marked.get_mark (ScopeVar.get_info x_uid) );
              ]
              "This definition does not indicate which state has to be \
               considered for variable %a."
              ScopeVar.format_t x_uid
          else None )
  | [y; x] ->
    let (subscope_uid, subscope_real_uid) : SubScopeName.t * ScopeName.t =
      match IdentMap.find_opt (Marked.unmark y) scope_ctxt.var_idmap with
      | Some (SubScope (v, u)) -> v, u
      | Some _ ->
        Errors.raise_spanned_error pos
          "Invalid access to input variable, %a is not a subscope"
          Print.lit_style (Marked.unmark y)
      | None ->
        Errors.raise_spanned_error pos "No definition found for subscope %a"
          Print.lit_style (Marked.unmark y)
    in
    let x_uid = get_var_uid subscope_real_uid ctxt x in
    Ast.ScopeDef.SubScopeVar (subscope_uid, x_uid, pos)
  | _ ->
    Errors.raise_spanned_error pos
      "This line is defining a quantity that is neither a scope variable nor a \
       subscope variable. In particular, it is not possible to define struct \
       fields individually in Catala."

let update_def_key_ctx
    (d : Surface.Ast.definition)
    (def_key_ctx : scope_def_context) : scope_def_context =
  (* First, we update the def key context with information about the
     definition's label*)
  let def_key_ctx =
    match d.Surface.Ast.definition_label with
    | None -> def_key_ctx
    | Some label ->
      let new_label_idmap =
        IdentMap.update (Marked.unmark label)
          (fun existing_label ->
            match existing_label with
            | Some existing_label -> Some existing_label
            | None -> Some (LabelName.fresh label))
          def_key_ctx.label_idmap
      in
      { def_key_ctx with label_idmap = new_label_idmap }
  in
  (* And second, we update the map of default rulenames for unlabeled
     exceptions *)
  match d.Surface.Ast.definition_exception_to with
  (* If this definition is an exception, it cannot be a default definition *)
  | UnlabeledException | ExceptionToLabel _ -> def_key_ctx
  (* If it is not an exception, we need to distinguish between several cases *)
  | NotAnException -> (
    match def_key_ctx.default_exception_rulename with
    (* There was already a default definition for this key. If we need it, it is
       ambiguous *)
    | Some old ->
      {
        def_key_ctx with
        default_exception_rulename =
          Some
            (Ambiguous
               ([Marked.get_mark d.definition_name]
               @
               match old with Ambiguous old -> old | Unique (_, pos) -> [pos]));
      }
    (* No definition has been set yet for this key *)
    | None -> (
      match d.Surface.Ast.definition_label with
      (* This default definition has a label. This is not allowed for unlabeled
         exceptions *)
      | Some _ ->
        {
          def_key_ctx with
          default_exception_rulename =
            Some (Ambiguous [Marked.get_mark d.definition_name]);
        }
      (* This is a possible default definition for this key. We create and store
         a fresh rulename *)
      | None ->
        {
          def_key_ctx with
          default_exception_rulename =
            Some (Unique (d.definition_id, Marked.get_mark d.definition_name));
        }))

let empty_def_key_ctx =
  {
    (* Here, this is the first time we encounter a definition for this
       definition key *)
    default_exception_rulename = None;
    label_idmap = IdentMap.empty;
  }

let process_definition
    (ctxt : context)
    (s_name : ScopeName.t)
    (d : Surface.Ast.definition) : context =
  (* We update the definition context inside the big context *)
  {
    ctxt with
    scopes =
      ScopeName.Map.update s_name
        (fun (s_ctxt : scope_context option) ->
          let def_key =
            get_def_key
              (Marked.unmark d.definition_name)
              d.definition_state s_name ctxt
              (Marked.get_mark d.definition_name)
          in
          match s_ctxt with
          | None -> assert false (* should not happen *)
          | Some s_ctxt ->
            Some
              {
                s_ctxt with
                scope_defs_contexts =
                  Ast.ScopeDefMap.update def_key
                    (fun def_key_ctx ->
                      Some
                        (update_def_key_ctx d
                           (Option.value ~default:empty_def_key_ctx def_key_ctx)))
                    s_ctxt.scope_defs_contexts;
              })
        ctxt.scopes;
  }

let process_scope_use_item
    (s_name : ScopeName.t)
    (ctxt : context)
    (sitem : Surface.Ast.scope_use_item Marked.pos) : context =
  match Marked.unmark sitem with
  | Rule r -> process_definition ctxt s_name (Surface.Ast.rule_to_def r)
  | Definition d -> process_definition ctxt s_name d
  | _ -> ctxt

let process_scope_use (ctxt : context) (suse : Surface.Ast.scope_use) : context
    =
  let s_name =
    match
      IdentMap.find_opt
        (Marked.unmark suse.Surface.Ast.scope_use_name)
        ctxt.typedefs
    with
    | Some (TScope (sn, _)) -> sn
    | _ ->
      Errors.raise_spanned_error
        (Marked.get_mark suse.Surface.Ast.scope_use_name)
        "\"%a\": this scope has not been declared anywhere, is it a typo?"
        (Cli.format_with_style [ANSITerminal.yellow])
        (Marked.unmark suse.Surface.Ast.scope_use_name)
  in
  List.fold_left
    (process_scope_use_item s_name)
    ctxt suse.Surface.Ast.scope_use_items

let process_use_item (ctxt : context) (item : Surface.Ast.code_item Marked.pos)
    : context =
  match Marked.unmark item with
  | ScopeDecl _ | StructDecl _ | EnumDecl _ -> ctxt
  | ScopeUse suse -> process_scope_use ctxt suse

(** {1 API} *)

(** Derive the context from metadata, in one pass over the declarations *)
let form_context (prgm : Surface.Ast.program) : context =
  let empty_ctxt =
    {
      local_var_idmap = IdentMap.empty;
      typedefs = IdentMap.empty;
      scopes = ScopeName.Map.empty;
      var_typs = ScopeVar.Map.empty;
      structs = StructName.Map.empty;
      field_idmap = IdentMap.empty;
      enums = EnumName.Map.empty;
      constructor_idmap = IdentMap.empty;
    }
  in
  let ctxt =
    List.fold_left
      (fun ctxt item -> process_law_structure ctxt item process_name_item)
      empty_ctxt prgm.program_items
  in
  let ctxt =
    List.fold_left
      (fun ctxt item -> process_law_structure ctxt item process_decl_item)
      ctxt prgm.program_items
  in
  let ctxt =
    List.fold_left
      (fun ctxt item -> process_law_structure ctxt item process_use_item)
      ctxt prgm.program_items
  in
  ctxt