(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr> Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Builds a context that allows for mapping each name to a precise uid, taking lexical scopes into
    account *)

open Utils

(** {1 Name resolution context} *)

type ident = string

type typ = Scopelang.Ast.typ

type unique_rulename = Ambiguous of Pos.t list | Unique of Desugared.Ast.RuleName.t

type scope_def_context = {
  default_exception_rulename : unique_rulename option;
  label_idmap : Desugared.Ast.LabelName.t Desugared.Ast.IdentMap.t;
  label_groups : Desugared.Ast.RuleSet.t Desugared.Ast.LabelMap.t;
}

type scope_context = {
  var_idmap : Scopelang.Ast.ScopeVar.t Desugared.Ast.IdentMap.t;  (** Scope variables *)
  scope_defs_contexts : scope_def_context Desugared.Ast.ScopeDefMap.t;
      (** What is the default rule to refer to for unnamed exceptions, if any *)
  sub_scopes_idmap : Scopelang.Ast.SubScopeName.t Desugared.Ast.IdentMap.t;
      (** Sub-scopes variables *)
  sub_scopes : Scopelang.Ast.ScopeName.t Scopelang.Ast.SubScopeMap.t;
      (** To what scope sub-scopes refer to? *)
}
(** Inside a scope, we distinguish between the variables and the subscopes. *)

type struct_context = typ Pos.marked Scopelang.Ast.StructFieldMap.t
(** Types of the fields of a struct *)

type enum_context = typ Pos.marked Scopelang.Ast.EnumConstructorMap.t
(** Types of the payloads of the cases of an enum *)

type context = {
  local_var_idmap : Scopelang.Ast.Var.t Desugared.Ast.IdentMap.t;
      (** Inside a definition, local variables can be introduced by functions arguments or pattern
          matching *)
  scope_idmap : Scopelang.Ast.ScopeName.t Desugared.Ast.IdentMap.t;  (** The names of the scopes *)
  struct_idmap : Scopelang.Ast.StructName.t Desugared.Ast.IdentMap.t;
      (** The names of the structs *)
  field_idmap : Scopelang.Ast.StructFieldName.t Scopelang.Ast.StructMap.t Desugared.Ast.IdentMap.t;
      (** The names of the struct fields. Names of fields can be shared between different structs *)
  enum_idmap : Scopelang.Ast.EnumName.t Desugared.Ast.IdentMap.t;  (** The names of the enums *)
  constructor_idmap :
    Scopelang.Ast.EnumConstructor.t Scopelang.Ast.EnumMap.t Desugared.Ast.IdentMap.t;
      (** The names of the enum constructors. Constructor names can be shared between different
          enums *)
  scopes : scope_context Scopelang.Ast.ScopeMap.t;  (** For each scope, its context *)
  structs : struct_context Scopelang.Ast.StructMap.t;  (** For each struct, its context *)
  enums : enum_context Scopelang.Ast.EnumMap.t;  (** For each enum, its context *)
  var_typs : (typ Pos.marked * bool) (* is it a condition? *) Scopelang.Ast.ScopeVarMap.t;
      (** The types of each scope variable declared *)
}
(** Main context used throughout {!module: Surface.Desugaring} *)

(** {1 Helpers} *)

(** Temporary function raising an error message saying that a feature is not supported yet *)
let raise_unsupported_feature (msg : string) (pos : Pos.t) =
  Errors.raise_spanned_error (Printf.sprintf "Unsupported feature: %s" msg) pos

(** Function to call whenever an identifier used somewhere has not been declared in the program
    previously *)
let raise_unknown_identifier (msg : string) (ident : ident Pos.marked) =
  Errors.raise_spanned_error
    (Printf.sprintf "\"%s\": unknown identifier %s"
       (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s" (Pos.unmark ident))
       msg)
    (Pos.get_position ident)

(** Gets the type associated to an uid *)
let get_var_typ (ctxt : context) (uid : Scopelang.Ast.ScopeVar.t) : typ Pos.marked =
  fst (Scopelang.Ast.ScopeVarMap.find uid ctxt.var_typs)

let is_var_cond (ctxt : context) (uid : Scopelang.Ast.ScopeVar.t) : bool =
  snd (Scopelang.Ast.ScopeVarMap.find uid ctxt.var_typs)

(** Get the variable uid inside the scope given in argument *)
let get_var_uid (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : context)
    ((x, pos) : ident Pos.marked) : Scopelang.Ast.ScopeVar.t =
  let scope = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  match Desugared.Ast.IdentMap.find_opt x scope.var_idmap with
  | None ->
      raise_unknown_identifier
        (Format.asprintf "for a variable of scope %a" Scopelang.Ast.ScopeName.format_t scope_uid)
        (x, pos)
  | Some uid -> uid

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : context)
    ((y, pos) : ident Pos.marked) : Scopelang.Ast.SubScopeName.t =
  let scope = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  match Desugared.Ast.IdentMap.find_opt y scope.sub_scopes_idmap with
  | None -> raise_unknown_identifier "for a subscope of this scope" (y, pos)
  | Some sub_uid -> sub_uid

(** [is_subscope_uid scope_uid ctxt y] returns true if [y] belongs to the subscopes of [scope_uid]. *)
let is_subscope_uid (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : context) (y : ident) : bool =
  let scope = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  Desugared.Ast.IdentMap.mem y scope.sub_scopes_idmap

(** Checks if the var_uid belongs to the scope scope_uid *)
let belongs_to (ctxt : context) (uid : Scopelang.Ast.ScopeVar.t)
    (scope_uid : Scopelang.Ast.ScopeName.t) : bool =
  let scope = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  Desugared.Ast.IdentMap.exists
    (fun _ var_uid -> Scopelang.Ast.ScopeVar.compare uid var_uid = 0)
    scope.var_idmap

(** Retrieves the type of a scope definition from the context *)
let get_def_typ (ctxt : context) (def : Desugared.Ast.ScopeDef.t) : typ Pos.marked =
  match def with
  | Desugared.Ast.ScopeDef.SubScopeVar (_, x)
  (* we don't need to look at the subscope prefix because [x] is already the uid referring back to
     the original subscope *)
  | Desugared.Ast.ScopeDef.Var x ->
      get_var_typ ctxt x

let is_def_cond (ctxt : context) (def : Desugared.Ast.ScopeDef.t) : bool =
  match def with
  | Desugared.Ast.ScopeDef.SubScopeVar (_, x)
  (* we don't need to look at the subscope prefix because [x] is already the uid referring back to
     the original subscope *)
  | Desugared.Ast.ScopeDef.Var x ->
      is_var_cond ctxt x

let label_groups (ctxt : context) (s_uid : Scopelang.Ast.ScopeName.t)
    (def : Desugared.Ast.ScopeDef.t) : Desugared.Ast.RuleSet.t Desugared.Ast.LabelMap.t =
  (Desugared.Ast.ScopeDefMap.find def
     (Scopelang.Ast.ScopeMap.find s_uid ctxt.scopes).scope_defs_contexts)
    .label_groups

(** {1 Declarations pass} *)

(** Process a subscope declaration *)
let process_subscope_decl (scope : Scopelang.Ast.ScopeName.t) (ctxt : context)
    (decl : Ast.scope_decl_context_scope) : context =
  let name, name_pos = decl.scope_decl_context_scope_name in
  let subscope, s_pos = decl.scope_decl_context_scope_sub_scope in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  match Desugared.Ast.IdentMap.find_opt subscope scope_ctxt.sub_scopes_idmap with
  | Some use ->
      Errors.raise_multispanned_error
        (Format.asprintf "Subscope name \"%s\" already used"
           (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s" subscope))
        [
          (Some "first use", Pos.get_position (Scopelang.Ast.SubScopeName.get_info use));
          (Some "second use", s_pos);
        ]
  | None ->
      let sub_scope_uid = Scopelang.Ast.SubScopeName.fresh (name, name_pos) in
      let original_subscope_uid =
        match Desugared.Ast.IdentMap.find_opt subscope ctxt.scope_idmap with
        | None -> raise_unknown_identifier "for a scope" (subscope, s_pos)
        | Some id -> id
      in
      let scope_ctxt =
        {
          scope_ctxt with
          sub_scopes_idmap =
            Desugared.Ast.IdentMap.add name sub_scope_uid scope_ctxt.sub_scopes_idmap;
          sub_scopes =
            Scopelang.Ast.SubScopeMap.add sub_scope_uid original_subscope_uid scope_ctxt.sub_scopes;
        }
      in
      { ctxt with scopes = Scopelang.Ast.ScopeMap.add scope scope_ctxt ctxt.scopes }

let is_type_cond ((typ, _) : Ast.typ Pos.marked) =
  match typ with
  | Ast.Base Ast.Condition | Ast.Func { arg_typ = _; return_typ = Ast.Condition, _ } -> true
  | _ -> false

(** Process a basic type (all types except function types) *)
let rec process_base_typ (ctxt : context) ((typ, typ_pos) : Ast.base_typ Pos.marked) :
    Scopelang.Ast.typ Pos.marked =
  match typ with
  | Ast.Condition -> (Scopelang.Ast.TLit TBool, typ_pos)
  | Ast.Data (Ast.Collection t) ->
      ( Scopelang.Ast.TArray
          (Pos.unmark (process_base_typ ctxt (Ast.Data (Pos.unmark t), Pos.get_position t))),
        typ_pos )
  | Ast.Data (Ast.Primitive prim) -> (
      match prim with
      | Ast.Integer -> (Scopelang.Ast.TLit TInt, typ_pos)
      | Ast.Decimal -> (Scopelang.Ast.TLit TRat, typ_pos)
      | Ast.Money -> (Scopelang.Ast.TLit TMoney, typ_pos)
      | Ast.Duration -> (Scopelang.Ast.TLit TDuration, typ_pos)
      | Ast.Date -> (Scopelang.Ast.TLit TDate, typ_pos)
      | Ast.Boolean -> (Scopelang.Ast.TLit TBool, typ_pos)
      | Ast.Text -> raise_unsupported_feature "text type" typ_pos
      | Ast.Named ident -> (
          match Desugared.Ast.IdentMap.find_opt ident ctxt.struct_idmap with
          | Some s_uid -> (Scopelang.Ast.TStruct s_uid, typ_pos)
          | None -> (
              match Desugared.Ast.IdentMap.find_opt ident ctxt.enum_idmap with
              | Some e_uid -> (Scopelang.Ast.TEnum e_uid, typ_pos)
              | None ->
                  Errors.raise_spanned_error
                    (Format.asprintf "Unknown type \"%s\", not a struct or enum previously declared"
                       (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s" ident))
                    typ_pos)))

(** Process a type (function or not) *)
let process_type (ctxt : context) ((typ, typ_pos) : Ast.typ Pos.marked) :
    Scopelang.Ast.typ Pos.marked =
  match typ with
  | Ast.Base base_typ -> process_base_typ ctxt (base_typ, typ_pos)
  | Ast.Func { arg_typ; return_typ } ->
      ( Scopelang.Ast.TArrow (process_base_typ ctxt arg_typ, process_base_typ ctxt return_typ),
        typ_pos )

(** Process data declaration *)
let process_data_decl (scope : Scopelang.Ast.ScopeName.t) (ctxt : context)
    (decl : Ast.scope_decl_context_data) : context =
  (* First check the type of the context data *)
  let data_typ = process_type ctxt decl.scope_decl_context_item_typ in
  let is_cond = is_type_cond decl.scope_decl_context_item_typ in
  let name, pos = decl.scope_decl_context_item_name in
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope ctxt.scopes in
  match Desugared.Ast.IdentMap.find_opt name scope_ctxt.var_idmap with
  | Some use ->
      Errors.raise_multispanned_error
        (Format.asprintf "var name \"%s\" already used"
           (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s" name))
        [
          (Some "first use", Pos.get_position (Scopelang.Ast.ScopeVar.get_info use));
          (Some "second use", pos);
        ]
  | None ->
      let uid = Scopelang.Ast.ScopeVar.fresh (name, pos) in
      let scope_ctxt =
        { scope_ctxt with var_idmap = Desugared.Ast.IdentMap.add name uid scope_ctxt.var_idmap }
      in
      {
        ctxt with
        scopes = Scopelang.Ast.ScopeMap.add scope scope_ctxt ctxt.scopes;
        var_typs = Scopelang.Ast.ScopeVarMap.add uid (data_typ, is_cond) ctxt.var_typs;
      }

(** Process an item declaration *)
let process_item_decl (scope : Scopelang.Ast.ScopeName.t) (ctxt : context)
    (decl : Ast.scope_decl_context_item) : context =
  match decl with
  | Ast.ContextData data_decl -> process_data_decl scope ctxt data_decl
  | Ast.ContextScope sub_decl -> process_subscope_decl scope ctxt sub_decl

(** Adds a binding to the context *)
let add_def_local_var (ctxt : context) (name : ident Pos.marked) : context * Scopelang.Ast.Var.t =
  let local_var_uid = Scopelang.Ast.Var.make name in
  let ctxt =
    {
      ctxt with
      local_var_idmap =
        Desugared.Ast.IdentMap.add (Pos.unmark name) local_var_uid ctxt.local_var_idmap;
    }
  in
  (ctxt, local_var_uid)

(** Process a scope declaration *)
let process_scope_decl (ctxt : context) (decl : Ast.scope_decl) : context =
  let name, _ = decl.scope_decl_name in
  let scope_uid = Desugared.Ast.IdentMap.find name ctxt.scope_idmap in
  List.fold_left
    (fun ctxt item -> process_item_decl scope_uid ctxt (Pos.unmark item))
    ctxt decl.scope_decl_context

(** Process a struct declaration *)
let process_struct_decl (ctxt : context) (sdecl : Ast.struct_decl) : context =
  let s_uid = Desugared.Ast.IdentMap.find (fst sdecl.struct_decl_name) ctxt.struct_idmap in
  List.fold_left
    (fun ctxt (fdecl, _) ->
      let f_uid = Scopelang.Ast.StructFieldName.fresh fdecl.Ast.struct_decl_field_name in
      let ctxt =
        {
          ctxt with
          field_idmap =
            Desugared.Ast.IdentMap.update
              (Pos.unmark fdecl.Ast.struct_decl_field_name)
              (fun uids ->
                match uids with
                | None -> Some (Scopelang.Ast.StructMap.singleton s_uid f_uid)
                | Some uids -> Some (Scopelang.Ast.StructMap.add s_uid f_uid uids))
              ctxt.field_idmap;
        }
      in
      {
        ctxt with
        structs =
          Scopelang.Ast.StructMap.update s_uid
            (fun fields ->
              match fields with
              | None ->
                  Some
                    (Scopelang.Ast.StructFieldMap.singleton f_uid
                       (process_type ctxt fdecl.Ast.struct_decl_field_typ))
              | Some fields ->
                  Some
                    (Scopelang.Ast.StructFieldMap.add f_uid
                       (process_type ctxt fdecl.Ast.struct_decl_field_typ)
                       fields))
            ctxt.structs;
      })
    ctxt sdecl.struct_decl_fields

(** Process an enum declaration *)
let process_enum_decl (ctxt : context) (edecl : Ast.enum_decl) : context =
  let e_uid = Desugared.Ast.IdentMap.find (fst edecl.enum_decl_name) ctxt.enum_idmap in
  List.fold_left
    (fun ctxt (cdecl, cdecl_pos) ->
      let c_uid = Scopelang.Ast.EnumConstructor.fresh cdecl.Ast.enum_decl_case_name in
      let ctxt =
        {
          ctxt with
          constructor_idmap =
            Desugared.Ast.IdentMap.update
              (Pos.unmark cdecl.Ast.enum_decl_case_name)
              (fun uids ->
                match uids with
                | None -> Some (Scopelang.Ast.EnumMap.singleton e_uid c_uid)
                | Some uids -> Some (Scopelang.Ast.EnumMap.add e_uid c_uid uids))
              ctxt.constructor_idmap;
        }
      in
      {
        ctxt with
        enums =
          Scopelang.Ast.EnumMap.update e_uid
            (fun cases ->
              let typ =
                match cdecl.Ast.enum_decl_case_typ with
                | None -> (Scopelang.Ast.TLit TUnit, cdecl_pos)
                | Some typ -> process_type ctxt typ
              in
              match cases with
              | None -> Some (Scopelang.Ast.EnumConstructorMap.singleton c_uid typ)
              | Some fields -> Some (Scopelang.Ast.EnumConstructorMap.add c_uid typ fields))
            ctxt.enums;
      })
    ctxt edecl.enum_decl_cases

(** Process the names of all declaration items *)
let process_name_item (ctxt : context) (item : Ast.code_item Pos.marked) : context =
  let raise_already_defined_error (use : Uid.MarkedString.info) name pos msg =
    Errors.raise_multispanned_error
      (Format.asprintf "%s name \"%s\" already defined" msg
         (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s" name))
      [ (Some "First definition:", Pos.get_position use); (Some "Second definition:", pos) ]
  in
  match Pos.unmark item with
  | ScopeDecl decl -> (
      let name, pos = decl.scope_decl_name in
      (* Checks if the name is already used *)
      match Desugared.Ast.IdentMap.find_opt name ctxt.scope_idmap with
      | Some use ->
          raise_already_defined_error (Scopelang.Ast.ScopeName.get_info use) name pos "scope"
      | None ->
          let scope_uid = Scopelang.Ast.ScopeName.fresh (name, pos) in
          {
            ctxt with
            scope_idmap = Desugared.Ast.IdentMap.add name scope_uid ctxt.scope_idmap;
            scopes =
              Scopelang.Ast.ScopeMap.add scope_uid
                {
                  var_idmap = Desugared.Ast.IdentMap.empty;
                  scope_defs_contexts = Desugared.Ast.ScopeDefMap.empty;
                  sub_scopes_idmap = Desugared.Ast.IdentMap.empty;
                  sub_scopes = Scopelang.Ast.SubScopeMap.empty;
                }
                ctxt.scopes;
          })
  | StructDecl sdecl -> (
      let name, pos = sdecl.struct_decl_name in
      match Desugared.Ast.IdentMap.find_opt name ctxt.struct_idmap with
      | Some use ->
          raise_already_defined_error (Scopelang.Ast.StructName.get_info use) name pos "struct"
      | None ->
          let s_uid = Scopelang.Ast.StructName.fresh sdecl.struct_decl_name in
          {
            ctxt with
            struct_idmap =
              Desugared.Ast.IdentMap.add (Pos.unmark sdecl.struct_decl_name) s_uid ctxt.struct_idmap;
          })
  | EnumDecl edecl -> (
      let name, pos = edecl.enum_decl_name in
      match Desugared.Ast.IdentMap.find_opt name ctxt.enum_idmap with
      | Some use ->
          raise_already_defined_error (Scopelang.Ast.EnumName.get_info use) name pos "enum"
      | None ->
          let e_uid = Scopelang.Ast.EnumName.fresh edecl.enum_decl_name in

          {
            ctxt with
            enum_idmap =
              Desugared.Ast.IdentMap.add (Pos.unmark edecl.enum_decl_name) e_uid ctxt.enum_idmap;
          })
  | ScopeUse _ -> ctxt

(** Process a code item that is a declaration *)
let process_decl_item (ctxt : context) (item : Ast.code_item Pos.marked) : context =
  match Pos.unmark item with
  | ScopeDecl decl -> process_scope_decl ctxt decl
  | StructDecl sdecl -> process_struct_decl ctxt sdecl
  | EnumDecl edecl -> process_enum_decl ctxt edecl
  | ScopeUse _ -> ctxt

(** Process a code block *)
let process_code_block (ctxt : context) (block : Ast.code_block)
    (process_item : context -> Ast.code_item Pos.marked -> context) : context =
  List.fold_left (fun ctxt decl -> process_item ctxt decl) ctxt block

(** Process a law structure, only considering the code blocks *)
let rec process_law_structure (ctxt : context) (s : Ast.law_structure)
    (process_item : context -> Ast.code_item Pos.marked -> context) : context =
  match s with
  | Ast.LawHeading (_, children) ->
      List.fold_left (fun ctxt child -> process_law_structure ctxt child process_item) ctxt children
  | Ast.CodeBlock (block, _, _) -> process_code_block ctxt block process_item
  | Ast.LawInclude _ | Ast.LawText _ -> ctxt

(** {1 Scope uses pass} *)

let get_def_key (name : Ast.qident) (scope_uid : Scopelang.Ast.ScopeName.t) (ctxt : context)
    (default_pos : Pos.t) : Desugared.Ast.ScopeDef.t =
  let scope_ctxt = Scopelang.Ast.ScopeMap.find scope_uid ctxt.scopes in
  match name with
  | [ x ] ->
      let x_uid = get_var_uid scope_uid ctxt x in
      Desugared.Ast.ScopeDef.Var x_uid
  | [ y; x ] ->
      let subscope_uid : Scopelang.Ast.SubScopeName.t = get_subscope_uid scope_uid ctxt y in
      let subscope_real_uid : Scopelang.Ast.ScopeName.t =
        Scopelang.Ast.SubScopeMap.find subscope_uid scope_ctxt.sub_scopes
      in
      let x_uid = get_var_uid subscope_real_uid ctxt x in
      Desugared.Ast.ScopeDef.SubScopeVar (subscope_uid, x_uid)
  | _ -> Errors.raise_spanned_error "Structs are not handled yet" default_pos

let process_definition (ctxt : context) (s_name : Scopelang.Ast.ScopeName.t) (d : Ast.definition) :
    context =
  (* We update the definition context inside the big context *)
  {
    ctxt with
    scopes =
      Scopelang.Ast.ScopeMap.update s_name
        (fun (s_ctxt : scope_context option) ->
          let def_key =
            get_def_key (Pos.unmark d.definition_name) s_name ctxt
              (Pos.get_position d.definition_expr)
          in
          match s_ctxt with
          | None -> assert false (* should not happen *)
          | Some s_ctxt ->
              Some
                {
                  s_ctxt with
                  scope_defs_contexts =
                    Desugared.Ast.ScopeDefMap.update def_key
                      (fun def_key_ctx ->
                        let def_key_ctx : scope_def_context =
                          Option.fold
                            ~none:
                              {
                                (* Here, this is the first time we encounter a definition for this
                                   definition key *)
                                default_exception_rulename = None;
                                label_idmap = Desugared.Ast.IdentMap.empty;
                                label_groups = Desugared.Ast.LabelMap.empty;
                              }
                            ~some:(fun x -> x)
                            def_key_ctx
                        in
                        (* First, we update the def key context with information about the
                           definition's label*)
                        let def_key_ctx =
                          match d.Ast.definition_label with
                          | None -> def_key_ctx
                          | Some label ->
                              let new_label_idmap =
                                Desugared.Ast.IdentMap.update (Pos.unmark label)
                                  (fun existing_label ->
                                    match existing_label with
                                    | Some existing_label -> Some existing_label
                                    | None -> Some (Desugared.Ast.LabelName.fresh label))
                                  def_key_ctx.label_idmap
                              in
                              let label_id =
                                Desugared.Ast.IdentMap.find (Pos.unmark label) new_label_idmap
                              in
                              {
                                def_key_ctx with
                                label_idmap = new_label_idmap;
                                label_groups =
                                  Desugared.Ast.LabelMap.update label_id
                                    (fun group ->
                                      match group with
                                      | None ->
                                          Some (Desugared.Ast.RuleSet.singleton d.definition_id)
                                      | Some existing_group ->
                                          Some
                                            (Desugared.Ast.RuleSet.add d.definition_id
                                               existing_group))
                                    def_key_ctx.label_groups;
                              }
                        in
                        (* And second, we update the map of default rulenames for unlabeled
                           exceptions *)
                        let def_key_ctx =
                          match d.Ast.definition_exception_to with
                          (* If this definition is an exception, it cannot be a default
                             definition *)
                          | UnlabeledException | ExceptionToLabel _ -> def_key_ctx
                          (* If it is not an exception, we need to distinguish between several
                             cases *)
                          | NotAnException -> (
                              match def_key_ctx.default_exception_rulename with
                              (* There was already a default definition for this key. If we need it,
                                 it is ambiguous *)
                              | Some old ->
                                  {
                                    def_key_ctx with
                                    default_exception_rulename =
                                      Some
                                        (Ambiguous
                                           ([ Pos.get_position d.definition_name ]
                                           @
                                           match old with
                                           | Ambiguous old -> old
                                           | Unique n ->
                                               [
                                                 Pos.get_position
                                                   (Desugared.Ast.RuleName.get_info n);
                                               ]));
                                  }
                              (* No definition has been set yet for this key *)
                              | None -> (
                                  match d.Ast.definition_label with
                                  (* This default definition has a label. This is not allowed for
                                     unlabeled exceptions *)
                                  | Some _ ->
                                      {
                                        def_key_ctx with
                                        default_exception_rulename =
                                          Some (Ambiguous [ Pos.get_position d.definition_name ]);
                                      }
                                  (* This is a possible default definition for this key. We create
                                     and store a fresh rulename *)
                                  | None ->
                                      {
                                        def_key_ctx with
                                        default_exception_rulename = Some (Unique d.definition_id);
                                      }))
                        in
                        Some def_key_ctx)
                      s_ctxt.scope_defs_contexts;
                })
        ctxt.scopes;
  }

let process_scope_use_item (s_name : Scopelang.Ast.ScopeName.t) (ctxt : context)
    (sitem : Ast.scope_use_item Pos.marked) : context =
  match Pos.unmark sitem with
  | Rule r -> process_definition ctxt s_name (Ast.rule_to_def r)
  | Definition d -> process_definition ctxt s_name d
  | _ -> ctxt

let process_scope_use (ctxt : context) (suse : Ast.scope_use) : context =
  let s_name =
    try Desugared.Ast.IdentMap.find (Pos.unmark suse.Ast.scope_use_name) ctxt.scope_idmap
    with Not_found ->
      Errors.raise_spanned_error
        (Format.asprintf "\"%s\": this scope has not been declared anywhere, is it a typo?"
           (Utils.Cli.print_with_style [ ANSITerminal.yellow ] "%s"
              (Pos.unmark suse.Ast.scope_use_name)))
        (Pos.get_position suse.Ast.scope_use_name)
  in
  List.fold_left (process_scope_use_item s_name) ctxt suse.Ast.scope_use_items

let process_use_item (ctxt : context) (item : Ast.code_item Pos.marked) : context =
  match Pos.unmark item with
  | ScopeDecl _ | StructDecl _ | EnumDecl _ -> ctxt
  | ScopeUse suse -> process_scope_use ctxt suse

(** {1 API} *)

(** Derive the context from metadata, in one pass over the declarations *)
let form_context (prgm : Ast.program) : context =
  let empty_ctxt =
    {
      local_var_idmap = Desugared.Ast.IdentMap.empty;
      scope_idmap = Desugared.Ast.IdentMap.empty;
      scopes = Scopelang.Ast.ScopeMap.empty;
      var_typs = Scopelang.Ast.ScopeVarMap.empty;
      structs = Scopelang.Ast.StructMap.empty;
      struct_idmap = Desugared.Ast.IdentMap.empty;
      field_idmap = Desugared.Ast.IdentMap.empty;
      enums = Scopelang.Ast.EnumMap.empty;
      enum_idmap = Desugared.Ast.IdentMap.empty;
      constructor_idmap = Desugared.Ast.IdentMap.empty;
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
