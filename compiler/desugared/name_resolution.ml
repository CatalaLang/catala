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

type unique_rulename = Ambiguous of Pos.t list | Unique of RuleName.t Mark.pos

type scope_def_context = {
  default_exception_rulename : unique_rulename option;
  label_idmap : LabelName.t Ident.Map.t;
}

type scope_context = {
  var_idmap : scope_var_or_subscope Ident.Map.t;
      (** All variables, including scope variables and subscopes *)
  scope_defs_contexts : scope_def_context Ast.ScopeDef.Map.t;
      (** What is the default rule to refer to for unnamed exceptions, if any *)
  scope_in_struct : StructName.t;
  scope_out_struct : StructName.t;
  sub_scopes : ScopeName.Set.t;
      (** Other scopes referred to by this scope. Used for dependency analysis *)
  scope_visibility : visibility;
}
(** Inside a scope, we distinguish between the variables and the subscopes. *)

type struct_context = typ StructField.Map.t
(** Types of the fields of a struct *)

type enum_context = typ EnumConstructor.Map.t
(** Types of the payloads of the cases of an enum *)

type var_sig = {
  var_sig_typ : typ;
  var_sig_is_condition : bool;
  var_sig_parameters :
    (Uid.MarkedString.info * Shared_ast.typ) list Mark.pos option;
  var_sig_io : Surface.Ast.scope_decl_context_io;
  var_sig_states_idmap : StateName.t Ident.Map.t;
  var_sig_states_list : StateName.t list;
}

(** Capitalised type names share a namespace on the user side, but may
    correspond to only one of the following *)
type typedef =
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TScope of ScopeName.t * scope_info  (** Implicitly defined output struct *)

type module_context = {
  current_revpath : ModuleName.t list;
  typedefs : typedef Ident.Map.t;
      (** Gathers the names of the scopes, structs and enums *)
  field_idmap : StructField.t StructName.Map.t Ident.Map.t;
      (** The names of the struct fields. Names of fields can be shared between
          different structs *)
  constructor_idmap : EnumConstructor.t EnumName.Map.t Ident.Map.t;
      (** The names of the enum constructors. Constructor names can be shared
          between different enums *)
  topdefs : TopdefName.t Ident.Map.t;  (** Global definitions *)
  used_modules : ModuleName.t Ident.Map.t;
  is_external : bool;
}
(** Context for name resolution, valid within a given module *)

type context = {
  scopes : scope_context ScopeName.Map.t;  (** For each scope, its context *)
  topdefs : (typ * visibility) TopdefName.Map.t;
  structs : (struct_context * visibility) StructName.Map.t;
      (** For each struct, its context *)
  enums : (enum_context * visibility) EnumName.Map.t;
      (** For each enum, its context *)
  var_typs : var_sig ScopeVar.Map.t;
      (** The signatures of each scope variable declared *)
  modules : module_context ModuleName.Map.t;
  local : module_context;
      (** Module being currently analysed (at the end: the root module) *)
}
(** Global context used throughout {!module: Surface.Desugaring} *)

(** {1 Attribute resolution handling} *)
type attribute_context =
  | ScopeDecl
  | StructDecl
  | EnumDecl
  | Topdef
  | ScopeDef
  | FieldDecl
  | ConstructorDecl
  | Expression
  | Type
  | FunctionArgument

let attribute_parsers :
    (string
    * string list
    * attribute_context list
    * (pos:Pos.t -> attr_value -> Pos.attr option))
    list
    ref =
  ref []

let register_attribute ~plugin ~path ~contexts callback =
  attribute_parsers := (plugin, path, contexts, callback) :: !attribute_parsers

let handle_extra_attributes context plugin path value pos =
  match List.find_all (fun (n, _, _, _) -> plugin = n) !attribute_parsers with
  | [] ->
    Message.warning ~pos
      "No plugin registered to handle attribute @{<magenta>#[%s.%s]@}" plugin
      (String.concat "." path);
    None
  | handlers -> (
    match List.find_all (fun (_, p, _, _) -> path = p) handlers with
    | [] ->
      Message.warning ~pos
        ~suggestion:
          (List.map (fun (_, p, _, _) -> String.concat "." p) handlers)
        "Invalid sub-attribute \"%s\" of plugin %s" (String.concat "." path)
        plugin;
      None
    | handlers -> (
      match
        List.find_opt (fun (_, _, ctx, _) -> List.mem context ctx) handlers
      with
      | None ->
        Message.warning ~pos
          "Attribute @{<magenta>#[%s.%s]@} is not allowed in this context"
          plugin (String.concat "." path);
        None
      | Some (_, _, _, callback) -> callback ~pos value))

let translate_attr ~context = function
  | Src ((p1 :: ps, ppos), v, pos) -> (
    match p1 with
    | "debug" -> (
      match ps with
      | ["print"] -> (
        if context <> Expression then (
          Message.warning ~pos
            "Attribute @{<magenta>#[debug.print]@} is not allowed in this \
             context";
          None)
        else
          match v with
          | Unit -> Some (DebugPrint { label = None })
          | String (s, _) -> Some (DebugPrint { label = Some s })
          | Surface.Ast.Expression (_, pos) ->
            Message.warning ~pos
              "Invalid value for attribute @{<magenta>#[debug.print]@}";
            None
          | _ ->
            Message.warning ~pos
              "Invalid value for attribute @{<magenta>#[debug.print]@}";
            None)
      | ps ->
        Message.warning ~pos:ppos ~suggestion:["print"]
          "Unknown debug attribute \"%s\"" (String.concat "." ps);
        None)
    | "test" -> (
      match ps with
      | [] ->
        if v <> Unit then (
          Message.warning ~pos
            "The @{<magenta>#[test]@} attribute doesn't allow specifying a \
             value";
          None)
        else if context <> ScopeDecl then (
          Message.warning ~pos
            "Attribute @{<magenta>#[test]@} is not allowed in this context";
          None)
        else Some Test
      | ps ->
        Message.warning ~pos:ppos "Unknown test sub-attribute \"%s\""
          (String.concat "." ps);
        None)
    | "doc" -> (
      match ps with
      | [] -> (
        if context = Expression then (
          Message.warning ~pos
            "Attribute @{<magenta>#[doc]@} is not allowed in this context";
          None)
        else
          match v with
          | String (s, pos) -> Some (Doc (s, pos))
          | _ ->
            Message.warning ~pos
              "Invalid value for the @{<magenta>#[doc]@} attribute: expecting \
               a string";
            None)
      | ps ->
        Message.warning ~pos:ppos "Unknown doc sub-attribute \"%s\""
          (String.concat "." ps);
        None)
    | "implicit_position_argument" -> (
      match ps with
      | [] ->
        if context <> FunctionArgument then (
          Message.warning ~pos
            "Attribute @{<magenta>#[implicit_position_argument]@} is not \
             allowed in this context";
          None)
        else if v <> Unit then (
          Message.warning ~pos
            "The @{<magenta>#[implicit_position_argument]@} attribute doesn't \
             allow specifying a value";
          None)
        else Some ImplicitPosArg
      | ps ->
        Message.warning ~pos:ppos
          "Unknown implicit_position_argument sub-attribute \"%s\""
          (String.concat "." ps);
        None)
    | "passthrough" ->
      (* This special case is used for internal testing: the rest of the
         attribute is kept as Src. See
         [tests/attributes/good/simple.catala_en] *)
      Some (Src ((ps, ppos), v, pos))
    | plugin ->
      if ps = [] then (
        Message.warning ~pos "Unrecognised attribute \"%s\"" p1;
        None)
      else handle_extra_attributes context plugin ps v ppos)
  | attr ->
    (* Docstrings (`## ` comments) end up here as they are `Doc` attributes
       already. No check that they are in a relevant spot is done, though. *)
    Some attr

let translate_pos context pos =
  Pos.attrs pos
  |> List.filter_map (translate_attr ~context)
  |> Pos.set_attrs pos

(** {1 Helpers} *)

(** Temporary function raising an error message saying that a feature is not
    supported yet *)
let raise_unsupported_feature (msg : string) (pos : Pos.t) =
  Message.error ~pos "Unsupported feature: %s" msg

(** Function to call whenever an identifier used somewhere has not been declared
    in the program previously *)
let raise_unknown_identifier (msg : string) (ident : Ident.t Mark.pos) =
  Message.error ~pos:(Mark.get ident)
    "@{<yellow>\"%s\"@}: unknown identifier %s" (Mark.remove ident) msg

(** Gets the type associated to an uid *)
let get_var_typ (ctxt : context) (uid : ScopeVar.t) : typ =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_typ

let is_var_cond (ctxt : context) (uid : ScopeVar.t) : bool =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_is_condition

let get_var_io (ctxt : context) (uid : ScopeVar.t) :
    Surface.Ast.scope_decl_context_io =
  (ScopeVar.Map.find uid ctxt.var_typs).var_sig_io

let get_scope_context (ctxt : context) (scope : ScopeName.t) : scope_context =
  ScopeName.Map.find scope ctxt.scopes

(** Get the variable uid inside the scope given in argument *)
let get_var_uid
    (scope_uid : ScopeName.t)
    (ctxt : context)
    ((x, pos) : Ident.t Mark.pos) : ScopeVar.t =
  let scope = get_scope_context ctxt scope_uid in
  match Ident.Map.find_opt x scope.var_idmap with
  | Some (ScopeVar uid) -> uid
  | _ ->
    raise_unknown_identifier
      (Format.asprintf "for a variable of scope %a" ScopeName.format scope_uid)
      (x, pos)

(** Get the subscope uid inside the scope given in argument *)
let get_subscope_uid
    (scope_uid : ScopeName.t)
    (ctxt : context)
    ((y, pos) : Ident.t Mark.pos) : ScopeVar.t =
  let scope = get_scope_context ctxt scope_uid in
  match Ident.Map.find_opt y scope.var_idmap with
  | Some (SubScope (sub_uid, _sub_id)) -> sub_uid
  | _ -> raise_unknown_identifier "for a subscope of this scope" (y, pos)

(** [is_subscope_uid scope_uid ctxt y] returns true if [y] belongs to the
    subscopes of [scope_uid]. *)
let is_subscope_uid (scope_uid : ScopeName.t) (ctxt : context) (y : Ident.t) :
    bool =
  let scope = get_scope_context ctxt scope_uid in
  match Ident.Map.find_opt y scope.var_idmap with
  | Some (SubScope _) -> true
  | _ -> false

(** Checks if the var_uid belongs to the scope scope_uid *)
let belongs_to (ctxt : context) (uid : ScopeVar.t) (scope_uid : ScopeName.t) :
    bool =
  let scope = get_scope_context ctxt scope_uid in
  Ident.Map.exists
    (fun _ -> function
      | ScopeVar var_uid -> ScopeVar.equal uid var_uid
      | _ -> false)
    scope.var_idmap

let get_var_def (def : Ast.ScopeDef.t) : ScopeVar.t =
  match def with
  | (v, _), Ast.ScopeDef.Var _
  | _, Ast.ScopeDef.SubScopeInput { var_within_origin_scope = v; _ } ->
    v

(** Retrieves the type of a scope definition from the context *)
let get_params (ctxt : context) (def : Ast.ScopeDef.t) :
    (Uid.MarkedString.info * typ) list Mark.pos option =
  (ScopeVar.Map.find (get_var_def def) ctxt.var_typs).var_sig_parameters

let is_def_cond (ctxt : context) (def : Ast.ScopeDef.t) : bool =
  is_var_cond ctxt (get_var_def def)

let get_enum ctxt id =
  match Ident.Map.find (Mark.remove id) ctxt.local.typedefs with
  | TEnum id -> id
  | TStruct sid ->
    Message.error
      ~extra_pos:
        [
          "", Mark.get id;
          "Structure defined at", Mark.get (StructName.get_info sid);
        ]
      "Expecting an enum, but found a structure"
  | TScope (sid, _) ->
    Message.error
      ~extra_pos:
        ["", Mark.get id; "Scope defined at", Mark.get (ScopeName.get_info sid)]
      "Expecting an enum, but found a scope"
  | exception Ident.Map.Not_found _ ->
    Message.error ~pos:(Mark.get id) "No enum named %s found" (Mark.remove id)

let get_struct ctxt id =
  match Ident.Map.find (Mark.remove id) ctxt.local.typedefs with
  | TStruct id | TScope (_, { out_struct_name = id; _ }) -> id
  | TEnum eid ->
    Message.error
      ~extra_pos:
        ["", Mark.get id; "Enum defined at", Mark.get (EnumName.get_info eid)]
      "Expecting a struct, but found an enum"
  | exception Ident.Map.Not_found _ ->
    Message.error ~pos:(Mark.get id) "No struct named %s found" (Mark.remove id)

let get_scope ctxt id =
  match Ident.Map.find (Mark.remove id) ctxt.local.typedefs with
  | TScope (id, _) -> id
  | TEnum eid ->
    Message.error
      ~extra_pos:
        ["", Mark.get id; "Enum defined at", Mark.get (EnumName.get_info eid)]
      "Expecting an scope, but found an enum"
  | TStruct sid ->
    Message.error
      ~extra_pos:
        [
          "", Mark.get id;
          "Structure defined at", Mark.get (StructName.get_info sid);
        ]
      "Expecting an scope, but found a structure"
  | exception Ident.Map.Not_found _ ->
    Message.error ~pos:(Mark.get id) "No scope named %s found" (Mark.remove id)

let get_modname ctxt (id, pos) =
  match Ident.Map.find_opt id ctxt.local.used_modules with
  | None -> Message.error ~pos "Module \"@{<blue>%s@}\" not found" id
  | Some modname -> modname

let get_module_ctx ctxt modname =
  { ctxt with local = ModuleName.Map.find modname ctxt.modules }

let module_ctx ctxt path0 =
  let rec loop acc ctxt = function
    | [] -> List.rev acc, ctxt
    | mod_id :: path ->
      let modname = get_modname ctxt mod_id in
      let ctxt = get_module_ctx ctxt modname in
      loop (modname :: acc) ctxt path
  in
  loop [] ctxt path0

let get_module_ctx ctxt id =
  let modname = get_modname ctxt id in
  get_module_ctx ctxt modname

(** {1 Declarations pass} *)

(** Process a subscope declaration *)
let process_subscope_decl
    (scope : ScopeName.t)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl_context_scope) : context =
  let name, name_pos = decl.scope_decl_context_scope_name in
  let name_pos = translate_pos ScopeDef name_pos in
  let subscope_io =
    {
      Surface.Ast.scope_decl_context_io_output =
        decl.Surface.Ast.scope_decl_context_scope_attribute
          .scope_decl_context_io_output;
      scope_decl_context_io_input =
        decl.Surface.Ast.scope_decl_context_scope_attribute
          .scope_decl_context_io_input;
    }
  in
  let (path, subscope), s_pos = decl.scope_decl_context_scope_sub_scope in
  let scope_ctxt = get_scope_context ctxt scope in
  match Ident.Map.find_opt (Mark.remove subscope) scope_ctxt.var_idmap with
  | Some use ->
    let info =
      match use with
      | ScopeVar v -> ScopeVar.get_info v
      | SubScope (ssc, _) -> ScopeVar.get_info ssc
    in
    Message.error
      ~extra_pos:["first use", Mark.get info; "second use", s_pos]
      "Subscope name @{<yellow>\"%s\"@} already used" (Mark.remove subscope)
  | None ->
    let sub_scope_uid = ScopeVar.fresh (name, name_pos) in
    let original_subscope_uid =
      let _, ctxt = module_ctx ctxt path in
      get_scope ctxt subscope
    in
    let scope_ctxt =
      {
        scope_ctxt with
        var_idmap =
          Ident.Map.add name
            (SubScope (sub_scope_uid, original_subscope_uid))
            scope_ctxt.var_idmap;
        sub_scopes =
          ScopeName.Set.add original_subscope_uid scope_ctxt.sub_scopes;
      }
    in
    let subscope_ctxt = get_scope_context ctxt original_subscope_uid in
    {
      ctxt with
      scopes = ScopeName.Map.add scope scope_ctxt ctxt.scopes;
      var_typs =
        ScopeVar.Map.add sub_scope_uid
          {
            var_sig_typ =
              ( TArrow
                  ( [TStruct subscope_ctxt.scope_in_struct, name_pos],
                    (TStruct subscope_ctxt.scope_out_struct, name_pos) ),
                name_pos );
            var_sig_is_condition = false;
            var_sig_parameters = None;
            (* We do not populate the parameter field for sub-scopes as the
               parameters are the scope's input variables. *)
            var_sig_io = subscope_io;
            var_sig_states_idmap = Shared_ast.Ident.Map.empty;
            var_sig_states_list = [];
          }
          ctxt.var_typs;
    }

let is_type_cond ((typ, _) : Surface.Ast.typ) =
  match typ with
  | Surface.Ast.Base Surface.Ast.Condition
  | Surface.Ast.Func { arg_typ = _; return_typ = Surface.Ast.Condition, _ } ->
    true
  | _ -> false

(** Process a basic type (all types except function types) *)
let rec process_base_typ
    ~rev_path
    ~vars
    (ctxt : context)
    ((typ, typ_pos) : Surface.Ast.base_typ Mark.pos) : typ =
  let typ_pos = translate_pos Type typ_pos in
  match typ with
  | Surface.Ast.Condition -> TLit TBool, typ_pos
  | Surface.Ast.Data (Surface.Ast.Collection t) ->
    ( TArray
        (process_base_typ ~rev_path ~vars ctxt
           (Surface.Ast.Data (Mark.remove t), Mark.get t)),
      typ_pos )
  | Surface.Ast.Data (Surface.Ast.Option t) ->
    ( TOption
        (process_base_typ ~rev_path ~vars ctxt
           (Surface.Ast.Data (Mark.remove t), Mark.get t)),
      typ_pos )
  | Surface.Ast.Data (Surface.Ast.TTuple tl) ->
    ( TTuple
        (List.map
           (fun t ->
             process_base_typ ~rev_path ~vars ctxt
               (Surface.Ast.Data (Mark.remove t), Mark.get t))
           tl),
      typ_pos )
  | Surface.Ast.Data (Surface.Ast.Primitive prim) -> (
    match prim with
    | Surface.Ast.Integer -> TLit TInt, typ_pos
    | Surface.Ast.Decimal -> TLit TRat, typ_pos
    | Surface.Ast.Money -> TLit TMoney, typ_pos
    | Surface.Ast.Duration -> TLit TDuration, typ_pos
    | Surface.Ast.Date -> TLit TDate, typ_pos
    | Surface.Ast.Boolean -> TLit TBool, typ_pos
    | Surface.Ast.Position -> TLit TPos, typ_pos
    | Surface.Ast.External name ->
      (* External types will be supported at some point *)
      Message.error ~pos:typ_pos "Unrecognised type name '@{<red>%s@}'" name
    | Surface.Ast.Named ([], (ident, _pos)) -> (
      let path = List.rev rev_path in
      match Ident.Map.find_opt ident ctxt.local.typedefs with
      | Some (TStruct s_uid) ->
        let s_uid = StructName.map_info (fun (_, x) -> path, x) s_uid in
        TStruct s_uid, typ_pos
      | Some (TEnum e_uid) ->
        let e_uid = EnumName.map_info (fun (_, x) -> path, x) e_uid in
        TEnum e_uid, typ_pos
      | Some (TScope (_, scope_str)) ->
        let s_uid =
          StructName.map_info (fun (_, x) -> path, x) scope_str.out_struct_name
        in
        TStruct s_uid, typ_pos
      | None ->
        Message.error ~pos:typ_pos
          "Unknown type @{<yellow>\"%s\"@}, not a struct or enum previously \
           declared"
          ident)
    | Surface.Ast.Named ((modul, mpos) :: path, id) -> (
      match Ident.Map.find_opt modul ctxt.local.used_modules with
      | None ->
        Message.error ~pos:mpos
          "This refers to module @{<blue>%s@}, which was not found" modul
      | Some mname ->
        let mod_ctxt = ModuleName.Map.find mname ctxt.modules in
        let rev_path : Uid.Path.t =
          match mod_ctxt.current_revpath with
          | [] -> rev_path
          | mname :: _ ->
            ModuleName.map_info (fun (s, _) -> s, mpos) mname :: rev_path
        in
        process_base_typ ~rev_path ~vars
          { ctxt with local = mod_ctxt }
          Surface.Ast.(Data (Primitive (Named (path, id))), typ_pos))
    | Surface.Ast.Var None ->
      Message.error ~pos:typ_pos
        "Specifying type `anything` is not allowed at this point"
    | Surface.Ast.Var (Some (id, pos)) -> (
      match String.Map.find_opt id vars with
      | Some (v, pos) -> TVar v, pos
      | None ->
        Message.error ~pos
          "Specifying type `anything` is not allowed at this point"))

let process_base_typ ~vars (ctxt : context) ty =
  process_base_typ ~rev_path:ctxt.local.current_revpath ~vars ctxt ty

(** Process a type (function or not) *)
let process_type (ctxt : context) ((naked_typ, typ_pos) : Surface.Ast.typ) : typ
    =
  match naked_typ with
  | Surface.Ast.Base base_typ ->
    process_base_typ ~vars:String.Map.empty ctxt (base_typ, typ_pos)
  | Surface.Ast.Func { arg_typ; return_typ } ->
    let rec get_vars ((count, vars) as acc) ty =
      match ty with
      | Surface.Ast.Primitive (Var (Some (name, npos))), pos ->
        if Ident.Map.mem name vars then acc, ty
        else
          let var = Bindlib.new_var (fun v -> TVar v) name in
          ( (count, Ident.Map.add name (var, npos) vars),
            (Surface.Ast.Primitive (Var (Some (name, npos))), pos) )
      | Surface.Ast.Primitive (Var None), pos ->
        let name = Printf.sprintf "'%d" count in
        (* note these idents can't conflict with the user-supplied ones *)
        let var = Bindlib.new_var (fun v -> TVar v) name in
        ( (count + 1, Ident.Map.add name (var, pos) vars),
          (Surface.Ast.Primitive (Var (Some (name, pos))), pos) )
      | Surface.Ast.Collection ty, pos ->
        let acc, ty = get_vars acc ty in
        acc, (Surface.Ast.Collection ty, pos)
      | Surface.Ast.TTuple ls, pos ->
        let acc, ls = List.fold_left_map get_vars acc ls in
        acc, (Surface.Ast.TTuple ls, pos)
      | Surface.Ast.Option ty, pos ->
        let acc, ty = get_vars acc ty in
        acc, (Surface.Ast.Option ty, pos)
      | Surface.Ast.Primitive _, _ -> acc, ty
    in
    let get_vars_base acc = function
      | Surface.Ast.Condition, pos -> acc, (Surface.Ast.Condition, pos)
      | Surface.Ast.Data ty, pos ->
        let acc, (ty, pos) = get_vars acc (ty, pos) in
        acc, (Surface.Ast.Data ty, pos)
    in
    let _arg_names, arg_ty = List.split arg_typ in
    let acc, return_typ = get_vars_base (1, Ident.Map.empty) return_typ in
    let (_, vars), arg_typ = List.fold_left_map get_vars_base acc arg_ty in
    let targs = List.map (process_base_typ ~vars ctxt) arg_typ in
    let ty = TArrow (targs, process_base_typ ~vars ctxt return_typ), typ_pos in
    if Ident.Map.is_empty vars then ty
    else
      let ty = Type.rebox ty in
      let vars =
        Ident.Map.values vars |> List.map Mark.remove |> Array.of_list
      in
      TForAll Bindlib.(unbox (bind_mvar vars ty)), typ_pos

(** Process data declaration *)
let process_data_decl
    (scope : ScopeName.t)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl_context_data) : context =
  (* First check the type of the context data *)
  let data_typ = process_type ctxt decl.scope_decl_context_item_typ in
  let is_cond = is_type_cond decl.scope_decl_context_item_typ in
  let name, pos = decl.scope_decl_context_item_name in
  let pos = translate_pos ScopeDef pos in
  let scope_ctxt = get_scope_context ctxt scope in
  match Ident.Map.find_opt name scope_ctxt.var_idmap with
  | Some use ->
    let info =
      match use with
      | ScopeVar v -> ScopeVar.get_info v
      | SubScope (ssc, _) -> ScopeVar.get_info ssc
    in
    Message.error
      ~extra_pos:["First use:", Mark.get info; "Second use:", pos]
      "Variable name @{<yellow>\"%s\"@} already used" name
  | None ->
    let uid = ScopeVar.fresh (name, pos) in
    let scope_ctxt =
      {
        scope_ctxt with
        var_idmap = Ident.Map.add name (ScopeVar uid) scope_ctxt.var_idmap;
      }
    in
    let states_idmap, states_list =
      List.fold_right
        (fun state_id ((states_idmap : StateName.t Ident.Map.t), states_list) ->
          let state_id_name = Mark.remove state_id in
          if Ident.Map.mem state_id_name states_idmap then
            Message.error
              ~fmt_pos:
                [
                  ( (fun ppf ->
                      Format.fprintf ppf
                        "First instance of state @{<yellow>\"%s\"@}:"
                        state_id_name),
                    Mark.get state_id );
                  ( (fun ppf ->
                      Format.fprintf ppf
                        "Second instance of state @{<yellow>\"%s\"@}:"
                        state_id_name),
                    Mark.get
                      (Ident.Map.find state_id_name states_idmap
                      |> StateName.get_info) );
                ]
              "%a" Format.pp_print_text
              "There are two states with the same name for the same variable: \
               this is ambiguous. Please change the name of either states.";
          let state_uid = StateName.fresh state_id in
          ( Ident.Map.add state_id_name state_uid states_idmap,
            state_uid :: states_list ))
        decl.scope_decl_context_item_states (Ident.Map.empty, [])
    in
    let var_sig_parameters =
      Option.map
        (Mark.map (List.map (fun (lbl, typ) -> lbl, process_type ctxt typ)))
        decl.scope_decl_context_item_parameters
    in
    {
      ctxt with
      scopes = ScopeName.Map.add scope scope_ctxt ctxt.scopes;
      var_typs =
        ScopeVar.Map.add uid
          {
            var_sig_typ = data_typ;
            var_sig_is_condition = is_cond;
            var_sig_parameters;
            var_sig_io = decl.scope_decl_context_item_attribute;
            var_sig_states_idmap = states_idmap;
            var_sig_states_list = states_list;
          }
          ctxt.var_typs;
    }

(** Process a struct declaration *)
let process_struct_decl
    ?(visibility = Public)
    (ctxt : context)
    (sdecl : Surface.Ast.struct_decl) : context =
  let s_uid = get_struct ctxt sdecl.struct_decl_name in
  if sdecl.struct_decl_fields = [] then
    Message.error
      ~pos:(Mark.get sdecl.struct_decl_name)
      "The struct@ %s@ does@ not@ have@ any@ fields;@ give it some for Catala \
       to be able to accept it."
      (Mark.remove sdecl.struct_decl_name);
  List.fold_left
    (fun ctxt (fdecl, _) ->
      let name, pos = fdecl.Surface.Ast.struct_decl_field_name in
      let pos = translate_pos FieldDecl pos in
      let f_uid = StructField.fresh (name, pos) in
      let local =
        {
          ctxt.local with
          field_idmap =
            Ident.Map.update
              (Mark.remove fdecl.Surface.Ast.struct_decl_field_name)
              (fun uids ->
                match uids with
                | None -> Some (StructName.Map.singleton s_uid f_uid)
                | Some uids -> Some (StructName.Map.add s_uid f_uid uids))
              ctxt.local.field_idmap;
        }
      in
      let ctxt = { ctxt with local } in
      let structs =
        StructName.Map.update s_uid
          (function
            | None ->
              Some
                ( StructField.Map.singleton f_uid
                    (process_type ctxt fdecl.Surface.Ast.struct_decl_field_typ),
                  visibility )
            | Some (fields, _) ->
              Some
                ( StructField.Map.add f_uid
                    (process_type ctxt fdecl.Surface.Ast.struct_decl_field_typ)
                    fields,
                  visibility ))
          ctxt.structs
      in
      { ctxt with structs })
    ctxt sdecl.struct_decl_fields

(** Process an enum declaration *)
let process_enum_decl
    ?(visibility = Public)
    (ctxt : context)
    (edecl : Surface.Ast.enum_decl) : context =
  let e_uid = get_enum ctxt edecl.enum_decl_name in
  if edecl.enum_decl_cases = [] then
    Message.error
      ~pos:(Mark.get edecl.enum_decl_name)
      "The enum@ %s@ does@ not@ have@ any@ cases;@ give it some for Catala to \
       be able to accept it."
      (Mark.remove edecl.enum_decl_name);
  List.fold_left
    (fun ctxt (cdecl, cdecl_pos) ->
      let name, pos = cdecl.Surface.Ast.enum_decl_case_name in
      let pos = translate_pos ConstructorDecl pos in
      let c_uid = EnumConstructor.fresh (name, pos) in
      let local =
        {
          ctxt.local with
          constructor_idmap =
            Ident.Map.update
              (Mark.remove cdecl.Surface.Ast.enum_decl_case_name)
              (fun uids ->
                match uids with
                | None -> Some (EnumName.Map.singleton e_uid c_uid)
                | Some uids -> Some (EnumName.Map.add e_uid c_uid uids))
              ctxt.local.constructor_idmap;
        }
      in
      let ctxt = { ctxt with local } in
      let enums =
        EnumName.Map.update e_uid
          (fun cases ->
            let typ =
              match cdecl.Surface.Ast.enum_decl_case_typ with
              | None -> TLit TUnit, cdecl_pos
              | Some typ -> process_type ctxt typ
            in
            match cases with
            | None -> Some (EnumConstructor.Map.singleton c_uid typ, visibility)
            | Some (fields, _) ->
              Some (EnumConstructor.Map.add c_uid typ fields, visibility))
          ctxt.enums
      in
      { ctxt with enums })
    ctxt edecl.enum_decl_cases

let process_topdef ?(visibility = Public) ctxt def =
  let uid =
    Ident.Map.find (Mark.remove def.Surface.Ast.topdef_name) ctxt.local.topdefs
  in
  let ty = process_type ctxt def.Surface.Ast.topdef_type in
  let ty =
    match ty, def.Surface.Ast.topdef_args with
    | (TArrow (targs, tret), pos), Some (argnames, _) ->
      ( TArrow
          ( List.map2
              (fun ty ((_, npos), _) ->
                if
                  Pos.has_attr
                    (translate_pos FunctionArgument npos)
                    ImplicitPosArg
                then
                  Mark.map_mark (fun pos -> Pos.add_attr pos ImplicitPosArg) ty
                else ty)
              targs argnames,
            tret ),
        pos )
    | ty, _ -> ty
  in
  {
    ctxt with
    topdefs =
      TopdefName.Map.update uid
        (fun prev_def ->
          let visibility =
            match prev_def, visibility with
            | Some (_, Private), Private | None, Private -> Private
            | Some (_, Public), _ | _, Public -> Public
          in
          Some (ty, visibility))
        ctxt.topdefs;
  }

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
let process_scope_decl
    ?(visibility = Public)
    (ctxt : context)
    (decl : Surface.Ast.scope_decl) : context =
  let scope_uid = get_scope ctxt decl.scope_decl_name in
  let ctxt =
    List.fold_left
      (fun ctxt item -> process_item_decl scope_uid ctxt (Mark.remove item))
      ctxt decl.scope_decl_context
  in
  (* Add an implicit struct def for the scope output type *)
  let output_fields =
    List.fold_right
      (fun item acc ->
        match Mark.remove item with
        | Surface.Ast.ContextData
            ({
               scope_decl_context_item_attribute =
                 { scope_decl_context_io_output = true, _; _ };
               _;
             } as data) ->
          Mark.add (Mark.get item)
            {
              Surface.Ast.struct_decl_field_name =
                data.scope_decl_context_item_name;
              Surface.Ast.struct_decl_field_typ =
                data.scope_decl_context_item_typ;
            }
          :: acc
        | Surface.Ast.ContextScope
            {
              scope_decl_context_scope_name = var;
              scope_decl_context_scope_sub_scope = (path, scope), pos;
              scope_decl_context_scope_attribute =
                { scope_decl_context_io_output = true, _; _ };
            } ->
          Mark.add (Mark.get item)
            {
              Surface.Ast.struct_decl_field_name = var;
              Surface.Ast.struct_decl_field_typ =
                Base (Data (Primitive (Named (path, scope)))), pos;
            }
          :: acc
        | _ -> acc)
      decl.scope_decl_context []
  in
  let visibility =
    if Pos.has_attr (Mark.get (ScopeName.get_info scope_uid)) Test then Public
    else visibility
  in
  if output_fields = [] then
    (* we allow scopes without output variables, and still define their (empty)
       output struct for convenience *)
    {
      ctxt with
      structs =
        StructName.Map.add
          (get_struct ctxt decl.scope_decl_name)
          (StructField.Map.empty, visibility)
          ctxt.structs;
    }
  else
    let ctxt =
      process_struct_decl ~visibility ctxt
        {
          struct_decl_name = decl.scope_decl_name;
          struct_decl_fields = output_fields;
        }
    in
    let out_struct_fields =
      let sco = get_scope_context ctxt scope_uid in
      let str = get_struct ctxt decl.scope_decl_name in
      Ident.Map.fold
        (fun id var svmap ->
          match var with
          | ScopeVar v | SubScope (v, _) ->
            let is_output = (get_var_io ctxt v).scope_decl_context_io_output in
            if Mark.remove is_output then
              try
                let field =
                  StructName.Map.find str
                    (Ident.Map.find id ctxt.local.field_idmap)
                in
                ScopeVar.Map.add v field svmap
              with StructName.Map.Not_found _ | Ident.Map.Not_found _ -> svmap
            else svmap)
        sco.var_idmap ScopeVar.Map.empty
    in
    let typedefs =
      Ident.Map.update
        (Mark.remove decl.scope_decl_name)
        (function
          | Some
              (TScope
                (scope, { in_struct_name; out_struct_name; visibility; _ })) ->
            Some
              (TScope
                 ( scope,
                   {
                     in_struct_name;
                     out_struct_name;
                     out_struct_fields;
                     visibility;
                   } ))
          | _ -> assert false)
        ctxt.local.typedefs
    in
    { ctxt with local = { ctxt.local with typedefs } }

let typedef_info = function
  | TStruct t -> StructName.get_info t
  | TEnum t -> EnumName.get_info t
  | TScope (s, _) -> ScopeName.get_info s

(** Process the names of all declaration items *)
let process_name_item
    (ctxt : context)
    (item_vis : Surface.Ast.code_item Mark.pos * visibility) : context =
  let raise_already_defined_error (use : Uid.MarkedString.info) name pos msg =
    Message.error
      ~fmt_pos:
        [
          ( (fun ppf -> Format.pp_print_string ppf "First definition:"),
            Mark.get use );
          (fun ppf -> Format.pp_print_string ppf "Second definition:"), pos;
        ]
      "%s name @{<yellow>\"%s\"@} already defined" msg name
  in
  let raise_forbidden_in_external_error pos =
    Message.error ~pos "Scopes are forbidden in external modules."
  in
  let is_external = ctxt.local.is_external in
  let path = List.rev ctxt.local.current_revpath in
  let item, visibility = item_vis in
  match Mark.remove item with
  | ScopeDecl decl ->
    let name, pos = decl.scope_decl_name in
    let pos = translate_pos ScopeDecl pos in
    if is_external then raise_forbidden_in_external_error pos;
    (* Checks if the name is already used *)
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "scope")
      (Ident.Map.find_opt name ctxt.local.typedefs);
    let scope_uid = ScopeName.fresh path (name, pos) in
    let in_struct_name = StructName.fresh path (name ^ "_in", pos) in
    let out_struct_name = StructName.fresh path (name, pos) in
    let visibility = if Pos.has_attr pos Test then Public else visibility in
    let typedefs =
      Ident.Map.add name
        (TScope
           ( scope_uid,
             {
               in_struct_name;
               out_struct_name;
               out_struct_fields = ScopeVar.Map.empty;
               visibility;
             } ))
        ctxt.local.typedefs
    in
    let scopes =
      ScopeName.Map.add scope_uid
        {
          var_idmap = Ident.Map.empty;
          scope_defs_contexts = Ast.ScopeDef.Map.empty;
          scope_in_struct = in_struct_name;
          scope_out_struct = out_struct_name;
          sub_scopes = ScopeName.Set.empty;
          scope_visibility = visibility;
        }
        ctxt.scopes
    in
    { ctxt with local = { ctxt.local with typedefs }; scopes }
  | StructDecl sdecl ->
    let name, pos = sdecl.struct_decl_name in
    let pos = translate_pos StructDecl pos in
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "struct")
      (Ident.Map.find_opt name ctxt.local.typedefs);
    let s_uid = StructName.fresh path sdecl.struct_decl_name in
    let typedefs =
      Ident.Map.add
        (Mark.remove sdecl.struct_decl_name)
        (TStruct s_uid) ctxt.local.typedefs
    in
    { ctxt with local = { ctxt.local with typedefs } }
  | EnumDecl edecl ->
    let name, pos = edecl.enum_decl_name in
    let pos = translate_pos EnumDecl pos in
    Option.iter
      (fun use ->
        raise_already_defined_error (typedef_info use) name pos "enum")
      (Ident.Map.find_opt name ctxt.local.typedefs);
    let e_uid = EnumName.fresh path (name, pos) in
    let typedefs =
      Ident.Map.add
        (Mark.remove edecl.enum_decl_name)
        (TEnum e_uid) ctxt.local.typedefs
    in
    { ctxt with local = { ctxt.local with typedefs } }
  | ScopeUse s_use ->
    if is_external then
      raise_forbidden_in_external_error (Mark.get s_use.scope_use_name);
    ctxt
  | Topdef def ->
    let name, pos = def.topdef_name in
    let pos = translate_pos Topdef pos in
    let uid =
      match Ident.Map.find_opt name ctxt.local.topdefs with
      | None -> TopdefName.fresh path (name, pos)
      | Some uid -> uid
      (* Topdef declaration may appear multiple times as long as their types
         match and only one contains an expression defining it *)
    in
    let topdefs = Ident.Map.add name uid ctxt.local.topdefs in
    { ctxt with local = { ctxt.local with topdefs } }

(** Process a code item that is a declaration *)
let process_decl_item
    (ctxt : context)
    ((item, visibility) : Surface.Ast.code_item Mark.pos * visibility) : context
    =
  match Mark.remove item with
  | ScopeDecl decl -> process_scope_decl ~visibility ctxt decl
  | StructDecl sdecl -> process_struct_decl ~visibility ctxt sdecl
  | EnumDecl edecl -> process_enum_decl ~visibility ctxt edecl
  | ScopeUse _ -> ctxt
  | Topdef def -> process_topdef ~visibility ctxt def

(** Process a code block *)
let process_code_block
    (visibility : visibility)
    (process_item :
      context -> Surface.Ast.code_item Mark.pos * visibility -> context)
    (ctxt : context)
    (block : Surface.Ast.code_block) : context =
  List.fold_left
    (fun ctxt decl -> process_item ctxt (decl, visibility))
    ctxt block

(** Process a law structure, only considering the code blocks *)
let rec process_law_structure
    (process_item :
      context -> Surface.Ast.code_item Mark.pos * visibility -> context)
    (ctxt : context)
    (s : Surface.Ast.law_structure) : context =
  match s with
  | Surface.Ast.LawHeading (_, children) ->
    List.fold_left
      (fun ctxt child -> process_law_structure process_item ctxt child)
      ctxt children
  | Surface.Ast.CodeBlock (block, _, is_meta) ->
    process_code_block
      (if is_meta then Public else Private)
      process_item ctxt block
  | Surface.Ast.ModuleDef (_, is_external) ->
    { ctxt with local = { ctxt.local with is_external } }
  | Surface.Ast.LawInclude _ | Surface.Ast.LawText _ -> ctxt
  | Surface.Ast.ModuleUse _ -> ctxt

(** {1 Scope uses pass} *)

let get_def_key
    (name : Surface.Ast.scope_var)
    (state : Surface.Ast.lident Mark.pos option)
    (scope_uid : ScopeName.t)
    (ctxt : context)
    (pos : Pos.t) : Ast.ScopeDef.t =
  let scope_ctxt = ScopeName.Map.find scope_uid ctxt.scopes in
  match name with
  | [x] ->
    let x_uid = get_var_uid scope_uid ctxt x in
    let var_sig = ScopeVar.Map.find x_uid ctxt.var_typs in
    ( (x_uid, pos),
      Ast.ScopeDef.Var
        (match state with
        | Some state -> (
          try
            Some
              (Ident.Map.find (Mark.remove state) var_sig.var_sig_states_idmap)
          with Ident.Map.Not_found _ ->
            Message.error
              ~extra_pos:
                [
                  "", Mark.get state;
                  "Variable declaration:", Mark.get (ScopeVar.get_info x_uid);
                ]
              "This identifier is not a state declared for variable@ %a."
              ScopeVar.format x_uid)
        | None ->
          if not (Ident.Map.is_empty var_sig.var_sig_states_idmap) then
            Message.error
              ~extra_pos:
                [
                  "", Mark.get x;
                  "Variable declaration:", Mark.get (ScopeVar.get_info x_uid);
                ]
              "This definition does not indicate which state has to@ be@ \
               considered@ for@ variable@ %a."
              ScopeVar.format x_uid
          else None) )
  | [y; x] ->
    let (subscope_var, name) : ScopeVar.t * ScopeName.t =
      match Ident.Map.find_opt (Mark.remove y) scope_ctxt.var_idmap with
      | Some (SubScope (v, u)) -> v, u
      | Some _ ->
        Message.error ~pos "Invalid definition,@ %a@ is@ not@ a@ subscope"
          Print.lit_style (Mark.remove y)
      | None ->
        Message.error ~pos "No definition found for subscope@ %a"
          Print.lit_style (Mark.remove y)
    in
    let var_within_origin_scope = get_var_uid name ctxt x in
    ( (subscope_var, pos),
      Ast.ScopeDef.SubScopeInput { name; var_within_origin_scope } )
  | _ ->
    Message.error ~pos "%a" Format.pp_print_text
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
        Ident.Map.update (Mark.remove label)
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
               ([Mark.get d.definition_name]
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
            Some (Ambiguous [Mark.get d.definition_name]);
        }
      (* This is a possible default definition for this key. We create and store
         a fresh rulename *)
      | None ->
        {
          def_key_ctx with
          default_exception_rulename =
            Some (Unique (d.definition_id, Mark.get d.definition_name));
        }))

let empty_def_key_ctx =
  {
    (* Here, this is the first time we encounter a definition for this
       definition key *)
    default_exception_rulename = None;
    label_idmap = Ident.Map.empty;
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
              (Mark.remove d.definition_name)
              d.definition_state s_name ctxt
              (Mark.get d.definition_name)
          in
          match s_ctxt with
          | None -> assert false (* should not happen *)
          | Some s_ctxt ->
            Some
              {
                s_ctxt with
                scope_defs_contexts =
                  Ast.ScopeDef.Map.update def_key
                    (fun def_key_ctx ->
                      Some
                        (update_def_key_ctx d
                           (Option.value ~default:empty_def_key_ctx def_key_ctx)))
                    s_ctxt.scope_defs_contexts;
              })
        ctxt.scopes;
  }

(** Translates a {!type: rule} into the corresponding {!type: definition} *)
let surface_rule_to_def (rule : Surface.Ast.rule) : Surface.Ast.definition =
  let consequence_expr =
    Surface.Ast.Literal (LBool (Mark.remove rule.rule_consequence))
  in
  {
    definition_label = rule.rule_label;
    definition_exception_to = rule.rule_exception_to;
    definition_name = rule.rule_name;
    definition_parameter = rule.rule_parameter;
    definition_condition = rule.rule_condition;
    definition_id = rule.rule_id;
    definition_expr = consequence_expr, Mark.get rule.rule_consequence;
    definition_state = rule.rule_state;
  }

let process_scope_use_item
    (s_name : ScopeName.t)
    (ctxt : context)
    (sitem : Surface.Ast.scope_use_item Mark.pos) : context =
  match Mark.remove sitem with
  | Rule r -> process_definition ctxt s_name (surface_rule_to_def r)
  | Definition d -> process_definition ctxt s_name d
  | _ -> ctxt

let process_scope_use (ctxt : context) (suse : Surface.Ast.scope_use) : context
    =
  let s_name =
    match
      Ident.Map.find_opt
        (Mark.remove suse.Surface.Ast.scope_use_name)
        ctxt.local.typedefs
    with
    | Some (TScope (sn, _)) -> sn
    | _ ->
      Message.error
        ~pos:(Mark.get suse.Surface.Ast.scope_use_name)
        "@{<yellow>\"%s\"@}: this scope has not been declared anywhere, is it \
         a typo?"
        (Mark.remove suse.Surface.Ast.scope_use_name)
  in
  List.fold_left
    (process_scope_use_item s_name)
    ctxt suse.Surface.Ast.scope_use_items

let process_use_item
    (ctxt : context)
    ((item, _) : Surface.Ast.code_item Mark.pos * visibility) : context =
  match Mark.remove item with
  | ScopeDecl _ | StructDecl _ | EnumDecl _ | Topdef _ -> ctxt
  | ScopeUse suse -> (
    let pos = Mark.get suse.scope_use_name in
    match
      Pos.get_attrs pos (function Src (_, _, pos) -> Some pos | _ -> None)
    with
    | pos :: _ ->
      Message.error ~pos "%a" Format.pp_print_text
        "No attributes are allowed at this point. They should be put in front \
         of the scope declaration."
    | [] -> process_scope_use ctxt suse)

(** {1 API} *)

let empty_module_ctxt lang =
  {
    current_revpath = [];
    typedefs = Ident.Map.empty;
    field_idmap = Ident.Map.empty;
    constructor_idmap =
      (let present = EnumName.Map.singleton Expr.option_enum Expr.some_constr in
       let absent = EnumName.Map.singleton Expr.option_enum Expr.none_constr in
       Ident.Map.of_list
         (match lang with
         | Global.En -> ["Present", present; "Absent", absent]
         | Global.Fr -> ["Présent", present; "Absent", absent]
         | Global.Pl -> ["Obecny", present; "Nieobecny", absent]
         | Global.Ro -> ["Prezent", present; "Absent", absent]));
    topdefs = Ident.Map.empty;
    used_modules = Ident.Map.empty;
    is_external = false;
  }

let empty_ctxt lang =
  {
    scopes = ScopeName.Map.empty;
    topdefs = TopdefName.Map.empty;
    var_typs = ScopeVar.Map.empty;
    structs = StructName.Map.empty;
    enums =
      EnumName.Map.singleton Expr.option_enum (Expr.option_enum_config, Private);
    modules = ModuleName.Map.empty;
    local = empty_module_ctxt lang;
  }

(** Derive the context from metadata, in one pass over the declarations *)
let form_context (surface, mod_uses) surface_modules : context =
  (* Gather struct fields and enum constrs from direct modules: this helps with
     disambiguation. *)
  let gather_struct_fields_ids ctxt modul_ctxt =
    let constructor_idmap, field_idmap =
      Ident.Map.fold
        (fun _ m_name (cmap, fmap) ->
          let lctx = ModuleName.Map.find m_name ctxt.modules in
          let cmap =
            Ident.Map.union
              (fun _ enu1 enu2 ->
                Some (EnumName.Map.union (fun _ e _ -> Some e) enu1 enu2))
              cmap lctx.constructor_idmap
          in
          let fmap =
            Ident.Map.union
              (fun _ str1 str2 ->
                Some (StructName.Map.union (fun _ s _ -> Some s) str1 str2))
              fmap lctx.field_idmap
          in
          cmap, fmap)
        modul_ctxt.used_modules
        (modul_ctxt.constructor_idmap, modul_ctxt.field_idmap)
    in
    { modul_ctxt with constructor_idmap; field_idmap }
  in
  let empty_ctxt = empty_ctxt surface.Surface.Ast.program_lang in
  let empty_module_ctxt = empty_ctxt.local in
  let rec process_modules ctxt revpath mod_uses =
    (* Recursing on [mod_uses] rather than folding on [modules] ensures a
       topological traversal. *)
    Ident.Map.fold
      (fun _alias m ctxt ->
        match ModuleName.Map.find_opt m ctxt.modules with
        | Some _ -> ctxt
        | None ->
          let module_content, mod_uses =
            ModuleName.Map.find m surface_modules
          in
          let revpath = m :: revpath in
          let ctxt = process_modules ctxt revpath mod_uses in
          let ctxt =
            {
              ctxt with
              local =
                {
                  ctxt.local with
                  used_modules = mod_uses;
                  current_revpath = revpath;
                  is_external =
                    module_content.Surface.Ast.module_modname.module_external;
                };
            }
          in
          let ctxt =
            match module_content.Surface.Ast.module_items with
            | Interface decl_items ->
              let ctxt = List.fold_left process_name_item ctxt decl_items in
              List.fold_left process_decl_item ctxt decl_items
            | Code module_items ->
              (* Whole-program *)
              let ctxt =
                { ctxt with local = gather_struct_fields_ids ctxt ctxt.local }
              in
              let ctxt =
                List.fold_left
                  (process_law_structure process_name_item)
                  ctxt module_items
              in
              let ctxt =
                List.fold_left
                  (process_law_structure process_decl_item)
                  ctxt module_items
              in
              List.fold_left
                (process_law_structure process_use_item)
                ctxt module_items
          in
          {
            ctxt with
            modules = ModuleName.Map.add m ctxt.local ctxt.modules;
            local = empty_module_ctxt;
          })
      mod_uses ctxt
  in
  let ctxt = process_modules empty_ctxt [] mod_uses in
  let ctxt =
    { ctxt with local = { empty_module_ctxt with used_modules = mod_uses } }
  in
  let ctxt =
    List.fold_left
      (process_law_structure process_name_item)
      ctxt surface.Surface.Ast.program_items
  in
  let ctxt =
    List.fold_left
      (process_law_structure process_decl_item)
      ctxt surface.Surface.Ast.program_items
  in
  let ctxt =
    List.fold_left
      (process_law_structure process_use_item)
      ctxt surface.Surface.Ast.program_items
  in
  let ctxt = { ctxt with local = gather_struct_fields_ids ctxt ctxt.local } in
  ctxt
