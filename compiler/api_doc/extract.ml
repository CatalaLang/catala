(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Arthur Carcano <arthur.carcano@ocamlpro.com>

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
module SurfAst = Surface.Ast
module NameRes = Desugared.Name_resolution

(* The types below are documented in [extract.mli] *)

type target = { target_module : ModuleName.t option; target_anchor : string }

type typ =
  | Builtin of SurfAst.primitive_typ
  | Named of { name : string; target : target option }
  | List of typ
  | Option of typ
  | Tuple of typ list

type base_typ = Condition | Data of typ
type arg = { arg_name : string; arg_typ : base_typ; arg_doc : string option }
type signature = { sig_typ : base_typ; sig_args : arg list }

type 'a member = {
  member_name : string;
  member_decl : 'a;
  member_doc : string option;
  member_anchor : string;
}

type scope_member =
  | Var of { input : SurfAst.io_input; output : bool; var_sig : signature }
  | Subscope of { output : bool; scope : typ }

type item_kind =
  | Struct of signature member list
  | Enum of base_typ option member list
  | Scope of scope_member member list
  | Constant of base_typ
  | Function of { return_typ : base_typ; args : base_typ member list }
  | Abstract

type item = {
  item_name : string;
  item_kind : item_kind;
  item_doc : string option;
  item_anchor : string;
}

type module_use = { use_module : ModuleName.t; use_alias : string }

type module_doc = {
  module_name : string;
  module_is_external : bool;
  module_uses : module_use list;
  module_items : item list;
}

(** Docstrings are attached by the parser to the position of the declared name,
    as one [Src_doc] attribute per [##] line or [#[doc]] attribute: their texts
    are joined in source order with newlines. Positions of the surface AST are
    not translated, so they hold no [Doc]; a [#[doc]] whose value is not a
    string is reported by name resolution, and ignored here. *)
let get_doc (pos : Pos.t) : string option =
  Pos.get_attrs_sorted pos (function
    | Src_doc (_, String (s, _), pos) -> Some (pos, s)
    | _ -> None)
  |> function [] -> None | l -> Some (String.trim (String.concat "\n" l))

(** Whether the declaration at [pos] is marked as a test with [#[test]] *)
let is_test (pos : Pos.t) : bool =
  (* Surface positions are not translated by name resolution, so [#[test]] is
     still a [Src] attribute here, not [Test]. Ignore it
     if it has a value (compiler gives a warning). *)
  Pos.get_attrs pos (function
    | Src ((["test"], _), Unit, _) -> Some ()
    | _ -> None)
  <> []

(** The HTML anchor of an item: its name, prefixed by its kind so that items of
    different kinds with the same name don't collide. Constants and functions
    share the [`Topdef] prefix. *)
let anchor
    (kind : [ `Struct | `Enum | `Scope | `Type | `Topdef ])
    (name : string) : string =
  let prefix =
    match kind with
    | `Struct -> "struct-"
    | `Enum -> "enum-"
    | `Scope -> "scope-"
    | `Type -> "type-"
    | `Topdef -> "topdef-"
  in
  prefix ^ name

(** [anchor], for a declaration found by name resolution *)
let typedef_anchor (td : NameRes.typedef) : string -> string =
  match td with
  | TStruct _ -> anchor `Struct
  | TEnum _ -> anchor `Enum
  | TAbstract _ -> anchor `Type
  | TScope _ -> anchor `Scope

(** The declaration that the type [name], qualified by the module [path], refers
    to, as a link target. [path] may use module aliases. Returns [None] if no
    such type exists, which name resolution has already reported. *)
let resolve (ctx : NameRes.context) (path : SurfAst.path) (name : string) :
    target option =
  let mpath, mctx = NameRes.module_ctx ctx path in
  Ident.Map.find_opt name mctx.local.typedefs
  |> Option.map (fun td ->
      {
        target_module = Uid.Path.last_member mpath;
        target_anchor = typedef_anchor td name;
      })

(** Converts a type, as written in the source, to be printed in the
    documentation. Its structure and names are kept as the author wrote them,
    including module aliases, but each named type is also resolved to the
    declaration it refers to, so that it can be linked.

    Only called on public declarations: a private type of the module found here
    is used by the public interface, but is not documented, so a warning is
    emitted. *)
let rec typ ctx : SurfAst.base_typ_data -> typ = function
  | Primitive (Named (path, (name, pos))) ->
    let target = resolve ctx path name in
    (match target with
    | Some { target_module = None; _ }
      when not (NameRes.tdef_is_public ctx name) ->
      Message.warning ~pos
        "@[<hov>%s@ is@ private,@ but@ the@ public@ interface@ uses@ it:@ it@ \
         is@ not@ documented,@ and@ other@ modules@ cannot@ name@ it.@ \
         Declare@ it@ in@ a@ @{<yellow;bold>```catala-metadata@}@ block@ to@ \
         make@ it@ public.@]"
        name
    | _ -> ());
    Named
      { name = String.concat "." (List.map Mark.remove path @ [name]); target }
  | Primitive p -> Builtin p
  | Collection t -> List (typ ctx (Mark.remove t))
  | Option t -> Option (typ ctx (Mark.remove t))
  | TTuple ts -> Tuple (List.map (fun t -> typ ctx (Mark.remove t)) ts)

(** [typ], for a type that can also be [condition] *)
let base_typ ctx : SurfAst.base_typ -> base_typ = function
  | Condition -> Condition
  | Data t -> Data (typ ctx t)

(** The type of a structure field or scope variable. For a function
    ([depends on]), this is its return type, and its arguments with their names,
    types and docstrings; otherwise, the arguments are empty. *)
let signature ctx (t : SurfAst.typ) : signature =
  match Mark.remove t with
  | Base t -> { sig_typ = base_typ ctx t; sig_args = [] }
  | Func { arg_typ; return_typ } ->
    {
      sig_typ = base_typ ctx (Mark.remove return_typ);
      sig_args =
        List.map
          (fun ((arg_name, pos), t) ->
            {
              arg_name;
              arg_typ = base_typ ctx (Mark.remove t);
              arg_doc = get_doc pos;
            })
          arg_typ;
    }

(** The documentation of a public declaration, or [None] if it is not
    documented. Each named type in it is resolved, which emits a warning if it
    is private. *)
let item ctx (code_item : SurfAst.code_item) : item option =
  (* Docstrings are attached by the parser to the position of each name *)
  let mk ((name, pos) : string Mark.pos) item_anchor item_kind : item =
    { item_name = name; item_kind; item_doc = get_doc pos; item_anchor }
  in
  (* Members are anchored under their item, e.g. [struct-S.field] *)
  let member item_anchor (((name, pos) : string Mark.pos), member_decl) =
    {
      member_name = name;
      member_decl;
      member_doc = get_doc pos;
      member_anchor = item_anchor ^ "." ^ name;
    }
  in
  match code_item with
  | StructDecl { struct_decl_name; struct_decl_fields } ->
    let anchor = anchor `Struct (Mark.remove struct_decl_name) in
    (* A field can be function-typed: [signature] keeps its arguments *)
    let field (f, _) =
      member anchor
        ( f.SurfAst.struct_decl_field_name,
          signature ctx f.SurfAst.struct_decl_field_typ )
    in
    let fields = List.map field struct_decl_fields in
    Some (mk struct_decl_name anchor (Struct fields))
  | EnumDecl { enum_decl_name; enum_decl_cases } ->
    let anchor = anchor `Enum (Mark.remove enum_decl_name) in
    (* The type carried by a case, if any *)
    let payload = function
      | None -> None
      | Some (SurfAst.Base t, _) -> Some (base_typ ctx t)
      | Some (SurfAst.Func _, _) -> assert false (* not allowed by the parser *)
    in
    let case (c, _) =
      member anchor
        (c.SurfAst.enum_decl_case_name, payload c.SurfAst.enum_decl_case_typ)
    in
    let cases = List.map case enum_decl_cases in
    Some (mk enum_decl_name anchor (Enum cases))
  | ScopeDecl { scope_decl_name; _ } when is_test (Mark.get scope_decl_name) ->
    (* Test scopes are public, but not part of the API *)
    None
  | ScopeDecl { scope_decl_name; scope_decl_context } ->
    let anchor = anchor `Scope (Mark.remove scope_decl_name) in
    (* Pure [internal] variables are hidden. The parser makes subscopes
       [internal], so only [output] ones are shown. *)
    let is_public (io : SurfAst.scope_decl_context_io) =
      Mark.remove io.scope_decl_context_io_input <> SurfAst.Internal
      || Mark.remove io.scope_decl_context_io_output
    in
    let scope_member (v, _) =
      match v with
      | SurfAst.ContextData d when is_public d.scope_decl_context_item_attribute
        ->
        (* A variable; its states are not shown *)
        let io = d.scope_decl_context_item_attribute in
        let input = Mark.remove io.scope_decl_context_io_input in
        let output = Mark.remove io.scope_decl_context_io_output in
        let var_sig = signature ctx d.scope_decl_context_item_typ in
        Some
          (member anchor
             (d.scope_decl_context_item_name, Var { input; output; var_sig }))
      | SurfAst.ContextScope s
        when is_public s.scope_decl_context_scope_attribute ->
        (* A subscope: its scope is resolved like a named type, to be
           linked *)
        let path, name = Mark.remove s.scope_decl_context_scope_sub_scope in
        let output =
          Mark.remove
            s.scope_decl_context_scope_attribute.scope_decl_context_io_output
        in
        let scope = typ ctx (Primitive (Named (path, name))) in
        Some
          (member anchor
             (s.scope_decl_context_scope_name, Subscope { output; scope }))
      | SurfAst.ContextData _ | SurfAst.ContextScope _ -> None
    in
    let members = List.filter_map scope_member scope_decl_context in
    Some (mk scope_decl_name anchor (Scope members))
  | Topdef { topdef_name; topdef_type; _ } -> (
    let anchor = anchor `Topdef (Mark.remove topdef_name) in
    match Mark.remove topdef_type with
    | Base t -> Some (mk topdef_name anchor (Constant (base_typ ctx t)))
    | Func { arg_typ; return_typ } ->
      (* The arguments of a function are members, each with its own docstring
         and anchor *)
      let arg (name, t) = member anchor (name, base_typ ctx (Mark.remove t)) in
      let args = List.map arg arg_typ in
      let return_typ = base_typ ctx (Mark.remove return_typ) in
      Some (mk topdef_name anchor (Function { return_typ; args })))
  | AbstractTypeDecl name ->
    Some (mk name (anchor `Type (Mark.remove name)) Abstract)
  | ScopeUse _ -> (* Implementation, never documented *) None

(** A topdef can be declared twice: by its signature (without [equals]), and
    separately with its definition. The compiler accepts them in any order and
    in any kind of block, with docstrings on both. [catala doc] only reads
    metadata blocks, where [get_interface] removes definitions, so the two
    declarations can't be told apart there. When both are in metadata blocks,
    the first one is documented, and docstrings on the second one are errors.
    This rule is specific to [catala doc]: to be discussed with the maintainers.

    Desugaring, which runs before, guarantees that there are at most two
    declarations. This reports the docstrings of the second one in metadata
    blocks, [second], given the [first] one, as delayed errors. *)
let check_second_decl ~(first : SurfAst.top_def) ~(second : SurfAst.top_def) :
    unit =
  let args =
    match Mark.remove second.topdef_type with
    | Func { arg_typ; _ } -> List.map (fun ((_, pos), _) -> pos) arg_typ
    | Base _ -> []
  in
  List.iter
    (fun pos ->
      Pos.get_attrs_sorted pos (function
        | Src_doc (_, _, p) -> Some (p, p)
        | _ -> None)
      |> List.iter (fun pos ->
          Message.delayed_error () ~pos
            ~extra_pos:["First declaration", Mark.get first.topdef_name]
            "This constant or function is declared twice in metadata@ blocks: \
             its documentation must be on the first declaration."))
    (Mark.get second.topdef_name :: args)

(** Documented in [extract.mli] *)
let extract (prg : SurfAst.program) (ctx : NameRes.context) : module_doc =
  (* The page prints keywords from [Lexer_en] and English labels, so for consistency,
  we currently reject languages other than English. *)
  if prg.program_lang <> `En then
    Message.error "Only modules written in English can be documented for now.";
  let module_name, module_is_external =
    match prg.program_module with
    | Some { module_name = name, _; module_is_external } ->
      name, module_is_external
    | None ->
      Message.error
        "This file does not define a module, so it has no public interface to \
         document. Add a '@{<cyan>> Module Name@}' directive at its start."
  in
  let _, interface = Surface.Parser_driver.get_interface prg in
  let ( @:: ) x xs = match x with Some x -> x :: xs | None -> xs in
  let add_decl (first_decls, acc) ((code_item, _), visibility) =
    match code_item, visibility with
    | _, Private -> first_decls, acc
    | SurfAst.Topdef d, Public -> (
      (* A topdef declared twice is documented by its first declaration: see
         [check_second_decl] *)
      let name = Mark.remove d.topdef_name in
      match String.Map.find_opt name first_decls with
      | None -> String.Map.add name d first_decls, item ctx (Topdef d) @:: acc
      | Some first ->
        check_second_decl ~first ~second:d;
        first_decls, acc)
    | _, Public -> first_decls, item ctx code_item @:: acc
  in
  let decls =
    interface
    |> List.fold_left add_decl (String.Map.empty, [])
    |> snd
    |> List.rev
  in
  Message.report_delayed_errors_if_any ();
  {
    module_name;
    module_is_external;
    module_uses =
      List.filter_map
        (fun u ->
          let alias = Mark.remove u.SurfAst.mod_use_alias in
          Ident.Map.find_opt alias ctx.local.used_modules
          |> Option.map (fun use_module -> { use_module; use_alias = alias }))
        prg.program_used_modules;
    module_items = decls;
  }
