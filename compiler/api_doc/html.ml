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
open Extract
module Tkns = Surface.Tokens

(** The stylesheet of a standalone page, with a dark variant *)
let style =
  {|
:root {
  --bg: #f7f8fa;
  --surface: #ffffff;
  --ink: #1b2330;
  --muted: #5d6675;
  --line: #dde1e7;
  --accent: #1d5c86;
  --code-bg: #f1f3f6;
  --kw: #7a3ea8;
  --ty: #1d5c86;
  --builtin: #2c7a4b;
  --mono: ui-monospace, "SFMono-Regular", Menlo, Consolas, monospace;
  --sans: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg: #12161c;
    --surface: #191e26;
    --ink: #e3e7ed;
    --muted: #9aa3b1;
    --line: #2d3541;
    --accent: #7fb6dc;
    --code-bg: #1e242d;
    --kw: #c79bea;
    --ty: #7fb6dc;
    --builtin: #7fcf9c;
  }
}

* {
  box-sizing: border-box;
}

body {
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font-family: var(--sans);
  font-size: 16px;
  line-height: 1.55;
}

.page {
  display: grid;
  grid-template-columns: 240px minmax(0, 1fr);
  max-width: 1180px;
  margin: 0 auto;
}

nav {
  position: sticky;
  top: 0;
  align-self: start;
  max-height: 100vh;
  overflow-y: auto;
  padding: 32px 20px;
  font-size: 14px;
}

nav .module {
  font-weight: 600;
  font-family: var(--mono);
  display: block;
  margin-bottom: 16px;
  color: var(--ink);
  text-decoration: none;
}

nav h3 {
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: .06em;
  color: var(--muted);
  margin: 18px 0 6px;
  font-weight: 600;
}

nav ul {
  list-style: none;
  margin: 0;
  padding: 0;
}

nav li a {
  display: block;
  padding: 2px 0;
  font-family: var(--mono);
  font-size: 13px;
  color: var(--accent);
  text-decoration: none;
  overflow-wrap: anywhere;
}

nav li a:hover {
  text-decoration: underline;
}

main {
  padding: 32px 32px 80px;
  min-width: 0;
}

header h1 {
  font-family: var(--mono);
  font-size: 30px;
  margin: 0 0 4px;
}

header .meta {
  color: var(--muted);
  margin: 0;
}

header .meta a {
  color: var(--accent);
}

.item {
  background: var(--surface);
  border: 1px solid var(--line);
  border-radius: 8px;
  margin-top: 24px;
  padding: 18px 20px;
  scroll-margin-top: 16px;
}

.item h3 {
  font-family: var(--mono);
  font-size: 19px;
  margin: 0;
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 10px;
}

.item h3 a {
  color: var(--ink);
  text-decoration: none;
}

.item h3 a:hover {
  text-decoration: underline;
}

.decl {
  background: var(--code-bg);
  border-radius: 6px;
  padding: 10px 14px;
  margin-top: 12px;
  overflow-x: auto;
}

.line {
  font-family: var(--mono);
  font-size: 14px;
  white-space: pre;
}

.member {
  padding: 4px 0 4px 2ch;
  scroll-margin-top: 16px;
}

.member + .member {
  border-top: 1px dashed var(--line);
}

.member .doc {
  margin-left: 2ch;
}

.doc {
  color: var(--ink);
}

.doc p {
  margin: 8px 0 0;
  max-width: 75ch;
}

.decl .doc p {
  margin: 2px 0 0;
  font-size: 14.5px;
  color: var(--muted);
}

dl.args {
  margin: 4px 0 0 2ch;
  font-size: 14px;
}

dl.args dt {
  font-family: var(--mono);
  color: var(--muted);
}

dl.args dd {
  margin: 0 0 4px 2ch;
}

dl.args .doc p {
  margin: 0;
}

.kw {
  color: var(--kw);
  font-weight: 600;
}

.ty {
  color: var(--ty);
}

a.ty {
  text-decoration: underline dotted;
}

.builtin {
  color: var(--builtin);
}

.tvar {
  font-style: italic;
}

.punct {
  color: var(--muted);
}

.empty {
  color: var(--muted);
  font-style: italic;
  margin-top: 24px;
}

@media (max-width: 760px) {
  .page {
    grid-template-columns: 1fr;
  }
  nav {
    position: static;
    max-height: none;
    padding: 20px 16px 0;
  }
  main {
    padding: 16px 16px 60px;
  }
}
|}

(** Prints [s], escaped to be used as HTML text or as an attribute value. Any
    text that comes from the program is printed with it. *)
let escape ppf (s : string) =
  String.iter
    (function
      | '&' -> Format.pp_print_string ppf "&amp;"
      | '<' -> Format.pp_print_string ppf "&lt;"
      | '>' -> Format.pp_print_string ppf "&gt;"
      | '"' -> Format.pp_print_string ppf "&quot;"
      | '\'' -> Format.pp_print_string ppf "&#39;"
      | c -> Format.pp_print_char ppf c)
    s

(** Prints [s], escaped, in a [<span>] of class [cls], used for syntax
    highlighting *)
let span cls ppf s =
  Format.fprintf ppf "<span class=\"%s\">%a</span>" cls escape s

(** Separates the elements of a list with a comma *)
let comma ppf () = Format.pp_print_string ppf ", "

(** The groups of items in the navigation sidebar *)
type section = Types | Topdefs | Scopes

(** All the sections, in the order of the sidebar *)
let sections = [Types; Topdefs; Scopes]

(** The section that lists the items of this kind *)
let section_of_kind = function
  | Struct _ | Enum _ | Abstract -> Types
  | Constant _ | Function _ -> Topdefs
  | Scope _ -> Scopes

type labels = {
  section_label : section -> string;
  catala_module : string;
  external_module : string;
  uses : string;
  alias : string;
  no_declarations : string;
}
(** The text of the page that doesn't come from the program *)

(** The labels in English, the only language supported for now *)
let labels =
  {
    section_label =
      (function
      | Types -> "Types"
      | Topdefs -> "Constants and functions"
      | Scopes -> "Scopes");
    catala_module = "Catala module";
    external_module = "external";
    uses = "uses";
    alias = "as";
    no_declarations = "This module has no public declarations.";
  }

type env = {
  local_anchors : String.Set.t;
      (** Anchors of the items of the page, the only local link targets *)
}
(** What rendering an item needs to know about the rest of the page *)

(** Prints the keyword of [tok], highlighted. Only English is supported for now,
    see [Extract.extract]. *)
let kw ppf (tok : Tkns.token) =
  match List.find_opt (fun (_, t) -> t = tok) Surface.Lexer_en.token_list with
  | Some (s, _) -> span "kw" ppf s
  | None -> assert false (* only called on keyword tokens *)

(** The name of a builtin type. Type names and [external] are not in
    [token_list], as the lexer recognises them by other means. *)
let primitive_type_name : Surface.Ast.primitive_typ -> string = function
  | Integer -> "integer"
  | Decimal -> "decimal"
  | Boolean -> "boolean"
  | Money -> "money"
  | Duration -> "duration"
  | Date -> "date"
  | Position -> "code_location"
  | Named _ | Var _ -> assert false (* printed by [typ] *)

(** Prints [text], linked to [target]. A local target that is not on the page,
    such as a private type, is printed without a link. *)
let link env (target : target) ppf (text : string) =
  let href =
    match target.target_module with
    | None ->
      if String.Set.mem target.target_anchor env.local_anchors then
        Some ("#" ^ target.target_anchor)
      else None
    | Some m -> Some (ModuleName.to_string m ^ ".html#" ^ target.target_anchor)
  in
  match href with
  | Some href ->
    Format.fprintf ppf "<a class=\"ty\" href=\"%a\">%a</a>" escape href escape
      text
  | None -> span "ty" ppf text

(** Prints a type, in Catala syntax, with named types linked to their
    declaration *)
let rec typ env ppf = function
  | Builtin (Var None) -> kw ppf Tkns.WILDCARD
  | Builtin (Var (Some (v, _))) ->
    Format.fprintf ppf "%a %a %a %a" kw Tkns.WILDCARD kw Tkns.OF kw Tkns.TYPE
      (span "tvar") v
  | Builtin p -> span "builtin" ppf (primitive_type_name p)
  | Named { name; target = Some target } -> link env target ppf name
  | Named { name; target = None } -> span "ty" ppf name
  | List t -> Format.fprintf ppf "%a %a" kw Tkns.LIST (typ env) t
  | Option t -> Format.fprintf ppf "%a %a" kw Tkns.OPTION (typ env) t
  | Tuple ts ->
    Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:comma (typ env)) ts

(** Prints [content t], or [condition] *)
let base_typ env ppf = function
  | Condition -> kw ppf Tkns.CONDITION
  | Data t -> Format.fprintf ppf "%a %a" kw Tkns.CONTENT (typ env) t

(** Prints the [depends on] clause of a function, preceded by a space, or
    nothing for a non-function. The docs of the arguments are printed
    separately, by [args_doc_html]. *)
let args env ppf = function
  | [] -> ()
  | args ->
    let arg ppf a =
      Format.fprintf ppf "%a %a" (span "name") a.arg_name (base_typ env)
        a.arg_typ
    in
    Format.fprintf ppf " %a %a" kw Tkns.DEPENDS
      (Format.pp_print_list ~pp_sep:comma arg)
      args

(** Prints the first line of the declaration of an item, as in the source, e.g.
    [declaration structure S:]. For a function, it ends with [depends on], and
    its arguments are printed on the following lines, as members. *)
let item_header env ppf (i : item) =
  let decl = Tkns.DECLARATION in
  let name = i.item_name in
  match i.item_kind with
  | Struct _ ->
    Format.fprintf ppf "%a %a %a:" kw decl kw Tkns.STRUCT (span "name") name
  | Enum _ ->
    Format.fprintf ppf "%a %a %a:" kw decl kw Tkns.ENUM (span "name") name
  | Scope _ ->
    Format.fprintf ppf "%a %a %a:" kw decl kw Tkns.SCOPE (span "name") name
  | Abstract ->
    (* [external] is not in [token_list] either *)
    Format.fprintf ppf "%a %a %a: %a" kw decl kw Tkns.TYPE (span "name") name
      (span "kw") "external"
  | Constant t ->
    Format.fprintf ppf "%a %a %a" kw decl (span "name") name (base_typ env) t
  | Function { return_typ; _ } ->
    Format.fprintf ppf "%a %a %a %a" kw decl (span "name") name (base_typ env)
      return_typ kw Tkns.DEPENDS

(** Prints the line declaring a structure field, as in the source *)
let field_line env ppf (f : signature member) =
  let { sig_typ; sig_args } = f.member_decl in
  match sig_typ with
  | Condition ->
    Format.fprintf ppf "%a %a%a" kw Tkns.CONDITION (span "name") f.member_name
      (args env) sig_args
  | Data t ->
    Format.fprintf ppf "%a %a %a %a%a" kw Tkns.DATA (span "name") f.member_name
      kw Tkns.CONTENT (typ env) t (args env) sig_args

(** Prints the line declaring an enumeration case, as in the source *)
let case_line env ppf (c : base_typ option member) =
  Format.fprintf ppf "%a %a" (span "punct") "--" (span "name") c.member_name;
  Option.iter (Format.fprintf ppf " %a" (base_typ env)) c.member_decl

(** Prints the line declaring a scope variable or subscope, as in the source *)
let scope_member_line env ppf (m : scope_member member) =
  let qualifier tok = Format.fprintf ppf "%a " kw tok in
  match m.member_decl with
  | Var { input; output; var_sig } ->
    (match input with
    | Input -> qualifier Tkns.INPUT
    | Context -> qualifier Tkns.CONTEXT
    | Internal -> ());
    if output then qualifier Tkns.OUTPUT;
    Format.fprintf ppf "%a %a%a" (span "name") m.member_name (base_typ env)
      var_sig.sig_typ (args env) var_sig.sig_args
  | Subscope { output; scope } ->
    if output then qualifier Tkns.OUTPUT;
    Format.fprintf ppf "%a %a %a" (span "name") m.member_name kw Tkns.SCOPE
      (typ env) scope

(** Prints the line declaring an argument of a function, as in the source.
    [last] tells whether it is the last one, which is not followed by a comma.
*)
let arg_line env ~last ppf (a : base_typ member) =
  Format.fprintf ppf "%a %a%s" (span "name") a.member_name (base_typ env)
    a.member_decl
    (if last then "" else ",")

(** Prints a docstring, if any, with one paragraph per block of non-blank lines.
    Within a paragraph, a line break is a soft break, as in Markdown: the lines
    are joined with a space. *)
let doc_html ppf (doc : string option) =
  let add_line (cur, acc) line =
    if String.trim line = "" then
      [], if cur = [] then acc else List.rev cur :: acc
    else line :: cur, acc
  in
  let space ppf () = Format.pp_print_char ppf ' ' in
  let paragraph ppf lines =
    Format.fprintf ppf "<p>%a</p>"
      (Format.pp_print_list ~pp_sep:space escape)
      lines
  in
  match doc with
  | None -> ()
  | Some doc ->
    let paragraphs =
      String.split_on_char '\n' doc
      |> List.fold_left add_line ([], [])
      |> fun (cur, acc) ->
      List.rev (if cur = [] then acc else List.rev cur :: acc)
    in
    Format.fprintf ppf "<div class=\"doc\">%a</div>"
      (Format.pp_print_list ~pp_sep:(fun _ () -> ()) paragraph)
      paragraphs

(** Prints the docs of the arguments of a function-typed field or variable,
    whose declaration prints them inline *)
let args_doc_html ppf (args : arg list) =
  let arg_doc a =
    Format.fprintf ppf "<dt>%a</dt><dd>%a</dd>" escape a.arg_name doc_html
      a.arg_doc
  in
  match List.filter (fun a -> a.arg_doc <> None) args with
  | [] -> ()
  | documented ->
    Format.fprintf ppf "<dl class=\"args\">";
    List.iter arg_doc documented;
    Format.fprintf ppf "</dl>"

(** Prints a member: its declaration line, printed by [line], its docs, and the
    docs of [args], the arguments of a function-typed member *)
let member_html ?(args = []) ~line ppf (m : _ member) =
  Format.fprintf ppf
    "<div class=\"member\" id=\"%a\"><div class=\"line\">%a</div>%a%a</div>\n"
    escape m.member_anchor line m doc_html m.member_doc args_doc_html args

(** Prints the members of an item, depending on its kind *)
let members_html env ppf (kind : item_kind) =
  let field (f : signature member) =
    member_html ~args:f.member_decl.sig_args ~line:(field_line env) ppf f
  in
  let case c = member_html ~line:(case_line env) ppf c in
  let scope_member (m : scope_member member) =
    let args =
      match m.member_decl with
      | Var { var_sig; _ } -> var_sig.sig_args
      | Subscope _ -> []
    in
    member_html ~args ~line:(scope_member_line env) ppf m
  in
  match kind with
  | Struct fields -> List.iter field fields
  | Enum cases -> List.iter case cases
  | Scope members -> List.iter scope_member members
  | Function { args; _ } ->
    let last_idx = List.length args - 1 in
    let arg idx a =
      member_html ~line:(arg_line env ~last:(idx = last_idx)) ppf a
    in
    List.iteri arg args
  | Constant _ | Abstract -> ()

(** Prints the section of an item: its title, its docs, and its declaration,
    with one line per member followed by its docs *)
let render_item env ppf (i : item) =
  Format.fprintf ppf "<section class=\"item\" id=\"%a\">\n" escape i.item_anchor;
  Format.fprintf ppf "<h3><a href=\"#%a\">%a</a></h3>\n" escape i.item_anchor
    escape i.item_name;
  if i.item_doc <> None then Format.fprintf ppf "%a\n" doc_html i.item_doc;
  Format.fprintf ppf "<div class=\"decl\">\n<div class=\"line\">%a</div>\n"
    (item_header env) i;
  members_html env ppf i.item_kind;
  Format.fprintf ppf "</div>\n</section>\n"

(** Prints the navigation sidebar: the items of each section, in alphabetical
    order *)
let render_nav ppf (module_name : string) (items : item list) =
  let by_name a b =
    match
      String.compare
        (String.lowercase_ascii a.item_name)
        (String.lowercase_ascii b.item_name)
    with
    | 0 -> String.compare a.item_name b.item_name
    | c -> c
  in
  let link i =
    Format.fprintf ppf "<li><a href=\"#%a\">%a</a></li>\n" escape i.item_anchor
      escape i.item_name
  in
  let render_section section =
    match
      List.filter (fun i -> section_of_kind i.item_kind = section) items
    with
    | [] -> ()
    | section_items ->
      Format.fprintf ppf "<h3>%a</h3>\n<ul>\n" escape
        (labels.section_label section);
      List.iter link (List.sort by_name section_items);
      Format.fprintf ppf "</ul>\n"
  in
  Format.fprintf ppf "<nav>\n<a class=\"module\" href=\"#top\">%a</a>\n" escape
    module_name;
  List.iter render_section sections;
  Format.fprintf ppf "</nav>\n"

(** Prints a module used by the documented one, linked to its page *)
let module_use_html ppf (u : module_use) =
  let m = ModuleName.to_string u.use_module in
  Format.fprintf ppf "<a href=\"%a.html\">%a</a>" escape m escape m;
  if u.use_alias <> m then
    Format.fprintf ppf " %a %a" escape labels.alias escape u.use_alias

(** Documented in [html.mli] *)
let render ~wrap ppf (doc : module_doc) =
  let items = doc.module_items in
  let local_anchors =
    List.to_seq items |> Seq.map (fun i -> i.item_anchor) |> String.Set.of_seq
  in
  let env = { local_anchors } in
  let name = doc.module_name in
  if wrap then begin
    Format.fprintf ppf
      "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n";
    Format.fprintf ppf
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n";
    Format.fprintf ppf "<title>%a</title>\n<style>%s</style>\n</head>\n<body>\n"
      escape name style
  end;
  Format.fprintf ppf "<div class=\"page\">\n";
  render_nav ppf doc.module_name items;
  Format.fprintf ppf "<main>\n<header id=\"top\">\n<h1>%a</h1>\n" escape name;
  Format.fprintf ppf "<p class=\"meta\">%a" escape labels.catala_module;
  if doc.module_is_external then
    Format.fprintf ppf " (%a)" escape labels.external_module;
  if doc.module_uses <> [] then
    Format.fprintf ppf " · %a %a" escape labels.uses
      (Format.pp_print_list ~pp_sep:comma module_use_html)
      doc.module_uses;
  Format.fprintf ppf "</p>\n</header>\n";
  if items = [] then
    Format.fprintf ppf "<p class=\"empty\">%a</p>\n" escape
      labels.no_declarations;
  List.iter (render_item env ppf) items;
  Format.fprintf ppf "</main>\n</div>\n";
  if wrap then Format.fprintf ppf "</body>\n</html>\n"
