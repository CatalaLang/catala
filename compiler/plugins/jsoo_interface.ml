(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Emile Rolley <emile.rolley@tuta.io>.

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
open Lcalc

let pp dest fmt = Format.kdprintf (fun k -> List.iter k dest) fmt

let format_var ppf var =
  let string_var = Format.asprintf "%a" To_ocaml.format_var var in
  let input_var = if string_var = "_" then "custom_jsoo_var" else string_var in
  Format.fprintf ppf "%s" input_var

(* Generation for Javascript template *)

let lit_mock_var_js (l : typ_lit) : string * string =
  match l with
  | TUnit -> "any", "0"
  | TBool -> "boolean", "true"
  | TInt -> "number", "1"
  | TRat -> "string", {|"1.23"|}
  | TMoney -> "string", {|"1.23"|}
  | TDuration -> {|{ "years": number, "months": number, "days": number }|}, ""
  | TDate -> "string", {|"1970-01-01"|}
  | TPos ->
    ( {|{fileName : string, startLine: integer, endLine: integer, endColumn: integer, lawHeadings: string[]}|},
      "" )

let format_typ_example_js (ctx : decl_ctx) (typ : typ) : string * string =
  let rec aux bctx typ =
    match Mark.remove typ with
    | TLit l -> lit_mock_var_js l
    | TTuple [] -> "any[]", "[]"
    | TTuple typs ->
      let js_typs, examples = List.split @@ List.map (aux bctx) typs in
      let js_typ = "[" ^ String.concat ", " js_typs ^ "]" in
      let js_example = "[" ^ String.concat ", " examples ^ "]" in
      js_typ, js_example
    | TStruct s ->
      let struct_fields = StructName.Map.find s ctx.ctx_structs in
      let lst_fields = StructField.Map.bindings struct_fields in
      let str_fields_examples =
        List.map
          (fun (field, typ) ->
            let name = StructField.to_string field in
            let str_typ, example = aux bctx typ in
            let field = Format.sprintf {|"%s": %s|} name in
            field str_typ, field example)
          lst_fields
      in
      let fields, examples = List.split str_fields_examples in
      let build_object field_lst = "{" ^ String.concat ", " field_lst ^ "}" in
      build_object fields, build_object examples
    | TOption t ->
      let typ, example = aux bctx t in
      Format.sprintf "%s[]" typ, Format.sprintf "[%s]" example
    | TDefault t -> aux bctx t
    | TEnum e ->
      let every_cons = EnumName.Map.find e ctx.ctx_enums in
      let enum_lst = EnumConstructor.Map.bindings every_cons in
      let enum_lst_examples =
        List.map
          (fun (variant, typ) ->
            let build_enum_cons =
              Format.asprintf "{%a: %s}" EnumConstructor.format variant
            in
            let typ, example = aux bctx typ in
            build_enum_cons typ, build_enum_cons example)
          enum_lst
      in
      let enum_lst, example = List.split enum_lst_examples in
      "(" ^ String.concat " | " enum_lst ^ ")", List.hd example
    | TAbstract _e -> "any", "0"
    | TArrow (_t1, _t2) -> "any", "0"
    | TArray t1 ->
      let typ, example = aux bctx t1 in
      typ ^ "[]", "[" ^ example ^ "]"
    | TVar v ->
      let typ = Bindlib.unbox @@ Bindlib.box_var v in
      aux bctx (Mark.ghost typ)
    | TForAll tb ->
      let _v, typ, bctx = Bindlib.unmbind_in bctx tb in
      aux bctx typ
    | TClosureEnv -> "any", "0"
    | TError -> assert false
  in
  aux Bindlib.empty_ctxt typ

let doc_for_function (ctx : decl_ctx) (ppjs : Format.formatter) lt te =
  let result_typ, example = format_typ_example_js ctx te in
  let ip = ref (-1) in
  Format.fprintf ppjs "@[<v 1>/**@,%a@,* @returns {%s} %s@,* */@]@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun fmt t ->
         incr ip;
         let type_js, example = format_typ_example_js ctx t in
         Format.fprintf fmt "* @param {%s} _param%d %s" type_js !ip example))
    lt result_typ example;
  example

let format_code_items_js
    (ctx : decl_ctx)
    (ppjs : Format.formatter)
    (code_items : 'm Ast.expr code_item_list) =
  pp [ppjs] "@[<v>";
  let _exports =
    BoundList.iter code_items ~f:(fun var item ->
        match item with
        | Topdef (_name, typ, vis, _e) ->
          if vis = Public then
            let rec aux bctx typ =
              match Mark.remove typ with
              | TArrow (lt, te) | TDefault (TArrow (lt, te), _) ->
                let ip, _ie = ref (-1), ref (-1) in
                let return_value = doc_for_function ctx ppjs lt te in
                Format.fprintf ppjs
                  "@[<v 2>%a: function(%a){@,@[<v 2>return %s;@]@]@,},@,"
                  format_var var
                  (Format.pp_print_list
                     ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                     (fun fmt _t ->
                       incr ip;
                       Format.fprintf fmt "_param%d" !ip))
                  lt return_value
              | TForAll tb ->
                let _v, typ, bctx = Bindlib.unmbind_in bctx tb in
                aux bctx typ
              | _ ->
                Format.fprintf ppjs "@[<v 2>%a: %s@]" format_var var
                  (snd @@ format_typ_example_js ctx typ)
            in
            aux Bindlib.empty_ctxt typ
        | ScopeDef (_name, _body) -> ())
  in
  pp [ppjs] "@]"

(* /** @param {string} tagName @return {Element} */ *)
let format_js_template ppjs name (p : 'm Ast.program) : unit =
  pp [ppjs]
    "// Mock for the catala external: %s\n\
     //\n\
     // This file must be loaded before the js file containing the call to the \
     external contract (with a <script> balise or a require).\n"
    name;
  pp [ppjs] "//@ts-check";
  Option.fold
    ~none:(pp [ppjs] "@[<v 2>{@,%a}]")
    ~some:(fun (modname, _) ->
      pp [ppjs] "@[<v 2>globalThis.%s = {@,%a@]@,}"
        (ModuleName.to_string modname))
    p.module_name
    (format_code_items_js p.decl_ctx)
    p.code_items

let jsoo
    includes
    stdlib
    output
    optimize
    check_invariants
    autotest
    closure_conversion
    options =
  let open Driver.Commands in
  let prg, type_ordering, _ =
    Driver.Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
      ~autotest ~typed:Shared_ast.Expr.typed ~closure_conversion
      ~keep_special_ops:true ~monomorphize_types:false
      ~lift_pos:(Some Lcalc.To_ocaml.op_needs_pos)
      ~renaming:(Some Lcalc.To_ocaml.renaming)
  in
  (* Gen external for jsoo backend generates a javascript file which is
     completely different from it's original purpose to generate an interface*)
  if options.gen_external then
    get_output_format options output ~ext:"js"
    @@ fun _output_file fmt ->
    let module_name =
      match prg.module_name with
      | None -> ""
      | Some (name, _) -> ModuleName.to_string name
    in
    format_js_template fmt module_name prg
  else (
    Message.debug "Compiling program to generate Js_of_ocaml interface...";
    get_output_format options output
      ~ext:(if Global.options.gen_external then "template.ml" else "ml")
      ~suffix:"_jsoo"
    @@ fun output_file fmt ->
    let hashf = Hash.finalise ~monomorphize_types:false in
    Lcalc.To_jsoo_interface.format_program output_file fmt prg ~hashf
      type_ordering)

let jsoo_cmd =
  let open Cmdliner in
  Term.(
    const jsoo
    $ Cli.Flags.include_dirs
    $ Cli.Flags.stdlib_dir
    $ Cli.Flags.output
    $ Cli.Flags.optimize
    $ Cli.Flags.check_invariants
    $ Cli.Flags.autotest
    $ Cli.Flags.closure_conversion)

let () =
  Driver.Plugin.register "jsoo" jsoo_cmd
    ~doc:
      "This plugin is for demonstration purposes and should be equivalent to \
       using the built-in Python backend"
