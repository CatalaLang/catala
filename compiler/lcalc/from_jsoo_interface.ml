(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

open Catala_utils
open Shared_ast

let pp dest fmt = Format.kdprintf (fun k -> List.iter k dest) fmt

let format_var ppf var =
  let string_var = Format.asprintf "%a" To_ocaml.format_var var in
  let input_var = if string_var = "_" then "custom_jsoo_var" else string_var in
  Format.fprintf ppf "%s" input_var

let js_object_name = String.uncapitalize_ascii

let format_code_items
    (module_name : string)
    (ppml : Format.formatter)
    (ppi : Format.formatter)
    (code_items : 'm Ast.expr code_item_list) =
  let js_object = js_object_name module_name in
  pp [ppml; ppi] "@[<v>";
  let _exports =
    BoundList.iter code_items ~f:(fun var item ->
        match item with
        | Topdef (_name, typ, vis, _e) ->
          if vis = Public then (
            (* J'imagine que ca en js ca doit pouvoir se transposer en
               property*)
            Format.fprintf ppi "@,@[<hov 2>val %a : %a@]@," format_var var
              To_ocaml.format_typ typ;
            let rec aux bctx typ =
              match Mark.remove typ with
              | TArrow (lt, te) | TDefault (TArrow (lt, te), _) ->
                let ip, ie = ref (-1), ref (-1) in
                Format.fprintf ppml
                  "@,\
                   @[<v 2>@[<hov 2>let %a : %a =@]@ fun %a -> %a(%s##%a_ %a)@]@,"
                  format_var var To_ocaml.format_typ typ
                  (Format.pp_print_list
                     ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
                     (fun fmt _t ->
                       incr ip;
                       Format.fprintf fmt "_x%d" !ip))
                  lt To_jsoo_interface.format_typ_of te js_object format_var var
                  (Format.pp_print_list
                     ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
                     (fun fmt t ->
                       incr ie;
                       Format.fprintf fmt "(%a _x%d)"
                         To_jsoo_interface.format_typ_to t !ie))
                  lt
              | TForAll tb ->
                let _v, typ, bctx = Bindlib.unmbind_in bctx tb in
                aux bctx typ
              | _ ->
                Format.fprintf ppml
                  "@,@[<v 2>@[<hov 2>let %a : %a =@]@ %a %s##.%a_@]@,"
                  format_var var To_ocaml.format_typ typ
                  To_jsoo_interface.format_typ_of typ js_object format_var var
            in
            aux Bindlib.empty_ctxt typ)
        | ScopeDef (_name, body) ->
          (* Ca c'est quand tu déclares un champ d'application *)
          if body.scope_body_visibility = Public then (
            let scope_input_var, _scope_body_expr =
              Bindlib.unbind body.scope_body_expr
            in
            let input_var =
              let string_var =
                Format.asprintf "%a" format_var scope_input_var
              in
              if string_var = "_" then "x" else string_var
            in
            Format.fprintf ppi
              "@,@[<hv 2>val %a_jsoo :@ @[<hv>%a.jsoo ->@ %a.jsoo@]@]@,"
              format_var var To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_input_struct)
              To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_output_struct);
            Format.fprintf ppml
              "@,\
               @[<hv 2>@[<hov 2>let %a_jsoo :@ %a.jsoo -> %a.jsoo =@ fun %s \
               ->@]@ %a.to_jsoo (%a (%a.of_jsoo %s))@]@,"
              format_var var To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_input_struct)
              To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_output_struct) input_var
              To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_output_struct) format_var var
              To_jsoo_interface.format_to_module_name
              (`Sname body.scope_body_input_struct) input_var))
  in
  ();
  pp [ppml; ppi] "@]"

let export_code_items ppml modname exports =
  Format.fprintf ppml "@[<hv 2>let () = %a (object%%js@;<1 0>%a@;<1 -2>end)@]"
    (fun fmt m ->
      match m with
      | None -> Format.fprintf fmt "Js.export_all"
      | Some m -> Format.fprintf fmt "Js.export \"%s\"" m)
    modname
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt e ->
         match e with
         | `top v ->
           Format.fprintf fmt "@[<hov 2>val %s =@ %s_jsoo@]"
             (To_jsoo_interface.method_name v)
             v
         | `scope f ->
           Format.fprintf fmt "@[<hov 2>method %s x =@ %s_jsoo x@]"
             (To_jsoo_interface.method_name f)
             f))
    exports

let format_module_registration fmt modname hash is_external =
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "let () =";
  Format.pp_print_space fmt ();
  Format.pp_open_hvbox fmt 2;
  Format.fprintf fmt "Catala_runtime.register_module \"%a\"" ModuleName.format
    modname;
  Format.pp_print_space fmt ();
  Format.pp_open_vbox fmt 2;
  Format.pp_print_string fmt "[]";
  Format.pp_print_space fmt ();
  Format.fprintf fmt "\"%a\""
    (fun ppf h ->
      if is_external then Format.pp_print_string ppf Hash.external_placeholder
      else Hash.format ppf h)
    hash;
  Format.pp_close_box fmt ();
  Format.pp_close_box fmt ();
  Format.pp_print_newline fmt ()

let format_program
    output_file
    ppml
    ~(hashf : Hash.t -> Hash.full)
    (p : 'm Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  File.with_secondary_out_channel ~output_file ~ext:"mli"
  @@ fun intf_file ppi ->
  let modname =
    match p.module_name, output_file with
    | Some (n, _), _ -> Some (ModuleName.to_string n)
    | None, Some filename ->
      Some
        (String.capitalize_ascii (String.to_id File.(basename filename -.- "")))
    | _ -> None
  in
  let module_name = Option.value modname ~default:"NoName" in
  pp [ppml; ppi]
    "@[<v>[%@%@%@ocaml.warning \"-4-26-27-32-33-34-37-41-42-69\"]@,\
     @,\
    \     open Js_of_ocaml@,\
    \     open Catala_runtime@,\
    \     open Catala_runtime_jsoo@,\
     @,";
  pp [ppml] "%a"
    (fun fmt -> function
      | None -> ()
      | Some obj ->
        Format.fprintf fmt "let %s = (Js.Unsafe.js_expr \"%s\")@,"
          (js_object_name obj) obj)
    modname;
  To_jsoo_interface.format_ctx ~include_:false type_ordering ppml ppi p.decl_ctx;
  format_code_items module_name ppml ppi p.code_items;
  p.module_name
  |> Option.iter (fun (modname, intf_id) ->
      format_module_registration ppml modname (hashf intf_id.hash) true);
  pp [ppml; ppi] "@]";
  Format.pp_print_flush ppml ();
  Format.pp_print_flush ppi ();
  Option.iter Ocamlformat.format output_file;
  Option.iter Ocamlformat.format intf_file

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
