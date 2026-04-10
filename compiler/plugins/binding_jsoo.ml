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
open Lcalc

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
            Format.fprintf ppi "@,@[<hov 2>val %a : %a@]@," format_var var
              To_ocaml.format_typ typ;
            let rec aux bctx typ =
              match Mark.remove typ with
              | TArrow ([(TLit TUnit, _)], te) ->
                Format.fprintf ppml
                  "@,@[<v 2>@[<hov 2>let %a : %a =@]@ fun () -> %a(%s##%a_)@]@,"
                  format_var var To_ocaml.format_typ typ
                  To_jsoo_interface.format_typ_of te js_object format_var var
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

let binding_jsoo
    includes
    stdlib
    output
    optimize
    check_invariants
    autotest
    closure_conversion
    options =
  let () = ignore (Global.enforce_options ~gen_external:true ()) in
  let prg, type_ordering, _ =
    Driver.Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
      ~autotest ~typed:Expr.typed ~closure_conversion ~keep_special_ops:true
      ~monomorphize_types:false ~lift_pos:(Some Lcalc.To_ocaml.op_needs_pos)
      ~renaming:(Some Lcalc.To_ocaml.renaming)
  in
  (* The goal is to shadow the real implementation of the ml file. So we don't
     append a suffix like _jsoo*)
  Message.debug "Compiling program to generate Js_of_ocaml interface...";
  Driver.Commands.get_output_format options output ~ext:"ml"
  @@ fun output_file fmt ->
  let hashf = Hash.finalise ~monomorphize_types:false in
  format_program output_file fmt prg ~hashf type_ordering

let binding_jsoo_cmd =
  let open Cmdliner in
  Term.(
    const binding_jsoo
    $ Cli.Flags.include_dirs
    $ Cli.Flags.stdlib_dir
    $ Cli.Flags.output
    $ Cli.Flags.optimize
    $ Cli.Flags.check_invariants
    $ Cli.Flags.autotest
    $ Cli.Flags.closure_conversion)

let () =
  Driver.Plugin.register "binding-jsoo" binding_jsoo_cmd
    ~doc:
      "Generates a Js_of_ocaml interface to use Catala program in javascript."
