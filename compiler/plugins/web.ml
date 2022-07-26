(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Emile Rolley <emile.rolley@tuta.io>, Louis Gesbert
   <louis.gesbert@inria.fr>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This file demonstrates the use of backend plugins for Catala. It's a simple
    wrapper on top of the OCaml backend that calls js_of_ocaml on the generated
    code. Not for production use. *)

open Utils
open Lcalc
open Lcalc.Ast
open Lcalc.Backends
open Lcalc.To_ocaml
module D = Dcalc.Ast

let name = "web"
let extension = ".ml"

module To_jsoo = struct
  let format_tlit (fmt : Format.formatter) (l : Dcalc.Ast.typ_lit) : unit =
    Dcalc.Print.format_base_type fmt
      (match l with
      | TUnit -> "unit"
      | TInt -> "int"
      | TRat -> "Js.number Js.t"
      | TMoney -> "Js.number Js.t"
      | TDuration -> "Runtime_jsoo.Runtime.duration Js.t"
      | TBool -> "bool Js.t"
      | TDate -> "Js.js_string Js.t")

  let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Marked.pos) :
      unit =
    let format_typ_with_parens
        (fmt : Format.formatter)
        (t : Dcalc.Ast.typ Marked.pos) =
      if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
      else Format.fprintf fmt "%a" format_typ t
    in
    match Marked.unmark typ with
    | TLit l -> Format.fprintf fmt "%a" format_tlit l
    | TTuple (_, Some s) -> Format.fprintf fmt "%a Js.t" format_struct_name s
    | TTuple (_, None) ->
      (* Tuples are encoded as an javascript polymorphic array. *)
      Format.fprintf fmt "Js.Unsafe.any_js_array Js.t "
    | TEnum ([t], e) when D.EnumName.compare e option_enum = 0 ->
      Format.fprintf fmt "@[<hov 2>(%a)@] %a" format_typ_with_parens t
        format_enum_name e
    | TEnum (_, e) when D.EnumName.compare e option_enum = 0 ->
      Errors.raise_spanned_error (Marked.get_mark typ)
        "Internal Error: found an typing parameter for an eoption type of the \
         wrong length."
    | TEnum (_, e) -> Format.fprintf fmt "%a Js.t" format_enum_name e
    | TArray t1 ->
      Format.fprintf fmt "@[%a@ Js.js_array Js.t@]" format_typ_with_parens t1
    | TAny -> Format.fprintf fmt "Js.Unsafe.any Js.t"
    | TArrow (t1, t2) ->
      Format.fprintf fmt "(@[<hov 2>%a, @ %a@]) Js.meth_callback"
        format_typ_with_parens t1 format_typ_with_parens t2

  let rec format_typ_to_jsoo fmt typ =
    match Marked.unmark typ with
    | Dcalc.Ast.TLit TBool -> Format.fprintf fmt "Js.bool"
    | Dcalc.Ast.TLit TInt -> Format.fprintf fmt "integer_to_int"
    | Dcalc.Ast.TLit TRat ->
      Format.fprintf fmt "Js.number_of_float %@%@ decimal_to_float"
    | Dcalc.Ast.TLit TMoney ->
      Format.fprintf fmt "Js.number_of_float %@%@ money_to_float"
    | Dcalc.Ast.TLit TDuration -> Format.fprintf fmt "duration_to_jsoo"
    | Dcalc.Ast.TLit TDate -> Format.fprintf fmt "date_to_jsoo"
    | Dcalc.Ast.TEnum (_, ename) ->
      Format.fprintf fmt "%a_to_jsoo" format_enum_name ename
    | Dcalc.Ast.TTuple (_, Some sname) ->
      Format.fprintf fmt "%a_to_jsoo" format_struct_name sname
    | Dcalc.Ast.TArray t ->
      Format.fprintf fmt "Js.array %@%@ Array.map (fun x -> %a x)"
        format_typ_to_jsoo t
    | Dcalc.Ast.TAny | Dcalc.Ast.TTuple (_, None) ->
      Format.fprintf fmt "Js.Unsafe.inject"
    | _ -> Format.fprintf fmt ""

  let rec format_typ_of_jsoo fmt typ =
    match Marked.unmark typ with
    | Dcalc.Ast.TLit TBool -> Format.fprintf fmt "Js.to_bool"
    | Dcalc.Ast.TLit TInt -> Format.fprintf fmt "integer_of_int"
    | Dcalc.Ast.TLit TRat ->
      Format.fprintf fmt "decimal_of_float %@%@ Js.float_of_number"
    | Dcalc.Ast.TLit TMoney ->
      Format.fprintf fmt
        "money_of_decimal %@%@ decimal_of_float %@%@ Js.float_of_number"
    | Dcalc.Ast.TLit TDuration -> Format.fprintf fmt "duration_of_jsoo"
    | Dcalc.Ast.TLit TDate -> Format.fprintf fmt "date_of_jsoo"
    | Dcalc.Ast.TEnum (_, ename) ->
      Format.fprintf fmt "%a_of_jsoo" format_enum_name ename
    | Dcalc.Ast.TTuple (_, Some sname) ->
      Format.fprintf fmt "%a_of_jsoo" format_struct_name sname
    | Dcalc.Ast.TArray t ->
      Format.fprintf fmt "Array.map (fun x -> %a x) %@%@ Js.to_array"
        format_typ_of_jsoo t
    | _ -> Format.fprintf fmt ""

  let to_camel_case (s : string) : string =
    String.split_on_char '_' s
    |> (function
         | hd :: tl -> hd :: List.map String.capitalize_ascii tl | l -> l)
    |> String.concat ""

  let format_struct_field_name_camel_case
      (fmt : Format.formatter)
      (v : Dcalc.Ast.StructFieldName.t) : unit =
    let s =
      Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v
      |> to_ascii |> to_lowercase |> avoid_keywords |> to_camel_case
    in
    Format.fprintf fmt "%s" s

  let format_var_camel_case (fmt : Format.formatter) (v : 'm var) : unit =
    let lowercase_name =
      Bindlib.name_of v |> to_ascii |> to_lowercase
      |> Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.") ~subst:(fun _ ->
             "_dot_")
      |> to_ascii |> avoid_keywords |> to_camel_case
    in
    if
      List.mem lowercase_name ["handle_default"; "handle_default_opt"]
      || Dcalc.Print.begins_with_uppercase (Bindlib.name_of v)
    then Format.fprintf fmt "%s" lowercase_name
    else if lowercase_name = "_" then Format.fprintf fmt "%s" lowercase_name
    else Format.fprintf fmt "%s_" lowercase_name

  let format_ctx
      (type_ordering : Scopelang.Dependency.TVertex.t list)
      (fmt : Format.formatter)
      (ctx : D.decl_ctx) : unit =
    let format_prop_or_meth fmt (struct_field_type : D.typ Marked.pos) =
      match Marked.unmark struct_field_type with
      | Dcalc.Ast.TArrow _ -> Format.fprintf fmt "Js.meth"
      | _ -> Format.fprintf fmt "Js.readonly_prop"
    in
    let format_struct_decl fmt (struct_name, struct_fields) =
      let fmt_struct_name fmt _ = format_struct_name fmt struct_name in
      let fmt_module_struct_name fmt _ =
        To_ocaml.format_to_module_name fmt (`Sname struct_name)
      in
      let fmt_to_jsoo fmt _ =
        Format.fprintf fmt "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt (struct_field, struct_field_type) ->
               match Marked.unmark struct_field_type with
               | Dcalc.Ast.TArrow (t1, t2) ->
                 Format.fprintf fmt
                   "method %a@ =@ Js.wrap_meth_callback@ (@[<hov 2>fun input ->@\n\
                    %a (%a.%a (%a input))@])"
                   format_struct_field_name_camel_case struct_field
                   format_typ_to_jsoo t2 fmt_struct_name ()
                   format_struct_field_name (None, struct_field)
                   format_typ_of_jsoo t1
               | _ ->
                 Format.fprintf fmt "val %a@ =@ %a %a.%a"
                   format_struct_field_name_camel_case struct_field
                   format_typ_to_jsoo struct_field_type fmt_struct_name ()
                   format_struct_field_name (None, struct_field)))
          struct_fields
      in
      let fmt_of_jsoo fmt _ =
        Format.fprintf fmt "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@\n")
             (fun fmt (struct_field, struct_field_type) ->
               match Marked.unmark struct_field_type with
               | Dcalc.Ast.TArrow _ ->
                 Format.fprintf fmt
                   "%a = failwith \"The function '%a' translation isn't yet \
                    supported...\""
                   format_struct_field_name (None, struct_field)
                   format_struct_field_name (None, struct_field)
               | _ ->
                 Format.fprintf fmt "%a@ =@ %a %a##.%a" format_struct_field_name
                   (None, struct_field) format_typ_of_jsoo struct_field_type
                   fmt_struct_name () format_struct_field_name_camel_case
                   struct_field))
          struct_fields
      in
      let fmt_conv_funs fmt _ =
        Format.fprintf fmt
          "let %a_to_jsoo (%a : %a.t) : %a Js.t = object%%js@\n\
           @[<hov 2>%a@]@\n\
           end@\n\
           let %a_of_jsoo (%a : %a Js.t) : %a.t = {@[<hov 2>%a@]}"
          fmt_struct_name () fmt_struct_name () fmt_module_struct_name ()
          fmt_struct_name () fmt_to_jsoo () fmt_struct_name () fmt_struct_name
          () fmt_struct_name () fmt_module_struct_name () fmt_of_jsoo ()
      in

      if List.length struct_fields = 0 then
        Format.fprintf fmt
          "class type %a =@ object end@\n\
           let %a_to_jsoo (_ : %a.t) : %a Js.t = object%%js end@\n\
           let %a_of_jsoo (_ : %a Js.t) : %a.t = ()" fmt_struct_name ()
          fmt_struct_name () fmt_module_struct_name () fmt_struct_name ()
          fmt_struct_name () fmt_struct_name () fmt_module_struct_name ()
      else
        Format.fprintf fmt
          "class type %a =@\n@[<hov 2>object@ @[<hov 2>@ @ %a@]@\nend@]@\n%a@\n"
          fmt_struct_name ()
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt (struct_field, struct_field_type) ->
               Format.fprintf fmt "method %a:@ %a %a"
                 format_struct_field_name_camel_case struct_field format_typ
                 struct_field_type format_prop_or_meth struct_field_type))
          struct_fields fmt_conv_funs ()
    in
    let format_enum_decl
        fmt
        (enum_name, (enum_cons : (D.EnumConstructor.t * D.typ Marked.pos) list))
        =
      let fmt_enum_name fmt _ = format_enum_name fmt enum_name in
      let fmt_module_enum_name fmt _ =
        To_ocaml.format_to_module_name fmt (`Ename enum_name)
      in
      let fmt_to_jsoo fmt _ =
        Format.fprintf fmt "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt (cname, typ) ->
               match Marked.unmark typ with
               | Dcalc.Ast.TTuple (_, None) ->
                 Cli.error_print
                   "Tuples aren't supported yet in the conversion to JS"
               | _ ->
                 Format.fprintf fmt
                   "| %a arg -> object%%js@[<hov 2>@\n\
                    val kind = Js.string \"%a\"@\n\
                    val payload = Js.Unsafe.coerce (Js.Unsafe.inject (%a \
                    arg))@]@\n\
                    end"
                   format_enum_cons_name cname format_enum_cons_name cname
                   format_typ_to_jsoo typ))
          enum_cons
      in
      let fmt_of_jsoo fmt _ =
        Format.fprintf fmt
          "match %a##.kind |> Js.to_string with@\n\
           %a@\n\
           | cons -> failwith (Printf.sprintf \"Unexpected '%%s' kind for the \
           enumeration '%a.t'\" cons)"
          fmt_enum_name ()
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt (cname, typ) ->
               match Marked.unmark typ with
               | Dcalc.Ast.TTuple (_, None) ->
                 Cli.error_print
                   "Tuples aren't yet supported in the conversion to JS..."
               | Dcalc.Ast.TLit TUnit ->
                 Format.fprintf fmt "| \"%a\" ->@\n%a.%a ()"
                   format_enum_cons_name cname fmt_module_enum_name ()
                   format_enum_cons_name cname
               | _ ->
                 Format.fprintf fmt
                   "| \"%a\" ->@\n%a.%a (%a (Js.Unsafe.coerce %a##.payload))"
                   format_enum_cons_name cname fmt_module_enum_name ()
                   format_enum_cons_name cname format_typ_of_jsoo typ
                   fmt_enum_name ()))
          enum_cons fmt_module_enum_name ()
      in

      let fmt_conv_funs fmt _ =
        Format.fprintf fmt
          "let %a_to_jsoo : %a.t -> %a Js.t = function@\n\
           @[<hov 2>%a@]@\n\
           let %a_of_jsoo (%a : %a Js.t) : %a.t = @[<hov 2>%a@]" fmt_enum_name
          () fmt_module_enum_name () fmt_enum_name () fmt_to_jsoo ()
          fmt_enum_name () fmt_enum_name () fmt_enum_name ()
          fmt_module_enum_name () fmt_of_jsoo ()
      in
      Format.fprintf fmt
        "class type %a =@\n\
         @[<hov 2>object@ @[<hov 2>@ @ method kind : Js.js_string Js.t \
         Js.readonly_prop@\n\
         @[<v 2>(** Expects one of:@\n\
         %a *)@\n\
         @\n\
         @]method payload : Js.Unsafe.any Js.t Js.readonly_prop@\n\
         @]@\n\
         end@]@\n\
         %a@\n"
        format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun fmt (enum_cons, _) ->
             Format.fprintf fmt "- \"%a\"" format_enum_cons_name enum_cons))
        enum_cons fmt_conv_funs ()
    in
    let is_in_type_ordering s =
      List.exists
        (fun struct_or_enum ->
          match struct_or_enum with
          | Scopelang.Dependency.TVertex.Enum _ -> false
          | Scopelang.Dependency.TVertex.Struct s' -> s = s')
        type_ordering
    in
    let scope_structs =
      List.map
        (fun (s, _) -> Scopelang.Dependency.TVertex.Struct s)
        (Dcalc.Ast.StructMap.bindings
           (Dcalc.Ast.StructMap.filter
              (fun s _ -> not (is_in_type_ordering s))
              ctx.ctx_structs))
    in
    List.iter
      (fun struct_or_enum ->
        match struct_or_enum with
        | Scopelang.Dependency.TVertex.Struct s ->
          Format.fprintf fmt "%a@\n" format_struct_decl (s, find_struct s ctx)
        | Scopelang.Dependency.TVertex.Enum e ->
          Format.fprintf fmt "%a@\n" format_enum_decl (e, find_enum e ctx))
      (type_ordering @ scope_structs)

  let fmt_input_struct_name fmt (scope_def : ('a expr, 'm) D.scope_def) =
    format_struct_name fmt scope_def.scope_body.scope_body_input_struct

  let fmt_output_struct_name fmt (scope_def : ('a expr, 'm) D.scope_def) =
    format_struct_name fmt scope_def.scope_body.scope_body_output_struct

  let rec format_scopes_to_fun
      (ctx : Dcalc.Ast.decl_ctx)
      (fmt : Format.formatter)
      (scopes : ('expr, 'm) Dcalc.Ast.scopes) =
    match scopes with
    | Dcalc.Ast.Nil -> ()
    | Dcalc.Ast.ScopeDef scope_def ->
      let scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
      let fmt_fun_call fmt _ =
        Format.fprintf fmt "%a |> %a_of_jsoo |> %a |> %a_to_jsoo"
          fmt_input_struct_name scope_def fmt_input_struct_name scope_def
          format_var scope_var fmt_output_struct_name scope_def
      in
      Format.fprintf fmt "@\n@\nlet %a (%a : %a Js.t) : %a Js.t =@\n%a@\n%a"
        format_var scope_var fmt_input_struct_name scope_def
        fmt_input_struct_name scope_def fmt_output_struct_name scope_def
        fmt_fun_call () (format_scopes_to_fun ctx) scope_next

  let rec format_scopes_to_callbacks
      (ctx : Dcalc.Ast.decl_ctx)
      (fmt : Format.formatter)
      (scopes : ('expr, 'm) Dcalc.Ast.scopes) : unit =
    match scopes with
    | Dcalc.Ast.Nil -> ()
    | Dcalc.Ast.ScopeDef scope_def ->
      let scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
      let fmt_meth_name fmt _ =
        Format.fprintf fmt "method %a : (%a Js.t -> %a Js.t) Js.callback"
          format_var_camel_case scope_var fmt_input_struct_name scope_def
          fmt_output_struct_name scope_def
      in
      Format.fprintf fmt "@\n@\n@[<hov 2> %a =@\n Js.wrap_callback@ %a@]%a"
        fmt_meth_name () format_var scope_var
        (format_scopes_to_callbacks ctx)
        scope_next

  let format_program
      (fmt : Format.formatter)
      (module_name : string)
      (prgm : 'm Lcalc.Ast.program)
      (type_ordering : Scopelang.Dependency.TVertex.t list) =
    let fmt_lib_name fmt _ =
      Format.fprintf fmt "%sLib"
        (List.nth (String.split_on_char ' ' module_name) 1
        |> String.split_on_char '_'
        |> List.map String.capitalize_ascii
        |> String.concat "")
    in

    Cli.call_unstyled (fun _ ->
        Format.fprintf fmt
          "(** This file has been generated by the Catala compiler, do not \
           edit! *)@\n\
           @\n\
           open Runtime_ocaml.Runtime@\n\
           open Runtime_jsoo.Runtime@\n\
           open Js_of_ocaml@\n\
           %s@\n\
           @\n\
           [@@@@@@ocaml.warning \"-4-26-27-32-41-42\"]@\n\
           @\n\
           (* Generated API *)\n\n\
           %a@\n\
           %a@\n\
           @\n\n\
          \  let _ =@ @[<hov 2> Js.export \"%a\"@\n\
           (object%%js@ @[\n\
           %a@]@\n\
           end)@]@?"
          module_name (format_ctx type_ordering) prgm.decl_ctx
          (format_scopes_to_fun prgm.decl_ctx)
          prgm.scopes fmt_lib_name ()
          (format_scopes_to_callbacks prgm.decl_ctx)
          prgm.scopes)
end

module To_json = struct
  let format_program
      (fmt : Format.formatter)
      (_prgm : 'm Lcalc.Ast.program)
      (_type_ordering : Scopelang.Dependency.TVertex.t list) =
    Cli.call_unstyled (fun _ -> Format.fprintf fmt "{@[<hov 2> TODO @]}")
end

let apply
    ~(output_file : string option)
    ~(scope : string option)
    (prgm : 'm Lcalc.Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) =
  let filename_without_ext_opt =
    Option.map
      (fun f -> Filename.basename f |> String.split_on_char '.' |> List.hd)
      output_file
  in
  let dirname =
    match output_file with Some f -> Filename.dirname f | None -> ""
  in
  File.with_formatter_of_opt_file output_file (fun fmt ->
      Cli.trace_flag := true;
      To_ocaml.format_program fmt prgm type_ordering;
      File.ocamlformat_file_opt output_file);

  let module_name =
    match filename_without_ext_opt with
    | Some name -> Printf.sprintf "open %s" (String.capitalize_ascii name)
    | None -> ""
  in
  let jsoo_output_file_opt =
    Option.map
      (fun f -> Filename.concat dirname (f ^ "_api_web.ml"))
      filename_without_ext_opt
  in
  File.with_formatter_of_opt_file jsoo_output_file_opt (fun fmt ->
      Cli.debug_print "Writing JSOO API code to %s..."
        (Option.value ~default:"stdout" jsoo_output_file_opt);
      To_jsoo.format_program fmt module_name prgm type_ordering;
      File.ocamlformat_file_opt jsoo_output_file_opt);
  match scope with
  | Some s ->
    (* NOTE: Will needs to have the ui_schema + defs too.*)
    let json_file = Filename.concat dirname (s ^ "_schema.json") in
    File.with_formatter_of_file json_file (fun fmt ->
        Cli.debug_print
          "Writing JSON schema corresponding to the scope '%s' to the file \
           %s..."
          s
          (Option.value ~default:"stdout" output_file);
        To_json.format_program fmt prgm type_ordering)
  | None -> ()

let () = Driver.Plugin.register_lcalc ~name ~extension apply
