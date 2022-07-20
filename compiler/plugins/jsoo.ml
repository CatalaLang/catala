(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>.

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

let name = "jsoo"
let extension = ".js"

module To_jsoo = struct
  let format_tlit (fmt : Format.formatter) (l : Dcalc.Ast.typ_lit) : unit =
    Dcalc.Print.format_base_type fmt
      (match l with
      | TUnit -> "unit" (* TODO: is it the best?*)
      | TInt -> "int"
      | TRat -> "float"
      | TMoney -> "float"
      | TDuration -> "string"
      | TBool -> "bool Js.t"
      | TDate -> "Js.date Js.t")

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
         wrong lenght."
    | TEnum (_, e) -> Format.fprintf fmt "%a Js.t" format_enum_name e
    | TArray t1 ->
      Format.fprintf fmt "@[%a@ Js.js_array Js.t@]" format_typ_with_parens t1
    | TAny -> Format.fprintf fmt "Js.Unsafe.any Js.t"
    | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" format_typ_with_parens t1
        format_typ_with_parens t2

  let to_camel_case (s : string) : string =
    String.split_on_char '_' s
    |> (function
         | hd :: tl -> hd :: List.map String.capitalize_ascii tl | l -> l)
    |> String.concat ""

  let format_struct_field_name_camel_case
      (fmt : Format.formatter)
      (v : Dcalc.Ast.StructFieldName.t) : unit =
    Format.fprintf fmt "%s"
      (Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v
      |> to_ascii |> to_lowercase |> To_ocaml.avoid_keywords |> to_camel_case)

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
      if List.length struct_fields = 0 then
        Format.fprintf fmt
          "class type %a =@ object end@\n\
           let %a_to_jsoo (_ : %a.t) : %a Js.t = object%%js end\n\
           let %a_of_jsoo (_ : %a Js.t) : %a.t = ()" fmt_struct_name ()
          fmt_struct_name () fmt_module_struct_name () fmt_struct_name ()
          fmt_struct_name () fmt_struct_name () fmt_module_struct_name ()
      else
        Format.fprintf fmt
          "class type %a =@\n@[<hov 2>object@ @[<hov 2>@ @ %a@]@\nend@]@\n"
          fmt_struct_name ()
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun _fmt (struct_field, struct_field_type) ->
               Format.fprintf fmt "method %a:@ %a %a"
                 format_struct_field_name_camel_case struct_field format_typ
                 struct_field_type format_prop_or_meth struct_field_type))
          struct_fields
      (* if !Cli.trace_flag then *)
      (*   format_struct_embedding fmt (struct_name, struct_fields) *)
    in
    let format_enum_decl fmt (enum_name, enum_cons) =
      if List.length enum_cons = 0 then
        (* TODO: should ne be possible no?*)
        Format.fprintf fmt
          "class type %a =@ object end @\n\
           let %a_to_jsoo (_ : %a.t) : %a Js.t = object%%js end@\n\
           let %a_of_jsoo (_ : %a Js.t) : %a.t = ()" format_enum_name enum_name
          format_enum_name enum_name To_ocaml.format_to_module_name
          (`Ename enum_name) format_enum_name enum_name format_enum_name
          enum_name format_enum_name enum_name To_ocaml.format_to_module_name
          (`Ename enum_name)
      else
        Format.fprintf fmt
          "class type %a =@\n\
           @[<hov 2>object@ @[<hov 2>@ @ method kind : Js.js_string Js.t \
           Js.readonly_prop@\n\
           @[<v 2>(** Expects one of:@\n\
           %a *)@\n\
           @\n\
           @]method args : Js.Unsafe.any_js_array Js.t Js.readonly_prop@\n\
           @]@\n\
           end@]@\n"
          format_enum_name enum_name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun _fmt (enum_cons, _) ->
               Format.fprintf fmt "- \"%a\"" format_enum_cons_name enum_cons))
          enum_cons
      (* if !Cli.trace_flag then format_enum_embedding fmt (enum_name,
         enum_cons) *)
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

  let rec format_scopes
      (ctx : Dcalc.Ast.decl_ctx)
      (fmt : Format.formatter)
      (scopes : ('expr, 'm) Dcalc.Ast.scopes) : unit =
    let format_typ_to_js fmt typ =
      match Marked.unmark typ with
      | Dcalc.Ast.TLit TUnit -> failwith "todo: TLit TUnit"
      | Dcalc.Ast.TLit TBool -> Format.fprintf fmt "Js.bool"
      | Dcalc.Ast.TLit TInt -> Format.fprintf fmt "integer_to_int"
      | Dcalc.Ast.TLit TRat -> Format.fprintf fmt "decimal_to_float"
      | Dcalc.Ast.TLit TMoney -> Format.fprintf fmt "money_to_float"
      | Dcalc.Ast.TLit TDuration ->
        Format.fprintf fmt "Js.string %@%@ duration_to_string"
      | Dcalc.Ast.TLit TDate -> failwith "todo: TLit TDate"
      | _ ->
        (* todo: format_typ_coerce *)
        Format.fprintf fmt ""
    in
    let _format_fun_call_res fmt struct_name =
      let struct_fields = find_struct struct_name ctx in
      Format.fprintf fmt "(@[<hov 2> object%%js@\n%a@\nend@])"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun _ (struct_field, struct_field_type) ->
             Format.fprintf fmt "val %a =@ %a result.%a"
               format_struct_field_name_camel_case struct_field format_typ_to_js
               struct_field_type format_struct_field_name
               (Some struct_name, struct_field)))
        struct_fields
    in
    match scopes with
    | Dcalc.Ast.Nil -> ()
    | Dcalc.Ast.ScopeDef scope_def ->
      let scope_input_var, _scope_body_expr =
        Bindlib.unbind scope_def.scope_body.scope_body_expr
      in
      let scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
      let fmt_input_struct_name fmt _ =
        format_struct_name fmt scope_def.scope_body.scope_body_input_struct
      in
      let fmt_output_struct_name fmt _ =
        format_struct_name fmt scope_def.scope_body.scope_body_output_struct
      in
      let fmt_meth_name fmt _ =
        Format.fprintf fmt "method %a : (%a -> %a) Js.callback"
          format_var_camel_case scope_var fmt_input_struct_name ()
          fmt_output_struct_name ()
      in
      let fmt_fun_call fmt _ =
        Format.fprintf fmt "%a |> %a_of_jsoo |> %a |> %a_to_jsoo"
          fmt_input_struct_name () fmt_input_struct_name () format_var scope_var
          fmt_output_struct_name ()
      in
      Format.fprintf fmt
        "@\n@\n@[<hov 2> %a =@\n Js.wrap_callback@ (fun %a -> %a)@]%a"
        fmt_meth_name () format_var scope_input_var fmt_fun_call ()
        (format_scopes ctx) scope_next

  let format_program
      (fmt : Format.formatter)
      (module_name : string)
      (prgm : 'm Lcalc.Ast.program)
      (type_ordering : Scopelang.Dependency.TVertex.t list) =
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
           @\n\n\
          \  let _ =@ @[<hov 2> Js.export_all@\n\
           (object%%js@ @[\n\
          \       val eventsManager = Runtime_jsoo.Runtime.event_manager@\n\n\
           %a@]end)@]@?"
          module_name (format_ctx type_ordering) prgm.decl_ctx
          (format_scopes prgm.decl_ctx)
          prgm.scopes)
end

let apply
    (output_file : string option)
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
          match jsoo_output_file_opt with
          | Some f ->
            if Sys.command (Printf.sprintf "ocamlformat %s -i" f) <> 0 then
              failwith "jsoo err"
          | None -> ()))

let () = Driver.Plugin.register_lcalc ~name ~extension apply
