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

  let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Pos.marked) :
      unit =
    let format_typ_with_parens
        (fmt : Format.formatter)
        (t : Dcalc.Ast.typ Pos.marked) =
      if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
      else Format.fprintf fmt "%a" format_typ t
    in
    match Pos.unmark typ with
    | TLit l -> Format.fprintf fmt "%a" format_tlit l
    | TTuple (_, Some s) -> Format.fprintf fmt "%a Js.t" format_struct_name s
    | TTuple (_, None) ->
      (* Tuples are encoded as an javascript polymorphic array. *)
      Format.fprintf fmt "Js.Unsafe.any_js_array Js.t "
    | TEnum ([t], e) when D.EnumName.compare e option_enum = 0 ->
      Format.fprintf fmt "@[<hov 2>(%a)@] %a" format_typ_with_parens t
        format_enum_name e
    | TEnum (_, e) when D.EnumName.compare e option_enum = 0 ->
      Errors.raise_spanned_error (Pos.get_position typ)
        "Internal Error: found an typing parameter for an eoption type of the \
         wrong lenght."
    | TEnum (_, e) -> Format.fprintf fmt "%a Js.t" format_enum_name e
    | TArray t1 ->
      Format.fprintf fmt "@[%a@ Js.js_array Js.t@]" format_typ_with_parens t1
    | TAny -> Format.fprintf fmt "Js.Unsafe.any Js.t"
    | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" format_typ_with_parens t1
        format_typ_with_parens t2

  let format_log_events_types (fmt : Format.formatter) _ : unit =
    Format.fprintf fmt
      "\n\
      \       class type source_position =\n\
      \  object\n\
      \    method fileName : Js.js_string Js.t Js.prop\n\
      \    method startLine : int Js.prop\n\
      \    method endLine : int Js.prop\n\
      \    method startColumn : int Js.prop\n\
      \    method endColumn : int Js.prop\n\
      \    method lawHeadings : Js.js_string Js.t Js.js_array Js.t Js.prop\n\
      \  end\n\n\
       class type raw_event =\n\
      \  object\n\
      \    method eventType : Js.js_string Js.t Js.prop\n\
      \    method information : Js.js_string Js.t Js.js_array Js.t Js.prop\n\
      \    method sourcePosition : source_position Js.t Js.optdef Js.prop\n\
      \    method loggedValueJson : Js.js_string Js.t Js.prop\n\
      \  end\n\n\
       class type event =\n\
      \  object\n\
      \    method data : Js.js_string Js.t Js.prop\n\
      \  end\n\
      \        "

  let format_log_events_funs (fmt : Format.formatter) _ : unit =
    Format.fprintf fmt
      "method resetLog : (unit -> unit) Js.callback = Js.wrap_callback \
       reset_log\n\n\
      \       method retrieveEvents : (unit -> event Js.t Js.js_array Js.t) \
       Js.callback\n\
      \           =\n\
      \         Js.wrap_callback (fun () ->\n\
      \             Js.array\n\
      \               (Array.of_list\n\
      \                  (retrieve_log () |> EventParser.parse_raw_events\n\
      \                  |> List.map (fun event ->\n\
      \                         object%%js\n\
      \                           val mutable data =\n\
      \                             event |> Runtime.yojson_of_event\n\
      \                             |> Yojson.Safe.to_string |> Js.string\n\
      \                         end))))\n\n\
      \       method retrieveRawEvents\n\
      \           : (unit -> raw_event Js.t Js.js_array Js.t) Js.callback =\n\
      \         Js.wrap_callback (fun () ->\n\
      \             Js.array\n\
      \               (Array.of_list\n\
      \                  (List.map\n\
      \                     (fun evt ->\n\
      \                       object%%js\n\
      \                         val mutable eventType =\n\
      \                           Js.string\n\
      \                             (match evt with\n\
      \                             | BeginCall _ -> \"Begin call\"\n\
      \                             | EndCall _ -> \"End call\"\n\
      \                             | VariableDefinition _ -> \"Variable \
       definition\"\n\
      \                             | DecisionTaken _ -> \"Decision taken\")\n\n\
      \                         val mutable information =\n\
      \                           Js.array\n\
      \                             (Array.of_list\n\
      \                                (match evt with\n\
      \                                | BeginCall info\n\
      \                                | EndCall info\n\
      \                                | VariableDefinition (info, _) ->\n\
      \                                  List.map Js.string info\n\
      \                                | DecisionTaken _ -> []))\n\n\
      \                         val mutable loggedValueJson =\n\
      \                           (match evt with\n\
      \                           | VariableDefinition (_, v) -> v\n\
      \                           | EndCall _ | BeginCall _ | DecisionTaken _ ->\n\
      \                             Runtime.unembeddable ())\n\
      \                           |> Runtime.yojson_of_runtime_value\n\
      \                           |> Yojson.Safe.to_string |> Js.string\n\n\
      \                         val mutable sourcePosition =\n\
      \                           match evt with\n\
      \                           | DecisionTaken pos ->\n\
      \                             Js.def\n\
      \                               (object%%js\n\
      \                                  val mutable fileName = Js.string \
       pos.filename\n\
      \                                  val mutable startLine = pos.start_line\n\
      \                                  val mutable endLine = pos.end_line\n\
      \                                  val mutable startColumn = \
       pos.start_column\n\
      \                                  val mutable endColumn = \
       pos.end_column\n\n\
      \                                  val mutable lawHeadings =\n\
      \                                    Js.array\n\
      \                                      (Array.of_list\n\
      \                                         (List.map Js.string \
       pos.law_headings))\n\
      \                               end)\n\
      \                           | _ -> Js.undefined\n\
      \                       end)\n\
      \                     (retrieve_log ()))))\n\
      \    "

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

  let format_var_camel_case (fmt : Format.formatter) (v : Var.t) : unit =
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
    let format_prop_or_meth fmt (struct_field_type : D.typ Pos.marked) =
      match Pos.unmark struct_field_type with
      | Dcalc.Ast.TArrow _ -> Format.fprintf fmt "Js.meth"
      | _ -> Format.fprintf fmt "Js.readonly_prop"
    in
    let format_struct_decl fmt (struct_name, struct_fields) =
      if List.length struct_fields = 0 then
        Format.fprintf fmt "class type %a =@ object end" format_struct_name
          struct_name
      else
        Format.fprintf fmt
          "class type %a =@\n@[<hov 2>object@ @[<hov 2>@ @ %a@]@\nend@]@\n"
          format_struct_name struct_name
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
        Format.fprintf fmt "class type %a =@ object end" format_enum_name
          enum_name
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
      (scopes : expr Dcalc.Ast.scopes) : unit =
    let format_fun_call_input fmt struct_name =
      let struct_fields = find_struct struct_name ctx in
      if List.length struct_fields = 0 then Format.fprintf fmt "()"
      else (*TODO: format_fun_call_inpute *)
        Format.fprintf fmt "()"
    in
    let format_typ_to_js fmt typ =
      match Pos.unmark typ with
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
    let format_fun_call_res fmt struct_name =
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
      Format.fprintf fmt
        "@\n\
         @\n\
         @[<hov 2>method %a : (%a -> %a) Js.callback =@\n\
        \ Js.wrap_callback@ (fun %a ->@[<hov 2>@\n\
        \  let result =@ %a (%a)@ in@\n\n\
        \       %a @])@]%a" format_var_camel_case scope_var format_struct_name
        scope_def.scope_body.scope_body_input_struct format_struct_name
        scope_def.scope_body.scope_body_output_struct format_var scope_input_var
        format_var scope_var format_fun_call_input
        scope_def.scope_body.scope_body_input_struct format_fun_call_res
        scope_def.scope_body.scope_body_output_struct
        (* (format_scope_body_expr ctx) scope_body_expr*)
        (format_scopes ctx)
        scope_next

  let format_program
      (fmt : Format.formatter)
      (module_name : string)
      (prgm : Lcalc.Ast.program)
      (type_ordering : Scopelang.Dependency.TVertex.t list) =
    Cli.style_flag := false;
    Format.fprintf fmt
      "(** This file has been generated by the Catala compiler, do not edit! *)@\n\
       @\n\
       open Runtime@\n\
       open Js_of_ocaml@\n\
       %s@\n\
       @\n\
       [@@@@@@ocaml.warning \"-4-26-27-32-41-42\"]@\n\
       @\n\
       (* Log events utilities *)\n\n\
       %a\n\
       (* Generated API *)\n\n\
       %a@\n\
       @\n\n\
      \  let _ =@ @[<hov 2> Js.export_all@\n\
       (object%%js@ @[%a%a@]end)@]@?" module_name format_log_events_types ()
      (format_ctx type_ordering) prgm.decl_ctx format_log_events_funs ()
      (format_scopes prgm.decl_ctx)
      prgm.scopes
end

let name = "jsoo"
let extension = ".js"

let finalise e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

let finally f k =
  match k () with
  | r ->
    f ();
    r
  | exception e -> finalise e f

let with_open_out file f =
  let oc = open_out file in
  finally (fun () -> close_out oc) (fun () -> f oc)

let with_temp_file pfx sfx f =
  let tmp = Filename.temp_file pfx sfx in
  match f tmp with
  | r ->
    Sys.remove tmp;
    r
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    Sys.remove tmp;
    Printexc.raise_with_backtrace e bt

let apply
    (output_file : string option)
    (prgm : Lcalc.Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) =
  with_temp_file "catala_jsoo_" ".ml" @@ fun ml_file ->
  File.with_formatter_of_opt_file output_file @@ fun fmt ->
  To_ocaml.format_program fmt prgm type_ordering;
  with_temp_file "catala_jsoo_" ".byte" @@ fun bytecode_file ->
  if
    Sys.command
      (Printf.sprintf
         "ocamlfind ocamlc -package catala.runtime -linkpkg %S -o %S" ml_file
         bytecode_file)
    <> 0
  then failwith "ocaml err";
  Cli.debug_print "OCaml compil ok";
  let out_arg =
    match output_file with Some f -> Printf.sprintf "%S" f | None -> "-"
  in
  if
    Sys.command
      (Printf.sprintf
         "js_of_ocaml +zarith_stubs_js/biginteger.js \
          +zarith_stubs_js/runtime.js %S -o %s"
         bytecode_file out_arg)
    <> 0
  then failwith "jsoo err";
  Cli.debug_print "Jsoo compil ok, output in %s"
    (Option.value ~default:"stdout" output_file)

let apply'
    (output_file : string option)
    (prgm : Lcalc.Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) =
  let filename_without_ext_opt =
    Option.map
      (fun f -> Filename.basename f |> String.split_on_char '.' |> List.hd)
      output_file
  in
  let dirname =
    match output_file with Some f -> Filename.dirname f | None -> ""
  in
  File.with_formatter_of_opt_file output_file @@ fun fmt ->
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
  File.with_formatter_of_opt_file jsoo_output_file_opt @@ fun fmt ->
  To_jsoo.format_program fmt module_name prgm type_ordering;
  match jsoo_output_file_opt with
  | Some f ->
    if Sys.command (Printf.sprintf "ocamlformat %s -i" f) <> 0 then
      failwith "jsoo err"
  | None -> ()

let () = Driver.Plugin.register_lcalc ~name ~extension apply'
