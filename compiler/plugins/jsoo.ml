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
open Lcalc.Ast
open Lcalc.Backends
module D = Dcalc.Ast

module To_jsoo = struct
  (* TODO: exctract common functions from the [To_ocaml] plugin. *)
  module Common_with_to_ocaml = struct
    let find_struct (s : D.StructName.t) (ctx : D.decl_ctx) :
        (D.StructFieldName.t * D.typ Pos.marked) list =
      try D.StructMap.find s ctx.D.ctx_structs
      with Not_found ->
        let s_name, pos = D.StructName.get_info s in
        Errors.raise_spanned_error pos
          "Internal Error: Structure %s was not found in the current \
           environment."
          s_name

    let find_enum (en : D.EnumName.t) (ctx : D.decl_ctx) :
        (D.EnumConstructor.t * D.typ Pos.marked) list =
      try D.EnumMap.find en ctx.D.ctx_enums
      with Not_found ->
        let en_name, pos = D.EnumName.get_info en in
        Errors.raise_spanned_error pos
          "Internal Error: Enumeration %s was not found in the current \
           environment."
          en_name

    let avoid_keywords (s : string) : string =
      if
        match s with
        (* list taken from
           http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:keywords *)
        | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint"
        | "do" | "done" | "downto" | "else" | "end" | "exception" | "external"
        | "false" | "for" | "fun" | "function" | "functor" | "if" | "in"
        | "include" | "inherit" | "initializer" | "land" | "lazy" | "let"
        | "lor" | "lsl" | "lsr" | "lxor" | "match" | "method" | "mod" | "module"
        | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or"
        | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try"
        | "type" | "val" | "virtual" | "when" | "while" | "with" ->
          true
        | _ -> false
      then s ^ "_"
      else s

    let format_struct_name (fmt : Format.formatter) (v : Dcalc.Ast.StructName.t) :
      unit =
    Format.asprintf "%a" Dcalc.Ast.StructName.format_t v
    |> to_ascii
    |> to_lowercase
    |> avoid_keywords
    |> Format.fprintf fmt "%s"
    [@@ocamlformat "disable"]

    let format_struct_field_name
        (fmt : Format.formatter)
        ((sname_opt, v) :
          Dcalc.Ast.StructName.t option * Dcalc.Ast.StructFieldName.t) : unit =
      (match sname_opt with
      | Some sname -> Format.fprintf fmt "%a.%s" format_struct_name sname
      | None -> Format.fprintf fmt "%s")
        (Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v
        |> to_ascii
        |> to_lowercase
        |> avoid_keywords
        |> String.split_on_char '_'
        |> (function
            | hd :: tl ->
              hd :: List.map String.capitalize_ascii tl
            | l -> l)
        |> String.concat ""
      )
      [@@ocamlformat "disable"]

    let format_enum_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumName.t) :
        unit =
      Format.fprintf fmt "%s"
        (avoid_keywords
           (to_lowercase
              (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumName.format_t v))))

    let format_enum_cons_name
        (fmt : Format.formatter)
        (v : Dcalc.Ast.EnumConstructor.t) : unit =
      Format.fprintf fmt "%s"
        (avoid_keywords
           (to_ascii
              (Format.asprintf "%a" Dcalc.Ast.EnumConstructor.format_t v)))

    let typ_needs_parens (e : Dcalc.Ast.typ Pos.marked) : bool =
      match Pos.unmark e with TArrow _ | TArray _ -> true | _ -> false

    let format_tlit (fmt : Format.formatter) (l : Dcalc.Ast.typ_lit) : unit =
      Dcalc.Print.format_base_type fmt
        (match l with
        | TUnit -> "unit" (* TODO: is it the best?*)
        | TInt -> "int"
        | TRat -> "float" (* TODO: is it the best?*)
        | TMoney -> "float"
        | TDuration -> "float" (* TODO: is it the best?*)
        | TBool -> "bool Js.t"
        | TDate -> "Js.date Js.t")
  end

  open Common_with_to_ocaml

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

  let format_var (fmt : Format.formatter) (v : Var.t) : unit =
    let lowercase_name = to_lowercase (to_ascii (Bindlib.name_of v)) in
    let lowercase_name =
      Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.")
        ~subst:(fun _ -> "_dot_")
        lowercase_name
    in
    let lowercase_name = avoid_keywords (to_ascii lowercase_name) in
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
               Format.fprintf fmt "method %a:@ %a %a" format_struct_field_name
                 (None, struct_field) format_typ struct_field_type
                 format_prop_or_meth struct_field_type))
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

  let format_program
      (fmt : Format.formatter)
      (prgm : Lcalc.Ast.program)
      (type_ordering : Scopelang.Dependency.TVertex.t list) =
    Cli.style_flag := false;
    Format.fprintf fmt
      "(** This file has been generated by the Catala compiler, do not edit! *)@\n\
       @\n\
       open Runtime@\n\
       open Js_of_ocaml@\n\
       @\n\
       [@@@@@@ocaml.warning \"-4-26-27-32-41-42\"]@\n\
       @\n\
       %a@?"
      (format_ctx type_ordering)
      prgm.decl_ctx (* (format_scopes prgm.decl_ctx) p.scopes *)
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
  Lcalc.To_ocaml.format_program fmt prgm type_ordering;
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
  File.with_formatter_of_opt_file output_file @@ fun fmt ->
  To_jsoo.format_program fmt prgm type_ordering;
  match output_file with
  | Some f ->
    if Sys.command (Printf.sprintf "ocamlformat %s -i" f) <> 0 then
      failwith "jsoo err"
  | None -> ()

let () = Driver.Plugin.register_lcalc ~name ~extension apply'
