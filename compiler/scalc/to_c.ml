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
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from
       https://learn.microsoft.com/en-us/cpp/c-language/c-keywords *)
    | "auto" | "break" | "case" | "char" | "const" | "continue" | "default"
    | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto"
    | "if" | "inline" | "int" | "long" | "register" | "restrict" | "return"
    | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef"
    | "union" | "unsigned" | "void" | "volatile" | "while" ->
      true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : StructName.t) : unit =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a" StructName.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_struct_field_name (fmt : Format.formatter) (v : StructField.t) : unit
    =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a" StructField.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a" EnumName.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_cons_name (fmt : Format.formatter) (v : EnumConstructor.t) :
    unit =
  Format.fprintf fmt "%s"
    (Format.asprintf "%a" EnumConstructor.format v
    |> String.to_ascii
    |> String.to_snake_case
    |> avoid_keywords)

let rec format_typ (decl_ctx : decl_ctx) (fmt : Format.formatter) (typ : typ) :
    unit =
  match Mark.remove typ with
  | TLit TUnit -> Format.fprintf fmt "void * /* Unit */"
  | TLit TMoney -> Format.fprintf fmt "int"
  | TLit TInt -> Format.fprintf fmt "int"
  | TLit TRat -> Format.fprintf fmt "double"
  | TLit TDate -> Format.fprintf fmt "double"
  | TLit TDuration -> Format.fprintf fmt "double"
  | TLit TBool -> Format.fprintf fmt "char"
  | TTuple ts ->
    Format.fprintf fmt "@[<v 2>struct {@,%a @]@,}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (t, i) ->
           Format.fprintf fmt "%a arg_%d" (format_typ decl_ctx) t i))
      (List.mapi (fun x y -> y, x) ts)
  | TStruct s -> Format.fprintf fmt "%a" format_struct_name s
  | TOption some_typ ->
    (* We translate the option type with an overloading to C's [NULL] *)
    Format.fprintf fmt "@[<h 2>union { %a;@ void * @]@,} /* eoption %a */"
      (format_typ decl_ctx) some_typ (Print.typ decl_ctx) some_typ
  | TDefault t -> format_typ decl_ctx fmt t
  | TEnum e -> Format.fprintf fmt "%a" format_enum_name e
  | TArrow (_t1, _t2) ->
    Format.fprintf fmt "void * /* %a */" (Print.typ decl_ctx) typ
  | TArray t1 -> Format.fprintf fmt "%a *" (format_typ decl_ctx) t1
  | TAny -> Format.fprintf fmt "void * /* any */"
  | TClosureEnv -> Format.fprintf fmt "void * /* closure_env */"

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    Format.fprintf fmt "@[<v 2>typedef struct %a {@ %a;@]@,} %a;"
      format_struct_name struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "@[<v>%a %a@]" (format_typ ctx) struct_field_type
             format_struct_field_name struct_field))
      fields format_struct_name struct_name;
    if false then
      Format.fprintf fmt
        "class %a:@\n\
        \    def __init__(self, %a) -> None:@\n\
         %a@\n\
         @\n\
        \    def __eq__(self, other: object) -> bool:@\n\
        \        if isinstance(other, %a):@\n\
        \            return @[<hov>(%a)@]@\n\
        \        else:@\n\
        \            return False@\n\
         @\n\
        \    def __ne__(self, other: object) -> bool:@\n\
        \        return not (self == other)@\n\
         @\n\
        \    def __str__(self) -> str:@\n\
        \        @[<hov 4>return \"%a(%a)\".format(%a)@]" format_struct_name
        struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "%a: %a" format_struct_field_name struct_field
               (format_typ ctx) struct_field_type))
        fields
        (if StructField.Map.is_empty struct_fields then fun fmt _ ->
           Format.fprintf fmt "        pass"
         else
           Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt (struct_field, _) ->
               Format.fprintf fmt "        self.%a = %a"
                 format_struct_field_name struct_field format_struct_field_name
                 struct_field))
        fields format_struct_name struct_name
        (if not (StructField.Map.is_empty struct_fields) then
           Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " and@ ")
             (fun fmt (struct_field, _) ->
               Format.fprintf fmt "self.%a == other.%a" format_struct_field_name
                 struct_field format_struct_field_name struct_field)
         else fun fmt _ -> Format.fprintf fmt "True")
        fields format_struct_name struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
           (fun fmt (struct_field, _) ->
             Format.fprintf fmt "%a={}" format_struct_field_name struct_field))
        fields
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (struct_field, _) ->
             Format.fprintf fmt "self.%a" format_struct_field_name struct_field))
        fields
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      Format.fprintf fmt
        "@[<hov 4>class %a_Code(Enum):@\n\
         %a@]@\n\
         @\n\
         class %a:@\n\
        \    def __init__(self, code: %a_Code, value: Any) -> None:@\n\
        \        self.code = code@\n\
        \        self.value = value@\n\
         @\n\
         @\n\
        \    def __eq__(self, other: object) -> bool:@\n\
        \        if isinstance(other, %a):@\n\
        \            return self.code == other.code and self.value == \
         other.value@\n\
        \        else:@\n\
        \            return False@\n\
         @\n\
         @\n\
        \    def __ne__(self, other: object) -> bool:@\n\
        \        return not (self == other)@\n\
         @\n\
        \    def __str__(self) -> str:@\n\
        \        @[<hov 4>return \"{}({})\".format(self.code, self.value)@]"
        format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun fmt (i, enum_cons, _enum_cons_type) ->
             Format.fprintf fmt "%a = %d" format_enum_cons_name enum_cons i))
        (List.mapi
           (fun i (x, y) -> i, x, y)
           (EnumConstructor.Map.bindings enum_cons))
        format_enum_name enum_name format_enum_name enum_name format_enum_name
        enum_name
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
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
        Format.fprintf fmt "%a@\n@\n" format_struct_decl
          (s, StructName.Map.find s ctx.ctx_structs)
      | Scopelang.Dependency.TVertex.Enum e ->
        Format.fprintf fmt "%a@\n@\n" format_enum_decl
          (e, EnumName.Map.find e ctx.ctx_enums))
    (type_ordering @ scope_structs)

let format_program
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Format.fprintf fmt
    "@[<v>/* This file has been generated by the Catala compiler, do not edit! \
     */@,\
     @,\
     %a@,\
     @]"
    (format_ctx type_ordering) p.decl_ctx
