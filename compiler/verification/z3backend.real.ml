(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Aymeric Fromherz <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Shared_ast
open Dcalc
open Ast
open Z3
module StringMap : Map.S with type key = String.t = Map.Make (String)
module Runtime = Runtime_ocaml.Runtime

type context = {
  ctx_z3 : Z3.context;
  (* The Z3 context, used to create symbols and expressions *)
  ctx_decl : decl_ctx;
  (* The declaration context from the Catala program, containing information to
     precisely pretty print Catala expressions *)
  ctx_var : (typed expr, typ) Var.Map.t;
  (* A map from Catala variables to their types, needed to create Z3 expressions
     of the right sort *)
  ctx_funcdecl : (typed expr, FuncDecl.func_decl) Var.Map.t;
  (* A map from Catala function names (represented as variables) to Z3 function
     declarations, used to only define once functions in Z3 queries *)
  ctx_z3vars : typed expr Var.t StringMap.t;
  (* A map from strings, corresponding to Z3 symbol names, to the Catala
     variable they represent. Used when to pretty-print Z3 models when a
     counterexample is generated *)
  ctx_z3datatypes : Sort.sort EnumMap.t;
  (* A map from Catala enumeration names to the corresponding Z3 sort, from
     which we can retrieve constructors and accessors *)
  ctx_z3matchsubsts : (typed expr, Expr.expr) Var.Map.t;
  (* A map from Catala temporary variables, generated when translating a match,
     to the corresponding enum accessor call as a Z3 expression *)
  ctx_z3structs : Sort.sort StructMap.t;
  (* A map from Catala struct names to the corresponding Z3 sort, from which we
     can retrieve the constructor and the accessors *)
  ctx_z3unit : Sort.sort * Expr.expr;
  (* A pair containing the Z3 encodings of the unit type, encoded as a tuple of
     0 elements, and the unit value *)
  ctx_z3constraints : Expr.expr list;
      (* A list of constraints about the created Z3 expressions accumulated
         during their initialization, for instance, that the length of an array
         is an integer which always is greater than 0 *)
}
(** The context contains all the required information to encode a VC represented
    as a Catala term to Z3. The fields [ctx_decl] and [ctx_var] are computed
    before starting the translation to Z3, and are thus unmodified throughout
    the translation. The [ctx_z3] context is an OCaml abstraction on top of an
    underlying C++ imperative implementation, it is therefore only created once.
    Unfortunately, the maps [ctx_funcdecl], [ctx_z3vars], and [ctx_z3datatypes]
    are computed dynamically during the translation requiring us to pass the
    context around in a functional way **)

(** [add_funcdecl] adds the mapping between the Catala variable [v] and the Z3
    function declaration [fd] to the context **)
let add_funcdecl
    (v : typed expr Var.t)
    (fd : FuncDecl.func_decl)
    (ctx : context) : context =
  { ctx with ctx_funcdecl = Var.Map.add v fd ctx.ctx_funcdecl }

(** [add_z3var] adds the mapping between [name] and the Catala variable [v] to
    the context **)
let add_z3var (name : string) (v : typed expr Var.t) (ctx : context) : context =
  { ctx with ctx_z3vars = StringMap.add name v ctx.ctx_z3vars }

(** [add_z3enum] adds the mapping between the Catala enumeration [enum] and the
    corresponding Z3 datatype [sort] to the context **)
let add_z3enum (enum : EnumName.t) (sort : Sort.sort) (ctx : context) : context
    =
  { ctx with ctx_z3datatypes = EnumMap.add enum sort ctx.ctx_z3datatypes }

(** [add_z3var] adds the mapping between temporary variable [v] and the Z3
    expression [e] representing an accessor application to the context **)
let add_z3matchsubst (v : typed expr Var.t) (e : Expr.expr) (ctx : context) :
    context =
  { ctx with ctx_z3matchsubsts = Var.Map.add v e ctx.ctx_z3matchsubsts }

(** [add_z3struct] adds the mapping between the Catala struct [s] and the
    corresponding Z3 datatype [sort] to the context **)
let add_z3struct (s : StructName.t) (sort : Sort.sort) (ctx : context) : context
    =
  { ctx with ctx_z3structs = StructMap.add s sort ctx.ctx_z3structs }

let add_z3constraint (e : Expr.expr) (ctx : context) : context =
  { ctx with ctx_z3constraints = e :: ctx.ctx_z3constraints }

(** For the Z3 encoding of Catala programs, we define the "day 0" as Jan 1, 1900
    **)
let base_day = Runtime.date_of_numbers 1900 1 1

(** [unique_name] returns the full, unique name corresponding to variable [v],
    as given by Bindlib **)
let unique_name (v : 'e Var.t) : string =
  Format.asprintf "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

(** [date_to_int] translates [date] to an integer corresponding to the number of
    days since Jan 1, 1900 **)
let date_to_int (d : Runtime.date) : int =
  (* Alternatively, could expose this from Runtime as a (noop) coercion, but
     would allow to break abstraction more easily elsewhere *)
  let period = Runtime.( -@ ) d base_day in
  let y, m, d = Runtime.duration_to_years_months_days period in
  assert (y = 0 && m = 0);
  d

(** [date_of_year] translates a [year], represented as an integer into an OCaml
    date corresponding to Jan 1st of the same year *)
let date_of_year (year : int) = Runtime.date_of_numbers year 1 1

(** Returns the date (as a string) corresponding to nb days after the base day,
    defined here as Jan 1, 1900 **)
let nb_days_to_date (nb : int) : string =
  Runtime.date_to_string
    (Runtime.( +@ ) base_day (Runtime.duration_of_numbers 0 0 nb))

(** [print_z3model_expr] pretty-prints the value [e] given by a Z3 model
    according to the Catala type [ty], corresponding to [e] **)
let rec print_z3model_expr (ctx : context) (ty : typ) (e : Expr.expr) : string =
  let print_lit (ty : typ_lit) =
    match ty with
    (* TODO: Print boolean according to current language *)
    | TBool -> Expr.to_string e
    (* TUnit is only used for the absence of an enum constructor argument.
       Hence, when pretty-printing, we print nothing to remain closer from
       Catala sources *)
    | TUnit -> ""
    | TInt -> Expr.to_string e
    | TRat -> Arithmetic.Real.to_decimal_string e !Cli.max_prec_digits
    (* TODO: Print the right money symbol according to language *)
    | TMoney ->
      let z3_str = Expr.to_string e in
      (* The Z3 model returns an integer corresponding to the amount of cents.
         We reformat it as dollars *)
      let to_dollars s =
        Runtime.money_to_string (Runtime.money_of_cents_string s)
      in
      if String.contains z3_str '-' then
        Format.asprintf "-%s $"
          (to_dollars (String.sub z3_str 3 (String.length z3_str - 4)))
      else Format.asprintf "%s $" (to_dollars z3_str)
    (* The Z3 date representation corresponds to the number of days since Jan 1,
       1900. We pretty-print it as the actual date *)
    (* TODO: Use differnt dates conventions depending on the language ? *)
    | TDate -> nb_days_to_date (int_of_string (Expr.to_string e))
    | TDuration -> Format.asprintf "%s days" (Expr.to_string e)
  in

  match Marked.unmark ty with
  | TLit ty -> print_lit ty
  | TStruct name ->
    let s = StructMap.find name ctx.ctx_decl.ctx_structs in
    let get_fieldname (fn : StructFieldName.t) : string =
      Marked.unmark (StructFieldName.get_info fn)
    in
    let fields =
      List.map2
        (fun (fn, ty) e ->
          Format.asprintf "-- %s : %s" (get_fieldname fn)
            (print_z3model_expr ctx ty e))
        s (Expr.get_args e)
    in

    let fields_str = String.concat " " fields in

    Format.asprintf "%s { %s }"
      (Marked.unmark (StructName.get_info name))
      fields_str
  | TTuple _ ->
    failwith "[Z3 model]: Pretty-printing of unnamed structs not supported"
  | TEnum name ->
    (* The value associated to the enum is a single argument *)
    let e' = List.hd (Expr.get_args e) in
    let fd = Expr.get_func_decl e in
    let fd_name = Symbol.to_string (FuncDecl.get_name fd) in

    let enum_ctrs = EnumMap.find name ctx.ctx_decl.ctx_enums in
    let case =
      List.find
        (fun (ctr, _) ->
          String.equal fd_name (Marked.unmark (EnumConstructor.get_info ctr)))
        enum_ctrs
    in

    Format.asprintf "%s (%s)" fd_name (print_z3model_expr ctx (snd case) e')
  | TOption _ -> failwith "[Z3 model]: Pretty-printing of options not supported"
  | TArrow _ -> failwith "[Z3 model]: Pretty-printing of arrows not supported"
  | TArray _ ->
    (* For now, only the length of arrays is modeled *)
    Format.asprintf "(length = %s)" (Expr.to_string e)
  | TAny -> failwith "[Z3 model]: Pretty-printing of Any not supported"

(** [print_model] pretty prints a Z3 model, used to exhibit counter examples
    where verification conditions are not satisfied. The context [ctx] is useful
    to retrieve the mapping between Z3 variables and Catala variables, and to
    retrieve type information about the variables that was lost during the
    translation (e.g., by translating a date to an integer) **)
let print_model (ctx : context) (model : Model.model) : string =
  let decls = Model.get_decls model in
  Format.asprintf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
       (fun fmt d ->
         if FuncDecl.get_arity d = 0 then
           (* Constant case *)
           match Model.get_const_interp model d with
           (* TODO: Better handling of this case *)
           | None ->
             failwith
               "[Z3 model]: A variable does not have an associated Z3 solution"
           (* Print "name : value\n" *)
           | Some e ->
             let symbol_name = Symbol.to_string (FuncDecl.get_name d) in
             let v = StringMap.find symbol_name ctx.ctx_z3vars in
             Format.fprintf fmt "%s %s : %s"
               (Cli.with_style [ANSITerminal.blue] "%s" "-->")
               (Cli.with_style [ANSITerminal.yellow] "%s" (Bindlib.name_of v))
               (print_z3model_expr ctx (Var.Map.find v ctx.ctx_var) e)
         else
           (* Declaration d is a function *)
           match Model.get_func_interp model d with
           (* TODO: Better handling of this case *)
           | None ->
             failwith
               "[Z3 model]: A variable does not have an associated Z3 solution"
           (* Print "name : value\n" *)
           | Some f ->
             let symbol_name = Symbol.to_string (FuncDecl.get_name d) in
             let v = StringMap.find symbol_name ctx.ctx_z3vars in
             Format.fprintf fmt "%s %s : %s"
               (Cli.with_style [ANSITerminal.blue] "%s" "-->")
               (Cli.with_style [ANSITerminal.yellow] "%s" (Bindlib.name_of v))
               (* TODO: Model of a Z3 function should be pretty-printed *)
               (Model.FuncInterp.to_string f)))
    decls

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal
    type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Sort.sort =
  match t with
  | TBool -> Boolean.mk_sort ctx.ctx_z3
  | TUnit -> fst ctx.ctx_z3unit
  | TInt -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> Arithmetic.Real.mk_sort ctx.ctx_z3
  | TMoney -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  (* Dates are encoded as integers, corresponding to the number of days since
     Jan 1, 1900 *)
  | TDate -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TDuration -> Arithmetic.Integer.mk_sort ctx.ctx_z3

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : naked_typ) : context * Sort.sort =
  match t with
  | TLit t -> ctx, translate_typ_lit ctx t
  | TStruct name -> find_or_create_struct ctx name
  | TTuple _ -> failwith "[Z3 encoding] TTuple type not supported"
  | TEnum e -> find_or_create_enum ctx e
  | TOption _ -> failwith "[Z3 encoding] TOption type not supported"
  | TArrow _ -> failwith "[Z3 encoding] TArrow type not supported"
  | TArray _ ->
    (* For now, we are only encoding the (symbolic) length of an array.
       Ultimately, the type of an array should also contain its elements *)
    ctx, Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TAny -> failwith "[Z3 encoding] TAny type not supported"

(** [find_or_create_enum] attempts to retrieve the Z3 sort corresponding to the
    Catala enumeration [enum]. If no such sort exists yet, it constructs it by
    creating a Z3 constructor for each Catala constructor of [enum], and adds it
    to the context *)
and find_or_create_enum (ctx : context) (enum : EnumName.t) :
    context * Sort.sort =
  (* Creates a Z3 constructor corresponding to the Catala constructor [c] *)
  let create_constructor (ctx : context) (c : EnumConstructor.t * typ) :
      context * Datatype.Constructor.constructor =
    let name, ty = c in
    let name = Marked.unmark (EnumConstructor.get_info name) in
    let ctx, arg_z3_ty = translate_typ ctx (Marked.unmark ty) in

    (* The mk_constructor_s Z3 function is not so well documented. From my
       understanding, its argument are: - a string corresponding to the name of
       the constructor - a recognizer as a symbol corresponding to the name
       (unsure why) - a list of symbols corresponding to the arguments of the
       constructor - a list of types, that must be of the same length as the
       list of arguments - a list of sort_refs, of the same length as the list
       of arguments. I'm unsure what this corresponds to *)
    ( ctx,
      Datatype.mk_constructor_s ctx.ctx_z3 name
        (Symbol.mk_string ctx.ctx_z3 name)
        (* We need a name for the argument of the constructor, we arbitrary pick
           the name of the constructor to which we append the special character
           "!" and the integer 0 *)
        [Symbol.mk_string ctx.ctx_z3 (name ^ "!0")]
        (* The type of the argument, translated to a Z3 sort *)
        [Some arg_z3_ty]
        [Sort.get_id arg_z3_ty] )
  in

  match EnumMap.find_opt enum ctx.ctx_z3datatypes with
  | Some e -> ctx, e
  | None ->
    let ctrs = EnumMap.find enum ctx.ctx_decl.ctx_enums in
    let ctx, z3_ctrs = List.fold_left_map create_constructor ctx ctrs in
    let z3_enum =
      Datatype.mk_sort_s ctx.ctx_z3
        (Marked.unmark (EnumName.get_info enum))
        z3_ctrs
    in
    add_z3enum enum z3_enum ctx, z3_enum

(** [find_or_create_struct] attemps to retrieve the Z3 sort corresponding to the
    struct [s]. If no such sort exists yet, we construct it as a datatype with
    one constructor taking all the fields as arguments, and add it to the
    context *)
and find_or_create_struct (ctx : context) (s : StructName.t) :
    context * Sort.sort =
  match StructMap.find_opt s ctx.ctx_z3structs with
  | Some s -> ctx, s
  | None ->
    let s_name = Marked.unmark (StructName.get_info s) in
    let fields = StructMap.find s ctx.ctx_decl.ctx_structs in
    let z3_fieldnames =
      List.map
        (fun f ->
          Marked.unmark (StructFieldName.get_info (fst f))
          |> Symbol.mk_string ctx.ctx_z3)
        fields
    in
    let ctx, z3_fieldtypes =
      List.fold_left_map
        (fun ctx f -> Marked.unmark (snd f) |> translate_typ ctx)
        ctx fields
    in
    let z3_sortrefs = List.map Sort.get_id z3_fieldtypes in
    let mk_struct_s = "mk!" ^ s_name in
    let z3_mk_struct =
      Datatype.mk_constructor_s ctx.ctx_z3 mk_struct_s
        (Symbol.mk_string ctx.ctx_z3 mk_struct_s)
        z3_fieldnames
        (List.map (fun x -> Some x) z3_fieldtypes)
        z3_sortrefs
    in

    let z3_struct = Datatype.mk_sort_s ctx.ctx_z3 s_name [z3_mk_struct] in
    add_z3struct s z3_struct ctx, z3_struct

(** [translate_lit] returns the Z3 expression as a literal corresponding to
    [lit] **)
let translate_lit (ctx : context) (l : lit) : Expr.expr =
  match l with
  | LBool b ->
    if b then Boolean.mk_true ctx.ctx_z3 else Boolean.mk_false ctx.ctx_z3
  | LEmptyError -> failwith "[Z3 encoding] LEmptyError literals not supported"
  | LInt n ->
    Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (Runtime.integer_to_int n)
  | LRat r ->
    Arithmetic.Real.mk_numeral_s ctx.ctx_z3
      (string_of_float (Runtime.decimal_to_float r))
  | LMoney m ->
    let z3_m = Runtime.integer_to_int (Runtime.money_to_cents m) in
    Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 z3_m
  | LUnit -> snd ctx.ctx_z3unit
  (* Encoding a date as an integer corresponding to the number of days since Jan
     1, 1900 *)
  | LDate d -> Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int d)
  | LDuration d ->
    let y, m, d = Runtime.duration_to_years_months_days d in
    if y <> 0 || m <> 0 then
      failwith
        "[Z3 encoding]: Duration literals containing years or months not \
         supported";
    Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 d

(** [find_or_create_funcdecl] attempts to retrieve the Z3 function declaration
    corresponding to the variable [v]. If no such function declaration exists
    yet, we construct it and add it to the context, thus requiring to return a
    new context *)
let find_or_create_funcdecl (ctx : context) (v : typed expr Var.t) :
    context * FuncDecl.func_decl =
  match Var.Map.find_opt v ctx.ctx_funcdecl with
  | Some fd -> ctx, fd
  | None -> (
    (* Retrieves the Catala type of the function [v] *)
    let f_ty = Var.Map.find v ctx.ctx_var in
    match Marked.unmark f_ty with
    | TArrow (t1, t2) ->
      let ctx, z3_t1 = translate_typ ctx (Marked.unmark t1) in
      let ctx, z3_t2 = translate_typ ctx (Marked.unmark t2) in
      let name = unique_name v in
      let fd = FuncDecl.mk_func_decl_s ctx.ctx_z3 name [z3_t1] z3_t2 in
      let ctx = add_funcdecl v fd ctx in
      let ctx = add_z3var name v ctx in
      ctx, fd
    | TAny ->
      failwith
        "[Z3 Encoding] A function being applied has type TAny, the type was \
         not fully inferred"
    | _ ->
      failwith
        "[Z3 Encoding] Ill-formed VC, a function application does not have a \
         function type")

let is_leap_year = Runtime.is_leap_year
(* Replace with [Dates_calc.Dates.is_leap_year] when existing *)

(** [translate_op] returns the Z3 expression corresponding to the application of
    [op] to the arguments [args] **)
let rec translate_op (ctx : context) (op : operator) (args : 'm expr list) :
    context * Expr.expr =
  match op with
  | Ternop _top ->
    let _e1, _e2, _e3 =
      match args with
      | [e1; e2; e3] -> e1, e2, e3
      | _ ->
        failwith
          (Format.asprintf
             "[Z3 encoding] Ill-formed ternary operator application: %a"
             (Shared_ast.Expr.format ctx.ctx_decl)
             ( EApp
                 ( (EOp op, Untyped { pos = Pos.no_pos }),
                   List.map
                     (fun arg -> Bindlib.unbox (Shared_ast.Expr.untype arg))
                     args ),
               Untyped { pos = Pos.no_pos } ))
    in

    failwith "[Z3 encoding] ternary operator application not supported"
  | Binop bop -> (
    (* Special case for GetYear comparisons *)
    match bop, args with
    | Lt KInt, [(EApp ((EOp (Unop GetYear), _), [e1]), _); (ELit (LInt n), _)]
      ->
      let n = Runtime.integer_to_int n in
      let ctx, e1 = translate_expr ctx e1 in
      let e2 =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year n))
      in
      (* e2 corresponds to the first day of the year n. GetYear e1 < e2 can thus
         be directly translated as < in the Z3 encoding using the number of
         days *)
      ctx, Arithmetic.mk_lt ctx.ctx_z3 e1 e2
    | Lte KInt, [(EApp ((EOp (Unop GetYear), _), [e1]), _); (ELit (LInt n), _)]
      ->
      let ctx, e1 = translate_expr ctx e1 in
      let nb_days = if is_leap_year n then 365 else 364 in
      let n = Runtime.integer_to_int n in
      (* We want that the year corresponding to e1 is smaller or equal to n. We
         encode this as the day corresponding to e1 is smaller or equal than the
         last day of the year [n], which is Jan 1st + 365 days if [n] is a leap
         year, Jan 1st + 364 else *)
      let e2 =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year n) + nb_days)
      in
      ctx, Arithmetic.mk_le ctx.ctx_z3 e1 e2
    | Gt KInt, [(EApp ((EOp (Unop GetYear), _), [e1]), _); (ELit (LInt n), _)]
      ->
      let ctx, e1 = translate_expr ctx e1 in
      let nb_days = if is_leap_year n then 365 else 364 in
      let n = Runtime.integer_to_int n in
      (* We want that the year corresponding to e1 is greater to n. We encode
         this as the day corresponding to e1 is greater than the last day of the
         year [n], which is Jan 1st + 365 days if [n] is a leap year, Jan 1st +
         364 else *)
      let e2 =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year n) + nb_days)
      in
      ctx, Arithmetic.mk_gt ctx.ctx_z3 e1 e2
    | Gte KInt, [(EApp ((EOp (Unop GetYear), _), [e1]), _); (ELit (LInt n), _)]
      ->
      let n = Runtime.integer_to_int n in
      let ctx, e1 = translate_expr ctx e1 in
      let e2 =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year n))
      in
      (* e2 corresponds to the first day of the year n. GetYear e1 >= e2 can
         thus be directly translated as >= in the Z3 encoding using the number
         of days *)
      ctx, Arithmetic.mk_ge ctx.ctx_z3 e1 e2
    | Eq, [(EApp ((EOp (Unop GetYear), _), [e1]), _); (ELit (LInt n), _)] ->
      let n = Runtime.integer_to_int n in
      let ctx, e1 = translate_expr ctx e1 in
      let min_date =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year n))
      in
      let max_date =
        Arithmetic.Integer.mk_numeral_i ctx.ctx_z3
          (date_to_int (date_of_year (n + 1)))
      in
      ( ctx,
        Boolean.mk_and ctx.ctx_z3
          [
            Arithmetic.mk_ge ctx.ctx_z3 e1 min_date;
            Arithmetic.mk_lt ctx.ctx_z3 e1 max_date;
          ] )
    | _ -> (
      let ctx, e1, e2 =
        match args with
        | [e1; e2] ->
          let ctx, e1 = translate_expr ctx e1 in
          let ctx, e2 = translate_expr ctx e2 in
          ctx, e1, e2
        | _ ->
          failwith
            (Format.asprintf
               "[Z3 encoding] Ill-formed binary operator application: %a"
               (Shared_ast.Expr.format ctx.ctx_decl)
               ( EApp
                   ( (EOp op, Untyped { pos = Pos.no_pos }),
                     List.map
                       (fun arg ->
                         arg |> Shared_ast.Expr.untype |> Bindlib.unbox)
                       args ),
                 Untyped { pos = Pos.no_pos } ))
      in

      match bop with
      | And -> ctx, Boolean.mk_and ctx.ctx_z3 [e1; e2]
      | Or -> ctx, Boolean.mk_or ctx.ctx_z3 [e1; e2]
      | Xor -> ctx, Boolean.mk_xor ctx.ctx_z3 e1 e2
      | Add KInt | Add KRat | Add KMoney | Add KDate | Add KDuration ->
        ctx, Arithmetic.mk_add ctx.ctx_z3 [e1; e2]
      | Sub KInt | Sub KRat | Sub KMoney | Sub KDate | Sub KDuration ->
        ctx, Arithmetic.mk_sub ctx.ctx_z3 [e1; e2]
      | Mult KInt | Mult KRat | Mult KMoney | Mult KDate | Mult KDuration ->
        ctx, Arithmetic.mk_mul ctx.ctx_z3 [e1; e2]
      | Div KInt | Div KRat | Div KMoney ->
        ctx, Arithmetic.mk_div ctx.ctx_z3 e1 e2
      | Div _ ->
        failwith
          "[Z3 encoding] application of non-integer binary operator Div not \
           supported"
      | Lt KInt | Lt KRat | Lt KMoney | Lt KDate | Lt KDuration ->
        ctx, Arithmetic.mk_lt ctx.ctx_z3 e1 e2
      | Lte KInt | Lte KRat | Lte KMoney | Lte KDate | Lte KDuration ->
        ctx, Arithmetic.mk_le ctx.ctx_z3 e1 e2
      | Gt KInt | Gt KRat | Gt KMoney | Gt KDate | Gt KDuration ->
        ctx, Arithmetic.mk_gt ctx.ctx_z3 e1 e2
      | Gte KInt | Gte KRat | Gte KMoney | Gte KDate | Gte KDuration ->
        ctx, Arithmetic.mk_ge ctx.ctx_z3 e1 e2
      | Eq -> ctx, Boolean.mk_eq ctx.ctx_z3 e1 e2
      | Neq -> ctx, Boolean.mk_not ctx.ctx_z3 (Boolean.mk_eq ctx.ctx_z3 e1 e2)
      | Map ->
        failwith
          "[Z3 encoding] application of binary operator Map not supported"
      | Concat ->
        failwith
          "[Z3 encoding] application of binary operator Concat not supported"
      | Filter ->
        failwith
          "[Z3 encoding] application of binary operator Filter not supported"))
  | Unop uop -> (
    let ctx, e1 =
      match args with
      | [e1] -> translate_expr ctx e1
      | _ ->
        failwith
          (Format.asprintf
             "[Z3 encoding] Ill-formed unary operator application: %a"
             (Shared_ast.Expr.format ctx.ctx_decl)
             ( EApp
                 ( (EOp op, Untyped { pos = Pos.no_pos }),
                   List.map
                     (fun arg -> arg |> Shared_ast.Expr.untype |> Bindlib.unbox)
                     args ),
               Untyped { pos = Pos.no_pos } ))
    in

    match uop with
    | Not -> ctx, Boolean.mk_not ctx.ctx_z3 e1
    | Minus _ ->
      failwith "[Z3 encoding] application of unary operator Minus not supported"
    (* Omitting the log from the VC *)
    | Log _ -> ctx, e1
    | Length ->
      (* For now, an array is only its symbolic length. We simply return it *)
      ctx, e1
    | IntToRat ->
      failwith
        "[Z3 encoding] application of unary operator IntToRat not supported"
    | MoneyToRat ->
      failwith
        "[Z3 encoding] application of unary operator MoneyToRat not supported"
    | RatToMoney ->
      failwith
        "[Z3 encoding] application of unary operator RatToMoney not supported"
    | GetDay ->
      failwith
        "[Z3 encoding] application of unary operator GetDay not supported"
    | GetMonth ->
      failwith
        "[Z3 encoding] application of unary operator GetMonth not supported"
    | GetYear ->
      failwith
        "[Z3 encoding] GetYear operator only supported in comparisons with \
         literal"
    | FirstDayOfMonth ->
      failwith
        "[Z3 encoding] FirstDayOfMonth operator only supported in comparisons \
         with literal"
    | LastDayOfMonth ->
      failwith
        "[Z3 encoding] LastDayOfMonth operator only supported in comparisons \
         with literal"
    | RoundDecimal ->
      failwith "[Z3 encoding] RoundDecimal operator  not implemented yet"
    | RoundMoney ->
      failwith "[Z3 encoding] RoundMoney operator  not implemented yet")

(** [translate_expr] translate the expression [vc] to its corresponding Z3
    expression **)
and translate_expr (ctx : context) (vc : typed expr) : context * Expr.expr =
  let translate_match_arm
      (head : Expr.expr)
      (ctx : context)
      (e : 'm expr * FuncDecl.func_decl list) : context * Expr.expr =
    let e, accessors = e in
    match Marked.unmark e with
    | EAbs (e, _) ->
      (* Create a fresh Catala variable to substitue and obtain the body *)
      let fresh_v = Var.make "arm!tmp" in
      let fresh_e = EVar fresh_v in

      (* Invariant: Catala enums always have exactly one argument *)
      let accessor = List.hd accessors in
      let proj = Expr.mk_app ctx.ctx_z3 accessor [head] in
      (* The fresh variable should be substituted by a projection into the enum
         in the body, we add this to the context *)
      let ctx = add_z3matchsubst fresh_v proj ctx in

      let body = Bindlib.msubst e [| fresh_e |] in
      translate_expr ctx body
    (* Invariant: Catala match arms are always lambda*)
    | _ -> failwith "[Z3 encoding] : Arms branches inside VCs should be lambdas"
  in

  match Marked.unmark vc with
  | EVar v -> (
    match Var.Map.find_opt v ctx.ctx_z3matchsubsts with
    | None ->
      (* We are in the standard case, where this is a true Catala variable *)
      let t = Var.Map.find v ctx.ctx_var in
      let name = unique_name v in
      let ctx = add_z3var name v ctx in
      let ctx, ty = translate_typ ctx (Marked.unmark t) in
      let z3_var = Expr.mk_const_s ctx.ctx_z3 name ty in
      let ctx =
        match Marked.unmark t with
        (* If we are creating a new array, we need to log that its length is
           greater than 0 *)
        | TArray _ ->
          add_z3constraint
            (Arithmetic.mk_ge ctx.ctx_z3 z3_var
               (Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 0))
            ctx
        | _ -> ctx
      in

      ctx, z3_var
    | Some e ->
      (* This variable is a temporary variable generated during VC translation
         of a match. It actually corresponds to applying an accessor to an enum,
         the corresponding Z3 expression was previously stored in the context *)
      ctx, e)
  | ETuple _ -> failwith "[Z3 encoding] ETuple unsupported"
  | ETupleAccess (s, idx, oname, _tys) ->
    let name =
      match oname with
      | None ->
        failwith "[Z3 encoding]: ETupleAccess of unnamed struct unsupported"
      | Some n -> n
    in
    let ctx, z3_struct = find_or_create_struct ctx name in
    (* This datatype should have only one constructor, corresponding to
       mk_struct. The accessors of this constructor correspond to the field
       accesses *)
    let accessors = List.hd (Datatype.get_accessors z3_struct) in
    let accessor = List.nth accessors idx in
    let ctx, s = translate_expr ctx s in
    ctx, Expr.mk_app ctx.ctx_z3 accessor [s]
  | EInj (e, idx, en, _tys) ->
    (* This node corresponds to creating a value for the enumeration [en], by
       calling the [idx]-th constructor of enum [en], with argument [e] *)
    let ctx, z3_enum = find_or_create_enum ctx en in
    let ctx, z3_arg = translate_expr ctx e in
    let ctrs = Datatype.get_constructors z3_enum in
    (* This should always succeed if the expression is well-typed in dcalc *)
    let ctr = List.nth ctrs idx in
    ctx, Expr.mk_app ctx.ctx_z3 ctr [z3_arg]
  | EMatch (arg, arms, enum) ->
    (* We will encode a match as a new variable, tmp_v, and add to the hypotheses
       that this variable is equal to the conjunction of all `A? arg ==> tmp_v == body`,
       where `A? arg ==> body` is an arm of the match *)

    (* We use the Var module to ensure that all names for temporary variables will be
       fresh, and thus will not clash in Z3 *)
    let fresh_v = Var.make "z3!match_tmp" in
    let name = unique_name fresh_v in
    let match_ty = failwith "GET TYPE" in
    let ctx, z3_ty = translate_typ ctx (Marked.unmark match_ty) in
    let z3_var = Expr.mk_const_s ctx.ctx_z3 name z3_ty in

    let ctx, z3_enum = find_or_create_enum ctx enum in
    let ctx, z3_arg = translate_expr ctx arg in
    let _ctx, z3_arms =
      List.fold_left_map
        (translate_match_arm z3_arg)
        ctx
        (List.combine arms (Datatype.get_accessors z3_enum))
    in
    let z3_arms =
      List.map2
        (fun r arm ->
          (* Encodes A? arg ==> z3_var = body *)
          let is_r = Expr.mk_app ctx.ctx_z3 r [z3_arg] in
          let eq = Boolean.mk_eq ctx.ctx_z3 z3_var arm in
          Boolean.mk_implies ctx.ctx_z3 is_r eq)
        (Datatype.get_recognizers z3_enum)
        z3_arms
    in

    (* Add the definition of z3_var to the hypotheses *)
    let ctx = add_z3constraint (Boolean.mk_and ctx.ctx_z3 z3_arms) ctx in
    ctx, z3_var

  | EArray _ -> failwith "[Z3 encoding] EArray unsupported"
  | ELit l -> ctx, translate_lit ctx l
  | EAbs _ -> failwith "[Z3 encoding] EAbs unsupported"
  | EApp (head, args) -> (
    match Marked.unmark head with
    | EOp op -> translate_op ctx op args
    | EVar v ->
      let ctx, fd = find_or_create_funcdecl ctx v in
      (* Fold_right to preserve the order of the arguments: The head argument is
         appended at the head *)
      let ctx, z3_args =
        List.fold_right
          (fun arg (ctx, acc) ->
            let ctx, z3_arg = translate_expr ctx arg in
            ctx, z3_arg :: acc)
          args (ctx, [])
      in
      ctx, Expr.mk_app ctx.ctx_z3 fd z3_args
    | _ ->
      failwith
        "[Z3 encoding] EApp node: Catala function calls should only include \
         operators or function names")
  | EAssert _ -> failwith "[Z3 encoding] EAssert unsupported"
  | EOp _ -> failwith "[Z3 encoding] EOp unsupported"
  | EDefault _ -> failwith "[Z3 encoding] EDefault unsupported"
  | EIfThenElse (e_if, e_then, e_else) ->
    (* Encode this as (e_if ==> e_then) /\ (not e_if ==> e_else) *)
    let ctx, z3_if = translate_expr ctx e_if in
    let ctx, z3_then = translate_expr ctx e_then in
    let ctx, z3_else = translate_expr ctx e_else in
    ( ctx,
      Boolean.mk_and ctx.ctx_z3
        [
          Boolean.mk_implies ctx.ctx_z3 z3_if z3_then;
          Boolean.mk_implies ctx.ctx_z3
            (Boolean.mk_not ctx.ctx_z3 z3_if)
            z3_else;
        ] )
  | ErrorOnEmpty _ -> failwith "[Z3 encoding] ErrorOnEmpty unsupported"

(** [create_z3unit] creates a Z3 sort and expression corresponding to the unit
    type and value respectively. Concretely, we represent unit as a tuple with 0
    elements **)
let create_z3unit (ctx : Z3.context) : Z3.context * (Sort.sort * Expr.expr) =
  let unit_sort = Tuple.mk_sort ctx (Symbol.mk_string ctx "unit") [] [] in
  let mk_unit = Tuple.get_mk_decl unit_sort in
  let unit_val = Expr.mk_app ctx mk_unit [] in
  ctx, (unit_sort, unit_val)

module Backend = struct
  type backend_context = context
  type vc_encoding = Z3.Expr.expr

  let print_encoding (vc : vc_encoding) : string = Expr.to_string vc

  type model = Z3.Model.model
  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  let solve_vc_encoding (ctx : backend_context) (encoding : vc_encoding) :
      solver_result =
    let solver = Z3.Solver.mk_solver ctx.ctx_z3 None in
    (* We take the negation of the query to check for possible
       counterexamples *)
    let query = Boolean.mk_not ctx.ctx_z3 encoding in
    (* Add all the hypotheses stored in the context *)
    let query_and_hyps = query :: ctx.ctx_z3constraints in
    Z3.Solver.add solver query_and_hyps;
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> ProvenTrue
    | SATISFIABLE -> ProvenFalse (Z3.Solver.get_model solver)
    | UNKNOWN -> Unknown

  let print_model (ctx : backend_context) (m : model) : string =
    print_model ctx m

  let is_model_empty (m : model) : bool = List.length (Z3.Model.get_decls m) = 0

  let translate_expr (ctx : backend_context) (e : typed expr) =
    translate_expr ctx e

  let init_backend () =
    Cli.debug_print "Running Z3 version %s" Version.to_string

  let make_context
      (decl_ctx : decl_ctx)
      (free_vars_typ : (typed expr, typ) Var.Map.t) : backend_context =
    let cfg =
      (if !Cli.disable_counterexamples then [] else ["model", "true"])
      @ ["proof", "false"]
    in
    let z3_ctx = mk_context cfg in
    let z3_ctx, z3unit = create_z3unit z3_ctx in
    {
      ctx_z3 = z3_ctx;
      ctx_decl = decl_ctx;
      ctx_var = free_vars_typ;
      ctx_funcdecl = Var.Map.empty;
      ctx_z3vars = StringMap.empty;
      ctx_z3datatypes = EnumMap.empty;
      ctx_z3matchsubsts = Var.Map.empty;
      ctx_z3structs = StructMap.empty;
      ctx_z3unit = z3unit;
      ctx_z3constraints = [];
    }
end

module Io = Io.MakeBackendIO (Backend)
