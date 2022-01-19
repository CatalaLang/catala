(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Dcalc
open Ast
open Z3

module StringMap : Map.S with type key = String.t = Map.Make (String)

type context = {
  ctx_z3 : Z3.context;
  (* The Z3 context, used to create symbols and expressions *)
  ctx_decl : decl_ctx;
  (* The declaration context from the Catala program, containing information to precisely pretty
     print Catala expressions *)
  ctx_var : typ Pos.marked VarMap.t;
  (* A map from Catala variables to their types, needed to create Z3 expressions of the right
     sort *)
  ctx_funcdecl : FuncDecl.func_decl VarMap.t;
  (* A map from Catala function names (represented as variables) to Z3 function declarations, used
     to only define once functions in Z3 queries *)
  ctx_z3vars : Var.t StringMap.t;
  (* A map from strings, corresponding to Z3 symbol names, to the Catala variable they represent.
     Used when to pretty-print Z3 models when a counterexample is generated *)
  ctx_z3datatypes : Sort.sort EnumMap.t;
  (* A map from Catala enumeration names to the corresponding Z3 sort, from which we can retrieve
     constructors and accessors *)
  ctx_z3matchsubsts : Expr.expr VarMap.t;
  (* A map from Catala temporary variables, generated when translating a match, to the corresponding
     enum accessor call as a Z3 expression *)
  ctx_z3structs : Sort.sort StructMap.t;
      (* A map from Catala struct names to the corresponding Z3 sort, from which we can retrieve the
         constructor and the accessors *)
}
(** The context contains all the required information to encode a VC represented as a Catala term to
    Z3. The fields [ctx_decl] and [ctx_var] are computed before starting the translation to Z3, and
    are thus unmodified throughout the translation. The [ctx_z3] context is an OCaml abstraction on
    top of an underlying C++ imperative implementation, it is therefore only created once.
    Unfortunately, the maps [ctx_funcdecl], [ctx_z3vars], and [ctx_z3datatypes] are computed
    dynamically during the translation requiring us to pass the context around in a functional way **)

(** [add_funcdecl] adds the mapping between the Catala variable [v] and the Z3 function declaration
    [fd] to the context **)
let add_funcdecl (v : Var.t) (fd : FuncDecl.func_decl) (ctx : context) : context =
  { ctx with ctx_funcdecl = VarMap.add v fd ctx.ctx_funcdecl }

(** [add_z3var] adds the mapping between [name] and the Catala variable [v] to the context **)
let add_z3var (name : string) (v : Var.t) (ctx : context) : context =
  { ctx with ctx_z3vars = StringMap.add name v ctx.ctx_z3vars }

(** [add_z3enum] adds the mapping between the Catala enumeration [enum] and the corresponding Z3
    datatype [sort] to the context **)
let add_z3enum (enum : EnumName.t) (sort : Sort.sort) (ctx : context) : context =
  { ctx with ctx_z3datatypes = EnumMap.add enum sort ctx.ctx_z3datatypes }

(** [add_z3var] adds the mapping between temporary variable [v] and the Z3 expression [e]
    representing an accessor application to the context **)
let add_z3matchsubst (v : Var.t) (e : Expr.expr) (ctx : context) : context =
  { ctx with ctx_z3matchsubsts = VarMap.add v e ctx.ctx_z3matchsubsts }

(** [add_z3struct] adds the mapping between the Catala struct [s] and the corresponding Z3 datatype
    [sort] to the context **)
let add_z3struct (s : StructName.t) (sort : Sort.sort) (ctx : context) : context =
  { ctx with ctx_z3structs = StructMap.add s sort ctx.ctx_z3structs }

(** For the Z3 encoding of Catala programs, we define the "day 0" as Jan 1, 1900 **)
let base_day = CalendarLib.Date.make 1900 1 1

(** [unique_name] returns the full, unique name corresponding to variable [v], as given by Bindlib **)
let unique_name (v : Var.t) : string =
  Format.asprintf "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

(** [date_to_int] translates [date] to an integer corresponding to the number of days since Jan 1,
    1900 **)
let date_to_int (d : Runtime.date) : int =
  (* Alternatively, could expose this from Runtime as a (noop) coercion, but would allow to break
     abstraction more easily elsewhere *)
  let date : CalendarLib.Date.t = CalendarLib.Printer.Date.from_string (Runtime.date_to_string d) in
  let period = CalendarLib.Date.sub date base_day in
  CalendarLib.Date.Period.nb_days period

(** [date_of_year] translates a [year], represented as an integer into an OCaml date corresponding
    to Jan 1st of the same year *)
let date_of_year (year : int) = Runtime.date_of_numbers year 1 1

(** Returns the date (as a string) corresponding to nb days after the base day, defined here as Jan
    1, 1900 **)
let nb_days_to_date (nb : int) : string =
  CalendarLib.Printer.Date.to_string
    (CalendarLib.Date.add base_day (CalendarLib.Date.Period.day nb))

(** [print_z3model_expr] pretty-prints the value [e] given by a Z3 model according to the Catala
    type [ty], corresponding to [e] **)
let rec print_z3model_expr (ctx : context) (ty : typ Pos.marked) (e : Expr.expr) : string =
  let print_lit (ty : typ_lit) =
    match ty with
    (* TODO: Print boolean according to current language *)
    | TBool -> Expr.to_string e
    | TUnit -> failwith "[Z3 model]: Pretty-printing of unit literals not supported"
    | TInt -> Expr.to_string e
    | TRat -> failwith "[Z3 model]: Pretty-printing of rational literals not supported"
    (* TODO: Print the right money symbol according to language *)
    | TMoney ->
        let z3_str = Expr.to_string e in
        (* The Z3 model returns an integer corresponding to the amount of cents. We reformat it as
           dollars *)
        let to_dollars s = Runtime.money_to_string (Runtime.money_of_cents_string s) in
        if String.contains z3_str '-' then
          Format.asprintf "-%s $" (to_dollars (String.sub z3_str 3 (String.length z3_str - 4)))
        else Format.asprintf "%s $" (to_dollars z3_str)
    (* The Z3 date representation corresponds to the number of days since Jan 1, 1900. We
       pretty-print it as the actual date *)
    (* TODO: Use differnt dates conventions depending on the language ? *)
    | TDate -> nb_days_to_date (int_of_string (Expr.to_string e))
    | TDuration -> failwith "[Z3 model]: Pretty-printing of duration literals not supported"
  in

  match Pos.unmark ty with
  | TLit ty -> print_lit ty
  | TTuple (_, Some name) ->
      let s = StructMap.find name ctx.ctx_decl.ctx_structs in
      let get_fieldname (fn : StructFieldName.t) : string =
        Pos.unmark (StructFieldName.get_info fn)
      in
      let fields =
        List.map2
          (fun (fn, ty) e ->
            Format.asprintf "-- %s : %s" (get_fieldname fn) (print_z3model_expr ctx ty e))
          s (Expr.get_args e)
      in

      let fields_str = String.concat " " fields in

      Format.asprintf "%s { %s }" (Pos.unmark (StructName.get_info name)) fields_str
  | TTuple (_, None) -> failwith "[Z3 model]: Pretty-printing of unnamed structs not supported"
  | TEnum (_tys, name) ->
      (* The value associated to the enum is a single argument *)
      let e' = List.hd (Expr.get_args e) in
      let fd = Expr.get_func_decl e in
      let fd_name = Symbol.to_string (FuncDecl.get_name fd) in

      let enum_ctrs = EnumMap.find name ctx.ctx_decl.ctx_enums in
      let case =
        List.find
          (fun (ctr, _) -> String.equal fd_name (Pos.unmark (EnumConstructor.get_info ctr)))
          enum_ctrs
      in

      Format.asprintf "%s (%s)" fd_name (print_z3model_expr ctx (snd case) e')
  | TArrow _ -> failwith "[Z3 model]: Pretty-printing of arrows not supported"
  | TArray _ -> failwith "[Z3 model]: Pretty-printing of arrays not supported"
  | TAny -> failwith "[Z3 model]: Pretty-printing of Any not supported"

(** [print_model] pretty prints a Z3 model, used to exhibit counter examples where verification
    conditions are not satisfied. The context [ctx] is useful to retrieve the mapping between Z3
    variables and Catala variables, and to retrieve type information about the variables that was
    lost during the translation (e.g., by translating a date to an integer) **)
let print_model (ctx : context) (model : Model.model) : string =
  let decls = Model.get_decls model in

  Format.asprintf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
       (fun fmt d ->
         match Model.get_const_interp model d with
         (* TODO: Better handling of this case *)
         | None -> failwith "[Z3 model]: A variable does not have an associated Z3 solution"
         (* Prints "name : value\n" *)
         | Some e ->
             if FuncDecl.get_arity d = 0 then
               (* Constant case *)
               let symbol_name = Symbol.to_string (FuncDecl.get_name d) in
               let v = StringMap.find symbol_name ctx.ctx_z3vars in
               Format.fprintf fmt "%s %s : %s"
                 (Cli.print_with_style [ ANSITerminal.blue ] "%s" "-->")
                 (Cli.print_with_style [ ANSITerminal.yellow ] "%s" (Bindlib.name_of v))
                 (print_z3model_expr ctx (VarMap.find v ctx.ctx_var) e)
             else failwith "[Z3 model]: Printing of functions is not yet supported"))
    decls

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Sort.sort =
  match t with
  | TBool -> Boolean.mk_sort ctx.ctx_z3
  | TUnit -> failwith "[Z3 encoding] TUnit type not supported"
  | TInt -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> failwith "[Z3 encoding] TRat type not supported"
  | TMoney -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  (* Dates are encoded as integers, corresponding to the number of days since Jan 1, 1900 *)
  | TDate -> Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TDuration -> failwith "[Z3 encoding] TDuration type not supported"

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : typ) : context * Sort.sort =
  match t with
  | TLit t -> (ctx, translate_typ_lit ctx t)
  | TTuple (_, Some name) -> find_or_create_struct ctx name
  | TTuple (_, None) -> failwith "[Z3 encoding] TTuple type of unnamed struct not supported"
  | TEnum (_, e) -> find_or_create_enum ctx e
  | TArrow _ -> failwith "[Z3 encoding] TArrow type not supported"
  | TArray _ -> failwith "[Z3 encoding] TArray type not supported"
  | TAny -> failwith "[Z3 encoding] TAny type not supported"

(** [find_or_create_enum] attempts to retrieve the Z3 sort corresponding to the Catala enumeration
    [enum]. If no such sort exists yet, it constructs it by creating a Z3 constructor for each
    Catala constructor of [enum], and adds it to the context *)
and find_or_create_enum (ctx : context) (enum : EnumName.t) : context * Sort.sort =
  (* Creates a Z3 constructor corresponding to the Catala constructor [c] *)
  let create_constructor (ctx : context) (c : EnumConstructor.t * typ Pos.marked) :
      context * Datatype.Constructor.constructor =
    let name, ty = c in
    let name = Pos.unmark (EnumConstructor.get_info name) in
    let ctx, arg_z3_ty = translate_typ ctx (Pos.unmark ty) in

    (* The mk_constructor_s Z3 function is not so well documented. From my understanding, its
       argument are: - a string corresponding to the name of the constructor - a recognizer as a
       symbol corresponding to the name (unsure why) - a list of symbols corresponding to the
       arguments of the constructor - a list of types, that must be of the same length as the list
       of arguments - a list of sort_refs, of the same length as the list of arguments. I'm unsure
       what this corresponds to *)
    ( ctx,
      Datatype.mk_constructor_s ctx.ctx_z3 name
        (Symbol.mk_string ctx.ctx_z3 name)
        (* We need a name for the argument of the constructor, we arbitrary pick the name of the
           constructor to which we append the special character "!" and the integer 0 *)
        [ Symbol.mk_string ctx.ctx_z3 (name ^ "!0") ]
        (* The type of the argument, translated to a Z3 sort *)
        [ Some arg_z3_ty ]
        [ Sort.get_id arg_z3_ty ] )
  in

  match EnumMap.find_opt enum ctx.ctx_z3datatypes with
  | Some e -> (ctx, e)
  | None ->
      let ctrs = EnumMap.find enum ctx.ctx_decl.ctx_enums in
      let ctx, z3_ctrs = List.fold_left_map create_constructor ctx ctrs in
      let z3_enum = Datatype.mk_sort_s ctx.ctx_z3 (Pos.unmark (EnumName.get_info enum)) z3_ctrs in
      (add_z3enum enum z3_enum ctx, z3_enum)

(** [find_or_create_struct] attemps to retrieve the Z3 sort corresponding to the struct [s]. If no
    such sort exists yet, we construct it as a datatype with one constructor taking all the fields
    as arguments, and add it to the context *)
and find_or_create_struct (ctx : context) (s : StructName.t) : context * Sort.sort =
  match StructMap.find_opt s ctx.ctx_z3structs with
  | Some s -> (ctx, s)
  | None ->
      let s_name = Pos.unmark (StructName.get_info s) in
      let fields = StructMap.find s ctx.ctx_decl.ctx_structs in
      let z3_fieldnames =
        List.map
          (fun f -> Pos.unmark (StructFieldName.get_info (fst f)) |> Symbol.mk_string ctx.ctx_z3)
          fields
      in
      let ctx, z3_fieldtypes =
        List.fold_left_map (fun ctx f -> Pos.unmark (snd f) |> translate_typ ctx) ctx fields
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

      let z3_struct = Datatype.mk_sort_s ctx.ctx_z3 s_name [ z3_mk_struct ] in
      (add_z3struct s z3_struct ctx, z3_struct)

(** [translate_lit] returns the Z3 expression as a literal corresponding to [lit] **)
let translate_lit (ctx : context) (l : lit) : Expr.expr =
  match l with
  | LBool b -> if b then Boolean.mk_true ctx.ctx_z3 else Boolean.mk_false ctx.ctx_z3
  | LEmptyError -> failwith "[Z3 encoding] LEmptyError literals not supported"
  | LInt n -> Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (Runtime.integer_to_int n)
  | LRat _ -> failwith "[Z3 encoding] LRat literals not supported"
  | LMoney m ->
      let z3_m = Runtime.integer_to_int (Runtime.money_to_cents m) in
      Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 z3_m
  | LUnit -> failwith "[Z3 encoding] LUnit literals not supported"
  (* Encoding a date as an integer corresponding to the number of days since Jan 1, 1900 *)
  | LDate d -> Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int d)
  | LDuration _ -> failwith "[Z3 encoding] LDuration literals not supported"

(** [find_or_create_funcdecl] attempts to retrieve the Z3 function declaration corresponding to the
    variable [v]. If no such function declaration exists yet, we construct it and add it to the
    context, thus requiring to return a new context *)
let find_or_create_funcdecl (ctx : context) (v : Var.t) : context * FuncDecl.func_decl =
  match VarMap.find_opt v ctx.ctx_funcdecl with
  | Some fd -> (ctx, fd)
  | None -> (
      (* Retrieves the Catala type of the function [v] *)
      let f_ty = VarMap.find v ctx.ctx_var in
      match Pos.unmark f_ty with
      | TArrow (t1, t2) ->
          let ctx, z3_t1 = translate_typ ctx (Pos.unmark t1) in
          let ctx, z3_t2 = translate_typ ctx (Pos.unmark t2) in
          let name = unique_name v in
          let fd = FuncDecl.mk_func_decl_s ctx.ctx_z3 name [ z3_t1 ] z3_t2 in
          let ctx = add_funcdecl v fd ctx in
          let ctx = add_z3var name v ctx in
          (ctx, fd)
      | _ ->
          failwith
            "[Z3 Encoding] Ill-formed VC, a function application does not have a function type")

(** [translate_op] returns the Z3 expression corresponding to the application of [op] to the
    arguments [args] **)
let rec translate_op (ctx : context) (op : operator) (args : expr Pos.marked list) :
    context * Expr.expr =
  match op with
  | Ternop _top ->
      let _e1, _e2, _e3 =
        match args with
        | [ e1; e2; e3 ] -> (e1, e2, e3)
        | _ ->
            failwith
              (Format.asprintf "[Z3 encoding] Ill-formed ternary operator application: %a"
                 (Print.format_expr ctx.ctx_decl)
                 (EApp ((EOp op, Pos.no_pos), args), Pos.no_pos))
      in

      failwith "[Z3 encoding] ternary operator application not supported"
  | Binop bop -> (
      (* Special case for GetYear comparisons *)
      match (bop, args) with
      | Lt KInt, [ (EApp ((EOp (Unop GetYear), _), [ e1 ]), _); (ELit (LInt n), _) ] ->
          let n = Runtime.integer_to_int n in
          let ctx, e1 = translate_expr ctx e1 in
          let e2 = Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int (date_of_year n)) in
          (* e2 corresponds to the first day of the year n. GetYear e1 < e2 can thus be directly
             translated as < in the Z3 encoding using the number of days *)
          (ctx, Arithmetic.mk_lt ctx.ctx_z3 e1 e2)
      | Lte KInt, [ (EApp ((EOp (Unop GetYear), _), [ e1 ]), _); (ELit (LInt n), _) ] ->
          let n = Runtime.integer_to_int n in
          let ctx, e1 = translate_expr ctx e1 in
          let nb_days = if CalendarLib.Date.is_leap_year n then 365 else 364 in
          (* We want that the year corresponding to e1 is smaller or equal to n. We encode this as
             the day corresponding to e1 is smaller or equal than the last day of the year [n],
             which is Jan 1st + 365 days if [n] is a leap year, Jan 1st + 364 else *)
          let e2 =
            Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int (date_of_year n) + nb_days)
          in
          (ctx, Arithmetic.mk_le ctx.ctx_z3 e1 e2)
      | Gt KInt, [ (EApp ((EOp (Unop GetYear), _), [ e1 ]), _); (ELit (LInt n), _) ] ->
          let n = Runtime.integer_to_int n in
          let ctx, e1 = translate_expr ctx e1 in
          let nb_days = if CalendarLib.Date.is_leap_year n then 365 else 364 in
          (* We want that the year corresponding to e1 is greater to n. We encode this as the day
             corresponding to e1 is greater than the last day of the year [n], which is Jan 1st +
             365 days if [n] is a leap year, Jan 1st + 364 else *)
          let e2 =
            Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int (date_of_year n) + nb_days)
          in
          (ctx, Arithmetic.mk_gt ctx.ctx_z3 e1 e2)
      | Gte KInt, [ (EApp ((EOp (Unop GetYear), _), [ e1 ]), _); (ELit (LInt n), _) ] ->
          let n = Runtime.integer_to_int n in
          let ctx, e1 = translate_expr ctx e1 in
          let e2 = Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (date_to_int (date_of_year n)) in
          (* e2 corresponds to the first day of the year n. GetYear e1 >= e2 can thus be directly
             translated as >= in the Z3 encoding using the number of days *)
          (ctx, Arithmetic.mk_ge ctx.ctx_z3 e1 e2)
      | _ -> (
          let ctx, e1, e2 =
            match args with
            | [ e1; e2 ] ->
                let ctx, e1 = translate_expr ctx e1 in
                let ctx, e2 = translate_expr ctx e2 in
                (ctx, e1, e2)
            | _ ->
                failwith
                  (Format.asprintf "[Z3 encoding] Ill-formed binary operator application: %a"
                     (Print.format_expr ctx.ctx_decl)
                     (EApp ((EOp op, Pos.no_pos), args), Pos.no_pos))
          in

          match bop with
          | And -> (ctx, Boolean.mk_and ctx.ctx_z3 [ e1; e2 ])
          | Or -> (ctx, Boolean.mk_or ctx.ctx_z3 [ e1; e2 ])
          | Xor -> (ctx, Boolean.mk_xor ctx.ctx_z3 e1 e2)
          | Add KInt -> (ctx, Arithmetic.mk_add ctx.ctx_z3 [ e1; e2 ])
          | Add _ ->
              failwith "[Z3 encoding] application of non-integer binary operator Add not supported"
          | Sub KInt -> (ctx, Arithmetic.mk_sub ctx.ctx_z3 [ e1; e2 ])
          | Sub _ ->
              failwith "[Z3 encoding] application of non-integer binary operator Sub not supported"
          | Mult KInt -> (ctx, Arithmetic.mk_mul ctx.ctx_z3 [ e1; e2 ])
          | Mult _ ->
              failwith "[Z3 encoding] application of non-integer binary operator Mult not supported"
          | Div KInt -> (ctx, Arithmetic.mk_div ctx.ctx_z3 e1 e2)
          | Div _ ->
              failwith "[Z3 encoding] application of non-integer binary operator Div not supported"
          | Lt KInt | Lt KMoney | Lt KDate -> (ctx, Arithmetic.mk_lt ctx.ctx_z3 e1 e2)
          | Lt _ ->
              failwith
                "[Z3 encoding] application of non-integer or money binary operator Lt not supported"
          | Lte KInt | Lte KMoney | Lte KDate -> (ctx, Arithmetic.mk_le ctx.ctx_z3 e1 e2)
          | Lte _ ->
              failwith
                "[Z3 encoding] application of non-integer or money binary operator Lte not \
                 supported"
          | Gt KInt | Gt KMoney | Gt KDate -> (ctx, Arithmetic.mk_gt ctx.ctx_z3 e1 e2)
          | Gt _ ->
              failwith
                "[Z3 encoding] application of non-integer or money binary operator Gt not supported"
          | Gte KInt | Gte KMoney | Gte KDate -> (ctx, Arithmetic.mk_ge ctx.ctx_z3 e1 e2)
          | Gte _ ->
              failwith
                "[Z3 encoding] application of non-integer or money binary operator Gte not \
                 supported"
          | Eq -> (ctx, Boolean.mk_eq ctx.ctx_z3 e1 e2)
          | Neq -> (ctx, Boolean.mk_not ctx.ctx_z3 (Boolean.mk_eq ctx.ctx_z3 e1 e2))
          | Map -> failwith "[Z3 encoding] application of binary operator Map not supported"
          | Concat -> failwith "[Z3 encoding] application of binary operator Concat not supported"
          | Filter -> failwith "[Z3 encoding] application of binary operator Filter not supported"))
  | Unop uop -> (
      let ctx, e1 =
        match args with
        | [ e1 ] -> translate_expr ctx e1
        | _ ->
            failwith
              (Format.asprintf "[Z3 encoding] Ill-formed unary operator application: %a"
                 (Print.format_expr ctx.ctx_decl)
                 (EApp ((EOp op, Pos.no_pos), args), Pos.no_pos))
      in

      match uop with
      | Not -> (ctx, Boolean.mk_not ctx.ctx_z3 e1)
      | Minus _ -> failwith "[Z3 encoding] application of unary operator Minus not supported"
      (* Omitting the log from the VC *)
      | Log _ -> (ctx, e1)
      | Length -> failwith "[Z3 encoding] application of unary operator Length not supported"
      | IntToRat -> failwith "[Z3 encoding] application of unary operator IntToRat not supported"
      | GetDay -> failwith "[Z3 encoding] application of unary operator GetDay not supported"
      | GetMonth -> failwith "[Z3 encoding] application of unary operator GetMonth not supported"
      | GetYear ->
          failwith "[Z3 encoding] GetYear operator only supported in comparisons with literal")

(** [translate_expr] translate the expression [vc] to its corresponding Z3 expression **)
and translate_expr (ctx : context) (vc : expr Pos.marked) : context * Expr.expr =
  let translate_match_arm (head : Expr.expr) (ctx : context)
      (e : expr Pos.marked * FuncDecl.func_decl list) : context * Expr.expr =
    let e, accessors = e in
    match Pos.unmark e with
    | EAbs (e, _) ->
        (* Create a fresh Catala variable to substitue and obtain the body *)
        let fresh_v = Var.make ("arm!tmp", Pos.no_pos) in
        let fresh_e = EVar (fresh_v, Pos.no_pos) in

        (* Invariant: Catala enums always have exactly one argument *)
        let accessor = List.hd accessors in
        let proj = Expr.mk_app ctx.ctx_z3 accessor [ head ] in
        (* The fresh variable should be substituted by a projection into the enum in the body, we
           add this to the context *)
        let ctx = add_z3matchsubst fresh_v proj ctx in

        let body = Bindlib.msubst (Pos.unmark e) [| fresh_e |] in
        translate_expr ctx body
    (* Invariant: Catala match arms are always lambda*)
    | _ -> failwith "[Z3 encoding] : Arms branches inside VCs should be lambdas"
  in

  match Pos.unmark vc with
  | EVar v -> (
      match VarMap.find_opt (Pos.unmark v) ctx.ctx_z3matchsubsts with
      | None ->
          (* We are in the standard case, where this is a true Catala variable *)
          let v = Pos.unmark v in
          let t = VarMap.find v ctx.ctx_var in
          let name = unique_name v in
          let ctx = add_z3var name v ctx in
          let ctx, ty = translate_typ ctx (Pos.unmark t) in
          (ctx, Expr.mk_const_s ctx.ctx_z3 name ty)
      | Some e ->
          (* This variable is a temporary variable generated during VC translation of a match. It
             actually corresponds to applying an accessor to an enum, the corresponding Z3
             expression was previously stored in the context *)
          (ctx, e))
  | ETuple _ -> failwith "[Z3 encoding] ETuple unsupported"
  | ETupleAccess (s, idx, oname, _tys) ->
      let name =
        match oname with
        | None -> failwith "[Z3 encoding]: ETupleAccess of unnamed struct unsupported"
        | Some n -> n
      in
      let ctx, z3_struct = find_or_create_struct ctx name in
      (* This datatype should have only one constructor, corresponding to mk_struct. The accessors
         of this constructor correspond to the field accesses *)
      let accessors = List.hd (Datatype.get_accessors z3_struct) in
      let accessor = List.nth accessors idx in
      let ctx, s = translate_expr ctx s in
      (ctx, Expr.mk_app ctx.ctx_z3 accessor [ s ])
  | EInj _ -> failwith "[Z3 encoding] EInj unsupported"
  | EMatch (arg, arms, enum) ->
      let ctx, z3_enum = find_or_create_enum ctx enum in
      let ctx, z3_arg = translate_expr ctx arg in
      let _ctx, z3_arms =
        List.fold_left_map (translate_match_arm z3_arg) ctx
          (List.combine arms (Datatype.get_accessors z3_enum))
      in
      let z3_arms =
        List.map2
          (fun r arm ->
            (* Encodes A? arg ==> body *)
            let is_r = Expr.mk_app ctx.ctx_z3 r [ z3_arg ] in
            Boolean.mk_implies ctx.ctx_z3 is_r arm)
          (Datatype.get_recognizers z3_enum)
          z3_arms
      in
      (ctx, Boolean.mk_and ctx.ctx_z3 z3_arms)
  | EArray _ -> failwith "[Z3 encoding] EArray unsupported"
  | ELit l -> (ctx, translate_lit ctx l)
  | EAbs _ -> failwith "[Z3 encoding] EAbs unsupported"
  | EApp (head, args) -> (
      match Pos.unmark head with
      | EOp op -> translate_op ctx op args
      | EVar v ->
          let ctx, fd = find_or_create_funcdecl ctx (Pos.unmark v) in
          (* Fold_right to preserve the order of the arguments: The head argument is appended at the
             head *)
          let ctx, z3_args =
            List.fold_right
              (fun arg (ctx, acc) ->
                let ctx, z3_arg = translate_expr ctx arg in
                (ctx, z3_arg :: acc))
              args (ctx, [])
          in
          (ctx, Expr.mk_app ctx.ctx_z3 fd z3_args)
      | _ ->
          failwith
            "[Z3 encoding] EApp node: Catala function calls should only include operators or \
             function names")
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
            Boolean.mk_implies ctx.ctx_z3 (Boolean.mk_not ctx.ctx_z3 z3_if) z3_else;
          ] )
  | ErrorOnEmpty _ -> failwith "[Z3 encoding] ErrorOnEmpty unsupported"

module Backend = struct
  type backend_context = context

  type vc_encoding = Z3.Expr.expr

  let print_encoding (vc : vc_encoding) : string = Expr.to_string vc

  type model = Z3.Model.model

  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  let solve_vc_encoding (ctx : backend_context) (encoding : vc_encoding) : solver_result =
    let solver = Z3.Solver.mk_solver ctx.ctx_z3 None in
    Z3.Solver.add solver [ Boolean.mk_not ctx.ctx_z3 encoding ];
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> ProvenTrue
    | SATISFIABLE -> ProvenFalse (Z3.Solver.get_model solver)
    | UNKNOWN -> Unknown

  let print_model (ctx : backend_context) (m : model) : string = print_model ctx m

  let is_model_empty (m : model) : bool = List.length (Z3.Model.get_decls m) = 0

  let translate_expr (ctx : backend_context) (e : Dcalc.Ast.expr Pos.marked) = translate_expr ctx e

  let init_backend () = Cli.debug_print (Format.asprintf "Running Z3 version %s" Version.to_string)

  let make_context (decl_ctx : decl_ctx) (free_vars_typ : typ Pos.marked VarMap.t) : backend_context
      =
    let cfg = [ ("model", "true"); ("proof", "false") ] in
    let z3_ctx = mk_context cfg in
    {
      ctx_z3 = z3_ctx;
      ctx_decl = decl_ctx;
      ctx_var = free_vars_typ;
      ctx_funcdecl = VarMap.empty;
      ctx_z3vars = StringMap.empty;
      ctx_z3datatypes = EnumMap.empty;
      ctx_z3matchsubsts = VarMap.empty;
      ctx_z3structs = StructMap.empty;
    }
end

module Io = Io.MakeSolverIO (Backend)
