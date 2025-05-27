open Catala_utils
open Shared_ast
open Shared_ast.Interpreter
open Trace_ast
open Print_trace

(*
let convert_expr_to_expr_with_holes :
type d t. ((d, yes) interpr_kind, t) gexpr -> ((d dcalc_slicing, yes) interpr_kind, t) gexpr =
fun e -> 
  let m = Mark.get e in 
  match Mark.remove e with
    | ELit l -> Mark.add m (ELit l)
    | _ -> Mark.add m EEmpty
*)


let evaluate_expr_with_trace :
    type d t.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, t) gexpr ->
    ((d, yes) interpr_kind, t) gexpr * ((d, yes) interpr_kind, t) Trace_ast.t =
fun ctx lang e ->
  let exception FatalError of Runtime.error * t mark * ((d, yes) interpr_kind, t) Trace_ast.t in
  let raise_fatal_error err m tr = raise (FatalError (err, m, TrFatalError { err ; tr })) in
  let rec evaluate_expr_with_trace_aux :
      decl_ctx ->
      (((d, yes) interpr_kind, t) gexpr, ((d, yes) interpr_kind, t) gexpr) Var.Map.t ->
      Global.backend_lang ->
      ((d, yes) interpr_kind, t) gexpr ->
      ((d, yes) interpr_kind, t) gexpr * ((d, yes) interpr_kind, t) Trace_ast.t =
  fun ctx local_ctx lang e ->
    (*let debug_print, e =
      Expr.take_attr e (function DebugPrint { label } -> Some label | _ -> None)
    in*)
    let m = Mark.get e in
    let pos = Expr.mark_pos m in
    (*(match debug_print with
    | None -> fun r -> r
    | Some label_opt ->
      fun r ->
        Message.debug "%a%a @{<grey>(at %s)@}"
          (fun ppf -> function
            | Some s -> Format.fprintf ppf "@{<bold;yellow>%s@} = " s
            | None -> ())
          label_opt (Print.expr ()) r (Pos.to_string_short pos);
        r)
    @@*)
    match Mark.remove e with
    | EVar x -> (
      match Var.Map.find_opt x local_ctx with
        | Some v -> v, TrVar x
        | None -> 
          Message.error ~pos "%a" Format.pp_print_text
            "free variable found at evaluation (should not happen if term was \
            well-typed)"
      )
    | EExternal { name } ->
      let path =
        match Mark.remove name with
        | External_value td -> TopdefName.path td
        | External_scope s -> ScopeName.path s
      in
      let ty =
        try
          match Mark.remove name with
          | External_value name ->
            let typ, _vis = TopdefName.Map.find name ctx.ctx_topdefs in
            typ
          | External_scope name ->
            let scope_info = ScopeName.Map.find name ctx.ctx_scopes in
            ( TArrow
                ( [TStruct scope_info.in_struct_name, pos],
                  (TStruct scope_info.out_struct_name, pos) ),
              pos )
        with TopdefName.Map.Not_found _ | ScopeName.Map.Not_found _ ->
          Message.error ~pos "Reference to %a@ could@ not@ be@ resolved"
            Print.external_ref name
      in
      let runtime_path =
        ( List.map ModuleName.to_string path,
          match Mark.remove name with
          | External_value name -> TopdefName.base name
          | External_scope name -> ScopeName.base name )
        (* we have the guarantee that the two cases won't collide because they
          have different capitalisation rules inherited from the input *)
      in
      let o = Runtime.lookup_value runtime_path in
      (runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m ty o), TrExternal { name }
    | EApp { f = e1; args; tys } -> (
      let e1, trf = evaluate_expr_with_trace_aux ctx local_ctx lang e1 in
      let args, trargs = List.split(List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) args) in
      match Mark.remove e1 with
      | EAbs { binder; _ } ->
        if Bindlib.mbinder_arity binder = List.length args then
          let vars, body = Bindlib.unmbind binder in
          let local_ctx = List.fold_left2 (fun ctx var arg -> Var.Map.update var (fun _ -> Some arg) ctx) local_ctx (Array.to_list vars) args in
          let v, trv = evaluate_expr_with_trace_aux ctx local_ctx lang body in 
            (v, TrApp {trf; trargs; tys; trv})
        else
          Message.error ~pos "wrong function call, expected %d arguments, got %d"
            (Bindlib.mbinder_arity binder)
            (List.length args)
      | ECustom { obj; targs; tret } ->
        (* Applies the arguments one by one to the curried form *)
        let o =
          List.fold_left2
            (fun fobj targ arg ->
              let arg =
                val_to_runtime (fun ctx -> evaluate_expr ctx lang) ctx targ arg
              in
              let f : Obj.t -> Obj.t =
                if Obj.tag fobj = Obj.first_non_constant_constructor_tag then
                  (* Function is not a closure, but a pair, we assume closure
                    conversion has been done *)
                  let (f, x0) : ('a -> Obj.t -> Obj.t) * 'a = Obj.obj fobj in
                  f x0
                else Obj.obj fobj
              in
              f arg)
            obj targs args
        in
        let v = runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m tret o in 
        v, TrApp {trf; trargs; tys; trv = TrExpr v}
      | _ ->
        Message.error ~pos ~internal:true "%a%a" Format.pp_print_text
          "function has not been reduced to a lambda at evaluation (should not \
          happen if the term was well-typed"
          (fun ppf e ->
            if Global.options.debug then Format.fprintf ppf ":@ %a" Expr.format e
            else ())
          e1)
    | EAppOp { op; args; tys } ->
      let args, trargs = List.split(List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) args) in
      let v = evaluate_operator (evaluate_expr ctx lang) op m lang args in
      v, TrAppOp { op; trargs; tys; trv = TrExpr v }
    | EAbs { binder; pos; tys } -> e, TrAbs { binder; pos; tys }
    | ELit l -> e, TrLit l
    | EPos _ -> assert false
    | ECustom { obj; targs; tret } -> e, TrCustom { obj; targs; tret }
    | EEmpty -> e, TrEmpty
    | EStruct { fields = es; name } ->
      let fields, es = List.split (StructField.Map.bindings es) in
      let es, tres = List.split(List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) es) in
      (Mark.add m
        (EStruct
          {
            fields =
              StructField.Map.of_seq
                (Seq.zip (List.to_seq fields) (List.to_seq es));
            name;
          })
      ), 
      TrStruct 
        {
          fields =
            StructField.Map.of_seq
              (Seq.zip (List.to_seq fields) (List.to_seq tres));
          name;
        }
    | EStructAccess { e; name = s; field } -> (
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e in
      match Mark.remove e with
      | EStruct { fields = es; name } -> (
        if not (StructName.equal s name) then
          Message.error
            ~extra_pos:["", pos; "", Expr.pos e]
            "%a" Format.pp_print_text
            "Error during struct access: not the same structs (should not happen \
            if the term was well-typed)";
        match StructField.Map.find_opt field es with
        | Some e' -> (e', TrStructAccess { name; tr; field })
        | None ->
          Message.error ~pos:(Expr.pos e)
            "Invalid field access %a@ in@ struct@ %a@ (should not happen if the \
            term was well-typed)"
            StructField.format field StructName.format s)
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "The expression %a@ should@ be@ a@ struct@ %a@ but@ is@ not@ (should \
          not happen if the term was well-typed)"
          (Print.UserFacing.expr lang)
          e StructName.format s)
    | ETuple es -> 
      let v, trv = List.split(List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) es) in
      (Mark.add m (ETuple v)), TrTuple trv
    | ETupleAccess { e = e1; index; size } -> (
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e1 in
      match e with
      | ETuple es, _ when List.length es = size -> (List.nth es index), TrTupleAccess { tr; index; size }
      | e ->
        Message.error ~pos:(Expr.pos e)
          "The expression %a@ was@ expected@ to@ be@ a@ tuple@ of@ size@ %d@ \
          (should not happen if the term was well-typed)"
          (Print.UserFacing.expr lang)
          e size)
    | EInj { e; name; cons } ->
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e in
      (Mark.add m (EInj { e; name; cons })), TrInj { tr; name; cons }
    | EMatch { e; cases; name } -> (
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e in
      match Mark.remove e with
      | EInj { e = e1; cons; name = name' } ->
        if not (EnumName.equal name name') then
          Message.error
            ~extra_pos:["", Expr.pos e; "", Expr.pos e1]
            "%a" Format.pp_print_text
            "Error during match: two different enums found (should not happen if \
            the term was well-typed)";
        let es_n =
          match EnumConstructor.Map.find_opt cons cases with
          | Some es_n -> es_n
          | None ->
            Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
              "sum type index error (should not happen if the term was \
              well-typed)"
        in
        let ty =
          EnumConstructor.Map.find cons (EnumName.Map.find name ctx.ctx_enums)
        in
        let new_e = Mark.add m (EApp { f = es_n; args = [e1]; tys = [ty] }) in
        let v, tv = evaluate_expr_with_trace_aux ctx local_ctx lang new_e in 
        let trexpr c = TrExpr c in
        let trcases = EnumConstructor.Map.map trexpr cases in
        v, TrMatch {tr; name; cases = EnumConstructor.Map.update cons (fun _ -> Some tv) trcases}
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "Expected a term having a sum type as an argument to a match (should \
          not happen if the term was well-typed")
    | EIfThenElse { cond; etrue; efalse } -> (
      let cond, trcond = evaluate_expr_with_trace_aux ctx local_ctx lang cond in
      match Mark.remove cond with
      | ELit (LBool true) -> 
        let v, trtrue = evaluate_expr_with_trace_aux ctx local_ctx lang etrue in 
        v, TrIfThenElse { trcond; trtrue; trfalse = TrExpr efalse }
      | ELit (LBool false) -> 
        let v, trfalse = evaluate_expr_with_trace_aux ctx local_ctx lang efalse in 
        v, TrIfThenElse { trcond; trtrue = TrExpr etrue; trfalse }
      | _ ->
        Message.error ~pos:(Expr.pos cond) "%a" Format.pp_print_text
          "Expected a boolean literal for the result of this condition (should \
          not happen if the term was well-typed)")
    | EArray es ->
      let es, tres = List.split(List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) es) in
      (Mark.add m (EArray es)), TrArray tres
    | EAssert e' -> (
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e' in
      match Mark.remove e with
      | ELit (LBool true) -> (Mark.add m (ELit LUnit)), TrAssert tr
      | ELit (LBool false) -> (* Mark.add m (EFatalError AssertionFailed) *)
        raise_fatal_error AssertionFailed m (TrAssert tr)
      | _ ->
        Message.error ~pos:(Expr.pos e') "%a" Format.pp_print_text
          "Expected a boolean literal for the result of this assertion (should \
          not happen if the term was well-typed)")
    | EFatalError err -> raise (Runtime.Error (err, [Expr.pos_to_runtime pos]))
    | EErrorOnEmpty e' -> (
      let e, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e' in
      match e with
      | EEmpty, _ -> (* raise Runtime.(Error (NoValue, [Expr.pos_to_runtime pos])) *)
        raise_fatal_error NoValue m (TrErrorOnEmpty tr)
      | exception Runtime.Empty ->
        raise_fatal_error NoValue m (TrErrorOnEmpty tr)
      | e -> e, TrErrorOnEmpty tr
      )
    | EDefault { excepts; just; cons } -> (
      let excepts, trexcepts = List.split (List.map (evaluate_expr_with_trace_aux ctx local_ctx lang) excepts) in
      let empty_count = List.length (List.filter is_empty_error excepts) in
      match List.length excepts - empty_count with
      | 0 -> (
        let just, trjust = evaluate_expr_with_trace_aux ctx local_ctx lang just in
        match Mark.remove just with
        | ELit (LBool true) -> 
            let v, trcons = evaluate_expr_with_trace_aux ctx local_ctx lang cons in
            v, TrDefault { trexcepts; trjust; trcons }
        | ELit (LBool false) -> 
            (Mark.copy e EEmpty), TrDefault { trexcepts; trjust; trcons = TrExpr cons }
        | _ ->
          Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
            "Default justification has not been reduced to a boolean at \
            evaluation (should not happen if the term was well-typed")
      | 1 -> (List.find (fun sub -> not (is_empty_error sub)) excepts), 
              TrDefault { trexcepts; trjust = TrExpr just; trcons = TrExpr cons }
      | _ ->
        (*let poslist =
          List.filter_map
            (fun ex ->
              if is_empty_error ex then None
              else Some Expr.(pos_to_runtime (pos ex)))
            excepts
        in
        raise Runtime.(Error (Conflict, poslist))*)
        raise_fatal_error Conflict m (TrDefault { trexcepts; trjust = TrExpr just; trcons = TrExpr cons })
      )
    | EPureDefault e -> let v, tr = evaluate_expr_with_trace_aux ctx local_ctx lang e in v, TrPureDefault tr
    | _ -> .
  in
  try evaluate_expr_with_trace_aux ctx Var.Map.empty lang e
  with 
    | FatalError (err, m, tr) -> (Mark.add m (EFatalError err)), tr

(*
let evaluate_expr_trace :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr =
 fun ctx lang e ->
  Runtime.reset_log ();
  evaluate_expr_with_trace ctx lang e
  (*
  Fun.protect
    (fun () -> evaluate_expr_with_trace ctx lang e)
    ~finally:(fun () ->
      match Global.options.trace with
      | None -> ()
      | Some (lazy ppf) ->
        let trace = Runtime.retrieve_log () in
        if trace = [] then
          (* FIXME: we call evaluate twice: once to generate the scope function
             and once for the actual call scope call. A proper fix would be to
             disable the trace for the the first pass. *)
          ()
        else
          let output_trace fmt =
            match Global.options.trace_format with
            | Human ->
              Format.pp_open_vbox ppf 0;
              ignore @@ List.fold_left (print_log ppf lang) 0 trace;
              Format.pp_close_box ppf ()
            | JSON ->
              Format.fprintf fmt "@[<v 2>[@,";
              Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
                Format.pp_print_string fmt
                (List.map Runtime.Json.raw_event trace);
              Format.fprintf fmt "]@]@."
          in
          Fun.protect
            (fun () -> output_trace ppf)
            ~finally:(fun () -> Format.pp_print_flush ppf ()))
  *)
*)

let evaluate_expr_safe :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr * ((d, yes) interpr_kind, 't) Trace_ast.t =
 fun ctx lang e ->
  try evaluate_expr_with_trace ctx lang e
  with Runtime.Error (err, rpos) ->
    Message.error
      ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) rpos)
      "During evaluation: %a." Format.pp_print_text
      (Runtime.error_message err)

let interpret
    (p : (dcalc, 'm) gexpr program)
    (s : ScopeName.t) (*: _ Trace_ast.t *) =
    Message.with_delayed_errors (fun () ->
      let ctx = p.decl_ctx in
      let e = Expr.unbox (Program.to_expr p s) in
      let v, tr = evaluate_expr_safe p.decl_ctx p.lang (addcustom e) in
      print_newline ();
      print_trace tr;
      print_newline ();
      match v with
      | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e ->
        begin
        (* At this point, the interpreter seeks to execute the scope but does
           not have a way to retrieve input values from the command line. [taus]
           contain the types of the scope arguments. For [context] arguments, we
           can provide an empty thunked term. But for [input] arguments of
           another type, we cannot provide anything so we have to fail. *)
        let application_term = Scope.empty_input_struct_dcalc ctx s_in mark_e in
        let to_interpret =
          Expr.make_app (Expr.box e) [application_term]
            [TStruct s_in, Expr.pos e]
            (Expr.pos e)
        in
        let v2, tr2 = evaluate_expr_safe ctx p.lang (Expr.unbox to_interpret) in
        print_newline ();
        print_trace tr2;
        print_newline ();
        match Mark.remove v2 with
        | EStruct { fields; _ } ->
          List.map
            (fun (fld, e) -> StructField.get_info fld, e)
            (StructField.Map.bindings fields)
        | _ ->
          Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
            "The interpretation of a program should always yield a struct \
             corresponding to the scope variables"
      end
      | _ ->
        Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
          "The interpreter can only interpret terms starting with functions \
           having thunked arguments")
