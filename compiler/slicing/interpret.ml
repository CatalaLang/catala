open Catala_utils
open Shared_ast
open Shared_ast.Interpreter
open Trace_utils

let hole_result m = ok Var.Map.empty (Mark.add m (EFatalError Unreachable)) tranyhole

let find_error_var_ty vts trs =
  List.find (fun (_, tr) ->
    match tr with
    | Trace_ast.TrExpr _ | TrHole _ -> false
    | _ -> true
  ) (List.combine vts trs)
  |> fst
  
let evaluate_expr_with_trace :
    type d t.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, t) gexpr ->
    ((d, yes) interpr_kind, t) gexpr * ((d, yes, yes) slicing_interpr_kind, t) Trace_ast.t =
fun ctx lang e ->
  let exception FatalError of Runtime.error * t mark in
  let raise_soft_fatal_error err m tr = Error (err, m, trfatalerror ~err ~tr) in

  let rec evaluate_expr_list_with_trace_aux :
    decl_ctx ->
    (((d, yes) interpr_kind, t) gexpr, ((d, yes) interpr_kind, t) gexpr) Var.Map.t ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, t) gexpr list ->
    (
      (((d, yes) interpr_kind, t) gexpr, ((d, yes) interpr_kind, t) gexpr) Var.Map.t *
      ((d, yes) interpr_kind, t) gexpr list * ((d, yes, yes) slicing_interpr_kind, t) Trace_ast.t list,
      Runtime.error * t mark * ((d, yes, yes) slicing_interpr_kind, t) Trace_ast.t list
    ) result =
  fun ctx local_ctx lang es ->
    (* if everything is ok, return a triple composed of a context, an expr list and a trace list
      but if there is an error somewhere in the list, it should return a list with holes everywhere except 
      the unique element where it found an error *)
    map_result_with_trace (evaluate_expr_with_trace_aux ctx local_ctx lang) es
  
  and evaluate_expr_with_trace_aux :
      decl_ctx ->
      (((d, yes) interpr_kind, t) gexpr, ((d, yes) interpr_kind, t) gexpr) Var.Map.t ->
      Global.backend_lang ->
      ((d, yes) interpr_kind, t) gexpr ->
      ( 
        (((d, yes) interpr_kind, t) gexpr, ((d, yes) interpr_kind, t) gexpr) Var.Map.t *
        ((d, yes) interpr_kind, t) gexpr * ((d, yes, yes) slicing_interpr_kind, t) Trace_ast.t,
        Runtime.error * t mark * ((d, yes, yes) slicing_interpr_kind, t) Trace_ast.t
      ) result =
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
        | Some v -> ok (Var.Map.singleton x v) v @@ trvar ~var:(Var.translate x) ~value:(addholes v)
        | None -> 
          Message.error ~pos "%a@ Variable : %a@ Context : %a" Format.pp_print_text
            "free variable found at evaluation (should not happen if term was \
            well-typed)" Format_trace.expr e Format_trace.context local_ctx
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
      ok Var.Map.empty (runtime_to_val (fun ctx -> evaluate_expr ctx lang) ctx m ty o) (trexternal ~name)
    | EApp { f = e1; args; tys } -> (
      let* ctxf, e1, trf = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e1
        |>(* When there is an error in the definition of the function, we cannot have access to the name of its arguments 
             So we create temporary ones (that will be removed by the slicer anyway since it is assigned a hole)*)
          let vars = Array.of_list (List.mapi (fun i _ -> Var.make ("tmp"^(string_of_int i))) args) in
          map_error_trace (fun _ trf -> trapp ~trf ~trargs:(List.map trexpr args) ~tys ~vars ~trv:trf) 
      in
      let* ctxargs, args, trargs = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang args
        |> (* To keep the subexpression invariant, I need to let the error behave differently depending on whether 
              the function is a lambda abstraction or a custom or a hole *)
          let vars = match Mark.remove e1 with
            | EAbs { binder; _} -> Array.map Var.translate @@ fst (Bindlib.unmbind binder)
            | _ -> [|Var.make "arg"|] 
          in 
          map_error_trace (fun err trargs ->
            let var, ty = find_error_var_ty (List.combine (Array.to_list vars) tys) trargs in
            let mhole = Mark.add m (EHole ty) in
            let trf = trabs ~binder:(Bindlib.unbox (Expr.bind [|var|] (Expr.box mhole))) ~tys:[ty] ~pos:[Pos.void] in
            trapp ~trf ~trargs ~tys ~vars ~trv:(trvar ~var ~value:(Mark.add m@@ EFatalError err)))
      in
      match Mark.remove e1 with
      | EAbs { binder; _ } ->
        if Bindlib.mbinder_arity binder = List.length args then
          let vars, body = Bindlib.unmbind binder in
          let local_ctx = List.fold_left2 
            (fun ctx var arg -> Var.Map.update var (fun _ -> Some arg) ctx) 
            local_ctx (Array.to_list vars) args 
          in
          let vars = Array.map Var.translate vars in
          let* new_ctx, v, trv = 
            evaluate_expr_with_trace_aux ctx (union_map local_ctx ctxargs) lang body 
            |> map_error_trace (fun _ trv -> trapp ~trf ~trargs ~tys ~vars ~trv)
          in 
          (* We add a context closure here for when there are scope calls *)
          (* It is the part that slows the interpret the most. *)
          (* It could certainly be optimized by handling substitutions differently *)
          let whole_ctx = (union_map new_ctx @@ union_map ctxf ctxargs) in
          let reduced_ctx, _ = substitute_bounded_vars whole_ctx v in
          ok reduced_ctx v @@ trcontextclosure ~context:reduced_ctx ~tr:(trapp ~trf ~trargs ~tys ~vars ~trv)
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
        ok Var.Map.empty v @@ trappcustom ~trcustom:trf ~custom:e ~trargs ~tys ~vargs:args ~v
      | EFatalError Unreachable -> hole_result m
      | _ ->
        Message.error ~pos ~internal:true "%a%a" Format.pp_print_text
          "function has not been reduced to a lambda at evaluation (should not \
          happen if the term was well-typed"
          (fun ppf e ->
            if Global.options.debug then Format.fprintf ppf ":@ %a" Expr.format e
            else ())
          e1)
    | EAppOp { op; args; tys } ->
      let* new_ctx, vargs, trargs = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang args
        |>let mhole = Mark.add m (EHole (TAny, Pos.void)) in
          map_error_trace (fun err trargs -> 
            let merror = Mark.add m (EFatalError err) in
            let vargs = List.map (fun tr -> match tr with Trace_ast.TrExpr _ | TrHole _ -> mhole | _ -> merror) trargs in
            trappop ~op ~trargs ~tys ~vargs ~traux:[])
      in 
      (try
      let* ctxaux, v, traux = 
        map_error_trace (fun _ traux -> trappop ~op ~trargs ~tys ~vargs ~traux) 
        @@
        match fst op, vargs with
        | Length, [(EArray es, _)] ->
          ok Var.Map.empty (Mark.add m @@ ELit (LInt (Runtime.integer_of_int (List.length es)))) []
        | Map, [EAbs {tys = tysf; _},mf as f; (EArray vs, _)] -> 
          (* In this case we need to know the trace of f(v) for every v in vs*)
          let eappf v = Mark.add mf (EApp {f; args = [v]; tys = tysf}) in
          let appf v = evaluate_expr_with_trace_aux ctx local_ctx lang (eappf v) in
          let* new_ctx, vals, traux = map_result_with_trace appf vs in
          ok new_ctx (Mark.add m (EArray vals)) traux
        | Map, [EFatalError Unreachable,_ ; (EArray vs, _)] -> 
          (* There are cases where there is a hole instead of the first agument to map Hole to all values in the array *)
          let vals = List.map (fun (_,mv)-> Mark.add mv (EFatalError Unreachable)) vs in 
          let traux = List.map (fun _ -> tranyhole) vs in
          ok Var.Map.empty (Mark.add m (EArray vals)) traux
        | Map2, [EAbs {tys = tysf; _},mf as f; (EArray vs1, _); (EArray vs2, _)] -> 
          (* In this case we need to know the trace of f(v1, v2) for every v1, v2 in vs1, vs2*)
          let eappf v1 v2 = Mark.add mf (EApp {f; args = [v1; v2]; tys = tysf}) in
          let appf v1 v2 = evaluate_expr_with_trace_aux ctx local_ctx lang (eappf v1 v2) in
          (try 
            let* new_ctx, vals, traux = map_result_with_trace2 appf vs1 vs2 in
            ok new_ctx (Mark.add m (EArray vals)) traux
          with Invalid_argument _ -> raise @@ FatalError (Runtime.NotSameLength,m))
        | Map2, [EFatalError Unreachable,_ ; (EArray vs1, _); (EArray vs2, _);] -> 
          (* There are cases where there is a hole instead of the first agument to map Hole to all values in the arrays *)
          (try 
            let vals = List.map2 (fun (_,mv) _ -> Mark.add mv (EFatalError Unreachable)) vs1 vs2 in 
            let traux = List.map (fun _ -> tranyhole) vs1 in
            ok Var.Map.empty (Mark.add m (EArray vals)) traux
          with Invalid_argument _ -> raise @@ FatalError (Runtime.NotSameLength,m))
        | Reduce, [_; EAbs {tys = tysd; _},md as default; (EArray [], _)] ->
          (* In this case we just need the trace of default() *)
          let eappd v = Mark.add md (EApp {f=default; args = [v]; tys = tysd}) in
          let* new_ctx, v, tr = evaluate_expr_with_trace_aux ctx local_ctx lang 
            (eappd (ELit LUnit, Expr.with_ty m (TLit TUnit, pos))) 
            |> map_error_trace (fun _ tr -> [tr])
          in
          ok new_ctx v [tr]
        | Reduce, [EAbs {tys = tysf; _},mf as f; _; (EArray (v0 :: vn), _)] ->
          (* In this case we need the trace of f(v) for every v fold from f and v0::vn *)
          let eappf v1 v2 = Mark.add mf (EApp {f; args = [v1; v2]; tys = tysf}) in
          let appf v1 v2 = evaluate_expr_with_trace_aux ctx local_ctx lang (eappf v1 v2) in
          fold_result_with_trace appf v0 vn
        | Reduce, [_; _; (EArray [v0], _)] -> 
          (* Need to add this case if there is only one element since slicing will replace the other arguments by holes*)
          ok Var.Map.empty v0 []
        | Filter, [EAbs {tys = tysf; _},mf as f; (EArray vs, _)] ->
          (* In this case we need to keep the trace of f(v) for every v in and its value *)
          (* In order to store these informations, the list will store alternatively a boolean corresponding to f(v) and its trace *)
          let eappf v = Mark.add mf (EApp {f; args = [v]; tys = tysf}) in
          let appf v = evaluate_expr_with_trace_aux ctx local_ctx lang (eappf v) in
          let* new_ctx, vals, traux = filter_result_with_trace appf vs in
          ok new_ctx (Mark.add m (EArray vals)) traux
        | Filter, [_ ; (EArray [], _)] ->
          (* Need to add this case for slicing reasons similar to the others *)
          ok Var.Map.empty (Mark.add m (EArray [])) []
        | Fold, [EAbs {tys = tysf; _},mf as f; init; (EArray vs, _)] -> 
          (* In this case we need the trace of f(v) for every v fold from f init and vs *)
          let eappf v1 v2 = Mark.add mf (EApp {f; args = [v1; v2]; tys = tysf}) in
          let appf v1 v2 = evaluate_expr_with_trace_aux ctx local_ctx lang (eappf v1 v2) in
          fold_result_with_trace appf init vs
        | Fold, [_; init; (EArray [], _)] -> 
          (* Need to add this case if the list is empty since it would will replace the other arguments by holes*)
          ok Var.Map.empty init []
        | (Length | Map | Map2 | Reduce | Filter | Fold) as op, _ -> 
          Message.error "Invalid argument for operator %a@.Expr : %a" (Print.operator ~debug:false) op Format_trace.expr e
        | _ -> (* The other cases do not need to carry any additional trace so we just evaluate them normally*)
          try
            let v = if List.exists (fun v -> match Mark.remove v with EFatalError Unreachable -> true |_ -> false) vargs then 
              Mark.add m @@ EFatalError Unreachable 
            else 
              evaluate_operator (evaluate_expr ctx lang) op m lang vargs 
            in
            ok Var.Map.empty v []
          with 
          | Runtime.Error (DivisionByZero|UncomparableDurations|AmbiguousDateRounding|IndivisibleDurations as err, _) -> raise @@ FatalError (err,m)
      in 
      ok (union_map new_ctx ctxaux) v @@ trappop ~op:(Operator.translate op) ~trargs ~tys ~vargs ~traux
      with
      | FatalError (DivisionByZero|NotSameLength|UncomparableDurations|AmbiguousDateRounding|IndivisibleDurations as err, m) -> 
        raise_soft_fatal_error err m @@ trappop ~op:(Operator.translate op) ~trargs ~tys ~vargs ~traux:[]
      )
      
    | EAbs _ -> (
      match Mark.remove(addholes e) with
      | EAbs { binder; pos; tys } -> 
        (* Functions do not carry contexts in Catala and it would be pretty complicated to
          recover the original expression if we perform substitutions so we will only do them
          at the final value if needed *)
        ok local_ctx e @@ trabs ~binder ~pos ~tys 
      | _ -> assert false
    )
    | ELit l -> ok Var.Map.empty e @@ trlit l
    | EPos _ -> assert false
    | ECustom { obj; targs; tret } -> ok local_ctx e @@ trcustom ~obj ~targs ~tret
    | EEmpty -> ok Var.Map.empty e trempty
    | EStruct { fields = es; name } ->
      let fields, es = List.split (StructField.Map.bindings es) in
      let new_fields lst = StructField.Map.of_seq (Seq.zip (List.to_seq fields) (List.to_seq lst)) in
      let* new_ctx, es, tres = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang es 
        |> map_error_trace (fun _ tres -> trstruct ~name ~fields:(new_fields tres))
      in
      ok new_ctx (Mark.add m (EStruct{ fields = new_fields es; name })) @@ trstruct ~name ~fields:(new_fields tres)
    | EStructAccess { e; name = s; field } -> (
      let* new_ctx, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e 
        |> map_error_trace (fun _ tr -> trstructaccess ~name:s ~tr ~field)
      in
      match Mark.remove e with
      | EStruct { fields = es; name } -> (
        if not (StructName.equal s name) then
          Message.error
            ~extra_pos:["", pos; "", Expr.pos e]
            "%a" Format.pp_print_text
            "Error during struct access: not the same structs (should not happen \
            if the term was well-typed)";
        match StructField.Map.find_opt field es with
        | Some e' -> ok new_ctx e' @@ trstructaccess ~name ~tr ~field
        | None ->
          Message.error ~pos:(Expr.pos e)
            "Invalid field access %a@ in@ struct@ %a@ (should not happen if the \
            term was well-typed)"
            StructField.format field StructName.format s)
      | EFatalError Unreachable -> hole_result m
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "The expression %a@ should@ be@ a@ struct@ %a@ but@ is@ not@ (should \
          not happen if the term was well-typed)"
          (Print.UserFacing.expr lang)
          e StructName.format s)
    | ETuple es -> 
      let* new_ctx, v, trv = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang es 
        |> map_error_trace (fun _ -> trtuple)
      in
      ok new_ctx (Mark.add m (ETuple v)) @@ trtuple trv
    | ETupleAccess { e = e1; index; size } -> (
      let* new_ctx, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e1 
        |> map_error_trace (fun _ tr -> trtupleaccess ~tr ~index ~size)
      in
      match Mark.remove e with
      | ETuple es when List.length es = size -> ok new_ctx (List.nth es index) (trtupleaccess ~tr ~index ~size)
      | EFatalError Unreachable -> hole_result m
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "The expression %a@ was@ expected@ to@ be@ a@ tuple@ of@ size@ %d@ \
          (should not happen if the term was well-typed)"
          (Print.UserFacing.expr lang)
          e size)
    | EInj { e; name; cons } ->
      let* new_ctx, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e 
        |> map_error_trace (fun _ tr -> trinj ~name ~tr ~cons)
      in
      ok new_ctx (Mark.add m (EInj { e; name; cons })) @@ trinj ~tr ~name ~cons
    | EMatch { e; cases; name } -> (
      let trcases = EnumConstructor.Map.map (fun c -> trexpr c) cases in
      let* ctx_cases, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e 
        |> map_error_trace (fun _ tr -> trmatch ~name ~tr ~cases:trcases)
      in
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
        let* new_ctx, v, tv = 
          evaluate_expr_with_trace_aux ctx local_ctx lang new_e 
          |> map_error_trace (fun _ tv -> trmatch ~name ~tr ~cases:(EnumConstructor.Map.update cons (fun _ -> Some tv) trcases))
        in 
        ok (union_map ctx_cases new_ctx) v @@ trmatch ~tr ~name ~cases:(EnumConstructor.Map.update cons (fun _ -> Some tv) trcases)
      | EFatalError Unreachable -> hole_result m
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "Expected a term having a sum type as an argument to a match (should \
          not happen if the term was well-typed")
    | EIfThenElse { cond; etrue; efalse } -> (
      let* ctxcond, cond, trcond = 
        evaluate_expr_with_trace_aux ctx local_ctx lang cond 
        |> map_error_trace (fun _ trcond -> trifthenelse ~trcond ~trtrue:(trexpr etrue) ~trfalse:(trexpr efalse))
      in
      match Mark.remove cond with
      | ELit (LBool true) -> 
        let* new_ctx, v, trtrue = 
          evaluate_expr_with_trace_aux ctx local_ctx lang etrue 
          |> map_error_trace (fun _ trtrue -> trifthenelse ~trcond ~trtrue ~trfalse:(trexpr efalse))
        in 
        ok (union_map ctxcond new_ctx) v @@ trifthenelse ~trcond ~trtrue ~trfalse:(trexpr efalse)     
      | ELit (LBool false) -> 
        let* new_ctx, v, trfalse = 
          evaluate_expr_with_trace_aux ctx local_ctx lang efalse 
          |> map_error_trace (fun _ trfalse -> trifthenelse ~trcond ~trtrue:(trexpr etrue) ~trfalse)
        in 
        ok (union_map ctxcond new_ctx) v @@ trifthenelse ~trcond ~trtrue:(trexpr etrue) ~trfalse
      | EFatalError Unreachable -> hole_result m
      | _ ->
        Message.error ~pos:(Expr.pos cond) "%a" Format.pp_print_text
          "Expected a boolean literal for the result of this condition (should \
          not happen if the term was well-typed)")
    | EArray es ->
      let* new_ctx, es, tres = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang es 
        |> map_error_trace (fun _ -> trarray)
      in
      ok new_ctx (Mark.add m (EArray es)) @@ trarray tres
    | EAssert e' -> (
      let* new_ctx, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e' 
        |> map_error_trace (fun _ -> trassert)
      in
      match Mark.remove e with
      | ELit (LBool true) -> ok new_ctx (Mark.add m (ELit LUnit)) @@ trassert tr
      | ELit (LBool false) -> (* Mark.add m (EFatalError AssertionFailed) *)
        raise_soft_fatal_error Runtime.AssertionFailed m (trassert tr)
      | _ ->
        Message.error ~pos:(Expr.pos e') "%a" Format.pp_print_text
          "Expected a boolean literal for the result of this assertion (should \
          not happen if the term was well-typed)"
    )
    | EFatalError Unreachable -> (*It's okay to reach that point but the result should not matter*)
      hole_result m
    | EFatalError err -> raise (Runtime.Error (err, [Expr.pos_to_runtime pos]))
    | EErrorOnEmpty e' -> (
      let* new_ctx, e, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e' 
        |> map_error_trace (fun _ -> trerroronempty)
      in
      match e with
      | EEmpty, _ | exception Runtime.Empty ->
        raise_soft_fatal_error Runtime.NoValue m (trerroronempty tr)
      | e -> ok new_ctx e @@ trerroronempty tr
    )
    | EDefault { excepts; just; cons } -> (
      let* ctxexcepts, vexcepts, trexcepts = 
        evaluate_expr_list_with_trace_aux ctx local_ctx lang excepts 
        |> let mhole = Mark.add m (EHole (TAny, Pos.void)) in
        map_error_trace (fun err trexcepts -> 
          let merror = Mark.add m (EFatalError err) in
          trdefault ~trexcepts ~vexcepts:(List.map (fun tr -> match tr with Trace_ast.TrExpr _ | TrHole _ -> mhole | _ -> merror) trexcepts) ~trjust:(trexpr just) ~trcons:(trexpr cons))
      in
      let empty_count = List.length (List.filter is_empty_error vexcepts) in
      match List.length vexcepts - empty_count with
      | 0 -> (
        let* ctxjust, just, trjust = 
          evaluate_expr_with_trace_aux ctx local_ctx lang just 
          |> map_error_trace (fun _ trjust -> trdefault ~trexcepts ~vexcepts ~trjust ~trcons:(trexpr cons))
        in
        match Mark.remove just with
        | ELit (LBool true) -> 
            let* ctxcons, v, trcons = 
              evaluate_expr_with_trace_aux ctx local_ctx lang cons 
              |> map_error_trace (fun _ trcons -> trdefault ~trexcepts ~vexcepts ~trjust ~trcons)
            in
            ok (union_map ctxexcepts @@ union_map ctxjust ctxcons) v @@ trdefault ~trexcepts ~vexcepts ~trjust ~trcons
        | ELit (LBool false) -> 
            ok (union_map ctxexcepts ctxjust) (Mark.copy e EEmpty) @@ trdefault ~trexcepts ~vexcepts ~trjust ~trcons:(trexpr cons)
        | _ ->
          Message.error ~pos:(Expr.pos e) "%a" Format.pp_print_text
            "Default justification has not been reduced to a boolean at \
            evaluation (should not happen if the term was well-typed")
      | 1 -> ok ctxexcepts (List.find (fun sub -> not (is_empty_error sub)) vexcepts)
        @@ trdefault ~trexcepts ~vexcepts ~trjust:(trexpr just) ~trcons:(trexpr cons)
      | _ ->
        (*let poslist =
          List.filter_map
            (fun ex ->
              if is_empty_error ex then None
              else Some Expr.(pos_to_runtime (pos ex)))
            excepts
        in
        raise Runtime.(Error (Conflict, poslist))*)
        raise_soft_fatal_error Conflict m @@ trdefault ~trexcepts ~vexcepts ~trjust:(trexpr just) ~trcons:(trexpr cons)
      )
    | EPureDefault e -> 
      let* new_ctx, v, tr = 
        evaluate_expr_with_trace_aux ctx local_ctx lang e 
        |> map_error_trace (fun _ -> trpuredefault)
      in ok new_ctx v @@ trpuredefault tr
    | _ -> .
  in
  match evaluate_expr_with_trace_aux ctx Var.Map.empty lang e
  with 
    | Ok (ctx, v, tr) ->
      let reduced_ctx, v = substitute_bounded_vars ctx v in 
      v, trcontextclosure ~context:reduced_ctx ~tr
    | Error (err, m, tr) -> Mark.add m (EFatalError err), tr

let evaluate_expr_safe :
    type d.
    decl_ctx ->
    Global.backend_lang ->
    ((d, yes) interpr_kind, 't) gexpr ->
    ((d, yes) interpr_kind, 't) gexpr * ((d, yes, yes) slicing_interpr_kind, 't) Trace_ast.t =
 fun ctx lang e ->
  try evaluate_expr_with_trace ctx lang e
  with Runtime.Error (err, rpos) ->
    Message.error
      ~extra_pos:(List.map (fun rp -> "", Expr.runtime_to_pos rp) rpos)
      "During evaluation: %a." Format.pp_print_text
      (Runtime.error_message err)