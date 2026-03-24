module Logging : Server.LOGGING = struct
  let debug fmt = Format.kasprintf (Format.printf "%s@.") fmt
  let log_info fmt = Format.kasprintf (Format.printf "%s@.") fmt
  let log_notice fmt = Format.kasprintf (Format.printf "%s@.") fmt
  let warn fmt = Format.kasprintf (Format.eprintf "%s@.") fmt
  let log_error fmt = Format.kasprintf (Format.eprintf "%s@.") fmt

  let lwt_debug fmt =
    Format.kasprintf (fun s -> Lwt.return (Format.printf "%s@." s)) fmt

  let lwt_log_info fmt =
    Format.kasprintf (fun s -> Lwt.return (Format.printf "%s@." s)) fmt

  let lwt_log_notice fmt =
    Format.kasprintf (fun s -> Lwt.return (Format.printf "%s@." s)) fmt

  let lwt_warn fmt =
    Format.kasprintf (fun s -> Lwt.return (Format.eprintf "%s@." s)) fmt

  let lwt_log_error fmt =
    Format.kasprintf (fun s -> Lwt.return (Format.eprintf "%s@." s)) fmt
end

module Yojson_encoding = Json_encoding.Make (Json_repr.Yojson)

module Encoding = struct
  include Yojson_encoding
  open Json_encoding

  type 'a t = 'a encoding
  type schema = Json_schema.schema

  let schema = Json_encoding.schema
  let unit = Json_encoding.unit
  let untyped = obj1 (req "untyped" string)
  let conv f g t = conv ~schema:(schema t) f g t

  module StringMap = Map.Make (String)

  let arg_encoding =
    let open Json_encoding in
    conv
      (fun { Resto.Arg.name; descr } -> name, descr)
      (fun (name, descr) -> { name; descr })
      (obj2 (req "name" string) (opt "descr" string))

  open Resto.Description

  let meth_encoding =
    Json_encoding.string_enum
      [
        "GET", `GET;
        "POST", `POST;
        "DELETE", `DELETE;
        "PUT", `PUT;
        "PATCH", `PATCH;
      ]

  let path_item_encoding =
    let open Json_encoding in
    union
      [
        case string
          (function PStatic s -> Some s | _ -> None)
          (fun s -> PStatic s);
        case arg_encoding
          (function PDynamic s -> Some s | _ -> None)
          (fun s -> PDynamic s);
      ]

  let query_kind_encoding =
    let open Json_encoding in
    union
      [
        case
          (obj1 (req "single" arg_encoding))
          (function Single s -> Some s | _ -> None)
          (fun s -> Single s);
        case
          (obj1 (req "optional" arg_encoding))
          (function Optional s -> Some s | _ -> None)
          (fun s -> Optional s);
        case
          (obj1 (req "flag" empty))
          (function Flag -> Some () | _ -> None)
          (fun () -> Flag);
        case
          (obj1 (req "multi" arg_encoding))
          (function Multi s -> Some s | _ -> None)
          (fun s -> Multi s);
      ]

  let query_item_encoding =
    let open Json_encoding in
    conv
      (fun { name; description; kind } -> name, description, kind)
      (fun (name, description, kind) -> { name; description; kind })
      (obj3 (req "name" string) (opt "description" string)
         (req "kind" query_kind_encoding))

  let service_descr_encoding =
    let open Json_encoding in
    conv
      (fun { meth; path; description; query; input; output; error } ->
        ( meth,
          path,
          description,
          query,
          (match input with
          | None -> None
          | Some input -> Some (Lazy.force input)),
          Lazy.force output,
          Lazy.force error ))
      (fun (meth, path, description, query, input, output, error) ->
        {
          meth;
          path;
          description;
          query;
          input =
            (match input with
            | None -> None
            | Some input -> Some (Lazy.from_val input));
          output = Lazy.from_val output;
          error = Lazy.from_val error;
        })
      (obj7 (req "meth" meth_encoding)
         (req "path" (list path_item_encoding))
         (opt "description" string)
         (req "query" (list query_item_encoding))
         (opt "input" any_schema) (req "output" any_schema)
         (req "error" any_schema))

  let directory_descr_encoding =
    let open Json_encoding in
    mu "service_tree"
    @@ fun directory_descr_encoding ->
    let static_subdirectories_descr_encoding =
      union
        [
          case
            (obj1
               (req "suffixes"
                  (list
                     (obj2 (req "name" string)
                        (req "tree" directory_descr_encoding)))))
            (function
              | Suffixes map -> Some (Resto.StringMap.bindings map) | _ -> None)
            (fun m ->
              let add acc (n, t) = Resto.StringMap.add n t acc in
              Suffixes (List.fold_left add Resto.StringMap.empty m));
          case
            (obj1
               (req "dynamic_dispatch"
                  (obj2 (req "arg" arg_encoding)
                     (req "tree" directory_descr_encoding))))
            (function Arg (ty, tree) -> Some (ty, tree) | _ -> None)
            (fun (ty, tree) -> Arg (ty, tree));
        ]
    in
    let static_directory_descr_encoding =
      conv
        (fun { services; subdirs } ->
          let find s =
            try Some (Resto.MethMap.find s services) with Not_found -> None
          in
          find `GET, find `POST, find `DELETE, find `PUT, find `PATCH, subdirs)
        (fun (get, post, delete, put, patch, subdirs) ->
          let add meth s services =
            match s with
            | None -> services
            | Some s -> Resto.MethMap.add meth s services
          in
          let services =
            Resto.MethMap.empty
            |> add `GET get
            |> add `POST post
            |> add `DELETE delete
            |> add `PUT put
            |> add `PATCH patch
          in
          { services; subdirs })
        (obj6
           (opt "get_service" service_descr_encoding)
           (opt "post_service" service_descr_encoding)
           (opt "delete_service" service_descr_encoding)
           (opt "put_service" service_descr_encoding)
           (opt "patch_service" service_descr_encoding)
           (opt "subdirs" static_subdirectories_descr_encoding))
    in
    union
      [
        case
          (obj1 (req "static" static_directory_descr_encoding))
          (function Static descr -> Some descr | _ -> None)
          (fun descr -> Static descr);
        case
          (obj1 (req "dynamic" (option string)))
          (function Dynamic descr -> Some descr | _ -> None)
          (fun descr -> Dynamic descr);
      ]

  let description_request_encoding =
    conv
      (fun { recurse } -> recurse)
      (function recurse -> { recurse })
      (obj1 (dft "recursive" bool false))

  let description_answer_encoding = directory_descr_encoding
end

module Rpc_server = Server.Make (Encoding) (Logging)

let json =
  Rpc_server.Media_type.
    {
      name = Cohttp.Accept.MediaType ("application", "json");
      q = None;
      pp =
        (fun _enc ppf raw ->
          try
            let json = Yojson.Safe.from_string raw in
            Format.fprintf ppf "%s" (Yojson.Safe.pretty_to_string json)
          with Yojson.Json_error err ->
            Format.fprintf ppf "@[<v 2>Invalid JSON (%s) - Raw data:@ %s@]" err
              raw);
      construct =
        (fun enc v ->
          Yojson_encoding.construct enc v |> Yojson.Safe.pretty_to_string);
      construct_seq = (fun _ -> assert false);
      destruct =
        (fun enc (body : string) ->
          try
            let body = Yojson.Safe.from_string body in
            Ok (Yojson_encoding.destruct enc body)
          with Json_encoding.Cannot_destruct _ as exn ->
            Format.kasprintf Result.error "%a"
              (fun fmt -> Json_encoding.print_error fmt)
              exn);
    }

let get_scopes (prg : Shared_ast.typed Dcalc.Ast.program) =
  let open Shared_ast in
  let exports = BoundList.last prg.code_items in
  List.filter_map (function KScope scope, _ -> Some scope | _ -> None) exports

let register_scope
    (prg : Shared_ast.typed Dcalc.Ast.program)
    scope
    (directory : unit Rpc_server.Directory.t) =
  let open Shared_ast in
  Message.debug "registering %a" ScopeName.format scope;
  let { in_struct_name; out_struct_name; _ } =
    ScopeName.Map.find scope prg.decl_ctx.ctx_scopes
  in
  let input, output =
    let pos = Expr.mark_pos Expr.typed in
    let input_ty = TStruct in_struct_name, pos in
    let output_ty = TStruct out_struct_name, pos in
    ( Shared_ast.Encoding.scope_input_encoding scope prg.decl_ctx input_ty,
      Encoding.scope_output_encoding scope prg.decl_ctx output_ty )
  in
  let open Rpc_server.Directory in
  let service =
    Service.post_service
      ~description:
        (Format.sprintf "Computes the scope %s" (ScopeName.to_string scope))
      ~query:Resto.Query.empty ~input ~output ~error:Json_encoding.string
      Resto.Path.(root / ScopeName.to_string scope)
  in
  Rpc_server.Directory.register0 directory service (fun () r ->
      let json = Yojson_encoding.construct input r in
      let fields = Interpreter.interpret_program_dcalc ~input:json prg scope in
      let { out_struct_name; _ } =
        ScopeName.Map.find scope prg.decl_ctx.ctx_scopes
      in
      let fields =
        List.fold_left
          (fun m (sf_s, e) ->
            StructField.Map.add (StructField.fresh sf_s) (Expr.box e) m)
          StructField.Map.empty fields
      in
      let r =
        Expr.estruct ~name:out_struct_name ~fields Expr.typed
        |> Expr.unbox
        |> Encoding.convert_from_gexpr prg.decl_ctx
      in
      Resto_directory.Answer.return r)

let build_file_directory file =
  let open Shared_ast in
  let mark = Expr.typed in
  let options =
    Global.enforce_options ~input_src:(Global.FileName file) ~debug:true ()
  in
  let (prg : typed Dcalc.Ast.program), _ =
    Driver.Passes.dcalc options ~includes:[] ~stdlib:None ~optimize:true
      ~check_invariants:false ~autotest:false ~typed:mark
  in
  Interpreter.load_runtime_modules
    ~hashf:Hash.(finalise ~monomorphize_types:false)
    prg;
  let scopes = get_scopes prg in
  List.fold_right (register_scope prg) scopes Rpc_server.Directory.empty

let serve file =
  let directory : unit Rpc_server.Directory.t =
    build_file_directory file
    |> fun dir ->
    let description_service =
      Rpc_server.Directory.Service.description_service
        ~description:"RPCs documentation and input/output schema"
        Json_encoding.empty
        Resto.Path.(root / "describe")
    in
    Rpc_server.Directory.register_describe_directory_service dir
      description_service
  in
  let directory =
    let service =
      Rpc_server.Directory.Service.get_service ~description:"list endpoints"
        ~query:Resto.Query.empty
        ~output:Json_encoding.(list string)
        ~error:Json_encoding.empty
        Resto.Path.(root / "endpoints")
    in
    Rpc_server.Directory.register0 directory service (fun () () ->
        let* r =
          Rpc_server.Directory.describe_directory ~recurse:true directory
        in
        let rec loop acc dir =
          match dir with
          | Resto.Description.Empty -> List.rev acc
          | Static { services; subdirs } -> (
            Format.eprintf "subdirs: %b@." (subdirs <> None);
            let l =
              let path_to_string = function
                | Resto.Description.PStatic s -> s
                | _ -> "<dyn>"
              in
              Resto.MethMap.bindings services
              |> List.rev_map (fun (a, (b : _ Resto.Description.service)) ->
                  Format.sprintf "<%s> /%s %s" (Resto.string_of_meth a)
                    (List.map path_to_string b.path |> String.concat " ")
                    (Option.value ~default:"none"
                       b.Resto.Description.description))
            in
            let acc = l @ acc in
            match subdirs with
            | Some (Suffixes sm) ->
              Resto.StringMap.bindings sm
              |> List.map snd
              |> List.fold_left loop acc
            | _ -> acc)
          | _ -> List.rev acc
        in
        Resto_directory.Answer.return (loop [] r))
  in
  Lwt_main.run
  @@ Lwt.catch
       (fun () ->
         let* prout =
           Rpc_server.Directory.describe_directory ~recurse:true directory
         in
         Message.debug "@[<v 2>directory:@ %a@]"
           Resto.Description.pp_print_directory prout;
         let server_port = `TCP (`Port 3000) in
         let* () =
           Rpc_server.init_and_launch ~media_types:[json] ~host:"localhost"
             directory server_port
         in
         (* never ending *)
         (fst (Lwt.wait ()) : unit Lwt.t))
       (function
         (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1312 This exception
            seems to be unreachable. *)
         | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
           Message.error "rpc port already in use"
         | exn -> raise exn);
  Cmdliner.Cmd.Exit.ok

let serve_cmd =
  let single_file =
    Cmdliner.Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"FILE" ~doc:"File to process")
  in
  let open Cmdliner in
  let doc = "Start RPC server." in
  Cmd.v (Cmd.info ~doc "serve") Term.(const serve $ single_file)

let main_cmd =
  let open Cmdliner in
  Cmd.group Cli.info [serve_cmd]

let () =
  let[@inline] exit_with_error excode emit =
    let bt = Printexc.get_raw_backtrace () in
    emit ();
    if Global.options.debug then Printexc.print_raw_backtrace stderr bt;
    exit excode
  in
  let open Cmdliner in
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.emit content Error
  | Message.CompilerErrors contents ->
    exit_with_error Cmd.Exit.some_error
    @@ fun () -> Message.Content.emit_n contents Error
  | Sys.Break ->
    Format.fprintf (Message.err_ppf ()) "@.- Interrupted -@.";
    exit_with_error 130 (fun () -> ())
  | Sys_error msg ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.(emit (of_string ("System error: " ^ msg)) Error)
  | e ->
    exit_with_error Cmd.Exit.internal_error
    @@ fun () ->
    Message.Content.(
      emit (of_string ("Unexpected error: " ^ Printexc.to_string e)) Error)
