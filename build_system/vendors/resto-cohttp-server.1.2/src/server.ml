(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Lwt.Infix
module ConnectionMap = Map.Make (Cohttp.Connection)

let ( >>? ) v f = match v with Ok x -> f x | Error err -> Lwt.return_error err

let lwt_return_ok_response r = Lwt.return_ok (`Response r)

let lwt_return_response r = Lwt.return (`Response r)

let wchunk oc (item, offset, length) =
  if length = 0 then Lwt.return_unit
  else
    Lwt_io.fprintf oc "%X\r\n" length >>= fun () ->
    Lwt_io.write_from_exactly oc item offset length >>= fun () ->
    Lwt_io.write_from_string_exactly oc "\r\n" 0 2 >>= fun () -> Lwt_io.flush oc

let rec drain seq =
  match seq () with
  | Seq.Nil -> Lwt.return_unit
  | Seq.Cons (_, seq) ->
      Lwt.pause () >>= fun () -> (drain [@ocaml.tailcall]) seq

let rec wseq ic oc seq =
  match seq () with
  | Seq.Nil ->
      Lwt_io.write_from_string_exactly oc "0\r\n\r\n" 0 5 >>= fun () ->
      Lwt_io.flush oc
  | Seq.Cons (chunk, seq) ->
      Lwt.try_bind
        (fun () -> wchunk oc chunk)
        (fun () ->
          Lwt.pause () >>= fun () -> (wseq [@ocaml.tailcall]) ic oc seq)
        (fun exc -> drain seq >>= fun () -> raise exc)

let wseq ic oc seq =
  Lwt.finalize (fun () -> wseq ic oc seq) (fun () -> Lwt_io.close ic)

module type LOGGING = sig
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_warn : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end

let ( >>=? ) = Lwt_result.bind

module Make_selfserver (Encoding : Resto.ENCODING) (Log : LOGGING) = struct
  open Cohttp
  module Service = Resto.MakeService (Encoding)
  module Directory = Resto_directory.Make (Encoding)
  module Media_type = Media_type.Make (Encoding)

  module Media = struct
    type medias = {
      media_types : Media_type.t list;
      default_media_type : string * Media_type.t;
    }

    let default_media_type media_types =
      match Media_type.first_complete_media media_types with
      | None ->
          invalid_arg "Resto_directory_cohttp.launch(empty media type list)"
      | Some ((l, r), m) -> (l ^ "/" ^ r, m)

    let input_media_type ?headers medias =
      match headers with
      | None -> Ok (snd medias.default_media_type)
      | Some headers -> (
          match Header.get headers "content-type" with
          | None -> Ok (snd medias.default_media_type)
          | Some content_type -> (
              match Resto.Utils.split_path content_type with
              | [x; y] -> (
                  match Media_type.find_media (x, y) medias.media_types with
                  | None -> Error (`Unsupported_media_type content_type)
                  | Some media_type -> Ok media_type)
              | _ -> Error (`Unsupported_media_type content_type)))

    let output_content_media_type ?headers medias =
      match headers with
      | None -> Ok medias.default_media_type
      | Some headers -> (
          match Header.get headers "accept" with
          | None -> Ok medias.default_media_type
          | Some accepted -> (
              match
                Media_type.resolve_accept_header
                  medias.media_types
                  (Some accepted)
              with
              | None -> Error `Not_acceptable
              | Some media_type -> Ok media_type))
  end

  module Agent = struct
    let default_agent = "OCaml-Resto"
  end

  module Handlers = struct
    let invalid_cors (cors : Cors.t) headers =
      cors.allowed_origins <> [] && not (Cors.check_host headers cors)

    let invalid_cors_response agent =
      let headers =
        Cohttp.Header.init_with
          (Format.asprintf "X-%s-CORS-Error" agent)
          "invalid host"
      in
      (Response.make ~headers ~status:`Forbidden (), Cohttp_lwt.Body.empty)

    let handle_error headers medias
        (error :
          [< `Cannot_parse_body of string
          | `Cannot_parse_path of string list * Resto.Arg.descr * string
          | `Cannot_parse_query of string
          | `Method_not_allowed of [< Resto.meth] list
          | `Not_acceptable
          | `Not_found
          | `Not_implemented
          | `Unsupported_media_type of 'a ]) :
        Cohttp.Response.t * Cohttp_lwt.Body.t =
      let open Resto.Arg in
      match error with
      | `Not_implemented ->
          ( Response.make ~status:`Not_implemented ~headers (),
            Cohttp_lwt.Body.empty )
      | `Method_not_allowed methods ->
          let headers =
            Header.add_multi
              headers
              "allow"
              (List.map Resto.string_of_meth methods)
          in
          ( Response.make ~status:`Method_not_allowed ~headers (),
            Cohttp_lwt.Body.empty )
      | `Cannot_parse_path (context, arg, value) ->
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parsed an argument in path. After \"%s\", the value \
               \"%s\" is not acceptable for type \"%s\""
              (String.concat "/" context)
              value
              arg.name )
      | `Cannot_parse_body s ->
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parse the request body: %s"
              s )
      | `Cannot_parse_query s ->
          let headers = Header.add headers "content-type" "text/plain" in
          ( Response.make ~status:`Bad_request ~headers (),
            Format.kasprintf
              Cohttp_lwt.Body.of_string
              "Failed to parse the query string: %s"
              s )
      | `Not_acceptable ->
          let accepted_encoding =
            Media_type.acceptable_encoding medias.Media.media_types
          in
          ( Response.make ~status:`Not_acceptable ~headers (),
            Cohttp_lwt.Body.of_string accepted_encoding )
      | `Unsupported_media_type _ ->
          ( Response.make ~status:`Unsupported_media_type ~headers (),
            Cohttp_lwt.Body.empty )
      | `Not_found ->
          (Response.make ~status:`Not_found ~headers (), Cohttp_lwt.Body.empty)

    let handle_rpc_answer con_string ?headers output answer =
      match answer with
      | `Ok o ->
          let body = output o in
          Log.debug "(%s) response code:200" con_string ;
          Log.debug "(%s) response body: %s" con_string body ;
          let encoding = Transfer.Fixed (Int64.of_int (String.length body)) in
          ( Response.make ~status:`OK ~encoding ?headers (),
            Cohttp_lwt.Body.of_string body )
      | `No_content ->
          Log.debug "(%s) response code:204 (no content)" con_string ;
          (Response.make ~status:`No_content (), Cohttp_lwt.Body.empty)
      | `Created s ->
          let headers = Header.init () in
          let headers =
            match s with
            | None -> headers
            | Some s -> Header.add headers "location" s
          in
          Log.debug "(%s) response code:201 (created)" con_string ;
          (Response.make ~status:`Created ~headers (), Cohttp_lwt.Body.empty)

    let handle_rpc_answer_error con_string ?headers error answer =
      match answer with
      | `Unauthorized e ->
          Log.log_info "(%s) response code: 401" con_string ;
          let body, encoding = error e in
          let status = `Unauthorized in
          (Response.make ~status ~encoding ?headers (), body)
      | `Forbidden e ->
          Log.log_info "(%s) response code: 403" con_string ;
          let body, encoding = error e in
          let status = `Forbidden in
          (Response.make ~status ~encoding ?headers (), body)
      | `Gone e ->
          Log.log_info "(%s) response code: 410" con_string ;
          let body, encoding = error e in
          let status = `Gone in
          (Response.make ~status ~encoding ?headers (), body)
      | `Not_found e ->
          Log.log_info "(%s) response code: 404" con_string ;
          let body, encoding = error e in
          let status = `Not_found in
          (Response.make ~status ~encoding ?headers (), body)
      | `Conflict e ->
          Log.log_info "(%s) response code: 409" con_string ;
          let body, encoding = error e in
          let status = `Conflict in
          (Response.make ~status ~encoding ?headers (), body)
      | `Error e ->
          Log.log_info "(%s) response code: 500" con_string ;
          let body, encoding = error e in
          let status = `Internal_server_error in
          (Response.make ~status ~encoding ?headers (), body)

    let handle_rpc_answer_chunk ?headers output_seq answer =
      match answer with
      | `OkChunk o ->
          let body = output_seq o in
          let encoding = Transfer.Chunked in
          ( Response.make ~status:`OK ~encoding ?headers (),
            fun ic oc -> wseq ic oc body )

    let handle_options root cors headers path =
      let origin_header = Header.get headers "origin" in
      (if (* Default OPTIONS handler for CORS preflight *)
          origin_header = None
      then Directory.allowed_methods root () path
      else
        match Header.get headers "Access-Control-Request-Method" with
        | None -> Directory.allowed_methods root () path
        | Some meth -> (
            match Code.method_of_string meth with
            | #Resto.meth as meth ->
                Directory.lookup root () meth path >>=? fun _handler ->
                Lwt.return_ok [meth]
            | _ -> Lwt.return_error `Not_found))
      >>=? fun cors_allowed_meths ->
      let headers = Header.init () in
      let headers =
        Header.add_multi
          headers
          "Access-Control-Allow-Methods"
          (List.map Resto.string_of_meth cors_allowed_meths)
      in
      let headers = Cors.add_headers headers cors origin_header in
      Lwt.return_ok
        ( Response.make ~flush:true ~status:`OK ~headers (),
          Cohttp_lwt.Body.empty )
  end
end

module Make (Encoding : Resto.ENCODING) (Log : LOGGING) = struct
  include Make_selfserver (Encoding) (Log)
  open Cohttp

  type server = {
    root : unit Directory.directory;
    mutable streams : (unit -> unit) ConnectionMap.t;
    cors : Cors.t;
    medias : Media.medias;
    stop : unit Lwt.t;
    stopper : unit Lwt.u;
    mutable acl : Acl.t;
    agent : string;
    mutable worker : unit Lwt.t;
  }

  type callback =
    Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    Cohttp_lwt_unix.Server.response_action Lwt.t

  let init_server ?(cors = Cors.default) ?(agent = Agent.default_agent)
      ?(acl = Acl.Allow_all {except = []}) ~media_types root =
    let default_media_type = Media.default_media_type media_types in
    let stop, stopper = Lwt.wait () in
    let medias : Media.medias = {media_types; default_media_type} in
    {
      root;
      streams = ConnectionMap.empty;
      cors;
      medias;
      stop;
      stopper;
      acl;
      agent;
      worker = Lwt.return_unit;
    }

  let create_stream server con to_string s =
    let con_string = Connection.to_string con in
    let stopped, stopper =
      (* [wait] makes uncancelable promises *)
      Lwt.wait ()
    in
    let stream =
      Lwt_stream.from (fun () ->
          let elt = s.Resto_directory.Answer.next () in
          (* [pick] will cancel [elt] if [stopped] resolves,
             [pick] will effectlessly attempt to cancel [stopped] if [elt] resolves *)
          Lwt.pick [stopped; elt] >|= Option.map to_string)
    in
    let shutdown () =
      Log.log_info "streamed connection closed %s" con_string ;
      Lwt.wakeup_later stopper None ;
      s.shutdown () ;
      server.streams <- ConnectionMap.remove con server.streams
    in
    server.streams <- ConnectionMap.add con shutdown server.streams ;
    stream

  let resto_callback server ((_io, con) : Cohttp_lwt_unix.Server.conn) req body
      =
    let con_string = Connection.to_string con in
    let uri = Request.uri req in
    let path_and_query = Uri.path_and_query uri in
    Log.lwt_log_info "(%s) received request %s" con_string path_and_query
    >>= fun () ->
    let req_headers = Request.headers req in
    Log.lwt_debug
      "(%s) request headers: %s"
      con_string
      (Header.to_string req_headers)
    >>= fun () ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    Log.lwt_debug "(%s) request body: %s" con_string body >>= fun () ->
    let path = Uri.path uri in
    let path = Resto.Utils.decode_split_path path in
    (match Request.meth req with
    | #Resto.meth when Handlers.invalid_cors server.cors req_headers ->
        lwt_return_ok_response @@ Handlers.invalid_cors_response server.agent
    | #Resto.meth as meth -> (
        Directory.lookup server.root () meth path
        >>=? fun (Directory.Service s) ->
        Media.input_media_type ~headers:req_headers server.medias
        >>? fun input_media_type ->
        Log.lwt_debug
          "(%s) input media type %s"
          con_string
          (Media_type.name input_media_type)
        >>= fun () ->
        Media.output_content_media_type ~headers:req_headers server.medias
        >>? fun (output_content_type, output_media_type) ->
        (match
           Resto.Query.parse
             s.types.query
             (List.map (fun (k, l) -> (k, String.concat "," l)) (Uri.query uri))
         with
        | exception Resto.Query.Invalid s ->
            Lwt.return_error (`Cannot_parse_query s)
        | query -> Lwt.return_ok query)
        >>=? fun query ->
        Log.lwt_debug
          "(%s) ouput media type %s"
          con_string
          (Media_type.name output_media_type)
        >>= fun () ->
        let headers = Header.init () in
        let headers = Header.add headers "content-type" output_content_type in
        let headers =
          Cors.add_allow_origin
            headers
            server.cors
            (Header.get req_headers "origin")
        in
        (if not @@ Acl.allowed server.acl ~meth ~path then
         Lwt.return_ok (`Unauthorized None)
        else
          match s.types.input with
          | Service.No_input -> s.handler query () >>= Lwt.return_ok
          | Service.Input input -> (
              match input_media_type.destruct input body with
              | Error s -> Lwt.return_error (`Cannot_parse_body s)
              | Ok body -> s.handler query body >>= Lwt.return_ok))
        >>=? fun answer ->
        match answer with
        | (`Ok _ | `No_content | `Created _) as a ->
            let output = output_media_type.construct s.types.output in
            let response =
              Handlers.handle_rpc_answer con_string ~headers output a
            in
            lwt_return_ok_response response
        | `OkChunk _ as a ->
            let output_seq = output_media_type.construct_seq s.types.output in
            Log.lwt_debug
              "(%s) response code:200 (with chunk transfer\n\
              \            encoding)"
              con_string
            >>= fun () ->
            Lwt.return_ok
              (`Expert (Handlers.handle_rpc_answer_chunk ~headers output_seq a))
        | `OkStream o ->
            let output = output_media_type.construct s.types.output in
            let body = create_stream server con output o in
            let encoding = Transfer.Chunked in
            Log.lwt_debug "(%s) response code:200 (streamed)" con_string
            >>= fun () ->
            lwt_return_ok_response
              ( Response.make ~status:`OK ~encoding ~headers (),
                Cohttp_lwt.Body.of_stream body )
        | ( `Unauthorized _ | `Forbidden _ | `Gone _ | `Not_found _
          | `Conflict _ | `Error _ ) as a ->
            let error = function
              | None ->
                  Log.log_info "(%s) response body (empty)" con_string ;
                  (Cohttp_lwt.Body.empty, Transfer.Fixed 0L)
              | Some e ->
                  let s = output_media_type.construct s.types.error e in
                  Log.log_info "(%s) response body: %s" con_string s ;
                  ( Cohttp_lwt.Body.of_string s,
                    Transfer.Fixed (Int64.of_int (String.length s)) )
            in
            let response =
              Handlers.handle_rpc_answer_error con_string ~headers error a
            in
            lwt_return_ok_response response)
    | `HEAD ->
        (* TODO ??? *)
        Lwt.return_error `Not_implemented
    | `OPTIONS -> (
        Handlers.handle_options server.root server.cors req_headers path
        >>= fun res ->
        Log.lwt_log_info "(%s) RPC preflight" con_string >>= fun () ->
        match res with
        | Ok res -> lwt_return_ok_response res
        | Error _ as e -> Lwt.return e)
    | _ -> Lwt.return_error `Not_implemented)
    >>= function
    | Ok answer -> Lwt.return answer
    | Error err ->
        let headers = Header.init () in
        let headers =
          Cors.add_allow_origin
            headers
            server.cors
            (Header.get req_headers "origin")
        in
        lwt_return_response @@ Handlers.handle_error headers server.medias err

  (* Promise a running RPC server. *)

  let launch ?(host = "::") server ?(conn_closed = ignore)
      ?(callback = resto_callback server) mode =
    Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    server.worker <-
      (let conn_closed ((_, con) as c) =
         let () = conn_closed c in
         let con_string = Connection.to_string con in
         Log.debug "connection closed %s" con_string ;
         try ConnectionMap.find con server.streams () with Not_found -> ()
       and on_exn = function
         | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
             Log.log_error
               "RPC server port already taken, the node will be shutdown" ;
             exit 1
         | Unix.Unix_error (ECONNRESET, _, _) | Unix.Unix_error (EPIPE, _, _) ->
             ()
         | exn ->
             Format.eprintf
               "@[<v 2>Uncaught (asynchronous) exception:@ %s@ %s@]%!"
               (Printexc.to_string exn)
               (Printexc.get_backtrace ())
       and callback (io, con) req body =
         Lwt.catch
           (fun () -> callback (io, con) req body)
           (function
             | Not_found ->
                 let status = `Not_found in
                 let body = Cohttp_lwt.Body.empty in
                 lwt_return_response (Response.make ~status (), body)
             | exn ->
                 let headers = Header.init () in
                 let headers =
                   Header.add headers "content-type" "text/ocaml.exception"
                 in
                 let status = `Internal_server_error in
                 let body =
                   Cohttp_lwt.Body.of_string (Printexc.to_string exn)
                 in
                 lwt_return_response (Response.make ~status ~headers (), body))
       in
       Cohttp_lwt_unix.Server.create
         ~stop:server.stop
         ~ctx
         ~mode
         ~on_exn
         (Cohttp_lwt_unix.Server.make_response_action ~callback ~conn_closed ())) ;
    Log.lwt_log_info "Server started (agent: %s)" server.agent

  let init_and_launch ?(host = "::") ?(cors = Cors.default)
      ?(agent = Agent.default_agent) ?(acl = Acl.Allow_all {except = []})
      ~media_types root mode =
    let server = init_server ~cors ~agent ~acl ~media_types root in
    launch ~host server mode

  let shutdown server =
    Lwt.wakeup_later server.stopper () ;
    server.worker >>= fun () ->
    ConnectionMap.iter (fun _ f -> f ()) server.streams ;
    Lwt.return_unit

  let set_acl server acl = server.acl <- acl
end
