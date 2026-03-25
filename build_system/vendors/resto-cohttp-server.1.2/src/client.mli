(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
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

(** Client calls to services. *)

(** The minimal interface for building a client. [Cohttp_lwt.S.Client]
    is an instance of this signature, modulo some additional
    optional parameters that resto does not use. See {!OfCohttp} below
    to obtain an exact intance based on [Cohttp_lwt.S.Client]. *)
module type CALL = sig
  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt.Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
end

module OfCohttp (Client : Cohttp_lwt.S.Client) : CALL

(** Whether or not an operation should follow redirects.

    Given a [limit], operations accepting a [redirect_behaviour] will follow up
    to that many redirects, inclusive, and fail with a [Too_many_redirects] error
    above that. If the [limit] parameter is negative, redirects will not be
    followed, just as if the behaviour were set to [Do_not_follow_redirects],
    but the error will be [Too_many_redirects] instead. *)
type redirect_behaviour =
  | Do_not_follow_redirects
  | Follow_redirects of {limit : int}

(** [Make(Encoding)(Client)] is a module that allows you to make calls to
    various [Resto] (or [EzResto]) services.

    The calls are type safe: you must provide the correct parameters to the
    services which are automatically encoded according to [Encoding], the answer
    is automatically decoded according to [Encoding]. The scheduling (waiting on
    answers, etc.) is provided by [Client]. *)
module Make (Encoding : Resto.ENCODING) (Call : CALL) : sig
  module Service : module type of struct
    include Resto.MakeService (Encoding)
  end

  (** Content-Type header specifications:
      https://tools.ietf.org/html/rfc7231#section-3.1.1.5
      and additional information:
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type *)
  type content_type = string * string

  type raw_content = Cohttp_lwt.Body.t * content_type option

  type content =
    Cohttp_lwt.Body.t * content_type option * Media_type.Make(Encoding).t option

  (** The type for possible results when calling over HTTP. Some results
      correspond to an HTTP return code, other results correspond to internal
      issues with resto. *)
  type ('o, 'e) generic_rest_result =
    [ `Ok of 'o option  (** 200 *)
    | `Conflict of 'e  (** 409 *)
    | `Error of 'e  (** 500 *)
    | `Forbidden of 'e  (** 403 *)
    | `Not_found of 'e  (** 404 *)
    | `Gone of 'e  (** 410 *)
    | `Unauthorized of 'e  (** 401 *)
    | `Bad_request of string  (** 400 *)
    | `Method_not_allowed of string list  (** 405 *)
    | `Unsupported_media_type  (** 415 *)
    | `Not_acceptable of string  (** 406 *)
    | `Unexpected_status_code of Cohttp.Code.status_code * content
      (** HTTP status code set by server is invalid or unsupported by resto *)
    | `Connection_failed of string
      (** Failure at one of the levels lower than HTTP (e.g., network) *)
    | `OCaml_exception of string
      (** Exception raised whilst decoding the result. *)
    | `Unauthorized_host of string option  (** CORS-related error *)
    | `Too_many_redirects of string  (** 310 *)
    | `Redirect_without_location of string
      (** Redirect did not have a Location header *) ]

  (** A [LOGGER] module is used for logging only *)
  module type LOGGER = sig
    type request

    val log_empty_request : Uri.t -> request Lwt.t

    val log_request :
      ?media:Media_type.Make(Encoding).t ->
      'a Encoding.t ->
      Uri.t ->
      string ->
      request Lwt.t

    val log_response :
      request ->
      ?media:Media_type.Make(Encoding).t ->
      'a Encoding.t ->
      Cohttp.Code.status_code ->
      string Lwt.t Lazy.t ->
      unit Lwt.t
  end

  type logger = (module LOGGER)

  val null_logger : logger

  val timings_logger :
    gettimeofday:(unit -> float) -> Format.formatter -> logger

  val full_logger : Format.formatter -> logger

  (** Low-level call primitive: use only for making calls for which there is no
      service defined, prefer making call to a defined service. *)
  val generic_call :
    [< Resto.meth] ->
    ?redirect_behaviour:redirect_behaviour ->
    ?headers:(string * string) list ->
    ?accept:Media_type.Make(Encoding).t list ->
    ?body:Cohttp_lwt.Body.t ->
    ?media:Media_type.Make(Encoding).t ->
    Uri.t ->
    (content, content) generic_rest_result Lwt.t

  (** The type for possible results when calling a service. This includes the
      possible HTTP results (see [generic_rest_result] and other
      service-specific errors. *)
  type ('o, 'e) service_result =
    [ ('o, 'e option) generic_rest_result
    | `Unexpected_content_type of raw_content
    | `Unexpected_content of (string * Media_type.Make(Encoding).t) * string
    | `Unexpected_error_content_type of raw_content
    | `Unexpected_error_content of
      (string * Media_type.Make(Encoding).t) * string ]

  (** [call_service media_types ?logger ?headers ?base service path_params
      query_params input] makes a call to [service] with the parameters
      [path_params], [query_params], and [input]. It returns a result (or an
      error).

      The OCaml type system guarantees that the parameters are as expected by
      the service. *)
  val call_service :
    Media_type.Make(Encoding).t list ->
    ?redirect_behaviour:redirect_behaviour ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    'p ->
    'q ->
    'i ->
    (Resto.meth * Uri.t * ('o, 'e) service_result) Lwt.t

  (** [call_streamed_service media_types ?logger ?headers ?base service
      ~on_chunk ~on_close path_params query_params input] makes a call to
      [service] with the parameters [path_params], [query_params], and [input].
      The values returned by the service are passed to the [on_chunk] callback,
      and when the server closes the connection the [on_close] callback is
      called.

      The function returns a [unit -> unit] function that consumes the remainder
      of the input without side-effects. Call this function when you want to
      discard all the queued-up chunks.

      The OCaml type system guarantees that the parameters are as expected by
      the service. *)
  val call_streamed_service :
    Media_type.Make(Encoding).t list ->
    ?redirect_behaviour:redirect_behaviour ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    on_chunk:('o -> unit) ->
    on_close:(unit -> unit) ->
    'p ->
    'q ->
    'i ->
    (Resto.meth * Uri.t * (unit -> unit, 'e) service_result) Lwt.t
end
