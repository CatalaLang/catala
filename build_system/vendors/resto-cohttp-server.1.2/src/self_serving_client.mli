(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [Make(Encoding)] returns a module that provides
    an instance of [Client.CALL] in its [launch] function.
    The [Client.CALL] instance is meant to be passed to [Client.Make]
    to obtain a local, self-serving client: a client that is their own server.

    A local, self-serving client is a client that performs RPCs on its own,
    without the need for a server. It does not open connections, sockets, files
    nor any such resource. Instead, it serves itself in-memory.

    This can be used for testing, for example for mocking a distant server;
    or in the case where some functionalities can either be provided
    locally or by a distant server.

    A local, self-serving client works the same as a server, except that its
    ACLs cannot be changed after constructing it (as opposed to
    [Server.Make.set_acl]). *)
module Make
    (Encoding : Resto.ENCODING)
    (Log : Resto_cohttp_server.Server.LOGGING) : sig
  (** The [middleware] parameter is ignored. *)
  val launch :
    ?cors:Cors.t ->
    ?agent:string ->
    ?acl:Acl.t ->
    media_types:Media_type.Make(Encoding).t list ->
    unit Resto_directory.Make(Encoding).directory ->
    (module Client.CALL)
end
