(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic-Labs                                           *)
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

(** Access Control Lists (ACL): types and values to allow or deny access to
    specific parts of an API based on path matching. *)

(** [path_matcher] is the type of values that describe a path or a set of paths.
    Specifically,
    - [Literal s] matches path chunks that are identical to [s],
    - [Wildcard] matches any single path chunk,
    - [Exact chunks] matches any path the chunks of which match [chunks]
      one-by-one,
    - [FollowedByAnySuffix chunks] matches any path that is a suffix of a path
      matched by [Exact chunks].

    E.g., [Exact [Literal "users"; Wildcard; Literal "display-name"]] matches
    the path ["/users/Alice/display-name"] and ["/users/Bob/display-name"], but
    not ["/users/foo/preferences/"], nor ["/users/Bar/display-name/normalised"].

    E.g., [FollowedByAnySuffix [Literal "admin"; Literal "keys"]] matches the
    path ["/admin/keys"] and ["admin/keys/gpg"].

    It is recommended that you use [FollowedByAnySuffix] in order to match a
    whole directory attached to a given prefix, or a whole set of services that
    are register under a common prefix.

    It is recommended that you use [Wildcard] to match path-parameters (such as
    with the path ["/users/<user-id>/"].

    It is _NOT_ recommended that you use [Literal _] to match on the specific
    value of a parameter.
*)
type chunk_matcher = Literal of string | Wildcard

type path_matcher =
  | Exact of chunk_matcher list
  | FollowedByAnySuffix of chunk_matcher list

(** [meth_matcher] is the type of values that describe a method or a set of
    methods. [Exact m] matches [m] whilst [Any] matches any method. *)
type meth_matcher = Exact of Resto.meth | Any

type matcher = {meth : meth_matcher; path : path_matcher}

(** [parse s] parses the string [s] as a matcher. It raises {!Invalid_argument}
    if the argument [s] is not in line with the following format.

    A string representation of a matcher has the following components one after
    the other.

    1. An optional method. If the method is absent, then [Any] is the method
       matcher. If the method is present it must be one of ["GET"; "POST";
       "DELETE"; "PUT"; "PATCH"].
    2. Any number (including none (0)) of spaces ([ ]).
    3. A path which must start with a slash ([/]) and be followed by a sequence
       of chunks separated by slashes ([/]). Each chunk is either a single asterisk
       ([*]) (for [Wildcard]) or a non-empty sequence of characters (for
       [Literal _]). Special characters (see below) must be percentage-encoded.
    4. An optional suffix slash-star-star (["/**"]) indicates
       [FollowedByAnySuffix] and it's absence is for [Exact].

    E.g., ["/**"] is a matcher for any method and any path.
    E.g., ["     /**"] is the same matcher with extra (ignored) space (for alignment).
    E.g., ["GET  /**"] is a matcher for the GET method and any path.
    E.g., ["POST /admin/**"] is a matcher for the POST method on any suffix of ["/admin"].
    E.g., ["PATCH/*"] is a matcher for the PATCH method on any single-chunk path.
    E.g., ["/users/*/display-name"] is a matcher for any method on paths that
    fit in the ["/users/<user-id>/display-name"] pattern.

    Chunks cannot contain the following special characters. These characters
    must be represented percent-encoded. (Note that chunks are percent-decoded
    and the character percent ([%]) should appear percent-encoded (%25).
    - slash ([/], represented by %2F)
    - asterisk ([*], represented by %2A)
    - question mark ([?], represented by %3F)
    - ampersand ([&], represented by %26)
    - hash ([#], represented by %23)
    - equal ([=], represented by %3D)

    Also note that each chunk is percent-decoded.

    E.g., ["GET /entries/by/year/2020/*/*"] is a matcher for the GET method on
    paths that fit in the ["/entries/by/year/2020/<month>/<day>"] pattern.
    E.g., ["GET /entries/by/year/20*/*/*"] is not a valid matcher. The character
    asterisk ([*]) is not allowed within literal chunks. To match on the
    specific string ["20*"] use the percent-encoding [20%2A]. You cannot match
    on regular expressions nor glob expansions using this Acl module.

    @raise {!Invalid_argument} if the path does not follow the format described
    above. *)
val parse : string -> matcher

val to_string : matcher -> string

(** access policy *)
type t =
  | Allow_all of {except : matcher list}
      (** Allow all by default but deny requests matched by the exception list. *)
  | Deny_all of {except : matcher list}
      (** Deny all by default but allow requests matched by the exception list.  *)

val allowed : t -> meth:Resto.meth -> path:string list -> bool
