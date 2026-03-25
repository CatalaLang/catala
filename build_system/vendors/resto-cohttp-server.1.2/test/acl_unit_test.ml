(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020      Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Acl

let () =
  List.iter
    (fun (string, matcher) ->
      try
        if parse string <> matcher then
          Format.kasprintf
            failwith
            "Parsing returned unexpected value for path %S"
            string
      with Invalid_argument _ ->
        Format.kasprintf failwith "Parsing failed for path %S" string)
    [
      ("/", {meth = Any; path = Exact []});
      (" /", {meth = Any; path = Exact []});
      ("                                    /", {meth = Any; path = Exact []});
      ("GET /", {meth = Exact `GET; path = Exact []});
      ("GET/", {meth = Exact `GET; path = Exact []});
      ("GET      /", {meth = Exact `GET; path = Exact []});
      ("POST /", {meth = Exact `POST; path = Exact []});
      ("/**", {meth = Any; path = FollowedByAnySuffix []});
      ("DELETE /**", {meth = Exact `DELETE; path = FollowedByAnySuffix []});
      ("DELETE/**", {meth = Exact `DELETE; path = FollowedByAnySuffix []});
      ( "/a/b/c/d/e",
        {
          meth = Any;
          path =
            Exact
              [Literal "a"; Literal "b"; Literal "c"; Literal "d"; Literal "e"];
        } );
      ( "/*/a/*/b",
        {
          meth = Any;
          path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
        } );
      ( "/*/a/*/b/**",
        {
          meth = Any;
          path =
            FollowedByAnySuffix [Wildcard; Literal "a"; Wildcard; Literal "b"];
        } );
      ( "PATCH /*/a/*/b/**",
        {
          meth = Exact `PATCH;
          path =
            FollowedByAnySuffix [Wildcard; Literal "a"; Wildcard; Literal "b"];
        } );
      ("/%3F%3F%3F/**", {meth = Any; path = FollowedByAnySuffix [Literal "???"]});
      ("/%2A%2F%25", {meth = Any; path = Exact [Literal "*/%"]});
    ]

let () =
  List.iter
    (fun matcher ->
      let string = to_string matcher in
      try
        if parse string <> matcher then
          Format.kasprintf
            failwith
            "parse-to_string roundtrip failure (%S)"
            string
      with Invalid_argument _ ->
        Format.kasprintf failwith "Parsing failed for path %S" string)
    [
      {meth = Any; path = Exact []};
      {meth = Exact `GET; path = Exact []};
      {meth = Exact `POST; path = Exact []};
      {meth = Any; path = FollowedByAnySuffix []};
      {meth = Exact `DELETE; path = FollowedByAnySuffix []};
      {
        meth = Any;
        path =
          Exact
            [Literal "a"; Literal "b"; Literal "c"; Literal "d"; Literal "e"];
      };
      {meth = Any; path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"]};
      {
        meth = Any;
        path =
          FollowedByAnySuffix [Wildcard; Literal "a"; Wildcard; Literal "b"];
      };
      {
        meth = Exact `PATCH;
        path =
          FollowedByAnySuffix [Wildcard; Literal "a"; Wildcard; Literal "b"];
      };
      {meth = Any; path = FollowedByAnySuffix [Literal "???"]};
      {meth = Any; path = Exact [Literal "*/%"]};
    ]

let () =
  List.iter
    (fun string ->
      try
        let _ = parse string in
        Format.kasprintf
          failwith
          "Parsing unexepectedly succeeded for invalid input %S"
          string
      with Invalid_argument _ -> ())
    [
      "NOTAMETH /a/b";
      "GET /a*";
      "GET /a&";
      "GET /a?";
      "GET /=a";
      " GET /a";
      " GET/";
      "a";
      " ";
      "GET    ";
      "GET";
      "G%45T/";
      "GET%2F";
      "GE/T";
      "GET ";
      "FOOOOOOOO";
      "\\";
      "";
      "GETGET/";
      "GETGET/*";
      "GETPUT/*";
      "GETx/*";
      "GET\n/*";
      "PATCH-/*";
      "PATCH#/*";
      "PUT**";
      "PUT*/";
      "/a*";
      "/**/*";
      "/**/*/*/**";
      "/**/**";
      "/**/a";
      "/**/a/b/*/c";
      "/**/a/b*";
      "/a/%25*%2524";
    ]

let () =
  List.iter
    (fun (policy, meth, path, exp) ->
      if allowed policy ~meth ~path <> exp then
        Format.kasprintf
          failwith
          "\"%s %a\" Unexpectedly %s"
          (Resto.string_of_meth meth)
          Format.(
            pp_print_list
              ~pp_sep:(fun ppf () -> pp_print_char ppf '/')
              pp_print_string)
          path
          (if exp then "forbidden" else "allowed"))
    [
      (Allow_all {except = []}, `GET, [], true);
      (Allow_all {except = []}, `GET, ["still"; "this"], true);
      (Deny_all {except = []}, `GET, [], false);
      (Deny_all {except = []}, `GET, ["still"; "this"], false);
      (Allow_all {except = [{meth = Any; path = Exact []}]}, `GET, [], false);
      (Allow_all {except = [{meth = Any; path = Exact []}]}, `GET, ["a"], true);
      (Deny_all {except = [{meth = Any; path = Exact []}]}, `GET, [], true);
      ( Deny_all {except = [{meth = Any; path = Exact []}]},
        `GET,
        ["here"],
        false );
      ( Deny_all {except = [{meth = Exact `GET; path = Exact []}]},
        `GET,
        [],
        true );
      ( Deny_all {except = [{meth = Exact `GET; path = Exact []}]},
        `POST,
        [],
        false );
      ( Deny_all {except = [{meth = Exact `GET; path = Exact []}]},
        `PATCH,
        [],
        false );
      ( Deny_all {except = [{meth = Exact `POST; path = Exact []}]},
        `GET,
        [],
        false );
      ( Deny_all {except = [{meth = Exact `POST; path = Exact []}]},
        `POST,
        [],
        true );
      ( Deny_all {except = [{meth = Exact `POST; path = Exact []}]},
        `DELETE,
        [],
        false );
      ( Deny_all {except = [{meth = Any; path = FollowedByAnySuffix []}]},
        `GET,
        [],
        true );
      ( Deny_all {except = [{meth = Any; path = FollowedByAnySuffix []}]},
        `POST,
        [],
        true );
      ( Deny_all {except = [{meth = Any; path = FollowedByAnySuffix []}]},
        `GET,
        ["a"; "b"; "v"],
        true );
      ( Deny_all {except = [{meth = Any; path = FollowedByAnySuffix []}]},
        `POST,
        ["this"; "and"; "that"],
        true );
      ( Deny_all {except = [{meth = Any; path = FollowedByAnySuffix []}]},
        `DELETE,
        ["none"],
        true );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["this"; "a"; "that"; "b"],
        true );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["foo"; "a"; "barbarbar"; "b"],
        true );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["foo"; "0"; "barbarbar"; "1"],
        true );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["foo"; "1"; "barbarbar"; "0"],
        false );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["foo"; "0"; "1"],
        false );
      ( Deny_all
          {
            except =
              [
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "a"; Wildcard; Literal "b"];
                };
                {
                  meth = Any;
                  path = Exact [Wildcard; Literal "0"; Wildcard; Literal "1"];
                };
              ];
          },
        `GET,
        ["foo"; "a"; "barbarbar"; "1"],
        false );
    ]
