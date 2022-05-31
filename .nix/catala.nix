{ lib
, pkgs
, fetchFromGitHub
, buildDunePackage
, ansiterminal
, sedlex_2
, menhir
, unionfind
, bindlib
, cmdliner_1_1_0
, re
, zarith
, zarith_stubs_js
, ocamlgraph
, calendar
, visitors
, benchmark
, js_of_ocaml
, js_of_ocaml-ppx
, camomile
, cppo
, ppx_deriving
, z3
, alcotest
, ppx_yojson_conv
, menhirLib ? null #for nixos-unstable compatibility.
}:

buildDunePackage rec {
  pname = "catala";
  version = "0.6.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  useDune2 = true;

  propagatedBuildInputs = [
    ansiterminal
    sedlex_2
    menhir
    menhirLib
    cmdliner_1_1_0
    re
    zarith
    zarith_stubs_js
    ocamlgraph
    calendar
    visitors
    benchmark
    js_of_ocaml
    js_of_ocaml-ppx
    ppx_yojson_conv
    camomile
    cppo
    z3

    pkgs.z3

    ppx_deriving

    alcotest

    unionfind
    bindlib
  ] ++ (if isNull menhirLib then [ ] else [ menhirLib ]);
  doCheck = true;

  meta = with lib; {
    homepage = "https://catala-lang.org";
    description =
      "Catala is a domain-specific programming language designed for deriving correct-by-construction implementations from legislative texts.";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
