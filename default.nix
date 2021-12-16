{ lib
, fetchFromGitHub
, buildDunePackage
, ansiterminal
, sedlex_2
, menhir
, unionfind
, bindlib
, cmdliner
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
, menhirLib ? null #for nixos-unstable compatibility.
}:

buildDunePackage rec {
  pname = "catala";
  version = "0.5.0";

  minimumOCamlVersion = "4.11";

  src = ./.;

  useDune2 = true;

  propagatedBuildInputs = [
    ansiterminal
    sedlex_2
    menhir
    menhirLib
    cmdliner
    re
    zarith
    zarith_stubs_js
    ocamlgraph
    calendar
    visitors
    benchmark
    js_of_ocaml
    js_of_ocaml-ppx
    camomile
    cppo

    ppx_deriving

    unionfind
    bindlib
  ] ++ (if isNull menhirLib then [ ] else [ menhirLib ]);
  doCheck = true;

  patches = [ ./.nix/no-web.patch ];

  meta = with lib; {
    homepage = "https://catala-lang.org";
    description =
      "Catala is a domain-specific programming language designed for deriving correct-by-construction implementations from legislative texts.";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}