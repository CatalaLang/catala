{ lib
, alcotest
, ansiterminal
, benchmark
, bindlib
, buildDunePackage
, cmdliner
, cppo
, dates_calc
, js_of_ocaml
, js_of_ocaml-ppx
, menhir
, menhirLib
, ocamlgraph
, pkgs
, ppx_deriving
, ppx_yojson_conv
, re
, sedlex
, ubase
, unionfind
, visitors
, z3
, zarith
, zarith_stubs_js
, ocaml-crunch
, cohttp-lwt-unix
, ocolor
, dune-build-info
}:

buildDunePackage {
  pname = "catala";
  version = "0.8.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  duneVersion = "3";

  nativeBuildInputs = [ cppo menhir ocaml-crunch ];

  propagatedBuildInputs = [
    alcotest
    ansiterminal
    benchmark
    bindlib
    cmdliner
    dates_calc
    js_of_ocaml
    js_of_ocaml-ppx
    menhirLib
    ocamlgraph
    pkgs.z3
    ppx_deriving
    ppx_yojson_conv
    re
    sedlex
    ubase
    unionfind
    visitors
    z3
    zarith
    zarith_stubs_js
    cohttp-lwt-unix
    ocolor
    dune-build-info
  ];

  # Currently there is no unit tests in catala and Cram tests are handled by clerk
  doCheck = false;

  meta = with lib; {
    homepage = "https://catala-lang.org";
    description =
      "Catala is a domain-specific programming language designed for deriving correct-by-construction implementations from legislative texts.";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
