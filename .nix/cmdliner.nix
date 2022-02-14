{ lib, stdenv, fetchurl, ocaml, findlib, ocamlbuild, topkg, result }:

let
  pname = "cmdliner";
in

assert lib.versionAtLeast ocaml.version "4.01.0";

let param =
  {
    version = "1.1.0";
    hash = "sha256-irWd4HTlJSYuz3HMgi1de2GVL2qus0QjeCe1WdsSs8Q=";
  }
; in

stdenv.mkDerivation rec {
  name = "ocaml${ocaml.version}-${pname}-${version}";
  inherit (param) version;

  src = fetchurl {
    url = "https://erratique.ch/software/${pname}/releases/${pname}-${version}.tbz";
    inherit (param) hash;
  };

  nativeBuildInputs = [ ocaml ocamlbuild findlib ];
  buildInputs = [ topkg ];
  propagatedBuildInputs = [ result ];

  inherit (topkg) buildPhase installPhase;

  meta = with lib; {
    homepage = "https://erratique.ch/software/cmdliner";
    description = "An OCaml module for the declarative definition of command line interfaces";
    license = licenses.bsd3;
    platforms = ocaml.meta.platforms or [];
    maintainers = [ ];
  };
}