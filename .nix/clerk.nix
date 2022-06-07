{ lib
, buildDunePackage
, odoc
, re
, ansiterminal
, cmdliner_1_1_0
, ninja_utils
, alcotest
}:

buildDunePackage rec {
  pname = "clerk";
  version = "0.6.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  useDune2 = true;

  propagatedBuildInputs = [
    odoc
    re
    ansiterminal
    cmdliner_1_1_0
    ninja_utils
    alcotest
  ];
  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/CatalaLang/catala";
    description =
      "Build system for Catala, a specification language for tax and social benefits computation rules";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
