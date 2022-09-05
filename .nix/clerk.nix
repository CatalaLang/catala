{ lib
, buildDunePackage
, odoc
, re
, ansiterminal
, cmdliner_1_1_0
, ninja_utils
, alcotest
, catala
}:

buildDunePackage rec {
  pname = "clerk";
  version = "0.7.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  duneVersion = "3";

  propagatedBuildInputs = [
    odoc
    re
    ansiterminal
    cmdliner_1_1_0
    ninja_utils
    alcotest
    catala
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
