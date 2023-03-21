{ lib
, buildDunePackage
, odoc
, re
, ansiterminal
, cmdliner
, ninja_utils
, alcotest
, catala
, ninja
, colordiff
}:

buildDunePackage {
  pname = "clerk";
  version = "0.8.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  duneVersion = "3";

  propagatedBuildInputs = [
    odoc
    re
    ansiterminal
    cmdliner
    ninja_utils
    alcotest
    catala
  ];

  # todo: the current colordiff in nixpkgs always prints the banner. This make the logs totally unreadable.
  nativeBuildInputs = [ catala ninja colordiff ];
  doCheck = true;

  meta = with lib; {
    homepage = "https://github.com/CatalaLang/catala";
    description =
      "Build system for Catala, a specification language for tax and social benefits computation rules";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
