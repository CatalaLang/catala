{ lib
, buildDunePackage
, odoc
, catala
}:

buildDunePackage {
  pname = "french_law";
  version = "0.8.0"; # TODO parse `catala.opam` with opam2json

  minimumOCamlVersion = "4.11";

  src = ../.;

  duneVersion = "3";

  propagatedBuildInputs = [
    odoc
    catala
  ];

  nativeBuildInputs = [ catala ];
  doCheck = true;

  meta = with lib; {
    homepage = "https://github.com/CatalaLang/catala";
    description =
      "Build system for Catala, a specification language for tax and social benefits computation rules";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
