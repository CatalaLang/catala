{ lib, fetchurl, buildDunePackage }:

buildDunePackage rec {
  pname = "unionFind";
  version = "20220122";

  minimumOCamlVersion = "4.0.8";

  useDune2 = true;

  src = fetchurl {
    url =
      "https://gitlab.inria.fr/fpottier/${pname}/-/archive/${version}/archive.tar.gz";
    hash = "sha256-85+5KNYKXsNAH568qR8/AFC9UDviLJEO/Fztc9cRHZA=";
  };

  meta = with lib; {
    homepage = "https://gitlab.inria.fr/fpottier/unionFind/";
    description = "Implementations of the union-find data structure";
    license = licenses.gpl2;
    maintainers = [ ];
  };
}
