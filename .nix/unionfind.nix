{ lib
, fetchurl
, buildDunePackage
}:

buildDunePackage rec {
  pname = "unionFind";
  version = "20200320";

  minimumOCamlVersion = "4.0.8";

  useDune2 = true;

  src = fetchurl {
    url = "https://gitlab.inria.fr/fpottier/unionFind/-/archive/20200320/archive.tar.gz";
    hash = "sha256-szIwK9QyAw6fIIWDOiiyfyrEFZaaErGPRLkGhIK9STI=";
  };

  meta = with lib; {
    homepage = "https://gitlab.inria.fr/fpottier/unionFind/";
    description = "Implementations of the union-find data structure";
    license = licenses.gpl2;
    maintainers = [ ];
  };
}
