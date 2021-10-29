{ lib, fetchFromGitHub, buildDunePackage }:

# We need the very last version "bleeding edge" since previous versions don't use dune.

buildDunePackage rec {
  pname = "bindlib";
  version = "5.0.1a";

  minimumOCamlVersion = "4.0.8";

  useDune2 = true;

  src = fetchFromGitHub {
    owner = "rlepigre";
    repo = "ocaml-${pname}";
    rev = "317f195d22c75f556053039cd94b52bd0c423709";
    name = pname;
    hash = "sha256-uO/Ko9PmQ+wE0d9jfEngd4G014B4nxGgfQyEvB52Pz8=";
  };

  meta = with lib; {
    homepage = "https://rlepigre.github.io/ocaml-bindlib/";
    description =
      "Bindlib is a library allowing the manipulation of data structures with bound variables";
    license = licenses.lgpl3;
    maintainers = [ ];
  };
}
