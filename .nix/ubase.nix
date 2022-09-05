{ lib, fetchurl, buildDunePackage, uutf }:

buildDunePackage rec {
  pname = "ubase";
  version = "0.05";

  minimumOCamlVersion = "4.05.0";

  useDune2 = true;

  propagatedBuildInputs = [
    uutf
  ];

  src = fetchurl
    {
      url = "https://github.com/sanette/${pname}/archive/${version}.tar.gz";
      sha256 = "sha256-D7/aCobZDS9/e5hLxd6pO9MJ4xXaSTACUXeQU4j5u0E=";
    };
}
