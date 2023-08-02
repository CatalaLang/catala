{ lib
, buildDunePackage
, odoc
, cppo
, ounit
, fetchurl
}:

buildDunePackage rec {
  pname = "ocolor";
  version = "1.3.0";
  
  minimumOCamlVersion = "4.11";

  src = fetchurl
    {
      url = "https://github.com/marc-chevalier/${pname}/archive/${version}.tar.gz";
      sha256 = "sha256-V7xuBRNbWWi3OpDRe7WakOO4Vz6nyoV8/gyn45fpmYA=";
    };

  duneVersion = "3";

  propagatedBuildInputs = [
    odoc
    ounit
    cppo
  ];

  nativeBuildInputs = [ cppo ];
  doCheck = true;

  meta = with lib; {
    homepage = "https://github.com/marc-chevalier/ocolor";
    description =
      "OColor is an OCaml library which help to format nicely using ANSI escape codes.";
    license = licenses.mit;
    maintainers = [ ];
  };
}
