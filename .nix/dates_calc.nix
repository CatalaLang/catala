{ lib, fetchurl, buildDunePackage }:

buildDunePackage rec {
  pname = "dates_calc";
  version = "0.0.4";

  minimumOCamlVersion = "4.11.0";

  duneVersion = "2";

  src = fetchurl
    {
      url = "https://github.com/CatalaLang/dates-calc/archive/${version}.tar.gz";
      sha256 = "sha256-lWhNBK50r3qhY3PXRHZVvqDee6Y4+3vC51ZfjLxGryg=";
    };
}
