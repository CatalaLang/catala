{ lib, fetchurl, buildDunePackage, ppxlib, ppx_yojson_conv_lib, ppx_js_style }:

buildDunePackage rec {
  pname = "ppx_yojson_conv";
  version = "0.14.0";

  minimumOCamlVersion = "4.0.8";

  useDune2 = true;

  propagatedBuildInputs = [
    ppxlib ppx_yojson_conv_lib ppx_js_style
  ];

  src = fetchurl
    {
      url = "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_yojson_conv-v0.14.0.tar.gz";
      sha256 = "0ls6vzj7k0wrjliifqczs78anbc8b88as5w7a3wixfcs1gjfsp2w";
    };
}
