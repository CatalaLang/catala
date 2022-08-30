{ ocamlPackages, fetchurl }:

ocamlPackages.overrideScope' (self: super: {
  cmdliner_1_1_0 = super.cmdliner.overrideAttrs (o: rec {
    version = "1.1.0";
    src = fetchurl {
      url = "https://erratique.ch/software/${o.pname}/releases/${o.pname }-${version}.tbz";
      sha256 = "sha256-irWd4HTlJSYuz3HMgi1de2GVL2qus0QjeCe1WdsSs8Q=";
    };
  });
  alcotest = (super.alcotest.override {
    cmdliner = self.cmdliner_1_1_0;
  }).overrideAttrs (_: {
    doCheck = false;
  });
  catala = self.callPackage ./catala.nix { };
  bindlib = self.callPackage ./bindlib.nix { };
  unionfind = self.callPackage ./unionfind.nix { };
  ninja_utils = self.callPackage ./ninja_utils.nix { };
  clerk = self.callPackage ./clerk.nix { };
  ppx_yojson_conv = self.callPackage ./ppx_yojson_conv.nix { };
})
