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
  # Use a more recent version of `re` than the one packaged in nixpkgs
  re = super.re.overrideAttrs (o: rec {
    version = "1.10.4";
    src = fetchurl {
      url = "https://github.com/ocaml/ocaml-${o.pname}/releases/download/${version}/${o.pname}-${version}.tbz";
      sha256 = "sha256-g+s+QwCqmx3HggdJAQ9DYuqDUkdCEwUk14wgzpnKdHw=";
    };
  });
  catala = self.callPackage ./catala.nix { };
  bindlib = self.callPackage ./bindlib.nix { };
  unionfind = self.callPackage ./unionfind.nix { };
  ninja_utils = self.callPackage ./ninja_utils.nix { };
  clerk = self.callPackage ./clerk.nix { };
  ppx_yojson_conv = self.callPackage ./ppx_yojson_conv.nix { };
  ubase = self.callPackage ./ubase.nix { };
})
