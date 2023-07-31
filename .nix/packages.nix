{ ocamlPackages, fetchurl }:

ocamlPackages.overrideScope' (self: super: {
  alcotest = (super.alcotest.override {}).overrideAttrs (_: {
    doCheck = false;
  });
  catala = self.callPackage ./catala.nix { };
  unionfind = self.callPackage ./unionfind.nix { };
  ninja_utils = self.callPackage ./ninja_utils.nix { };
  clerk = self.callPackage ./clerk.nix { };
  ubase = self.callPackage ./ubase.nix { };
  dates_calc = self.callPackage ./dates_calc.nix { };
  french_law = self.callPackage ./french_law.nix { };
  ocolor = self.callPackage ./ocolor.nix { };
})
