{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {nixpkgs, flake-utils, ...}:
    let
      systems = [ "x86_64-linux" ];
    in flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPackages = pkgs.callPackage ./.nix/packages.nix {};
      in
        rec {
          packages = {
            catala = ocamlPackages.catala;
            clerk = ocamlPackages.clerk;
          };
          defaultPackage = packages.catala;
          devShell = pkgs.mkShell {
            inputsFrom = [ packages.catala ];
            buildInputs = [
              pkgs.inotify-tools
              ocamlPackages.merlin
              pkgs.ocamlformat_0_21_0
              ocamlPackages.ocp-indent
              ocamlPackages.utop
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              pkgs.groff
              pkgs.obelisk
              pkgs.ninja
            ];
          };
        }
    );
}
