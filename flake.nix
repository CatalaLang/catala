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
            french_law = ocamlPackages.french_law;
          };
          defaultPackage = packages.clerk;
          devShell = pkgs.mkShell {
            inputsFrom = [ packages.clerk packages.catala ];
            buildInputs = [
              pkgs.inotify-tools
              ocamlPackages.merlin
              pkgs.ocamlformat_0_26_0
              ocamlPackages.ocp-indent
              ocamlPackages.utop
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              pkgs.groff
              pkgs.obelisk
              pkgs.ninja
              pkgs.colordiff
              pkgs.pandoc
              pkgs.python3.pkgs.pygments
              pkgs.nodejs
              pkgs.nodePackages.npm
            ];
          };
        }
    );
}
