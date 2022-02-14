{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  pkg = ocamlPackages.callPackage ./. {
    bindlib = ocamlPackages.callPackage ./.nix/bindlib.nix { };
    unionfind = ocamlPackages.callPackage ./.nix/unionfind.nix { };
    cmdliner = ocamlPackages.callPackage ./.nix/cmdliner.nix { };
  };
in mkShell {
  inputsFrom = [ pkg ];
  buildInputs = pkg.propagatedBuildInputs ++ [
    inotify-tools
    ocamlPackages.merlin
    ocamlformat
    ocamlPackages.ocp-indent
    ocamlPackages.utop
    ocamlPackages.ocaml-lsp
  ];
}
