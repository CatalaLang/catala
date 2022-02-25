{ pkgs ? import <nixpkgs> { } }:

with pkgs;
ocamlPackages.callPackage ./. {
  bindlib = ocamlPackages.callPackage ./.nix/bindlib.nix { };
  unionfind = ocamlPackages.callPackage ./.nix/unionfind.nix { };
}
