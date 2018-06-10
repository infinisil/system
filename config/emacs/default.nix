{ pkgs ? import ../../external/nixpkgs {}
, lib ? pkgs.lib
, config ? {}
}:

(lib.evalModules {
  modules = [
    (import ./module.nix { inherit pkgs; })
    config
  ];
}).config
