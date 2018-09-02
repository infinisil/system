{ lib ? import ./external/nixpkgs/lib
, config ? import ./config.nix
}:

lib.evalModules {
  modules = [
    ./main.nix
    config
  ];
}
