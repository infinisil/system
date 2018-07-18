{ pkgs ? import ../../external/nixpkgs {}, lib ? pkgs.lib, debug ? false, passwords ? true }:

with lib;

let
  pkgsFolder = ./pkgs;
in

mapAttrs' (name: type: {
  name = removeSuffix ".nix" name;
  value = let file = pkgsFolder + "/${name}"; in
  lib.callPackageWith (pkgs // {
    inherit debug;
  } // optionalAttrs passwords {
    passwords = import ../../external/private/passwords/gen.nix;
  }) file {};
}) (filterAttrs (name: type:
  (type == "directory" && builtins.pathExists "${toString pkgsFolder}/${name}/default.nix") ||
  (type == "regular" && hasSuffix ".nix" name)
) (builtins.readDir pkgsFolder))
