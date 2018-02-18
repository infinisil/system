{ pkgs ? import ../nixpkgs {}, lib ? pkgs.lib, debug ? false }:

with lib;

mapAttrs' (name: type: {
  name = removeSuffix ".nix" name;
  value = let file = ./. + "/${name}"; in
    lib.callPackageWith (pkgs // { inherit debug; }) file {};
}) (filterAttrs (name: type:
  (type == "directory" && builtins.pathExists "${toString ./.}/${name}/default.nix") ||
  (type == "regular" && hasSuffix ".nix" name && ! (name == "default.nix") && ! (name == "pkgs.nix"))
) (builtins.readDir ./.))
