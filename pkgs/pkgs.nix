{ pkgs ? import ../nixpkgs {}, lib ? pkgs.lib }:

with lib;

mapAttrs' (name: type: {
  name = removeSuffix ".nix" name;
  value = pkgs.callPackage "${./.}/${name}" {};
}) (filterAttrs (name: type:
  (type == "directory" && builtins.pathExists "${toString ./.}/${name}/default.nix") ||
  (type == "regular" && hasSuffix ".nix" name && ! (name == "default.nix") && ! (name == "pkgs.nix"))
) (builtins.readDir ./.))

