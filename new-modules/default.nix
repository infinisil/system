{ lib, ... }:

with lib;

let

  getDir = dir: mapAttrs (file: type:
    if type == "directory" then getDir "${dir}/${file}" else type
  ) (builtins.readDir dir);

  files = dir: collect isString (mapAttrsRecursive (path: type: concatStringsSep "/" path) (getDir dir));

  validFiles = dir: map (file: ./. + "/${file}") (filter (file: hasSuffix ".nix" file && file != "default.nix") (files dir));

in

{

  imports = validFiles ./.;

}
